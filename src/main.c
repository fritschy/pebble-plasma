#include <pebble.h>

#if 1
#undef APP_LOG
#define APP_LOG(...)
#define START_TIME_MEASURE() {
#define END_TIME_MEASURE(x) }
#define DBG(...)
#else
#define DBG(...) APP_LOG(APP_LOG_LEVEL_DEBUG, __VA_ARGS__)
static unsigned int get_time(void) {
   time_t s;
   uint16_t ms;
   time_ms(&s, &ms);
   return (s & 0xfffff) * 1000 + ms;
}

#define START_TIME_MEASURE() \
   {                         \
   unsigned tm_0 = get_time()
#define END_TIME_MEASURE(x)                                       \
   unsigned tm_1 = get_time();                                    \
   APP_LOG(APP_LOG_LEVEL_DEBUG, "%s: took %dms", x, tm_1 - tm_0); \
   }
#endif

struct App {
   Window *w;
   Animation *a;
   uint8_t color;
};

struct App *g;

static uint8_t *fb;

#define FBW 144
#define FBH 168
#define FBSIZE (FBW * FBH)
#define FBW2 (FBW / 2)
#define FBH2 (FBH / 2)
#define FBC GPoint(FBW2, FBH2)
#define FBBOUNDS GRect(0, 0, FBW, FBH)
#define PM ((uint8_t)0x3f)

typedef struct rgb {
   int32_t r, g, b;
} rgb_t;

#define mix256(x,y,a) ((x * (0xff - a)) / 256 + (y * a) / 255)

static inline rgb_t mixrgb(rgb_t x, rgb_t y, int a) {
   return (rgb_t){
      mix256(x.r, y.r, a),
      mix256(x.g, y.g, a),
      mix256(x.b, y.b, a),
   };
}

static inline rgb_t to_rgb(uint32_t o) {
   return (rgb_t) { ((o>>4)&3)<<6, ((o>>2)&3)<<6, (o&3)<<6 };
}

static inline uint32_t from_rgb(rgb_t oc) {
   return (uint32_t)((((uint32_t)oc.r >> 6) << 4) | (((uint32_t)oc.g >> 6) << 2) |
                     (((uint32_t)oc.b >> 6) << 0) | 0xc0);
}

static void xmemset(uint32_t *p, uint32_t v, size_t len) {
   for (size_t i = 0; i < len; i++)
      p[i] = 0;
}

static int sqrti(int f)
{
   int v = f / 2;
#define IT() v = (v + f / v) / 2
   IT();
   IT();
   IT();
   IT();
#undef IT
   return v;
}

#define PLAW (FBW/4+1)
#define PLAH (FBH/4+1)
struct plasma {
   int time;
   uint8_t data[PLAH][PLAW];
};

struct plasma g_plasma;

// pre-compute plasma each frame
static void plasma_compute(void) {
   int lta = g_plasma.time * 2048;
   for (int y = 0; y < PLAH; y++) {
      int ly = (y*4 - FBW2) * 1024;
      int iy = (y*4 - FBW);
      for (int x = 0; x < PLAW; x++) {
         uint32_t s = 0;
         int lx = (x*4 - FBW2) * 1024;
         int ix = (x*4 - FBW);
         int cx = sin_lookup(lta / 4) >> 8;
         int cy = cos_lookup(lta / 2) >> 8;
         s += (sin_lookup(sqrti(cx*cx + cy*cy) + lta * 3 / 8) + 0xffff) >> 8;
         s += (sin_lookup(ly + lta) + 0xffff) >> 8;
         s += (sin_lookup((lx+ly+lta) / 2) + 0xffff) >> 8;
         s += (sin_lookup((lx + lta) / 2) + 0xffff) >> 8;
         s >>= 3;
         if (s < 128)
            s = mix256(0, 0xff, (s << 1));
         else
            s = mix256(0xff, 0, ((s - 128) << 1));
         g_plasma.data[y][x] = s;
      }
   }
   g_plasma.time++;
}

static int plasma_lookup(int x, int y) {
   // can filter some, but it costs dearly
   return   g_plasma.data[y/4  ][x/4  ]; /* * 3 / 8
          + g_plasma.data[y/4+1][x/4+1] * 1 / 8
          + g_plasma.data[y/4+1][x/4+0] * 2 / 8
          + g_plasma.data[y/4+0][x/4+1] * 2 / 8;*/
}

static void __attribute__((optimize(2))) fbPlasma(uint8_t c0_, uint8_t c1_) {
   rgb_t c0 = to_rgb(c0_);
   rgb_t c1 = to_rgb(c1_);

   plasma_compute();

   // width+2 items to eliminate extra branche sin the inner loop and...
   static rgb_t errors[2*(FBW+2)];
   // init error[0] to zero each time ...
   xmemset((void*) errors, 0, sizeof(errors) / 2 / 4);
   // ... set the error buffers 1 item ahead to eliminate extra branches in the inner loop
   rgb_t *error[2] = { errors+1, errors+(FBW+2) + 1 };

   for (int y = 0; y < FBH; y++) {
      rgb_t next = { 0,0,0 };                        // holds the "ahead" error

      // this still is not the same as the naive approach ...
      uint8_t *lfb = fb+y*FBW;
      for (int32_t *error0 = &error[0][0].r,
                   *error1 = &error[1][-1].r,
                   *error0end = error0+FBW*3,
                   x = 0; error0 != error0end; x++) {
         rgb_t const c = mixrgb(c0, c1, plasma_lookup(x, y));
         int8_t out = 0xc; // alpha, will be shifted up towards MSB 2 times
         int32_t const *ni = &next.r;
         int32_t const *ci = &c.r;
         int32_t *no = &next.r;

#define USAT(v,b,s) __asm__("usat %0, %2, %1, asr %3" : "=r"(v) : "r"(v), "i"(b), "i"(s) : )
#define PALETTE(x) (((x) + (PM + 1) / 2) & ~PM)

         // grouped computations so that _maybe_ the compiler can carry out the
         // computations with less spills, as opposed to computing all the oX, nX,
         // qeX etc and writing them back at the end.
         int32_t o0 = *ci++ + *ni++ + *error0;
         int32_t n0 = PALETTE(o0);
         int32_t qe0 = o0 - n0;
         USAT(n0, 3, 6);
         out |= n0; /* "or in" current channel */
         qe0 /= 2;
         *no++ = qe0;
         qe0 /= 2;
         *error1++ += qe0;
         *error0++ = 0;

         int32_t o1 = *ci++ + *ni++ + *error0;
         int32_t n1 = PALETTE(o1);
         int32_t qe1 = o1 - n1;
         USAT(n1, 3, 6);
         out <<= 2; /* shift current 2 places up towards alpha */
         out |= n1; /* "or in" current channel */
         qe1 /= 2;
         *no++ = qe1;
         qe1 /= 2;
         *error1++ += qe1;
         *error0++ = 0;

         int32_t o2 = *ci + *ni + *error0;
         int32_t n2 = PALETTE(o2);
         int32_t qe2 = o2 - n2;
         USAT(n2, 3, 6);
         out <<= 2; /* shift current 2 places up towards alpha */
         out |= n2; /* "or in" current channel */
         *lfb++ = out;
         qe2 /= 2;
         *no = qe2;
         qe2 /= 2;
         *error1++ += qe2;
         *error0++ = 0;

         int32_t *error2 = error1;
         *error2++ = qe0;
         *error2++ = qe1;
         *error2 = qe2;
      }

      // swap error buffers
      rgb_t *es = error[0];
      error[0] = error[1];
      error[1] = es;
   }
}

static void draw(void) {
   START_TIME_MEASURE();
   fbPlasma(0xc0, 0xff);
   END_TIME_MEASURE("drawing");
}

static void update(Layer *layer, GContext *ctx) {
   GBitmap *bmp = graphics_capture_frame_buffer(ctx);
   fb = gbitmap_get_data(bmp);

   draw();

   graphics_release_frame_buffer(ctx, bmp);
   fb = NULL;
}

static void a_setup(Animation *a) {}

static void a_update(Animation *a, AnimationProgress t) {
   layer_mark_dirty(window_get_root_layer(g->w));
}

static void a_teardown(Animation *a) {}

static AnimationImplementation a_impl = {
   .setup = a_setup, .update = a_update, .teardown = a_teardown};

static void window_load(Window *w) {
   struct App *a = g;
   layer_set_update_proc(window_get_root_layer(w), update);
   a->a = animation_create();
   animation_set_implementation(a->a, &a_impl);
   animation_set_duration(a->a, ANIMATION_DURATION_INFINITE);
   animation_schedule(a->a);
}

static void window_unload(Window *w) {
   animation_destroy(g->a);
   g->a = NULL;
}

static void init(struct App *a) {
   g = a;
   a->w = window_create();
   window_set_user_data(a->w, a);
   window_set_window_handlers(a->w,
                              (WindowHandlers){
                                 .load = window_load, .unload = window_unload,
                              });
   window_stack_push(a->w, false);
}

static void fini(struct App *a) {
   window_destroy(a->w);
   a->w = NULL;
   g = NULL;
}

int main(void) {
   struct App a;
   init(&a);
   app_event_loop();
   fini(&a);
   return 0;
}