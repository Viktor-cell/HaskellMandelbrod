#include "./raylib/include/raylib.h"

#define DrawPixel_(x, y, r, g, b, a) \
        DrawPixel(x, y, (Color){r, g, b, a});

#define DrawRectangle_(x, y, w, h, r, g, b, a) \
    DrawRectangle(x, y, w, h, (Color) {r, g, b, a});
