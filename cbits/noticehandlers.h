#include <geos_c.h>

void geos_notice_handler(const char *fmt, ...);
void geos_error_handler(const char *fmt, ...);

GEOSContextHandle_t init_GEOS();
void                finalise_GEOS(GEOSContextHandle_t handle);

