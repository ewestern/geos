#include <geos_c.h>

void geos_notice_handler(const char *msg, void *userdata);
void geos_error_handler(const char *msg, void *userdata);
GEOSContextHandle_t init_GEOS();

