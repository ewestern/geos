#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <geos_c.h>

void 
geos_notice_handler(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
}

GEOSContextHandle_t init_GEOS() {
  //printf("USING GEOS [%d.%d]\n", GEOS_VERSION_MAJOR, GEOS_VERSION_MINOR);
#if GEOS_VERSION_MAJOR > 3 && GEOS_VERSION_MINOR > 4
  GEOSContextHandle_t handle = GEOS_init_r();
  GEOSContext_setNoticeHandler_r(handle, geos_notice_handler);
  GEOSContext_setErrorHandler_r(handle, geos_notice_handler);
  return handle;
#else
  return initGEOS_r(geos_notice_handler, geos_notice_handler);
#endif
}

void finalise_GEOS(GEOSContextHandle_t handle)
{
  // printf("FINISHING\n");
#if GEOS_VERSION_MAJOR > 3 && GEOS_VERSION_MINOR > 4
  GEOS_finish_r(handle);
#else
  finishGEOS_r(handle);
#endif
}

