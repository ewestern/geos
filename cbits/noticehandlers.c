#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <geos_c.h>

void 
geos_notice_handler(const char *msg, void *userdata)
{
#ifdef DEBUG
  puts(msg);
#else
#endif
}

GEOSContextHandle_t init_GEOS() {
  void *pVoid;
  GEOSContextHandle_t handle = GEOS_init_r();
  GEOSContext_setNoticeMessageHandler_r(handle, geos_notice_handler, pVoid);
  GEOSContext_setErrorMessageHandler_r(handle, geos_notice_handler, pVoid);
  return handle;
}


