#include "platform.h"

M2_string system_realpath(M2_string filename) {
  char *fn = M2_tocharstar(filename);
  char buf[PATH_MAX+1];
  char *r = realpath(*fn ? fn : ".",buf);
  if (isDirectory(r)) strcat(r,"/");
  GC_FREE(fn);
  return r == NULL ? NULL : M2_tostring(buf);
}

