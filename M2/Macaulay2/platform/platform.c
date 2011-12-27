#include <tokens-exports.h>
#include "platform.h"
#include <M2mem.h>


M2_string system_realpath(M2_string filename) {
  char *fn = M2_tocharstar(filename);
  char buf[PATH_MAX+1];
  char *r = realpath(*fn ? fn : ".",buf);
  if (platformIsDirectory(r)) strcat(r,"/");
  GC_FREE(fn);
  return r == NULL ? NULL : M2_tostring(buf);
}

M2_string system_getcwd()
{
     /* this function now adds a terminal / to the directory name */
     char buf[700];
     static const char slash[] = "/";
     char *x = getcwd(buf,sizeof(buf)-strlen(slash));
#if defined(_WIN32)
     char *p;
     for (p=x; *p; p++) if (*p == '\\') *p = '/';
#endif
     if (0 != strcmp(buf,slash)) strcat(buf,slash);
     if (x != NULL) return M2_tostring(x);
     return M2_tostring("");
     }

M2_string system_getenv(M2_string s) {
     char *ss = M2_tocharstar(s);
     char *x = getenv(ss);
     GC_FREE(ss);
     if (x == NULL) return M2_tostring("");
     else return M2_tostring(x);
     }
