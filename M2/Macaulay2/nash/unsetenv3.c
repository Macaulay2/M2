#include "env.h"
#include <string.h>

extern char **environ;

void unsetenv(char *v) {
  char **p;
  int n = strlen(v);
  for (p = environ; *p; p++) {
    if (0 == strncmp(v,*p,n) && (*p)[n] == '=') {
      for (; *p; p++) p[0]=p[1];
      return;
    }
  }
}
