#include <malloc.h>
#include <string.h>

#define ERROR (-1)
extern char **environ;

int setenv(char *v) {
  char **p, **newenv, **q;
  char *veq = strchr(v,'=');
  int n, ne;
  if (veq == NULL) return ERROR;
  n = veq - v;
  for (p = environ; *p; p++) {
    if (0 == strncmp(v,*p,n) && (*p)[n] == '=') {
      *p = v;
      return 0;
    }
  }
  for (ne = 0, p = environ; *p; p++) ne++;
  newenv = (char **)malloc((ne+2)*sizeof(char *));
  if (newenv == NULL) return ERROR;
  for (p = environ, q = newenv; *p; p++,q++) *q = *p;
  *q++ = v;
  *q = NULL;
  return 0;
}
