#include "env.h"
#include <stdlib.h>
#include <string.h>

#define ERROR (-1)
extern char **environ;

int setenv(const char *name, const char *value, int overwrite) {
  char **p, **newenv, **q;
  int n, numenv;
  for (numenv = 0, p = environ; *p; p++) numenv++;
  n = strlen(name);
  char *newitem = malloc(n + 1 + strlen(value) + 1);
  strcpy(newitem,name);
  strcat(newitem,"=");
  strcat(newitem,value);
  for (p = environ; *p; p++) {
    if (0 == strncmp(name,*p,n) && (*p)[n] == '=') {
      *p = newitem;
      return 0;
    }
  }
  newenv = (char **)malloc((numenv+2)*sizeof(char *));
  if (newenv == NULL) return ERROR;
  for (p = environ, q = newenv; *p; p++,q++) *q = *p;
  *q++ = newitem;
  *q = NULL;
  return 0;
}
