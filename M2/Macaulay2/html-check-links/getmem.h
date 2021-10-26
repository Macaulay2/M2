#ifndef GETMEM_H
#define GETMEM_H
#include <string.h>

#include <M2/gc-include.h>

extern void outofmem() __attribute__ ((noreturn));
static inline char *getmem(unsigned n) {
  char *x = GC_malloc(n);
  if (x == NULL) outofmem();
  return x;
}
#define new(type) (type *)getmem(sizeof(type))
#define newarray(type,n) (type *) getmem((n) * sizeof(type))
#define getmemfor(x) ((x) = (typeof(x))getmem(sizeof *x))
#define clearmem(x)  memset(&(x),0,sizeof(x))
static inline char *strperm(char *s) {
  char *t = getmem(strlen(s)+1);
  strcpy(t,s);
  return t;
}
static inline char *strnperm(char *s,int n) {
  int l = strlen(s);
  char *t;
  if (n < l) l = n;
  t = getmem(l+1);
  t[l]=0;
  strncpy(t,s,l);
  return t;
}
static inline char *concat(char *s, char *t) {
  unsigned int n = strlen(s);
  char *u = getmem(n + strlen(t) + 1);
  strcpy(u,s);
  strcpy(u+n,t);
  return u;
}
#endif
