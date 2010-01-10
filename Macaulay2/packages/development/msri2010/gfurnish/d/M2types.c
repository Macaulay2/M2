#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "M2types.h"
#include "M2mem.h"

#if defined(_WIN32)
#define NEWLINE "\r\n"
#elif defined(__MACINTOSH__)	/* is this obsolete??? */
#define NEWLINE "\r"
#else
#define NEWLINE "\n"
#endif

char newline[] = NEWLINE;

static struct M2_string_struct system_newline_contents = { 1, { '\n' } };
M2_string system_newline = &system_newline_contents;

const char *nullstringer(const char *s) {
  return *s ? s : NULL;
}

char *tocharstar(M2_string s)
{
  char *p = getmem_atomic(s->len + 1);
  memcpy(p,s->array,s->len);
  p[s->len] = 0;
  return p;
}

char **tocharstarstar(M2_stringarray p)
{
  int n = p->len;
  char **s = (char **)getmem((n + 1)*sizeof(char *));
  unsigned int i;
  for (i=0; i<n; i++) s[i] = tocharstar(p->array[i]);
  s[n] = NULL;
  return s;
}

char *tocharstar_malloc(M2_string s)
{
  char *p = getmem_malloc(s->len + 1);
  memcpy(p,s->array,s->len);
  p[s->len] = 0;
  return p;
}

char **tocharstarstar_malloc(M2_stringarray p)
{
  int n = p->len;
  char **s = (char **)getmem_malloc((n + 1)*sizeof(char *));
  unsigned int i;
  for (i=0; i<n; i++) s[i] = tocharstar_malloc(p->array[i]);
  s[n] = NULL;
  return s;
}

static struct M2_string_struct emptyM2String;

M2_string strings_substr(M2_string x, int start, int len)
{
  M2_string p;
  if (start < 0) start += x->len;	/* start<0 means count from the end */
  if (start < 0) len += start, start = 0;
  if (start + len > (int)x->len) len = x->len - start;
  if (len <= 0) return &emptyM2String;
  if (start == 0 && len == (int)x->len) return x;
  p = (M2_string) getmem_atomic(sizeofarray(p,len));
  p->len = len;
  memcpy(p->array,x->array+start,len);
  return p;
}

M2_string strings_substrAlwaysCopy(M2_string x, int start, int len)
{
  M2_string p;
  if (start < 0) start += x->len;	/* start<0 means count from the end */
  if (start < 0) len += start, start = 0;
  if (start + len > (int)x->len) len = x->len - start;
  if (len <= 0) return &emptyM2String;
  /* if (start == 0 && len == (int)x->len) return x; */
  p = (M2_string) getmem_atomic(sizeofarray(p,len));
  p->len = len;
  memcpy(p->array,x->array+start,len);
  return p;
}

M2_string strings_substr_1(M2_string x, int start)
{
  return strings_substr(x,start,x->len - start);
}

M2_string tostring(char const *s)
{
  int n = s ? strlen(s) : 0;
  M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
  p->len = n;
  memcpy(p->array,s,n);
  return p;
}

M2_string tostring2(const char const *s) /* identical to tostring! */
{
  int n = s ? strlen(s) : 0;
  M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
  p->len = n;
  memcpy(p->array,s,n);
  return p;
}

M2_string tostringn(const char *s,int n)
{
  M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
  p->len = n;
  memcpy(p->array,s,n);
  return p;
}

M2_arrayint toarrayint(int n,int *p)
{
  M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  memcpy(z->array,p,n * sizeof(int));
  return z;
}

M2_arrayint makearrayint(int n)
{
  M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
  z->len = n;
  return z; /* Note that getmem_atomic returns zeroed memory */
}

M2_stringarray tostrings(int n,const char **s)
{
  int i;
  M2_stringarray a;
  a = (M2_stringarray) getmem (sizeofarray(a,n));
  a->len = n;
  for (i=0; i<n; i++) a->array[i] = tostring(s[i]);
  return a;
}

M2_string strings_join(M2_string x,M2_string y)
{
  M2_string p;
  p = (M2_string) getmem_atomic(sizeofarray(p,x->len+y->len));
  p->len = x->len + y->len;
  memcpy(p->array,x->array,x->len);
  memcpy(p->array+x->len,y->array,y->len);
  return p;
}

M2_string strings_enlarge(int n, M2_string x) {
  M2_string p;
  int m = x->len + n;
  if (n <= 0) return x;
  p = (M2_string) getmem_atomic(sizeofarray(p,m));
  p->len = m;
  memcpy(p->array,x->array,x->len);
  memset(p->array+x->len,0,n);
  return p;
}

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
