#ifndef _M2types_h
#define _M2types_h

#undef malloc
#undef free
#include <gmp.h>

extern char newline[];
extern char startupString1[];
extern char startupString2[];

typedef struct M2_string_struct {
    unsigned int len;
    char array[1];
  } *M2_string;

typedef char M2_bool;
#define M2_true 1
#define M2_false 0

typedef struct M2_arrayint_struct {
     unsigned int len;
     int array[1];
     } *M2_arrayint;

typedef struct M2_stringarray_struct {
     unsigned int len;
     M2_string array[1];
     } *M2_stringarray;

typedef double M2_RR;
typedef struct M2_CC_struct { double re, im; } *M2_CC; /* same as Complex (see tokens.d) */

typedef __mpz_struct *M2_Integer;
typedef __mpq_struct *M2_Rational;
typedef __mpf_struct *M2_BigReal; /* must agree with BigReal in gmp.d */
typedef struct M2_BigComplex_struct { __mpf_struct re, im; } *M2_BigComplex; /* must agree with BigComplex in gmp.d */

#ifndef DCODE
/* The C code produced from the D language has its own declarations for these things */
extern M2_string system_newline;

#if defined(__cplusplus)
extern "C" {
#endif
  extern M2_string tostring(char const *);
  extern M2_string tostringn(char *s,int n);
  extern M2_string strings_substr(M2_string x, int start, int len);
  extern M2_string strings_substr_1(M2_string x, int start);
  extern M2_string strings_join(M2_string x,M2_string y);
  extern char *tocharstar(M2_string);
  extern char *tocharstar_malloc(M2_string);
  
  extern M2_arrayint toarrayint(int n,int *p);
  extern M2_arrayint makearrayint(int n); /* Make an array of n 0's */
  
  extern char **tocharstarstar(M2_stringarray);
  extern char **tocharstarstar_malloc(M2_stringarray);
  extern M2_stringarray tostrings(int,char **);
#if defined(__cplusplus)
}
#endif

#endif

#define sizeofarray(s,len) (sizeof(*s) - sizeof(s->array) + (len)*sizeof(s->array[0]))

#include <gc.h>
extern void dummy_GC_warn_proc(char *, GC_word);

#endif

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
 End:
*/
