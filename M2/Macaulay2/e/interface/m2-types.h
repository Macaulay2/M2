#pragma once
// IWYU pragma: private, include "engine-includes.hpp"

#ifndef _GNU_SOURCE
 #define _GNU_SOURCE
#endif
#include <stdint.h>
#define hash_t uint64_t
#include <M2/config.h>
 extern char newline[]; 

#ifdef HAVE_UNISTD_H
 #include <unistd.h>
#endif
#ifdef HAVE_MATH_H
 #include <math.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
 #include <sys/resource.h>
#endif

#ifdef __cplusplus
  #define BASECLASS : public our_new_delete
  #include "newdelete.hpp"
#else
  #define BASECLASS
#endif

  #include <M2/math-include.h>

#if !defined(SAFEC_EXPORTS)
typedef char M2_bool;

typedef struct M2_string_struct * M2_string;
struct M2_string_struct {int len;signed char array[];};

typedef struct M2_arrayint_struct * M2_arrayint;
typedef M2_arrayint M2_arrayintOrNull;
struct M2_arrayint_struct {int len;int array[];};

typedef struct M2_ArrayString_struct * M2_ArrayString;
typedef M2_ArrayString M2_ArrayStringOrNull;
struct M2_ArrayString_struct {int len;M2_string array[];};

struct MonomialOrdering;
struct Ring;
struct M2SLEvaluator;
struct M2SLProgram;
struct StraightLineProgram;
struct PathTracker;
struct M2PointArray;
struct MutableMatrix;
struct MutableComplex;
struct Matrix;
struct FreeModule;
struct Monoid;
struct MonomialIdeal;

typedef mpz_ptr gmp_ZZmutable;
typedef mpz_srcptr gmp_ZZ;
typedef mpz_srcptr gmp_ZZorNull;

typedef mpq_ptr gmp_QQmutable;
typedef mpq_srcptr gmp_QQ;
typedef mpq_srcptr gmp_QQorNull;

typedef mpfr_srcptr gmp_RR;
typedef mpfr_srcptr gmp_RRorNull;
typedef mpfr_ptr gmp_RRmutable;

typedef mpfi_ptr gmp_RRimutable;
typedef mpfi_srcptr gmp_RRi;
typedef mpfi_srcptr gmp_RRiorNull;

typedef struct gmp_CC_struct * gmp_CC;
typedef gmp_CC gmp_CCorNull;
struct gmp_CC_struct BASECLASS {mpfr_srcptr re;mpfr_srcptr im;};
typedef struct gmp_CCmutable_struct * gmp_CCmutable;
struct gmp_CCmutable_struct BASECLASS {mpfr_ptr re;mpfr_ptr im;};

typedef struct gmp_CCi_struct * gmp_CCi;
typedef gmp_CCi gmp_CCiorNull;
struct gmp_CCi_struct BASECLASS {mpfi_srcptr re;mpfi_srcptr im;};
typedef struct gmp_CCimutable_struct * gmp_CCimutable;
struct gmp_CCimutable_struct BASECLASS {mpfi_ptr re;mpfi_ptr im;};

typedef struct gmp_arrayZZ_struct * gmp_arrayZZ;
struct gmp_arrayZZ_struct {int len;mpz_srcptr array[];};

typedef struct engine_RawMonomialArray_struct * engine_RawMonomialArray;
struct engine_RawMonomialArray_struct {int len;const struct EngineMonomial * array[];};
typedef const struct RingElement * engine_RawRingElement;

typedef struct engine_RawRingElementArray_struct * engine_RawRingElementArray;
typedef engine_RawRingElementArray engine_RawRingElementArrayOrNull;
struct engine_RawRingElementArray_struct {int len;const struct RingElement * array[];};

typedef struct engine_RawRingElementArrayArray_struct * engine_RawRingElementArrayArray;
typedef engine_RawRingElementArrayArray engine_RawRingElementArrayArrayOrNull;
struct engine_RawRingElementArrayArray_struct {int len;engine_RawRingElementArray array[];};

typedef struct engine_RawArrayPair_struct * engine_RawArrayPair;
typedef engine_RawArrayPair engine_RawArrayPairOrNull;
struct engine_RawArrayPair_struct BASECLASS {engine_RawMonomialArray monoms;engine_RawRingElementArray coeffs;};

typedef struct engine_RawArrayIntPair_struct * engine_RawArrayIntPair;
typedef engine_RawArrayIntPair engine_RawArrayIntPairOrNull;
struct engine_RawArrayIntPair_struct BASECLASS {M2_arrayint a;M2_arrayint b;};

typedef struct engine_RawMutableMatrixArray_struct * engine_RawMutableMatrixArray;
typedef engine_RawMutableMatrixArray engine_RawMutableMatrixArrayOrNull;
struct engine_RawMutableMatrixArray_struct {int len;struct MutableMatrix * array[];};

typedef struct engine_RawMatrixArray_struct * engine_RawMatrixArray;
typedef engine_RawMatrixArray engine_RawMatrixArrayOrNull;
struct engine_RawMatrixArray_struct {int len;const struct Matrix * array[];};

typedef struct engine_RawMatrixAndInt_struct * engine_RawMatrixAndInt;
struct engine_RawMatrixAndInt_struct BASECLASS {const struct Matrix * M;int i;};

typedef struct engine_RawMatrixPair_struct * engine_RawMatrixPair;
typedef engine_RawMatrixPair engine_RawMatrixPairOrNull;
struct engine_RawMatrixPair_struct BASECLASS {const struct Matrix * a;const struct Matrix * b;};

typedef struct engine_RawRingElementPair_struct * engine_RawRingElementPair;
typedef engine_RawRingElementPair engine_RawRingElementPairOrNull;
struct engine_RawRingElementPair_struct BASECLASS {const struct RingElement * a;const struct RingElement * b;};

typedef struct gmp_ZZpair_struct * gmp_ZZpair;
typedef gmp_ZZpair gmp_ZZpairOrNull;
struct gmp_ZZpair_struct BASECLASS {mpz_srcptr a;mpz_srcptr b;};

//typedef struct M2SLEvaluator * engine_RawSLEvaluator;
//typedef struct M2SLEvaluator * engine_RawSLEvaluatorOrNull;

/* typedef const struct MonomialOrdering * engine_RawMonomialOrdering; */
/* typedef const struct MonomialOrdering * engine_RawMonomialOrderingOrNull; */

typedef struct engine_RawMonomialOrderingArray_struct * engine_RawMonomialOrderingArray;
struct engine_RawMonomialOrderingArray_struct {int len;const struct MonomialOrdering * array[];};

/*
struct EngineMonomial;
typedef const struct MonomialOrdering * engine_RawMonomialOrdering;
typedef const struct MonomialOrdering * engine_RawMonomialOrderingOrNull;
typedef const struct Monoid * engine_RawMonoid;
typedef const struct Monoid * engine_RawMonoidOrNull;
typedef const struct Ring * engine_RawRing;
typedef const struct RingMap * engine_RawRingMap;
typedef const struct Ring * engine_RawRingOrNull;
typedef const struct RingElement * engine_RawRingElement;
typedef const struct RingElement * engine_RawRingElementOrNull;
typedef const struct MonomialIdeal * engine_RawMonomialIdeal;
typedef const struct MonomialIdeal * engine_RawMonomialIdealOrNull;
typedef const struct FreeModule * engine_RawFreeModule;
typedef const struct FreeModule * engine_RawFreeModuleOrNull;
typedef const struct Matrix * engine_RawMatrix;
typedef const struct Matrix * engine_RawMatrixOrNull;
*/

#if defined(__cplusplus)
extern "C" {
#endif

  M2_arrayint M2_makearrayint(int n);
  char * M2_tocharstar(M2_string s);
  M2_string M2_join(M2_string x, M2_string y);
  M2_string M2_tostring(const char* s);
  M2_string M2_tostringn(char *s, int n);

#if defined(__cplusplus)
}
#endif


extern char newline[];
extern int M2_numTBBThreads;
extern int M2_gbTrace;
extern int M2_numericalAlgebraicGeometryTrace;

#else
typedef struct gmp_CC_struct * gmp_CC;

#endif // !defined(SAFEC_EXPORTS)

