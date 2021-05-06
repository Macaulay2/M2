// Copyright 1995  Michael E. Stillman

#ifndef _ringelem_hh_
#define _ringelem_hh_

#include <stddef.h>
#if !defined(SAFEC_EXPORTS)
#include <engine-exports.h>
#endif
#include "interface/gmp-util.h"
#include "newdelete.hpp"

using ZZ = mpz_srcptr;
using ZZmutable = mpz_ptr;
using QQ = mpq_srcptr;
using QQmutable = mpq_ptr;
using RRRelement = mpfr_srcptr;
using RRRmutable = mpfr_ptr;
using RRielement = mpfi_srcptr;
using RRimutable = mpfi_ptr;

// The following is the data type used for complex numbers in aring-CCC
// Perhaps we should have it be 
typedef struct 
{
  __mpfr_struct re;
  __mpfr_struct im;
} cc_struct;
using cc_ptr = cc_struct *;
using cc_srcptr = cc_struct const *;

typedef struct
{
  double re;
  double im;
} cc_doubles_struct;
using cc_doubles_srcptr = cc_doubles_struct const *;
using cc_doubles_ptr = cc_doubles_struct *;

struct Nterm;
typedef Nterm *tpoly;
class schur_poly;
struct local_elem;

union ring_elem
{
  Nterm *poly_val;
  
 private:  // move this line up to the top eventually
  const schur_poly *schur_poly_val;
  const local_elem* local_val;
  int int_val;
  long long_val;
  double double_val;
  QQ mpq_val;
  ZZ mpz_val;
  mpfr_srcptr mpfr_val;
  mpfi_srcptr mpfi_val;
  cc_doubles_srcptr cc_doubles_val;
  cc_srcptr cc_val;
  const void *mPolyVal;
 public:
  ring_elem() : poly_val(nullptr) {}
  // explicit ring_elem(int a) : int_val(a) {} // really want this version...
  ring_elem(int a) : int_val(a) {}
  ring_elem(Nterm *a) : poly_val(a) {}
  explicit ring_elem(mpz_srcptr a) : mpz_val(a) {}
  explicit ring_elem(long a) : long_val(a) {}
  explicit ring_elem(double a) : double_val(a) {}
  explicit ring_elem(mpq_srcptr a) : mpq_val(a) {}
  explicit ring_elem(mpfr_srcptr a) : mpfr_val(a) {}
  explicit ring_elem(mpfi_srcptr a) : mpfi_val(a) {}
  explicit ring_elem(cc_srcptr a) : cc_val(a) {}
  explicit ring_elem(cc_doubles_srcptr a) : cc_doubles_val(a) {}
  explicit ring_elem(local_elem* a) : local_val(a) {}
  explicit ring_elem(const void* a) : mPolyVal(a) {} // non-commutative polynomials
  explicit ring_elem(schur_poly* a) : schur_poly_val(a) {}

  //  operator int() const { return int_val; }
  operator tpoly() const { return poly_val; }
  Nterm *get_poly() const { return poly_val; }

  int get_int() const { return int_val; }
  long get_long() const { return long_val; }
  double get_double() const { return double_val; }
  mpz_srcptr get_mpz() const { return mpz_val; }
  const void* get_Poly() const { return mPolyVal; }
  mpq_srcptr get_mpq() const { return mpq_val; }
  mpfr_srcptr get_mpfr() const { return mpfr_val; }
  mpfi_srcptr get_mpfi() const { return mpfi_val; }
  cc_srcptr get_cc() const { return cc_val; }
  cc_doubles_srcptr get_cc_doubles() const { return cc_doubles_val; }
  const local_elem* get_local_elem() const { return local_val; }
  const schur_poly* get_schur_poly() const { return schur_poly_val; }
};

struct Nterm
{
  Nterm *next;
  ring_elem coeff;
  int monom[1];
};

typedef struct vecterm *vec;
struct vecterm : public our_new_delete
{
  vec next;
  int comp;
  ring_elem coeff;
};

#define MPQ_VAL(f) ((f).get_mpq())

// these should only be used as temporary const.  Do not store results!
#define BIGCC_IM(f) (&(f).get_cc()->im)
#define BIGCC_RE(f) (&(f).get_cc()->re) 

// TODO: these need to be replaced... no casting, need new slot in ring_elem union type
#define TOWER_VAL(f) (reinterpret_cast<poly>((f).poly_val))
#define TOWER_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
