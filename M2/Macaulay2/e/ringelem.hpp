// Copyright 1995  Michael E. Stillman

#ifndef _ringelem_hh_
#define _ringelem_hh_

#include <stddef.h>
#include <gmp.h>
#include <mpfr.h>
#if !defined(SAFEC_EXPORTS)
#include <engine-exports.h>
#endif
#include "newdelete.hpp"
#include "gmp-util.h"

using ZZ = mpz_srcptr;
using ZZmutable = mpz_ptr;
using QQ = mpq_srcptr;
using QQmutable = mpq_ptr;
using RRRelement = mpfr_srcptr;
using RRRmutable = mpfr_ptr;

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
  double double_val;
  QQ mpq_val;
  ZZ mpz_val;
  mpfr_srcptr mpfr_val;
 public:
  ring_elem() : poly_val(nullptr) {}
  // explicit ring_elem(int a) : int_val(a) {} // really want this version...
  ring_elem(int a) : int_val(a) {}
  ring_elem(Nterm *a) : poly_val(a) {}
  explicit ring_elem(mpz_srcptr a) : mpz_val(a) {}
  explicit ring_elem(double a) : double_val(a) {}
  explicit ring_elem(mpq_srcptr a) : mpq_val(a) {}
  explicit ring_elem(mpfr_srcptr a) : mpfr_val(a) {}
  explicit ring_elem(local_elem* a) : local_val(a) {}
  explicit ring_elem(schur_poly* a) : schur_poly_val(a) {}

  //  operator int() const { return int_val; }
  operator tpoly() const { return poly_val; }
  Nterm *get_poly() const { return poly_val; }

  int get_int() const { return int_val; }
  double get_double() const { return double_val; }
  mpz_srcptr get_mpz() const { return mpz_val; }
  mpq_srcptr get_mpq() const { return mpq_val; }
  mpfr_srcptr get_mpfr() const { return mpfr_val; }
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

#define CCELEM_VAL(f) (reinterpret_cast<gmp_CC>((f).poly_val))
#define CC_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))
#define CC_IM(f) ((CCELEM_VAL(f))->im)
#define CC_RE(f) ((CCELEM_VAL(f))->re)
#define CC_NORM(f) (sqrt(CC_RE(f) * CC_RE(f) + CC_IM(f) * CC_IM(f)))

#define MPF_VAL(f) (reinterpret_cast<mpfr_ptr>((f).poly_val))
#define MPF_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#define BIGCC_VAL(f) (reinterpret_cast<gmp_CC>((f).poly_val))
#define BIGCC_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))
#define BIGCC_RE(f) (BIGCC_VAL(f)->re)  // returns actual value, not copy
#define BIGCC_IM(f) (BIGCC_VAL(f)->im)

#define TOWER_VAL(f) (reinterpret_cast<poly>((f).poly_val))
#define TOWER_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
