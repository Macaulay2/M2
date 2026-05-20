// Copyright 1995  Michael E. Stillman

#ifndef _ringelem_hh_
#define _ringelem_hh_

/**
 * @file ringelem.hpp
 * @brief `ring_elem` --- the universal value type carried by every `Ring*` in the engine.
 *
 * Declares the `ring_elem` union plus the GMP/MPFR/MPFI const
 * and mutable pointer aliases (`ZZ`, `ZZmutable`, `QQ`,
 * `QQmutable`, `RRRelement`, `RRRmutable`, `RRielement`,
 * `RRimutable`) and the inline `cc_struct` (MPFR pair),
 * `cc_doubles_struct` (`double` pair), and `cci_struct` (MPFI
 * pair) complex-number record types. The aliases exist so
 * engine function signatures can make const-ness explicit
 * (`void foo(ZZ a, ZZmutable result)` reads more clearly than
 * its `mpz_srcptr` / `mpz_ptr` expansion) and so a single
 * search-and-replace can flip the underlying GMP / MPFR / MPFI
 * type if that surface ever changes.
 *
 * `ring_elem` is the value the `Ring`-virtual API trafficks in
 * everywhere; each `Ring` subclass knows how to interpret the
 * union. The fields available are `int_val` / `long_val` /
 * `double_val` (primitives), `mpz_val` / `mpq_val` / `mpfr_val`
 * / `mpfi_val` (arbitrary-precision pointers),
 * `cc_doubles_val` / `cc_val` / `cci_val` (complex variants),
 * `poly_val` (`Nterm*` for `PolyRing` lists), `mPolyVal`
 * (opaque `void*` for non-commutative polynomials),
 * `schur_poly_val`, and `local_val`. Constructors and `get_*`
 * accessors pair up so each ring writes its values in and
 * reads them back out through the matching tag.
 *
 * @see ring.hpp
 * @see relem.hpp
 */

#include "M2/math-include.h"  // for mpfi_srcptr, mpfr_srcptr, mpq_srcptr
#include "monoid.hpp"         // for monomial
#include "newdelete.hpp"      // for our_new_delete

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
struct cc_struct
{
  __mpfr_struct re;
  __mpfr_struct im;
};
using cc_ptr = cc_struct *;
using cc_srcptr = cc_struct const *;

struct cc_doubles_struct
{
  double re;
  double im;
};
using cc_doubles_srcptr = cc_doubles_struct const *;
using cc_doubles_ptr = cc_doubles_struct *;

struct cci_struct
{
  __mpfi_struct re;
  __mpfi_struct im;
};
using cci_ptr = cci_struct *;
using cci_srcptr = cci_struct const *;

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
  cci_srcptr cci_val;
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
  explicit ring_elem(cci_srcptr a) : cci_val(a) {}
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
  cci_srcptr get_cci() const { return cci_val; }
  cc_doubles_srcptr get_cc_doubles() const { return cc_doubles_val; }
  const local_elem* get_local_elem() const { return local_val; }
  const schur_poly* get_schur_poly() const { return schur_poly_val; }
};

/**
 * @brief Singly linked-list node carrying one term of a polynomial-ring
 * element.
 *
 * @details `next` chains to the next term, `coeff` holds the term's
 * coefficient (in the parent ring's `ring_elem` representation),
 * and `monom[1]` is a C99-style flexible-array tail holding the
 * monomial's encoded `int` payload --- the actual length is
 * determined by the ring's monoid. Polynomial values stored in a
 * `ring_elem` via the `poly_val` union arm point at the head of a
 * chain of these.
 */
/* Implements a linked list of ring monomials along with coefficients */
struct Nterm
{
  Nterm *next;
  ring_elem coeff;
  // TODO: should this have type monomial?
  int monom[1];
};

typedef struct vecterm *vec;
/* Implements a linked list of module monomials along with coefficients */
// TODO: why is this garbage collected?
struct vecterm : public our_new_delete
{
  vec next;
  int comp;
  ring_elem coeff;
};

/* Implements an iterator for linked list-based multi-termed structs
 *
 * For example, the functions begin(Nterm*) and end(Nterm*) return
 * a TermIterator<Nterm> object which makes for(Nterm& t : f) work. */
template<typename T>
struct TermIterator
{
  T* p;

  TermIterator():              p(nullptr) {}
  TermIterator(T* ptr):        p(ptr)     {}
  TermIterator(ring_elem ptr): p(ptr)     {}

  TermIterator& operator++() { p = p->next; return *this; }

  T const& operator*() const  { return *p; }
  T&       operator*()        { return *p; }
  T const* operator->() const { return p; }
  T*       operator->()       { return p; }

  bool operator==(TermIterator const& rhs) const { return p == rhs.p; }
  bool operator!=(TermIterator const& rhs) const { return p != rhs.p; }
};

TermIterator<Nterm> begin(Nterm* ptr);
TermIterator<Nterm> end(Nterm*);

TermIterator<vecterm> begin(vecterm* ptr);
TermIterator<vecterm> end(vecterm*);


#define MPQ_VAL(f) ((f).get_mpq())

// these should only be used as temporary const.  Do not store results!
#define BIGCC_IM(f) (&(f).get_cc()->im)
#define BIGCC_RE(f) (&(f).get_cc()->re) 

// TODO: these need to be replaced... no casting, need new slot in ring_elem union type
#define TOWER_VAL(f) (reinterpret_cast<ARingTowerPolynomial>((f).poly_val))
#define TOWER_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
