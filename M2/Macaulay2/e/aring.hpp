// Copyright 2011 Michael E. Stillman

#ifndef _aring_hpp_
#define _aring_hpp_

#include <assert.h>
#include "ringelem.hpp"
#include "buffer.hpp"

#if 0
#define RING(T, A) static_cast<const ConcreteRing<T> *>(A)->R_
#define RELEM(T, a) static_cast<RElementWrap<T> &>(a).val_
#define constRELEM(T, a) static_cast<const RElementWrap<T> &>(a).val_
#endif

class PolynomialRing;
class RingMap;

namespace M2 {

////////////////////////////////////////////////////////
// Programming level interfaces ////////////////////////
////////////////////////////////////////////////////////

/**
\ingroup rings
*/
enum RingID {
  ring_example = 0,
  ring_ZZ,
  ring_ZZFlint,
  ring_QQ,
  ring_QQFlint,
  ring_ZZp,
  ring_ZZpFfpack,
  ring_ZZpFlint,
  ring_GFM2,
  ring_GFGivaro,
  ring_GFFlintBig,
  ring_GFFlintZech,
  ring_RR,
  ring_CC,
  ring_RRR,
  ring_CCC,
  ring_tower_ZZp,
  ring_old      ///< refers to all rings which are not ConcreteRing's.
};

/**
\ingroup rings
*/
class RingInterface : public our_new_delete
{
};  ///< inherit from this if the class is to be used as a template parameter
    /// for ConcreteRing

class DummyRing : public RingInterface
{
 public:
  const PolynomialRing *mOriginalRing;
  typedef long FieldType;
  typedef long ElementType;

  typedef ElementType elem;

  int characteristic() const { return 0; }
  unsigned int computeHashValue(const elem &a) const
  {
    return static_cast<unsigned int>(a);
  }

  M2_arrayint getModPolynomialCoeffs() const { return 0; }
  M2_arrayint getGeneratorCoeffs() const { return 0; }
  void getGenerator(elem &result) const { result = 0; }
  const PolynomialRing &originalRing() const { return *mOriginalRing; }
  long coerceToLongInteger(ElementType a) const { return a; }
  void lift_to_original_ring(ring_elem &result, const ElementType &f) const {}
  M2_arrayint fieldElementToM2Array(ElementType el) const { return 0; }
  void to_ring_elem(ring_elem &result, const ElementType &a) const {}
  void from_ring_elem(ElementType &result, const ring_elem &a) const {}
  bool promote(const Ring *Rf, const ring_elem f, ElementType &result) const
  {
    return false;
  }

  bool lift(const Ring *Rg, const ElementType f, ring_elem &result) const
  {
    return false;
  }

  void eval(const RingMap *map,
            const elem f,
            int first_var,
            ring_elem &result) const
  {
  }

  void text_out(buffer &o) const { o << "GF(dummy)"; }
  void elem_text_out(buffer &o,
                     const ElementType a,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const {};

  void init_set(elem &result, elem a) const { result = a; }
  void set(elem &result, elem a) const { result = a; }
  void set_from_long(elem &result, long a) const { result = a; }
  void init(elem &result) const { result = 0; }
  void set_from_mpz(elem &result, mpz_srcptr a) const { result = 0; }
  bool set_from_mpq(elem &result, mpq_srcptr a) const { return false; }
  bool set_from_BigReal(elem &result, gmp_RR a) const { return false; }
  void set_var(elem &result, int v) const { result = 1; }
  bool is_unit(const ElementType f) const { return false; }
  bool is_zero(const ElementType f) const { return true; }
  bool is_equal(const ElementType f, const ElementType g) const
  {
    return false;
  }
  int compare_elems(const ElementType f, const ElementType g) const
  {
    return 1;
  }

  void clear(elem &result) const { result = 0; }
  void set_zero(elem &result) const { result = 0; }
  void copy(elem &result, const elem a) const { result = a; }
  void negate(elem &result, const elem a) const {};
  ;

  void invert(elem &result, const elem a) const {};
  ;

  void add(elem &result, const elem a, const elem b) const {};
  ;

  void subtract(ElementType &result,
                const ElementType a,
                const ElementType b) const {};
  ;

  void subtract_multiple(elem &result, const elem a, const elem b) const {};
  ;

  void mult(elem &result, const elem a, const elem b) const {};
  ;

  ///@brief test doc
  void divide(elem &result, const elem a, const elem b) const {};
  ;

  void power(elem &result, const elem a, const int n) const {};
  ;

  void power_mpz(elem &result, const elem a, mpz_srcptr n) const {};
  ;

  void syzygy(const ElementType a,
              const ElementType b,
              ElementType &x,
              ElementType &y) const {};
  ;

  void random(ElementType &result) const { result = 0; }
  void swap(ElementType &a, ElementType &b) const { assert(false); };
};

#if 0

/**
\ingroup rings
*/
  class PolynomialRingInterface {}; ///< inherit from this if the class is to be used as a template param for PolynomialConcreteRing
  class UserObject {};


  template <class RingType> class AConcreteRing;


  class RElement
  {
  public:
    RElement() {}
    virtual ~RElement() {}
    //    virtual RElement *clone() = 0;
    //    virtual RElement &operator=(const RElement &a) = 0;
  };

/**
\ingroup rings
*/
  class ARing : public UserObject
  {
  public:
    template<class RingType>
    const AConcreteRing<RingType> * cast_to_AConcreteRing() const { return dynamic_cast< const AConcreteRing<RingType> * >(this) ; }
    // result will be either 0, or this.

    virtual RingID getRingID() const = 0;

    virtual void init_set(RElement &a, long b) const = 0;

    virtual void add_to(RElement &a, const RElement &b) const = 0;

    static bool converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  };

#endif

};  // namespace M2

#endif

// Some basic functions:
// (a) Create a ring
//  create a ZZp ring:
//    top level function (in x-relem.cpp)
//    function: create an ARing from a basic ring.
//      issue: the basic ring needs to know its ARing?  Preferably not?
//      if so: the createRing needs to set the ARing in the basic ring object
//      afterwards.
//      if not: to make a toplevel object, need to know ARing from somewhere
//      else.
//       problem: make the lead coefficient (i.e. need to know the coefficient
//       ring.  So
//       APolynomialRing will need to know this, and it will mirror what the
//       RingInterface object knows already)
//  CLEANER: keep the two layers separate...  SO:
//    An APolynomialRing must have functions: getCoefficientRing.
//    And what about creating a matrix over a ring?
// (b) m1 = m + n (matrices).
// (c) Create a new matrix

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
