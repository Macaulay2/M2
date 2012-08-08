// Copyright 2010  Michael E. Stillman

#ifndef _coeffrings_zz_hpp_
#define _coeffrings_zz_hpp_

#include "ringelem.hpp"

#include "ZZ.hpp"
#include "ntl-interface.hpp"

/**
 * \ingroup coeffrings
 */
class CoefficientRingZZ_NTL : public our_new_delete
{
public:
  typedef RingZZ ring_type;
  typedef NTL::ZZ elem;
  typedef NTL::ZZ ElementType;

  CoefficientRingZZ_NTL(const RingZZ *R0) { }

  void init_set(elem &result, const elem &a) const { result = a; }

  void set_zero(elem &result) const { result = 0; }

  void set(elem &result, const elem &a) const { result = a; }

  bool is_zero(const elem &result) const { return result == 0; }

  bool is_equal(const elem &a, const elem &b) const { return a == b; }

  void invert(elem &result, const elem &a) const
  {
    if (a == 1 || a == -1)
      result = a;
    else
      result = 0;
  }

  void subtract_multiple(elem &result, elem a, elem b) const;
    // result -= a*b

  void add(elem &result, const elem &a, const elem &b) const
  {
    result = a+b;
  }

  void negate(elem &result, const elem &a) const
  {
    result = -a;
  }

  void subtract(elem &result, const elem &a, const elem &b) const
  {
    result = a-b;
  }

  void mult(elem &result, const elem &a, const elem &b) const
  {
    result = a*b;
  }

  void divide(elem &result, const elem &a, const elem &b) const
  {
    result = a/b;
  }

  void to_ring_elem(ring_elem &result, const elem &a) const
  {
    mpz_ptr r = globalZZ->new_elem();
    ntl_ZZ_to_mpz(r, a);
    result = r;
  }

  void from_ring_elem(elem &result, const ring_elem &a) const
  {
    result = ntl_ZZ_from_mpz(a.get_mpz());
  }

  void swap(elem &a, elem &b) const
  {
    NTL::swap(a,b);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
