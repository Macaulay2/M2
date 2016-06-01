// Copyright 2012 Michael E. Stillman

#ifndef _aring_RR_hpp_
#define _aring_RR_hpp_

#include "exceptions.hpp"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "ringmap.hpp"

class RingMap;

namespace M2 {
/**
\ingroup rings
*/
  class ARingRR : public RingInterface
  {
    // approximate real numbers, implemented as doubles.
  public:
    static const RingID ringID = ring_RR;

    typedef double elem;
    typedef elem ElementType;

    ARingRR() {}

    // ring informational
    size_t characteristic() const { return 0; }
    unsigned long get_precision() const { return 53; }
    void text_out(buffer &o) const;

    unsigned int computeHashValue(const elem& a) const 
    { 
      return static_cast<unsigned int>(a);
    }

    /////////////////////////////////
    // ElementType informational ////
    /////////////////////////////////

    bool is_unit(const ElementType& f) const { 
      return !is_zero(f); 
    }

    bool is_zero(const ElementType& f) const {
      return f == 0.0;
    }
    
    bool is_equal(const ElementType& f, const ElementType& g) const { 
      return f == g;
    }

    int compare_elems(const ElementType& f, const ElementType& g) const {
      double cmp = f - g;
      if (cmp < 0) return -1;
      if (cmp > 0) return 1;
      return 0;
    }

    ////////////////////////////
    // to/from ringelem ////////
    ////////////////////////////
    // These simply repackage the element as either a ringelem or an 'ElementType'.
    // No reinitialization is done.
    // Do not take the same element and store it as two different ring_elem's!!
    void to_ring_elem(ring_elem &result, const ElementType &a) const
    {
      double* res = newitem(double);
      *res = a;
      result.poly_val = reinterpret_cast<Nterm*>(res);
    }

    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
      result = * reinterpret_cast<double*>(a.poly_val);
    }

    // 'init', 'init_set' functions

    void init(ElementType &result) const { 
      result = 0.0;
    }

    void init_set(ElementType &result, const ElementType& a) const { 
      result = a;
    }

    void set(ElementType &result, const ElementType& a) const { 
      result = a;
    }

    void set_zero(ElementType &result) const { 
      result = 0.0;
    }

    void clear(ElementType &result) const { 
      // do nothing
    }

    void copy(ElementType &result, const ElementType& a) const { 
      set(result,a); 
    }

    void set_from_long(ElementType &result, long a) const {
      result = static_cast<double>(a);
    }

    void set_var(ElementType &result, int v) const { 
      result = 1.0;
    }

    void set_from_mpz(ElementType &result, mpz_ptr a) const {
      result = mpz_get_d(a);
    }

    void set_from_mpq(ElementType &result, const mpq_ptr a) const {
      result = mpq_get_d(a);
    }

    bool set_from_BigReal(ElementType &result, gmp_RR a) const {
      result = mpfr_get_d(a, GMP_RNDN);
      return true;
    }
    bool set_from_double(ElementType &result, double a) const {
      result = a;
      return true;
    }


    // arithmetic
    void negate(ElementType &result, const ElementType& a) const
    {
      result = -a;
    }

    void invert(ElementType &result, const ElementType& a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
      result = 1.0/a;
    }

    void add(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result = a+b;
    }

    void addMultipleTo(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result += a*b;
    }

    void subtract(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result = a-b;
    }

    void subtract_multiple(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result -= a*b;
    }

    void mult(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result = a*b;
    }

    void divide(ElementType &result, const ElementType& a, const ElementType& b) const
    {
      result = a/b;
    }

    void abs_squared(ElementType& result, const ElementType& a) const
    {
      result = a*a;
    }

    void abs(ElementType& result, const ElementType& a) const
    {
      result = fabs(a);
    }

    void power(ElementType &result, const ElementType& a, int n) const
    {
      result = pow(a,n);
    }

    void power_mpz(ElementType &result, const ElementType& a, mpz_ptr n) const
    {
      if(mpz_fits_sint_p(n)) {
        int m = static_cast<int>(mpz_get_si(n)); 
        power(result,a,m);
      } else throw exc::engine_error("expected small integer");
    }

    void swap(ElementType &a, ElementType &b) const
    {
      std::swap(a,b);
    }

    void elem_text_out(buffer &o,
                       const ElementType &a,
                       bool p_one=true,
                       bool p_plus=false,
                       bool p_parens=false) const;

    void syzygy(const ElementType& a, const ElementType& b,
                ElementType &x, ElementType &y) const // remove?
    // returns x,y s.y. x*a + y*b == 0.
    // if possible, x is set to 1.
    // no need to consider the case a==0 or b==0.
    {
      // TODO: remove this?
      set_var(x,0); //set x=1
      if(!is_zero(b))
        {
          set(y,a);
          negate(y,y);
          divide(y,y,b);
        }
    }

    void random(ElementType &result) const // redo?
    {
      mpfr_t val;
      mpfr_init2(val, 53);
      rawRandomMpfr(val, 53);
      result = mpfr_get_d(val, GMP_RNDN);
      mpfr_clear(val);
    }

    void eval(const RingMap *map, ElementType &f, int first_var, ring_elem &result) const
    {
      map->get_ring()->from_double(f, result);
    }

    void zeroize_tiny(gmp_RR epsilon, ElementType &a) const
    {
      if (mpfr_cmp_d(epsilon, fabs(a)) > 0)
        set_zero(a);
    }

    void increase_norm(gmp_RR& norm, const ElementType& a) const
    {
      double d = fabs(a);
      if (mpfr_cmp_d(norm, d) < 0)
        mpfr_set_d(norm, d, GMP_RNDN);
    }

    double coerceToDouble(const ElementType& a) const
    {
      return a;
    }
    
  };

}; // end namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
