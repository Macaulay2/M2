// Copyright 2013 Michael E. Stillman

#ifndef _aring_QQ_flint_hpp_
#define _aring_QQ_flint_hpp_

#include <iosfwd>

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
//#include <flint/arith.h>
#include <flint/fmpq.h>
#pragma GCC diagnostic pop

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"

#include "exceptions.hpp"

// promote needs ring.hpp.  After moving promote out, remove it here!
#include "ring.hpp"

namespace M2 {
  /**
     @ingroup rings
     
     @brief wrapper for the flint fmpq_t integer representation
  */
  
  class ARingQQFlint : public RingInterface
  {
  public:
    static const RingID ringID = ring_QQFlint;
    
    typedef fmpq ElementType;
    typedef ElementType elem;

    ARingQQFlint();
    ~ARingQQFlint();
  public:
    // ring informational
    size_t characteristic() const { return 0; }
    
    size_t cardinality() const { return static_cast<size_t>(-1); }

    unsigned int computeHashValue(const ElementType& a) const 
    { 
      unsigned long numhash = fmpz_get_ui(fmpq_numref(&a));
      unsigned long denhash = fmpz_get_ui(fmpq_denref(&a));
      return static_cast<unsigned int>(13253 * numhash + 7647 * denhash);
    }
    
    /** @name properties
        @{     
    */
    
    bool is_pm_one(const ElementType& f) const {
      return fmpq_is_one(&f) or ((fmpz_cmp_si(fmpq_numref(&f), -1) == 0) and fmpz_is_one(fmpq_denref(&f)));
    }
    bool is_unit(const ElementType& f) const { return not is_zero(f); }

    bool is_zero(const ElementType& f) const {return fmpq_is_zero(&f);}
    
    /** @} */
    
    
    /** @name operators
        @{ */
    
    bool is_equal(const ElementType& f,const ElementType& g) const {return fmpq_equal(&f,&g);}
    int compare_elems(const ElementType& f,const ElementType& g) const {
      int cmp = fmpq_cmp(&f,&g);
      if (cmp > 0) return 1;
      if (cmp < 0) return -1;
      return 0;
    }
    /** @} */
    
    /** @name init_set
        @{ */
    
    void init_set(ElementType& result, const ElementType& a) const {
      fmpq_init(&result);
      fmpq_set(&result, &a);
    }

    void init(ElementType& result) const {fmpq_init(&result);}

    void clear(ElementType& result) const {fmpq_clear(&result);}
    
    void set(ElementType& result, const ElementType& a) const {fmpq_set(&result, &a);}
    
    void set_zero(ElementType& result) const {fmpq_zero(&result);}
    
    void set_from_long(ElementType& result, long a) const {fmpq_set_si(&result, a, 1);}
    
    void set_from_mpz(ElementType& result,const mpz_ptr a) const {
      // printf("ARingQQFlint::calling set_from_mpz\n");
      fmpz_set_mpz(fmpq_numref(&result), a);
      fmpz_one(fmpq_denref(&result));
    }
    
    void set_from_mpq(ElementType& result,const mpq_ptr a) const { 
      // printf("ARingQQFlint::calling set_from_mpq\n");
      fmpq_set_mpq(&result, a); 
    }
    
    bool set_from_BigReal(ElementType& result, gmp_RR a) const {return false;}
    
    void set_var(ElementType& result, int v) const {fmpq_set_si(&result,1,1);}
    
    /** @} */
    
    
    /** @name arithmetic
        @{ */
    void negate(ElementType& result,const ElementType& a) const {fmpq_neg(&result,&a);}
    
    bool invert(ElementType& result,const ElementType& a) const {
      if (is_unit(a)) 
        { 
          fmpq_inv(&result, &a); 
          return true;
        }
      set_zero(result);
      return false;
    }
    
    void add(ElementType& result, const ElementType& a, const ElementType& b) const {fmpq_add(&result,&a,&b);}
    
    void subtract(ElementType& result, const ElementType& a, const ElementType& b) const {fmpq_sub(&result,&a,&b);}
    
    void subtract_multiple(ElementType& result, const ElementType& a, const ElementType& b) const {fmpq_submul(&result,&a,&b);}
    
    void mult(ElementType& result, const ElementType& a, const ElementType& b) const {fmpq_mul(&result,&a,&b);}
    
    ///@brief test doc
    bool divide(ElementType& result, const ElementType& a, const ElementType& b) const {
      if (is_zero(b)) return false;
      fmpq_div(&result, &a, &b);
      return true;
    }
    
    void power(ElementType& result, const ElementType& a, long n) const {
      M2_ASSERT(n >= 0);
      return fmpq_pow_si(&result,&a,n);
    }
    
    void power_mpz(ElementType& result,const  ElementType& a,const mpz_ptr n) const {
      if (mpz_fits_slong_p(n))
        fmpq_pow_si(&result,&a,mpz_get_si(n));
      else
        throw exc::engine_error("attempted to take a power of an integer to too large of a power");
    }
    
    void syzygy(const ElementType& a, const ElementType& b,
                ElementType& x, ElementType& y) const;
    /** @} */
    
    
    /** @name misc
        @{ */
    void swap(ElementType& a, ElementType& b) const {fmpq_swap(&a,&b);}
    
    void random(ElementType& result) const {fmpq_randtest(&result, mRandomState, mMaxHeight);}
    /** @} */


    /** @name IO
        @{ 
    */
    void text_out(buffer &o) const { o << "QQFlint"; }
    
    void elem_text_out(buffer &o, 
                       const ElementType& a,
                       bool p_one=true, 
                       bool p_plus=false, 
                       bool p_parens=false) const;
    /** @} */

    /** @name translation functions
        @{ */
    
    void to_ring_elem(ring_elem &result, const ElementType& a) const
    {
      gmp_QQ b = getmemstructtype(gmp_QQ);
      mpq_init(b);
      fmpq_get_mpq(b,&a);
      result.poly_val = reinterpret_cast<Nterm*>(b);
    }
    
    void from_ring_elem(ElementType& result, const ring_elem &a) const
    {
      // Currently, until QQ becomes a ConcreteRing, elements of QQ are gmp_QQ (aka mpq_t)
      gmp_QQ t = reinterpret_cast<gmp_QQ>(const_cast<Nterm*>(a.poly_val));
      fmpq_set_mpq(&result, t);
    }
    
    /** @} */
    
    bool promote(const Ring *Rf, const ring_elem f, ElementType& result) const {
      // printf("ARingQQFlint::calling promote\n");
      // Rf = ZZ ---> QQ
      if (Rf->is_ZZ())
        {
          set_from_mpz(result, f.get_mpz());
          return true;
        }
      return false;
    }
    
    bool lift(const Ring *Rg, const ElementType& f, ring_elem &result) const {
      // printf("ARingQQFlint::calling lift\n");
      return false;
    }
    
    // map : this --> target(map)
    //       primelem --> map->elem(first_var)
    // evaluate map(f)
    void eval(const RingMap *map, const ElementType& f, int first_var, ring_elem &result) const;
    
  private:
    mutable flint_rand_t mRandomState;
    long int mMaxHeight;
  };
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
