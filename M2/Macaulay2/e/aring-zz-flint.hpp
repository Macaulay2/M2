// Copyright 2013 Michael E. Stillman

#ifndef _aring_zz_flint_hpp_
#define _aring_zz_flint_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include <iosfwd>

#if not defined(HAVE_FLINT)  
namespace M2 {
  class ARingZZ : public DummyRing
  {
  public:
    static const RingID ringID = ring_ZZZ;
    typedef M2::ARingZZ ring_type ;
    
    ARingZZ() {};
  };
};
#else

#include <flint/arith.h>

namespace M2 {
  /**
     @ingroup rings
     
     @brief wrapper for the flint fmpz_t integer representation
  */
  
  class ARingZZ : public RingInterface
  {
  public:
    static const RingID ringID = ring_ZZZ;
    
    typedef fmpz_t ElementType;
    typedef ElementType elem;

    ARingZZ();
  public:
    // ring informational
    size_t characteristic() const { return 0; }
    
    size_t cardinality() const { return static_cast<size_t>(-1); }
    
    /** @name IO
        @{ 
    */
    void text_out(buffer &o) const { o << "ZZFlint"; }
    
    void elem_text_out(buffer &o, 
                       const  ElementType a,
                       bool p_one=true, 
                       bool p_plus=false, 
                       bool p_parens=false) const;
    /** @} */
    
    /** @name properties
        @{     
    */
    
    bool is_unit(const ElementType f) const ;
    bool is_zero(const ElementType f) const ;
    
    /** @} */
    
    
    /** @name translation functions
        @{ */
    
    void to_ring_elem(ring_elem &result, const ElementType &a) const
    {
    }
    
    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
    }
    
    /** @} */
    
    /** @name operators
        @{ */
    
    bool is_equal(const ElementType f,const ElementType g) const;
    int compare_elems(const ElementType f,const ElementType g) const;
    /** @} */
    
    /** @name init_set
        @{ */
    
    void init_set(elem &result, elem a) const {fmpz_init_set(result, a);}
    
    void set(elem &result, elem a) const {fmpz_set(result, a);}
    
    void init(elem &result) const ;
    
    void clear(elem &result) const ;
    
    void set_zero(elem &result) const ;
    
    void copy(elem &result,const elem a) const ;
    
    void set_from_int(elem &result, int a) const ;
    
    void set_from_mpz(elem &result,const mpz_ptr a) const ;
    
    void set_from_mpq(elem &result,const mpq_ptr a) const ;
    
    bool set_from_BigReal(elem &result, gmp_RR a) const { return false; }
    
    void set_var(elem &result, int v) const;
    
    /** @} */
    
    
    /** @name arithmetic
        @{ */
    void negate(elem &result,const elem a) const;
    
    void invert(elem &result,const elem a) const;
    
    void add(elem &result, const elem a,const elem b) const;
    
    void subtract(ElementType &result,const  ElementType a,const  ElementType b) const;
    
    void subtract_multiple(elem &result,const  elem a,const  elem b) const;
    
    void mult(elem &result,const  elem a,const  elem b) const;
    
    ///@brief test doc
    void divide(elem &result,const  elem a,const  elem b) const;
    
    void power(elem &result,const  elem a,const int n) const;
    
    void power_mpz(elem &result,const  elem a,const  mpz_ptr n) const;
    
    void syzygy(const ElementType a, const ElementType b,
                ElementType &x, ElementType &y) const;
    /** @} */
    
    
    /** @name misc
        @{ */
    void swap(ElementType &a, ElementType &b) const;
    
    void random(ElementType &result) const;
    /** @} */
    
    bool promote(const Ring *Rf, const ring_elem f, elem &result) const;
    
    bool lift(const Ring *Rg, const elem f, ring_elem &result) const;
    
    // map : this --> target(map)
    //       primelem --> map->elem(first_var)
    // evaluate map(f)
    void eval(const RingMap *map, const elem f, int first_var, ring_elem &result) const;
    
  };
};


#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
