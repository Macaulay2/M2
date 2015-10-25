// Copyright 2013 Michael E. Stillman

#ifndef _aring_example_hpp_
#define _aring_example_hpp_

#include "aring.hpp"

// This file documents what functions need to be present in an "ARing" type
// so that it can be used with DMat, SMat, ConcreteRing

namespace M2 {
  /**
     @ingroup rings
     
     @brief example interface, with contract
  */
  
  class ARingExample : public RingInterface
  {
  public:
    // This is a unique id for your type.
    // The list of all such is in aring.hpp
    static const RingID ringID = ring_EXAMPLE;

     // ElementType can be any type you need.
    typedef long ElementType;

    // The following type 'elem' should be deprecated or removed.
    // But until then, this is needed.
    typedef ElementType elem;

    // The constructor.  Can take arguments.
    ARingExample();
  public:
    // ring informational

    // Should return either 0 or a prime.
    // What about things like ZZ/n ?  Should it return 0 or n?
    // An unfortunate assumption here is that the characteristic 
    // will fit into a size_t.
    size_t characteristic() const { return 0; }

    // If the size is too large too place in a size_t, then return the
    // largest size_t number (static_cast<size_t>(-1)).
    size_t cardinality() const { return static_cast<size_t>(-1); }

    // Hash values are used in the front end of M2 to implement keys for hash tables.
    // It is worthwhile to have this be a meaningful hash function
    // (But doesn't need to be cryptographic!)
    unsigned int computeHashValue(const ElementType& a) const { return a; }
    
    /** @name properties
        @{     
    */

    // for fields, is_unit and is_zero are the same (up to negation)
    // for approximate fields, should it be within an epsilon of 0?
    bool is_unit(const ElementType& f) const ;
    bool is_zero(const ElementType& f) const ;
    
    /** @} */
    
    
    /** @name translation functions
        @{ */

    // Eventually, these two routines will not be needed.
    // Right now though: they should COPY the result, keeping
    // the original element constant.
    void to_ring_elem(ring_elem &result, const ElementType& a) const
    {
    }
    
    void from_ring_elem(ElementType& result, const ring_elem &a) const
    {
    }
    
    /** @} */
    
    /** @name operators
        @{ */
    
    bool is_equal(const ElementType& f,const ElementType& g) const;

    // This is needed for general M2 comparison.  However, it is only
    // used for sorting elements, so is not really important
    int compare_elems(const ElementType& f,const ElementType& g) const;
    /** @} */
    
    /** @name init_set
        @{ */

    // The general interface is like gmp or mpir: one needs to "init"
    // an element first, then after you are done, it needs to be "clear"ed
    // Other routines require an ElementType that has been so initialized.
    // Below, each routine says what it requires about it.    

    // This first one intializes an element to be used, to the 0 element.  
    // In gmp, it is not set to 0, but here, we make sure it is initialized to a 0 value.
    void init(ElementType& result) const {fmpz_init(result);}

    // Do we really need this?
    void init_set(ElementType& result, ElementType& a) const {fmpz_init_set(result, a);}

    // This is the only routine to clear the space associated to an ElementType
    void clear(ElementType& result) const {fmpz_clear(result);}

    void set(ElementType& result, ElementType& a) const {fmpz_set(result, a);}
    
    void set_zero(ElementType& result) const {fmpz_set_si(result, 0);}
    
    void copy(ElementType& result,const ElementType& a) const {fmpz_set(result, a);}

    // The following functions coerce from other types.
    // mpz ints are used in the front end, so that is why
    // we use those type here.
    void set_from_long(ElementType& result, long a) const {fmpz_set_si(result, a);}
    void set_from_mpz(ElementType& result,const mpz_ptr a) const {fmpz_set_mpz(result,a);}

    // Set from a rational number.
    // What should this function do, if such an element cannot coerced in?
    // Answer: it returns 'false' if the element cannot be moved in.
    // e.g: to ZZ: if the denom is non-zero, return false
    //      to ZZ/p: if the denom is divisible by p, then return false
    // 
    bool set_from_mpq(ElementType& result,const mpq_ptr a) const;

    // set from an arbitrary precision gmp real number.
    // LEAVE OUT if not needed
    bool set_from_BigReal(ElementType& result, gmp_RR a) const { return false; }

    // set from an arbitrary precision gmp complex number.
    // LEAVE OUT if not needed
    bool set_from_BigComplex(elem &result, gmp_CC a) const { return false; }    

    // If the ring has variables, set 'result' to that variable.
    // If not, false whould be returned.
    bool set_var(ElementType& result, int v) const {fmpz_set_si(result,1);;}
    
    /** @} */
    
    
    /** @name arithmetic
        @{ */
    void negate(ElementType& result,const ElementType& a) const;
    
    void add(ElementType& result, const ElementType& a,const ElementType& b) const;
    
    void subtract(ElementType& result,const  ElementType& a,const  ElementType& b) const;
    
    void subtract_multiple(ElementType& result,const ElementType& a,const ElementType& b) const;
    
    void mult(ElementType& result,const  ElementType& a,const  ElementType& b) const;

    void power(ElementType& result,const  ElementType& a,const int n) const;

    // If the ring is such that taking a large power, larger than a long,
    // doesn't make sense, then throw an error (or set ERROR, set 'result' to 0
    void power_mpz(ElementType& result,const  ElementType& a,const  mpz_ptr n) const;

    // If 'a' is not invertible, then return false, don't touch result.
    bool invert(ElementType& result,const ElementType& a) const;
    
    ///@brief test doc
    // This is exact division.  If b does not divide a, then do not set
    // result, and return false.
    // Otherwise:  result = a/b.
    bool divide(ElementType& result,const  ElementType& a,const  ElementType& b) const;
    
    void syzygy(const ElementType& a, const ElementType& b,
                ElementType& x, ElementType& y) const;
    /** @} */
    

    /** @name IO
        @{ 
    */
    void text_out(buffer &o) const { o << "ARingExample"; }
    
    void elem_text_out(buffer &o, 
                       const  ElementType& a,
                       bool p_one=true, 
                       bool p_plus=false, 
                       bool p_parens=false) const;
    /** @} */
    
    
    /** @name misc
        @{ */

    // Do we need this one??
    void swap(ElementType& a, ElementType& b) const;

    // This one should take a 'random' object
    void random(ElementType& result) const;
    /** @} */


    // The following three functions are used for inter-operability with
    // "legacy" ring code.
    bool promote(const Ring *Rf, const ring_elem f, ElementType& result) const;
    
    bool lift(const Ring *Rg, const ElementType& f, ring_elem &result) const;
    
    // map : this --> target(map)
    //       primelem --> map->elem(first_var)
    // evaluate map(f)
    void eval(const RingMap *map, const ElementType& f, int first_var, ring_elem &result) const;
    
  };
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
