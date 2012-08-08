// Copyright 2011 Michael E. Stillman

#ifndef _aring_ffpack_hpp_
#define _aring_ffpack_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include <iostream>


//enable this lines to trick Kdevelop and do not forget to comment out them before committing. Other ideas ?
// #define HAVE_FFLAS_FFPACK 1 
// #define HAVE_GIVARO 1

#if not defined(HAVE_FFLAS_FFPACK)  
namespace M2 {

class ARingZZpFFPACK : public DummyRing
  {
     public:
        static const RingID ringID = ring_FFPACK;

       
        typedef M2::ARingZZpFFPACK             ring_type ;

        ARingZZpFFPACK( int charac_ ) {};
  };

};
#else
#include <fflas-ffpack/field/modular-balanced.h>
#include <fflas-ffpack/ffpack/ffpack.h>
#include <givaro/givgfq.h>

namespace M2 {

  /**
     @ingroup rings
     
     @brief wrapper for the  FFPACK::ModularBalanced<double>   field implementation
  */
  
  class ARingZZpFFPACK : public RingInterface
  {
  public:
    static const RingID ringID = ring_FFPACK;
    
    typedef FFPACK::ModularBalanced<double> FieldType;

    typedef FieldType::Element ElementType;
    typedef ElementType elem;

    typedef  uint32_t UTT; ////// attention: depends on STT;currently manual update

    // to use the signed_trait thing, we need givaro....
    typedef Signed_Trait<UTT>::signed_type STT;///< types depends on FieldType definition!

    // if no givaro, use this:
    //typedef  int32_t STT; /// attention: depends on UTT; currently manual update

    // @todo: problem, wenn typ von cHarakteristif 
    ARingZZpFFPACK( UTT charac);

  private:
    const FieldType mFfpackField;
    mutable  FieldType::RandIter     mFfpackRandomIterator;
    
    UTT mCharac;
    UTT mDimension; ///< same as extensionDegree
    
    mutable ElementType generator_m;///< use getGenerator() to access it since generator is cached and not computed if not required.

    mutable bool generatorComputed_m;
    //  int p1; // p-1
    // int minus_one;
    // int prim_root; // element we will use for our primitive root
    
  public:
    // ring informational
    UTT characteristic() const { return mCharac; }

    ElementType getGenerator() const
    {
        if (not generatorComputed_m)
        {
            generator_m = computeGenerator();
            generatorComputed_m=true;
        }
        return generator_m;
    }

    // 
    const FieldType field() const { return mFfpackField; }
    
    /** @name IO
	@{ 
    */
    void text_out(buffer &o) const { o << "ZZpFPACK(" << mCharac << "," << mDimension << ")"; }

    void elem_text_out(buffer &o, 
		       const  ElementType a,
		       bool p_one, 
		       bool p_plus, 
		       bool p_parens) const;
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
        result.int_val = static_cast<int>(a);
      }
    
      void from_ring_elem(ElementType &result, const ring_elem &a) const
      {
        result = a.int_val;
      }

    /** @} */

    /** @name operators
	@{ */

    bool is_equal(const ElementType f,const ElementType g) const    ;
    int compare_elems(const ElementType f,const ElementType g) const ;
    /** @} */

    /** @name get functions
        @{ */
        int get_int(elem f) const ;
        int get_repr(elem f) const ;
    /** @} */


    /** @name init_set
    @{ */

        void init_set(elem &result, elem a) const { result = a; }

        void set(elem &result, elem a) const { result = a; }

        void init(elem &result) const ;

        void clear(elem &result) const ;

        void set_zero(elem &result) const ;

        void copy(elem &result,const elem a) const ;

        void set_from_int(elem &result, int a) const ;

        void set_from_mpz(elem &result,const mpz_ptr a) const ;

        void set_from_mpq(elem &result,const mpq_ptr a) const ;
        
        ElementType computeGenerator ( ) const; 

        void set_var(elem &result, int v) const         { result = getGenerator(); }

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

        void power(elem &result,const  elem a,const  STT n) const;

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
     
    static inline double getMaxModulus() 
    {
      return FieldType::getMaxModulus();
    }
  };

};


#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
