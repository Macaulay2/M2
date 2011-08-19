// Copyright 2011 Michael E. Stillman

#ifndef _aring_gf_hpp_
#define _aring_gf_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include <iostream>

#if defined(HAVE_FFLAS_FFPACK) && defined(HAVE_GIVARO)
#include <givaro/givgfq.h>
#include <givaro/givpower.h>
#include <givaro/givtimer.h>



namespace M2 {
  class ARingGF : RingInterface
  {

	
  public:
	static const RingID ringID = ring_GF;

 	typedef Givaro::GFqDom<long> FieldType;
	typedef FieldType::Element ElementType;
    	typedef ElementType elem;

	typedef  FieldType::Residu_t UTT; // types depends on FieldType definition!
	typedef Signed_Trait<FieldType::Residu_t>::signed_type STT;// types depends on FieldType definition!

    
   
    ARingGF( UTT charac_,   UTT dimension_);

  private:

  const FieldType givaroField;

 UTT charac;
UTT dimension; ///< same as extensionDegree
    //  int p1; // p-1
    // int minus_one;
    // int prim_root; // element we will use for our primitive root

  public:
    // ring informational
   UTT	 characteristic() const { return charac; }

	/** @name IO
	@{ */
    		void text_out(buffer &o) const { o << "GF(" << charac << "," << dimension << ")"; }
		  void elem_text_out(buffer &o, 
		       ElementType a, 
		       bool p_one, 
		       bool p_plus, 
		       bool p_parens) const;

	/** @} */


	/** @name properties
	@{ */
		bool is_unit(ElementType f) const ;
		bool is_zero(ElementType f) const ;
	/** @} */


	/** @name operators
	@{ */
		bool is_equal(ElementType f, ElementType g) const 	;
		
		int compare_elems(ElementType f, ElementType g) const ;
	/** @} */


	/** @name get functions
		@{ */
		int get_int(elem f) const ;
		int get_repr(elem f) const ;
	/** @} */


	/** @name init_set
	@{ */
		void init(elem &result) const 		;
	
		void clear(elem &result) const 		;
	
		void set_zero(elem &result) const ;
		
		void copy(elem &result, elem a) const ;
		
		void set_from_int(elem &result, int a) const ;

		void set_from_mpz(elem &result, mpz_ptr a) const ;
		
		void set_from_mpq(elem &result, mpq_ptr a) const ;
	/** @} */


	/** @name arithmetic
	@{ */
		void negate(elem &result, elem a) const;
		
		void invert(elem &result, elem a) const;
		
		void add(elem &result, elem a, elem b) const;
		
		void subtract(ElementType &result, ElementType a, ElementType b) const;
		
		void subtract_multiple(elem &result, elem a, elem b) const;
		
		void mult(elem &result, elem a, elem b) const;
		
		void divide(elem &result, elem a, elem b) const;
		
		void power(elem &result, elem a, STT n) const;
		
		void power_mpz(elem &result, elem a, mpz_ptr n) const;
		
	   	void syzygy(ElementType a, ElementType b,
				ElementType &x, ElementType &y) const;
	/** @} */


	/** @name misc
	@{ */
    		void swap(ElementType &a, ElementType &b) const;

    		void random(ElementType &result) const;
	/** @} */
  };
  
};


#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
