// Copyright 2011 Michael E. Stillman

#include "aring-ffpack.hpp"
#include "error.h"

#if defined(HAVE_FFLAS_FFPACK)

#include "ringmap.hpp"

namespace M2 {



  ARingZZpFFPACK::ARingZZpFFPACK(	 UTT charact_ )  :   
                                        mFfpackField( FieldType((double)charact_) ),
                                        mFfpackRandomIterator(mFfpackField),
                                        mCharac(charact_),
                                        mDimension(1),
                                        generatorComputed_m(false)
                                        //mGenerator(computeGenerator())
  {
    assert( FieldType::getMaxModulus()>=mCharac );
  }
 
/// @mike correct output : print generator variable of the ring instead of 'X', whatever generator variable will be 
        void ARingZZpFFPACK::elem_text_out(buffer &o, 
        const ElementType  elem, 
        bool p_one, 
        bool p_plus, 
        bool p_parens) const
{
    o << (STT)elem; // todo: modulo reduction ?
}

  /**  @todo Remove this function?
   //  http://www.johnkerl.org/doc/ffcomp.pdf
   */

ARingZZpFFPACK::ElementType ARingZZpFFPACK::computeGenerator ( ) const
{
    for (UTT currIntElem=2;currIntElem<mCharac; currIntElem++)
    {
        ElementType currElem;
        set_from_int(currElem,currIntElem);
        bool found = true;
        ElementType tmpElem=currElem;
        for (UTT count=0;count<mCharac-2; count++)
        {
            mult(tmpElem,tmpElem,currElem);
            if (is_equal(currElem,tmpElem))
                found = false;
        }
        if (found) 
        {
            std::cerr << "generator = " << currElem << std::endl;
            return currElem;
        }
    }
    assert(false);
    return ElementType(1);    
}

        
        
bool ARingZZpFFPACK::is_unit(const ElementType f) const 	
    {   return ! mFfpackField.isZero(f); }

bool ARingZZpFFPACK::is_zero(const ElementType f) const 	
    {   return mFfpackField.isZero(f); }

bool ARingZZpFFPACK::is_equal(const ElementType f, const ElementType g) const 	
{   return	mFfpackField.areEqual(f,g); }


/// compare exponents of the used generator 
/// @return -1: f < g, 1: f>g; 0: f==g;
int ARingZZpFFPACK::compare_elems(const ElementType f, const ElementType g) const 
{
    //std::cerr << "ARingZZpFFPACK::compare_elems" << std::endl;
    if (f < g) return -1; 
    if (f > g) return 1;

    return 0;
}


///@todo how should this function behave? 
///@todo also consider conversion problems depending on 'elem' type
/// @todo: review return type. 
int ARingZZpFFPACK::get_int(const ElementType f) const 
{
    std::cerr << "ARingZZpFFPACK::get_int" << std::endl;
    return static_cast<int>(f);
}


/// @todo where this function will be used? ; 
/// @todo problems if type 'elem' is bigger than  int
int ARingZZpFFPACK::get_repr(const ElementType f) const 
{
    std::cerr << "ARingZZpFFPACK::get_repr" << std::endl;
    return static_cast<int>(f); 
}

    // 'init', 'init_set' functions

    void ARingZZpFFPACK::init(ElementType &result) const                { result = 0;  } //{ result = mFfpackField.zero;  }

    void ARingZZpFFPACK::clear(ElementType &result) const             { /* nothing */ }

    void ARingZZpFFPACK::set_zero(ElementType &result) const         { result = 0;  } //{ result = mFfpackField.zero;  }

    void ARingZZpFFPACK::copy(ElementType &result, const ElementType a) const { result = a; }


/// @todo possible problem if type UTT is smaller than an int?
void ARingZZpFFPACK::set_from_int(ElementType &result, int a) const 
{
    //std::cerr << "ARingGF::set_from_int" << std::endl;
    mFfpackField.init(result, a);
}

    void ARingZZpFFPACK::set_from_mpz(ElementType &result, const mpz_ptr a) const 
    {
        //std::cerr << "set_from_mpz" << std::endl;
        UTT b = static_cast< UTT>(mpz_fdiv_ui(a, mCharac));
       // std::cerr << "b " << b << std::endl;
        mFfpackField.init(result,  b);
       // std::cerr << "result " << result << std::endl;
    }

    void ARingZZpFFPACK::set_from_mpq(ElementType &result, const mpq_ptr a) const {
      //  std::cerr << "set_from_mpq" << std::endl;
        ElementType n, d;
        set_from_mpz(n, mpq_numref(a));
        set_from_mpz(d, mpq_denref(a));
        divide(result,n,d);
    }

    // arithmetic
    void ARingZZpFFPACK::negate(ElementType &result, const ElementType a) const
    {
        mFfpackField.neg(result,a);
    }

    /// if a is zero, the result is 1 , but is that what we expect?
    /// I vote for two invert functions, one with this check and one without.(Jakob)
    void ARingZZpFFPACK::invert(ElementType &result, const ElementType a) const
    {
       // std::cerr << "ARingGF::invert" << std::endl;
        if ( mFfpackField.isZero(a))
            ERROR(" division by zero");
        mFfpackField.inv(result,a);
    }


    void ARingZZpFFPACK::add(ElementType &result, const ElementType a, const ElementType b) const
    {
        mFfpackField.add(result,a,b);
    }

    void ARingZZpFFPACK::subtract(ElementType &result, const ElementType a, const ElementType b) const
    {
        mFfpackField.sub(result,a,b);
    }

    /// @param c[in][out] c = c- a*b
    void ARingZZpFFPACK::subtract_multiple(ElementType &c, const ElementType a, const ElementType b) const
    {
       ///@todo negin was added in later ffpack revision
      ElementType nega=a ;
      mFfpackField.negin(nega);
        mFfpackField. axpyin(c,nega,b);
    }

    void ARingZZpFFPACK::mult(ElementType &result, const ElementType a, const ElementType b) const
    {
        mFfpackField.mul(result,a,b);
    }

    void ARingZZpFFPACK::divide(ElementType &result, const ElementType a, const ElementType b) const
    {
        if ( mFfpackField.isZero(b))
           ERROR(" division by zero");
        mFfpackField.div(result,a,b);
    }

  /// @jakob: overflow can be occured due to multiplication. use exact mpz for multiply and modulo operation instead!
  /// @jakob  should 'power' be implemented 
  /// @todo: use a different algorithm for power.  Once division by mCharac-1 is done, use divide by 2 method
  ///
    void ARingZZpFFPACK::power(ElementType &result, const ElementType a, const  STT n) const
    {
        if (! mFfpackField.isZero(a)) 
        {
            mpz_t  mpz_n;
            mpz_t  mpz_tmp;
            mpz_init( mpz_n);
            mpz_init( mpz_tmp);
            mpz_set_si   (mpz_n,n);
            //std::cerr << "n = " << n << std::endl;
            //std::cerr << "mpz_n = " << mpz_n << std::endl;
             STT tmp  = static_cast< STT>(mpz_fdiv_r_ui(mpz_tmp, mpz_n, mCharac -1)  );
            if ( tmp==0 )
            {
                //result = mFfpackField.one;
                result = 1.;
                return;
                // result=givaroField.one;
            }
             
            //std::cerr << "tmp = " << tmp << std::endl;
            assert(tmp>0); // tmp<0 should never occur
            result = 1.;
            while (tmp>0) 
            {
              mFfpackField.mulin(result,a);
              tmp--;
            }
        }
        else
        {
            if (n<0)
                 ERROR(" division by zero");
            result = a;
        }
    }

    /// @pre ensure that  mFfpackField.cardinality() fits in a unsigned long, otherwise instead of mpz_fdiv_ui a different function has to be called)
    void ARingZZpFFPACK::power_mpz(ElementType &result, const  ElementType a, const  mpz_ptr n) const
    {
        STT n1 = static_cast< STT>(mpz_fdiv_ui(n, mFfpackField.cardinality()-1));

        //std::cerr << "exponent = " << n << std::endl;
        //std::cerr << "n1 = " << n1 << std::endl;
        power(result,a,n1);
    }

    ///@note dublicate code
    void ARingZZpFFPACK::swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }



    /** @brief returns x,y  s.y.  x*a + y*b == 0.
       if possible, x is set to 1.
       no need to consider the case a==0 or b==0.
    */

    void ARingZZpFFPACK::syzygy(const ElementType a, const ElementType b,
                        ElementType &x, ElementType &y) const
   
    {
     // x = mFfpackField.one;
       x = 1.;
      divide(y,a,b);
      negate(y,y);
    }

    /// @jakob: document possible overflow and other nasty things
    void ARingZZpFFPACK::random(ElementType &result) const
    {
        mFfpackRandomIterator.random(result);
    }

  void ARingZZpFFPACK::eval(const RingMap *map, const elem f, int first_var, ring_elem &result) const
  {
    // translate f to 
    int a = static_cast<int>(f);  //TODO: JAKOB: f is an element in this FFPACK ring.  How to get its integer value (an element in range [0, characteristic()-1) ?
    result = map->get_ring()->from_int(a);
  }

};

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
