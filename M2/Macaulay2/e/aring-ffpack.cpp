// Copyright 2011 Michael E. Stillman

#include "aring-ffpack.hpp"
#include "error.h"

#if defined(HAVE_FFLAS_FFPACK)


namespace M2 {



  ARingFFPACK::ARingFFPACK(	 UTT charact_ )  :   charac(charact_),
                                        dimension(1),
                                        ffpackField( FieldType((double)charact_) ),
                                        ffpackRandomIterator(ffpackField),
                                        generator(computeGenerator())
  {
    assert( FieldType::getMaxModulus()>=charact );
  }
 
/// @mike correct output : print generator variable of the ring instead of 'X', whatever generator variable will be 
        void ARingFFPACK::elem_text_out(buffer &o, 
        const ElementType  elem, 
        bool p_one, 
        bool p_plus, 
        bool p_parens) const
{
    o << (STT)elem; // todo: modulo reduction ?
}

//  http://www.johnkerl.org/doc/ffcomp.pdf
ARingFFPACK::ElementType ARingFFPACK::computeGenerator ( ) const
 {
            
      for (UTT currIntElem=2;currIntElem<charac; currIntElem++)
      {
        ElementType currElem;
        set_from_int(currElem,currIntElem);
        bool found = true;
        ElementType tmpElem=currElem;
        for (UTT count=0;count<charac-2; count++)
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
}
        
        
bool ARingFFPACK::is_unit(const ElementType f) const 	
    {   return ! ffpackField.isZero(f); }

bool ARingFFPACK::is_zero(const ElementType f) const 	
    {   return ffpackField.isZero(f); }

bool ARingFFPACK::is_equal(const ElementType f, const ElementType g) const 	
{   return	ffpackField.areEqual(f,g); }


/// compare exponents of the used generator 
/// @return -1: f < g, 1: f>g; 0: f==g;
int ARingFFPACK::compare_elems(const ElementType f, const ElementType g) const 
{
    //std::cerr << "ARingFFPACK::compare_elems" << std::endl;
    if (f < g) return -1; 
    if (f > g) return 1;

    return 0;
}


///@todo how should this function behave? 
///@todo also consider conversion problems depending on 'elem' type
/// @todo: review return type. 
int ARingFFPACK::get_int(const ElementType f) const 
{
    std::cerr << "ARingFFPACK::get_int" << std::endl;
    return (int)f;
}


/// @todo where this function will be used? ; 
/// @todo problems if type 'elem' is bigger than  int
int ARingFFPACK::get_repr(const ElementType f) const 
{
    std::cerr << "ARingFFPACK::get_repr" << std::endl;
    return f; 
}

    // 'init', 'init_set' functions

    void ARingFFPACK::init(ElementType &result) const                { result = 0;  } //{ result = ffpackField.zero;  }

    void ARingFFPACK::clear(ElementType &result) const             { /* nothing */ }

    void ARingFFPACK::set_zero(ElementType &result) const         { result = 0;  } //{ result = ffpackField.zero;  }

    void ARingFFPACK::copy(ElementType &result, const ElementType a) const { result = a; }


/// @todo possible problem if type UTT is smaller than an int?
void ARingFFPACK::set_from_int(ElementType &result, int a) const 
{
    //std::cerr << "ARingGF::set_from_int" << std::endl;
    ffpackField.init(result, a);
}

    void ARingFFPACK::set_from_mpz(ElementType &result, const mpz_ptr a) const 
    {
        //std::cerr << "set_from_mpz" << std::endl;
        UTT b = static_cast< UTT>(mpz_fdiv_ui(a, charac));
       // std::cerr << "b " << b << std::endl;
        ffpackField.init(result,  b);
       // std::cerr << "result " << result << std::endl;
    }

    void ARingFFPACK::set_from_mpq(ElementType &result, const mpq_ptr a) const {
      //  std::cerr << "set_from_mpq" << std::endl;
        ElementType n, d;
        set_from_mpz(n, mpq_numref(a));
        set_from_mpz(d, mpq_denref(a));
        divide(result,n,d);
    }

    // arithmetic
    void ARingFFPACK::negate(ElementType &result, const ElementType a) const
    {
        ffpackField.neg(result,a);
    }

    /// if a is zero, the result is 1 , but is that what we expect?
    /// I vote for two invert functions, one with this check and one without.(Jakob)
    void ARingFFPACK::invert(ElementType &result, const ElementType a) const
    {
       // std::cerr << "ARingGF::invert" << std::endl;
        if ( ffpackField.isZero(a))
            ERROR(" division by zero");
        ffpackField.inv(result,a);
    }


    void ARingFFPACK::add(ElementType &result, const ElementType a, const ElementType b) const
    {
        ffpackField.add(result,a,b);
    }

    void ARingFFPACK::subtract(ElementType &result, const ElementType a, const ElementType b) const
    {
        ffpackField.sub(result,a,b);
    }

    /// @param c[in][out] c = c- a*b
    void ARingFFPACK::subtract_multiple(ElementType &c, const ElementType a, const ElementType b) const
    {
       ///@todo negin was added in later ffpack revision
      ElementType nega=a ;
      ffpackField.negin(nega);
        ffpackField. axpyin(c,nega,b);
    }

    void ARingFFPACK::mult(ElementType &result, const ElementType a, const ElementType b) const
    {
        ffpackField.mul(result,a,b);
    }

    void ARingFFPACK::divide(ElementType &result, const ElementType a, const ElementType b) const
    {
        if ( ffpackField.isZero(b))
           ERROR(" division by zero");
        ffpackField.div(result,a,b);
    }

   /// @jakob: overflow can be occured due to multiplication. use exact mpz for multiply and modulo operation instead!
   /// @jakob  should 'power' be implemented 
    void ARingFFPACK::power(ElementType &result, const ElementType a, const  STT n) const
    {
        if (! ffpackField.isZero(a)) 
        {
            mpz_t  mpz_n;
            mpz_t  mpz_tmp;
            mpz_init( mpz_n);
            mpz_init( mpz_tmp);
            mpz_set_si   (mpz_n,n);
            //std::cerr << "n = " << n << std::endl;
            //std::cerr << "mpz_n = " << mpz_n << std::endl;
             STT tmp  = static_cast< STT>(mpz_fdiv_r_ui(mpz_tmp, mpz_n, charac -1)  );
            if ( tmp==0 )
            {
                //result = ffpackField.one;
                result = 1.;
                return;
                // result=givaroField.one;
            }
             
            //std::cerr << "tmp = " << tmp << std::endl;
            assert(tmp>0); // tmp<0 should never occur
            result = 1.;
            while (tmp>0) 
            {
              ffpackField.mulin(result,a);
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

    /// @pre ensure that  ffpackField.cardinality() fits in a unsigned long, otherwise instead of mpz_fdiv_ui a different function has to be called)
    void ARingFFPACK::power_mpz(ElementType &result, const  ElementType a, const  mpz_ptr n) const
    {
        STT n1 = static_cast< STT>(mpz_fdiv_ui(n, ffpackField.cardinality()-1));

        //std::cerr << "exponent = " << n << std::endl;
        //std::cerr << "n1 = " << n1 << std::endl;
        power(result,a,n1);
    }

    ///@note dublicate code
    void ARingFFPACK::swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }



    /** @brief returns x,y  s.y.  x*a + y*b == 0.
       if possible, x is set to 1.
       no need to consider the case a==0 or b==0.
    */

    void ARingFFPACK::syzygy(const ElementType a, const ElementType b,
                        ElementType &x, ElementType &y) const
   
    {
     // x = ffpackField.one;
       x = 1.;
      divide(y,a,b);
      negate(y,y);
    }

    /// @jakob: document possible overflow and other nasty things
    void ARingFFPACK::random(ElementType &result) const
    {
        ffpackRandomIterator.random(result);
    }

};

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
