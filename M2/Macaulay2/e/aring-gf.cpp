// Copyright 2011 Michael E. Stillman

#include "aring-gf.hpp"
#include "error.h"

#if defined(HAVE_FFLAS_FFPACK) && defined(HAVE_GIVARO)

namespace M2 {



  ARingGF::ARingGF(	 UTT charact_, 
             UTT extensionDegree_)  :   charac(charact_),
                                        dimension(extensionDegree_),
                                        givaroField(FieldType(charact_, extensionDegree_))
  {

  }

/// @mike correct output : print generator variable of the ring instead of 'X', whatever generator variable will be 
        void ARingGF::elem_text_out(buffer &o, 
        const ElementType a, 
        bool p_one, 
        bool p_plus, 
        bool p_parens) const
  {
    UTT  rep;
    rep=givaroField.convert(rep,a);
    long exp=0;
    if (  rep==0)  o << "0";
    while (rep !=0)
    {
        UTT  remainder = rep%charac;
        rep=rep/charac;
        if (exp >0 )
             o << " + ";
        o << remainder <<"*" << "X^" << exp;
        exp++;
    }
}

bool ARingGF::is_unit(const ElementType f) const 	
    {   return givaroField.isunit(f); }

bool ARingGF::is_zero(const ElementType f) const 	
    {   return givaroField.isZero(f); }

bool ARingGF::is_equal(const ElementType f, const ElementType g) const 	
{   return	givaroField.areEqual(f,g); }


/// compare exponents of the used generator
int ARingGF::compare_elems(const ElementType f, const ElementType g) const 
{
    std::cerr << "ARingGF::compare_elems" << std::endl;
    if (f < g) return -1; 
    if (f > g) return 1;

    return 0;
}


///@todo how should this function behave? 
///@todo also consider conversion problems depending on 'elem' type
int ARingGF::get_int(const ElementType f) const 
{
    
    std::cerr << "ARingGF::get_int" << std::endl;
    assert(false);
    return f; 
}


/// @todo where this function will be used? ; 
/// @todo problems if type 'elem' is bigger than  int
int ARingGF::get_repr(const ElementType f) const 
{
    std::cerr << "get_repr" << std::endl;
    int result;
    givaroField.convert(result,f);
    return result; 
}

    // 'init', 'init_set' functions

    void ARingGF::init(ElementType &result) const              { result = givaroField.zero;  }

    void ARingGF::clear(ElementType &result) const             { /* nothing */ }

    void ARingGF::set_zero(ElementType &result) const          { result = givaroField.zero; }

    void ARingGF::copy(ElementType &result, const ElementType a) const { result = a; }


/// @todo possible problem if type UTT is smaller than an int?
void ARingGF::set_from_int(ElementType &result, int a) const 
{
    //std::cerr << "ARingGF::set_from_int" << std::endl;
    a = a % charac; 
    if (a < 0) a += charac;
    givaroField.init(result, a);
}

    void ARingGF::set_from_mpz(ElementType &result, const mpz_ptr a) const 
    {
        //std::cerr << "set_from_mpz" << std::endl;
        UTT b = static_cast< UTT>(mpz_fdiv_ui(a, charac));
       // std::cerr << "b " << b << std::endl;
        givaroField.init(result,  b);
       // std::cerr << "result " << result << std::endl;
    }

    void ARingGF::set_from_mpq(ElementType &result, const mpq_ptr a) const {
      //  std::cerr << "set_from_mpq" << std::endl;
        ElementType n, d;
        set_from_mpz(n, mpq_numref(a));
        set_from_mpz(d, mpq_denref(a));
        divide(result,n,d);
    }

    // arithmetic
    void ARingGF::negate(ElementType &result, const ElementType a) const
    {
        givaroField.neg(result,a);
    }

    /// if a is zero, the result is 1 , but is that what we expect?
    /// I vote for two invert functions, one with this check and one without.(Jakob)
    void ARingGF::invert(ElementType &result, const ElementType a) const
    {
       // std::cerr << "ARingGF::invert" << std::endl;
        if ( givaroField.isZero(a))
            ERROR(" division by zero");
        givaroField.inv(result,a);
    }


    void ARingGF::add(ElementType &result, const ElementType a, const ElementType b) const
    {
        givaroField.add(result,a,b);
    }

    void ARingGF::subtract(ElementType &result, const ElementType a, const ElementType b) const
    {
        givaroField.sub(result,a,b);
    }

    /// @param c[in][out] c = c- a*b
    void ARingGF::subtract_multiple(ElementType &c, const ElementType a, const ElementType b) const
    {
        givaroField. maxpyin(c,a,b);
    }

    void ARingGF::mult(ElementType &result, const ElementType a, const ElementType b) const
    {
        givaroField.mul(result,a,b);
    }

    void ARingGF::divide(ElementType &result, const ElementType a, const ElementType b) const
    {
        if ( givaroField.isZero(b))
           ERROR(" division by zero");
        givaroField.div(result,a,b);
    }

   /// @jakob: overflow can be occured due to multiplication. use exact mpz for multiply and modulo operation instead!
    void ARingGF::power(ElementType &result, const ElementType a, const  STT n) const
    {
        if (givaroField.isnzero(a)) 
        {
            mpz_t  mpz_a;
            mpz_t  mpz_n;
            mpz_t  mpz_tmp;
            mpz_init( mpz_a);
            mpz_init( mpz_n);
            mpz_init( mpz_tmp);
            mpz_set_si   (mpz_n,n);
            mpz_set_ui (mpz_a,a);
            //std::cerr << "a = " << a << std::endl;
            //std::cerr << "mpz_a = " << mpz_a << std::endl;
            //std::cerr << "n = " << n << std::endl;
            //std::cerr << "mpz_n = " << mpz_n << std::endl;
            mpz_fdiv_r_ui(mpz_tmp, mpz_n, givaroField.cardinality() -1)  ;
            mpz_mul(mpz_n, mpz_a, mpz_tmp);
            STT tmp = static_cast< STT>(mpz_fdiv_ui( mpz_n, givaroField.cardinality() -1))  ;
            if ( tmp==0 )
            {
                tmp+=givaroField.cardinality()-1;
                // result=givaroField.one;
            }
             
            //std::cerr << "tmp = " << tmp << std::endl;
            assert(tmp>=0); // tmp<0 should never occur
             if (tmp < 0) tmp += givaroField.cardinality()-1 ; 
            result = tmp;
        }
        else
        {
            if (n<0)
                 ERROR(" division by zero");
            result = 0;
        }
    }

    ///@todo ensure that  givaroField.cardinality() fits in a unsigned long, otherwise instead of mpz_fdiv_ui a different function has to be called)
    void ARingGF::power_mpz(ElementType &result, const  ElementType a, const  mpz_ptr n) const
    {
        STT n1 = static_cast< STT>(mpz_fdiv_ui(n, givaroField.cardinality()-1));

        //std::cerr << "exponent = " << n << std::endl;
        //std::cerr << "n1 = " << n1 << std::endl;
        power(result,a,n1);
    }

    ///@note dublicate code
    void ARingGF::swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }

    void ARingGF::elem_text_out(buffer &o, 
                const ElementType a, 
                bool p_one, 
                bool p_plus, 
                bool p_parens) const;

    /** @brief returns x,y  s.y.  x*a + y*b == 0.
       if possible, x is set to 1.
       no need to consider the case a==0 or b==0.
    */

    void ARingGF::syzygy(const ElementType a, const ElementType b,
                        ElementType &x, ElementType &y) const
   
    {
      x = givaroField.one;
      divide(y,a,b);
      negate(y,y);
    }

    /// @jakob: document possible overflow and other nasty things
    void ARingGF::random(ElementType &result) const
    {
        //result = rawRandomInt((int32_t) givaroField.cardinality());
        result = givaroRandomIterator() %   givaroField.cardinality();
    }

};

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
