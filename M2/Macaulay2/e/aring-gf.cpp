// Copyright 2011 Michael E. Stillman

#include "aring-gf.hpp"


#if defined(HAVE_FFLAS_FFPACK) && defined(HAVE_GIVARO)

namespace M2 {





  ARingGF::ARingGF(      UTT charact_,
                                 UTT extensionDegree_)
    : charac(charact_),
      dimension(extensionDegree_),
        givaroField(FieldType(charact_, extensionDegree_))
  {

  }



  void ARingGF::elem_text_out(buffer &o,
                               const ElementType a,
                               bool p_one,
                               bool p_plus,
                               bool p_parens) const
  {
        /// todo: derive type of rep from givaroField
        UTT  rep;
        rep=givaroField.convert(rep,a);
        long exp=0;
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
{       return givaroField.isunit(f); }

bool ARingGF::is_zero(const ElementType f) const
{       return givaroField.isZero(f); }

bool ARingGF::is_equal(const ElementType f, const ElementType g) const
{ return        givaroField.areEqual(f,g); }


/// compare exponents of the used generator
int ARingGF::compare_elems(const ElementType f, const ElementType g) const
{
        std::cerr << "ARingGF::compare_elems" << std::endl;
        if (f < g) return -1;
        if (f > g) return 1;

        return 0;
}



int ARingGF::get_int(const elem f) const
{
        std::cerr << "ARingGF::get_int" << std::endl;
        return f;
}


int ARingGF::get_repr(const elem f) const
{
        std::cerr << "get_repr" << std::endl;
        int result;
         givaroField.convert(result,f);
        return result;
}

    // 'init', 'init_set' functions

        void ARingGF::init(elem &result) const          { result = givaroField.zero;  }

        void ARingGF::clear(elem &result) const                 { /* nothing */ }

    void ARingGF::set_zero(elem &result) const { result = givaroField.zero; }

    void ARingGF::copy(elem &result, const elem a) const { result = a; }


void ARingGF::set_from_int(elem &result, int a) const
{

        std::cerr << "ARingGF::set_from_int" << std::endl;
        a = a % charac;
        if (a < 0) a += charac;
        givaroField.init(result, a);
}

        void ARingGF::set_from_mpz(elem &result, const mpz_ptr a) const
        {
                std::cerr << "set_from_mpz" << std::endl;
                //assert(false);
                 UTT b = static_cast< UTT>(mpz_fdiv_ui(a, charac));
                 //UTT b = static_cast< UTT>(mpz_divexact_ui(a, charac));

                std::cerr << "b " << b << std::endl;
                givaroField.init(result,  b);
                std::cerr << "result " << result << std::endl;
        }

    void ARingGF::set_from_mpq(elem &result, const mpq_ptr a) const {
        std::cerr << "set_from_mpq" << std::endl;
      elem n, d;
      set_from_mpz(n, mpq_numref(a));
      set_from_mpz(d, mpq_denref(a));
      divide(result,n,d);
    }

    // arithmetic
    void ARingGF::negate(elem &result, const elem a) const
    {
     givaroField.neg(result,a);
    }

    void ARingGF::invert(elem &result, const elem a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
        givaroField.inv(result,a);
    }
    /// @todo: could be
    void ARingGF::add(elem &result, const elem a, const elem b) const
    {
        givaroField.add(result,a,b);
    }

    void ARingGF::subtract(ElementType &result, const ElementType a, const ElementType b) const
    {
     givaroField.sub(result,a,b);
    }
        /// @param c[in][out]
    /// c+=-a*b
    void ARingGF::subtract_multiple(elem &c, const elem a, const elem b) const
    {
                givaroField. maxpyin(c,a,b);
        //givaroField.invin(result);
        //givaroField.addin(c,givaroField.mul(result,a,b);
    }

    void ARingGF::mult(elem &result, const elem a, const elem b) const
    {
        givaroField.mul(result,a,b);
    }

    void ARingGF::divide(elem &result, const elem a, const elem b) const
    {
        givaroField.div(result,a,b);
    }

   /// @todo: overflow can be occured due to multiplication. use mpz to multiply instead?
    void ARingGF::power(elem &result, const elem a, const  STT n) const
    {
      if (givaroField.isnzero(a))
        {
                STT tmp = (a*(n % (charac-1)) ) % (charac-1);
                if (tmp <= 0) tmp += (charac-1);
                result = tmp;
        }
      else
        result = 0;
    }


    void ARingGF::power_mpz(elem &result, const  elem a, const  mpz_ptr n) const
    {
      STT n1 = static_cast< STT>(mpz_fdiv_ui(n, charac-1));
        //STT n1 = static_cast< STT>(mpz_divexact_ui(n, charac-1));
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

    void ARingGF::syzygy(const ElementType a, const ElementType b,
                ElementType &x, ElementType &y) const
    // returns x,y s.y. x*a + y*b == 0.
    // if possible, x is set to 1.
    // no need to consider the case a==0 or b==0.
    {
      x = givaroField.one;
      divide(y,a,b);
      negate(y,y);
    }

        /// @todo: document possible overflow and other nasty things
    void ARingGF::random(ElementType &result) const
    {
      result = rawRandomInt((int32_t)charac*dimension);
    }

};

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
