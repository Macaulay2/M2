// Copyright 2011 Michael E. Stillman

#ifndef _ring_glue_hh_
#define _ring_glue_hh_

#include "ring.hpp"
#include "aring.hpp"

#include "ring.hpp"

namespace M2 {
  template <class RingType>
  class RingWrap : public Ring
  {
    const RingType *R;
    RingWrap(const RingType *R0) : R(R0) {}
    virtual ~RingWrap() {}
  public:
    typedef typename RingType::ElementType ElementType;

    static RingWrap<RingType> * create(const RingType *R);

    //  Z_mod * cast_to_Z_mod() { return this; }
    //  const Z_mod * cast_to_Z_mod() const { return this; }

    virtual int coerce_to_int(ring_elem a) const
    {
      return 0;
    }
    virtual int discrete_log(ring_elem a) const // returns -1 if a is 0
    {
      return 0;
    }

    // The following are all the routines required by 'ring'

    void to_ring_elem(ring_elem &result, const ElementType &a) const;
    void from_ring_elem(ElementType &result, const ring_elem &a) const; // result should have been 'init'ed

    virtual void text_out(buffer &o) const { return R->text_out(o); }

    virtual ring_elem from_int(int n) const
    {
      fprintf(stderr, "calling from_int\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_int(a,n);
      to_ring_elem(result, a);
      return result;
    }

    virtual ring_elem from_int(mpz_ptr n) const
    {
      fprintf(stderr, "calling from_int(mpz)\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpz(a,n);
      to_ring_elem(result, a);
      return result;
    }
    virtual ring_elem from_rational(mpq_ptr q) const
    {
      fprintf(stderr, "calling from_rational\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpq(a,q);
      to_ring_elem(result, a);
      return result;
    }

    virtual ring_elem var(int v) const
    {
      fprintf(stderr, "calling var\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_var(a,v);
      to_ring_elem(result, a);
      return result;
    }

    virtual bool promote(const Ring *S, const ring_elem f, ring_elem &result) const
    {
      fprintf(stderr, "calling promote\n");
      return false;
    }
    virtual bool lift(const Ring *S, const ring_elem f, ring_elem &result) const
    {
      fprintf(stderr, "calling lift\n");
      return false;
    }

    virtual bool is_unit(const ring_elem f) const
    {
      fprintf(stderr, "calling is_unit\n");
      ElementType a;
      R->init(a);
      from_ring_elem(a, f);
      return R->is_unit(f);
    }

    virtual bool is_zero(const ring_elem f) const
    {
      fprintf(stderr, "calling is_zero\n");
      ElementType a;
      R->init(a);
      from_ring_elem(a, f);
      return R->is_zero(f);
    }

    virtual bool is_equal(const ring_elem f, const ring_elem g) const
    {
      fprintf(stderr, "calling is_equal\n");
      ElementType a, b;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      return R->is_equal(a,b);
    }

    virtual int compare_elems(const ring_elem f, const ring_elem g) const
    {
      fprintf(stderr, "calling compare_elems\n");
      ElementType a, b;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      return R->compare_elems(a,b);
    }

    virtual ring_elem copy(const ring_elem f) const
    {
      fprintf(stderr, "calling copy\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      R->copy(b,a);
      to_ring_elem(result,b);
      return result;
    }

    virtual void remove(ring_elem &f) const
    {
      fprintf(stderr, "calling remove\n");
    }

    virtual ring_elem negate(const ring_elem f) const
    {
      fprintf(stderr, "calling negate\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      R->negate(b,a);
      to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem add(const ring_elem f, const ring_elem g) const
    {
      fprintf(stderr, "calling add\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      R->add(c,a,b);
      to_ring_elem(result,c);
      return result;
    }

    virtual ring_elem subtract(const ring_elem f, const ring_elem g) const
    {
      fprintf(stderr, "calling subtract\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      R->subtract(c,a,b);
      to_ring_elem(result,c);
      return result;
    }

    virtual ring_elem mult(const ring_elem f, const ring_elem g) const
    {
      fprintf(stderr, "calling mult\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      R->mult(c,a,b);
      to_ring_elem(result,c);
      return result;
    }

    virtual ring_elem power(const ring_elem f, mpz_t n) const
    {
      fprintf(stderr, "calling power mpz\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      R->power_mpz(b,a,n);
      to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem power(const ring_elem f, int n) const
    {
      fprintf(stderr, "calling power int\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      R->power(b,a,n);
      to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem invert(const ring_elem f) const
    {
      fprintf(stderr, "calling invert\n");
      ElementType a,b;
      ring_elem result;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      R->invert(b,a);
      to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem divide(const ring_elem f, const ring_elem g) const
    {
      fprintf(stderr, "calling divide\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(a);
      R->init(b);
      R->init(c);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      R->divide(c,a,b);
      to_ring_elem(result,c);
      return result;
    }

    virtual void syzygy(const ring_elem f, const ring_elem g,
                        ring_elem &x, ring_elem &y) const
    {
      fprintf(stderr, "calling syzygy\n");
      ElementType a, b, xe, ye;
      R->init(a);
      R->init(b);
      R->init(xe);
      R->init(ye);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      R->syzygy(a,b, xe,ye);
      to_ring_elem(x,xe);
      to_ring_elem(y,ye);
    }

    virtual ring_elem random() const
    {
      fprintf(stderr, "calling random\n");
      ring_elem result;
      ElementType a;
      R->random(a);
      to_ring_elem(result,a);
      return result;
    }

    virtual void elem_text_out(buffer &o,
                               const ring_elem f,
                               bool p_one=true,
                               bool p_plus=false,
                               bool p_parens=false) const
    {
      fprintf(stderr, "calling elem_text_out\n");
      ElementType a;
      R->init(a);
      from_ring_elem(a, f);
      R->elem_text_out(o,a,p_one,p_plus,p_parens);
    }

    virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const
    {
      fprintf(stderr, "calling eval\n");
      return 0;
    }
  };
};

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
