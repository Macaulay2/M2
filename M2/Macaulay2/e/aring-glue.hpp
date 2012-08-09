// Copyright 2011 Michael E. Stillman

#ifndef _ring_glue_hh_
#define _ring_glue_hh_

#include "aring.hpp"
#include "ring.hpp"

static const bool displayArithmeticCalls = false;

#define COERCE_RING(RingType,R) dynamic_cast<const RingType *>(R)

namespace M2 {
  template<class RingType>
  MutableMatrix* makeMutableZeroMatrix(const Ring* Rgeneral,
                                       const RingType* R,
                                       size_t nrows,
                                       size_t ncols,
                                       bool dense);
/**
    @ingroup rings
*/
  template <class RingType>
  class ConcreteRing : public Ring
  {
    const RingType *R;
    ConcreteRing(const RingType *R0) : R(R0) {}
    virtual ~ConcreteRing() {}
  public:
    typedef typename RingType::ElementType ElementType;

    static ConcreteRing<RingType> * create(const RingType *R);

    virtual M2::RingID ringID() const { return RingType::ringID; }

    const RingType & ring() const { return *R; }

    /// Create either a dense or sparse MutableMatrix of the given size
    virtual MutableMatrix* makeMutableMatrix(size_t nrows, size_t ncols, bool dense) const
    {
      //return R->makeMutableMatrix(this, nrows, ncols, dense);
      return makeMutableZeroMatrix(this, R, nrows, ncols, dense);
    }

    ////////////////////////////
    // Functions on elements ///
    ////////////////////////////
    virtual int coerce_to_int(ring_elem a) const
    {
      //TODO: implement or remove
      return 0;
    }

    // The following are all the routines required by 'ring'
    virtual void text_out(buffer &o) const { return R->text_out(o); }

    virtual ring_elem from_int(int n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling from_int\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_int(a,n);
      R->to_ring_elem(result, a);
      return result;
    }

    virtual ring_elem from_int(mpz_ptr n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling from_int(mpz)\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpz(a,n);
      R->to_ring_elem(result, a);
      return result;
    }
    virtual ring_elem from_rational(mpq_ptr q) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling from_rational\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpq(a,q);
      R->to_ring_elem(result, a);
      return result;
    }
    virtual bool from_BigReal(gmp_RR q, ring_elem &result) const
    {
      ElementType a;
      R->init(a);
      bool ret = R->set_from_BigReal(a,q);
      R->to_ring_elem(result, a);
      return ret;
    }

    virtual ring_elem var(int v) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling var\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_var(a,v);
      R->to_ring_elem(result, a);
      return result;
    }

    /** If there is a "natural" map S --> R=this, where f is an element of
    // S , then result is set to the image of f, and true is returned.
    // Otherwise, false is returned.
    //
    // The map must one-step, e.g. for k --> k[x] --> k[x][y], promotion
    // must be done with two consecutive calls to promote(with different arguments).
    // Examples of natural maps:
    //  ZZ --> R, for any R
    //  QQ --> RR --> CC
    //  ZZ --> ZZ/p
    //  ZZ/p --> GF(p^n)
    //  GF(p^m) --> GF(p^n), where m|n
    //  A --> A[vars]/I
    //  A[vars]/J --> A[vars]/I  (assumption: I contains J).
    */

    virtual bool promote(const Ring *S, const ring_elem f, ring_elem &result) const;

    //* If there is a "natural" map S --> R=this, where f is an element of
    // R, then result is set to an element f in S which maps to f, and true is returned.
    // Otherwise, false is returned.
    //
    // For examples of maps, see 'promote'.
    //
    // Lifting elements of ZZ/p to QQ does not count, as there is no actual homomorphism
    // from QQ --> ZZ/p

    virtual bool lift(const Ring *S, const ring_elem f, ring_elem &result) const
    {
      //TODO: implement
      if (displayArithmeticCalls) fprintf(stderr, "calling lift\n");
      return false;
    }

    virtual bool is_unit(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling is_unit\n");
      ElementType a;
      R->from_ring_elem(a, f);
      return R->is_unit(a);
    }

    virtual bool is_zero(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling is_zero\n");
      ElementType a;
      R->from_ring_elem(a, f);
      return R->is_zero(a);
    }

    virtual bool is_equal(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling is_equal\n");
      ElementType a, b;
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      return R->is_equal(a,b);
    }

    virtual int compare_elems(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling compare_elems\n");
      ElementType a, b;
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      return R->compare_elems(a,b);
    }

    virtual ring_elem copy(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling copy\n");
      ElementType a,b;
      ring_elem result;
      R->init(b);
      R->from_ring_elem(a, f);
      R->copy(b,a);
      R->to_ring_elem(result,b);
      return result;
    }

    virtual void remove(ring_elem &f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling remove\n");
      /* currently, do nothing... */
    }

    virtual ring_elem negate(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling negate\n");
      ElementType a,b;
      ring_elem result;
      R->init(b);
      R->from_ring_elem(a, f);
      R->negate(b,a);
      R->to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem add(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling add\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->add(c,a,b);
      R->to_ring_elem(result,c);
      return result;
    }

    virtual ring_elem subtract(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling subtract\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->subtract(c,a,b);
      R->to_ring_elem(result,c);
      return result;
    }

    virtual ring_elem mult(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling mult\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->mult(c,a,b);
      R->to_ring_elem(result,c);
      return result;
    }

    virtual ring_elem power(const ring_elem f, mpz_t n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling power mpz\n");
      ElementType a,b;
      ring_elem result;
      R->init(b);
      R->from_ring_elem(a, f);
      R->power_mpz(b,a,n);
      R->to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem power(const ring_elem f, int n) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling power int\n");
      ElementType a,b;
      ring_elem result;
      R->init(b);
      R->from_ring_elem(a, f);
      R->power(b,a,n);
      R->to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem invert(const ring_elem f) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling invert\n");
      ElementType a,b;
      ring_elem result;
      R->init(b);
      R->from_ring_elem(a, f);
      R->invert(b,a);
      R->to_ring_elem(result,b);
      return result;
    }

    virtual ring_elem divide(const ring_elem f, const ring_elem g) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling divide\n");
      ElementType a, b, c;
      ring_elem result;
      R->init(c);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->divide(c,a,b);
      R->to_ring_elem(result,c);
      return result;
    }

    virtual void syzygy(const ring_elem f, const ring_elem g,
                        ring_elem &x, ring_elem &y) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling syzygy\n");
      ElementType a, b, xe, ye;
      R->init(xe);
      R->init(ye);
      R->from_ring_elem(a, f);
      R->from_ring_elem(b,g);
      R->syzygy(a,b, xe,ye);
      R->to_ring_elem(x,xe);
      R->to_ring_elem(y,ye);
    }

    virtual ring_elem random() const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling random\n");
      ring_elem result;
      ElementType a;
      R->init(a);
      R->random(a);
      R->to_ring_elem(result,a);
      return result;
    }

    virtual void elem_text_out(buffer &o,
                               const ring_elem f,
                               bool p_one=true,
                               bool p_plus=false,
                               bool p_parens=false) const
    {
      if (displayArithmeticCalls) fprintf(stderr, "calling elem_text_out\n");
      ElementType a;
      R->from_ring_elem(a, f);
      R->elem_text_out(o,a,p_one,p_plus,p_parens);
    }

    // map : this = R --> S, f in R
    // sending primelem --> map->elem(firstvar)
    // return map(f) as a ring_elem in S
    virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const
    {
      ElementType a;
      ring_elem result;
      R->from_ring_elem(a, f);
      R->eval(map, a, first_var, result);
      return result;
    }
  };

};

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
