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
    }
    virtual int discrete_log(ring_elem a) const // returns -1 if a is 0
    {
    }
    
    // The following are all the routines required by 'ring'
    
    void to_ring_elem(ring_elem &result, const ElementType &a) const;
    void from_ring_elem(ElementType &result, const ring_elem &a) const; // result should have been 'init'ed
    
    virtual void text_out(buffer &o) const { return R->text_out(o); }
    
    virtual ring_elem from_int(int n) const 
    { 
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_int(a,n); 
      to_ring_elem(result, a);
      return result;
    }
    
    virtual ring_elem from_int(mpz_ptr n) const 
    {
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpz(a,n); 
      to_ring_elem(result, a);
      return result;
    }
    virtual ring_elem from_rational(mpq_ptr q) const
    {
      ring_elem result;
      ElementType a;
      R->init(a);
      R->set_from_mpq(a,q); 
      to_ring_elem(result, a);
      return result;
    }
    
    virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const
    {
    }
    virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const
    {
    }
    
    virtual bool is_unit(const ring_elem f) const
    {
      ElementType a;
      R->init(a);
      from_ring_elem(a, f);
      return R->is_unit(f);
    }
    
    virtual bool is_zero(const ring_elem f) const
    {
      ElementType a;
      R->init(a);
      from_ring_elem(a, f);
      return R->is_zero(f);
    }
    
    virtual bool is_equal(const ring_elem f, const ring_elem g) const
    {
      ElementType a, b;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      return R->is_equal(a,b);
    }
    
    virtual int compare_elems(const ring_elem f, const ring_elem g) const
    {
      ElementType a, b;
      R->init(a);
      R->init(b);
      from_ring_elem(a, f);
      from_ring_elem(b,g);
      return R->compare_elems(a,b);
    }
    
    virtual ring_elem copy(const ring_elem f) const
    {
    }
    
    virtual void remove(ring_elem &f) const
    {
    }
    
    virtual ring_elem negate(const ring_elem f) const
    {
    }

    virtual ring_elem add(const ring_elem f, const ring_elem g) const
    {
    }

    virtual ring_elem subtract(const ring_elem f, const ring_elem g) const
    {
    }

    virtual ring_elem mult(const ring_elem f, const ring_elem g) const
    {
    }

    virtual ring_elem power(const ring_elem f, mpz_t n) const
    {
    }

    virtual ring_elem power(const ring_elem f, int n) const
    {
    }

    virtual ring_elem invert(const ring_elem f) const
    {
    }

    virtual ring_elem divide(const ring_elem f, const ring_elem g) const
    {
    }
    
    virtual void syzygy(const ring_elem a, const ring_elem b,
			ring_elem &x, ring_elem &y) const
    {
    }
    
    virtual ring_elem random() const
    {
    }
    
    virtual void elem_text_out(buffer &o, 
			       const ring_elem f, 
			       bool p_one=true, 
			       bool p_plus=false, 
			       bool p_parens=false) const
    {
    }
    
    virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const
    {
    }
  };
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

