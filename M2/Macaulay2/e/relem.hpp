// Copyright 1995 Michael E. Stillman

#ifndef _ring_elem_hh_
#define _ring_elem_hh_

#include "ring.hpp"

// Using arbitrary precision integers in the engine.
//
// A RingElement, whose ring is ZZ (the only object ot type Z),
// is a reference counted bignum.
// 
// Usual operations, such as +, -, *, / are defined automatically.
//
// To check that the ring of f is Z: 
//    f.Ring_of()->is_Z()
//
// To extract the various limbs of f: (NOT FUNCTIONAL YET)
//    
//    f.extract_limb(0)    gets the lowest part, usually 32 or 64 bits.
//    f.extract_limb(1)    next higher part
//    f.n_limbs()
//
// To create a bignum from a set of 16 bit integers: (NOT FUNCTIONAL YET)
//    (low order parts first).
//
//    Z::make_int(int nparts, int parts[])
// To check the sign of a number, or whether it is zero:
//
//    f.is_zero()
//    f.sign()         returns -1,0,or 1 (NOT FUNCTIONAL YET)
class RingElement_rec : public object_element
{
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { 
       return mystash->new_elem(); 
  }
  void operator delete(void *p) { mystash->delete_elem(p); }
  friend class RingElement;

  const Ring *R;
  ring_elem val;

  RingElement_rec  (const Ring *RR)
    : R(RR), val(R->from_int(0)) { bump_up((Ring *)R); }

  RingElement_rec  (const Ring *RR, ring_elem f) 
    : R(RR), val(f) { bump_up((Ring *)R); }

  ~RingElement_rec () { R->remove(val); bump_down((Ring *)R); }

  // Infrastructure
  object_types type_of          () const { return TY_RING_ELEM; }
  const char * type_name        () const { return "Ring element"; }
  RingElement    cast_to_RingElement();

  int          int_of() const { return R->coerce_to_int(val); }
  int          length_of() const;

  void         text_out (ostream &o) const;
  void         bin_out  (ostream &o) const;
};

class RingElement
{
  POINTER(RingElement, RingElement_rec)
public:
  RingElement(const Ring *R);
  RingElement(const Ring *R, int n);
  RingElement(const Ring *R, mpz_t n);
  RingElement(const Ring *R, int v, int e);
  RingElement(const Ring *R, ring_elem f);
  RingElement(const Ring *R, RingElement a, Monomial m);

  ring_elem    get_value ()             const { return obj->val; }
  void         set_value (ring_elem f)       { obj->val = f; }
  const Ring * Ring_of  ()             const { return obj->R; }

  // ring arithmetic

  bool is_zero() const;
  bool is_equal(const RingElement &b) const;
  bool is_unit() const;

  RingElement operator-() const;
  RingElement operator+(const RingElement &b) const;
  RingElement operator-(const RingElement &b) const;
  RingElement operator*(const RingElement &b) const;
  RingElement operator*(int n) const;
  RingElement power(mpz_t n) const;
  RingElement power(int n) const;

  RingElement operator/(const RingElement &b) const;
  RingElement operator%(const RingElement &b) const;
  RingElement divide(const RingElement &b, 
		      RingElement &rem) const;
  RingElement gcd(const RingElement &b) const;
  RingElement gcd_extended(const RingElement &b, 
			    RingElement &u, RingElement &v) const;
  RingElement invert() const;

  static RingElement random(const Ring *R);
  static RingElement random(const Ring *R, int homog, const intarray &deg);

  void text_out (ostream &o) const;

  // We have several ways of moving from one ring to the next:
  //    R ---> R[x1..xn]
  //    R ---> R/I
  //    R ---> frac R
  //    Z/p[x]/F(x) ---> GF(p,n)
  //    R ---> local(R,I)    (much later...)

  // Both of the following routines assume that S ---> 'this'
  // is one of these construction steps.  Promote takes an element of
  // S, and maps it into 'this', while lift goes the other way.

  int promote(const Ring *S, RingElement &result) const;
  int lift(const Ring *S, RingElement &result) const;

  // functions primarily for polynomial ring elements

  RingElement lead_term(int n=-1) const;
  RingElement rest() const;
  RingElement get_terms(int lo, int hi) const;
  RingElement get_coeff(const Monomial &m) const;
  RingElement lead_coeff() const;
  Monomial  lead_monom() const;
  int       is_homogeneous() const;
  RingElement homogenize(int v, const int *wts) const;
  RingElement homogenize(int v, int deg, const int *wts) const;
  intarray  degree() const;

  // functions for fraction fields

  RingElement numerator() const;
  RingElement denominator() const;
};

//-----------------------------------------------------------------
inline RingElement RingElement_rec :: cast_to_RingElement() 
{ return RingElement(this,caster); }
//-----------------------------------------------------------------

inline RingElement::RingElement(const Ring *R) : 
  obj(new RingElement_rec(R))
{  
}

inline RingElement::RingElement(const Ring *R, int n) : 
  obj(new RingElement_rec(R, R->from_int(n)))
{
}

inline RingElement::RingElement(const Ring *R, mpz_t n) : 
  obj(new RingElement_rec(R, R->from_int(n)))
{
}

inline RingElement::RingElement(const Ring *R, int v, int e) : 
  obj(new RingElement_rec(R, R->var(v,e)))
{
}

inline RingElement::RingElement(const Ring *R, ring_elem f) : 
  obj(new RingElement_rec(R, f))
{
}

inline bool RingElement::is_zero() const
{
  return Ring_of()->is_zero(obj->val);
}

inline bool RingElement::is_unit() const
{
  return Ring_of()->is_unit(obj->val);
}

inline bool RingElement::is_equal(const RingElement &b) const
{
  if (this == &b) return true;
  if (Ring_of() != b.Ring_of())
    {
      *gError << "cannot compare ring elements from different rings";
      return false;
    }
  return Ring_of()->is_equal(obj->val, b.obj->val);
}

#endif
