// (c) 1994 Michael E. Stillman

#ifndef _monomial_hh_
#define _monomial_hh_

#include "varpower.hpp"
#include "object.hpp"

class monomial_rec : public immutable_object
{
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  intarray val;

  friend class Monomial;

  monomial_rec() : val() { varpower::one(val); }
  virtual ~monomial_rec() {}

  // Infrastructure
  class_identifier class_id() const { return CLASS_monomial; }
  type_identifier  type_id () const { return TY_MONOMIAL; }
  const char * type_name   () const { return "monomial"; }

  Monomial cast_to_Monomial();

  intarray *intarray_of() { return &val; }

  virtual bool equals(const object_element *o) const;

  void text_out(buffer &o) const { varpower::elem_text_out(o, val.raw()); }
  void bin_out(buffer &o) const { varpower::elem_bin_out(o, val.raw()); }
};

class Monomial
{
  POINTER(Monomial, monomial_rec)
public:
  Monomial(int v);
  Monomial(int v, int e);
  Monomial(const int *vp);
  Monomial(char *&s, int &len);

  intarray &get_intarray() { return obj->val; }
  const intarray &get_intarray() const { return obj->val; }
  int * ints() { return obj->val.raw(); }
  const int * ints() const { return obj->val.raw(); }

  Monomial operator*(const Monomial &b) const;
  Monomial operator/(const Monomial &b) const;
  Monomial power(int n) const;
  void monsyz(const Monomial &b, Monomial &sa, Monomial &sb) const;
  Monomial lcm(const Monomial &b) const;
  Monomial gcd(const Monomial &b) const;

  Monomial radical() const;
  Monomial erase(const Monomial &b) const;

  bool is_one() const;
  bool is_equal(const Monomial &b) const;
  int divides(const Monomial &b) const;
  int compare(const Monomial &b) const;
  int simple_degree() const;
};

inline Monomial monomial_rec::cast_to_Monomial() 
{ return Monomial(this,caster); }

#if 0
inline Monomial::Monomial() : 
  obj(new monomial_rec())
{  
}
#endif

inline Monomial::Monomial(int v, int e) : 
  obj(new monomial_rec())
{
  get_intarray().shrink(0);
  varpower::var(v, e, get_intarray());
}

inline Monomial::Monomial(const int *vp) : 
  obj(new monomial_rec())
{
  get_intarray().shrink(0);
  varpower::copy(vp, get_intarray());
}

inline Monomial::Monomial(int v) : 
  obj(new monomial_rec())
{
  get_intarray().shrink(0);
  varpower::var(v, 1, get_intarray());
}

inline bool Monomial::is_one() const
{
  return varpower::is_one(ints());
}

inline bool Monomial::is_equal(const Monomial &b) const
{
  if (this == &b) return true;
  return varpower::is_equal(ints(), b.ints());
}
#endif
