// (c) 1994 Michael E. Stillman

#ifndef _monomial_hh_
#define _monomial_hh_

#include "varpower.hpp"
#include "object.hpp"

class monomial_rec : public object_element
{
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  intarray val;

  friend class monomial;

  monomial_rec() : val() { varpower::one(val); }
  ~monomial_rec() {}

  // Infrastructure
  object_types type_of    () const { return TY_MONOMIAL; }
  const char * type_name  () const { return "monomial"; }
  monomial cast_to_monomial();

  intarray *intarray_of() { return &val; }

  void text_out(ostream &o) const { varpower::elem_text_out(o, val.raw()); }
  void bin_out(ostream &o) const { varpower::elem_bin_out(o, val.raw()); }
};

class monomial
{
  POINTER(monomial, monomial_rec)
public:
  monomial(int v);
  monomial(int v, int e);
  monomial(const int *vp);
  monomial(char *&s, int &len);

  intarray &get_intarray() { return obj->val; }
  const intarray &get_intarray() const { return obj->val; }
  int * ints() { return obj->val.raw(); }
  const int * ints() const { return obj->val.raw(); }

  monomial operator*(const monomial &b) const;
  monomial operator/(const monomial &b) const;
  monomial power(int n) const;
  void monsyz(const monomial &b, monomial &sa, monomial &sb) const;
  monomial lcm(const monomial &b) const;
  monomial gcd(const monomial &b) const;

  monomial radical() const;
  monomial erase(const monomial &b) const;

  bool is_one() const;
  bool is_equal(const monomial &b) const;
  int divides(const monomial &b) const;
  int compare(const monomial &b) const;
  int simple_degree() const;
};

inline monomial monomial_rec::cast_to_monomial() 
{ return monomial(this,caster); }

#if 0
inline monomial::monomial() : 
  obj(new monomial_rec())
{  
}
#endif

inline monomial::monomial(int v, int e) : 
  obj(new monomial_rec())
{
  get_intarray().shrink(0);
  varpower::var(v, e, get_intarray());
}

inline monomial::monomial(const int *vp) : 
  obj(new monomial_rec())
{
  get_intarray().shrink(0);
  varpower::copy(vp, get_intarray());
}

inline monomial::monomial(int v) : 
  obj(new monomial_rec())
{
  get_intarray().shrink(0);
  varpower::var(v, 1, get_intarray());
}

inline bool monomial::is_one() const
{
  return varpower::is_one(ints());
}

inline bool monomial::is_equal(const monomial &b) const
{
  if (this == &b) return true;
  return varpower::is_equal(ints(), b.ints());
}
#endif
