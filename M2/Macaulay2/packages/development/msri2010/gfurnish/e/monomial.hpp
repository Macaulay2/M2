// (c) 1994 Michael E. Stillman

#ifndef _monomial_hh_
#define _monomial_hh_

#include "engine.h"
#include "varpower.hpp"
#include "hash.hpp"

class Monomial : public immutable_object
{
  intarray val;
  
  Monomial();
  Monomial(int v, int e);
  Monomial(const int *vp);
  Monomial(M2_arrayint a);

  int * ints() { return val.raw(); }
  void set_hash_code();
public:
  static Monomial *make(int v, int e);
  static Monomial *make(M2_arrayint m);
  static Monomial *make(const int *vp);
  const int * ints() const { return val.raw(); }

  Monomial *operator*(const Monomial &b) const;
  Monomial *operator/(const Monomial &b) const;
  Monomial *power(int n) const;
  void monsyz(const Monomial &b, Monomial * &sa, Monomial * &sb) const;
  Monomial *lcm(const Monomial &b) const;
  Monomial *gcd(const Monomial &b) const;

  Monomial *radical() const;
  Monomial *erase(const Monomial &b) const;

  bool is_one() const;
  bool is_equal(const Monomial &b) const;
  bool divides(const Monoid *M, const Monomial &b) const;
  int compare(const Monoid *M, const Monomial &b) const;
  int simple_degree() const;

  void text_out(buffer &o) const { varpower::elem_text_out(o, val.raw()); }
  M2_arrayint to_arrayint() const { return varpower::to_arrayint(val.raw()); }
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
