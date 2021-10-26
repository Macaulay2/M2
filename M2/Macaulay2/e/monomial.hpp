// (c) 1994 Michael E. Stillman

#ifndef _monomial_hh_
#define _monomial_hh_

#include "hash.hpp"
#include "engine-includes.hpp"
#include "buffer.hpp"
#include "varpower.hpp"

#include <vector>

class Monomial : public EngineObject
{
  // The format of a monomial is from varpower.hpp:
  // [2n+1, v1, e1, ..., vn, en]
  intarray val;

  Monomial();
  Monomial(int v, int e);
  Monomial(const int *vp);
  Monomial(M2_arrayint a);

  Monomial(const std::vector<int>& vp);
  
  int * ints() { return val.raw(); }
protected:
  virtual unsigned int computeHashValue() const;

 public:
  static Monomial *make(int v, int e);
  static Monomial *make(M2_arrayint m);
  static Monomial *make(const int *vp);

  // format for this is that of a 'varpower' monomial:
  // [2n+1, v1, e1, v2, e2, ..., vn, en]
  // with each ei != 0.
  static Monomial * make(const std::vector<int>& monom);
  
  const int * ints() const { return val.raw(); }

  Monomial *operator*(const Monomial &b) const;
  Monomial *operator/(const Monomial &b) const;
  Monomial *power(int n) const;
  void monsyz(const Monomial &b, Monomial *&sa, Monomial *&sb) const;
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
// indent-tabs-mode: nil
// End:
