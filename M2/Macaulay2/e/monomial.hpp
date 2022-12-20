// (c) 1994 Michael E. Stillman

#ifndef _monomial_hh_
#define _monomial_hh_

#include <vector>

#include "ExponentList.hpp"
#include "hash.hpp"
#include "engine-includes.hpp"
#include "buffer.hpp"

// TODO: can this be combined with varpower using templates?
class EngineMonomial : public EngineObject
{
  // The format of a monomial is from ExponentList.hpp:
  // [2n+1, v1, e1, ..., vn, en]
  gc_vector<int> val;

  EngineMonomial();
  EngineMonomial(int v, int e);
  EngineMonomial(const int *vp);
  EngineMonomial(M2_arrayint a);
  EngineMonomial(const std::vector<int>& vp);

 protected:
  virtual unsigned int computeHashValue() const;

 public:
  static EngineMonomial *make(int v, int e);
  static EngineMonomial *make(M2_arrayint m);
  static EngineMonomial *make(const int *vp);
  static EngineMonomial *make(const std::vector<int>& vp);
  // format for this is that of a 'varpower' monomial:
  // [2n+1, v1, e1, v2, e2, ..., vn, en]
  // with each ei != 0.

  int * ints() { return val.data(); }
  const int * ints() const { return val.data(); }

  EngineMonomial *operator*(const EngineMonomial &b) const;
  EngineMonomial *operator/(const EngineMonomial &b) const;
  EngineMonomial *power(int n) const;
  void monsyz(const EngineMonomial &b, EngineMonomial *&sa, EngineMonomial *&sb) const;
  EngineMonomial *lcm(const EngineMonomial &b) const;
  EngineMonomial *gcd(const EngineMonomial &b) const;

  EngineMonomial *radical() const;
  EngineMonomial *erase(const EngineMonomial &b) const;

  bool is_one() const;
  bool is_equal(const EngineMonomial &b) const;
  bool divides(const Monoid *M, const EngineMonomial &b) const;
  int compare(const Monoid *M, const EngineMonomial &b) const;
  int simple_degree() const;

  void text_out(buffer &o) const { varpower::elem_text_out(o, val.data()); }
  M2_arrayint to_arrayint() const { return varpower::to_arrayint(val.data()); }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
