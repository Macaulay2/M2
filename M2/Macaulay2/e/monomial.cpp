// (c) 1995 Michael E. Stillman

#include "monomial.hpp"
#include "error.h"
#include "monoid.hpp"

EngineMonomial::EngineMonomial()
{
  // This routine is private because it leaves the object in
  // an incorrect state... to be filled in by varpower routines.
}

  // SIGH... the front end reverses monomials.  For commutative ones, this
  // is not a problem.  FOr non-commutative ones, one needs to reverse the
  // varpower pairs before calling this function.
  // Thus, if we are non-commutative, and
  //   vp = [7 0 1 2 1 0 2]
  // then the corresponding monomial is
  //    a^2ca

EngineMonomial::EngineMonomial(const std::vector<int>& vp) { varpower::copy(vp.data(), val); }
EngineMonomial::EngineMonomial(int v, int e) { varpower::var(v, e, val); }
EngineMonomial::EngineMonomial(const int *vp) { varpower::copy(vp, val); }
EngineMonomial::EngineMonomial(M2_arrayint m) { varpower::from_arrayint(m, val); }

EngineMonomial *EngineMonomial::make(int v, int e)
{
  EngineMonomial *result = new EngineMonomial(v, e);
  if (error()) return 0;
  return result;
}

EngineMonomial *EngineMonomial::make(M2_arrayint m)
{
  if ((m->len % 2) != 0)
    {
      ERROR("Monomial expected an even number of elements");
      return 0;
    }
  for (unsigned int i = 2; i < m->len; i += 2)
    if (m->array[i - 2] <= m->array[i])
      {
        ERROR("Monomial expects variables in descending order");
        return 0;
      }
  EngineMonomial *result = new EngineMonomial(m);
  if (error()) return 0;
  return result;
}

EngineMonomial *EngineMonomial::make(const int *vp)
{
  EngineMonomial *result = new EngineMonomial(vp);
  if (error()) return 0;
  return result;
}

EngineMonomial* EngineMonomial::make(const std::vector<int>& vp)
{
  EngineMonomial* result = new EngineMonomial(vp);
  return result;
}

unsigned int EngineMonomial::computeHashValue() const
{
  return varpower::computeHashValue(val.data());
}

bool EngineMonomial::is_one() const { return varpower::is_one(ints()); }
bool EngineMonomial::is_equal(const EngineMonomial &b) const
{
  if (this == &b) return true;
  return varpower::is_equal(ints(), b.ints());
}

int EngineMonomial::compare(const Monoid *M, const EngineMonomial &b) const
{
  monomial monom1 = M->make_one();
  monomial monom2 = M->make_one();
  M->from_varpower(ints(), monom1);
  M->from_varpower(b.ints(), monom2);
  int result = M->compare(monom1, monom2);
  M->remove(monom1);
  M->remove(monom2);
  return result;
}

bool EngineMonomial::divides(const Monoid *M, const EngineMonomial &b) const
{
  monomial monom1 = M->make_one();
  monomial monom2 = M->make_one();
  M->from_varpower(ints(), monom1);
  M->from_varpower(b.ints(), monom2);
  bool result = M->divides(monom1, monom2);
  M->remove(monom1);
  M->remove(monom2);
  return result;
}

int EngineMonomial::simple_degree() const { return varpower::simple_degree(ints()); }
EngineMonomial *EngineMonomial::lcm(const EngineMonomial &b) const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::lcm(ints(), b.ints(), result->val);
  return result;
}

EngineMonomial *EngineMonomial::gcd(const EngineMonomial &b) const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::gcd(ints(), b.ints(), result->val);
  return result;
}

void EngineMonomial::monsyz(const EngineMonomial &b, EngineMonomial *&sa, EngineMonomial *&sb) const
{
  sa = new EngineMonomial;
  sb = new EngineMonomial;
  varpower::monsyz(ints(), b.ints(), sa->val, sb->val);
}

EngineMonomial *EngineMonomial::operator*(const EngineMonomial &b) const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::mult(ints(), b.ints(), result->val);
  if (error()) return 0;
  return result;
}

EngineMonomial *EngineMonomial::operator/(const EngineMonomial &b) const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::quotient(ints(), b.ints(), result->val);
  return result;
}

EngineMonomial *EngineMonomial::erase(const EngineMonomial &b) const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::erase(ints(), b.ints(), result->val);
  return result;
}

EngineMonomial *EngineMonomial::power(int n) const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::power(ints(), n, result->val);
  if (error()) return 0;
  return result;
}

EngineMonomial *EngineMonomial::radical() const
{
  EngineMonomial *result = new EngineMonomial;
  varpower::radical(ints(), result->val);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
