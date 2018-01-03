// (c) 1995 Michael E. Stillman

#include "monomial.hpp"
#include "error.h"
#include "monoid.hpp"

Monomial::Monomial()
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

Monomial::Monomial(const std::vector<int>& vp) { varpower::copy(vp.data(), val); }
Monomial::Monomial(int v, int e) { varpower::var(v, e, val); }
Monomial::Monomial(const int *vp) { varpower::copy(vp, val); }
Monomial::Monomial(M2_arrayint m) { varpower::from_arrayint(m, val); }

Monomial *Monomial::make(int v, int e)
{
  Monomial *result = new Monomial(v, e);
  if (error()) return 0;
  return result;
}

Monomial *Monomial::make(M2_arrayint m)
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
  Monomial *result = new Monomial(m);
  if (error()) return 0;
  return result;
}

Monomial *Monomial::make(const int *vp)
{
  Monomial *result = new Monomial(vp);
  if (error()) return 0;
  return result;
}

Monomial* Monomial::make(const std::vector<int>& vp)
{
  Monomial* result = new Monomial(vp);
  return result;
}

unsigned int Monomial::computeHashValue() const
{
  unsigned int hashval = 0;
  const int *vp = val.raw();
  for (int i = 1; i <= *vp; i++)
    {
      hashval += i * (*++vp);
    }
  return hashval;
}

bool Monomial::is_one() const { return varpower::is_one(ints()); }
bool Monomial::is_equal(const Monomial &b) const
{
  if (this == &b) return true;
  return varpower::is_equal(ints(), b.ints());
}

int Monomial::compare(const Monoid *M, const Monomial &b) const
{
  int *monom1 = M->make_one();
  int *monom2 = M->make_one();
  M->from_varpower(ints(), monom1);
  M->from_varpower(b.ints(), monom2);
  int result = M->compare(monom1, monom2);
  M->remove(monom1);
  M->remove(monom2);
  return result;
}

bool Monomial::divides(const Monoid *M, const Monomial &b) const
{
  int *monom1 = M->make_one();
  int *monom2 = M->make_one();
  M->from_varpower(ints(), monom1);
  M->from_varpower(b.ints(), monom2);
  bool result = M->divides(monom1, monom2);
  M->remove(monom1);
  M->remove(monom2);
  return result;
}

int Monomial::simple_degree() const { return varpower::simple_degree(ints()); }
Monomial *Monomial::lcm(const Monomial &b) const
{
  Monomial *result = new Monomial;
  varpower::lcm(ints(), b.ints(), result->val);
  return result;
}

Monomial *Monomial::gcd(const Monomial &b) const
{
  Monomial *result = new Monomial;
  varpower::gcd(ints(), b.ints(), result->val);
  return result;
}

void Monomial::monsyz(const Monomial &b, Monomial *&sa, Monomial *&sb) const
{
  sa = new Monomial;
  sb = new Monomial;
  varpower::monsyz(ints(), b.ints(), sa->val, sb->val);
}

Monomial *Monomial::operator*(const Monomial &b) const
{
  Monomial *result = new Monomial;
  varpower::mult(ints(), b.ints(), result->val);
  if (error()) return 0;
  return result;
}

Monomial *Monomial::operator/(const Monomial &b) const
{
  Monomial *result = new Monomial;
  varpower::quotient(ints(), b.ints(), result->val);
  return result;
}

Monomial *Monomial::erase(const Monomial &b) const
{
  Monomial *result = new Monomial;
  varpower::erase(ints(), b.ints(), result->val);
  return result;
}

Monomial *Monomial::power(int n) const
{
  Monomial *result = new Monomial;
  varpower::power(ints(), n, result->val);
  if (error()) return 0;
  return result;
}

Monomial *Monomial::radical() const
{
  Monomial *result = new Monomial;
  varpower::radical(ints(), result->val);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
