// (c) 1995 Michael E. Stillman

#include "monomial.hpp"

stash *monomial_rec::mystash;

Monomial::Monomial(char *&s, int &len)
 :  obj(new monomial_rec)
{
  get_intarray().shrink(0);
  varpower::from_binary(s, len, get_intarray());
}

int Monomial::compare(const Monomial &b) const
{
  return varpower::compare(ints(), b.ints());
}

int Monomial::simple_degree() const
{
  return varpower::simple_degree(ints());
}

int Monomial::divides(const Monomial &b) const
{
  return varpower::divides(ints(), b.ints());
}

Monomial Monomial::lcm(const Monomial &b) const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::lcm(ints(), b.ints(), result.get_intarray());
  return result;
}

Monomial Monomial::gcd(const Monomial &b) const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::gcd(ints(), b.ints(), result.get_intarray());
  return result;
}

void Monomial::monsyz(const Monomial &b, Monomial &sa, Monomial &sb) const
{
  sa.get_intarray().shrink(0);
  sb.get_intarray().shrink(0);
  varpower::monsyz(ints(), b.ints(), 
		    sa.get_intarray(), sb.get_intarray());
}

Monomial Monomial::operator*(const Monomial &b) const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::mult(ints(), b.ints(), result.get_intarray());
  return result;
}

Monomial Monomial::operator/(const Monomial &b) const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::divide(ints(), b.ints(), result.get_intarray());
  return result;
}

Monomial Monomial::erase(const Monomial &b) const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::erase(ints(), b.ints(), result.get_intarray());
  return result;
}

Monomial Monomial::power(int n) const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::power(ints(), n, result.get_intarray());
  return result;
}

Monomial Monomial::radical() const
{
  Monomial result(0);
  result.get_intarray().shrink(0);
  varpower::radical(ints(), result.get_intarray());
  return result;
}
