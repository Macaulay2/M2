// (c) 1995 Michael E. Stillman

#include "monomial.hpp"

stash *monomial_rec::mystash;

monomial::monomial(char *&s, int &len)
 :  obj(new monomial_rec)
{
  get_intarray().shrink(0);
  varpower::from_binary(s, len, get_intarray());
}

int monomial::compare(const monomial &b) const
{
  return varpower::compare(ints(), b.ints());
}

int monomial::simple_degree() const
{
  return varpower::simple_degree(ints());
}

int monomial::divides(const monomial &b) const
{
  return varpower::divides(ints(), b.ints());
}

monomial monomial::lcm(const monomial &b) const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::lcm(ints(), b.ints(), result.get_intarray());
  return result;
}

monomial monomial::gcd(const monomial &b) const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::gcd(ints(), b.ints(), result.get_intarray());
  return result;
}

void monomial::monsyz(const monomial &b, monomial &sa, monomial &sb) const
{
  sa.get_intarray().shrink(0);
  sb.get_intarray().shrink(0);
  varpower::monsyz(ints(), b.ints(), 
		    sa.get_intarray(), sb.get_intarray());
}

monomial monomial::operator*(const monomial &b) const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::mult(ints(), b.ints(), result.get_intarray());
  return result;
}

monomial monomial::operator/(const monomial &b) const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::divide(ints(), b.ints(), result.get_intarray());
  return result;
}

monomial monomial::erase(const monomial &b) const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::erase(ints(), b.ints(), result.get_intarray());
  return result;
}

monomial monomial::power(int n) const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::power(ints(), n, result.get_intarray());
  return result;
}

monomial monomial::radical() const
{
  monomial result(0);
  result.get_intarray().shrink(0);
  varpower::radical(ints(), result.get_intarray());
  return result;
}
