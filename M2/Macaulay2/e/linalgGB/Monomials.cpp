#include "Monomials.hpp"
#include "../buffer.hpp"

void monomial_text_out(buffer &o, monomial m)
{
  index_monomial i = m;
  if (!i.valid())
    o << "1";
  else
    for ( ; i.valid(); ++i)
      {
	int v = i.var();
	int e = i.exponent();
	if (v < 26) o << char('a' + v);
	else if (v < 52) o << char('A' + v - 26);
	else o << "x[" << v << "]";
	if (e > 1) o << e;
	else if (e < 0) o << "^(" << e << ")";
      }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
