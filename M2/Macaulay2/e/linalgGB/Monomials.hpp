#ifndef __Monomials_h_
#define __Monomials_h_

typedef int * monomial;
typedef int * uninterned_monomial;

class index_monomial
{
  const int *loc;
  const int *hi;
public:
  index_monomial() : loc(0), hi(0) {}
  index_monomial(monomial m) : loc(m+1), hi(m+(2*(*m))) {}

				     //  index_monomial(const int *m, int) 
				     //    : lo(m+1), hi(m+*m-2) { loc = hi; }

  index_monomial(const index_monomial &i) : loc(i.loc), hi(i.hi) {}

  int valid() { return loc < hi; }
  index_monomial &operator++() { loc += 2; return *this; }

  //  index_monomial &operator--() { loc -= 2; return *this; }

  int var() { return *loc; }
  int exponent() { return loc[1]; }
};

class buffer;

void monomial_text_out(buffer &o, monomial m);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
