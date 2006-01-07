/* Copyright 2006 by Michael E. Stillman */

#ifndef _varpower_monomial_hpp_
#define _varpower_monomial_hpp_

#include <cstdio>
#include "../../d/M2types.h"

typedef long * varpower_monomial;
typedef const long * const_varpower_monomial;

class varpower_monomials {

  static long length(const_varpower_monomial m) { return ((*(m))*2+1); }

  static long simple_degree(const_varpower_monomial m);

  static long weight(const_varpower_monomial m, 
		     M2_arrayint wts);

  static int equal(const_varpower_monomial m1,
		   const_varpower_monomial m2);

  static int compare(const_varpower_monomial m1,
		     const_varpower_monomial m2); // Which compare is this?
  
  static void mult(const_varpower_monomial m1, 
		   const_varpower_monomial m2,
		   varpower_monomial result);
  
  static void quotient(const_varpower_monomial m1, 
		       const_varpower_monomial m2,
		       varpower_monomial result);

  static void lcm(const_varpower_monomial m1, 
		  const_varpower_monomial m2,
		  varpower_monomial result);

  static int divides(const_varpower_monomial m1, 
		     const_varpower_monomial m2,
		     varpower_monomial result);

  static void monomial_elem_text_out(FILE *fil, 
				     const_varpower_monomial m);
  
};

class index_varpower_monomial
{
  const long *loc;
  const long *hi;
public:
  index_varpower_monomial() : loc(0), hi(0) {}
  index_varpower_monomial(const_varpower_monomial m) : loc(m+1), hi(m+(2*(*m))) {}

				     //  index_monomial(const int *m, int) 
				     //    : lo(m+1), hi(m+*m-2) { loc = hi; }

  index_varpower_monomial(const index_varpower_monomial &i) : loc(i.loc), hi(i.hi) {}

  bool valid() { return loc < hi; }
  index_varpower_monomial &operator++() { loc += 2; return *this; }

  //  index_monomial &operator--() { loc -= 2; return *this; }

  long var() { return *loc; }
  long exponent() { return loc[1]; }
};


#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/
