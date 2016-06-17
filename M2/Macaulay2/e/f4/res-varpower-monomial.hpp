/* Copyright 2006-2016 by Michael E. Stillman */

#ifndef _res_varpower_monomial_hpp_
#define _res_varpower_monomial_hpp_

#include "res-monomial-types.hpp"

//#include <cstdio>

#if !defined(SAFEC_EXPORTS)
#include <engine-exports.h>
#endif

class res_varpower_monomials {
public:
  static long length(res_const_varpower_monomial m) { return ((*(m))*2+1); }

  static res_varpower_word simple_degree(res_const_varpower_monomial m);

  static res_varpower_word weight(res_const_varpower_monomial m,
                              M2_arrayint wts);

  static int equal(res_const_varpower_monomial m1,
                   res_const_varpower_monomial m2);

  static int compare(res_const_varpower_monomial m1,
                     res_const_varpower_monomial m2); // Which compare is this?

  static void mult(res_const_varpower_monomial m1,
                   res_const_varpower_monomial m2,
                   res_varpower_monomial result);

  static void quotient(res_const_varpower_monomial m1,
                       res_const_varpower_monomial m2,
                       res_varpower_monomial result);

  static void lcm(res_const_varpower_monomial m1,
                  res_const_varpower_monomial m2,
                  res_varpower_monomial result);

  static int divides(res_const_varpower_monomial m1,
                     res_const_varpower_monomial m2,
                     res_varpower_monomial result);

  static void elem_text_out(FILE *fil,
                            res_const_varpower_monomial m);

};

class index_res_varpower_monomial
{
  res_const_varpower_monomial loc;
  res_const_varpower_monomial hi;
public:
  index_res_varpower_monomial() : loc(0), hi(0) {}
  index_res_varpower_monomial(res_const_varpower_monomial m) : loc(m+1), hi(m+(2*(*m))) {}

                                     //  index_monomial(const int *m, int)
                                     //    : lo(m+1), hi(m+*m-2) { loc = hi; }

  index_res_varpower_monomial(const index_res_varpower_monomial &i) : loc(i.loc), hi(i.hi) {}

  bool valid() { return loc < hi; }
  index_res_varpower_monomial &operator++() { loc += 2; return *this; }

  //  index_monomial &operator--() { loc -= 2; return *this; }

  res_varpower_word var() { return *loc; }
  res_varpower_word exponent() { return loc[1]; }
};


#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
