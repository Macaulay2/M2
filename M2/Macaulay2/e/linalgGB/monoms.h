/* Copyright 2004 by Michael E. Stillman */

#ifndef _monoms_h_
#define _monoms_h_

#include <stdio.h>
#include "../engine.h"
typedef int * uninterned_monomial;

#define MONOMIAL_LENGTH(m) ((*(m))*2+1)

/* Monomial routines */
#if defined(__cplusplus)
extern "C" {
#endif

  int monomial_simple_degree(uninterned_monomial m);

  int monomial_weight(uninterned_monomial, M2_arrayint wts);

  int monomial_equal(uninterned_monomial m1,
		     uninterned_monomial m2);

  int monomial_compare(uninterned_monomial m1,
		     uninterned_monomial m2);
  
  void monomial_mult(uninterned_monomial m1, 
		     uninterned_monomial m2,
		     uninterned_monomial result);

  void monomial_quotient(uninterned_monomial m1, 
			 uninterned_monomial m2,
			 uninterned_monomial result);

  void monomial_lcm(uninterned_monomial m1, 
		    uninterned_monomial m2,
		    uninterned_monomial result);

  int monomial_divides(uninterned_monomial m1, 
		       uninterned_monomial m2,
		       uninterned_monomial result);

  void monomial_elem_text_out(FILE *fil, 
			      uninterned_monomial m);
  
#if defined(__cplusplus)
}
#endif

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
*/
