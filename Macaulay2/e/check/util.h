#ifndef __util_h_
#define __util_h_

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>

#include "engine.h"

M2_Integer make_integer(long int a);
M2_arrayint arrayint(int len, ...);
const Monomial *monom(int len, ...);
MonomialOrdering * monorder(int len, ...);
const RingElement_array *make_ringelem_array(int len, ...);

M2_stringarray make_names(char *s, int n);
Monoid *make_degree_monoid(int ndegs);
const Ring *make_poly_ring(int charac, int nvars);

void display_monomial(const Monomial *m);
void display_monorder(const MonomialOrdering *mo);
void display_monoid(const Monoid *M);
void display_ring(const Ring *R);
void display_relem(const RingElement *f);
void display_freemodule(const FreeModule *F);
void display_matrix(const Matrix *f);
void display_sparsemat(const MutableMatrix *f);

int is_eq(M2_string elem, char *answer);
int arrayint_is_eq(M2_arrayint a, M2_arrayint b);

#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check "
// End:
*/

