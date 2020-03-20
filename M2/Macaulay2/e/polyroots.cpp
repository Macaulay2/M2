#include <mps/mps.h>
#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>
#include <string.h>

#include "polyroots.hpp"
#define abs(x) (((x) < 0) ? -(x) : (x))
#define max(a, b) (((a) > (b)) ? (a) : (b))

engine_RawRingElementArrayOrNull rawRoots(const RingElement *p,
                                          long prec,
                                          int unique)
{
  const Ring *R = p->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  const Monoid *M = P->getMonoid();
  if (P == 0) {
    ERROR("expected a polynomial ring");
    return nullptr;
  }
  if (P->n_vars() != 1) {
    ERROR("expected a univariate polynomial ring");
    return nullptr;
  }
  const Ring *K = P->getCoefficients();
  int lodeg,hideg; // lowest,highest degree
  P->degree_of_var(0,p->get_value(),lodeg,hideg);
  if (prec == -1)
    prec = (K->get_precision() == 0 ? 53 : K->get_precision());
    
  ///////////// TODO: implement roots of a polynomial, via e.g. arb or mpsolve.
  mps_context *s = mps_context_new ();
  mps_monomial_poly *mps_p = mps_monomial_poly_new (s, hideg);
  mps_context_select_algorithm(s, MPS_ALGORITHM_SECULAR_GA);
      
  if (K->ringID() != M2::ring_RR) {
    ERROR("'roots' is implemented only for RR coefficients.");
    return nullptr;
  }

  for (Nterm *t = p->get_value(); t != nullptr; t = t->next) {
    double c = t->coeff.get_double();
    int deg;
    M->to_expvector(t->monom, &deg); // number of vars = 1
    mps_monomial_poly_set_coefficient_d (s, mps_p, deg, c, 0); 
  }

  /* Set the input polynomial */
  mps_context_set_input_poly (s, MPS_POLYNOMIAL (mps_p));

  /* Allocate space to hold the results. We check only floating point results
   * in here */
  cplx_t *result_mps = cplx_valloc (hideg);

  /* Actually solve the polynomial */
  mps_mpsolve (s);

  /* Save roots computed in the vector results */
  mps_context_get_roots_d (s, &result_mps, nullptr);

  /* Print out roots */
  for (int i = 0; i < hideg; i++) {
    cplx_out_str (stdout, result_mps[i]);
    printf ("\n");
  }

  ///////////////////////////////
  // copy to mps_result to result
  engine_RawRingElementArrayOrNull result = nullptr;

  const size_t num_roots = hideg; // is this correct for non-squarefree?
  result = getmemarraytype(engine_RawRingElementArray, num_roots);
  result->len = static_cast<int>(num_roots);
  ring_elem m2_root;

  const RingCC *CC = dynamic_cast<const RingCC *>(IM2_Ring_CCC(prec)); // ???
  for (int i = 0; i < hideg; i++) {
    auto& mps_root = result_mps[i];
    const complex root = {cplx_Re(mps_root),cplx_Im(mps_root)};
    CC->ring().to_ring_elem(m2_root, root);
    result->array[i] = RingElement::make_raw(CC, m2_root);
  }

  free (result_mps);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
