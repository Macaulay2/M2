#include "interface/factory.h"
#include "interface/ring.h"

// mps uses the c++ keyword register, which is no longer allowed in the C++17
// standard, so we disable it by defining an empty macro.
#define register
#include <mps/mps.h>
#undef register
#include <stdlib.h>

#include "aring-CCC.hpp"
#include "aring.hpp"
#include "error.h"
#include "monoid.hpp"
#include "polyring.hpp"
#include "relem.hpp"
#include "ring.hpp"
#include "ringelem.hpp"

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
  if (p->is_zero()) {
    ERROR("expected a nonzero polynomial");
    return nullptr;    
  }
  const Ring *K = P->getCoefficients();
  int lodeg,hideg; // lowest,highest degree
  P->degree_of_var(0,p->get_value(),lodeg,hideg);
  if (prec == -1)
    prec = (K->get_precision() == 0 ? 53 : K->get_precision());
  
  mps_context *s = mps_context_new ();
  mps_monomial_poly *mps_p = mps_monomial_poly_new (s, hideg);
  mps_context_select_algorithm(s, MPS_ALGORITHM_SECULAR_GA);

  const auto ID = K->ringID();
  for (Nterm *t = p->get_value(); t != nullptr; t = t->next) {
    int deg;
    M->to_expvector(t->monom, &deg); // number of vars = 1
    if (ID==M2::ring_RR or ID==M2::ring_CC) {
      cc_doubles_struct cc;
      if (ID==M2::ring_RR) {
        cc.re = t->coeff.get_double();
        cc.im = 0;
      } else cc = *t->coeff.get_cc_doubles();
      mps_monomial_poly_set_coefficient_d (s, mps_p, deg, cc.re, cc.im); 
    } else if (ID==M2::ring_RRR or ID==M2::ring_CCC) {
      mpc_t mpc_cc;
      mpc_init2(mpc_cc,K->get_precision());
      if (ID==M2::ring_RRR) {
          mpfr_get_f(mpc_cc->r,t->coeff.get_mpfr(),MPFR_RNDN);
      } else {
        mpfr_get_f(mpc_cc->r,&t->coeff.get_cc()->re,MPFR_RNDN);
        mpfr_get_f(mpc_cc->i,&t->coeff.get_cc()->im,MPFR_RNDN);
      }
      mps_monomial_poly_set_coefficient_f (s, mps_p, deg, mpc_cc); 
      mpc_clear(mpc_cc);
    } else if ((ID==M2::ring_old and K->is_ZZ()) or ID==M2::ring_QQ) {
      mpq_t q, zero;
      mpq_init(zero);
      mpq_init(q);
      if (ID==M2::ring_QQ)
        mpq_set(q, t->coeff.get_mpq());
      else mpq_set_z(q, t->coeff.get_mpz());
      mps_monomial_poly_set_coefficient_q(s, mps_p, deg, q, zero); 
      mpq_clear(q);
      mpq_clear(zero);
    } else {
      ERROR("'roots' expects coefficients in ZZ, QQ, RR or CC");
      return nullptr;
    }
  }

  /* Set the input polynomial */
  mps_context_set_input_poly (s, MPS_POLYNOMIAL (mps_p));
  mps_context_set_output_prec (s, prec);
  mps_context_set_output_goal (s, MPS_OUTPUT_GOAL_APPROXIMATE);

  /* Actually solve the polynomial */
  mps_mpsolve (s);

  // result to be returned
  engine_RawRingElementArrayOrNull result = nullptr;
  result = getmemarraytype(engine_RawRingElementArray, hideg);
  result->len = static_cast<int>(hideg);

  ////////////////
  if (prec <= 53) {
    /* Allocate space to hold the results. We check only floating point results in here */
    cplx_t *result_mps = cplx_valloc (hideg);

    /* Save roots computed in the vector results */
    mps_context_get_roots_d (s, &result_mps, nullptr);

    /* Print out roots */
    // for (int i = 0; i < hideg; i++) {
    //   cplx_out_str (stdout, result_mps[i]);
    //   printf ("\n");
    // }

    ///////////////////////////////
    // copy to mps_result to result

    const Ring *C = IM2_Ring_CCC(prec);
    for (int i = 0; i < hideg; i++) {
      auto& mps_root = result_mps[i];
      ring_elem m2_root;
      C->from_complex_double(cplx_Re(mps_root), cplx_Im(mps_root),
                             m2_root);
      result->array[i] = RingElement::make_raw(C, m2_root);
    }
 
    free (result_mps);
  } else { // prec > 53
    mpc_t *roots = nullptr;
    rdpe_t *radii = nullptr;

    mps_context_get_roots_m (s, &roots, &radii);
    /* Sort roots in the order of increasing real part */
    //sort_roots(hideg, roots, radii);
    // for (int i = 0; i < hideg; i++) {
    //   mpc_out_str (stdout, 10, prec/4, roots[i]);
    //   printf ("\n");
    // }
    
    const Ring *C = IM2_Ring_CCC(prec);
    const M2::ARingCCC C0(prec);

    M2::ARingCCC::ElementType cc;
    C0.init(cc);
    for (int i = 0; i < hideg; i++) {
      auto& mps_root = roots[i];
      mpfr_set_f(&cc.re,mpc_Re(mps_root),MPFR_RNDN);
      mpfr_set_f(&cc.im,mpc_Im(mps_root),MPFR_RNDN);
      ring_elem m2_root;
      C0.to_ring_elem(m2_root, cc);
      result->array[i] = RingElement::make_raw(C, m2_root);
    }
    C0.clear(cc);
        
    free(roots);
    free(radii);
  }

  mps_monomial_poly_free(s, (mps_polynomial *)mps_p);
  mps_context_free(s);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
