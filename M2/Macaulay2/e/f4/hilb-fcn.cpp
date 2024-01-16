/* Copyright 2010-2021, Michael E. Stillman */

#include "hilb-fcn.hpp"

#include "error.h"            // for error
#include "freemod.hpp"        // for FreeModule
#include "hilb.hpp"           // for hilb_comp
#include "matrix-con.hpp"     // for MatrixConstructor
#include "monoid.hpp"         // for Monoid, monomial
#include "polyring.hpp"       // for PolynomialRing
#include "relem.hpp"          // for RingElement
#include "ring.hpp"           // for Ring
#include "style.hpp"          // for INTSIZE

#include <cstdio>             // for fprintf, stderr
#include <vector>             // for vector

class Matrix;

HilbertController::HilbertController(const FreeModule *F0,
                                     const RingElement *hf)
    : F(F0),
      leadterms(0),  // set later
      hilb_new_elems(true),
      hilb_n_in_degree(0),  // will be really set later
      hf_orig(hf),
      hf_diff(0)  // will be set later
{
  // TODO: check and write
  R = F->get_ring()->cast_to_PolynomialRing();
  fprintf(stderr, "initializing hilbert controller\n");
}

HilbertController::~HilbertController()
{
  // TODO: check and write
  delete leadterms;
  F = 0;
  hf_orig = 0;
  hf_diff = 0;
}

bool HilbertController::setDegree(int this_degree)
{
  // Recomputes Hilbert function, returns #elems expected in degree this_degree
  // There should be NO elements expected in lower degrees than that.
  // false is returned if either the computation was interrupted, or
  // it was determined that the Hilbert function could not be correct
  // (by expecting a negative number of elements).
  if (!recomputeHilbertFunction()) return false;
  hilb_n_in_degree = hilb_comp::coeff_of(hf_diff, this_degree);
  if (error()) return false;
  return true;
}

bool HilbertController::addMonomial(int *a, int comp)
// return true if that was the last one expected
{
  monomial m = R->getMonoid()->make_one();
  R->getMonoid()->from_expvector(a, m);
  ring_elem r = R->make_flat_term(R->getCoefficientRing()->one(), m);
  vec v = R->make_vec(comp - 1, r);
  elems.push_back(v);
  hilb_new_elems = true;
  hilb_n_in_degree--;
  return hilb_n_in_degree == 0;
}

bool HilbertController::recomputeHilbertFunction()
{
  if (hilb_new_elems)
    {
      // Recompute h, hf_diff
      Matrix *M = make_lead_term_matrix();
      RingElement *h = hilb_comp::hilbertNumerator(M);
      if (h == 0) return false;  // computation was interrupted
      hf_diff = (*h) - (*hf_orig);
      hilb_new_elems = false;
    }
  return true;
}

Matrix *HilbertController::make_lead_term_matrix()
{
  MatrixConstructor mat(F, INTSIZE(elems));
  for (int i = 0; i < elems.size(); i++) mat.set_column(i, elems[i]);
  return mat.to_matrix();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
