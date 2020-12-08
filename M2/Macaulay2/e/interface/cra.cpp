#include "interface/cra.h"

#include "cra.hpp"
#include "error.h"
#include "freemod.hpp"
#include "matrix-con.hpp"
#include "matrix.hpp"
#include "monoid.hpp"
#include "poly.hpp"
#include "relem.hpp"
#include "ring.hpp"
#include "ringelem.hpp"

const RingElement *rawRingElementCRA(const RingElement *f,
                                     const RingElement *g,
                                     mpz_srcptr m,
                                     mpz_srcptr n)
{
  // Assumption: f and g are either in ZZ, or in a polynomial ring whose coeff
  // ring is ZZ.  The output is a ring element in the same ring.

  const Ring *Rf = f->get_ring();
  const Ring *Rg = g->get_ring();
  if (Rf != Rg)
    {
      ERROR("expected same ring");
      return 0;
    }
  const PolyRing *P = Rf->cast_to_PolyRing();
  if (P == 0)
    {
      // check whether Rf is ZZ.  If not, error.
      if (!Rf->is_ZZ())
        {
          ERROR("expected ZZ, or polynomial ring over ZZ");
          return 0;
        }
      ERROR("not implemented yet");
      return 0;
    }
  else
    {
      const Ring *K = P->getCoefficientRing();
      if (K->is_ZZ())
        {
          ring_elem rf = f->get_value();
          ring_elem rg = g->get_value();
          ring_elem result = ChineseRemainder::CRA(P, rf, rg, m, n);
          return RingElement::make_raw(Rf, result);
        }
      else
        {
          ERROR("expected coefficient ring to be ZZ");
          return 0;
        }
    }
  ERROR("not written yet");
  return 0;
}

const Matrix *rawMatrixCRA(const Matrix *f, const Matrix *g, mpz_srcptr m, mpz_srcptr n)
{
  // Error handling:
  if (f->get_ring() != g->get_ring())
    {
      ERROR("matrices have different base rings");
      return 0;
    }
  if (f->rows()->rank() != g->rows()->rank() ||
      f->cols()->rank() != g->cols()->rank())
    {
      ERROR("matrices have different shapes");
      return 0;
    }

  // Assumption: f and g are either matrices over ZZ, or over a polynomial ring
  // whose coeff
  // ring is ZZ.  The output is a matrix in the same ring.

  mpz_t um, vn, mn;
  mpz_init(um);
  mpz_init(vn);
  mpz_init(mn);
  ChineseRemainder::computeMultipliers(m, n, um, vn, mn);
  mpz_t result_coeff;
  mpz_init(result_coeff);
  Matrix *result = ChineseRemainder::CRA(f, g, um, vn, mn);
  mpz_clear(um);
  mpz_clear(vn);
  mpz_clear(mn);
  return result;
}

const RingElement *rawRingElementRatConversion(const RingElement *f,
                                               mpz_srcptr m,
                                               const Ring *RQ)
{
  const Ring *Rf = f->get_ring();
  const PolyRing *P = Rf->cast_to_PolyRing();
  const PolyRing *PQ = RQ->cast_to_PolyRing();

  if (P == 0)
    {
      // check whether Rf is ZZ.  If not, error.
      if (!Rf->is_ZZ())
        {
          ERROR("expected ZZ, or polynomial ring over ZZ");
          return 0;
        }
      ERROR("not implemented yet");
      return 0;
    }
  else
    {
      const Ring *K = P->getCoefficientRing();
      if (K->is_ZZ())
        {
          ring_elem rf = f->get_value();
          ring_elem result = ChineseRemainder::ratConversion(rf, m, PQ);
          return RingElement::make_raw(PQ, result);  // debug this line!
        }
      else
        {
          ERROR("expected coefficient ring to be ZZ");
          return 0;
        }
    }
  ERROR("not written yet");
  return 0;
}

// f should be an element in the polynomial ring R (over ZZ).
// RQ should be the same ring as R, but with rational coefficients

const Matrix *rawMatrixRatConversion(const Matrix *f, mpz_srcptr m, const Ring *RQ)
{
  const PolyRing *R = f->get_ring()->cast_to_PolyRing();
  const PolyRing *PQ = RQ->cast_to_PolyRing();

  if (R == 0)
    {
      ERROR("expected polynomial ring over ZZ");
      return 0;
    }

  const FreeModule *F = f->rows();
  const FreeModule *G = f->cols();
  const FreeModule *FQ = PQ->make_FreeModule(F->rank());
  const FreeModule *GQ = PQ->make_FreeModule(G->rank());

  const int *deg;

  deg = f->degree_monoid()->make_one();

  MatrixConstructor mat(FQ, GQ, deg);
  for (int i = 0; i < f->n_cols(); i++)
    {
      vec u = ChineseRemainder::ratConversion(f->elem(i), m, PQ);
      mat.set_column(i, u);
    }
  return mat.to_matrix();
}

// Local Variables:
// indent-tabs-mode: nil
// End:
