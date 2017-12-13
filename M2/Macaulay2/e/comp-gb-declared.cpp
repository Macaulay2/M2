#include "gbring.hpp"
#include "comp-gb-declared.hpp"
#include "reducedgb-marked.hpp"
#include "matrix.hpp"
#include "reducedgb.hpp"
#include "polyring.hpp"

#include <vector>
#include <memory>
// Operations:
// 1. translate from Matrix type
// 2. sort in an order that works for minimalization
//   sort monomials
//   sort monomials and coefficients
// 3. minimalize
// 4. auto-reduce
// 5. remainder/lift.

// maybe POLY should really be:
using NEWPOLY = std::pair<std::unique_ptr<gbvector>, std::unique_ptr<gbvector>>;

VECTOR(POLY) fromMatrix(const Matrix* gb, // must be non-null
                        const Matrix* change) // can be nullptr
{
  VECTOR(NEWPOLY) elemsNew;
  VECTOR(POLY) elems;

  const Ring *R = gb->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  assert(P != nullptr);
  GBRing *GR = P->get_gb_ring();
  const Ring *K = GR->get_flattened_coefficients();
  
  const FreeModule *F = gb->rows();
  const FreeModule *Fsyz = change->rows();

  for (int i = 0; i < gb->n_cols(); i++)
    {
      POLY g;
      ring_elem denom1, denom2, u, v;

      if (gb->elem(i) == nullptr)
        continue;  // Do not even consider including 0 elements.
      g.f = P->translate_gbvector_from_vec(F, gb->elem(i), denom1);
      g.fsyz = P->translate_gbvector_from_vec(Fsyz, change->elem(i), denom2);

      K->syzygy(denom1, denom2, u, v);
      GR->gbvector_mult_by_coeff_to(g.f, u);
      K->negate_to(v);
      GR->gbvector_mult_by_coeff_to(g.fsyz, v);

      elems.push_back(g);
    }
  return elems;
}

enum {
  FLAG_MINIMAL = 1,
  FLAG_SORTED = 2,
  FLAG_AUTO_REDUCE = 4
};

GBDeclared::GBDeclared(const Matrix *m0,
                       const Matrix *gb,
                       const Matrix *change,
                       const Matrix *syz0,
                       int flags0)
  : trimmed_gens(m0), syz(syz0), flags(flags0)
{
  bool is_minimal = flags % FLAG_MINIMAL;
  bool is_sorted = flags % FLAG_SORTED;
  bool do_reduce = flags & FLAG_AUTO_REDUCE;
  set_status(COMP_DONE);
  const Ring *R = gb->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  GBRing *GR = P->get_gb_ring();

  const FreeModule *F = m0->rows();
  const FreeModule *Fsyz = change->rows();

  G = ReducedGB::create(P, F, Fsyz);

  // Now add in the elements
  VECTOR(POLY) polys = fromMatrix(gb, change);
  G->set_gb(polys, is_minimal, is_sorted, do_reduce);
}

GBDeclared::GBDeclared(const Matrix *m0,
                       const Matrix *gb,
                       const Matrix *change,
                       const Matrix *syz0)
  : trimmed_gens(m0), syz(syz0), flags(0)
{
  set_status(COMP_DONE);
  const Ring *R = gb->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  GBRing *GR = P->get_gb_ring();
  const Ring *K = GR->get_flattened_coefficients();

  const FreeModule *F = m0->rows();
  const FreeModule *Fsyz = change->rows();

  G = ReducedGB::create(P, F, Fsyz);

  // Now add in the elements
  VECTOR(POLY) elems;
  for (int i = 0; i < gb->n_cols(); i++)
    {
      POLY g;
      ring_elem denom1, denom2, u, v;

      if (gb->elem(i) == 0)
        continue;  // Do not even consider including 0 elements.
      g.f = P->translate_gbvector_from_vec(F, gb->elem(i), denom1);
      g.fsyz = P->translate_gbvector_from_vec(Fsyz, change->elem(i), denom2);

      K->syzygy(denom1, denom2, u, v);
      GR->gbvector_mult_by_coeff_to(g.f, u);
      K->negate_to(v);
      GR->gbvector_mult_by_coeff_to(g.fsyz, v);

      elems.push_back(g);
    }
  G->minimalize(elems);
}

GBDeclared::GBDeclared(const Matrix *leadterms,
                       const Matrix *m0,
                       const Matrix *gb,
                       const Matrix *change,
                       const Matrix *syz0)
    : trimmed_gens(m0), syz(syz0)
{
  set_status(COMP_DONE);
  const Ring *R = gb->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  GBRing *GR = P->get_gb_ring();
  const Ring *K = GR->get_flattened_coefficients();

  const FreeModule *F = m0->rows();
  const FreeModule *Fsyz = change->rows();

  MarkedGB *G0 = MarkedGB::create(P, F, Fsyz);
  G = G0;

  // Now add in the elements
  VECTOR(POLY) elems;
  VECTOR(gbvector *) leads;
  for (int i = 0; i < gb->n_cols(); i++)
    {
      POLY g;
      gbvector *lead;
      ring_elem denom1, denom2, denom3, u, v;

      if (gb->elem(i) == 0)
        continue;  // Do not even consider including 0 elements.
      g.f = P->translate_gbvector_from_vec(F, gb->elem(i), denom1);
      g.fsyz = P->translate_gbvector_from_vec(Fsyz, change->elem(i), denom2);
      lead = P->translate_gbvector_from_vec(F, leadterms->elem(i), denom3);
      K->syzygy(denom1, denom2, u, v);
      GR->gbvector_mult_by_coeff_to(g.f, u);
      K->negate_to(v);
      GR->gbvector_mult_by_coeff_to(g.fsyz, v);

      elems.push_back(g);
      leads.push_back(lead);
    }
  G0->add_marked_elems(leads, elems, true);
}

GBComputation *GBDeclared::create(const Matrix *m,
                                  const Matrix *gb,
                                  const Matrix *change,
                                  const Matrix *syz,
                                  int flags)
{
  // Check:
  //   the rings are all the same, and all are not NULL.
  //   m->rows(), gb->rows() are the same
  //   change->rows(), syz->rows() are the same.
  assert(m != 0 && gb != 0 && change != 0 && syz != 0);
  const Ring *R = gb->get_ring();
  if (R != m->get_ring() || R != change->get_ring() || R != syz->get_ring())
    {
      ERROR("expected the same ring");
      return 0;
    }

  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("declaring a GB requires a polynomial ring");
      return 0;
    }
  // Then: create and return the object
  if (flags != 0)
    return new GBDeclared(m, gb, change, syz, flags);
  else
    return new GBDeclared(m, gb, change, syz);
}

GBComputation *GBDeclared::create(const Matrix *leadterms,
                                  const Matrix *m,
                                  const Matrix *gb,
                                  const Matrix *change,
                                  const Matrix *syz)
{
  // Check:
  //   the rings are all the same, and all are not NULL.
  //   m->rows(), gb->rows() are the same
  //   change->rows(), syz->rows() are the same.
  assert(leadterms != 0 && m != 0 && gb != 0 && change != 0 && syz != 0);
  const Ring *R = gb->get_ring();
  if (R != m->get_ring() || R != leadterms->get_ring() ||
      R != change->get_ring() || R != syz->get_ring())
    {
      ERROR("expected the same ring");
      return 0;
    }

  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("declaring a GB requires a polynomial ring");
      return 0;
    }
  if (leadterms->n_rows() != gb->n_rows() ||
      leadterms->n_cols() != gb->n_cols())
    {
      ERROR(
          "expected same number of lead terms as marked Groebner basis "
          "elements");
      return 0;
    }
  // Then: create and return the object
  return new GBDeclared(leadterms, m, gb, change, syz);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
