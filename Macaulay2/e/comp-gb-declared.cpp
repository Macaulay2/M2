#include "comp-gb-declared.hpp"
#include "reducedgb-marked.hpp"
#include "matrix.hpp"
#include "reducedgb.hpp"
#include "poly.hpp"

GBDeclared::GBDeclared(const Matrix *m0,
		       const Matrix *gb,
		       const Matrix *change,
		       const Matrix *syz0)
  : trimmed_gens(m0),
    syz(syz0)
{
  set_status(COMP_DONE);
  const Ring *R = gb->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  GBRing *GR = P->get_gb_ring();
  const Ring *K = GR->get_flattened_coefficients();

  const FreeModule *F = m0->rows();
  const FreeModule *Fsyz = change->rows();

  G = ReducedGB::create(P,F,Fsyz);

  // Now add in the elements
  VECTOR(POLY) elems;
  for (int i=0; i<gb->n_cols(); i++)
    {
      POLY g;
      ring_elem denom1,denom2,u,v;

      if (gb->elem(i) == 0) continue; // Do not even consider including 0 elements.
      g.f = P->translate_gbvector_from_vec(F, 
					    gb->elem(i), 
					    denom1);
      g.fsyz = P->translate_gbvector_from_vec(Fsyz, 
					       change->elem(i), 
					       denom2);

      K->syzygy(denom1,denom2,u,v);
      GR->gbvector_mult_by_coeff_to(g.f,u);
      K->negate_to(v);
      GR->gbvector_mult_by_coeff_to(g.fsyz,v);

      elems.push_back(g);
    }
  G->minimalize(elems);
}

GBDeclared::GBDeclared(const Matrix *leadterms,
		       const Matrix *m0,
		       const Matrix *gb,
		       const Matrix *change,
		       const Matrix *syz0)
  : trimmed_gens(m0),
    syz(syz0)
{
  set_status(COMP_DONE);
  const Ring *R = gb->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  GBRing *GR = P->get_gb_ring();
  const Ring *K = GR->get_flattened_coefficients();

  const FreeModule *F = m0->rows();
  const FreeModule *Fsyz = change->rows();

  MarkedGB *G0 = MarkedGB::create(P,F,Fsyz);
  G = G0;

  // Now add in the elements
  VECTOR(POLY) elems;
  VECTOR(gbvector *) leads;
  for (int i=0; i<gb->n_cols(); i++)
    {
      POLY g;
      gbvector *lead;
      ring_elem denom1,denom2,denom3,u,v;

      if (gb->elem(i) == 0) continue; // Do not even consider including 0 elements.
      g.f = P->translate_gbvector_from_vec(F, 
					    gb->elem(i), 
					    denom1);
      g.fsyz = P->translate_gbvector_from_vec(Fsyz, 
					       change->elem(i), 
					       denom2);
      lead = P->translate_gbvector_from_vec(F,
					    leadterms->elem(i),
					    denom3);
      K->syzygy(denom1,denom2,u,v);
      GR->gbvector_mult_by_coeff_to(g.f,u);
      K->negate_to(v);
      GR->gbvector_mult_by_coeff_to(g.fsyz,v);

      elems.push_back(g);
      leads.push_back(lead);
    }
  G0->add_marked_elems(leads,elems,true);
}

GBComputation *GBDeclared::create(const Matrix *m,
				  const Matrix *gb,
				  const Matrix *change,
				  const Matrix *syz)
{
  // Check:
  //   the rings are all the same, and all are not NULL.
  //   m->rows(), gb->rows() are the same
  //   change->rows(), syz->rows() are the same.
  assert(m != 0 && gb != 0 && change != 0 && syz != 0);
  const Ring *R = gb->get_ring();
  if (R != m->get_ring() 
      || R != change->get_ring()
      || R != syz->get_ring())
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
  return new GBDeclared(m,gb,change,syz);
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
  if (R != m->get_ring()
      || R != leadterms->get_ring()
      || R != change->get_ring()
      || R != syz->get_ring())
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
  if (leadterms->n_rows() != gb->n_rows()
      || leadterms->n_cols() != gb->n_cols())
    {
      ERROR("expected same number of lead terms as marked Groebner basis elements");
      return 0;
    }
  // Then: create and return the object
  return new GBDeclared(leadterms,m,gb,change,syz);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
