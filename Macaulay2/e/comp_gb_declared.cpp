#include "comp_gb_declared.hpp"
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

  const FreeModule *F = m0->rows();
  const FreeModule *Fsyz = change->rows();

  G = ReducedGB::create(P,F,Fsyz);

  // Now add in the elements
  std::vector<POLY, gc_allocator<POLY> > elems;
  for (int i=0; i<gb->n_cols(); i++)
    {
      POLY g;
      ring_elem result_denominator; // not used.
      g.f = P->translate_gbvector_from_vec(F, 
					    gb->elem(i), 
					    result_denominator);
      g.fsyz = P->translate_gbvector_from_vec(Fsyz, 
					       change->elem(i), 
					       result_denominator);
      elems.push_back(g);
    }
  G->minimalize(elems);
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
  
  // Then: create and return the object
  return new GBDeclared(m,gb,change,syz);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
