#include "reducedgb.hpp"

#include "matrix-con.hpp"
#include "poly.hpp"
#include "matrix.hpp"

#include "reducedgb-field.hpp"
#include "reducedgb-field-local.hpp"
#include "reducedgb-ZZ.hpp"

ReducedGB *ReducedGB::create(const PolynomialRing *originalR0,
			     const FreeModule *F0,
			     const FreeModule *Fsyz0,
			     const GBWeight *wt0) // better be 0 or set with same F0
{
  // Depending on whether the ring is over a field, or over ZZ, or
  // has local variables, we create a different class.

  bool over_ZZ = originalR0->coefficient_type() == Ring::COEFF_ZZ;
  M2_arrayint local_vars = originalR0->getMonoid()->getNonTermOrderVariables();
  bool is_local = (local_vars && local_vars->len > 0);
  GBRing *R = originalR0->get_gb_ring();

  if (over_ZZ)
    return new ReducedGB_ZZ(R,originalR0,F0,Fsyz0);
  else
    {
      if (is_local)
	return new ReducedGB_Field_Local(R,originalR0,F0,Fsyz0,wt0);
      else
	return new ReducedGB_Field(R,originalR0,F0,Fsyz0);
    }
}


ReducedGB::ReducedGB(GBRing *R0,
		     const PolynomialRing *originalR0,
		     const FreeModule *F0,
		     const FreeModule *Fsyz0) 
  : R(R0), 
    originalR(originalR0), 
    F(F0), 
    Fsyz(Fsyz0)
{
  set_status(COMP_DONE);
}

ReducedGB::~ReducedGB()
{
  for (int i=0; i<polys.size(); i++)
    {
      R->gbvector_remove(polys[i].f);
      R->gbvector_remove(polys[i].fsyz);
    }
}

const MatrixOrNull *ReducedGB::get_gb()
{
  MatrixConstructor mat(F,0);
  for (VECTOR(POLY)::const_iterator i = polys.begin(); i != polys.end(); i++)
    {
      vec v = originalR->translate_gbvector_to_vec(F, (*i).f);
      mat.append(v);
    }
  return mat.to_matrix();
}

const MatrixOrNull *ReducedGB::get_mingens()
{
#ifdef DEVELOPMENT
#warning "mingens?"
#endif
  return 0;
}

const MatrixOrNull *ReducedGB::get_syzygies()
{
#ifdef DEVELOPMENT
#warning "syzygies?"
#endif
  return 0;
}

const MatrixOrNull *ReducedGB::get_change()
{
  MatrixConstructor mat(Fsyz,0);
  for (VECTOR(POLY)::const_iterator i = polys.begin(); i != polys.end(); i++)
    {
      vec v = originalR->translate_gbvector_to_vec(Fsyz, (*i).fsyz);
      mat.append(v);
    }
  return mat.to_matrix();
}

const MatrixOrNull *ReducedGB::get_initial(int nparts)
{
  MatrixConstructor mat(F,0);
  for (VECTOR(POLY)::const_iterator i = polys.begin(); i != polys.end(); i++)
    {
      gbvector *f = R->gbvector_lead_term(nparts, F, (*i).f);
      mat.append(originalR->translate_gbvector_to_vec(F, f));
    }
  return mat.to_matrix();
}

void ReducedGB::text_out(buffer &o) const
{
}

const MatrixOrNull *ReducedGB::matrix_remainder(const Matrix *m)
{
  if (m->get_ring() != originalR)
    {
      ERROR("expected matrix over the same ring");
      return 0;
    }

  if (m->n_rows() != F->rank()) {
       ERROR("expected matrices to have same number of rows");
       return 0;
  }

  MatrixConstructor red(m->rows(), m->cols(), m->degree_shift());
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *g = originalR->translate_gbvector_from_vec(F, (*m)[i], denom);

      remainder(g, true, denom);

      vec fv = originalR->translate_gbvector_to_vec_denom(F, g, denom);
      red.set_column(i, fv);
    }
  return red.to_matrix();

}

void ReducedGB::matrix_lift(const Matrix *m,
			    MatrixOrNull **result_remainder,
			    MatrixOrNull **result_quotient)
{
  if (m->get_ring() != originalR)
    {
      ERROR("expected matrix over the same ring");
      *result_remainder = 0;
      *result_quotient = 0;
      return;
    }
  if (m->n_rows() != F->rank()) {
       ERROR("expected matrices to have same number of rows");
      *result_remainder = 0;
      *result_quotient = 0;
      return;
  }

  MatrixConstructor mat_remainder(m->rows(), m->cols(), m->degree_shift());
  MatrixConstructor mat_quotient(Fsyz, m->cols(), 0);

#ifdef DEVELOPMENT
#warning "K should be the denominator ring?"
#endif
  const Ring *K = R->get_flattened_coefficients();
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = originalR->translate_gbvector_from_vec(F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, true, denom);

      vec fv = originalR->translate_gbvector_to_vec_denom(F, g.f, denom);
      K->negate_to(denom);
      vec fsyzv = originalR->translate_gbvector_to_vec_denom(Fsyz,g.fsyz, denom);
      mat_remainder.set_column(i, fv);
      mat_quotient.set_column(i, fsyzv);
    }
  *result_remainder = mat_remainder.to_matrix();
  *result_quotient = mat_quotient.to_matrix();
}

int ReducedGB::contains(const Matrix *m)
{
  // Reduce each column of m one by one.
  if (m->get_ring() != originalR)
    {
      ERROR("expected matrix over the same ring");
      return -2;
    }

  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *g = originalR->translate_gbvector_from_vec(F,(*m)[i], denom);

      remainder(g, false, denom);

      if (g != NULL)
	{
	  R->gbvector_remove(g);
	  return i;
	}
    }
  return -1;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
