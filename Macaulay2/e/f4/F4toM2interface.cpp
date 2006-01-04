// Copyright 2005 Michael E. Stillman
#include <vector>
#include "../polyring.hpp"
#include "../freemod.hpp"
#include "../matrixcon.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"

#include "F4toM2interface.hpp"
template<typename CoeffRing, typename MonInfo>
void F4toM2Interface<CoeffRing,MonInfo>::from_M2_vec(const CoeffRing *K,
					       const MonInfo *MI,
					       const FreeModule *F, 
					       vec v,
					       poly &result)
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  int n = 0;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	n++;
    }
  int *exp = newarray(int, M->n_vars()+1);
  long *lexp = newarray(long, M->n_vars()+1);

  result.len = n;
  result.coeffs = newarray(COEFF_TYPE, n);
  result.monoms = newarray(packed_monomial, n);
  result.monom_space = newarray(long, n * MI->max_monomial_size());
  n = 0;
  long *nextmonom = result.monom_space;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	{
	  K->from_ring_elem(result.coeffs[n], t->coeff);
	  M->to_expvector(t->monom, exp);
	  for (int a =0; a<M->n_vars(); a++)
	    lexp[a] = exp[a];
	  result.monoms[n] = nextmonom;
	  MI->from_exponent_vector(lexp, w->comp, nextmonom);
	  nextmonom += MI->max_monomial_size();
	  n++;
	}
    }
}

template<typename CoeffRing, typename MonInfo>
void F4toM2Interface<CoeffRing,MonInfo>::poly_set_degrees(const CoeffRing *K,
							  const MonInfo *MI,
							  const M2_arrayint wts,
							  const poly &f,
							  int &deg, 
							  int &alpha)
{
  int leaddeg = MI->monomial_weight(f.monoms[0], wts);
  deg = leaddeg;
  for (int i=1; i<f.len; i++)
    {
      int degi = MI->monomial_weight(f.monoms[i],wts);
      if (degi > deg) deg = degi;
    }
  alpha = deg-leaddeg;
}

template<typename CoeffRing, typename MonInfo>
void F4toM2Interface<CoeffRing,MonInfo>::from_M2_matrix(const CoeffRing *K,
							const MonInfo *MI,
							const Matrix *m, 
							M2_arrayint wts,
							gb_array &result_polys)
{
  const FreeModule *F = m->rows();
  for (int i=0; i<m->n_cols(); i++)
    {
      gbelem *g = new gbelem;
      from_M2_vec(K,MI,F,m->elem(i),g->f);
      if (wts != 0)
	poly_set_degrees(K,MI,wts,g->f,g->deg,g->alpha);
      result_polys.push_back(g);
    }
}

template<typename CoeffRing, typename MonInfo>
vec F4toM2Interface<CoeffRing,MonInfo>::to_M2_vec(const CoeffRing *K,
						  const MonInfo *MI,
						  const poly &f,
						  const FreeModule *F)
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  
  int *m1 = M->make_one();

  Nterm **comps = newarray(Nterm *, F->rank());
  Nterm **last = newarray(Nterm *, F->rank());
  for (int i=0; i<F->rank(); i++)
    {
      comps[i] = 0;
      last[i] = 0;
    }

  int *exp = newarray(int, M->n_vars()+1);
  long *lexp = newarray(long, M->n_vars()+1);

  for (int i=0; i<f.len; i++)
    {
      long comp;
      ring_elem c;
      K->to_ring_elem(c, f.coeffs[i]);
      MI->to_exponent_vector(f.monoms[i], lexp, comp);
      for (int a=0; a<M->n_vars(); a++)
	exp[a] = lexp[a];
      M->from_expvector(exp, m1);
      Nterm * g = R->make_flat_term(c, m1);
      g->next = 0;
      if (last[comp] == 0)
	{
	  comps[comp] = g;
	  last[comp] = g;
	}
      else
	{
	  last[comp]->next = g;
	  last[comp] = g;
	}
    }
  vec result = 0;
  for (int i=0; i<F->rank(); i++)
    {
      if (comps[i] != 0)
	{
	  vec v = R->make_vec(i,comps[i]);
	  R->add_vec_to(result,v);
	  comps[i] = 0;
	  last[i] = 0;
	}
    }
  return result;
}

template<typename CoeffRing, typename MonInfo>
Matrix *F4toM2Interface<CoeffRing,MonInfo>::to_M2_matrix(const CoeffRing *K,
							 const MonInfo *MI,
							 gb_array &polys, 
							 const FreeModule *F)
{
  MatrixConstructor result(F,polys.size());
  for (int i=0; i<polys.size(); i++)
    result.set_column(i, to_M2_vec(K,MI,polys[i]->f, F));
  return result.to_matrix();
}

template<typename CoeffRing, typename MonInfo>
MutableMatrix * F4toM2Interface<CoeffRing,MonInfo>::to_M2_MutableMatrix(  
    const RingType *K,
    coefficient_matrix *mat)
{
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  MutableMat<CoeffRing, MATTYPE<CoeffRing> > *result = 
    MutableMat<CoeffRing, MATTYPE<CoeffRing> >::zero_matrix(K,nrows,ncols);
  MATTYPE<CoeffRing> *M = result->get_Mat();
  for (int r=0; r<nrows; r++)
    {
      row_elem &row = mat->rows[r];
      for (int i=0; i<row.len; i++)
	{
	  int c = row.comps[i];
	  ////	  c = mat->columns[c].ord;
	  M->set_entry(r,c,row.coeffs[i]);
	}
    }
  return result;
}

#include "moninfo.hpp"
template class F4toM2Interface<CoefficientRingZZp,MonomialInfo>;

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
