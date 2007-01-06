// Copyright 2005 Michael E. Stillman
#include <vector>
#include "../polyring.hpp"
#include "../freemod.hpp"
#include "../matrixcon.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"
#include "../newdelete.hpp"
#include "F4toM2interface.hpp"

void F4toM2Interface::from_M2_vec(const Gausser *KK,
				  const MonomialInfo *MI,
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
  int *exp = newarray_atomic(int, M->n_vars()+1);
  ntuple_word *lexp = newarray_atomic(ntuple_word, M->n_vars()+1);

  result.len = n;
  ring_elem *relem_array = newarray(ring_elem, n);
  result.monoms = newarray_atomic(monomial_word, n * MI->max_monomial_size());
  n = 0;
  monomial_word *nextmonom = result.monoms;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	{
	  relem_array[n] = t->coeff;
	  M->to_expvector(t->monom, exp);
	  for (int a =0; a<M->n_vars(); a++)
	    lexp[a] = exp[a];
	  MI->from_exponent_vector(lexp, w->comp, nextmonom);
	  nextmonom += MI->monomial_size(nextmonom);
	  n++;
	}
    }
  result.coeffs = KK->from_ringelem_array(n, relem_array);
  deletearray(relem_array);
}

void F4toM2Interface::poly_set_degrees(const Gausser *KK,
						  const MonomialInfo *MI,
						  const M2_arrayint wts,
						  const poly &f,
						  int &deg, 
						  int &alpha)
{
  const monomial_word *w = f.monoms;
  monomial_word leaddeg = MI->monomial_weight(w, wts);
  deg = leaddeg;
  
  for (int i=1; i<f.len; i++)
    {
      w = w + MI->monomial_size(w);
      monomial_word degi = MI->monomial_weight(w,wts);
      if (degi > deg) deg = degi;
    }
  alpha = deg-leaddeg;
}

void F4toM2Interface::from_M2_matrix(const Gausser *KK,
				     const MonomialInfo *MI,
				     const Matrix *m, 
				     M2_arrayint wts,
				     gb_array &result_polys)
{
  const FreeModule *F = m->rows();
  for (int i=0; i<m->n_cols(); i++)
    {
      gbelem *g = new gbelem;
      from_M2_vec(KK,MI,F,m->elem(i),g->f);
      if (wts != 0)
	poly_set_degrees(KK,MI,wts,g->f,g->deg,g->alpha);
      result_polys.push_back(g);
    }
}

vec F4toM2Interface::to_M2_vec(const Gausser *KK,
			       const MonomialInfo *MI,
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

  int *exp = newarray_atomic(int, M->n_vars()+1);
  ntuple_word *lexp = newarray_atomic(ntuple_word, M->n_vars()+1);

  ring_elem *relem_array = newarray(ring_elem, f.len);
  KK->to_ringelem_array(f.len, f.coeffs, relem_array);

  const monomial_word *w = f.monoms;
  for (int i=0; i<f.len; i++)
    {
      long comp;
      MI->to_exponent_vector(w, lexp, comp);
      w = w + MI->monomial_size(w);
      for (int a=0; a<M->n_vars(); a++)
	exp[a] = lexp[a];
      M->from_expvector(exp, m1);
      Nterm * g = R->make_flat_term(relem_array[i], m1);
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

  deletearray(relem_array);
  return result;
}

Matrix *F4toM2Interface::to_M2_matrix(const Gausser *KK,
				      const MonomialInfo *MI,
				      gb_array &polys, 
				      const FreeModule *F)
{
  MatrixConstructor result(F,polys.size());
  for (int i=0; i<polys.size(); i++)
    result.set_column(i, to_M2_vec(KK,MI,polys[i]->f, F));
  return result.to_matrix();
}

MutableMatrix * F4toM2Interface::to_M2_MutableMatrix(const Gausser *KK,
						     coefficient_matrix *mat,
						     gb_array &gens,
						     gb_array &gb)
{
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  MutableMatrix *M = IM2_MutableMatrix_make(KK->get_ring(), nrows, ncols, false);
  for (int r=0; r<nrows; r++)
    {
      row_elem &row = mat->rows[r];
      ring_elem *rowelems = newarray(ring_elem, row.len);
      if (row.coeffs == 0)
	{
	  if (row.monom == 0)
	    KK->to_ringelem_array(row.len, gens[row.elem]->f.coeffs, rowelems);
	  else
	    KK->to_ringelem_array(row.len, gb[row.elem]->f.coeffs, rowelems);
	}
      else
	{
	  KK->to_ringelem_array(row.len, row.coeffs, rowelems);
	}
      for (int i=0; i<row.len; i++)
 	{
 	  int c = row.comps[i];
 	  ////	  c = mat->columns[c].ord;
 	  M->set_entry(r,c,rowelems[i]);
	}
      deletearray(rowelems);
    }
  return M;
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
