#include "../polyring.hpp"
#include "../freemod.hpp"
#include "Monomials.hpp"
#include "MonomialSet.hpp"
#include "SPairSet.hpp"
#include "lingb.hpp"
#include "monoms.h"

#include "../matrixcon.hpp"
#include "../matrix.hpp"
#include <vector>
#include "../sparsemat.hpp"

template<typename CoefficientRing>
void M2Interface<CoefficientRing>::from_M2_vec(CoefficientRing *K,
					       MonomialSet *H,
					       const FreeModule *F, 
					       vec v,
					       mypoly<typename CoefficientRing::elem> &result)
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  int n = 0;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	n++;
    }
  intarray vp;
  result.len = n;
  result.coeffs = newarray(COEFF_TYPE, n);
  result.monoms = newarray(monomial, n);
  n = 0;
  for (vec w = v; w != 0; w = w->next)
    {
      for (Nterm *t = w->coeff; t != 0; t = t->next)
	{
	  vp.shrink(0);
	  COEFF_TYPE a = t->coeff;
	  K->init_set(result.coeffs[n], a);
	  //result.coeffs[n] = t->coeff; // COPY THIS
	  M->to_varpower(t->monom, vp);
	  vp[0] = (vp[0]-1)/2;
	  H->find_or_insert(vp.raw(), result.monoms[n]);
	  n++;
	}

    }
}

template<typename CoefficientRing>
void M2Interface<CoefficientRing>::poly_set_degrees(CoefficientRing *K,
						    const M2_arrayint wts,
						    const poly &f,
						    int &deg, 
						    int &alpha)
{
  int leaddeg = monomial_weight(f.monoms[0], wts);
  deg = leaddeg;
  for (int i=1; i<f.len; i++)
    {
      int degi = monomial_weight(f.monoms[i],wts);
      if (degi > deg) deg = degi;
    }
  alpha = deg-leaddeg;
}

template<typename CoefficientRing>
void M2Interface<CoefficientRing>::from_M2_matrix(CoefficientRing *K,
		    const Matrix *m, 
		    MonomialSet *H,
		    M2_arrayint wts,
		    gb_array &result_polys)
{
  const FreeModule *F = m->rows();
  for (int i=0; i<m->n_cols(); i++)
    {
      gbelem *g = new gbelem;
      from_M2_vec(K,H,F,m->elem(i),g->f);
      if (wts != 0)
	poly_set_degrees(K,wts,g->f,g->deg,g->alpha);
      result_polys.push_back(g);
    }
}

template<typename CoefficientRing>
vec M2Interface<CoefficientRing>::to_M2_vec(CoefficientRing *K,
					    poly &f,
					    const FreeModule *F)
{
#warning "only handles polynomials in one component, assumes polynomials are in order"
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  
  Nterm head;
  Nterm *inresult = &head;

  intarray vp;
  int *m1 = M->make_one();

  for (int i=0; i<f.len; i++)
    {
      vp.shrink(0);
      ring_elem c = f.coeffs[i];
      monomial m = f.monoms[i];
      int *vpa = vp.alloc(2*(*m)+1);
      for (int j=0; j<2*(*m)+1; j++)
	vpa[j] = m[j];
      vpa[0] = 2*(*m)+1;
      M->from_varpower(vpa, m1);
      inresult->next = R->make_flat_term(c, m1);
      inresult = inresult->next;
    }
  inresult->next = 0;
  return R->make_vec(0, head.next);
}

template<typename CoefficientRing>
Matrix *M2Interface<CoefficientRing>::to_M2_matrix(CoefficientRing *K,
						   gb_array &polys, const FreeModule *F)
{
  MatrixConstructor result(F,polys.size());
  for (int i=0; i<polys.size(); i++)
    result.set_column(i, to_M2_vec(K,polys[i]->f, F));
  return result.to_matrix();
}

#if 0
void spair_testing(MonomialSet *H,
		   gb_array &polys)
{
  SPairSet *S = new SPairSet(H);
  gb_array gb;
  for (int i=0; i<polys.size(); i++)
    {
      // First make a gbelem, and insert it into gb
      gbelem *g;
      *g = *(polys[i]);
      g->is_minimal = 1;
      gb.push_back(g);

      // Now call spair update
      S->find_new_pairs(gb, false);
      S->display();
    }
}
#endif

template<typename CoefficientRing>
MutableMatrix * M2Interface<CoefficientRing>::to_M2_MutableMatrix(  
    const Ring *K,
    coefficient_matrix<COEFF_TYPE> *mat)
{
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  SparseMutableMatrix *result = SparseMutableMatrix::zero_matrix(K,nrows,ncols);
  for (int r=0; r<nrows; r++)
    {
      typename coefficient_matrix<COEFF_TYPE>::row_elem &row = mat->rows[r];
      for (int i=0; i<row.len; i++)
	{
	  ring_elem a = K->copy(row.coeffs[i]);
	  result->set_entry(r,row.comps[i],a);
	}
    }
  return result;
}

#include "SPairSet.cpp"
#include "linalgGB.cpp"

template class M2Interface<CoefficientRingZZp>;
template class LinAlgGB<CoefficientRingZZp>;
template int SPairSet::find_new_pairs<>(
   const LinAlgGB<CoefficientRingZZp>::gb_array &gb,
   bool remove_disjoints);

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
