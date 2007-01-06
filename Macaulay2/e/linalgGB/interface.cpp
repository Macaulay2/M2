// Copyright 2005 Michael E. Stillman
#include <vector>
#include "../polyring.hpp"
#include "../freemod.hpp"
#include "../matrixcon.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"

#include "interface.hpp"

#include "monoms.h"
#include "Monomials.hpp"
#include "MonomialSet.hpp"
#include "MonomialTable.hpp"

#include "SPairSet.hpp"
#include "lingb.hpp"

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
	  K->from_ring_elem(result.coeffs[n], t->coeff);
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
#ifdef DEVELOPMENT
#warning "only handles polynomials in one component, assumes polynomials are in order"
#endif
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();
  
  Nterm head;
  Nterm *inresult = &head;

  intarray vp;
  int *m1 = M->make_one();

  ring_elem c;

  for (int i=0; i<f.len; i++)
    {
      vp.shrink(0);
      K->to_ring_elem(c, f.coeffs[i]);
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

MonomialLookupTable *make_monideal(const Matrix *M, MonomialSet &H)
{
  const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  const Monoid *MF = P->getMonoid();
  queue <tagged_monomial *> new_elems;
  intarray vp;

  for (int i=0; i<M->n_cols(); i++)
    {
      ring_elem f = M->elem(0,i);
      Nterm *t = f; // numerator of f
      for ( ; t != 0; t=t->next)
	{
	  monomial m;
	  vp.shrink(0);
	  MF->to_varpower(t->monom, vp);
	  vp[0] = (vp[0]-1)/2;
	  H.find_or_insert(vp.raw(), m);
	  new_elems.insert(new tagged_monomial(m,0));
	}
    }

  MonomialLookupTable *result = new MonomialLookupTable(new_elems);
  return result;
}

#if 0
//   const PolynomialRing *R = M->get_ring()->cast_to_PolynomialRing();
//   const Monoid *MF = R->getMonoid();
//   MonomialSet H;
// 
//   gb_array pols;
// 
//   from_M2_matrix(M,&H,NULL,pols);
//   H.dump();
//   spair_testing(&H,pols);
//   return to_M2_matrix(pols,M->rows());
#endif

#if 0
//   intarray vp;
//   for (int i=0; i<M->n_cols(); i++)
//     {
//       ring_elem f = M->elem(0,i);
//       Nterm *t = f; // numerator of f
//       for ( ; t != 0; t=t->next)
// 	{
// 	  monomial m;
// 	  vp.shrink(0);
// 	  MF->to_varpower(t->monom, vp);
// 	  vp[0] = (vp[0]-1)/2;
// 	  H.find_or_insert(vp.raw(), m);
// 	}
//     }
//   H.dump();
// 
//   // Now make a MonomialTable
//   MonomialLookupTable *T = make_monideal(M,H);
// 
//   buffer o;
//   o << "Number of elements in MonomialTable = " << T->length() << newline;
//   emit(o.str());
#endif
  
#if 0
//   // Now make a MonomialHeap
//   MonomialHeap H1;
//   for (int i=0; i<M->n_cols(); i++)
//     {
//       ring_elem f = M->elem(0,i);
//       Nterm *t = f; // numerator of f
//       for ( ; t != 0; t=t->next)
// 	{
// 	  monomial m;
// 	  vp.shrink(0);
// 	  MF->to_varpower(t->monom, vp);
// 	  vp[0] = (vp[0]-1)/2;
// 	  H.find_or_insert(vp.raw(), m);
// 	  H1.insert(vp.raw(), m);
// 	}
//     }
//   H1.dump(stderr);
#endif


#if 0
// void spair_testing(MonomialSet *H,
// 		   gb_array &polys)
// {
//   SPairSet *S = new SPairSet(H);
//   gb_array gb;
//   for (int i=0; i<polys.size(); i++)
//     {
//       // First make a gbelem, and insert it into gb
//       gbelem *g;
//       *g = *(polys[i]);
//       g->is_minimal = 1;
//       gb.push_back(g);
// 
//       // Now call spair update
//       S->find_new_pairs(gb, false);
//       S->display();
//     }
// }
#endif

template<typename CoeffRing>
MutableMatrix * M2Interface<CoeffRing>::to_M2_MutableMatrix(  
    const RingType *K,
    coefficient_matrix<COEFF_TYPE> *mat)
{
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  MutableMat<CoeffRing, MATTYPE<CoeffRing> > *result = 
    MutableMat<CoeffRing, MATTYPE<CoeffRing> >::zero_matrix(K,nrows,ncols);
  MATTYPE<CoeffRing> *M = result->get_Mat();
  for (int r=0; r<nrows; r++)
    {
      typename coefficient_matrix<COEFF_TYPE>::row_elem &row = mat->rows[r];
      for (int i=0; i<row.len; i++)
	{
	  int c = row.comps[i];
	  ////	  c = mat->columns[c].ord;
	  M->set_entry(r,c,row.coeffs[i]);
	}
    }
  return result;
}

template<typename CoeffRing>
MATTYPE<CoeffRing> * M2Interface<CoeffRing>::to_M2_Mat(  
    const RingType *K,
    coefficient_matrix<COEFF_TYPE> *mat)
{
  int nrows = mat->rows.size();
  int ncols = mat->columns.size();
  MATTYPE<CoeffRing> *result = new MATTYPE<CoeffRing>(K,nrows,ncols);
  for (int r=0; r<nrows; r++)
    {
      typename coefficient_matrix<COEFF_TYPE>::row_elem &row = mat->rows[r];
      for (int i=0; i<row.len; i++)
	{
	  int c = row.comps[i];
	  ////c = mat->columns[c].ord;
	  result->set_entry(r,c,row.coeffs[i]);
	}
    }
  return result;
}

template class M2Interface<CoefficientRingZZp>;

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
