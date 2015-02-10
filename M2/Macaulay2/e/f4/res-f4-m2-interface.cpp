// Copyright 2005 Michael E. Stillman

#include "../polyring.hpp"
#include "../freemod.hpp"
#include "../matrix-con.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"
#include "../newdelete.hpp"
#include "res-f4-m2-interface.hpp"
#include "../gbring.hpp"

#include <vector>

void ResF4toM2Interface::from_M2_vec(const ResPolyRing& R,
                                  const FreeModule *F,
                                  vec v,
                                  poly &result)
{
  const PolynomialRing *origR = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = origR->getMonoid();

  ring_elem denom;
  gbvector *f = origR->translate_gbvector_from_vec(F,v, denom);
  GBRing *GR = origR->get_gb_ring();
  int n = GR->gbvector_n_terms(f);

  int *exp = new int[M->n_vars()+1];
  ntuple_word *lexp = new ntuple_word[M->n_vars()+1];

  result.len = n;
  ring_elem *relem_array = new ring_elem[n]; // doesn't need to be allocated with gc, as
          // all these pointers (or values) are still in the element f.
  result.monoms = new monomial_word[n * R.monoid().max_monomial_size()];
  n = 0;
  monomial_word *nextmonom = result.monoms;
  for (gbvector *t = f; t != 0; t=t->next)
    {
      relem_array[n] = t->coeff;
      M->to_expvector(t->monom, exp);
      for (int a =0; a<M->n_vars(); a++)
        lexp[a] = exp[a];
      R.monoid().from_exponent_vector(lexp, t->comp-1, nextmonom); // gbvector components are shifted up by one
      nextmonom += R.monoid().monomial_size(nextmonom);
      n++;
    }
  result.coeffs = R.resGausser().from_ringelem_array(n, relem_array);
  delete [] relem_array;
}

void ResF4toM2Interface::poly_set_degrees(const ResPolyRing& R,
                                          const M2_arrayint wts,
                                          const poly &f,
                                          int &deg_result,
                                          int &alpha)
{
  const monomial_word *w = f.monoms;
  monomial_word leaddeg = R.monoid().monomial_weight(w, wts);
  monomial_word deg = leaddeg;

  for (int i=1; i<f.len; i++)
    {
      w = w + R.monoid().monomial_size(w);
      monomial_word degi = R.monoid().monomial_weight(w,wts);
      if (degi > deg) deg = degi;
    }
  alpha = static_cast<int>(deg-leaddeg);
  deg_result = static_cast<int>(deg);
}

#if 0
void ResF4toM2Interface::from_M2_matrix(const ResGausser *KK,
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
#endif

vec ResF4toM2Interface::to_M2_vec(const ResPolyRing& R,
                               const poly &f,
                               const FreeModule *F)
{
  const PolynomialRing *origR = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = origR->getMonoid();

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
  R.resGausser().to_ringelem_array(f.len, f.coeffs, relem_array);

  const monomial_word *w = f.monoms;
  for (int i=0; i<f.len; i++)
    {
      long comp;
      R.monoid().to_exponent_vector(w, lexp, comp);
      w = w + R.monoid().monomial_size(w);
      for (int a=0; a<M->n_vars(); a++)
        exp[a] = static_cast<int>(lexp[a]);
      M->from_expvector(exp, m1);
      Nterm * g = origR->make_flat_term(relem_array[i], m1);
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
          vec v = origR->make_vec(i,comps[i]);
          origR->add_vec_to(result,v);
          comps[i] = 0;
          last[i] = 0;
        }
    }

  deletearray(relem_array);
  return result;
}

Matrix *ResF4toM2Interface::to_M2_matrix(F4Res& C,
                                         int lev,
                                         const FreeModule *F)
{
  auto& thislevel = C.frame().level(lev);
  MatrixConstructor result(F,INTSIZE(thislevel));
  long j = 0;
  for (auto i = thislevel.cbegin(); i != thislevel.cend(); ++i, ++j)
    {
      result.set_column(j, to_M2_vec(C.ring(),i->mSyzygy, F));
    }
  return result.to_matrix();
}

MutableMatrix* ResF4toM2Interface::to_M2_MutableMatrix(F4Res& C,
                                                       int lev,
                                                       int degree)
{
  // Now we loop through the elements of degree 'degree' at level 'lev'
  auto& thislevel = C.frame().level(lev);
  int n = 0;
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree == degree) n++;
    }

  auto& prevlevel = C.frame().level(lev-1);
  int* newcomps = new int[prevlevel.size()];
  int nextcomp = 0;
  for (int i=0; i<prevlevel.size(); i++)
    if (prevlevel[i].mDegree == degree)
      newcomps[i] = nextcomp++;
    else
      newcomps[i] = -1;

  // create the mutable matrix
  MutableMatrix* result = MutableMatrix::zero_matrix(C.resGausser().get_ring(),
                                                     nextcomp,
                                                     n,
                                                     true);
  // Now loop through the elements at thislevel,
  // and for each, loop through the terms of mSyzygy.
  // if the component x satisfies newcomps[x] >= 0, then place
  // this coeff into the mutable matrix.
  int col = 0;
  
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree != degree) continue;
      auto& f = p->mSyzygy;
      auto end = poly_iter(C.ring(), f, 1);
      auto i = poly_iter(C.ring(), f);
      for ( ; i != end; ++i)
        {
          long comp = C.monoid().get_component(i.monomial());
          if (newcomps[comp] >= 0)
            {
              ring_elem a;
              a.int_val = i.coefficient(); // TODO: remove this hack!
              result->set_entry(newcomps[comp], col, a);
            }
        }
      ++col;
    }

  return result;
}

#if 0
Matrix *ResF4toM2Interface::to_M2_matrix(const ResGausser *KK,
                                      const MonomialInfo *MI,
                                      gb_array &polys,
                                      const FreeModule *F)
{
  MatrixConstructor result(F,INTSIZE(polys));
  for (int i=0; i<polys.size(); i++)
    result.set_column(i, to_M2_vec(KK,MI,polys[i]->f, F));
  return result.to_matrix();
}

MutableMatrix * ResF4toM2Interface::to_M2_MutableMatrix(const ResGausser *KK,
                                                     coefficient_matrix *mat,
                                                     gb_array &gens,
                                                     gb_array &gb)
{
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);
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
          ////    c = mat->columns[c].ord;
          M->set_entry(r,c,rowelems[i]);
        }
      deletearray(rowelems);
    }
  return M;
}
#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
