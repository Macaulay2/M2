// Copyright 2005-2021 Michael E. Stillman

#include "f4/f4-m2-interface.hpp"
#include "f4/gausser.hpp"              // for Gausser
#include "f4/moninfo.hpp"              // for monomial_word, MonomialInfo
#include "f4/ntuple-monomial.hpp"      // for ntuple_word
#include "freemod.hpp"                 // for FreeModule
#include "gbring.hpp"                  // for gbvector, GBRing
#include "interface/mutable-matrix.h"  // for IM2_MutableMatrix_make
#include "mat.hpp"                     // for MutableMatrix
#include "matrix-con.hpp"              // for MatrixConstructor
#include "matrix.hpp"                  // for Matrix
#include "monoid.hpp"                  // for Monoid
#include "newdelete.hpp"               // for newarray, newarray_atomic, del...
#include "polyring.hpp"                // for PolynomialRing
#include "ring.hpp"                    // for Ring
#include "style.hpp"                   // for INTSIZE
#include "VectorArithmetic.hpp"

void F4toM2Interface::from_M2_vec(const VectorArithmetic* VA,
                                  const MonomialInfo *MI,
                                  const FreeModule *F,
                                  vec v,
                                  GBF4Polynomial &result)
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();

  ring_elem denom;
  gbvector *f = R->translate_gbvector_from_vec(F, v, denom);
  GBRing *GR = R->get_gb_ring();
  int n = GR->gbvector_n_terms(f);

  int *exp = new int[M->n_vars() + 1]; // newarray_atomic(int, M->n_vars() + 1);
  ntuple_word *lexp = new ntuple_word[M->n_vars() + 1]; // newarray_atomic(ntuple_word, M->n_vars() + 1);

  result.len = n;
  std::vector<ring_elem> relem_array;
  result.monoms = newarray_atomic(monomial_word, n * MI->max_monomial_size());
  n = 0;
  monomial_word *nextmonom = result.monoms;
  for (gbvector *t = f; t != nullptr; t = t->next)
    {
      relem_array.push_back(t->coeff);
      M->to_expvector(t->monom, exp);
      for (int a = 0; a < M->n_vars(); a++) lexp[a] = exp[a];
      MI->from_exponent_vector(
          lexp,
          t->comp - 1,
          nextmonom);  // gbvector components are shifted up by one
      nextmonom += MI->monomial_size(nextmonom);
      n++;
    }
  result.coeffs = VA->elementArrayFromContainer(relem_array);
  delete [] exp;
  delete [] lexp;
}

void F4toM2Interface::poly_set_degrees(const VectorArithmetic* VA,
                                       const MonomialInfo *MI,
                                       const M2_arrayint wts,
                                       const GBF4Polynomial &f,
                                       int &deg_result,
                                       int &alpha)
{
  const monomial_word *w = f.monoms;
  monomial_word leaddeg = MI->monomial_weight(w, wts);
  monomial_word deg = leaddeg;

  for (int i = 1; i < f.len; i++)
    {
      w = w + MI->monomial_size(w);
      monomial_word degi = MI->monomial_weight(w, wts);
      if (degi > deg) deg = degi;
    }
  alpha = static_cast<int>(deg - leaddeg);
  deg_result = static_cast<int>(deg);
}

void F4toM2Interface::from_M2_matrix(const VectorArithmetic* VA,
                                     const MonomialInfo *MI,
                                     const Matrix *m,
                                     M2_arrayint wts,
                                     gb_array &result_polys)
{
  const FreeModule *F = m->rows();
  for (int i = 0; i < m->n_cols(); i++)
    {
      gbelem *g = new gbelem;
      from_M2_vec(VA, MI, F, m->elem(i), g->f);
      if (wts != nullptr) poly_set_degrees(VA, MI, wts, g->f, g->deg, g->alpha);
      result_polys.push_back(g);
    }
}

vec F4toM2Interface::to_M2_vec(const VectorArithmetic* VA,
                               const MonomialInfo *MI,
                               const GBF4Polynomial &f,
                               const FreeModule *F)
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = R->getMonoid();

  int *m1 = M->make_one();

  Nterm **comps = newarray(Nterm *, F->rank());
  Nterm **last = newarray(Nterm *, F->rank());
  for (int i = 0; i < F->rank(); i++)
    {
      comps[i] = 0;
      last[i] = 0;
    }

  int *exp = newarray_atomic(int, M->n_vars() + 1);
  ntuple_word *lexp = newarray_atomic(ntuple_word, M->n_vars() + 1);

  const monomial_word *w = f.monoms;
  for (int i = 0; i < f.len; i++)
    {
      long comp;
      MI->to_exponent_vector(w, lexp, comp);
      w = w + MI->monomial_size(w);
      for (int a = 0; a < M->n_vars(); a++) exp[a] = static_cast<int>(lexp[a]);
      M->from_expvector(exp, m1);
      ring_elem a = VA->ringElemFromElementArray(f.coeffs, i);
      Nterm *g = R->make_flat_term(a, m1);
      g->next = nullptr;
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
  vec result = nullptr;
  for (int i = 0; i < F->rank(); i++)
    {
      if (comps[i] != 0)
        {
          vec v = R->make_vec(i, comps[i]);
          R->add_vec_to(result, v);
          comps[i] = 0;
          last[i] = 0;
        }
    }

  return result;
}

Matrix *F4toM2Interface::to_M2_matrix(const VectorArithmetic* VA,
                                      const MonomialInfo *MI,
                                      gb_array &polys,
                                      const FreeModule *F)
{
  MatrixConstructor result(F, INTSIZE(polys));
  for (int i = 0; i < polys.size(); i++)
    result.set_column(i, to_M2_vec(VA, MI, polys[i]->f, F));
  return result.to_matrix();
}

MutableMatrix *F4toM2Interface::to_M2_MutableMatrix(const VectorArithmetic* VA,
                                                    coefficient_matrix *mat,
                                                    gb_array &gens,
                                                    gb_array &gb)
{
  int nrows = INTSIZE(mat->rows);
  int ncols = INTSIZE(mat->columns);
  MutableMatrix *M =
      IM2_MutableMatrix_make(VA->ring(), nrows, ncols, false);
  for (int r = 0; r < nrows; r++)
    {
      row_elem &row = mat->rows[r];
      const ElementArray* coeffs;
      if (row.coeffs.isNull())
        {
          if (row.monom == nullptr)
            coeffs = & gens[row.elem]->f.coeffs;
          else
            coeffs = & gb[row.elem]->f.coeffs;
        }
      else
        {
          coeffs = & row.coeffs;
        }
      for (int i = 0; i < row.len; i++)
        {
          int c = row.comps[i];
          ring_elem a = VA->ringElemFromElementArray(* coeffs, i);
          M->set_entry(r, c, a);
        }
    }
  return M;
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  indent-tabs-mode: nil
//  End:
