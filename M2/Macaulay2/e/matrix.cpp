// Copyright 1995-2004 Michael E. Stillman

#include "style.hpp"
#include "text-io.hpp"
#include "ring.hpp"
#include "matrix.hpp"
#include "comb.hpp"
#include "polyring.hpp"
#include "assprime.hpp"
#include "monideal.hpp"
#include "relem.hpp"
#include "freemod.hpp"
#include "ntuple.hpp"

#include "exptable.h"

#include <vector>

#include "matrix-con.hpp"

Matrix::Matrix(const FreeModule *rows0,
               const FreeModule *cols0,
               const int *degree_shift0,
               VECTOR(vec) & entries0)
{
  mTarget = const_cast<FreeModule *>(rows0);
  mSource = const_cast<FreeModule *>(cols0);
  mDegreeShift = const_cast<int *>(degree_shift0);
  for (int i = 0; i < cols0->rank(); i++) mEntries.push_back(entries0[i]);
}

unsigned int Matrix::computeHashValue() const
{
  unsigned int hashval = 234123 + (7 * n_rows() + 157 * n_cols());

  iterator i(this);
  for (int c = 0; c < n_cols(); c++)
    {
      int count = 0;  // only use first 2 non-zero entries in each column
      for (i.set(c); i.valid(); i.next())
        {
          hashval = 34224 * hashval + get_ring()->computeHashValue(i.entry());
          if (++count > 2) break;
        }
    }
  return hashval;
}

const Matrix /* or null */ *Matrix::make(const FreeModule *target,
                                         int ncols,
                                         const engine_RawRingElementArray M)
{
  // Checks to make:
  // each vector in V is over same ring.

  const Ring *R = target->get_ring();

  if (M != nullptr)
    for (unsigned int i = 0; i < M->len; i++)
      {
        if (R != M->array[i]->get_ring())
          {
            ERROR("expected vectors in the same ring");
            return nullptr;
          }
      }

  MatrixConstructor mat(target, ncols);
  if (M != nullptr)
    {
      unsigned int next = 0;
      for (int r = 0; r < target->rank(); r++)
        for (int c = 0; c < ncols && next < M->len; c++)
          {
            mat.set_entry(r, c, M->array[next]->get_value());
            next++;
          }
      mat.compute_column_degrees();
    }
  return mat.to_matrix();
}
const Matrix /* or null */ *Matrix::make(const FreeModule *target,
                                         const FreeModule *source,
                                         M2_arrayint deg,
                                         const engine_RawRingElementArray M)
{
  const Ring *R = target->get_ring();
  if (source->get_ring() != R)
    {
      ERROR("expected free modules over the same ring");
      return nullptr;
    }
  if (R->degree_monoid()->n_vars() != static_cast<int>(deg->len))
    {
      ERROR("expected degree of matrix to have %d entries",
            R->degree_monoid()->n_vars());
      return nullptr;
    }

  if (M != nullptr)
    {
      for (unsigned int i = 0; i < M->len; i++)
        {
          if (R != M->array[i]->get_ring())
            {
              ERROR("expected vectors in the same ring");
              return nullptr;
            }
        }
    }

  int *degshift = R->degree_monoid()->make_one();
  R->degree_monoid()->from_expvector(deg->array, degshift);
  MatrixConstructor mat(target, source, degshift);

  if (M != nullptr)
    {
      unsigned int next = 0;
      for (int r = 0; r < target->rank(); r++)
        {
          for (int c = 0; c < source->rank(); c++)
            {
              mat.set_entry(r, c, M->array[next]->get_value());
              next++;
              if (next >= M->len) break;
            }
        }
    }
  return mat.to_matrix();
}

bool Matrix::make_sparse_vecs(MatrixConstructor &mat,
                              const FreeModule *target,
                              int ncols,
                              M2_arrayint rows,
                              M2_arrayint cols,
                              const engine_RawRingElementArray entries)
// returns false if an error, true otherwise.
// Places the elements into 'mat'.
{
  const Ring *R = target->get_ring();
  for (unsigned int i = 0; i < entries->len; i++)
    {
      if (R != entries->array[i]->get_ring())
        {
          ERROR("expected vectors in the same ring");
          return false;
        }
    }
  if (rows->len != cols->len || rows->len != entries->len)
    {
      ERROR("sparse matrix creation: encountered different length arrays");
      return false;
    }
  for (int x = 0; x < entries->len; x++)
    {
      int r = rows->array[x];
      int c = cols->array[x];
      if (r < 0 || r >= target->rank())
        {
          ERROR("sparse matrix creation: row index out of range");
          return false;
        }
      if (c < 0 || c >= ncols)
        {
          ERROR("sparse matrix creation: column index out of range");
          return false;
        }
    }

  for (int x = 0; x < entries->len; x++)
    {
      int r = rows->array[x];
      int c = cols->array[x];
      mat.set_entry(r, c, entries->array[x]->get_value());
    }
  return true;
}

const Matrix /* or null */ *Matrix::make_sparse(
    const FreeModule *target,
    int ncols,
    M2_arrayint rows,
    M2_arrayint cols,
    const engine_RawRingElementArray entries)
{
  MatrixConstructor mat(target, ncols);
  if (!Matrix::make_sparse_vecs(mat, target, ncols, rows, cols, entries))
    return nullptr;  // error message has already been sent
  mat.compute_column_degrees();
  return mat.to_matrix();
}

const Matrix /* or null */ *Matrix::make_sparse(
    const FreeModule *target,
    const FreeModule *source,
    M2_arrayint deg,
    M2_arrayint rows,
    M2_arrayint cols,
    const engine_RawRingElementArray entries)
{
#ifdef DEVELOPMENT
#warning "check that all rings are correct, give error otherwise"
#endif
  const Ring *R = target->get_ring();
  int *degshift = R->degree_monoid()->make_one();
  R->degree_monoid()->from_expvector(deg->array, degshift);

  MatrixConstructor mat(target, source, degshift);
  if (!Matrix::make_sparse_vecs(
          mat, target, source->rank(), rows, cols, entries))
    return nullptr;  // error message has already been sent
  return mat.to_matrix();
}

const Matrix /* or null */ *Matrix::remake(const FreeModule *target,
                                           const FreeModule *source,
                                           M2_arrayint deg) const
{
  if (n_rows() != target->rank() || n_cols() != source->rank())
    {
      ERROR("wrong number of rows or columns");
      return nullptr;
    }
  if (deg->len != degree_monoid()->n_vars())
    {
      ERROR("degree for matrix has the wrong length");
      return nullptr;
    }
  const Ring *R = get_ring();
  const Ring *Rtarget = target->get_ring();
  const Ring *Rsource = source->get_ring();
  if (R != Rtarget || Rtarget != Rsource)
    {
      ERROR("expected same ring");
      return nullptr;
    }

  int *degshift = R->degree_monoid()->make_one();
  R->degree_monoid()->from_expvector(deg->array, degshift);
  MatrixConstructor mat(target, source, degshift);
  for (int i = 0; i < source->rank(); i++)
    mat.set_column(i, R->copy_vec(mEntries[i]));
  return mat.to_matrix();
}

const Matrix /* or null */ *Matrix::remake(const FreeModule *target) const
{
  if (n_rows() != target->rank())
    {
      ERROR("wrong number of rows");
      return nullptr;
    }
  const Ring *R = get_ring();
  if (R != target->get_ring())
    {
      ERROR("expected same ring");
      return nullptr;
    }

  MatrixConstructor mat(target, n_cols());
  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, R->copy_vec(mEntries[i]));
  mat.compute_column_degrees();
  return mat.to_matrix();
}

const Matrix /* or null */ *Matrix::make(const MonomialIdeal *mi)
{
  const PolynomialRing *P = mi->get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected a matrix over a polynomial ring");
      return nullptr;
    }
  const Monoid *M = P->getMonoid();
  int *mon = M->make_one();

  MatrixConstructor mat(P->make_FreeModule(1), mi->length());
  int next = 0;
  for (Index<MonomialIdeal> i = mi->last(); i.valid(); i--)
    {
      M->from_varpower((*mi)[i]->monom().raw(), mon);
      ring_elem f =
          P->make_flat_term(P->getCoefficientRing()->from_long(1), mon);
      mat.set_entry(0, next++, f);
    }
  M->remove(mon);

  mat.compute_column_degrees();
  return mat.to_matrix();
}

ring_elem Matrix::elem(int i, int j) const
{
  return get_ring()->get_entry(elem(j), i);
}

bool Matrix::is_equal(const Matrix &m) const
{
  if (this == &m) return true;
  if (hash() != m.hash()) return false;
  if (get_ring() != m.get_ring()) return false;
  if (n_rows() != m.n_rows()) return false;
  if (n_cols() != m.n_cols()) return false;
  for (int i = 0; i < n_cols(); i++)
    if (!get_ring()->is_equal(elem(i), m.elem(i))) return false;
  return true;
}

bool Matrix::is_zero() const
{
  for (int i = 0; i < n_cols(); i++)
    if (elem(i) != nullptr) return false;
  return true;
}

bool Matrix::is_homogeneous() const
{
  if (!get_ring()->is_graded()) return 0;
  int *d = degree_monoid()->make_one();
  for (int i = 0; i < n_cols(); i++)
    {
      if (elem(i) == nullptr) continue;
      if (!get_ring()->vec_is_homogeneous(rows(), elem(i)))
        {
          degree_monoid()->remove(d);
          return 0;
        }

      get_ring()->vec_degree(rows(), elem(i), d);
      degree_monoid()->divide(d, degree_shift(), d);
      if (0 != degree_monoid()->compare(d, cols()->degree(i)))
        {
          degree_monoid()->remove(d);
          return 0;
        }
    }
  degree_monoid()->remove(d);
  return 1;
}

Matrix *Matrix::homogenize(int v, M2_arrayint wts) const
{
  MatrixConstructor mat(rows(), n_cols());
  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, get_ring()->vec_homogenize(rows(), elem(i), v, wts));
  mat.compute_column_degrees();
  return mat.to_matrix();
}

Matrix *Matrix::zero(const FreeModule *F, const FreeModule *G)
{
  if (F->get_ring() != G->get_ring())
    {
      ERROR("free modules have different base rings");
      return nullptr;
    }
  MatrixConstructor mat(F, G);
  return mat.to_matrix();
}

Matrix *Matrix::identity(const FreeModule *F)
{
  const Ring *R = F->get_ring();
  const ring_elem one = R->from_long(1);
  MatrixConstructor mat(F, F, nullptr);
  for (int i = 0; i < F->rank(); i++) mat.set_entry(i, i, R->copy(one));
  return mat.to_matrix();
}

Matrix *Matrix::operator+(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrices have different base rings");
      return nullptr;
    }
  if (rows()->rank() != m.rows()->rank() || cols()->rank() != m.cols()->rank())
    {
      ERROR("matrices have different shapes");
      return nullptr;
    }

  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  const int *deg;

  if (!rows()->is_equal(m.rows())) F = R->make_FreeModule(n_rows());

  if (!cols()->is_equal(m.cols())) G = R->make_FreeModule(n_cols());

  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    deg = degree_shift();
  else
    deg = degree_monoid()->make_one();

  MatrixConstructor mat(F, G, deg);
  for (int i = 0; i < n_cols(); i++)
    {
      vec v = R->copy_vec(elem(i));
      vec w = R->copy_vec(m[i]);
      R->add_vec_to(v, w);
      mat.set_column(i, v);
    }
  return mat.to_matrix();
}

Matrix *Matrix::operator-(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrices have different base rings");
      return nullptr;
    }
  if (rows()->rank() != m.rows()->rank() || cols()->rank() != m.cols()->rank())
    {
      ERROR("matrices have different shapes");
      return nullptr;
    }

  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  const int *deg;

  if (!rows()->is_equal(m.rows())) F = R->make_FreeModule(n_rows());

  if (!cols()->is_equal(m.cols())) G = R->make_FreeModule(n_cols());

  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    deg = degree_shift();
  else
    deg = degree_monoid()->make_one();

  MatrixConstructor mat(F, G, deg);
  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, R->subtract_vec(elem(i), m[i]));
  return mat.to_matrix();
}

Matrix *Matrix::operator-() const
{
  MatrixConstructor mat(rows(), cols(), degree_shift());
  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, get_ring()->negate_vec(elem(i)));
  return mat.to_matrix();
}

Matrix /* or null */ *Matrix::sub_matrix(M2_arrayint r, M2_arrayint c) const
{
  const FreeModule *F = rows()->sub_space(r);
  const FreeModule *G = cols()->sub_space(c);
  if (F == NULL || G == NULL) return nullptr;

  int *trans = newarray_atomic(int, n_rows());
  for (int i = 0; i < n_rows(); i++) trans[i] = -1;

  for (unsigned j = 0; j < r->len; j++)
    if (r->array[j] >= 0 && r->array[j] < n_rows()) trans[r->array[j]] = j;

  MatrixConstructor mat(F, G, degree_shift());
  for (unsigned int i = 0; i < c->len; i++)
    {
      vec v = elem(c->array[i]);
      for (; v != NULL; v = v->next)
        if (trans[v->comp] != -1) mat.set_entry(trans[v->comp], i, v->coeff);
    }
  deletearray(trans);
  return mat.to_matrix();
}

Matrix /* or null */ *Matrix::sub_matrix(M2_arrayint c) const
{
  const FreeModule *G = cols()->sub_space(c);
  if (G == NULL) return nullptr;

  MatrixConstructor mat(rows(), G, degree_shift());
  for (unsigned int i = 0; i < c->len; i++)
    mat.set_column(i, get_ring()->copy_vec(elem(c->array[i])));
  return mat.to_matrix();
}

Matrix /* or null */ *Matrix::reshape(const FreeModule *F,
                                      const FreeModule *G) const
// Reshape 'this' : F <--- G, where
// (rank F)(rank G) = (nrows this)(ncols this)
{
  if (F->get_ring() != get_ring() || G->get_ring() != get_ring())
    {
      ERROR("reshape: expected same ring");
      return nullptr;
    }
  if (n_rows() * n_cols() != F->rank() * G->rank())
    {
      ERROR("reshape: ranks of freemodules incorrect");
      return nullptr;
    }

  // EFFICIENCY: might be better to sort columns at end?
  MatrixConstructor mat(F, G, degree_shift());
  for (int c = 0; c < n_cols(); c++)
    for (vecterm *p = elem(c); p != NULL; p = p->next)
      {
        // Determine new component
        int loc = c * n_rows() + p->comp;
        int result_col = loc / F->rank();
        int result_row = loc % F->rank();

        mat.set_entry(result_row, result_col, p->coeff);
      }
  return mat.to_matrix();
}

Matrix /* or null */ *Matrix::flip(const FreeModule *F, const FreeModule *G)
{
  const Ring *R = F->get_ring();
  if (R != G->get_ring())
    {
      ERROR("flip: expected same ring");
      return nullptr;
    }
  const FreeModule *H = F->tensor(G);
  const FreeModule *K = G->tensor(F);

  MatrixConstructor mat(K, H, nullptr);
  int next = 0;
  for (int f = 0; f < F->rank(); f++)
    for (int g = 0; g < G->rank(); g++)
      mat.set_column(next++, R->e_sub_i(f + g * F->rank()));
  return mat.to_matrix();
}

Matrix *Matrix::transpose() const
{
  const FreeModule *F = cols()->transpose();
  const FreeModule *G = rows()->transpose();
  const Ring *R = F->get_ring();

  MatrixConstructor mat(F, G, degree_shift());

  // The efficiency of this code relies on the way of ordering
  // the sparse vectors (lead term has largest component)
  iterator i(this);
  for (int c = 0; c < n_cols(); c++)
    {
      for (i.set(c); i.valid(); i.next())
        {
          ring_elem f = i.entry();
          mat.set_entry(c, i.row(), R->antipode(f));
        }
    }
  return mat.to_matrix();
}

Matrix *Matrix::scalar_mult(const ring_elem r, bool opposite_mult) const
{
  const Ring *R = get_ring();
  int *deg = degree_monoid()->make_one();
  if (!R->is_zero(r)) R->degree(r, deg);
  degree_monoid()->mult(deg, degree_shift(), deg);
  MatrixConstructor mat(rows(), cols(), deg);
  for (int i = 0; i < n_cols(); i++)
    {
      vec w = R->copy_vec(elem(i));
      R->mult_vec_to(w, r, opposite_mult);
      mat.set_column(i, w);
    }
  return mat.to_matrix();
}

Matrix *Matrix::concat(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("concat: different base rings");
      return nullptr;
    }
  if (n_rows() != m.n_rows())
    {
      ERROR("concat: matrices have different numbers of rows");
      return nullptr;
    }

  const FreeModule *G = cols()->direct_sum(m.cols());
  MatrixConstructor mat(rows(), G, nullptr);
  int i;
  int nc = n_cols();
  for (i = 0; i < nc; i++) mat.set_column(i, get_ring()->copy_vec(elem(i)));
  for (i = 0; i < m.n_cols(); i++)
    mat.set_column(nc + i, get_ring()->copy_vec(m.elem(i)));
  return mat.to_matrix();
}

Matrix *Matrix::direct_sum(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("concat: different base rings");
      return nullptr;
    }

  // direct_sum ignores the degree shift of each summand.
  /// const int *deg;
  ///  if (EQ == degree_monoid()->compare(degree_shift(), m->degree_shift()))
  ///    deg = degree_shift();
  ///  else
  ///    deg = nullptr;

  const FreeModule *F = rows()->direct_sum(m->rows());
  const FreeModule *G = cols()->direct_sum(m->cols());

  MatrixConstructor mat(F, G, nullptr);

  int i;
  int nr = n_rows();
  int nc = n_cols();
  for (i = 0; i < nc; i++) mat.set_column(i, get_ring()->copy_vec(elem(i)));
  for (i = 0; i < m->n_cols(); i++)
    mat.set_column(nc + i, get_ring()->component_shift(nr, m->elem(i)));
  return mat.to_matrix();
}

Matrix *Matrix::mult(const Matrix *m, bool opposite_mult) const
{
  const Ring *R = get_ring();
  if (R != m->get_ring())
    {
      ERROR("matrix mult: different base rings");
      return nullptr;
    }
  if (n_cols() != m->n_rows())
    {
      ERROR("matrix mult: matrix sizes don't match");
      return nullptr;
    }

  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m->degree_shift(), deg);

  MatrixConstructor mat(rows(), m->cols(), deg);

  degree_monoid()->remove(deg);

  for (int i = 0; i < m->n_cols(); i++)
    mat.set_column(i, R->mult_vec_matrix(this, m->elem(i), opposite_mult));
  return mat.to_matrix();
}

Matrix *Matrix::module_tensor(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("module tensor: different base rings");
      return nullptr;
    }
  FreeModule *F = rows()->tensor(m->rows());
  FreeModule *G = rows()->tensor(m->cols());
  FreeModule *G1 = m->rows()->tensor(cols());
  G->direct_sum_to(G1);
  deleteitem(G1);

  MatrixConstructor mat(F, G, nullptr);

  int i, j, next = 0;

  for (i = 0; i < n_rows(); i++)
    for (j = 0; j < m->n_cols(); j++)
      mat.set_column(next++,
                     get_ring()->component_shift(i * m->n_rows(), m->elem(j)));

  for (i = 0; i < m->n_rows(); i++)
    for (j = 0; j < n_cols(); j++)
      mat.set_column(next++, get_ring()->tensor_shift(m->n_rows(), i, elem(j)));
  return mat.to_matrix();
}

Matrix *Matrix::random(
    const Ring *R,
    int r,
    int c,
    double density,
    int special_type)  // 0: general, 1:upper triangular, others?
{
  bool doing_fraction = false;
  int threshold = 0;

  FreeModule *F = R->make_FreeModule(r);
  FreeModule *G = R->make_FreeModule(c);
  MatrixConstructor mat(F, G, nullptr);

  // Loop through all selected elements, flip a 'fraction_non_zero' coin, and if
  // non-zero
  // set that element.

  if (density != 1.0)
    {
      doing_fraction = true;
      threshold = static_cast<int>(density * 10000);
    }

  if (special_type == 0)
    {
      for (int i = 0; i < c; i++)
        for (int j = 0; j < r; j++)
          {
            if (doing_fraction)
              {
                int32_t u = rawRandomInt((int32_t)10000);
                if (u > threshold) continue;
              }
            mat.set_entry(j, i, R->random());
          }
    }
  else if (special_type == 1)
    {
      for (int i = 0; i < c; i++)
        {
          int top = (i >= r ? r : i);
          for (int j = 0; j < top; j++)
            {
              if (doing_fraction)
                {
                  int32_t u = rawRandomInt((int32_t)10000);
                  if (u > threshold) continue;
                }
              mat.set_entry(j, i, R->random());
            }
        }
    }
  return mat.to_matrix();
}

Matrix *Matrix::tensor(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("matrix tensor: different base rings");
      return 0;
    }

  const FreeModule *F = rows()->tensor(m->rows());
  const FreeModule *G = cols()->tensor(m->cols());
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m->degree_shift(), deg);

  MatrixConstructor mat(F, G, deg);
  degree_monoid()->remove(deg);
  int i, j, next = 0;
  for (i = 0; i < n_cols(); i++)
    for (j = 0; j < m->n_cols(); j++)
      mat.set_column(next++,
                     get_ring()->tensor(rows(), elem(i), m->rows(), (*m)[j]));
  return mat.to_matrix();
}

Matrix *Matrix::diff(const Matrix *m, int use_coef) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected a polynomial ring");
      return nullptr;
    }
  if (P != m->get_ring())
    {
      ERROR("matrix diff: different base rings");
      return nullptr;
    }
  FreeModule *F1 = rows()->transpose();
  const FreeModule *F = F1->tensor(m->rows());
  FreeModule *G1 = cols()->transpose();
  const FreeModule *G = G1->tensor(m->cols());
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(m->degree_shift(), degree_shift(), deg);
  deleteitem(F1);
  deleteitem(G1);

  MatrixConstructor mat(F, G, deg);
  degree_monoid()->remove(deg);
  int i, j, next = 0;
  for (i = 0; i < n_cols(); i++)
    for (j = 0; j < m->n_cols(); j++)
      mat.set_column(
          next++,
          P->vec_diff(elem(i), m->rows()->rank(), m->elem(j), use_coef));
  return mat.to_matrix();
}

Matrix *Matrix::lead_term(int nparts) const
// Select those monomials in each column
// which are maximal in the order under
// the first n parts of the monomial order.
{
  MatrixConstructor mat(rows(), cols(), degree_shift());

  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, get_ring()->vec_lead_term(nparts, rows(), elem(i)));
  return mat.to_matrix();
}

#if 0
// void Matrix::minimal_lead_terms_ZZ(intarray &result) const
// {
//   int x;
//   M2_arrayint indices;
//   array<TermIdeal *> mis;
//   const array<vec> vecs = _entries;
//   indices = rows()->sort(vecs, NULL, 0, 1);
//   const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
//   const FreeModule *Rsyz = P->get_Rsyz(); // NULL if not a quotient ring.
//   FreeModule *Gsyz = P->make_FreeModule(vecs.length());
//   for (x=0; x<n_cols(); x++)
//     mis.append(new TermIdeal(P,Gsyz));
//   for (int i=0; i<vecs.length(); i++)
//     {
//       vec v = vecs[indices->array[i]];
//       vec gsyz, rsyz;
//       if (v == NULL) continue;
//       if (TI_TERM != mis[v->comp]->search(v->coeff, v->monom, gsyz, rsyz))
//      {
//        mis[v->comp]->insert_minimal(
//                                     new tagged_term(P->getCoefficientRing()->copy(v->coeff),
//                                                     P->getMonoid()->make_new(v->monom),
//                                                     NULL,
//                                                     NULL));
//        result.append(indices->array[i]);
//      }
//       Gsyz->remove(gsyz);
//       if (rsyz != NULL) Rsyz->remove(rsyz);
//     }
//   for (x=0; x<n_cols(); x++)
//     deleteitem(mis[x]);
// }
#endif

#if 0
// void Matrix::minimal_lead_terms(intarray &result) const
// {
//   if (get_ring()->getCoefficientRing()->is_ZZ())
//     {
//       minimal_lead_terms_ZZ(result);
//       return;
//     }
//   M2_arrayint indices;
//   intarray vp;
//   array<MonomialIdeal *> mis;
//   const array<vec> vecs = _entries;
//   indices = rows()->sort(vecs, NULL, 0, 1);
//   for (int x=0; x<n_rows(); x++)
//     mis.append(new MonomialIdeal(get_ring()));
//   for (int i=0; i<vecs.length(); i++)
//     {
//       vec v = vecs[indices->array[i]];
//       if (v == NULL) continue;
//       // Reduce each one in turn, and replace.
//       Bag *junk_bag;
//       vp.shrink(0);
//       rows()->lead_varpower(v, vp);
//       if (!mis[v->comp]->search(vp.raw(),junk_bag))
//      {
//        Bag *b = new Bag(indices->array[i], vp);
//        mis[v->comp]->insert(b);
//        result.append(indices->array[i]);
//      }
//     }
// }
//
#endif

M2_arrayintOrNull Matrix::support() const
{
  const PolynomialRing *R = get_ring()->cast_to_PolynomialRing();
  if (R == nullptr)
    {
      ERROR("expected a polynomial ring");
      return nullptr;
    }
  int n = R->n_vars();
  int nsupp = 0;
  int *exp = newarray_atomic(int, R->n_vars());
  int *exp2 = newarray_atomic(int, R->n_vars());
  for (int i = 0; i < R->n_vars(); i++) exp[i] = exp2[i] = 0;
  for (int j = 0; j < n_cols(); j++)
    for (vec v = elem(j); v != nullptr; v = v->next)
      {
        for (const Nterm *f = v->coeff; f != nullptr; f = f->next)
          {
            R->getMonoid()->to_expvector(f->monom, exp2);
            for (int k = 0; k < n; k++)
              if (exp2[k] != 0 && exp[k] == 0)
                {
                  exp[k] = 1;
                  if (++nsupp == n) goto out;
                }
          }
      }
out:
  M2_arrayint result = M2_makearrayint(nsupp);
  int next = 0;
  for (int i = 0; i < n; i++)
    if (exp[i] > 0) result->array[next++] = i;
  deletearray(exp);
  deletearray(exp2);
  return result;
}

Matrix *Matrix::top_coefficients(Matrix *&monoms) const
{
  const PolynomialRing *R = get_ring()->cast_to_PolynomialRing();
  if (R == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  MatrixConstructor result(rows(), 0);
  MatrixConstructor cons_monoms(R->make_FreeModule(1), 0);
  for (int i = 0; i < n_cols(); i++)
    {
      int var, exp;
      vec u = elem(i);
      vec v = R->vec_top_coefficient(u, var, exp);
      result.append(v);
      if (v == nullptr)
        cons_monoms.append(v);
      else
        {
          ring_elem b;
          if (var < R->n_vars())
            {
              ring_elem a = R->var(var);
              b = R->power(a, exp);
            }
          else
            {
              b = R->from_long(1);
            }
          vec w = R->make_vec(0, b);
          cons_monoms.append(w);
        }
    }
  monoms = cons_monoms.to_matrix();
  return result.to_matrix();
}

M2_arrayintOrNull Matrix::elim_vars(int nparts) const
{
  intarray keep;
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  int nslots = P->getMonoid()->n_slots(nparts);
  for (int i = 0; i < n_cols(); i++)
    if (P->vec_in_subring(nslots, elem(i))) keep.append(i);
  M2_arrayint result = M2_makearrayint(keep.length());
  for (unsigned int i = 0; i < result->len; i++) result->array[i] = keep[i];
  return result;
}

M2_arrayintOrNull Matrix::elim_keep(int nparts) const
{
  intarray keep;
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  int nslots = P->getMonoid()->n_slots(nparts);
  for (int i = 0; i < n_cols(); i++)
    if (!P->vec_in_subring(nslots, elem(i))) keep.append(i);
  M2_arrayint result = M2_makearrayint(keep.length());
  for (unsigned int i = 0; i < result->len; i++) result->array[i] = keep[i];
  return result;
}

Matrix *Matrix::divide_by_var(int n, int maxd, int &maxdivided) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  MatrixConstructor mat(rows(), 0);
  maxdivided = 0;
  for (int i = 0; i < n_cols(); i++)
    {
      if (elem(i) != NULL)
        {
          int lo, hi;
          P->vec_degree_of_var(n, elem(i), lo, hi);
          if (maxd >= 0 && lo > maxd) lo = maxd;
          if (lo > maxdivided) maxdivided = lo;
          mat.append(P->vec_divide_by_var(n, lo, elem(i)));
        }
    }
  return mat.to_matrix();
}

// ideal operations
Matrix /* or null */ *Matrix::koszul(int p) const
{
  if (n_rows() != 1)
    {
      ERROR("expected a matrix with one row");
      return nullptr;
    }

  FreeModule *F = cols()->exterior(p - 1);
  FreeModule *G = cols()->exterior(p);
  const Ring *R = get_ring();
  MatrixConstructor mat(F, G, degree_shift());
  if (p <= 0 || p > n_cols()) return mat.to_matrix();

  Subsets C(n_cols(), p);
  Subset a(p, 0);
  for (int c = 0; c < G->rank(); c++)
    {
      C.decode(c, a);
      int negate = ((p % 2) != 0);
      for (int r = p - 1; r >= 0; r--)
        {
          negate = !negate;
          size_t x = C.encodeBoundary(r, a);
          ring_elem f = elem(0, static_cast<int>(a[r]));
          if (negate) R->negate_to(f);

          mat.set_entry(static_cast<int>(x), c, f);
        }
    }
  return mat.to_matrix();
}

//////////////////////////////////////////
// koszul monomials //////////////////////
//////////////////////////////////////////
static MonomialIdeal *makemonideal(const Matrix *A)
{
  const PolynomialRing *P = A->get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  const Monoid *M = P->getMonoid();
  queue<Bag *> new_elems;

  for (int i = 0; i < A->n_cols(); i++)
    {
      vec v = A->elem(i);
      if (v == nullptr) continue;
      Bag *b = new Bag(i);
      M->to_varpower(P->lead_flat_monomial(v->coeff), b->monom());
      new_elems.insert(b);
    }

  MonomialIdeal *result = new MonomialIdeal(P, new_elems);
  return result;
}
static int signdivide(int n, const int *a, const int *b, int *exp)
{
  int sign = 0;
  int sum = 0;
  for (int i = 0; i < n; i++)
    {
      int e = a[i] - b[i];
      if (e < 0) return 0;
      exp[i] = e;
      sign += sum * e;
      sum += b[i];
    }
  sign %= 2;
  if (sign == 0) return 1;
  return -1;
}
Matrix /* or null */ *Matrix::koszul_monomials(int nskew,
                                               const Matrix *r,
                                               const Matrix *c)
// The first nskew variables are considered skew commuting for the purpose of
// computing signs.
{
  // First check rings: r,c,'this' should be row vectors.
  // and the ring should be a polynomial ring
  const FreeModule *F = r->cols();

  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  const MonomialIdeal *A = makemonideal(r);

  const Ring *K = P->getCoefficients();
  const Monoid *M = P->getMonoid();

  MatrixConstructor mat(F, c->cols(), nullptr);

  int nvars = M->n_vars();
  int *skew_list = newarray_atomic(int, nskew);
  for (int j = 0; j < nskew; j++) skew_list[j] = j;
  SkewMultiplication skew(nvars, nskew, skew_list);
  int ncols = c->n_cols();
  const int *a;  // a monomial

  int *aexp = newarray_atomic(int, nvars);
  int *bexp = newarray_atomic(int, nvars);
  int *result_exp = newarray_atomic(int, nvars);
  int *m = M->make_one();
  VECTOR(Bag *) divisors;
  for (int i = 0; i < ncols; i++)
    {
      if (c->elem(i) == nullptr) continue;
      a = P->lead_flat_monomial(c->elem(i)->coeff);
      M->to_expvector(a, aexp);
      divisors.clear();
      A->find_all_divisors(aexp, divisors);
      for (int j = 0; j < divisors.size(); j++)
        {
          int rownum = divisors[j]->basis_elem();
          const int *b = P->lead_flat_monomial(r->elem(rownum)->coeff);
          M->to_expvector(b, bexp);
          ntuple::divide(nvars, aexp, bexp, result_exp);
          int sign = skew.mult_sign(result_exp, bexp);
          if (sign != 0)
            {
              M->from_expvector(result_exp, m);
              ring_elem s = (sign > 0 ? K->one() : K->minus_one());
              ring_elem f = P->make_flat_term(s, m);
              mat.set_entry(rownum, i, f);
            }
        }
    }
  deletearray(aexp);
  deletearray(bexp);
  deletearray(result_exp);
  return mat.to_matrix();
}

Matrix /* or null */ *Matrix::koszul(const Matrix *r, const Matrix *c)
{
  // First check rings: r,c,'this' should be row vectors.
  // and the ring should be a polynomial ring
  const FreeModule *F = r->cols();

  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == NULL) return nullptr;
  const Ring *K = P->getCoefficients();
  const Monoid *M = P->getMonoid();

  MatrixConstructor mat(F, c->cols(), nullptr);

  int nvars = M->n_vars();
  int nrows = r->n_cols();
  int ncols = c->n_cols();
  const int *a, *b;  // monomials
  int *aexp = newarray_atomic(int, nvars);
  int *bexp = newarray_atomic(int, nvars);
  int *result_exp = newarray_atomic(int, nvars);
  for (int i = 0; i < ncols; i++)
    {
      if (c->elem(i) == nullptr) continue;
      a = P->lead_flat_monomial(c->elem(i)->coeff);
      M->to_expvector(a, aexp);
      for (int j = 0; j < nrows; j++)
        {
          if (r->elem(j) == nullptr) continue;
          b = P->lead_flat_monomial(r->elem(j)->coeff);
          M->to_expvector(b, bexp);
          int sign = signdivide(nvars, aexp, bexp, result_exp);
          if (sign != 0)
            {
              int *m = M->make_one();
              M->from_expvector(result_exp, m);
              ring_elem s = (sign > 0 ? K->one() : K->minus_one());
              ring_elem f = P->make_flat_term(s, m);
              mat.set_entry(j, i, f);
            }
        }
    }
  deletearray(aexp);
  deletearray(bexp);
  deletearray(result_exp);
  return mat.to_matrix();
}

Matrix *Matrix::wedge_product(int p, int q, const FreeModule *F)
{
  const FreeModule *Fp = F->exterior(p);
  const FreeModule *Fq = F->exterior(q);
  const FreeModule *Fn = F->exterior(p + q);
  const FreeModule *G = Fp->tensor(Fq);
  const Ring *R = F->get_ring();

  MatrixConstructor mat(Fn, G, nullptr);

  if (p < 0 || q < 0 || p + q > F->rank()) return mat.to_matrix();

  if (p == 0 || q == 0)
    {
      for (int i = 0; i < G->rank(); i++) mat.set_entry(i, i, R->one());
      return mat.to_matrix();
    }

  Subsets C(F->rank(), p + q);
  Subset a(p, 0);
  Subset b(q, 0);
  Subset c(p + q, 0);
  int col = 0;

  for (int i = 0; i < Fp->rank(); i++)
    {
      C.decode(i, a);
      for (int j = 0; j < Fq->rank(); j++)
        {
          C.decode(j, b);
          int sgn = Subsets::concatenateSubsets(a, b, c);
          if (sgn == 0)
            {
              col++;
              continue;
            }
          ring_elem r = F->get_ring()->from_long(sgn);
          int row = static_cast<int>(C.encode(c));
          mat.set_entry(row, col++, r);
        }
    }
  return mat.to_matrix();
}

void Matrix::text_out(buffer &o) const
{
  int nrows = n_rows();
  int ncols = n_cols();

  buffer *p = new buffer[nrows];
  //  buffer *p = new buffer[nrows];
  int r;
  for (int c = 0; c < ncols; c++)
    {
      int maxcount = 0;
      for (r = 0; r < nrows; r++)
        {
          ring_elem f = elem(r, c);
          get_ring()->elem_text_out(p[r], f);
          get_ring()->remove(f);
          if (p[r].size() > maxcount) maxcount = p[r].size();
        }
      for (r = 0; r < nrows; r++)
        for (int k = maxcount + 1 - p[r].size(); k > 0; k--) p[r] << ' ';
    }
  for (r = 0; r < nrows; r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  delete[] p;
}

Matrix *Matrix::compress() const
{
  MatrixConstructor result(rows(), 0);
  for (int i = 0; i < n_cols(); i++)
    if (elem(i) != nullptr) result.append(elem(i), cols()->degree(i));
  return result.to_matrix();
}

#if 0
// int Matrix::moneq(const int *exp, int *m, const int *vars, int *exp2) const
//     // Internal private routine for 'coeffs'.
//     // exp2 is a scratch value.  It is a paramter so we only have to allocate
//     // it once...
// {
//   get_ring()->getMonoid()->to_expvector(m, exp2);
//   int nvars = get_ring()->n_vars();
//   for (int i=0; i<nvars; i++)
//     {
//       if (vars[i] == 0) continue;
//       if (exp[i] != exp2[i])
//      return 0;
//       else
//      exp2[i] = 0;
//     }
//   get_ring()->getMonoid()->from_expvector(exp2, m);
//   return 1;
// }
// vec Matrix::strip_vector(vec &f, const int *vars,
//                            const FreeModule *F, vec &vmonom) const
//     // private routine for 'coeffs'
// {
//   if (f == NULL)
//     {
//       vmonom = NULL;
//       return NULL;
//     }
//   if (get_ring()->getMonoid() == NULL)
//     {
//       vmonom = F->e_sub_i(0);
//       vec result = f;
//       f = NULL;
//       return result;
//     }
//   // At this point, we know that we have a polynomial ring
//   int nvars = get_ring()->n_vars();
//   int *exp = newarray_atomic(int,nvars);
//   int *scratch_exp = newarray_atomic(int,nvars);
//   const Monoid *M = get_ring()->getMonoid();
//
//   M->to_expvector(f->monom, exp);
//   for (int i=0; i<nvars; i++)
//     if (vars[i] == 0) exp[i] = 0;
//
//   // the following two lines do NOT work if 'F' is a Schreyer free module,
//   // but this routine is private to 'coeffs', where this is not the case.
//   vmonom = F->e_sub_i(0);
//   M->from_expvector(exp, vmonom->monom);
//
//   vecterm head;
//   vecterm *newf = &head;
//   vec result = NULL;
//
//   // Loop through f: if monomial matches 'exp', strip and add to result,
//   // otherwise leave alone, and place on head list.
//   while (f != NULL)
//     {
//       if (moneq(exp, f->monom, vars, scratch_exp))
//      {
//        vec temp = f;
//        f = f->next;
//        temp->next = NULL;
//        rows()->add_to(result, temp);
//      }
//       else
//      {
//        newf->next = f;
//        f = f->next;
//        newf = newf->next;
//        newf->next = NULL;
//      }
//     }
//   newf->next = NULL;
//   f = head.next;
//
//   deletearray(exp);
//   deletearray(scratch_exp);
//   return result;
// }
#endif

Matrix *Matrix::remove_scalar_multiples() const
{
  bool keep;
  MatrixConstructor result(rows(), 0);
  for (int i = 0; i < n_cols(); i++)
    {
      vec f = elem(i);
      if (f == NULL) continue;
      keep = true;
      for (int j = i + 1; j < n_cols(); j++)
        {
          vec g = elem(j);
          if (g == NULL) continue;
          if (get_ring()->vec_is_scalar_multiple(f, g))
            {
              keep = false;
              break;
            }
        }
      if (keep) result.append(get_ring()->copy_vec(f));
    }
  return result.to_matrix();
}

Matrix *Matrix::remove_monomial_factors(bool make_squarefree_only) const
// Divide each column v by the maximal monomial 'm' which divides v.
// If keep_one is true, divide by somewhat less, making the resulting monomial
// factor squarefree.
{
  MatrixConstructor result(rows(), 0);
  for (int i = 0; i < n_cols(); i++)
    {
      vec f = get_ring()->vec_remove_monomial_factors(elem(i),
                                                      make_squarefree_only);
      result.append(f);
    }
  return result.to_matrix();
}

#if 0
// // MES Aug 2002
// Matrix *Matrix::simplify(int n) const
// {
//   int i,j, keep;
//   Matrix *result = new Matrix(rows());
//
//   switch (n) {
//   case 1:
//     for (i=0; i<n_cols(); i++)
//       {
//      vec f = elem(i);
//      if (f == NULL) continue;
//      result->append(rows()->copy(f));
//       }
//     break;
//     //  case SIMP_SCALAR_MULTIPLES:
//   case 2:
//     for (i=0; i<n_cols(); i++)
//       {
//      vec f = elem(i);
//      if (f == NULL) continue;
//      keep = 1;
//      for (j=i+1; j<n_cols(); j++)
//        {
//          vec g = elem(j);
//          if (g == NULL) continue;
//          if (rows()->is_scalar_multiple(f, g))
//            {
//              keep = 0;
//              break;
//            }
//        }
//      if (keep) result->append(rows()->copy(f));
//       }
//     break;
//   case 3:
//     // Remove multiple monomial divisors (i.e. x^2*f --> x*f)
//     for (i=0; i<n_cols(); i++)
//       {
//      vec f = elem(i);
//      if (f == NULL) continue;
//      result->append(rows()->monomial_squarefree(f));
//       }
//     break;
//   case 4:
//     // Remove monomial divisors (i.e. x*f --> f)
//     for (i=0; i<n_cols(); i++)
//       {
//      vec f = elem(i);
//      if (f == NULL) continue;
//      result->append(rows()->remove_monomial_divisors(f));
//       }
//     break;
// #if 0
// //   case SIMP_ZEROS:
// //     break;
// //   case SIMP_MULTIPLES:
// //     break;
// //   case SIMP_AUTO_REDUCE:
// //     break;
// //   case SIMP_SQUAREFREE:
// //     break;
// //   case SIMP_MONOMIAL_DIVISORS:
// //     break;
// #endif
//   default:
//     ERROR("bad simplification type");
//     return 0;
//   }
//
//   return result;
// }
#endif
#if 0
// // MES Aug 2002
// Matrix *Matrix::auto_reduce() const
// {
//   array<vec> vecs;
//   int i;
//   for (i=0; i<n_cols(); i++)
//     vecs.append(rows()->copy(elem(i)));
//   rows()->auto_reduce(vecs);
//   Matrix *result = new Matrix(rows());
//   for (i=0; i<vecs.length(); i++)
//     result->append(vecs[i]);
//   return result;
// }
#endif

#if 0
// Matrix *Matrix::coeffs(const int *vars, Matrix * &result_monoms) const
// {
//   Matrix *result_coeffs = new Matrix(rows());
//   result_monoms = new Matrix(get_ring()->make_FreeModule(1));        // One row matrix
//   for (int j=0; j<n_cols(); j++)
//     {
//       vec f = rows()->copy(elem(j));
//       vec vmonom;
//       while (f != NULL)
//      {
//        vec g = strip_vector(f, vars, result_monoms->rows(), vmonom);
//        result_coeffs->append(g);
//        result_monoms->append(vmonom);
//      }
//     }
//   // MES: now sort both matrices...
//   return result_coeffs;
// }
#endif

Matrix /* or null */ *Matrix::monomials(M2_arrayint vars) const
// Returns a one row matrix of all of the monomials in the variable subset
// 'vars'
// which occur in 'this'.  These monomials are sorted into increasing degree
// order.
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected a matrix over a polynomial ring");
      return nullptr;
    }
  const Monoid *M = P->getMonoid();
  const Ring *K = P->getCoefficients();
  int nvars = M->n_vars();
  // Check that 'vars' is valid
  for (unsigned int i = 0; i < vars->len; i++)
    if (vars->array[i] < 0 || vars->array[i] >= nvars)
      {
        ERROR("expected a list of indices of indeterminates");
        return nullptr;
      }

  // Now collect all of the monomials
  int *mon = M->make_one();
  int *exp = newarray_atomic(int, M->n_vars());
  ring_elem one = K->from_long(1);
  exponent_table *E =
      exponent_table_new(50000, vars->len + 1);  // the +1 is for the component

  for (int c = 0; c < n_cols(); c++)
    {
      vec v = elem(c);
      for (; v != nullptr; v = v->next)
        {
          for (Nterm *t = v->coeff; t != nullptr; t = t->next)
            {
              int *exp1 = newarray_atomic(int, vars->len + 1);
              M->to_expvector(t->monom, exp);
              for (unsigned int i = 0; i < vars->len; i++)
                exp1[i] = exp[vars->array[i]];
              exp1[vars->len] = v->comp;
              exponent_table_put(E, exp1, 1);
            }
        }
    }

  // Take all of these monomials and make an array_ out of them
  MatrixConstructor mat(rows(), 0);
  const void **monoms = exponent_table_to_array(E);
  for (int i = 0; i < nvars; i++) exp[i] = 0;
  for (int i = 0; monoms[i] != 0; i += 2)
    {
      const int *exp1 = reinterpret_cast<const int *>(monoms[i]);
      for (unsigned int j = 0; j < vars->len; j++)
        exp[vars->array[j]] = exp1[j];
      int x = exp1[vars->len];  // component
      M->from_expvector(exp, mon);
      ring_elem a = P->make_flat_term(one, mon);
      mat.append(P->make_vec(x, a));
    }

  // Remove the garbage memory
  deletearray(exp);
  M->remove(mon);
  exponent_table_free(&E);

  // Finally, we sort them
  Matrix *result = mat.to_matrix();
  M2_arrayint perm = result->sort(0, -1);
  return result->sub_matrix(perm);
}

static void get_part_of_expvector(M2_arrayint vars,
                                  exponent big,
                                  int comp,
                                  exponent result)
// sets result[0..vars->len-1] with the corresponding exponents in 'big'
// sets result[vars->len] to be the component
// zeros out any variables in big which are placed into result.
//
// private routine for 'coeffs'.
{
  for (int j = 0; j < vars->len; j++)
    {
      int v = vars->array[j];
      result[j] = big[v];
      big[v] = 0;
    }
  result[vars->len] = comp;
}

static vec coeffs_of_vec(exponent_table *E,
                         M2_arrayint vars,
                         const FreeModule *F,
                         vec f)
// private routine for 'coeffs'.
#ifdef DEVELOPMENT
#warning "coeffs_of_vec should maybe be in PolynomialRing"
#endif
{
  if (f == NULL) return nullptr;
  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == nullptr) return nullptr;
  const Monoid *M = P->getMonoid();
  int *mon = M->make_one();

  // At this point, we know that we have a polynomial ring
  int nvars = M->n_vars();
  int *exp = newarray_atomic(int, nvars);
  int *scratch_exp = newarray_atomic(int, 1 + vars->len);

  vec result = nullptr;
  for (vec g = f; g != nullptr; g = g->next)
    {
      for (Nterm *h = g->coeff; h != nullptr; h = h->next)
        {
          M->to_expvector(h->monom, exp);
          get_part_of_expvector(vars, exp, g->comp, scratch_exp);
          int val = static_cast<int>(exponent_table_get(E, scratch_exp));
          if (val > 0)
            {
              M->from_expvector(exp, mon);
              ring_elem t = P->make_flat_term(h->coeff, mon);
              vec v = P->make_vec(val - 1, t);
              v->next = result;
              result = v;
            }
        }
    }
  deletearray(exp);
  deletearray(scratch_exp);
  M->remove(mon);
  P->vec_sort(result);
  return result;
}

Matrix /* or null */ *Matrix::coeffs(M2_arrayint vars,
                                     const Matrix *monoms) const
{
  // Given an array_ of variable indices, 'vars', and given
  // that 'monoms' and 'this' both have one row, makes a matrix
  // having number of rows = ncols(monoms),
  //        number of cols = ncols(this),
  // whose (r,c) entry is the coefficient (in the other variables)
  // of this[0,c] in the monomial monoms[0,r].

  // Step 0: Do some error checking
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  int nvars = P->n_vars();
  int nelements = monoms->n_cols();
  if (monoms->n_rows() != n_rows())
    {
      ERROR("expected matrices with the same number of rows");
      return nullptr;
    }
  for (unsigned int i = 0; i < vars->len; i++)
    if (vars->array[i] < 0 || vars->array[i] >= nvars)
      {
        ERROR("coeffs: expected a set of variable indices");
        return nullptr;
      }

  // Step 1: Make an exponent_table of all of the monoms.
  // We set the value of the i th monomial to be 'i+1', since 0
  // indicates a non-existent entry.

  // The extra size in monomial refers to the component:
  exponent_table *E = exponent_table_new(nelements, 1 + vars->len);
  exponent EXP = newarray_atomic(int, nvars);
  for (int i = 0; i < nelements; i++)
    {
      vec v = monoms->elem(i);
      if (v == nullptr)
        {
          ERROR("expected non-zero column");
          return nullptr;
        }
      ring_elem f = v->coeff;
      const int *m = P->lead_flat_monomial(f);
      P->getMonoid()->to_expvector(m, EXP);

      // grab only that part of the monomial we need
      exponent e = newarray_atomic(int, 1 + vars->len);
      get_part_of_expvector(vars, EXP, v->comp, e);
      exponent_table_put(E, e, i + 1);
    }

  // Step 2: for each vector column of 'this'
  //     create a column, and put this vector into result.

  MatrixConstructor mat(P->make_FreeModule(nelements), 0);
  for (int i = 0; i < n_cols(); i++)
    mat.append(coeffs_of_vec(E, vars, rows(), elem(i)));

  return mat.to_matrix();
}

MonomialIdeal *Matrix::make_monideal(
    int n,
    bool use_only_monomials_with_unit_coeffs) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  bool coeffsZZ = (P->coefficient_type() == Ring::COEFF_ZZ &&
                   use_only_monomials_with_unit_coeffs);
  const Monoid *M = P->getMonoid();
  queue<Bag *> new_elems;
  for (int i = 0; i < n_cols(); i++)
    {
      vec v = elem(i);
      if (v == nullptr) continue;
      const vecterm *w = P->vec_locate_lead_term(rows(), v);
      if (w->comp != n) continue;
      if (coeffsZZ && !globalZZ->is_unit(P->lead_flat_coeff(w->coeff)))
        continue;
      Bag *b = new Bag(i);
      M->to_varpower(P->lead_flat_monomial(w->coeff), b->monom());
      new_elems.insert(b);
    }

  // If the base ring is a quotient ring, include these lead monomials.
  for (int i = 0; i < P->n_quotients(); i++)
    {
      Nterm *f = P->quotient_element(i);
      if (coeffsZZ && !globalZZ->is_unit(f->coeff)) continue;
      Bag *b = new Bag(-1);
      M->to_varpower(f->monom, b->monom());
      new_elems.insert(b);
    }

  // If the base ring has skew commuting variables, include their squares
  if (P->is_skew_commutative())
    {
      for (int i = 0; i < M->n_vars(); i++)
        if (P->is_skew_var(i))
          {
            Bag *b = new Bag(-1);
            varpower::var(i, 2, b->monom());
            new_elems.insert(b);
          }
    }

  MonomialIdeal *result = new MonomialIdeal(P, new_elems);
  return result;
}

int Matrix::dimension1() const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const Ring *K = (P != nullptr ? P->getCoefficientRing() : get_ring());
  bool is_ZZ = K->is_ZZ();
  int base = (is_ZZ ? 1 : 0);
  int result = -1;
  if (P != nullptr)
    {
      int n = P->n_vars();
      for (int i = 0; i < n_rows(); i++)
        {
          MonomialIdeal *mi = make_monideal(i);
          AssociatedPrimes ap(mi);
          int d = n - ap.codimension();
          if (d > result) result = d;
        }
      if (result != -1) result += base;
      return result;
    }
  else
    {
      // This handles the case when the coefficients are a field, or ZZ
      int i, j;
      int *dims = newarray_atomic(int, n_rows());
      for (i = 0; i < n_rows(); i++) dims[i] = base;
      for (j = 0; j < n_cols(); j++)
        {
          vec f = elem(j);
          if (f == nullptr) continue;
          if (dims[f->comp] == -1) continue;
          if (K->is_unit(f->coeff))
            dims[f->comp] = -1;
          else
            dims[f->comp] = 0;
        }
      for (i = 0; i < n_rows(); i++)
        if (dims[i] > result) result = dims[i];
      deletearray(dims);
      return result;
    }
}

const Matrix /* or null */ *Matrix::content() const
{
  const Ring *R = get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  const Ring *targetR = (P == nullptr ? R : P->getCoefficients());

  const FreeModule *F = targetR->make_FreeModule(1);
  MatrixConstructor mat(F, n_cols());
  for (int i = 0; i < n_cols(); i++)
    mat.set_entry(0, i, R->vec_content(elem(i)));
  return mat.to_matrix();
}

const Matrix /* or null */ *Matrix::remove_content() const
{
  const Ring *R = get_ring();
  MatrixConstructor mat(rows(), cols(), degree_shift());
  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, R->vec_divide_by_content(elem(i)));
  return mat.to_matrix();
}

const Matrix /* or null */ *Matrix::split_off_content(
    const Matrix /* or null */ *&result) const
{
  const Ring *R = get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  const Ring *targetR = (P == nullptr ? R : P->getCoefficients());

  const FreeModule *F = targetR->make_FreeModule(1);
  MatrixConstructor mat_content(F, n_cols());
  MatrixConstructor mat(rows(), cols(), degree_shift());

  for (int i = 0; i < n_cols(); i++)
    {
      vec g;
      ring_elem c = R->vec_split_off_content(elem(i), g);
      mat_content.set_entry(0, i, c);
      mat.set_column(i, g);
    }
  result = mat.to_matrix();
  return mat_content.to_matrix();
}

Matrix /* or null */ *Matrix::clean(gmp_RR epsilon) const
{
  MatrixConstructor mat(rows(), cols(), degree_shift());
  for (int i = 0; i < n_cols(); i++)
    mat.set_column(i, get_ring()->vec_zeroize_tiny(epsilon, elem(i)));
  return mat.to_matrix();
}

gmp_RRorNull Matrix::norm(gmp_RR p) const
{
  const Ring *R = get_ring();
  if (R->get_precision() == 0)
    {
      ERROR("expected ring over an RR or CC");
      return nullptr;
    }
  gmp_RRmutable nm = getmemstructtype(gmp_RRmutable);
  mpfr_init2(nm, mpfr_get_prec(p));
  mpfr_ui_div(nm, 1, p, GMP_RNDN);
  if (!mpfr_zero_p(nm))
    {
      ERROR("Lp norm only implemented for p = infinity");
      mpfr_clear(nm);
      return nullptr;
    }

  for (int i = 0; i < n_cols(); i++) R->vec_increase_maxnorm(nm, elem(i));

  return moveTo_gmpRR(nm);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
