/* Copyright 2017 Mahrud Sayrafi and Michael E. Stillman
   Mahrud Sayrafi's code in this file is in the public domain. */

#include "mutablecomplex.hpp"
#include "engine-includes.hpp"
#include "debug.hpp"
#include "ring.hpp"
#include "polyring.hpp"
#include <utility>
#include <iostream>
#include <algorithm>

// TODO: make enums class
#define FLAG_TRACE_MORPHISMS 1  // See `help pruningMap` in M2
#define FLAG_TRIM_COMPLEX 2     // Delete pruned rows and columns
#define FLAG_PRUNE_PMONE 4      // Only prune -1,+1
#define FLAG_PRUNE_SCALARS 8    // Only prune constants
#define FLAG_PRUNE_ORIGIN 16    // Only prune functions with constants
#define FLAG_PRUNE_MAXIMAL 32   // Pruning for maximal ideal
#define FLAG_PRUNE_PRIME 64     // Pruning for prime ideal
#define FLAG_BEST_UNIT 1024     // Prune sparsest unit first
#define FLAG_BEST_MATRIX 2048   // Prune best matrix first
#define FLAG_REV_ORDER 65536    // Prune the matrices in reverse order

size_t MutableComplex::complexity(const iterator &i, const size_t flags) const
// Heuristics
// TODO: save row/column totals in a separate vector and only update when
// pruning a unit
{
  ring_elem e;
  const size_t n = i.index();
  const std::pair<size_t, size_t> u = *i;
  mDifferential[n]->get_entry(u.first, u.second, e);
  // Count terms in the column
  int s = 0;
  if (mLocalRing == 0)
    s = mPolynomialRing->n_terms(e);
  else
    s = mLocalRing->n_terms(e);
  for (size_t r = 0; r < mBetti[n]; ++r)
    {
      if (r == u.first) continue;
      mDifferential[n]->get_entry(r, u.second, e);
      if (mRing->is_zero(e)) continue;
      if (mLocalRing == 0)
        s += mPolynomialRing->n_terms(e);
      else
        s += mLocalRing->n_terms(e);
    }
  // Count terms in the row
  for (size_t c = 0; c < mBetti[n + 1]; ++c)
    {
      if (c == u.second) continue;
      mDifferential[n]->get_entry(u.first, c, e);
      if (mRing->is_zero(e)) continue;
      if (mLocalRing == 0)
        s += mPolynomialRing->n_terms(e);
      else
        s += mLocalRing->n_terms(e);
    }
  // std::cout<<s<<std::endl;
  // Local case: (size numerator elt) * (row/numerator/size//sum) *
  // (col/numerator/size//sum)
  return static_cast<size_t>(s);
}

bool MutableComplex::next_unit(iterator &i, const size_t flags) const
{
  ring_elem e;
  for (; i < i.end(); ++i)
    {
      mDifferential[i.index()]->get_entry((*i).first, (*i).second, e);
      if (mRing->is_unit(e)) return true;
    }
  return false;
}

bool MutableComplex::find_unit(iterator &i, const size_t flags) const
{
  if (!(flags & FLAG_BEST_UNIT)) return next_unit(i, flags);

  iterator j(*this, i.index());
  if (!next_unit(j, flags)) return false;
  size_t t = -1, s = complexity(j, flags);
  while (next_unit(j, flags))
    {
      s = complexity(j, flags);
      if (s < t)
        {
          t = s;
          *i = *j;
        }
      ++j;
    }
  return true;
}

// bool MutableComplex::unit_cmp(const iterator &a, const iterator &b);

std::vector<MutableComplex::iterator> MutableComplex::list_units(
    size_t n,
    const size_t flags) const
{
  iterator i(*this, n);
  std::vector<iterator> units;
  while (next_unit(i, flags))
    {
      units.push_back(i);
      ++i;
    }
  // sort units here?
  // std::sort(units.begin(), units.end(), unit_cmp());
  return units;
}

void MutableComplex::prune_unit(const iterator &i, const size_t flags)
{
  const size_t n = i.index();
  const std::pair<size_t, size_t> u = *i;
  const bool trace = flags & FLAG_TRACE_MORPHISMS;
  /* There is some redundancy here, can set some stuff to zero.
     TODO: Maybe check to see which is more efficient?
           Compare with using rawReduceByPivot
  */
  // Move to last row
  mDifferential[n]->interchange_rows(u.first, mBetti[n] - 1);
  if (0 < n) mDifferential[n - 1]->interchange_columns(u.first, mBetti[n] - 1);
  if (trace) mMorphisms[n]->interchange_columns(u.first, mBetti[n] - 1);
  // Move to last column
  mDifferential[n]->interchange_columns(u.second, mBetti[n + 1] - 1);
  if (n < mDifferential.size() - 1)
    mDifferential[n + 1]->interchange_rows(u.second, mBetti[n + 1] - 1);
  if (trace)
    mMorphisms[n + 1]->interchange_columns(u.second, mBetti[n + 1] - 1);
  // Get the pivot
  ring_elem p, q, f;
  mDifferential[n]->get_entry(mBetti[n] - 1, mBetti[n + 1] - 1, p);
  p = mRing->invert(p);
  q = mRing->negate(p);
  // Clear the column
  for (size_t r = 0; r < mBetti[n] - 1; ++r)
    {
      mDifferential[n]->get_entry(r, mBetti[n + 1] - 1, f);
      mDifferential[n]->row_op(r, mRing->mult(f, q), mBetti[n] - 1);
      if (0 < n)
        mDifferential[n - 1]->column_op(mBetti[n] - 1, mRing->mult(f, p), r);
      if (trace) mMorphisms[n]->column_op(mBetti[n] - 1, mRing->mult(f, p), r);
    }
  // Clear the row
  for (size_t c = 0; c < mBetti[n + 1] - 1; ++c)
    {
      mDifferential[n]->get_entry(mBetti[n] - 1, c, f);
      mDifferential[n]->column_op(c, mRing->mult(f, q), mBetti[n + 1] - 1);
      if (n < mDifferential.size() - 1)
        mDifferential[n + 1]->row_op(mBetti[n + 1] - 1, mRing->mult(f, p), c);
      if (trace)
        mMorphisms[n + 1]->column_op(c, mRing->mult(f, q), mBetti[n + 1] - 1);
    }
  --mBetti[n];
  --mBetti[n + 1];
}

// Prunes a single matrix by reducing the units
void MutableComplex::prune_matrix(size_t n, size_t flags)
{
  iterator i(*this, n);
  while (find_unit(i, flags))
    {
      prune_unit(i, flags);
      *i = *iterator(*this, n);
    }
}

void MutableComplex::prune_complex(size_t nsteps, size_t flags)
{
  // Initialize pruning maps
  const bool dense = mDifferential[0]->is_dense();
  if (flags & FLAG_TRACE_MORPHISMS)
    for (size_t n = 0; n < mDifferential.size() + 1; ++n)
      mMorphisms.push_back(MutableMatrix::identity(mRing, mBetti[n], dense));
  for (size_t n = 0; n < mDifferential.size() && n < nsteps; n++)
    {
      if (flags & FLAG_REV_ORDER)
        prune_matrix(mDifferential.size() - n - 1, flags);
      else
        prune_matrix(n, flags);
    }
}

std::vector<size_t> MutableComplex::prune_betti(size_t nsteps, size_t flags)
{
  /* TODO:
   * separate all units into a square matrix (efficiently?)
   * start with the ones in sparcest row/column.
   */
  //  prune_complex(nsteps, flags);
  return mBetti;
}

VECTOR(MutableMatrix *)
MutableComplex::prune_morphisms(size_t nsteps, size_t flags)
{
  for (size_t n = 0; n < mMorphisms.size(); ++n)
    if (flags & FLAG_TRACE_MORPHISMS)
      if (mBetti[n] < mMorphisms[n]->n_cols())
        mMorphisms[n]->delete_columns(mBetti[n], mMorphisms[n]->n_cols() - 1);
  return mMorphisms;
}

/*
MutableComplex* MutableComplex::trim_complex(size_t nsteps, size_t flags)
{
  M2_arrayint rows, cols;
  VECTOR(MutableMatrix *) D;
  for(size_t n = 0; n < mDifferential.size(); n++)
    {
      rows = M2_makearrayint(mBetti[n]);
      for(int i = 0; i < mBetti[n]; ++i) rows->array[i] = i;
      cols = M2_makearrayint(mBetti[n+1]);
      for(int i = 0; i < mBetti[n+1]; ++i) cols->array[i] = i;

      D.push_back(mDifferential[n]->submatrix(rows, cols));
    }
  // FIXME how to access the matrices?
  return new MutableComplex(D);
}
*/

void MutableComplex::text_out(buffer &o) const
{
  for (size_t i = 0; i < mDifferential.size(); i++) o << mBetti[i] << " <-- ";
  o << mBetti[mDifferential.size()];

  // auto C = const_cast <MutableComplex *> (this);
}

/********************************************************************************/
/*                               Global functions */
/********************************************************************************/

extern "C" { // TODO: remove when this function is in e/interface

engine_RawMutableMatrixArray rawPruningMorphism(MutableComplex *C, int n, int f)
{
  size_t nsteps = static_cast<size_t>(n), flags = static_cast<size_t>(f);
  //  std::min(static_cast<int>(C->mMorphisms.size()),
  //  static_cast<int>(nsteps));
  VECTOR(MutableMatrix *) M = C->prune_morphisms(nsteps, flags);
  int N = static_cast<int>(M.size());
  engine_RawMutableMatrixArray A =
      getmemarraytype(engine_RawMutableMatrixArray, N);
  A->len = N;
  for (int i = 0; i < N; ++i) A->array[i] = M[i];
  return A;
}

M2_arrayint rawPruneBetti(MutableComplex *C, int n, int f)
{
  size_t nsteps = static_cast<size_t>(n), flags = static_cast<size_t>(f);
  std::vector<size_t> betti = C->prune_betti(nsteps, flags);
  //  std::min(static_cast<int>(betti.size()), static_cast<int>(nsteps));
  int N = static_cast<int>(betti.size());
  M2_arrayint B = M2_makearrayint(N);
  for (int i = 0; i < N; ++i) B->array[i] = static_cast<int>(betti[i]);
  return B;
}

MutableComplex *rawPruneComplex(MutableComplex *C, int n, int f)
{
  size_t nsteps = static_cast<size_t>(n), flags = static_cast<size_t>(f);
  C->prune_complex(nsteps, flags);
  //  if (flags & FLAG_TRIM_COMPLEX)
  //    C = C->trim_complex(nsteps, flags);
  return C;
}

MutableComplex *rawMutableComplex(const engine_RawMutableMatrixArray M)
{
  VECTOR(MutableMatrix *) D;
  for (int i = 0; i < M->len; ++i) D.push_back(M->array[i]);
  return new MutableComplex(D);
}

M2_string rawMutableComplexToString(const MutableComplex *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

unsigned int rawMutableComplexHash(const MutableComplex *M)
{
  return M->hash();
}

} // TODO: remove when this function is in e/interface

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
