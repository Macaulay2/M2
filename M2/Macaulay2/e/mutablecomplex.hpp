/* Copyright 2017 Mahrud Sayrafi and Michael E. Stillman
   Mahrud Sayrafi's code in this file is in the public domain. */

#ifndef _mutablecomplex_hpp_
#define _mutablecomplex_hpp_

/**
 * @file mutablecomplex.hpp
 * @brief `MutableComplex` --- in-place chain complex of `MutableMatrix` differentials.
 *
 * Declares `MutableComplex`, a `MutableEngineObject` subclass
 * carrying a sequence of `MutableMatrix*` differentials chained
 * together with a parallel `mBetti` vector of dimensions and
 * cached `mRing` / `mLocalRing` / `mPolynomialRing` pointers.
 * The constructor accepts a `VECTOR(MutableMatrix*)` and infers
 * the ring contexts from the first matrix (`get_ring` /
 * `cast_to_LocalRing` / `cast_to_PolynomialRing`). The `prune_*`
 * family is the working API: `prune_unit(iter, flags)` removes
 * one unit entry and propagates the corresponding row / column
 * update to the neighbouring differential to preserve the
 * composition-zero invariant; `prune_matrix(n, flags)` reduces
 * one matrix's worth of units; `prune_complex(nsteps, flags)`
 * runs the same pass across the whole complex; and
 * `prune_betti` / `prune_morphisms` extract the new dimensions
 * / the reduction morphisms after pruning. The nested
 * `iterator` class walks `(matrix_index, (row, col))` positions
 * and `next_unit` / `find_unit` / `list_units` locate
 * unit-pivot entries to prune.
 *
 * Typical workflows construct a `MutableComplex` from an
 * existing resolution (`comp-res.hpp`) and reduce it to a
 * minimal form by sequential unit pruning. The TODO at the top
 * of the header flags the open question of templating over
 * sparse vs. dense `MutableMatrix`.
 *
 * @see mat.hpp
 * @see comp-res.hpp
 * @see localring.hpp
 */

#include "localring.hpp"
#include "style.hpp"
#include "hash.hpp"
#include "mat.hpp"
#include "debug.hpp"
#include <iostream>

/**
 * @brief Sequence of `MutableMatrix` differentials representing an in-progress
 * chain complex, used for engine-side minimisation / pruning.
 *
 * @details Holds the underlying ring (`mRing`), and, if applicable, its
 * `LocalRing` / `PolynomialRing` casts so `prune_*` can switch to
 * the local-ring path when warranted. `mDifferential[i]` is the
 * matrix from position `i+1` to position `i`, and `mBetti` caches
 * the per-position Betti numbers (matrix sizes). The pruning API
 * (`prune_unit`, `prune_matrix`, `prune_complex`) walks over
 * `(matrix, row, col)` triples produced by the nested `iterator`
 * and uses any unit entry it finds to row/column reduce the
 * complex in place, lowering the Betti numbers while preserving
 * the homology.
 */
// TODO how to seamlessly use sparse or dense mutable matrices?
// template <typename MutableMatrix>
class MutableComplex : public MutableEngineObject
{
 public:
  MutableComplex() {}
  MutableComplex(VECTOR(MutableMatrix *) & D)
      : mRing(D[0]->get_ring()),
        mLocalRing(mRing->cast_to_LocalRing()),
        mPolynomialRing(mLocalRing == nullptr ? mRing->cast_to_PolynomialRing()
                                        : mLocalRing->get_ring()),
        mDifferential(D)
  {
#if 0
    if (mLocalRing != 0)
      std::cout << "Got a mutable complex over a local ring." << std::endl;
    if (mPolynomialRing == 0)
      std::cout << "Not a polynomial ring or local ring." << std::endl;
#endif
    for (size_t i = 0; i < D.size(); ++i) mBetti.push_back(D[i]->n_rows());
    mBetti.push_back(D[D.size() - 1]->n_cols());
    // TODO: Check to make sure mBetti's are compatible, or define isWellDefined
  }
  virtual ~MutableComplex() {}  // destructor

  class iterator;

  size_t complexity(const iterator &i, const size_t flags) const;
  bool next_unit(iterator &i, const size_t flags) const;
  bool find_unit(iterator &i, const size_t flags) const;
  // TODO improve list_units to move all units in a square
  std::vector<iterator> list_units(size_t n, const size_t flags) const;

  void prune_unit(const iterator &i, const size_t flags);
  void prune_matrix(size_t n, const size_t flags);
  void prune_complex(const size_t nsteps, const size_t flags);
  std::vector<size_t> prune_betti(const size_t nsteps, const size_t flags);
  VECTOR(MutableMatrix *)
  prune_morphisms(const size_t nsteps, const size_t flags);
  //  MutableComplex* trim_complex(const size_t nsteps, const size_t flags)

  /**
   * @brief Cursor pointing at one entry of one differential matrix in the
   * complex: a `(matrix index, (row, col))` triple.
   *
   * @details `mIndex` selects which `MutableMatrix` in `mDifferential` and
   * `mAddr` selects a position inside it. The pruning API uses
   * iterators to walk candidate unit entries; `next_unit` /
   * `find_unit` advance the cursor and `prune_unit(it)` row /
   * column reduces the complex around that entry.
   */
  class iterator
  {
   public:
    iterator(const MutableComplex &C,
             const size_t n,
             std::pair<size_t, size_t> m)
        : mComplex(C), mIndex(n), mAddr(m)
    {
    }
    iterator(const MutableComplex &C, const size_t n)
        : mComplex(C), mIndex(n), mAddr(std::pair<size_t, size_t>(0, 0))
    {
    }
    iterator(const iterator &i, std::pair<size_t, size_t> m)
        : mComplex(i.mComplex), mIndex(i.mIndex), mAddr(m)
    {
    }

    size_t index() const { return mIndex; }

    std::pair<size_t, size_t> &operator*() { return mAddr; }
    const std::pair<size_t, size_t> &operator*() const { return mAddr; }

    iterator &operator++()
    {
      ++mAddr.second;
      if (mAddr.second >= mComplex.mBetti[mIndex + 1])
        {
          ++mAddr.first;
          mAddr.second = 0;
        }
      return *this;
    }
    // TODO: define != instead
    bool operator<(const iterator &o) const
    {
      return mIndex < o.mIndex || mAddr < o.mAddr;
    }
    iterator end() const
    {
      return iterator(*this,
                      std::pair<size_t, size_t>(mComplex.mBetti[mIndex], 0));
    }

   private:
    const MutableComplex &mComplex;
    const size_t mIndex;
    std::pair<size_t, size_t> mAddr;
  };

  void text_out(buffer &o) const;

 private:
  const Ring *mRing;
  const LocalRing *mLocalRing;
  const PolynomialRing *mPolynomialRing;  // FIXME change to PolyRing?
  VECTOR(MutableMatrix *) mDifferential;
  VECTOR(MutableMatrix *) mMorphisms;
  VECTOR(MutableMatrix *) mDegrees; // TODO keep track of the degree changes
  std::vector<size_t> mBetti;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
