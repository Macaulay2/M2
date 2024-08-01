/* Copyright 2017 Mahrud Sayrafi and Michael E. Stillman
   Mahrud Sayrafi's code in this file is in the public domain. */

#ifndef _mutablecomplex_hpp_
#define _mutablecomplex_hpp_

#include "localring.hpp"
#include "style.hpp"
#include "hash.hpp"
#include "mat.hpp"
#include "debug.hpp"
#include <iostream>

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
