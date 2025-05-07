// Copyright 2014  Michael E. Stillman

#ifndef _dmat_gf_flint__hpp_
#define _dmat_gf_flint__hpp_

#include <utility>                // for swap
#include "aring-gf-flint.hpp"     // for ARingGFFlint

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fq_nmod_mat.h>  // for fq_zech_mat_entry, fq_zech_mat_clear
#include <flint/fq_zech_mat.h>  // for fq_zech_mat_t
#pragma GCC diagnostic pop

template <typename ACoeffRing>
class DMat;

/////////////////////////////////////////////////////////////////
// Flint: use fq_nmod_mat for implementation of dense matrices //
/////////////////////////////////////////////////////////////////
template <>
class DMat<M2::ARingGFFlint>
{
 public:
  typedef M2::ARingGFFlint ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef ACoeffRing::ElementType ElementType;
  // typedef ElementType elem;
  // typedef ACoeffRing::Element Element;

  DMat() : mRing(0) {}
  DMat(const ACoeffRing& R, size_t nrows, size_t ncols) : mRing(&R)
  {
    fq_zech_mat_init(mArray, nrows, ncols, ring().flintContext());
  }

  DMat(const DMat<ACoeffRing>& M) : mRing(&M.ring())
  {
    fq_zech_mat_init_set(mArray, M.mArray, ring().flintContext());
  }

  ~DMat() { fq_zech_mat_clear(mArray, ring().flintContext()); }

  // swap the actual matrices of 'this' and 'M'.
  // The rings must be the same.
  void swap(DMat<ACoeffRing>& M)
  {
    std::swap(mRing, M.mRing);
    std::swap(mArray, M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const
  {
    return fq_zech_mat_nrows(mArray, ring().flintContext());
  }
  size_t numColumns() const
  {
    return fq_zech_mat_ncols(mArray, ring().flintContext());
  }

  ElementType& entry(size_t row, size_t column)
  {
    return *fq_zech_mat_entry(mArray, row, column);
  }
  const ElementType& entry(size_t row, size_t column) const
  {
    return *fq_zech_mat_entry(mArray, row, column);
  }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }

  // These are labelled 'unsafe', as it s possible the rows
  // are out of order (which happens in particular if
  // certain flint functions created this.
  const ElementType* unsafeArray() const { return mArray->entries; }
  ElementType*& unsafeArray() { return mArray->entries; }

 public:
  // Access routines so that the flint fq_zech_mat interface may be used
  const fq_zech_mat_t& fq_zech_mat() const { return mArray; }
  fq_zech_mat_t& fq_zech_mat() { return mArray; }
 private:
  const ACoeffRing* mRing;
  fq_zech_mat_t mArray;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
