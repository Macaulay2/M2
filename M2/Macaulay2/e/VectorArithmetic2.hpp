#ifndef __vector_arithmetic_2__
#define __vector_arithmetic_2__

#include "NCAlgebras/Range.hpp"  // for Range

class Ring;
union ring_elem;
using ComponentIndex = long;
class VectorArithmeticZZp;
class MemoryBlock2;

class CoefficientVector2
{
  friend class VectorArithmeticZZp;
  // disallow copy...
 public:
  CoefficientVector2() : mValue(nullptr) {}
  bool isNull() const { return mValue == nullptr; }
  void swap(CoefficientVector2& b) { std::swap(mValue, b.mValue); }
 private:
  void* mValue;
};

class DenseCoefficientVector2
{
  friend class VectorArithmeticZZp;
  // disallow copy...
 public:
  DenseCoefficientVector2() : mValue(nullptr) {}
  bool isNull() const { return mValue == nullptr; }
  void swap(DenseCoefficientVector2& b) { std::swap(mValue, b.mValue); }
 private:
  void* mValue;
};

class VectorArithmetic2
{
public:
  virtual size_t size(const CoefficientVector2& coeffs) const = 0;
  virtual size_t size(const DenseCoefficientVector2& coeffs) const = 0;

  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  /// Create a coefficient vector with room for `nelems` coefficients
  virtual CoefficientVector2 allocateCoefficientVector(ComponentIndex nelems) const = 0;
  virtual DenseCoefficientVector2 allocateDenseCoefficientVector(ComponentIndex nelems) const = 0;

  /// Create a coefficient vector with 0 elements.  Can be increased in size.
  virtual CoefficientVector2 allocateCoefficientVector() const = 0;

  /// Deallocate all the coefficients, and the array itself.
  virtual void deallocateCoefficientVector(CoefficientVector2& coeffs) const = 0;
  virtual void deallocateCoefficientVector(DenseCoefficientVector2& coeffs) const = 0;

  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////
  virtual void sparseRowToDenseRow(DenseCoefficientVector2& dense,
                                   const CoefficientVector2& coeffs,
                                   const Range<int>& comps) const = 0;

  virtual void denseRowCancelFromSparse(DenseCoefficientVector2& dense,
                                        const CoefficientVector2& coeffs,
                                        const Range<int>& comps) const = 0;

  virtual int denseRowNextNonzero(DenseCoefficientVector2& dense,
                                  int first,
                                  int last) const = 0;

  virtual void denseRowToSparseRow(DenseCoefficientVector2& dense,
                                   CoefficientVector2& coeffs, // sets coeffs
                                   Range<int>& comps, // sets comps
                                   MemoryBlock2& monomialSpace,
                                   int first,
                                   int last) const = 0; // TODO: have a MemoryBlock2 entry for where to put comps (and perhaps coeffs?)

  virtual void sparseRowMakeMonic(CoefficientVector2& coeffs) const = 0;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:


