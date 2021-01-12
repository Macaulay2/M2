#ifndef __vector_arithmetic_2__
#define __vector_arithmetic_2__

#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for VECTOR

#include <tbb/tbb.h>

class Ring;
union ring_elem;
using ComponentIndex = int;
class VectorArithmeticZZpFlint;
class MemoryBlock;

class CoefficientVector2
{
  friend class VectorArithmeticZZpFlint;
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
  friend class VectorArithmeticZZpFlint;
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
  // virtual CoefficientVector2 allocateCoefficientVector() const = 0;

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
                                   int first,
                                   int last,
                                   MemoryBlock& monomialSpace) const = 0; 

  virtual void safeDenseRowToSparseRow(DenseCoefficientVector2& dense,
                                       CoefficientVector2& coeffs, // sets coeffs
                                       Range<int>& comps, // sets comps
                                       int first,
                                       int last,
                                       MemoryBlock& monomialSpace,
                                       tbb::queuing_mutex& lock) const = 0; 

  virtual void sparseRowMakeMonic(CoefficientVector2& coeffs) const = 0;

  /////////////////////////////
  /// Translation    //////////
  /////////////////////////////

  // want to do these...
  // template<template<typename> class Container>
  // virtual void appendSparseVectorToContainer(CoefficientVector2& coeffs,
  //                                            Container<ring_elem>& c) const = 0;

  // template<template<typename> class Container>
  // virtual void fillSparseVectorFromContainer(CoefficientVector2& coeffs,
  //                                            Container<ring_elem>& c) const = 0;

  virtual void appendSparseVectorToContainer(const CoefficientVector2& coeffs,
                                             VECTOR(ring_elem)& c) const = 0;

  virtual CoefficientVector2 sparseVectorFromContainer(const VECTOR(ring_elem)& c) const = 0;

  virtual ring_elem ringElemFromSparseVector(const CoefficientVector2& coeffs,
                                             int index) const = 0;

};

const VectorArithmetic2* vectorArithmetic2(const Ring* R);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:


