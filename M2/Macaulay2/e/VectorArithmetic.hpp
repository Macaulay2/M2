#ifndef __vector_arithmetic__
#define __vector_arithmetic__

#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for VECTOR
#include "MemoryBlock.hpp"
#include "ringelem.hpp"
#include <variant>

#include "aring-glue.hpp"

#include <tbb/tbb.h>

class Ring;
union ring_elem;
using ComponentIndex = int;

class CoeffVector
{
  template<typename T>
  friend class ConcreteVectorArithmetic;
  template<typename T>
  friend class ConcreteVectorArithmetic2;
  // disallow copy...
 public:
  CoeffVector() : mValue(nullptr) {}
  bool isNull() const { return mValue == nullptr; }
  void swap(CoeffVector& b) { std::swap(mValue, b.mValue); }
 private:
  void* mValue;
};

class DenseCoeffVector
{
  template<typename T>
  friend class ConcreteVectorArithmetic;
  template<typename T>
  friend class ConcreteVectorArithmetic2;
  // disallow copy...
 public:
  DenseCoeffVector() : mValue(nullptr) {}
  bool isNull() const { return mValue == nullptr; }
  void swap(DenseCoeffVector& b) { std::swap(mValue, b.mValue); }
 private:
  void* mValue;
};

class AbstractVectorArithmetic
{
public:
  virtual size_t size(const CoeffVector& coeffs) const = 0;
  virtual size_t size(const DenseCoeffVector& coeffs) const = 0;

  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  /// Create a coefficient vector with room for `nelems` coefficients
  virtual CoeffVector allocateCoeffVector(ComponentIndex nelems) const = 0;
  virtual DenseCoeffVector allocateDenseCoeffVector(ComponentIndex nelems) const = 0;

  /// Create a coefficient vector with 0 elements.  Can be increased in size.
  // virtual CoeffVector allocateCoeffVector() const = 0;

  /// Deallocate all the coefficients, and the array itself.
  virtual void deallocateCoeffVector(CoeffVector& coeffs) const = 0;
  virtual void deallocateCoeffVector(DenseCoeffVector& coeffs) const = 0;

  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////
  virtual void sparseRowToDenseRow(DenseCoeffVector& dense,
                                   const CoeffVector& coeffs,
                                   const Range<int>& comps) const = 0;

  virtual void denseRowCancelFromSparse(DenseCoeffVector& dense,
                                        const CoeffVector& coeffs,
                                        const Range<int>& comps) const = 0;

  virtual int denseRowNextNonzero(DenseCoeffVector& dense,
                                  int first,
                                  int last) const = 0;

  virtual void denseRowToSparseRow(DenseCoeffVector& dense,
                                   CoeffVector& coeffs, // sets coeffs
                                   Range<int>& comps, // sets comps
                                   int first,
                                   int last,
                                   MemoryBlock& monomialSpace) const = 0; 

  // want to do...
  // template<typename LockType>
  // virtual void safeDenseRowToSparseRow(DenseCoeffVector& dense,
  //                                      CoeffVector& coeffs, // sets coeffs
  //                                      Range<int>& comps, // sets comps
  //                                      int first,
  //                                      int last,
  //                                      MemoryBlock& monomialSpace,
  //                                      LockType& lock) const = 0;
  
  // but we can't because you can't have pure virtual function templates
  // making the CRTP changes will avoid this.
  virtual void safeDenseRowToSparseRow(DenseCoeffVector& dense,
                                       CoeffVector& coeffs, // sets coeffs
                                       Range<int>& comps, // sets comps
                                       int first,
                                       int last,
                                       MemoryBlock& monomialSpace,
                                       tbb::queuing_mutex& lock) const = 0;

  virtual void safeDenseRowToSparseRow(DenseCoeffVector& dense,
                                       CoeffVector& coeffs, // sets coeffs
                                       Range<int>& comps, // sets comps
                                       int first,
                                       int last,
                                       MemoryBlock& monomialSpace,
                                       tbb::null_mutex& lock) const = 0;

  virtual void sparseRowMakeMonic(CoeffVector& coeffs) const = 0;

  /////////////////////////////
  /// Translation    //////////
  /////////////////////////////

  // want to do these...
  // template<template<typename> class Container>
  // virtual void appendSparseVectorToContainer(CoeffVector& coeffs,
  //                                            Container<ring_elem>& c) const = 0;

  // template<template<typename> class Container>
  // virtual void fillSparseVectorFromContainer(CoeffVector& coeffs,
  //                                            Container<ring_elem>& c) const = 0;

  // will also allow to template off the container once CRTP changes made
  virtual void appendSparseVectorToContainer(const CoeffVector& coeffs,
                                             VECTOR(ring_elem)& c) const = 0;

  virtual CoeffVector sparseVectorFromContainer(const VECTOR(ring_elem)& c) const = 0;

  virtual ring_elem ringElemFromSparseVector(const CoeffVector& coeffs,
                                             int index) const = 0;

};

typedef AbstractVectorArithmetic VectorArithmetic;

// this should really be the constructor for the VectorArithmetic class
// which performs the various ConcreteVA class creations
const VectorArithmetic* vectorArithmetic(const Ring* R);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:


