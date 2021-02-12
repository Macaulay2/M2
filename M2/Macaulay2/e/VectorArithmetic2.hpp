#ifndef __vector_arithmetic_2__
#define __vector_arithmetic_2__

#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for VECTOR
#include "ringelem.hpp"
#include "aring-glue.hpp"
#include "VectorArithmetic.hpp"
#include <variant>
#include <functional>

#include <tbb/tbb.h>

class Ring;
union ring_elem;
using ComponentIndex = int;
class MemoryBlock;

/*
class CoeffVector
{
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
  friend class ConcreteVectorArithmetic2;
  // disallow copy...
 public:
  DenseCoeffVector() : mValue(nullptr) {}
  bool isNull() const { return mValue == nullptr; }
  void swap(DenseCoeffVector& b) { std::swap(mValue, b.mValue); }
 private:
  void* mValue;
};
*/

template<typename RingType>
class ConcreteVectorArithmetic2
{
  // This class is valid for prime characteristic up to a certain size.
  using RT = RingType;
  using FieldElement = typename RT::ElementType;
  using DenseFieldElement = FieldElement;

  friend class VectorArithmetic2;

  ConcreteVectorArithmetic2() : mRing(nullptr) {}

private:
  const RT* mRing;

  std::vector<FieldElement>* coeffVector(CoeffVector f) const
  {
    return reinterpret_cast<std::vector<FieldElement>*>(f.mValue);
  }

  std::vector<DenseFieldElement>* denseCoeffVector(DenseCoeffVector f) const
  {
    return reinterpret_cast<std::vector<DenseFieldElement>*>(f.mValue);
  }
  
  CoeffVector coeffVector(std::vector<FieldElement>* vals) const
  {
    CoeffVector result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

  DenseCoeffVector denseCoeffVector(std::vector<DenseFieldElement>* vals) const
  {
    DenseCoeffVector result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

public:
  ConcreteVectorArithmetic2(const RT* R) : mRing(R) {}
  
  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  CoeffVector allocateCoeffVector(ComponentIndex nelems) const
  {
    auto tempPtr = new std::vector<FieldElement>(nelems);
    for (auto& d : *tempPtr)
      {
	mRing->init(d);
	mRing->set_zero(d);
      }
    return coeffVector(tempPtr);    
  }

  DenseCoeffVector allocateDenseCoeffVector(ComponentIndex nelems) const
  {
    auto tempPtr = new std::vector<DenseFieldElement>(nelems);
    for (auto& d : *tempPtr)
      {
	mRing->init(d);
	mRing->set_zero(d);
      }
    return denseCoeffVector(tempPtr);    
  }
  // CoeffVector allocateCoeffVector() const
  // {
  //   return coeffVector(new std::vector<FieldElement>);
  // }
  void deallocateCoeffVector(CoeffVector& coeffs) const
  {
    delete coeffVector(coeffs);
  }

  void deallocateCoeffVector(DenseCoeffVector& coeffs) const
  {
    delete denseCoeffVector(coeffs);
  }
  
  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////

  size_t size(const CoeffVector& coeffs) const
  {
    return coeffVector(coeffs)->size();
  }

  size_t size(const DenseCoeffVector& coeffs) const
  {
    return denseCoeffVector(coeffs)->size();
  }
  
  // better name: fillDenseRow.
  void sparseRowToDenseRow(DenseCoeffVector& dense,
                           const CoeffVector& sparse,
                           const Range<int>& comps) const
  {
    // Note: this function simply fills in the values coming from '(sparse, comps)'.
    //   Other values are not touched.
    // In our intended uses, the input `dense` is the vector consisting of all zeros`.
    
    auto& dvec = * denseCoeffVector(dense);
    auto& svec = * coeffVector(sparse);
    
    assert(comps.size() == svec.size());
    assert(comps[comps.size()-1] < dvec.size());
    
    auto len = comps.size();
    for (ComponentIndex i = 0; i < len; i++) mRing->set(dvec[comps[i]],svec[i]);
  }

  void denseRowCancelFromSparse(DenseCoeffVector& dense,
                                const CoeffVector& sparse,
                                const Range<int>& comps) const
  {
    // ASSUMPTION: svec[0] == 1.
    auto& dvec = * denseCoeffVector(dense);
    auto& svec = * coeffVector(sparse);

    DenseFieldElement a;
    mRing->init_set(a,dvec[comps[0]]);
    for (int i=0; i < comps.size(); ++i)
      mRing->subtract_multiple(dvec[comps[i]], a, svec[i]);
  }

  int denseRowNextNonzero(DenseCoeffVector& dense,
                          int first,
                          int last) const
  {
    auto& dvec = * denseCoeffVector(dense);
    for (int i = first; i <= last; i++)
      {
	if (mRing->is_zero(dvec[i])) continue;
	// these lines give the gist of how to handle delayed modulus
	//if (dvec[i] > mCharacteristic) dvec[i] %= mCharacteristic;
	//else if (dvec[i] < 0) dvec[i] %= mCharacteristic;
	//if (dvec[i] == 0) continue;
	return i;
      }
    return last + 1;
  }

  void denseRowToSparseRow(DenseCoeffVector& dense,
                           CoeffVector& sparse, // output value: sets this value
                           Range<int>& comps, // output value: sets comps
                           int first,
                           int last,
                           MemoryBlock& monomialSpace) const
  { 
    tbb::null_mutex noLock;
    safeDenseRowToSparseRow<tbb::null_mutex>(dense,
					     sparse,
					     comps,
					     first,
					     last,
					     monomialSpace,
					     noLock);
  }

  template<typename LockType>
  void safeDenseRowToSparseRow(DenseCoeffVector& dense,
                               CoeffVector& sparse, // output value: sets this value
                               Range<int>& comps, // output value: sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               LockType& lock) const
  {
    auto& dvec = * denseCoeffVector(dense);
    
    int len = 0;
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dense[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
      {
	if (not mRing->is_zero(dvec[i])) len++;
      }

    comps = monomialSpace.safeAllocateArray<int,LockType>(len,lock);
    sparse = allocateCoeffVector(len);
    auto& svec = * coeffVector(sparse);

    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (not mRing->is_zero(dvec[i]))
	{
	  mRing->init_set(svec[next],dvec[i]);
	  comps[next] = i;
	  ++next;
	  mRing->set_zero(dvec[i]);
	}
  }

  void sparseRowMakeMonic(CoeffVector& sparse) const
  {
    auto& svec = * coeffVector(sparse);

    FieldElement leadCoeffInv;

    mRing->init(leadCoeffInv);
    mRing->invert(leadCoeffInv,svec[0]);

    for (auto& c : svec) { mRing->mult(c, c, leadCoeffInv); }
  }
  
  template<typename Container>
  void appendSparseVectorToContainer(const CoeffVector& sparse,
                                     Container& c) const
  {
    auto& svec = * coeffVector(sparse);
    for (auto a : svec)
      {
	ring_elem tmp;
	mRing->to_ring_elem(tmp,a);
	c.push_back(tmp);
      }
  }

  template<typename Container>
  CoeffVector sparseVectorFromContainer(const Container& c) const
  {
    CoeffVector sparse = allocateCoeffVector(c.size());
    auto& svec = * coeffVector(sparse);
    for (auto i = 0; i < c.size(); ++i)
      {
	mRing->from_ring_elem(svec[i], c[i]);
      }
    return sparse;
  }

  ring_elem ringElemFromSparseVector(const CoeffVector& sparse,
                                     int index) const
  {
    auto& svec = * coeffVector(sparse);
    ring_elem tmp;
    mRing->to_ring_elem(tmp,svec[index]);
    return tmp;
  }

  std::ostream& displayVector(std::ostream& o, const CoeffVector& v) const
  {
    auto& svec = * coeffVector(v);

    bool first = true;
    o << "[(" << svec.size() << ")";
    for (const auto& a : svec)
      {
        buffer b;
        mRing->elem_text_out(b, a, true, true, true);
        if (first)
          first = false;
        else
          o << ",";
        o << b.str();
      }
    o << "]" << std::endl;
    return o;
  }
  
};

// this is the dispatching class using std::variant
// this class
class VectorArithmetic2
{
  using ConcreteVectorType = std::variant<ConcreteVectorArithmetic2<M2::ARingZZpFlint>*,
                                          ConcreteVectorArithmetic2<M2::ARingGFFlintBig>*,
                                          ConcreteVectorArithmetic2<M2::ARingGFFlint>*,
                                          ConcreteVectorArithmetic2<M2::ARingQQGMP>*,
                                          ConcreteVectorArithmetic2<M2::ARingZZpFFPACK>*,
                                          ConcreteVectorArithmetic2<M2::ARingZZp>*,
                                          ConcreteVectorArithmetic2<M2::ARingGFM2>*,
                                          ConcreteVectorArithmetic2<M2::ARingGFGivaro>*,
					  ConcreteVectorArithmetic2<M2::DummyRing>*>;

private:
  ConcreteVectorType mConcreteVector;

public:
  VectorArithmetic2(const Ring* R)
  {
    // if the rings coming in are defined in a similar way, we could avoid the switch
    // statement here as well and make it compile time.  It would make the class templated
    // on the variant class describing all the rings.
    switch (R->ringID())
      {
      case M2::ring_ZZpFlint:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingZZpFlint>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFlint>* >(R)->ring());
        break;
      case M2::ring_GFFlintBig:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingGFFlintBig>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingGFFlintBig>* >(R)->ring());
        break;
      case M2::ring_GFFlintZech:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingGFFlint>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingGFFlint>* >(R)->ring());
        break;
      case M2::ring_QQ:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingQQGMP>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingQQGMP>* >(R)->ring());
	break;
      case M2::ring_ZZpFfpack:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingZZpFFPACK>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFFPACK>* >(R)->ring());
	break;
      case M2::ring_ZZp:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingZZp>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingZZp>* >(R)->ring());
        break;
      case M2::ring_GFM2:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingGFM2>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingGFM2>* >(R)->ring());
        break;
      case M2::ring_GFGivaro:
        mConcreteVector = new ConcreteVectorArithmetic2<M2::ARingGFGivaro>
          (&dynamic_cast< const M2::ConcreteRing<M2::ARingGFGivaro>* >(R)->ring());
        break;
      default:
        // dummy ring type for default
	// throw an error here...
        std::cerr << "*** error! *** ring ID not found....!" << std::endl;
        mConcreteVector = new ConcreteVectorArithmetic2<M2::DummyRing>();
      }
  }

  // provide simple visitor interface to underlying std::variant types
  size_t size(const CoeffVector& coeffs) const {
    return std::visit([&](auto& arg) -> size_t { return arg->size(coeffs); }, mConcreteVector);
  }
  
  size_t size(const DenseCoeffVector& coeffs) const {
    return std::visit([&](auto& arg) -> size_t { return arg->size(coeffs); }, mConcreteVector);
  }

  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  /// Create a coefficient vector with room for `nelems` coefficients
  CoeffVector allocateCoeffVector(ComponentIndex nelems) const {
    return std::visit([&](auto& arg) -> CoeffVector { return arg->allocateCoeffVector(nelems);}, mConcreteVector);
  }

  DenseCoeffVector allocateDenseCoeffVector(ComponentIndex nelems) const {
    return std::visit([&](auto& arg) -> DenseCoeffVector { return arg->allocateDenseCoeffVector(nelems);}, mConcreteVector);
  }

  /// Create a coefficient vector with 0 elements.  Can be increased in size.
  // CoeffVector allocateCoeffVector() const = 0;

  /// Deallocate all the coefficients, and the array itself.
  void deallocateCoeffVector(CoeffVector& coeffs) const {
    std::visit([&](auto& arg) { delete arg->coeffVector(coeffs); }, mConcreteVector);
  }
  
  void deallocateCoeffVector(DenseCoeffVector& coeffs) const {
    std::visit([&](auto& arg) { delete arg->denseCoeffVector(coeffs); }, mConcreteVector);
  }

  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////
  void sparseRowToDenseRow(DenseCoeffVector& dense,
			   const CoeffVector& coeffs,
			   const Range<int>& comps) const {
    std::visit([&](auto& arg) { arg->sparseRowToDenseRow(dense,coeffs,comps); }, mConcreteVector);
  }

  void denseRowCancelFromSparse(DenseCoeffVector& dense,
                                const CoeffVector& coeffs,
                                const Range<int>& comps) const {
    std::visit([&](auto& arg) { arg->denseRowCancelFromSparse(dense,coeffs,comps); }, mConcreteVector);
  }

  int denseRowNextNonzero(DenseCoeffVector& dense,
                          int first,
                          int last) const {
    return std::visit([&](auto& arg) -> int { return arg->denseRowNextNonzero(dense,first,last); }, mConcreteVector);
  }

  void denseRowToSparseRow(DenseCoeffVector& dense,
                           CoeffVector& coeffs, // sets coeffs
                           Range<int>& comps, // sets comps
                           int first,
                           int last,
                           MemoryBlock& monomialSpace) const {
    std::visit([&](auto& arg) { arg->denseRowToSparseRow(dense,coeffs,comps,first,last,monomialSpace); }, mConcreteVector);  
  }

  template<typename LockType>
  void safeDenseRowToSparseRow(DenseCoeffVector& dense,
                               CoeffVector& coeffs, // sets coeffs
                               Range<int>& comps, // sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               LockType& lock) const {
    // the existence of 'template' here is a bit jarring to me, but it tells the compiler
    // that safeDenseRowToSparseRow is a template function
    std::visit([&](auto& arg) { arg->template safeDenseRowToSparseRow<LockType>(dense,coeffs,comps,first,last,monomialSpace,lock); }, mConcreteVector);      
  }
  
  void sparseRowMakeMonic(CoeffVector& coeffs) const {
    std::visit([&](auto& arg) { arg->sparseRowMakeMonic(coeffs); }, mConcreteVector);      
  }

  /////////////////////////////
  /// Translation    //////////
  /////////////////////////////

  template<class Container>
  void appendSparseVectorToContainer(const CoeffVector& coeffs,
                                     Container& c) const {
    std::visit([&](auto& arg) { arg->template appendSparseVectorToContainer<Container>(coeffs, c); }, mConcreteVector);      
  }

  template<class Container>
  CoeffVector sparseVectorFromContainer(const Container& c) const {
    return std::visit([&](auto& arg) -> CoeffVector { return arg->template sparseVectorFromContainer<Container>(c); }, mConcreteVector);
  }

  ring_elem ringElemFromSparseVector(const CoeffVector& coeffs,
                                     int index) const {
    return std::visit([&](auto& arg) -> ring_elem { return arg->ringElemFromSparseVector(coeffs,index); }, mConcreteVector);
  }

  std::ostream& displayVector(std::ostream& o, const CoeffVector& v) const
  {
    return std::visit([&](auto& arg) -> std::ostream& { return arg->displayVector(o, v); }, mConcreteVector);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
