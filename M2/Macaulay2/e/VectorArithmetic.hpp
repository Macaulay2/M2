#ifndef __vector_arithmetic_hpp__
#define __vector_arithmetic_hpp__

#include "m2tbb.hpp"
#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for VECTOR
#include "MemoryBlock.hpp"       // for MemoryBlock
#include "ringelem.hpp"
#include "aring-glue.hpp"
#include <variant>
#include <type_traits>
#include "f4/f4-mem.hpp"         // for F4Mem
#include "ARingElem.hpp"

class Ring;
union ring_elem;
using ComponentIndex = int;

// this is still deriving from our_new_delete, since this
// class doesn't know whether mValue points to a std::vector or a VECTOR.
// this issue persists in the Row struct in NCF4 as well.
class ElementArray
{
  template<typename RingType, typename ElementArrayType> friend class ConcreteVectorArithmetic;

public:
  ElementArray() : mValue(nullptr) {}
  bool  isNull() const { return mValue == nullptr; }
  void  swap(ElementArray& b) { std::swap(mValue, b.mValue); }

private:
  // disallow copy...
  ElementArray(void *p) : mValue(p) {} // transfers ownership of this pointer to this object

  //ElementArray(ElementArray &b) { mValue = nullptr; std::swap(mValue,b.mValue); }
  //ElementArray(const ElementArray && b) : mValue(b.mValue) { b.mValue = nullptr; } 
  //ElementArray(ElementArray &b) = delete;
  void* getValue() const { return mValue; }
  void  setValue(void* newValue) { mValue = newValue; }

  void* mValue;
};

template<typename RingType,
         typename ElementArrayType = ElementArray>
class ConcreteVectorArithmetic
{
public:
  using FieldElement = typename RingType::ElementType;

  using ElementArrayContainer = typename RingType::ElementContainerType;
  // we may want to change the container type of dense vectors in the future.

  friend class VectorArithmetic;

  ConcreteVectorArithmetic() : mOriginalRing(nullptr), mRing(nullptr) {}

 private:
  const Ring* mOriginalRing;
  const RingType* mRing;

  ElementArrayContainer* elementArray(const ElementArrayType& f) const
  {
    return reinterpret_cast<ElementArrayContainer*>(f.getValue());
  }

public:
  ConcreteVectorArithmetic(const Ring* origR, const RingType* R) : mOriginalRing(origR), mRing(R) {}

  const Ring* ring() const { return mOriginalRing; }
  
  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  ElementArrayType copyElementArray(const ElementArray& sparse) const
  {
    auto& v = * elementArray(sparse);
    auto tempPtr = new ElementArrayContainer(v.size());

    int n = 0;
    for (auto& d : *tempPtr)
      {
	mRing->init_set(d, v[n]);
        ++n;
      }
    return ElementArrayType {tempPtr};    
  }

  ElementArrayType allocateElementArray(ComponentIndex nelems) const
  {
    auto tempPtr = new ElementArrayContainer(nelems);
    for (auto& d : *tempPtr)
      {
	mRing->init(d);
	mRing->set_zero(d);
      }
    return ElementArrayType {tempPtr};    
  }

  void deallocateElementArray(ElementArrayType& coeffs) const
  {
    auto& svec = * elementArray(coeffs);
    for (auto& e : svec) {
      mRing->clear(e);
    }
    delete &svec;
    coeffs.setValue(nullptr);
  }

  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////

  size_t size(const ElementArrayType& coeffs) const
  {
    return elementArray(coeffs)->size();
  }

  void fillDenseArray(ElementArrayType& dense,
                      const ElementArrayType& sparse,
                      const Range<int>& comps) const
  {
    // Note: this function simply fills in the values coming from '(sparse, comps)'.
    //   Other values are not touched.
    // In our intended uses, the input `dense` is the vector consisting of all zeros.
    
    auto& dvec = * elementArray(dense);
    auto& svec = * elementArray(sparse);
    
    assert(comps.size() == svec.size());
    assert(comps[comps.size()-1] < dvec.size());
    
    auto len = comps.size();
    for (ComponentIndex i = 0; i < len; i++) mRing->set(dvec[comps[i]],svec[i]);
  }

  void denseCancelFromSparse(ElementArrayType& dense,
                             const ElementArrayType& sparse,
                             const Range<int>& comps) const
  {
    // ASSUMPTION: svec[0] == 1.
    auto& dvec = * elementArray(dense);
    auto& svec = * elementArray(sparse);

    ARingElem a(mRing,dvec[comps[0]]);
    for (int i=0; i < comps.size(); ++i)
      mRing->subtract_multiple(dvec[comps[i]], *a, svec[i]);
  }

  int denseNextNonzero(ElementArrayType& dense,
                       int first,
                       int last) const
  {
    auto& dvec = * elementArray(dense);
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

  void denseToSparse(ElementArrayType& dense,
                     ElementArrayType& sparse, // output value: sets this value
                     Range<int>& comps, // output value: sets comps
                     int first,
                     int last,
                     MemoryBlock& monomialSpace) const
  { 
    mtbb::null_mutex noLock;
    safeDenseToSparse<mtbb::null_mutex>(dense,
					     sparse,
					     comps,
					     first,
					     last,
					     monomialSpace,
					     noLock);
  }

  template<typename LockType>
  void safeDenseToSparse(ElementArrayType& dense,
                               ElementArrayType& sparse, // output value: sets this value
                               Range<int>& comps, // output value: sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               LockType& lock) const
  {
    auto& dvec = * elementArray(dense);
    
    int len = 0;
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dense[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
	if (not mRing->is_zero(dvec[i])) len++;

    comps = monomialSpace.safeAllocateArray<int,LockType>(len,lock);
    deallocateElementArray(sparse);
    sparse = allocateElementArray(len);
    auto& svec = * elementArray(sparse);

    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (not mRing->is_zero(dvec[i]))
	{
	  mRing->set(svec[next],dvec[i]);
	  comps[next] = i;
	  ++next;
	  mRing->set_zero(dvec[i]);
	}
  }

  void denseToSparse(ElementArrayType& dense,
                           ElementArrayType& sparse, // output value: sets this value
                           int*& comps,
                           int first,
                           int last,
                           F4Vec& f4Vec) const
  {
    auto& dvec = * elementArray(dense);
    
    int len = 0;
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dense[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
	if (not mRing->is_zero(dvec[i])) len++;

    comps = f4Vec.allocate(len);

    sparse = allocateElementArray(len);
    auto& svec = * elementArray(sparse);

    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (not mRing->is_zero(dvec[i]))
	{
	  mRing->set(svec[next],dvec[i]);
	  comps[next] = i;
	  ++next;
	  mRing->set_zero(dvec[i]);
	}
  }

  void setZeroInRange(ElementArrayType& dense,
                      int first,
                      int last) const
  {
    auto& dvec = * elementArray(dense);
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dvec[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
      mRing->set_zero(dvec[i]);
  }
  
  void makeMonic(ElementArrayType& sparse) const
  {
    auto& svec = * elementArray(sparse);

    ARingElem leadCoeffInv(mRing);

    mRing->invert(*leadCoeffInv,svec[0]);

    for (auto& c : svec) { mRing->mult(c, c, *leadCoeffInv); }
  }

  template<typename Container>
  void appendToContainer(const ElementArrayType& sparse,
                         Container& c) const
  {
    auto& svec = * elementArray(sparse);
    for (const auto& a : svec)
      {
	ring_elem tmp;
	mRing->to_ring_elem(tmp,a);
	c.push_back(tmp);
      }
  }

  template<typename Container>
  ElementArrayType elementArrayFromContainer(const Container& c) const
  {
    ElementArrayType sparse = allocateElementArray(c.size());
    auto& svec = * elementArray(sparse);
    for (auto i = 0; i < c.size(); ++i)
    {
      mRing->init(svec[i]);
      mRing->from_ring_elem(svec[i], c[i]);
    }
    return sparse;
  }

  ring_elem ringElemFromElementArray(const ElementArrayType& sparse,
                                     int index) const
  {
    auto& svec = * elementArray(sparse);
    ring_elem tmp;
    mRing->to_ring_elem(tmp,svec[index]);
    return tmp;
  }

  std::ostream& displayElementArray(std::ostream& o, const ElementArrayType& v) const
  {
    auto& svec = * elementArray(v);

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
  
  ////////////////////////
  /// Append support /////
  ////////////////////////
  
  void pushBackOne(ElementArrayType& coeffs) const
  {
    auto& svec = * elementArray(coeffs);
    FieldElement one;
    mRing->init(one);
    mRing->set_from_long(one, 1);
    svec.emplace_back(one); // This grabs 'one' in cases where it is allocated...  I think...!
  }

  void pushBackMinusOne(ElementArrayType& coeffs) const
  {
    auto& svec = * elementArray(coeffs);
    FieldElement minus_one;
    mRing->init(minus_one);
    mRing->set_from_long(minus_one,-1);
    svec.emplace_back(minus_one);
  }

  void pushBackElement(ElementArrayType& coeffs,
                       const ElementArrayType& take_from_here,
                       size_t loc) const
  {
    auto& svec = * elementArray(coeffs);
    auto& svec2 = * elementArray(take_from_here);
    assert(loc < svec2.size());
    FieldElement a;
    mRing->init_set(a, svec2[loc]);
    svec.emplace_back(a);
  }
  
  void pushBackNegatedElement(ElementArrayType& coeffs,
                              const ElementArrayType& take_from_here,
                              size_t loc) const
  {
    auto& svec = * elementArray(coeffs);
    auto& svec2 = * elementArray(take_from_here);
    assert(loc < svec2.size());
    FieldElement a;
    mRing->init_set(a, svec2[loc]);
    mRing->negate(a, a);
    svec.emplace_back(a);
  }
};

// `overloaded` construct (not standard until C++20)
template<class... Ts> 
struct overloaded : Ts... { using Ts::operator()...; };

template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

// this is the dispatching class using std::variant
class VectorArithmetic
{
  using ElementArrayType = ElementArray;

  // does the order of the variant types matter?
  using CVA_Type = std::variant<ConcreteVectorArithmetic<M2::ARingZZpFlint>*,
                                ConcreteVectorArithmetic<M2::ARingGFFlintBig>*,
                                ConcreteVectorArithmetic<M2::ARingGFFlint>*,
                                ConcreteVectorArithmetic<M2::ARingQQGMP>*,
                                ConcreteVectorArithmetic<M2::ARingZZpFFPACK>*,
                                ConcreteVectorArithmetic<M2::ARingZZp>*,
                                ConcreteVectorArithmetic<M2::ARingGFM2>*,
                                ConcreteVectorArithmetic<M2::ARingGFGivaro>*,
                                ConcreteVectorArithmetic<CoefficientRingR>*,
                                ConcreteVectorArithmetic<CoefficientRingZZp>*,
                                ConcreteVectorArithmetic<M2::DummyRing>*>;

private:
  CVA_Type mConcreteVector;

public:
  VectorArithmetic(const Ring* R)
  {
    // if the rings coming in are defined in a similar way, we could avoid the switch
    // statement here as well and make it compile time.  It would make the class templated
    // on the variant class describing all the rings.
    switch (R->ringID())
      {
      case M2::ring_ZZpFlint:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFlint>* >(R)->ring()};
        break;
      case M2::ring_GFFlintBig:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingGFFlintBig>* >(R)->ring()};
        break;
      case M2::ring_GFFlintZech:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingGFFlint>* >(R)->ring()};
        break;
      case M2::ring_QQ:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingQQGMP>* >(R)->ring()};
	break;
      case M2::ring_ZZpFfpack:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFFPACK>* >(R)->ring()};
	break;
      case M2::ring_ZZp:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingZZp>* >(R)->ring()};
        break;
      case M2::ring_GFM2:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingGFM2>* >(R)->ring()};
        break;
      case M2::ring_GFGivaro:
        mConcreteVector = new ConcreteVectorArithmetic
          {R, &dynamic_cast< const M2::ConcreteRing<M2::ARingGFGivaro>* >(R)->ring()};
        break;
      case M2::ring_old:
        if (R->cast_to_Z_mod() != nullptr)
          mConcreteVector = new ConcreteVectorArithmetic{R, R->cast_to_Z_mod()->get_CoeffRing()};
        else
          {
            std::cout << "Using GC ring in VectorArithmetic." << std::endl;
            mConcreteVector = new ConcreteVectorArithmetic{R, R->getCoefficientRingR()};
          }
        break;
      default:
        // dummy ring type for default
	// throw an error here...
        std::cerr << "*** error! *** ring ID not found....!" << std::endl;
        mConcreteVector = new ConcreteVectorArithmetic<M2::DummyRing>();
      }
  }

  ~VectorArithmetic() { 
    std::visit([&](auto& arg) -> void { delete arg; }, mConcreteVector);
  }

  const Ring* ring() const {
    return std::visit([&](auto& arg) -> const Ring* { return arg->ring(); }, mConcreteVector);
  }

  // provide simple visitor interface to underlying std::variant types
  size_t size(const ElementArrayType& coeffs) const {
    return std::visit([&](auto& arg) -> size_t { return arg->size(coeffs); }, mConcreteVector);
  }
  
  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  /// Create a coefficient vector with room for `nelems` coefficients
  ElementArrayType allocateElementArray(ComponentIndex nelems) const {
    return std::visit([&](auto& arg) -> ElementArrayType { return arg->allocateElementArray(nelems);}, mConcreteVector);
  }

  ElementArrayType copyElementArray(const ElementArray& sparse) const {
    return std::visit([&](auto& arg) -> ElementArrayType { return arg->copyElementArray(sparse);}, mConcreteVector);
  }

  /// Create a coefficient vector with 0 elements.  Can be increased in size.
  // ElementArrayType allocateElementArray() const = 0;

  /// Deallocate all the coefficients, and the array itself.
  void deallocateElementArray(ElementArrayType& coeffs) const {
    std::visit([&](auto& arg) { arg->deallocateElementArray(coeffs); }, mConcreteVector);
  }
  
  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////
  void fillDenseArray(ElementArrayType& dense,
			   const ElementArrayType& coeffs,
			   const Range<int>& comps) const {
    std::visit([&](auto& arg) { arg->fillDenseArray(dense,coeffs,comps); }, mConcreteVector);
  }

  void denseCancelFromSparse(ElementArrayType& dense,
                                const ElementArrayType& coeffs,
                                const Range<int>& comps) const {
    std::visit([&](auto& arg) { arg->denseCancelFromSparse(dense,coeffs,comps); }, mConcreteVector);
  }

  int denseNextNonzero(ElementArrayType& dense,
                          int first,
                          int last) const {
    return std::visit([&](auto& arg) -> int { return arg->denseNextNonzero(dense,first,last); }, mConcreteVector);
  }

  void denseToSparse(ElementArrayType& dense,
                           ElementArrayType& coeffs, // sets coeffs
                           Range<int>& comps, // sets comps
                           int first,
                           int last,
                           MemoryBlock& monomialSpace) const {
    std::visit([&](auto& arg) { arg->denseToSparse(dense,coeffs,comps,first,last,monomialSpace); }, mConcreteVector);  
  }

  void denseToSparse(ElementArrayType& dense,
                           ElementArrayType& coeffs, // sets coeffs
                           int*& comps, // sets comps
                           int first,
                           int last,
                           F4Vec& f4Vec) const {
    std::visit([&](auto& arg) { arg->denseToSparse(dense,coeffs,comps,first,last,f4Vec); }, mConcreteVector);  
  }

  template<typename LockType>
  void safeDenseToSparse(ElementArrayType& dense,
                               ElementArrayType& coeffs, // sets coeffs
                               Range<int>& comps, // sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               LockType& lock) const {
    // the existence of 'template' here is a bit jarring to me, but it tells the compiler
    // that safeDenseToSparse is a template function
    std::visit([&](auto& arg) { arg->template safeDenseToSparse<LockType>(dense,coeffs,comps,first,last,monomialSpace,lock); }, mConcreteVector);      
  }

  void setZeroInRange(ElementArrayType& dense, int first, int last) const {
    std::visit([&](auto& arg) { arg->setZeroInRange(dense, first, last); }, mConcreteVector);      
  }
  
  void makeMonic(ElementArrayType& coeffs) const {
    std::visit([&](auto& arg) { arg->makeMonic(coeffs); }, mConcreteVector);      
  }

  /////////////////////////////
  /// Translation    //////////
  /////////////////////////////

  template<class Container>
  void appendToContainer(const ElementArrayType& coeffs,
                                     Container& c) const {
    std::visit([&](auto& arg) { arg->template appendToContainer<Container>(coeffs, c); }, mConcreteVector);      
  }

  template<class Container>
  ElementArrayType elementArrayFromContainer(const Container& c) const {
    return std::visit([&](auto& arg) -> ElementArrayType { return arg->template elementArrayFromContainer<Container>(c); }, mConcreteVector);
  }

  ring_elem ringElemFromElementArray(const ElementArrayType& coeffs,
                                     int index) const {
    return std::visit([&](auto& arg) -> ring_elem { return arg->ringElemFromElementArray(coeffs,index); }, mConcreteVector);
  }

  std::ostream& displayElementArray(std::ostream& o, const ElementArrayType& v) const
  {
    return std::visit([&](auto& arg) -> std::ostream& { return arg->displayElementArray(o, v); }, mConcreteVector);
  }

  //////////////////////////
  /// Append support   /////
  //////////////////////////
  
  void pushBackOne(ElementArrayType& coeffs) const
  {
    return std::visit([&](auto& arg) { arg->pushBackOne(coeffs); }, mConcreteVector);
  }

  void pushBackMinusOne(ElementArrayType& coeffs) const
  {
    return std::visit([&](auto& arg) { arg->pushBackMinusOne(coeffs); }, mConcreteVector);
  }

  void pushBackElement(ElementArrayType& coeffs,
                       const ElementArrayType& take_from_here,
                       size_t loc) const
  {
    return std::visit([&](auto& arg) { arg->pushBackElement(coeffs,take_from_here,loc); }, mConcreteVector);
  }
  
  void pushBackNegatedElement(ElementArrayType& coeffs,
                              const ElementArrayType& take_from_here,
                              size_t loc) const
  {
    return std::visit([&](auto& arg) { arg->pushBackNegatedElement(coeffs,take_from_here,loc); }, mConcreteVector);
  }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
