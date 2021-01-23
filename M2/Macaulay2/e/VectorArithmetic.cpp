#include "VectorArithmetic.hpp"
#include "MemoryBlock.hpp"
#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for newarray, newarray_atomic
#include "ring.hpp"              // for Ring
#include "ringelem.hpp"          // for ring_elem

#include "aring-glue.hpp"

#include <iostream>

template<typename RingType>
const VectorArithmetic* vectorArithmetic(const RingType& R)
{
  return nullptr;
  //  return new ConcreteVectorArithmetic<RingType>(R);
}

//////////////////////////////////////////////////////////////////////////////////
// ZZ/p arithmetic ///////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////
// Notes: perhaps have two types: FieldElement is an int size of some sort.
//   Make dense array an array of larger size integers?  I.e. delay modulus?
template<typename RingType>
class ConcreteVectorArithmetic : public AbstractVectorArithmetic
{
  // This class is valid for prime characteristic up to a certain size.
  using RT = RingType;
  using FieldElement = typename RT::ElementType;
  using DenseFieldElement = FieldElement;

private:
  const RT& mRing;

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
  ConcreteVectorArithmetic(const RingType& R) : mRing(R) {}
  
  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  CoeffVector allocateCoeffVector(ComponentIndex nelems) const override
  {
    auto tempPtr = new std::vector<FieldElement>(nelems);
    for (auto& d : *tempPtr)
    {
      mRing.init(d);
      mRing.set_zero(d);
    }
    return coeffVector(tempPtr);    
  }
  DenseCoeffVector allocateDenseCoeffVector(ComponentIndex nelems) const override
  {
    auto tempPtr = new std::vector<DenseFieldElement>(nelems);
    for (auto& d : *tempPtr)
    {
      mRing.init(d);
      mRing.set_zero(d);
    }
    return denseCoeffVector(tempPtr);    
  }
  // CoeffVector allocateCoeffVector() const override
  // {
  //   return coeffVector(new std::vector<FieldElement>);
  // }
  void deallocateCoeffVector(CoeffVector& coeffs) const override
  {
    delete coeffVector(coeffs);
  }
  void deallocateCoeffVector(DenseCoeffVector& coeffs) const override
  {
    delete denseCoeffVector(coeffs);
  }
  
  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////

  size_t size(const CoeffVector& coeffs) const override
  {
    return coeffVector(coeffs)->size();
  }
  size_t size(const DenseCoeffVector& coeffs) const override
  {
    return denseCoeffVector(coeffs)->size();
  }
  
  // better name: fillDenseRow.
  void sparseRowToDenseRow(DenseCoeffVector& dense,
                           const CoeffVector& sparse,
                           const Range<int>& comps) const override
  {
    // Note: this function simply fills in the values coming from '(sparse, comps)'.
    //   Other values are not touched.
    // In our intended uses, the input `dense` is the vector consisting of all zeros`.
    
    auto& dvec = * denseCoeffVector(dense);
    auto& svec = * coeffVector(sparse);
    
    assert(comps.size() == svec.size());
    assert(comps[comps.size()-1] < dvec.size());
    
    auto len = comps.size();
    for (ComponentIndex i = 0; i < len; i++) mRing.set(dvec[comps[i]],svec[i]);
  }

  void denseRowCancelFromSparse(DenseCoeffVector& dense,
                                const CoeffVector& sparse,
                                const Range<int>& comps) const override
  {
    // ASSUMPTION: svec[0] == 1.
    auto& dvec = * denseCoeffVector(dense);
    auto& svec = * coeffVector(sparse);

    DenseFieldElement a;
    mRing.init_set(a,dvec[comps[0]]);
    for (int i=0; i < comps.size(); ++i)
      mRing.subtract_multiple(dvec[comps[i]], a, svec[i]);
  }

  int denseRowNextNonzero(DenseCoeffVector& dense,
                          int first,
                          int last) const override
  {
    auto& dvec = * denseCoeffVector(dense);
    for (int i = first; i <= last; i++)
      {
        if (mRing.is_zero(dvec[i])) continue;
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
                           MemoryBlock& monomialSpace) const override
  { 
    tbb::null_mutex noLock;
    generalDenseRowToSparseRow<tbb::null_mutex>(dense,
                                                sparse,
                                                comps,
                                                first,
                                                last,
                                                monomialSpace,
                                                noLock);
  }

  void safeDenseRowToSparseRow(DenseCoeffVector& dense,
                               CoeffVector& sparse, // output value: sets this value
                               Range<int>& comps, // output value: sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               tbb::queuing_mutex& lock) const override
  {
    generalDenseRowToSparseRow<tbb::queuing_mutex>(dense,
                                                   sparse,
                                                   comps,
                                                   first,
                                                   last,
                                                   monomialSpace,
                                                   lock);
  }

  void safeDenseRowToSparseRow(DenseCoeffVector& dense,
                               CoeffVector& sparse, // output value: sets this value
                               Range<int>& comps, // output value: sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               tbb::null_mutex& noLock) const override
  {
    generalDenseRowToSparseRow<tbb::null_mutex>(dense,
                                                sparse,
                                                comps,
                                                first,
                                                last,
                                                monomialSpace,
                                                noLock);
  }

  // at this point, LockType can be tbb::null_mutex or tbb::queuing_mutex.
  template<typename LockType>
  void generalDenseRowToSparseRow(DenseCoeffVector& dense,
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
        if (not mRing.is_zero(dvec[i])) len++;
      }

    comps = monomialSpace.safeAllocateArray<int,LockType>(len,lock);
    sparse = allocateCoeffVector(len);
    auto& svec = * coeffVector(sparse);

    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (not mRing.is_zero(dvec[i]))
        {
          mRing.init_set(svec[next],dvec[i]);
          comps[next] = i;
          ++next;
          mRing.set_zero(dvec[i]);
        }
  }

  void sparseRowMakeMonic(CoeffVector& sparse) const override
  {
    auto& svec = * coeffVector(sparse);

    FieldElement leadCoeffInv;
    mRing.init(leadCoeffInv);
    mRing.invert(leadCoeffInv,svec[0]);

    for (auto& c : svec) { mRing.mult(c, c, leadCoeffInv); }
  }

  void appendSparseVectorToContainer(const CoeffVector& sparse,
                                     VECTOR(ring_elem)& c) const override
  {
    auto& svec = * coeffVector(sparse);
    for (auto a : svec)
    {
      ring_elem tmp;
      mRing.to_ring_elem(tmp,a);
      c.push_back(tmp);
    }
  }
  
  CoeffVector sparseVectorFromContainer(const VECTOR(ring_elem)& c) const override
  {
    CoeffVector sparse = allocateCoeffVector(c.size());
    auto& svec = * coeffVector(sparse);
    for (int i = 0; i < c.size(); ++i)
      {
        mRing.init(svec[i]);
        mRing.from_ring_elem(svec[i],c[i]);
      }
    return sparse;
  }

  ring_elem ringElemFromSparseVector(const CoeffVector& sparse,
                                     int index) const override
  {
    auto& svec = * coeffVector(sparse);
    ring_elem tmp;
    mRing.to_ring_elem(tmp,svec[index]);
    return tmp;
  }
};

const VectorArithmetic* vectorArithmetic(const Ring* R)
{
  // Returns nullptr if the ring R cannot currently be used with this ring type.
  // TODO: call ERROR, or do an exception if not?

  switch (R->ringID())
    {
    case M2::ring_ZZpFlint:
      return new ConcreteVectorArithmetic<M2::ARingZZpFlint>
        (dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFlint>* >(R)->ring());
    case M2::ring_GFFlintBig:
      return new ConcreteVectorArithmetic<M2::ARingGFFlintBig>
        (dynamic_cast< const M2::ConcreteRing<M2::ARingGFFlintBig>* >(R)->ring());
    case M2::ring_GFFlintZech:
      return new ConcreteVectorArithmetic<M2::ARingGFFlint>
        (dynamic_cast< const M2::ConcreteRing<M2::ARingGFFlint>* >(R)->ring());
    case M2::ring_QQ:
      return new ConcreteVectorArithmetic<M2::ARingQQGMP>
        (dynamic_cast< const M2::ConcreteRing<M2::ARingQQGMP>* >(R)->ring());
#if 0      
    // Seldom used rings
    case M2::ring_ZZpFfpack:
      return vectorArithmetic(* dynamic_cast<const M2::ConcreteRing<M2::ARingZZpFFPACK*>>(R)->ring());
    case M2::ring_ZZp:
      return vectorArithmetic(* dynamic_cast<const M2::ConcreteRing<M2::ARingZZp*>>(R)->ring());
    case M2::ring_GFM2:
      return vectorArithmetic(* dynamic_cast<const M2::ConcreteRing<M2::ARingGFM2*>>(R)->ring());
    case M2::ring_GFGivaro:
      return vectorArithmetic(* dynamic_cast<const M2::ConcreteRing<M2::ARingGFGivaro*>>(R)->ring());
#endif
    default:
      return nullptr;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
