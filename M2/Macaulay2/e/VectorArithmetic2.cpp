#include "VectorArithmetic2.hpp"
#include "NCAlgebras/MemoryBlock.hpp"
#include "NCAlgebras/Range.hpp"  // for Range
#include "newdelete.hpp"         // for newarray, newarray_atomic
#include "ring.hpp"              // for Ring
#include "ringelem.hpp"          // for ring_elem

#include "aring-glue.hpp"

#include <iostream>
#include <execution>

const VectorArithmetic2* vectorArithmetic2(const M2::ARingZZp&);
const VectorArithmetic2* vectorArithmetic2(const M2::ARingZZpFlint&);
const VectorArithmetic2* vectorArithmetic2(const M2::ARingGFM2&);
const VectorArithmetic2* vectorArithmetic2(const M2::ARingGFFlint&);
const VectorArithmetic2* vectorArithmetic2(const M2::ARingGFFlintBig&);

template<typename RingType>
const VectorArithmetic2* vectorArithmetic2(const RingType& R)
{
  return nullptr;
  //  return new ConcreteVectorArithmetic2<RingType>(R);
}

const VectorArithmetic2* vectorArithmetic2(const Ring* R)
{
  // Returns nullptr if the ring R cannot currently be used with this ring type.
  // TODO: call ERROR, or do an exception if not?

  switch (R->ringID())
    {
    case M2::ring_ZZpFlint:
      return vectorArithmetic2(dynamic_cast< const M2::ConcreteRing<M2::ARingZZpFlint>* >(R)->ring());
#if 0      
    case M2::ring_GFFlintBig:
      return vectorArithmetic2(* dynamic_cast<const M2::ConcreteRing<M2::ARingGFFlintBig*>>(R)->ring());
    case M2::ring_GFFlintZech:
      return vectorArithmetic2(* dynamic_cast<const M2::ConcreteRing<M2::ARingGFFlint*>>(R)->ring());

    // Seldom used rings
    case M2::ring_ZZpFfpack:
      return vectorArithmetic2(* dynamic_cast<const M2::ConcreteRing<M2::ARingZZpFFPACK*>>(R)->ring());
    case M2::ring_ZZp:
      return vectorArithmetic2(* dynamic_cast<const M2::ConcreteRing<M2::ARingZZp*>>(R)->ring());
    case M2::ring_GFM2:
      return vectorArithmetic2(* dynamic_cast<const M2::ConcreteRing<M2::ARingGFM2*>>(R)->ring());
    case M2::ring_GFGivaro:
      return vectorArithmetic2(* dynamic_cast<const M2::ConcreteRing<M2::ARingGFGivaro*>>(R)->ring());
#endif
    default:
      return nullptr;
    }
}

//////////////////////////////////////////////////////////////////////////////////
// ZZ/p arithmetic ///////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////
// Notes: perhaps have two types: FieldElement is an int size of some sort.
//   Make dense array an array of larger size integers?  I.e. delay modulus?
class VectorArithmeticZZpFlint : public VectorArithmetic2
{
  // This class is valid for prime characteristic up to a certain size.
  using FieldElement = M2::ARingZZpFlint::ElementType;
  using DenseFieldElement = FieldElement;
private:
  const M2::ARingZZpFlint& mRing;

  std::vector<FieldElement>* coefficientVector(CoefficientVector2 f) const
  {
    return reinterpret_cast<std::vector<FieldElement>*>(f.mValue);
  }

  std::vector<DenseFieldElement>* denseCoefficientVector(DenseCoefficientVector2 f) const
  {
    return reinterpret_cast<std::vector<DenseFieldElement>*>(f.mValue);
  }
  
  CoefficientVector2 coefficientVector(std::vector<FieldElement>* vals) const
  {
    CoefficientVector2 result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

  DenseCoefficientVector2 denseCoefficientVector(std::vector<DenseFieldElement>* vals) const
  {
    DenseCoefficientVector2 result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

public:
  VectorArithmeticZZpFlint(const M2::ARingZZpFlint& R) : mRing(R) {}
  
  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  CoefficientVector2 allocateCoefficientVector(ComponentIndex nelems) const override
  {
    return coefficientVector(new std::vector<FieldElement>(nelems));
  }
  DenseCoefficientVector2 allocateDenseCoefficientVector(ComponentIndex nelems) const override
  {
    // note that here we are using that 0 represents the 0 element in ZZpFlint
    // if that is not the case in another class, need to change
    auto tempPtr = new std::vector<DenseFieldElement>(nelems,0);
    return denseCoefficientVector(tempPtr);    
  }
  // CoefficientVector2 allocateCoefficientVector() const override
  // {
  //   return coefficientVector(new std::vector<FieldElement>);
  // }
  void deallocateCoefficientVector(CoefficientVector2& coeffs) const override
  {
    delete coefficientVector(coeffs);
  }
  void deallocateCoefficientVector(DenseCoefficientVector2& coeffs) const override
  {
    delete denseCoefficientVector(coeffs);
  }

  
  ////////////////////////
  /// Linear Algebra /////
  ////////////////////////

  size_t size(const CoefficientVector2& coeffs) const override
  {
    return coefficientVector(coeffs)->size();
  }
  size_t size(const DenseCoefficientVector2& coeffs) const override
  {
    return denseCoefficientVector(coeffs)->size();
  }
  
  // better name: fillDenseRow.
  void sparseRowToDenseRow(DenseCoefficientVector2& dense,
                           const CoefficientVector2& sparse,
                           const Range<int>& comps) const override
  {
    // Note: this function simply fills in the values coming from '(sparse, comps)'.
    //   Other values are not touched.
    // In our intended uses, the input `dense` is the vector consisting of all zeros`.
    
    auto& dvec = * denseCoefficientVector(dense);
    auto& svec = * coefficientVector(sparse);
    
    assert(comps.size() == svec.size());
    assert(comps[comps.size()-1] < dense.size());
    
    auto len = comps.size();
    for (ComponentIndex i = 0; i < len; i++) dvec[comps[i]] = svec[i];
  }

  void denseRowCancelFromSparse(DenseCoefficientVector2& dense,
                                const CoefficientVector2& sparse,
                                const Range<int>& comps) const override
  {
    // ASSUMPTION: svec[0] == 1.
    auto& dvec = * denseCoefficientVector(dense);
    auto& svec = * coefficientVector(sparse);

    DenseFieldElement a = dvec[comps[0]];
    for (int i=0; i < comps.size(); ++i)
      mRing.subtract_multiple(dvec[comps[i]], a, svec[i]);
  }

  int denseRowNextNonzero(DenseCoefficientVector2& dense,
                          int first,
                          int last) const override
  {
    auto& dvec = * denseCoefficientVector(dense);
    for (int i = first; i <= last; i++)
      {
        if (dvec[i] == 0) continue;
        // these lines give the gist of how to handle delayed modulus
        //if (dvec[i] > mCharacteristic) dvec[i] %= mCharacteristic;
        //else if (dvec[i] < 0) dvec[i] %= mCharacteristic;
        //if (dvec[i] == 0) continue;
        return i;
      }
    return last + 1;
  }

  void denseRowToSparseRow(DenseCoefficientVector2& dense,
                           CoefficientVector2& sparse, // output value: sets this value
                           Range<int>& comps, // output value: sets comps
                           int first,
                           int last,
                           MemoryBlock& monomialSpace) const override
  {
    auto& dvec = * denseCoefficientVector(dense);
    
    int len = 0;
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dense[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
      {
        if (dvec[i] != 0) len++;
      }

    comps = monomialSpace.allocateArray<int>(len);
    sparse = allocateCoefficientVector(len);
    auto& svec = * coefficientVector(sparse);

    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (dvec[i] != 0)
        {
          svec[next] = dvec[i];
          comps[next] = i;
          ++next;
          dvec[i] = 0;
        }
  }

  void safeDenseRowToSparseRow(DenseCoefficientVector2& dense,
                               CoefficientVector2& sparse, // output value: sets this value
                               Range<int>& comps, // output value: sets comps
                               int first,
                               int last,
                               MemoryBlock& monomialSpace,
                               tbb::queuing_mutex& lock) const override
  {
    auto& dvec = * denseCoefficientVector(dense);
    
    int len = 0;
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dense[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
      {
        if (dvec[i] != 0) len++;
      }

    comps = monomialSpace.safeAllocateArray<int>(len,lock);
    sparse = allocateCoefficientVector(len);
    auto& svec = * coefficientVector(sparse);

    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (dvec[i] != 0)
        {
          svec[next] = dvec[i];
          comps[next] = i;
          ++next;
          dvec[i] = 0;
        }
  }

  void sparseRowMakeMonic(CoefficientVector2& sparse) const override
  {
    auto& svec = * coefficientVector(sparse);

    FieldElement leadCoeffInv;
    mRing.init(leadCoeffInv);
    mRing.invert(leadCoeffInv,svec[0]);

    for (auto& c : svec) { mRing.mult(c, c, leadCoeffInv); }
  }

  void appendSparseVectorToContainer(const CoefficientVector2& sparse,
                                     VECTOR(ring_elem)& c) const override
  {
    auto& svec = * coefficientVector(sparse);
    for (auto a : svec)
    {
      ring_elem tmp;
      mRing.to_ring_elem(tmp,a);
      c.push_back(tmp);
    }
  }
  
  CoefficientVector2 sparseVectorFromContainer(const VECTOR(ring_elem)& c) const override
  {
    CoefficientVector2 sparse = allocateCoefficientVector(c.size());
    auto& svec = * coefficientVector(sparse);
    for (int i = 0; i < c.size(); ++i)
      {
        mRing.init(svec[i]);
        mRing.from_ring_elem(svec[i],c[i]);
      }
    return sparse;
  }

  ring_elem ringElemFromSparseVector(const CoefficientVector2& sparse,
                                     int index) const override
  {
    auto& svec = * coefficientVector(sparse);
    ring_elem tmp;
    mRing.to_ring_elem(tmp,svec[index]);
    return tmp;
  }
};

// const VectorArithmetic2* vectorArithmetic2(const M2::ARingZZp& R)
// {
//   new VectorArithmeticZZp(R, R.characteristic());
// }
const VectorArithmetic2* vectorArithmetic2(const M2::ARingZZpFlint& R)
{
  return new VectorArithmeticZZpFlint(R);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
