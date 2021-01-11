#include "VectorArithmetic2.hpp"
#include "MemoryBlock2.hpp"
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
class VectorArithmeticZZp : public VectorArithmetic2
{
  // This class is valid for prime characteristic up to a certain size.
  using FieldElement = int;
  using DenseFieldElement = long;
private:
  const M2::ARingZZp& mRing;
  FieldElement mCharacteristic;

  std::vector<FieldElement>* coefficientVector(CoefficientVector2 f) const
  {
    return reinterpret_cast<std::vector<FieldElement>*>(f.mValue);
  }

  std::vector<DenseFieldElement>* coefficientVector(DenseCoefficientVector2 f) const
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

  DenseCoefficientVector2 coefficientVector(std::vector<DenseFieldElement>* vals) const
  {
    DenseCoefficientVector2 result;
    result.mValue = vals;
    vals = nullptr;
    return result;
  }

public:
  VectorArithmeticZZp(const M2::ARingZZp& R, int characteristic) : mRing(R), mCharacteristic(characteristic) {}
  
  /////////////////////////////
  // Allocation/Deallocation //
  /////////////////////////////
  CoefficientVector2 allocateCoefficientVector(ComponentIndex nelems) const override
  {
    return coefficientVector(new std::vector<FieldElement>(nelems));
  }
  DenseCoefficientVector2 allocateDenseCoefficientVector(ComponentIndex nelems) const override
  {
    return coefficientVector(new std::vector<DenseFieldElement>(nelems));
  }
  CoefficientVector2 allocateCoefficientVector() const override
  {
    return coefficientVector(new std::vector<FieldElement>);
  }
  void deallocateCoefficientVector(CoefficientVector2& coeffs) const override
  {
    delete coefficientVector(coeffs);
  }
  void deallocateCoefficientVector(DenseCoefficientVector2& coeffs) const override
  {
    delete coefficientVector(coeffs);
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
    return coefficientVector(coeffs)->size();
  }
  
  // better name: fillDenseRow.
  void sparseRowToDenseRow(DenseCoefficientVector2& dense,
                           const CoefficientVector2& sparse,
                           const Range<int>& comps) const override
  {
    // Note: this function simply fills in the values coming from '(sparse, comps)'.
    //   Other values are not touched.
    // In our intended uses, the input `dense` is the vector consisting of all zeros`.
    
    auto& dvec = * coefficientVector(dense);
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
    auto& dvec = * coefficientVector(dense);
    auto& svec = * coefficientVector(sparse);

    DenseFieldElement a = dvec[comps[0]];
    for (int i=0; i < comps.size(); ++i)
      dvec[comps[i]] += mCharacteristic - (a * svec[i]);
  }

  int denseRowNextNonzero(DenseCoefficientVector2& dense,
                          int first,
                          int last) const override
  {
    auto& dvec = * coefficientVector(dense);
    for (int i = first; i <= last; i++)
      {
        if (dvec[i] == 0) continue;
        if (dvec[i] > mCharacteristic) dvec[i] %= mCharacteristic;
        else if (dvec[i] < 0) dvec[i] %= mCharacteristic;
        if (dvec[i] == 0) continue;
        return i;
      }
    return last + 1;
  }

  void denseRowToSparseRow(DenseCoefficientVector2& dense,
                           CoefficientVector2& sparse, // output value: sets this value
                           Range<int>& comps, // output value: sets comps
                           MemoryBlock2& monomialSpace,
                           int first,
                           int last) const override // TODO: have a MemoryBlock2 entry for where to put comps (and perhaps coeffs?)
  {
    auto& dvec = * coefficientVector(dense);
    
    int len = 0;
    
    // first can be -1 if the row is zero.  in this case, we should
    // not be accessing dense[i] for i negative.
    for (int i = first; i >= 0 and i <= last; i++)
      {
        if (dvec[i] == 0) len++;
      }

    // Redo these 2 lines
    //    ring_elem* ptr = newarray(ring_elem, len);
    //    coeffs = Range<ring_elem>(ptr, ptr + len);

    // Fix this line
    comps = monomialSpace.allocateArray<int>(len);
    
    int next = 0;
    for (int i = first; i >= 0 and i <= last; i++)
      if (dvec[i] != 0)
        {
          //TODO          svec[next] = dvec[i];
          comps[next] = i;
          ++next;
          dvec[i] = 0;
        }
  }

  void sparseRowMakeMonic(CoefficientVector2& sparse) const override
  {
    auto& svec = * coefficientVector(sparse);

    // TODO: get the inverse of coeffs[0].
    // FieldElement leadCoeff = coeffs[0];

    // TODO
    //    for (auto& c : svec) { c = mRing.divide(c, leadCoeff); }
  }

};

const VectorArithmetic2* vectorArithmetic2(const M2::ARingZZp& R)
{
  new VectorArithmeticZZp(R, R.characteristic());
}
// const VectorArithmetic2* vectorArithmetic2(const M2::ARingZZpFlint& R)
// {
//   new VectorArithmeticZZp(R, R.mCharacteristic());
// }

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
