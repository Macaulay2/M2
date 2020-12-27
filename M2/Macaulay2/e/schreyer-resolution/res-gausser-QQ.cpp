// Copyright 2005-2017 Michael E. Stillman.

#include "schreyer-resolution/res-gausser-QQ.hpp"
#include <cstdio>    // fprintf, stdout
#include <iostream>  // for operator<<, ostream, basic_ostream, endl, cout

class Ring;

ResGausserQQ::ResGausserQQ(const Ring* K1, size_t p1)
    : ResGausser(K1), Kp1(p1), mMaxDenominatorSize(1)
{
  // set RR, ring of ZZ/p too.
  mZero = FieldElement{0.0, 0, 0};
  mOne = FieldElement{1.0, 0, 0};
  mMinusOne = FieldElement{-1.0, 0, 0};
  Kp1.set_zero(mZero.mMod1);
  Kp1.set_from_long(mOne.mMod1, 1);
  Kp1.negate(mMinusOne.mMod1, mOne.mMod1);
}

//////////////////////////////////
// CoefficientVector handling ////
//////////////////////////////////
long ResGausserQQ::to_modp_long(CoefficientVector& coeffs, size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  return Kp1.coerceToLongInteger(elems[loc].mMod1);
}

void ResGausserQQ::pushBackOne(CoefficientVector& coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  elems.push_back(mOne);
}

void ResGausserQQ::pushBackMinusOne(CoefficientVector& coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  elems.push_back(mMinusOne);
}

void ResGausserQQ::pushBackElement(CoefficientVector& coeffs,
                                   const CoefficientVector& take_from_here,
                                   size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  auto& elems_to_take = coefficientVector(take_from_here);
  elems.push_back(elems_to_take[loc]);
}
void ResGausserQQ::pushBackNegatedElement(
    CoefficientVector& coeffs,
    const CoefficientVector& take_from_here,
    size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  auto& elems_to_take = coefficientVector(take_from_here);

  FieldElement a = elems_to_take[loc];
  a.mDouble = -a.mDouble;
  Kp1.negate(a.mMod1, a.mMod1);
  elems.push_back(a);
}

CoefficientVector ResGausserQQ::allocateCoefficientVector(
    ComponentIndex nelems) const
// create a row of 0's (over K).
{
  auto result = new std::vector<FieldElement>(nelems);
  for (ComponentIndex i = 0; i < nelems; i++) (*result)[i] = mZero;
  return coefficientVector(result);
}
CoefficientVector ResGausserQQ::allocateCoefficientVector() const
// create a row of 0's (over K).
{
  return coefficientVector(new std::vector<FieldElement>);
}

void ResGausserQQ::clear(CoefficientVector r,
                         ComponentIndex first,
                         ComponentIndex last) const
// set the elements in the range first..last to 0.
{
  auto& elems = coefficientVector(r);
  for (ComponentIndex i = first; i <= last; i++) elems[i] = mZero;
}

void ResGausserQQ::deallocate(CoefficientVector r) const
{
  delete reinterpret_cast<std::vector<FieldElement>*>(r.mValue);
  r.mValue = nullptr;
}

ComponentIndex ResGausserQQ::nextNonzero(CoefficientVector r,
                                         ComponentIndex first,
                                         ComponentIndex last) const
// returns last+1 in the case when there are no non-zero elements left.
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  elems += first;
  for (ComponentIndex i = first; i <= last; i++)
    if (!Kp1.is_zero((*elems++).mMod1)) return i;
  return last + 1;
}

void ResGausserQQ::fillFromSparse(CoefficientVector r,
                                  ComponentIndex len,
                                  CoefficientVector sparse,
                                  ComponentIndex* comps) const
// Fills 'r' from 'sparse' (and 'comps')
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  auto& svec = coefficientVector(sparse);
  auto sparseelems = svec.data();

  for (ComponentIndex i = 0; i < len; i++) elems[*comps++] = *sparseelems++;
}

void ResGausserQQ::sparseCancel(CoefficientVector r,
                                CoefficientVector sparse,
                                ComponentIndex* comps,
                                CoefficientVector result_loc) const
// dense += c * sparse, where c is chosen to cancel column comps[0].
// ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
// The value of c is appended to result_
{
  // r += a*sparse
  // ASSUMPTIONS:
  //   len > 0,
  //   sparse[0] = 1 or -1 (in the field)
  // where a is
  //   r[comps[0]], if sparse[0]==1
  //   -r[comps[0]], if sparse[0]==-1
  // r = [...., b, .....]
  // sparse = [1,...]  then a = -b
  // sparse = [-1,...] then a = b

  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  auto& svec = coefficientVector(sparse);
  auto sparseelems = svec.data();
  ComponentIndex len = static_cast<ComponentIndex>(svec.size());

  auto& result = coefficientVector(result_loc);

  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  FieldElement a = elems[*comps];
  if (sparseelems[0].mDouble <
      0)  // should be minus_one, since it is either 1 or -1.
    {
      a.mDouble = -a.mDouble;
      Kp1.negate(a.mMod1, a.mMod1);
    }
  for (ComponentIndex i = len; i > 0; i--)
    {
      FieldElement& sparse = *sparseelems++;
      FieldElement& result = elems[*comps++];
      result.mDouble = result.mDouble - a.mDouble * sparse.mDouble;
      Kp1.subtract_multiple(result.mMod1, a.mMod1, sparse.mMod1);
      int newsize = a.mDenominatorSize + sparse.mDenominatorSize;
      if (newsize > result.mDenominatorSize) result.mDenominatorSize = newsize;
    }
  a.mDouble = -a.mDouble;
  Kp1.negate(a.mMod1, a.mMod1);
  if (a.mDenominatorSize > mMaxDenominatorSize)
    {
      mMaxDenominatorSize = a.mDenominatorSize;
      std::cout << "coeff with den = " << a.mDenominatorSize << ": ";
      out(std::cout, a);
      std::cout << std::endl;
    }
  result.push_back(a);
}

void ResGausserQQ::sparseCancel(CoefficientVector r,
                                CoefficientVector sparse,
                                ComponentIndex* comps) const
// dense += c * sparse, where c is chosen to cancel column comps[0].
// ASSUMPTION: the lead coeff of 'sparse' is 1 or -1 (in the field)
// The value of c is not recorded.
{
  // r += a*sparse
  // ASSUMPTIONS:
  //   len > 0,
  //   sparse[0] = 1 or -1 (in the field)
  // where a is
  //   r[comps[0]], if sparse[0]==1
  //   -r[comps[0]], if sparse[0]==-1
  // r = [...., b, .....]
  // sparse = [1,...]  then a = -b
  // sparse = [-1,...] then a = b

  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  auto& svec = coefficientVector(sparse);
  auto sparseelems = svec.data();
  ComponentIndex len = static_cast<ComponentIndex>(svec.size());

  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  FieldElement a = elems[*comps];
  if (sparseelems[0].mDouble <
      0)  // should be minus_one, since it is either 1 or -1.
    {
      a.mDouble = -a.mDouble;
      Kp1.negate(a.mMod1, a.mMod1);
    }
  for (ComponentIndex i = len; i > 0; i--)
    {
      FieldElement& sparse = *sparseelems++;
      FieldElement& result = elems[*comps++];
      result.mDouble = result.mDouble - a.mDouble * sparse.mDouble;
      Kp1.subtract_multiple(result.mMod1, a.mMod1, sparse.mMod1);
      int newsize = a.mDenominatorSize + sparse.mDenominatorSize;
      if (newsize > result.mDenominatorSize) result.mDenominatorSize = newsize;
    }
}

std::ostream& ResGausserQQ::out(std::ostream& o, FieldElement& f) const
{
  o << "[" << f.mDouble << "," << Kp1.coerceToLongInteger(f.mMod1) << ","
    << f.mDenominatorSize << "]";
  return o;
}

std::ostream& ResGausserQQ::out(std::ostream& o,
                                CoefficientVector r,
                                int loc) const
{
  if (r.isNull())
    {
      o << "[vector is null!]";
      return o;
    }
  auto& elems = coefficientVector(r);
  out(o, elems[loc]);
  return o;
}
std::ostream& ResGausserQQ::debugDisplay(std::ostream& o,
                                         CoefficientVector r) const
{
  if (r.isNull()) fprintf(stdout, "vector is null!");
  auto& elems = coefficientVector(r);
  for (int j = 0; j < elems.size(); ++j)
    {
      out(o, r, j);
      o << " ";
    }
  o << std::endl;
  return o;
}

std::ostream& ResGausserQQ::debugDisplayRow(std::ostream& o,
                                            int ncolumns,
                                            const std::vector<int>& comps,
                                            CoefficientVector coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  auto monom = comps.begin();
  auto coeff = elems.begin();
  auto end = elems.end();
  for (ComponentIndex c = 0; c < ncolumns; c++)
    {
      if (coeff == end or *monom != c)
        o << " .";
      else
        {
          out(o, *coeff);
          ++coeff;
          ++monom;
        }
    }
  o << std::endl;
  return o;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
