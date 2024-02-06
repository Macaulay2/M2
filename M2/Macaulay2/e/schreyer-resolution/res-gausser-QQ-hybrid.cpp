// Copyright 2005-2017 Michael E. Stillman.

#include "schreyer-resolution/res-gausser-QQ-hybrid.hpp"
#include <cstdio>            // fprintf, stdout
#include <iostream>          // for operator<<, basic_ostream, endl, cout
#include <type_traits>       // for move
#include "buffer.hpp"        // for buffer
#include "engine-exports.h"  // for M2_gbTrace

long ResGausser::numAdditions = 0;

ResGausserQQHybrid::ResGausserQQHybrid(const Ring* K1,
                                       unsigned long precision1,
                                       size_t p1,
                                       size_t p2)
    : ResGausser(K1),
      Kp1(p1),
      Kp2(p2),
      mRRing(precision1),
      mMaxDenominatorSize(1)
{
  numAdditions = 0;

  init_element(mZero);
  from_long_element(mZero, 0);

  init_element(mOne);
  from_long_element(mOne, 1);

  init_element(mMinusOne);
  from_long_element(mMinusOne, -1);
}

void ResGausserQQHybrid::init_element(FieldElement& a) const
{
  a.mDouble = 0.0;
  Kp1.set_from_long(a.mMod1, 0);
  Kp2.set_from_long(a.mMod2, 0);
  mRRing.init(a.mLongDouble);
  mRRing.set_from_long(a.mLongDouble, 0);
  a.mDenominatorSize = 0;
  a.mAccuracy = 53;
  a.mIsPresent = false;
}

void ResGausserQQHybrid::clear_element(FieldElement& a) const
{
  mRRing.clear(a.mLongDouble);
}

void ResGausserQQHybrid::from_long_element(FieldElement& a, long val) const
{
  a.mDouble = val;
  Kp1.set_from_long(a.mMod1, val);
  Kp2.set_from_long(a.mMod2, val);
  mRRing.set_from_long(a.mLongDouble, val);
  a.mDenominatorSize = 0;
  a.mAccuracy = 1;  // what should this be??
  if (val != 0)
    a.mIsPresent = true;
  else
    a.mIsPresent = false;
}

void ResGausserQQHybrid::from_mpq_element(FieldElement& a,
                                          mpq_t val,
                                          int denomPower) const
{
  a.mDouble = mpq_get_d(val);
  Kp1.set_from_mpq(a.mMod1, val);
  Kp2.set_from_mpq(a.mMod2, val);
  mRRing.set_from_mpq(a.mLongDouble, val);
  a.mDenominatorSize = denomPower;
  a.mAccuracy = 53;
  a.mIsPresent = true;
}

void ResGausserQQHybrid::copy_element(FieldElement& result,
                                      const FieldElement& a) const
{
  result.mDouble = a.mDouble;
  result.mMod1 = a.mMod1;
  result.mMod2 = a.mMod2;
  mRRing.set(result.mLongDouble, a.mLongDouble);
  result.mDenominatorSize = a.mDenominatorSize;
  result.mAccuracy = a.mAccuracy;
  result.mIsPresent = a.mIsPresent;
}

void ResGausserQQHybrid::negate_element(FieldElement& a) const
{
  a.mDouble = -a.mDouble;
  Kp1.negate(a.mMod1, a.mMod1);
  Kp2.negate(a.mMod2, a.mMod2);
  mRRing.negate(a.mLongDouble, a.mLongDouble);
}

bool ResGausserQQHybrid::is_zero_element(FieldElement& a) const
{
  return not a.mIsPresent;
  bool result = (Kp1.is_zero(a.mMod1) and Kp2.is_zero(a.mMod2));
  //  bool result = (Kp1.is_zero(a.mMod1) and Kp2.is_zero(a.mMod2)) and
  //  (fabs(a.mDouble) < 1e-10);
  if (result and a.mDouble > 1e-10)
    {
      std::cout << "element is zero: ";
      out(std::cout, a) << std::endl;
    }
  return result;
}

void ResGausserQQHybrid::subtract_multiple_to_element(
    FieldElement& result,
    const FieldElement& a,
    const FieldElement& b) const
{
  result.mDouble = result.mDouble - a.mDouble * b.mDouble;
  Kp1.subtract_multiple(result.mMod1, a.mMod1, b.mMod1);
  Kp2.subtract_multiple(result.mMod2, a.mMod2, b.mMod2);
  mRRing.subtract_multiple(result.mLongDouble, a.mLongDouble, b.mLongDouble);
  int newsize = a.mDenominatorSize + b.mDenominatorSize;
  if (newsize > result.mDenominatorSize) result.mDenominatorSize = newsize;
  if (result.mIsPresent) numAdditions++;
  result.mIsPresent = true;
}

//////////////////////////////////
// CoefficientVector handling ////
//////////////////////////////////
long ResGausserQQHybrid::to_modp_long(CoefficientVector& coeffs,
                                      size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  return Kp1.coerceToLongInteger(elems[loc].mMod1);
}

void ResGausserQQHybrid::pushBackOne(CoefficientVector& coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  FieldElement uninitializedElement;
  elems.emplace_back(uninitializedElement);
  init_element(elems.back());
  copy_element(elems.back(), mOne);
}

void ResGausserQQHybrid::pushBackMinusOne(CoefficientVector& coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  FieldElement uninitializedElement;
  elems.emplace_back(uninitializedElement);
  init_element(elems.back());
  copy_element(elems.back(), mMinusOne);
}

void ResGausserQQHybrid::pushBackElement(
    CoefficientVector& coeffs,
    const CoefficientVector& take_from_here,
    size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  auto& elems_to_take = coefficientVector(take_from_here);
  FieldElement uninitializedElement;
  elems.emplace_back(uninitializedElement);
  init_element(elems.back());
  copy_element(elems.back(), elems_to_take[loc]);
}
void ResGausserQQHybrid::pushBackNegatedElement(
    CoefficientVector& coeffs,
    const CoefficientVector& take_from_here,
    size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  auto& elems_to_take = coefficientVector(take_from_here);

  FieldElement uninitializedElement;
  elems.emplace_back(uninitializedElement);
  init_element(elems.back());
  copy_element(elems.back(), elems_to_take[loc]);
  negate_element(elems.back());
}

CoefficientVector ResGausserQQHybrid::allocateCoefficientVector(
    ComponentIndex nelems) const
// create a row of 0's (over K).
{
  auto result = new std::vector<FieldElement>(nelems);
  for (ComponentIndex i = 0; i < nelems; i++)
    {
      init_element((*result)[i]);
      copy_element((*result)[i], mZero);
    }
  return coefficientVector(result);
}
CoefficientVector ResGausserQQHybrid::allocateCoefficientVector() const
// create a row of 0's (over K).
{
  return coefficientVector(new std::vector<FieldElement>);
}

void ResGausserQQHybrid::clear(CoefficientVector r,
                               ComponentIndex first,
                               ComponentIndex last) const
// set the elements in the range first..last to 0.
{
  auto& elems = coefficientVector(r);
  for (ComponentIndex i = first; i <= last; i++) copy_element(elems[i], mZero);
}

void ResGausserQQHybrid::deallocate(CoefficientVector r) const
{
  auto& elems = coefficientVector(r);
  for (auto i = elems.begin(); i != elems.end(); ++i) clear_element(*i);
  delete reinterpret_cast<std::vector<FieldElement>*>(r.mValue);
  r.mValue = nullptr;
}

ComponentIndex ResGausserQQHybrid::nextNonzero(CoefficientVector r,
                                               ComponentIndex first,
                                               ComponentIndex last) const
// returns last+1 in the case when there are no non-zero elements left.
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  elems += first;
  for (ComponentIndex i = first; i <= last; ++i, ++elems)
    if (!is_zero_element(*elems)) return i;

  return last + 1;
}

void ResGausserQQHybrid::fillFromSparse(CoefficientVector r,
                                        ComponentIndex len,
                                        CoefficientVector sparse,
                                        ComponentIndex* comps) const
// Fills 'r' from 'sparse' (and 'comps')
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  auto& svec = coefficientVector(sparse);
  auto sparseelems = svec.data();

  for (ComponentIndex i = 0; i < len; i++)
    copy_element(elems[*comps++], *sparseelems++);
}

void ResGausserQQHybrid::sparseCancel(CoefficientVector r,
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
  FieldElement a;
  init_element(a);
  copy_element(a, elems[*comps]);
  if (sparseelems[0].mDouble <
      0)  // should be minus_one, since it is either 1 or -1.
    {
      negate_element(a);
    }
  from_long_element(elems[*comps], 0);
  comps++;
  sparseelems++;
  for (ComponentIndex i = len - 1; i > 0; i--)
    {
      FieldElement& sparse = *sparseelems++;
      FieldElement& result = elems[*comps++];
      subtract_multiple_to_element(result, a, sparse);
    }
  negate_element(a);
  if (a.mDenominatorSize > mMaxDenominatorSize)
    {
      mMaxDenominatorSize = a.mDenominatorSize;
      if (M2_gbTrace >= 1)
        {
          std::cout << "coeff with den = " << a.mDenominatorSize << ": ";
          out(std::cout, a);
          std::cout << std::endl;
        }
    }
  result.emplace_back(std::move(a));
}

void ResGausserQQHybrid::sparseCancel(CoefficientVector r,
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
  FieldElement a;
  init_element(a);
  copy_element(a, elems[*comps]);
  if (sparseelems[0].mDouble <
      0)  // should be minus_one, since it is either 1 or -1.
    {
      negate_element(a);
    }
  for (ComponentIndex i = len; i > 0; i--)
    {
      FieldElement& sparse = *sparseelems++;
      FieldElement& result = elems[*comps++];
      subtract_multiple_to_element(result, a, sparse);
    }
  clear_element(a);
}

std::ostream& ResGausserQQHybrid::out(std::ostream& o, FieldElement& f) const
{
  buffer buf;
  mRRing.elem_text_out(buf, f.mLongDouble, true, false, false);
  o << "[" << f.mDouble << "," << Kp1.coerceToLongInteger(f.mMod1) << ","
    << Kp2.coerceToLongInteger(f.mMod1) << "," << buf.str() << ","
    << f.mDenominatorSize << "]";
  return o;
}

std::ostream& ResGausserQQHybrid::out(std::ostream& o,
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
std::ostream& ResGausserQQHybrid::debugDisplay(std::ostream& o,
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

std::ostream& ResGausserQQHybrid::debugDisplayRow(
    std::ostream& o,
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
