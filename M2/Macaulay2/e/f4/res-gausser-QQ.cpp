// Copyright 2005-2017 Michael E. Stillman.

#include "res-gausser.hpp"

ResGausser *ResGausser::newResGausser(const Ring* K1)
{
  if (!K1->isFinitePrimeField())
    {
      ERROR("currently, res(...,FastNonminimal=>true) requires finite prime fields");
      return nullptr;
    }
  auto p = K1->characteristic();
  if (p > 32767)
    {
      ERROR("currently, res(...,FastNonminimal=>true) requires finite prime fields with p < 32767");
      return nullptr;
    }
  return new ResGausser(K1);
}

ResGausser::ResGausser(const Ring* K1)
  : typ(ZZp), K(K1), n_dense_row_cancel(0), n_subtract_multiple(0)
{
  int p = static_cast<int>(K->characteristic());
  auto K2 = Z_mod::create(p);
  Kp = K2->get_CoeffRing();
}

//////////////////////////////////
// CoefficientVector handling ////
//////////////////////////////////
void ResGausser::pushBackOne(CoefficientVector& coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  FieldElement one;
  set_one(one);
  elems.push_back(one);
}

void ResGausser::pushBackMinusOne(CoefficientVector& coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  FieldElement minus_one;
  set_one(minus_one);
  negate(minus_one, minus_one);
  elems.push_back(minus_one);
}
void ResGausser::pushBackElement(CoefficientVector& coeffs,
                                 const CoefficientVector& take_from_here,
                                 size_t loc) const
{
  auto& elems = coefficientVector(coeffs); 
  auto& elems_to_take = coefficientVector(take_from_here);
  elems.push_back(elems_to_take[loc]);
}
void ResGausser::pushBackNegatedElement(CoefficientVector& coeffs,
                                 const CoefficientVector& take_from_here,
                                 size_t loc) const
{
  auto& elems = coefficientVector(coeffs); 
  auto& elems_to_take = coefficientVector(take_from_here);
 
  FieldElement a = elems_to_take[loc];
  negate(a,a);
  elems.push_back(a);
}

CoefficientVector ResGausser::allocateCoefficientVector(ComponentIndex nelems) const
  // create a row of 0's (over K).
{
  auto result = new std::vector<int>(nelems);
  for (ComponentIndex i=0; i<nelems; i++)
    Kp->set_zero((*result)[i]);
  return coefficientVector(result);
}
CoefficientVector ResGausser::allocateCoefficientVector() const
  // create a row of 0's (over K).
{
  return coefficientVector(new std::vector<int>);
}

void ResGausser::clear(CoefficientVector r, ComponentIndex first, ComponentIndex last) const
  // set the elements in the range first..last to 0.
{
  auto& elems = coefficientVector(r);
  for (ComponentIndex i=first; i<=last; i++)
    Kp->set_zero(elems[i]);
}

void ResGausser::deallocate(CoefficientVector r) const
{
  delete reinterpret_cast<std::vector<FieldElement>*>(r.mValue);
  r.mValue = nullptr;
}

ComponentIndex ResGausser::nextNonzero(CoefficientVector r, ComponentIndex first, ComponentIndex last) const
  // returns last+1 in the case when there are no non-zero elements left.
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  elems += first;
  for (ComponentIndex i=first; i<=last; i++)
    if (!Kp->is_zero(*elems++))
      return i;
  return last+1;
}

void ResGausser::fillFromSparse(CoefficientVector r,
                                ComponentIndex len,
                                CoefficientVector sparse,
                                ComponentIndex* comps) const
  // Fills 'r' from 'sparse' (and 'comps')
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  auto& svec = coefficientVector(sparse);
  auto sparseelems = svec.data();

  for (ComponentIndex i=0; i<len; i++)
    elems[*comps++] = *sparseelems++;
}

void ResGausser::sparseCancel(CoefficientVector r,
                              CoefficientVector sparse,
                              ComponentIndex* comps,
                              CoefficientVector result_loc
                              ) const
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

  int one;
  set_one(one);
  
  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  FieldElement a = elems[*comps];
  if (sparseelems[0] != one) // should be minus_one
    Kp->negate(a, a);
  for (ComponentIndex i=len; i>0; i--)
    Kp->subtract_multiple(elems[*comps++], a, *sparseelems++);
  Kp->negate(a,a);
  result.push_back(a);
}

void ResGausser::debugDisplay(CoefficientVector r) const
{
  if (r.isNull())
    fprintf(stdout, "vector is null!");
  auto& elems = coefficientVector(r);
  for (long j=0; j<elems.size(); ++j)
    fprintf(stdout, " %d", coeff_to_int(elems[j]));
}

void ResGausser::debugDisplayRow(int ncolumns, const std::vector<int>& comps, CoefficientVector coeffs) const
{
  auto& elems = coefficientVector(coeffs);
  auto monom = comps.begin();
  auto coeff = elems.begin();
  auto end = elems.end();
  for (ComponentIndex c=0; c<ncolumns; c++)
    {
      if (coeff == end or *monom != c)
        fprintf(stdout, " .");
      else
        {
          fprintf(stdout, " %d", coeff_to_int(*coeff));
          ++coeff;
          ++monom;
        }
    }
  fprintf(stdout, "\n");
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
