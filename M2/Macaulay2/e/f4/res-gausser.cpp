// Copyright 2005-2016 Michael E. Stillman.

#include "res-gausser.hpp"

ResGausser *ResGausser::newResGausser(int p)
{
  assert(p < 32767);
  assert(p > 1);
  return new ResGausser(p);
}

ResGausser::ResGausser(int p)
  : typ(ZZp), n_dense_row_cancel(0), n_subtract_multiple(0)
{
  K = Z_mod::create(p);
  Kp = K->get_CoeffRing();
}

ResGausser::CoefficientArray ResGausser::from_ints(ComponentIndex len, const int* elems) const
{
  int i;
  switch (typ) {
  case ZZp:
    int *result = new int[len];
    for (i=0; i<len; i++)
      Kp->set_from_long(result[i], elems[i]);
    return result;
  };
  return nullptr;
}

void ResGausser::to_ints(ComponentIndex len, CoefficientArray F, int* result) const
{
  int* elems = F;
  int i;
  switch (typ) {
  case ZZp:
    for (i=0; i<len; i++)
      result[i] = coeff_to_int(elems[i]);
  };
}

void ResGausser::deallocate_F4CCoefficientArray(CoefficientArray &F, ComponentIndex len) const
{
  int* elems = F;
  switch (typ) {
  case ZZp:
    delete [] elems;
    F = nullptr;
  };
}
/////////////////////////////////////////////////////////////////////
///////// Dense row routines ////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

void ResGausser::dense_row_allocate(dense_row &r, ComponentIndex nelems) const
{
  int *elems = new int[nelems];
  r.coeffs = elems;
  r.len = nelems;
  for (ComponentIndex i=0; i<nelems; i++)
    Kp->set_zero(elems[i]);
}

void ResGausser::dense_row_clear(dense_row &r, ComponentIndex first, ComponentIndex last) const
{
  int* elems = r.coeffs;
  for (ComponentIndex i=first; i<=last; i++)
    Kp->set_zero(elems[i]);
}

void ResGausser::dense_row_deallocate(dense_row &r) const
{
  deallocate_F4CCoefficientArray(r.coeffs, r.len);
  r.len = 0;
}

void ResGausser::dense_row_fill_from_sparse(dense_row &r,
                                         ComponentIndex len,
                                         CoefficientArray sparse,
                                         ComponentIndex *comps) const
{
  int* elems = r.coeffs;
  int* sparseelems = sparse;
  for (ComponentIndex i=0; i<len; i++)
    elems[*comps++] = *sparseelems++;

}

ComponentIndex ResGausser::dense_row_next_nonzero(dense_row &r, ComponentIndex first, ComponentIndex last) const
{
  int* elems = r.coeffs;
  elems += first;
  for (ComponentIndex i=first; i<=last; i++)
    if (!Kp->is_zero(*elems++))
      return i;
  return last+1;
}

void ResGausser::dense_row_cancel_sparse_monic(dense_row &r,
                                      ComponentIndex len,
                                      CoefficientArray sparse,
                                      ComponentIndex *comps) const
{
  // KEY ASSUMPTION HERE: sparse[0] is "1".
  int* elems = r.coeffs;
  int* sparseelems = sparse;

  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  int a = elems[*comps];
  for (ComponentIndex i=len; i>0; i--)
    Kp->subtract_multiple(elems[*comps++], a, *sparseelems++);
}

void ResGausser::dense_row_cancel_sparse(dense_row &r,
                                         ComponentIndex len,
                                         CoefficientArray sparse,
                                         ComponentIndex *comps,
                                         FieldElement& a
                                         ) const
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
  
  int* elems = r.coeffs;
  int* sparseelems = sparse;
  int one;
  set_one(one);
  
  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  a = elems[*comps];
  if (sparse[0] != one) // should be minus_one
    Kp->negate(a, a);
  for (ComponentIndex i=len; i>0; i--)
    Kp->subtract_multiple(elems[*comps++], a, *sparseelems++);
  Kp->negate(a,a);
}

void ResGausser::dense_row_cancel_sparse(dense_row &r,
                                         ComponentIndex len,
                                         CoefficientArray sparse,
                                         ComponentIndex *comps,
                                         std::vector<FieldElement>& coeffInserter
                                         ) const
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
  
  int* elems = r.coeffs;
  int* sparseelems = sparse;
  int one;
  set_one(one);
  
  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  FieldElement a = elems[*comps];
  if (sparse[0] != one) // should be minus_one
    Kp->negate(a, a);
  for (ComponentIndex i=len; i>0; i--)
    Kp->subtract_multiple(elems[*comps++], a, *sparseelems++);
  Kp->negate(a,a);
  coeffInserter.push_back(a);
}


//////////////////////////////////
// CoefficientVector handling ////
//////////////////////////////////

CoefficientVector ResGausser::allocateCoefficientVector(ComponentIndex nelems) const
  // create a row of 0's (over K).
{
  auto result = new std::vector<int>(nelems);
  for (ComponentIndex i=0; i<nelems; i++)
    Kp->set_zero((*result)[i]);
  return coefficientVector(result);
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

void ResGausser::sparseCancelGivenMonic(CoefficientVector r,
                                        ComponentIndex len,
                                        CoefficientVector sparse,
                                        ComponentIndex* comps) const
  // r += c * sparse, where c is chosen to cancel column comps[0].
  // ASSUMPTION: the lead coeff of 'sparse' is 1.
{
  auto& vec = coefficientVector(r);
  auto elems = vec.data();
  auto& svec = coefficientVector(sparse);
  auto sparseelems = svec.data();

  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  n_dense_row_cancel++;
  n_subtract_multiple += len;
  int a = elems[*comps];
  for (ComponentIndex i=len; i>0; i--)
    Kp->subtract_multiple(elems[*comps++], a, *sparseelems++);
}

void ResGausser::sparseCancel(CoefficientVector r,
                              ComponentIndex len,
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
