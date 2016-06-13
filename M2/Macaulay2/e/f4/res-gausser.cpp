// Copyright 2005-2016 Michael E. Stillman.

#include "res-gausser.hpp"

ResGausser *ResGausser::newResGausser(int p)
{
  M2_ASSERT(p < 32767);
  M2_ASSERT(p > 1);
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
