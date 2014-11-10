// Copyright 2014 Michael E. Stillman.

#include "res-gausser.hpp"
#include "f4-mem.hpp"

ResGausser *ResGausser::create(const Ring *K, F4Mem *Mem0)
{
  auto Kp = dynamic_cast<const InterfaceRingType*>(K);
  if (Kp != 0)
    return new ResGausser(Kp->ring(),Mem0);
  return 0;
}

ResGausser::ResGausser(const RingType& K0, F4Mem *Mem0)
  : mRing(K0), Mem(Mem0)
{
  mStats.mNumCallsDenseRowCancel = 0;
  mStats.mNumCallsSubtractMultiple = 0;
}

ResGausser::CoefficientArray ResGausser::allocateCoefficientArray(ComponentIndex len) const
{
  return new ElementType[len]; // Mem->coefficients.allocate(len);
}

ResGausser::CoefficientArray ResGausser::from_ringelem_array(ComponentIndex len, ring_elem *elems) const
{
  CoefficientArray result = allocateCoefficientArray(len);
  for (ComponentIndex i=0; i<len; i++)
    ring().from_ring_elem(result[i], elems[i]);
  return result;
}

void ResGausser::to_ringelem_array(ComponentIndex len, CoefficientArray F, ring_elem *result) const
{
  ElementType* elems = F;
  for (ComponentIndex i=0; i<len; i++)
    ring().to_ring_elem(result[i], elems[i]);
}

ResGausser::CoefficientArray ResGausser::copy_CoefficientArray(ComponentIndex len, CoefficientArray F) const
{
  ElementType* elems = F;
  ElementType* result = allocateCoefficientArray(len);
  for (ComponentIndex i=0; i<len; i++)
    result[i] = elems[i];
  return result;
}

void ResGausser::deallocate_CoefficientArray(CoefficientArray &F, ComponentIndex len) const
{
  ElementType* elems = F;
  delete [] elems;
  //Mem->coefficients.deallocate(elems);
  F = 0;
}
/////////////////////////////////////////////////////////////////////
///////// Dense row routines ////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

void ResGausser::dense_row_allocate(dense_row &r, ComponentIndex nelems) const
{
  //  int *elems = newarray_atomic(int, nelems);
  ElementType* elems = allocateCoefficientArray(nelems);
  r.coeffs = elems;
  r.len = nelems;
  for (ComponentIndex i=0; i<nelems; i++)
    ring().set_zero(elems[i]);
}

void ResGausser::dense_row_clear(dense_row &r, ComponentIndex first, ComponentIndex last) const
{
  ElementType* elems = r.coeffs;
  for (ComponentIndex i=first; i<=last; i++)
    ring().set_zero(elems[i]);
}

void ResGausser::dense_row_deallocate(dense_row &r) const
{
  deallocate_CoefficientArray(r.coeffs, r.len);
  r.len = 0;
}

void ResGausser::dense_row_fill_from_sparse(dense_row &r,
                                         ComponentIndex len,
                                         CoefficientArray sparse,
                                         ComponentIndex *comps) const
{
  ElementType* elems = r.coeffs;
  ElementType* sparseelems = sparse;
  for (ComponentIndex i=0; i<len; i++)
    elems[*comps++] = *sparseelems++;

}

ResGausser::ComponentIndex ResGausser::dense_row_next_nonzero(dense_row &r, ComponentIndex first, ComponentIndex last) const
{
  ElementType* elems = r.coeffs + first;
  for (ComponentIndex i=first; i<=last; i++)
    if (!ring().is_zero(*elems++))
      return i;
  return last+1;
}

void ResGausser::dense_row_cancel_sparse(dense_row &r,
                                      ComponentIndex len,
                                      CoefficientArray sparse,
                                      ComponentIndex *comps) const
{
  ElementType* elems = r.coeffs;
  ElementType* sparseelems = sparse;

  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  mStats.mNumCallsDenseRowCancel++;
  mStats.mNumCallsSubtractMultiple += len;
  ElementType a = elems[*comps];
  for (ComponentIndex i=len; i>0; i--)
    ring().subtract_multiple(elems[*comps++], a, *sparseelems++);
}

void ResGausser::dense_row_to_sparse_row(dense_row &r,
                                         ComponentIndex &result_len,
                                         CoefficientArray &result_sparse,
                                         ComponentIndex *&result_comps,
                                         ComponentIndex first,
                                         ComponentIndex last) const
{
  ElementType* elems = r.coeffs;
  ComponentIndex len = 0;
  for (ComponentIndex i=first; i<=last; i++)
    if (!ring().is_zero(elems[i])) len++;
  ElementType* in_sparse = allocateCoefficientArray(len);
  ComponentIndex *in_comps = Mem->coefficients.allocate(len);
  result_len = len;
  result_sparse = in_sparse;
  result_comps = in_comps;
  for (ComponentIndex i=first; i<=last; i++)
    if (!ring().is_zero(elems[i]))
      {
        *in_sparse++ = elems[i];
        *in_comps++ = i;
        ring().set_zero(elems[i]);
      }
}

void ResGausser::sparse_row_make_monic(ComponentIndex len,
                                       CoefficientArray sparse) const
{
  ElementType* elems = sparse;
  ElementType lead = *elems;
  // invert lead:
  ring().invert(lead,lead);
  for (ComponentIndex i=0; i<len; i++, elems++)
    {
      // multiply the non-zero value *elems by lead.
      ring().mult(*elems, *elems, lead);
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
