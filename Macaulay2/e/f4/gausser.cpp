// Copyright 2005 Michael E. Stillman.

#include "gausser.hpp"
#include "f4-mem.hpp"
#include "moninfo.hpp"

Gausser *Gausser::newGausser(const Ring *K)
{
  const Z_mod *Kp = K->cast_to_Z_mod();
  if (Kp != 0)
    return new Gausser(Kp);
  return 0;
}

Gausser::Gausser(const Z_mod *K0)
  : typ(ZZp), K(K0), Kp(K0->get_CoeffRing())
{
}

F4CoefficientArray Gausser::from_ringelem_array(int len, ring_elem *elems) const
{
  int i;
  switch (typ) {
  case ZZp:
    int *result = F4Mem::coefficients.allocate(len);//F4Mem::allocate_coeff_array(len);
    for (i=0; i<len; i++)
      result[i] = elems[i].int_val;
    return result;
  };
  return 0;
}

void Gausser::to_ringelem_array(int len, F4CoefficientArray F, ring_elem *result) const
{
  int *elems = static_cast<int *>(F);
  int i;
  switch (typ) {
  case ZZp:
    for (i=0; i<len; i++)
      result[i].int_val = elems[i];
  };
}

F4CoefficientArray Gausser::copy_F4CoefficientArray(int len, F4CoefficientArray F) const
{
  int *elems = static_cast<int *>(F);
  int i;
  switch (typ) {
  case ZZp:
    int *result = F4Mem::coefficients.allocate(len); //F4Mem::allocate_coeff_array(len);
    for (i=0; i<len; i++)
      result[i] = elems[i];
    return result;
  };
  return 0;
}

void Gausser::deallocate_F4CCoefficientArray(F4CoefficientArray &F, int len) const
{
  int *elems = static_cast<int *>(F);
  switch (typ) {
  case ZZp:
    F4Mem::coefficients.deallocate(elems);//F4Mem::deallocate_coeff_array(elems, len);
    F = 0;
  };
}
/////////////////////////////////////////////////////////////////////
///////// Dense row routines ////////////////////////////////////////
/////////////////////////////////////////////////////////////////////

void Gausser::dense_row_allocate(dense_row &r, long nelems) const
{
  //  int *elems = newarray_atomic(int, nelems);
  int *elems = F4Mem::coefficients.allocate(nelems);//F4Mem::allocate_coeff_array(nelems);
  r.coeffs = elems;
  r.len = nelems;
  for (int i=0; i<nelems; i++)
    Kp->set_zero(elems[i]);
}

void Gausser::dense_row_clear(dense_row &r, int first, int last) const
{
  int *elems = static_cast<int *>(r.coeffs);
  for (int i=first; i<=last; i++)
    Kp->set_zero(elems[i]);
}

void Gausser::dense_row_deallocate(dense_row &r) const
{
  deallocate_F4CCoefficientArray(r.coeffs, r.len);
  r.len = 0;
}

void Gausser::dense_row_fill_from_sparse(dense_row &r,
					 int len,
					 F4CoefficientArray sparse,
					 int *comps) const
{
  int *elems = static_cast<int *>(r.coeffs);
  int *sparseelems = static_cast<int *>(sparse);
  for (int i=0; i<len; i++)
    elems[*comps++] = *sparseelems++;

}

int Gausser::dense_row_next_nonzero(dense_row &r, int first, int last) const
{
  int *elems = static_cast<int *>(r.coeffs);
  elems += first;
  for (int i=first; i<=last; i++)
    if (!Kp->is_zero(*elems++))
      return i;
  return last+1;
}

void Gausser::dense_row_cancel_sparse(dense_row &r,
				      int len,
				      F4CoefficientArray sparse,
				      int *comps) const
{
  int *elems = static_cast<int *>(r.coeffs);
  int *sparseelems = static_cast<int *>(sparse);

  // Basically, over ZZ/p, we are doing: r += a*sparse,
  // where sparse is monic, and a is -r.coeffs[*comps].

  int a = elems[*comps];
  for (int i=0; i<len; i++)
    Kp->subtract_multiple(elems[*comps++], a, *sparseelems++);
}

void Gausser::dense_row_to_sparse_row(dense_row &r,
				      int &result_len,
				      F4CoefficientArray &result_sparse,
				      int *&result_comps,
				      int first,
				      int last) const
{
  int *elems = static_cast<int *>(r.coeffs);
  int len = 0;
  for (int i=first; i<=last; i++)
    if (!Kp->is_zero(elems[i])) len++;
  int *in_sparse = F4Mem::coefficients.allocate(len);//F4Mem::allocate_coeff_array(len);
  int *in_comps = F4Mem::components.allocate(len);
  result_len = len;
  result_sparse = in_sparse;
  result_comps = in_comps;
  for (int i=first; i<=last; i++)
    if (!Kp->is_zero(elems[i]))
      {
	*in_sparse++ = elems[i];
	*in_comps++ = i;
	Kp->set_zero(elems[i]);
      }
}

void Gausser::sparse_row_make_monic(int len,
				    F4CoefficientArray sparse) const
{
  int *elems = static_cast<int *>(sparse);
  int lead = *elems;
  // invert lead:
  Kp->invert(lead,lead);
  for (int i=0; i<len; i++, elems++)
    {
      // multiply the non-zero value *elems by lead.
      Kp->mult(*elems, *elems, lead);
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
