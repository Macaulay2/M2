#include "gbweight.hpp"
#include "freemod.hpp"
#include "gbring.hpp"
#include "poly.hpp"

GBWeight::GBWeight(const FreeModule *F, M2_arrayint wts0)
  : F_(F)
{
  // If wts has length 0 or is NULL:
  // Use the primary degrees, if they are positive.
  // For each that is negative, use "1"

  // If the wt vector is not part of the degrees, then set
  // use_component_degrees to false.

  const PolynomialRing * A = F->get_ring()->cast_to_PolynomialRing();
  assert(A != 0);
  R_ = A->get_gb_ring();

  const Monoid *M = R_->get_flattened_monoid();
  nvars_ = R_->n_vars();
  
  wts_ = makearrayint(nvars_);

  if (!wts0 || wts0->len != nvars_)
    {
      use_component_degrees_ = true;
      for (int i=0; i<nvars_; i++)
	{
	  int d = M->primary_degree_of_var(i);
	  wts_->array[i] = (d > 0 ? d : 1);
	  if (d <= 0)
	    use_component_degrees_ = false;
	}
    }
  else
    {
      // Use the provided wt vector
      use_component_degrees_ = false;
      for (int i=0; i<nvars_; i++)
	{
	  int d = wts0->array[i];
	  wts_->array[i] = (d > 0 ? d : 1);
	}
    }

  EXP_ = newarray_atomic(int, nvars_);

  if (use_component_degrees_)
    {
      Fdegs_ = newarray_atomic(long, F->rank()+1);
      Fdegs_[0] = 0;
      for (int j=0; j<F->rank(); j++)
	Fdegs_[j+1] = F->primary_degree(j);
    }
  else
    Fdegs_ = 0;
}

int GBWeight::exponents_weight(const int *e, int comp) const
{
  int sum = 0;
  for (int i=0; i<nvars_; i++)
    sum += e[i] * wts_->array[i];
  if (use_component_degrees_ && comp > 0)
    sum += Fdegs_[comp];
  return sum;
}

int GBWeight::gbvector_term_weight(const gbvector *f) const
{
  if (f == 0) return 0;
  R_->gbvector_get_lead_exponents(F_,f,EXP_);
  return exponents_weight(EXP_,f->comp);
}

int GBWeight::gbvector_weight(const gbvector *f, int &initial_term_weight) const
{
  /* Return the maximum degree of any term of f */

  if (f == 0) 
    {
      initial_term_weight = 0;
      return 0;
    }
  int deg = gbvector_term_weight(f); 
  initial_term_weight = deg;
  for (const gbvector *t=f->next; t != 0; t = t->next)
    {
      int tdeg = gbvector_term_weight(t);
      if (tdeg > deg) deg = tdeg;
    }

  return deg;
}

int GBWeight::gbvector_weight(const gbvector *f) const
{
  int not_used;
  return gbvector_weight(f, not_used);
}

int GBWeight::monomial_weight(const int *monom, int comp) const
{
  R_->get_flattened_monoid()->to_expvector(monom, EXP_);
  return exponents_weight(EXP_, comp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
