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

  EXP_ = newarray(int, nvars_);
}

int GBWeight::exponents_weight(const int *e) const
{
  int sum = 0;
  for (int i=0; i<nvars_; i++)
    sum += e[i] * wts_->array[i];
  return sum;
}

int GBWeight::gbvector_term_weight(const gbvector *f) const
{
  if (f == 0) return 0;
  R_->gbvector_get_lead_exponents(F_,f,EXP_);
  int fdeg = 0;
  if (use_component_degrees_ && f->comp > 0)
    fdeg = F_->primary_degree(f->comp-1);
  fdeg += exponents_weight(EXP_);
  return fdeg;
}

int GBWeight::gbvector_weight(const gbvector *f) const
{
  /* Return the maximum degree of any term of f */
  bool first_term = true;
  int deg = 0;
  if (f == 0) return 0;
  for (const gbvector *t=f; t != 0; t = t->next)
    {
      R_->gbvector_get_lead_exponents(F_,t,EXP_);
      int tdeg = 0;
      if (use_component_degrees_ && t->comp > 0)
	tdeg = F_->primary_degree(t->comp-1);
      tdeg += exponents_weight(EXP_);
      if (first_term)
	{
	  deg = tdeg;
	  first_term = false;
	}
      else
	if (tdeg > deg) deg = tdeg;
    }
  return deg;
}

int GBWeight::monomial_weight(const int *monom, int comp) const
{
  R_->get_flattened_monoid()->to_expvector(monom, EXP_);
  int fdeg = 0;
  if (use_component_degrees_ && comp >= 0)
    fdeg = F_->primary_degree(comp);
  fdeg += exponents_weight(EXP_);
  return fdeg;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
