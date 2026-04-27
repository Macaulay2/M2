#include "gbweight.hpp"
#include "freemod.hpp"
#include "gbring.hpp"
#include "polyring.hpp"


GBWeight::GBWeight(const FreeModule *F, M2_arrayint wts0) : F_(F)
{
  // If wts has length 0 or is NULL:
  // Use the primary degrees, if they are positive.
  // For each that is negative, use "1"

  // If the wt vector is not part of the degrees, then set
  // use_component_degrees to false.

  const PolynomialRing *A = F->get_ring()->cast_to_PolynomialRing();
  assert(A != 0);
  R_ = A->get_gb_ring();

  const Monoid *M = R_->get_flattened_monoid();
  nvars_ = R_->n_vars();

  wts_ = M2_makearrayint(nvars_);

  if (!wts0 || wts0->len != nvars_)
    {
      use_component_degrees_ = true;
      for (int i = 0; i < nvars_; i++)
        {
          int d = M->primary_degree_of_var(i);
          wts_->array[i] = (d > 0 ? d : 1);
          if (d <= 0) use_component_degrees_ = false;
        }
    }
  else
    {
      // Use the provided wt vector
      use_component_degrees_ = false;
      for (int i = 0; i < nvars_; i++)
        {
          int d = wts0->array[i];
          wts_->array[i] = (d > 0 ? d : 1);
        }
    }

  exp_size = EXPONENT_BYTE_SIZE(nvars_);

  if (use_component_degrees_)
    {
      Fdegs_ = newarray_atomic(long, F->rank() + 1);
      Fdegs_[0] = 0;
      for (int j = 0; j < F->rank(); j++) Fdegs_[j + 1] = F->primary_degree(j);
    }
  else
    Fdegs_ = nullptr;
}

int GBWeight::exponents_weight(const_exponents e, int comp) const
{
  int sum = exponents::weight(nvars_, e, wts_);
  if (use_component_degrees_ && comp > 0) sum += Fdegs_[comp];
  return sum;
}

int GBWeight::gbvector_term_weight(const gbvector *f) const
{
  if (f == nullptr) return 0;
  exponents_t EXP = ALLOCATE_EXPONENTS(exp_size);
  R_->gbvector_get_lead_exponents(F_, f, EXP);
  return exponents_weight(EXP, f->comp);
}

int GBWeight::gbvector_weight(const gbvector *f, int &initial_term_weight) const
{
  /* Return the maximum degree of any term of f */

  if (f == nullptr)
    {
      initial_term_weight = 0;
      return 0;
    }
  int deg = gbvector_term_weight(f);
  initial_term_weight = deg;
  for (const gbvector *t = f->next; t != nullptr; t = t->next)
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

int GBWeight::monomial_weight(const_monomial monom, int comp) const
{
  exponents_t EXP = ALLOCATE_EXPONENTS(exp_size);
  R_->get_flattened_monoid()->to_expvector(monom, EXP);
  return exponents_weight(EXP, comp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
