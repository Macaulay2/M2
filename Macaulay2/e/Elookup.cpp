// Copyright 1999  Michael E. Stillman
#include "EGB1.hpp"
int EMonomialLookupTable<egb_elem>::compare(egb_elem *p, egb_elem *q) const
{
  const monomial *m = I.lead_monomial(p->f);
  const monomial *n = I.lead_monomial(q->f);
  return I.compare_monomials(m,n);
}
int EMonomialLookupTable<ering_elem>::compare(ering_elem *p, ering_elem *q) const
{
  const monomial *m = I.lead_monomial_of_polynomial(p->f);
  const monomial *n = I.lead_monomial_of_polynomial(q->f);
  return I.compare_monomials(m,n);
}

