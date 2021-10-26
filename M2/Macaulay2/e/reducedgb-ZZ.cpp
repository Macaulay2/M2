// Copyright 2005, Michael E. Stillman

#include "reducedgb-ZZ.hpp"
#include "monideal.hpp"
#include <functional>
#include <algorithm>
#include "text-io.hpp"
ReducedGB_ZZ::~ReducedGB_ZZ()
{
  delete T;
  ringtableZZ = 0;
}

ReducedGB_ZZ::ReducedGB_ZZ(GBRing *R0,
                           const PolynomialRing *originalR0,
                           const FreeModule *F0,
                           const FreeModule *Fsyz0)
    : ReducedGB(R0, originalR0, F0, Fsyz0),
      T(nullptr),
      ringtableZZ(nullptr)
{
  T = MonomialTableZZ::make(R0->n_vars());
  if (originalR->is_quotient_ring())
    ringtableZZ = originalR->get_quotient_MonomialTableZZ();
}

void ReducedGB_ZZ::set_gb(VECTOR(POLY) & polys0) {}
struct ReducedGB_ZZ_sorter : public std::binary_function<int, int, bool>
{
  GBRing *R;
  const FreeModule *F;
  const VECTOR(POLY) & gb;
  ReducedGB_ZZ_sorter(GBRing *R0,
                      const FreeModule *F0,
                      const VECTOR(POLY) & gb0)
      : R(R0), F(F0), gb(gb0)
  {
  }
  bool operator()(int xx, int yy)
  {
    gbvector *x = gb[xx].f;
    gbvector *y = gb[yy].f;
    int cmp = R->gbvector_compare(F, x, y);
    if (cmp == LT) return true;
    if (cmp == GT) return false;
    // Now order them in ascending order on the coeff (which should always be
    // POSITIVE).
    return (mpz_cmp(x->coeff.get_mpz(), y->coeff.get_mpz()) < 0);
  }
};

void ReducedGB_ZZ::minimalize(const VECTOR(POLY) & polys0, bool auto_reduced)
// I have to decide: does this ADD to the existing set?
{
  // First sort these elements via increasing lex order (or monomial order?)
  // Next insert minimal elements into T, and polys

  VECTOR(int) positions;
  positions.reserve(polys0.size());

  for (int i = 0; i < polys0.size(); i++) positions.push_back(i);

  std::stable_sort(
      positions.begin(), positions.end(), ReducedGB_ZZ_sorter(R, F, polys0));

  // Now loop through each element, and see if the lead monomial is in T.
  // If not, add it in , and place element into 'polys'.

  for (VECTOR(int)::iterator i = positions.begin(); i != positions.end(); i++)
    {
      gbvector *f = polys0[*i].f;
      exponents e = R->exponents_make();
      R->gbvector_get_lead_exponents(F, f, e);
      if ((!ringtableZZ ||
           !ringtableZZ->find_term_divisors(1, f->coeff.get_mpz(), e, 1)) &&
          T->find_term_divisors(1, f->coeff.get_mpz(), e, f->comp) == 0)
        {
          // Keep this element

          POLY h;
          ring_elem junk;

          h.f = R->gbvector_copy(f);
          h.fsyz = R->gbvector_copy(polys0[*i].fsyz);

          if (auto_reduced) remainder(h, false, junk);  // This auto-reduces h.

          if (h.f != 0 && mpz_sgn(h.f->coeff.get_mpz()) < 0)
            {
              R->gbvector_mult_by_coeff_to(h.f, globalZZ->minus_one());
              R->gbvector_mult_by_coeff_to(h.fsyz, globalZZ->minus_one());
            }

          T->insert(h.f->coeff.get_mpz(), e, h.f->comp, INTSIZE(polys));
          polys.push_back(h);
        }
      else
        R->exponents_delete(e);
    }
}

enum ReducedGB_ZZ::divisor_type ReducedGB_ZZ::find_divisor(exponents exp,
                                                           int comp,
                                                           int &result_loc)
{
  int w = T->find_smallest_coeff_divisor(exp, comp);  // gives smallest coeff
  int r = -1;
  if (ringtableZZ) r = ringtableZZ->find_smallest_coeff_divisor(exp, 1);

  if (r < 0)
    {
      if (w < 0) return DIVISOR_NONE;
      result_loc = w;
      return DIVISOR_MODULE;
    }
  // r >= 0
  if (w < 0)
    {
      result_loc = r;
      return DIVISOR_RING;
    }
  // r >= 0, w >= 0
  mpz_srcptr rc = originalR->quotient_gbvector(r)->coeff.get_mpz();
  mpz_srcptr wc = polys[w].f->coeff.get_mpz();
  if (mpz_cmpabs(rc, wc) > 0)
    {
      result_loc = w;
      return DIVISOR_MODULE;
    }
  result_loc = r;
  return DIVISOR_RING;
}

void ReducedGB_ZZ::remainder(POLY &f, bool use_denom, ring_elem &denom)
{
  gbvector *zero = 0;
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  exponents EXP = ALLOCATE_EXPONENTS(R->exponent_byte_size());
  gbvector *r;
  POLY g;
  while (!R->gbvector_is_zero(h.f))
    {
      int w;
      R->gbvector_get_lead_exponents(F, h.f, EXP);
      int x = h.f->comp;
      enum divisor_type typ = find_divisor(EXP, x, w);
      switch (typ)
        {
          case DIVISOR_RING:
            r = const_cast<gbvector *>(originalR->quotient_gbvector(w));
            if (R->gbvector_reduce_lead_term_ZZ(F, Fsyz, h.f, zero, r, zero))
              continue;
            break;
          case DIVISOR_MODULE:
            g = polys[w];
            if (R->gbvector_reduce_lead_term_ZZ(
                    F, Fsyz, h.f, h.fsyz, g.f, g.fsyz))
              continue;
            break;
          case DIVISOR_NONE:
            break;
        }
      frem->next = h.f;
      frem = frem->next;
      h.f = h.f->next;
      frem->next = 0;
    }
  h.f = head.next;
  f.f = h.f;
  originalR->get_quotient_info()->gbvector_normal_form(Fsyz, h.fsyz);
  f.fsyz = h.fsyz;
}

void ReducedGB_ZZ::remainder(gbvector *&f, bool use_denom, ring_elem &denom)
{
  gbvector *zero = 0;
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  gbvector *h = f;
  exponents EXP = ALLOCATE_EXPONENTS(R->exponent_byte_size());

  gbvector *r;
  POLY g;
  while (!R->gbvector_is_zero(h))
    {
      int w;
      R->gbvector_get_lead_exponents(F, h, EXP);
      int x = h->comp;
      enum divisor_type typ = find_divisor(EXP, x, w);
      switch (typ)
        {
          case DIVISOR_RING:
            r = const_cast<gbvector *>(originalR->quotient_gbvector(w));
            if (R->gbvector_reduce_lead_term_ZZ(F, Fsyz, h, zero, r, zero))
              continue;
            break;
          case DIVISOR_MODULE:
            g = polys[w];
            if (M2_gbTrace >= 4)
              {
                buffer o;
                R->gbvector_text_out(o, F, h);
                o << newline << "  divisor " << w << " is ";
                R->gbvector_text_out(o, F, g.f);
                o << newline;
                emit(o.str());
              }
            if (R->gbvector_reduce_lead_term_ZZ(F, Fsyz, h, zero, g.f, zero))
              continue;
            break;
          case DIVISOR_NONE:
            break;
        }
      frem->next = h;
      frem = frem->next;
      h = h->next;
      frem->next = 0;
    }
  h = head.next;
  f = h;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
