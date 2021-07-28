// Copyright 2005, Michael E. Stillman

#include <functional>
#include "reducedgb-marked.hpp"
#include "monideal.hpp"
#include "matrix-con.hpp"

MarkedGB *MarkedGB::create(const PolynomialRing *originalR0,
                           const FreeModule *F0,
                           const FreeModule *Fsyz0)
{
  return new MarkedGB(originalR0, F0, Fsyz0);
}

MarkedGB::~MarkedGB()
{
  delete T;
  freemem(leadterms);
}

void MarkedGB::set_gb(VECTOR(POLY) & polys0) {}

struct MarkedGB_sorter : public std::binary_function<int, int, bool>
{
  GBRing *R;
  const FreeModule *F;
  const VECTOR(POLY) & gb;
  MarkedGB_sorter(GBRing *R0, const FreeModule *F0, const VECTOR(POLY) & gb0)
      : R(R0), F(F0), gb(gb0)
  {
  }
  bool operator()(int xx, int yy)
  {
    gbvector *x = gb[xx].f;
    gbvector *y = gb[yy].f;
    return R->gbvector_compare(F, x, y) == LT;
  }
};

MarkedGB::MarkedGB(const PolynomialRing *originalR0,
                   const FreeModule *F0,
                   const FreeModule *Fsyz0)
    : ReducedGB(originalR0->get_gb_ring(), originalR0, F0, Fsyz0), T(0)
{
  T = MonomialTable::make(R->n_vars());
}

void MarkedGB::add_marked_elems(const VECTOR(gbvector *) & leadterms0,
                                const VECTOR(POLY) & polys0,
                                bool auto_reduced)
{
  // First sort these elements via increasing lex order (or monomial order?)
  // Next insert minimal elements into T, and polys
  const Monoid *M = originalR->getMonoid();

  leadterms = newarray(gbvector *, leadterms0.size());

  // Now loop through each element, and see if the lead monomial is in T.
  // If not, add it in , and place element into 'polys'.

  for (int i = 0; i < leadterms0.size(); i++)
    {
      POLY h;
      ring_elem junk;

      gbvector *f = polys0[i].f;
      gbvector *inf = leadterms0[i];

      h.f = R->gbvector_copy(f);
      h.fsyz = R->gbvector_copy(polys0[i].fsyz);

      gbvector *iinf = 0;
      for (gbvector *t = h.f; t != 0; t = t->next)
        if (inf->comp == t->comp && EQ == M->compare(inf->monom, t->monom))
          {
            iinf = t;
            break;
          }
      if (!iinf)
        {
          ERROR("lead term does not appear in the polynomial!");
          iinf = f;
        }
      leadterms[i] = iinf;

      exponents e = R->exponents_make();
      R->gbvector_get_lead_exponents(F, iinf, e);

      //      if (auto_reduced)
      //        remainder(h,false,junk); // This auto-reduces h. MES MES!! Maybe
      //        not for marked GB!!

      R->gbvector_remove_content(h.f, h.fsyz);

      T->insert(e, iinf->comp, i);
      polys.push_back(h);
    }

  auto_reduce();
}

void MarkedGB::auto_reduce()
{
  // For each element, reduce all terms which are not
  // the marked lead term
  ring_elem not_used;

  for (int i = 0; i < polys.size(); i++)
    marked_remainder(polys[i], false, not_used, leadterms[i]);
}

void MarkedGB::marked_remainder(POLY &f,
                                bool use_denom,
                                ring_elem &denom,
                                gbvector *marked_lead_term)
// If marked_lead_term is not NULL, then it should be a pointer
// to a term in f.f.  This term will not be reduced, and the new lead term will
// replace this one.
// (This could happen if the coefficient changes).
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  exponents EXP = ALLOCATE_EXPONENTS(R->exponent_byte_size());

  while (!R->gbvector_is_zero(h.f))
    {
      if (h.f != marked_lead_term)
        {
          R->gbvector_get_lead_exponents(F, h.f, EXP);
          int x = h.f->comp;
          int w = T->find_divisor(EXP, x);
          if (w >= 0)
            {
              POLY g = polys[w];
              R->gbvector_reduce_with_marked_lead_term(F,
                                                       Fsyz,
                                                       head.next,
                                                       h.f,
                                                       h.fsyz,
                                                       leadterms[w],
                                                       g.f,
                                                       g.fsyz,
                                                       use_denom,
                                                       denom);
              continue;
            }
        }
      frem->next = h.f;
      frem = frem->next;
      h.f = h.f->next;
      frem->next = 0;
    }

  h.f = head.next;
  f.f = h.f;
  f.fsyz = h.fsyz;
  R->gbvector_sort(F, f.f);
  R->gbvector_sort(Fsyz, f.fsyz);
}

void MarkedGB::remainder(POLY &f, bool use_denom, ring_elem &denom)
{
  marked_remainder(f, use_denom, denom, NULL);
}

void MarkedGB::remainder(gbvector *&f, bool use_denom, ring_elem &denom)
{
  //  return geo_remainder(f,use_denom,denom);
  gbvector *zero = 0;
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  gbvector *h = f;
  exponents EXP = ALLOCATE_EXPONENTS(R->exponent_byte_size());

  while (!R->gbvector_is_zero(h))
    {
      R->gbvector_get_lead_exponents(F, h, EXP);
      int x = h->comp;
      int w = T->find_divisor(EXP, x);
      if (w < 0)
        {
          frem->next = h;
          frem = frem->next;
          h = h->next;
          frem->next = 0;
        }
      else
        {
          POLY g = polys[w];
          R->gbvector_reduce_with_marked_lead_term(F,
                                                   Fsyz,
                                                   head.next,
                                                   h,
                                                   zero,
                                                   leadterms[w],
                                                   g.f,
                                                   zero,
                                                   use_denom,
                                                   denom);
        }
    }
  h = head.next;
  f = h;
  R->gbvector_sort(F, f);
}

void MarkedGB::geo_remainder(gbvector *&f, bool use_denom, ring_elem &denom)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;

  gbvectorHeap fb(R, F);
  gbvectorHeap zero(R, Fsyz);
  fb.add(f);

  const gbvector *lead;
  exponents EXP = ALLOCATE_EXPONENTS(R->exponent_byte_size());
  while ((lead = fb.get_lead_term()) != NULL)
    {
      R->gbvector_get_lead_exponents(F, lead, EXP);
      int x = lead->comp;
      int w = T->find_divisor(EXP, x);
      if (w < 0)
        {
          frem->next = fb.remove_lead_term();
          frem = frem->next;
          frem->next = 0;
        }
      else
        {
          POLY g = polys[w];
          R->reduce_marked_lead_term_heap(
              F, Fsyz, lead, EXP, head.next, fb, zero, leadterms[w], g.f, 0);
        }
    }
  f = head.next;
  R->gbvector_sort(F, f);
}

const Matrix /* or null */ *MarkedGB::get_initial(int nparts)
{
  if (nparts > 0)
    {
      ERROR("Cannot determine given initial monomials");
      return 0;
    }
  MatrixConstructor mat(F, 0);
  for (int i = 0; i < polys.size(); i++)
    {
      gbvector *f = R->gbvector_lead_term(-1, F, leadterms[i]);
      mat.append(originalR->translate_gbvector_to_vec(F, f));
    }
  return mat.to_matrix();
}

const Matrix /* or null */ *MarkedGB::get_parallel_lead_terms(M2_arrayint w)
{
  MatrixConstructor mat(F, 0);
  for (int i = 0; i < polys.size(); i++)
    {
      gbvector *f =
          R->gbvector_parallel_lead_terms(w, F, leadterms[i], polys[i].f);
      mat.append(originalR->translate_gbvector_to_vec(F, f));
    }
  return mat.to_matrix();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
