// Copyright 2005, Michael E. Stillman

#include "reducedgb-field-local.hpp"
#include "monideal.hpp"
#include "montable.hpp"
#include "gbweight.hpp"
#include "polyring.hpp"
#include <functional>
#include <algorithm>
#include "text-io.hpp"

ReducedGB_Field_Local::~ReducedGB_Field_Local() {}
ReducedGB_Field_Local::ReducedGB_Field_Local(GBRing *R0,
                                             const PolynomialRing *originalR0,
                                             const FreeModule *F0,
                                             const FreeModule *Fsyz0,
                                             const GBWeight *wt0)
    : ReducedGB_Field(R0, originalR0, F0, Fsyz0), T1(0), wt(wt0)
{
  // fprintf(stderr, "creating GB with local order\n");
  if (wt == 0) wt = new GBWeight(F0, 0);
  for (int i = 0; i < originalR0->n_quotients(); i++)
    {
      int f_lead_wt;
      const gbvector *f = originalR0->quotient_gbvector(i);
      int d = wt->gbvector_weight(f, f_lead_wt);
      int a = d - f_lead_wt;

      divisor_info t;
      t.g.f = const_cast<gbvector *>(f);
      t.g.fsyz = 0;
      t.size = R->gbvector_n_terms(f);
      t.alpha = a;

      ring_elems.push_back(t);
    }
}

struct ReducedGB_Field_Local_sorter
    : public std::binary_function<int, int, bool>
{
  GBRing *R;
  const FreeModule *F;
  const VECTOR(POLY) & gb;
  std::vector<int> degs;
  ReducedGB_Field_Local_sorter(GBRing *R0,
                               const FreeModule *F0,
                               const VECTOR(POLY) & gb0)
      : R(R0), F(F0), gb(gb0)
  {
    auto M = R->get_flattened_monoid();
    for (size_t i = 0; i < gb0.size(); i++)
      {
        gbvector *f = gb0[i].f;
        degs.push_back(M->simple_degree(f->monom));
      }
  }
  bool operator()(int xx, int yy)
  {
    // this is the < operation
    if (degs[xx] < degs[yy]) return true;
    if (degs[xx] > degs[yy]) return false;
    gbvector *x = gb[xx].f;
    gbvector *y = gb[yy].f;
    return R->gbvector_compare(F, x, y) == LT;
  }
};

void ReducedGB_Field_Local::minimalize(const VECTOR(POLY) & polys0,
                                       bool auto_reduced)
{
  // First sort these elements via increasing lex order (or monomial order?)
  // Next insert minimal elements into T, and polys

  VECTOR(int) positions;
  positions.reserve(polys0.size());

  for (int i = 0; i < polys0.size(); i++) positions.push_back(i);

  //  displayElements("-- before sort --", R, polys0, [](auto& g) { return g.f;
  //  } );

  std::stable_sort(positions.begin(),
                   positions.end(),
                   ReducedGB_Field_Local_sorter(R, F, polys0));

  //  VECTOR(gbvector*) sorted_elements_debug_only;
  //  for (int i=0; i<positions.size(); i++)
  //    sorted_elements_debug_only.push_back(polys0[positions[i]].f);
  //  displayElements("-- after sort --", R, sorted_elements_debug_only,
  //  [](auto& g) { return g; } );

  // Now loop through each element, and see if the lead monomial is in T.
  // If not, add it in , and place element into 'polys'.

  for (VECTOR(int)::iterator i = positions.begin(); i != positions.end(); i++)
    {
      Bag *not_used;
      gbvector *f = polys0[*i].f;
      exponents e = R->exponents_make();
      R->gbvector_get_lead_exponents(F, f, e);
      if ((!Rideal || !Rideal->search_expvector(e, not_used)) &&
          T->find_divisors(1, e, f->comp) == 0)
        {
          // Keep this element

          POLY h;
          ring_elem junk;

          h.f = R->gbvector_copy(f);
          h.fsyz = R->gbvector_copy(polys0[*i].fsyz);

          if (false and auto_reduced)
            remainder(h, false, junk);  // This auto-reduces h.

          R->gbvector_remove_content(h.f, h.fsyz);

          T->insert(e, f->comp, INTSIZE(polys));
          polys.push_back(h);
        }
      else
        R->exponents_delete(e);
    }

  for (int i = 0; i < polys.size(); i++)
    {
      int f_lead_wt;
      gbvector *f = polys[i].f;
      int d = wt->gbvector_weight(f, f_lead_wt);
      int a = d - f_lead_wt;

      divisor_info t;
      t.g = polys[i];
      t.size = R->gbvector_n_terms(f);
      t.alpha = a;
      gb_elems.push_back(t);
    }
}

#if 0
// old code
void ReducedGB_Field_Local::minimalize(const VECTOR(POLY) &polys0,
                                       bool auto_reduced)
{
  // auto_reduced flag is ignored, since it can lead to infinite loops here
  ReducedGB_Field::minimalize(polys0,false);

  //displayElements("-- after minimize in field case -- ", R, polys, [](const POLY& g) { return g.f; } );

  for (int i=0; i<polys.size(); i++)
    {
      int f_lead_wt;
      gbvector *f = polys[i].f;
      int d = wt->gbvector_weight(f,f_lead_wt);
      int a = d - f_lead_wt;

      divisor_info t;
      t.g = polys[i];
      t.size = R->gbvector_n_terms(f);
      t.alpha = a;
      gb_elems.push_back(t);

      // gb_elems.push_back({polys[i], R->gbvector_n_terms(f), a});
    }
}
#endif

bool ReducedGB_Field_Local::find_good_divisor(
    exponents h_exp,
    int h_comp,
    int h_deg,
    int &h_alpha,         // result value
    POLY &result_g,       // result value
    int &result_g_alpha)  // result value
{
  VECTOR(MonomialTable::mon_term *) divisors;
  MonomialTable *ringtable = originalR->get_quotient_MonomialTable();

  h_alpha = h_deg - wt->exponents_weight(h_exp, h_comp);

  int n0 = (ringtable ? ringtable->find_divisors(-1, h_exp, 1, &divisors) : 0);
  int n1 = T1->find_divisors(-1, h_exp, h_comp, &divisors);
  int n2 = T->find_divisors(-1, h_exp, h_comp, &divisors);
  int n = INTSIZE(divisors);
  if (n == 0) return false;

  divisor_info *div_info = newarray(divisor_info, divisors.size());

  int next = 0;

  // ring divisors
  for (int i = 0; i < n0; i++)
    {
      int id = divisors[i]->_val;
      div_info[next++] = ring_elems[id];
    }
  // new divisors
  for (int i = 0; i < n1; i++)
    {
      int id = divisors[n0 + i]->_val;
      div_info[next++] = new_poly_elems[id];
    }
  // gb divisors
  for (int i = 0; i < n2; i++)
    {
      int id = divisors[n0 + n1 + i]->_val;
      div_info[next++] = gb_elems[id];
    }

  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << "\nfind good divisor:";
      if (n0 > 0)
        {
          o << "\n  ndivisors from quotient ring elements " << n0;
          for (int j = 0; j < n0; j++)
            {
              divisor_info &t = div_info[j];
              o << "\n    size " << t.size << " alpha " << t.alpha << " lead ";
              gbvector *f = R->gbvector_lead_term(-1, F, t.g.f);
              R->gbvector_text_out(o, F, f);
              R->gbvector_remove(f);
            }
        }
      if (n1 > 0)
        {
          o << "\n  ndivisors from appended elements " << n1;
          for (int j = 0; j < n1; j++)
            {
              divisor_info &t = div_info[n0 + j];
              o << "\n    size " << t.size << " alpha " << t.alpha << " lead ";
              gbvector *f = R->gbvector_lead_term(-1, F, t.g.f);
              R->gbvector_text_out(o, F, f);
              R->gbvector_remove(f);
            }
        }
      if (n2 > 0)
        {
          o << "\n  ndivisors from gb elements " << n1;
          for (int j = 0; j < n2; j++)
            {
              divisor_info &t = div_info[n0 + n1 + j];
              o << "\n    size " << t.size << " alpha " << t.alpha << " lead ";
              gbvector *f = R->gbvector_lead_term(-1, F, t.g.f);
              R->gbvector_text_out(o, F, f);
              R->gbvector_remove(f);
            }
        }
      emit(o.str());
    }

  // Now all of the desired elements are in div_info
  // First, find the minimum alpha value
  int min_alpha = div_info[0].alpha;
  for (int i = 1; i < n; i++)
    if (div_info[i].alpha < min_alpha) min_alpha = div_info[i].alpha;
  result_g_alpha = min_alpha;

  int min_size = -1;
  int result_i = -1;
  int nmatches = 0;
  // Now, out of the ones with this alpha, find the minimum size
  for (int i = 0; i < n; i++)
    {
      if (div_info[i].alpha == min_alpha)
        {
          int this_size = div_info[i].size;
          if (min_size < 0 || this_size < min_size)
            {
              min_size = this_size;
              result_i = i;
              nmatches = 1;
            }
          else if (this_size == min_size)
            {
              nmatches++;
            }
        }
    }

  if (nmatches > 1 && M2_gbTrace == 3)
    {
      buffer o;
      o << nmatches;
      emit_wrapped(o.str());
    }

  // At this point, result_i points to the element we wish to return
  assert(result_i >= 0);
  result_g = div_info[result_i].g;

  if (M2_gbTrace >= 4)
    {
      buffer o;
      if (nmatches > 1) o << "\n  nmatches " << n;
      o << "\n  chosen value: ";
      int size = R->gbvector_n_terms(result_g.f);
      o << "\n    size " << size << " alpha " << result_g_alpha << " lead ";
      gbvector *f = R->gbvector_lead_term(-1, F, result_g.f);
      R->gbvector_text_out(o, F, f);
      R->gbvector_remove(f);
      emit(o.str());
    }

  return true;

#if 0

  MonomialTable *ringtable = originalR->get_quotient_MonomialTable();
  if (ringtable)
    {
      n = ringtable->find_divisors(-1, h_exp, 1, &divisors);

      if (n > 0)
        {
          POLY p;
          p.fsyz = 0;
          for (int i=0; i<divisors.size(); i++)
            {
              MonomialTable::mon_term *t = divisors[i];
              int id = t->_val;
              p.f = const_cast<gbvector *>(originalR->quotient_gbvector(id));
              int g_alpha = ring_alpha[id];
              if (g_alpha <= h_alpha)
                {
                  result_g = p;
                  result_g_alpha = g_alpha;
                  return true;
                }
              if (min_alpha < 0 || g_alpha < min_alpha)
                {
                  min_alpha = g_alpha;
                  result_g = p;
                  result_g_alpha = g_alpha;
                }
            }
        }
    }
  divisors.clear();

  if (M2_gbTrace>=4)
    {
      buffer o;
      o << "\nfind good divisor:";
      emit(o.str());
    }

  // check the new polys
  n = T1->find_divisors(-1, h_exp, h_comp, &divisors);
  if (n > 0)
    {
      POLY p;
      if (M2_gbTrace>=4)
        {
          buffer o;
          o << "\n  ndivisors from appended elements " << n;
          for (int j=0; j<n; j++)
            {
              MonomialTable::mon_term *t = divisors[j];
              int id = t->_val;
              p = newpol[id];
              int g_alpha = newpol_alpha[id];
              int size = R->gbvector_n_terms(p.f);
              o << "\n    size " << size << " alpha " << g_alpha << " lead ";
              gbvector *f = R->gbvector_lead_term(-1,F,p.f);
              R->gbvector_text_out(o,F,f);
            }
          emit(o.str());
        }
      for (int i=0; i<divisors.size(); i++)
        {
          MonomialTable::mon_term *t = divisors[i];
          int id = t->_val;
          p = newpol[id];
          int g_alpha = newpol_alpha[id];
          if (result_g_alpha < 0 && g_alpha <= h_alpha)
            {
              result_g = p;
              result_g_alpha = g_alpha;
              min_alpha = g_alpha;
              //break; //return true;
            }
          if (min_alpha < 0 ||  g_alpha < min_alpha)
            {
              min_alpha = g_alpha;
              result_g = p;
              result_g_alpha = g_alpha;
            }
        }
    }
  divisors.clear();

  // Now check the GB itself
  n = T->find_divisors(-1, h_exp, h_comp, &divisors);
  if (n > 0)
    {
      POLY p;
      if (M2_gbTrace>=4)
        {
          buffer o;
          o << "\n  ndivisors from GB " << n;
          for (int j=0; j<n; j++)
            {
              MonomialTable::mon_term *t = divisors[j];
              int id = t->_val;
              p = polys[id];
              int g_alpha = alpha[id];
              int size = R->gbvector_n_terms(p.f);
              o << "\n     size " << size << " alpha " << g_alpha << " lead ";
              gbvector *f = R->gbvector_lead_term(-1,F,p.f);
              R->gbvector_text_out(o,F,f);
            }
          emit(o.str());
        }

      for (int i=0; i<divisors.size(); i++)
        {
          MonomialTable::mon_term *t = divisors[i];
          int id = t->_val;
          p = polys[id];
          int g_alpha = alpha[id];
          if (result_g_alpha < 0 && g_alpha <= h_alpha)
            {
              result_g = p;
              result_g_alpha = g_alpha;
              min_alpha = g_alpha;
              //break;
              //return true;
            }
          if (min_alpha < 0 || g_alpha < min_alpha)
            {
              min_alpha = g_alpha;
              result_g = p;
              result_g_alpha = g_alpha;
            }
        }
    }
  divisors.clear();


  if (M2_gbTrace>=4)
    {
      buffer o;
      o << "\n  chosen value: ";
      int size = R->gbvector_n_terms(result_g.f);
      o << "\n    size " << size << " alpha " << result_g_alpha << " lead ";
      gbvector *f = R->gbvector_lead_term(-1,F,result_g.f);
      R->gbvector_text_out(o,F,f);
      R->gbvector_remove(f);
      emit(o.str());
    }

  return (min_alpha >= 0);
#endif
}

void ReducedGB_Field_Local::reset_table()
{
  new_poly_elems.clear();
  delete T1;
}

void ReducedGB_Field_Local::store_in_table(const POLY &h,
                                           exponents h_exp,
                                           int h_comp,
                                           int h_alpha)
{
  int id = INTSIZE(new_poly_elems);
  divisor_info t;
  t.g.f = R->gbvector_copy(h.f);
  t.g.fsyz = R->gbvector_copy(h.fsyz);
  t.alpha = h_alpha;
  t.size = R->gbvector_n_terms(t.g.f);
  new_poly_elems.push_back(t);
  T1->insert(h_exp, h_comp, id);  // grabs h_exp
}

void ReducedGB_Field_Local::remainder(POLY &f, bool use_denom, ring_elem &denom)
{
  if (M2_gbTrace >= 4) {
      buffer o;
      text_out(o);
      emit(o.str());
    }
  if (f.f == 0) return;
  T1 = MonomialTable::make(R->n_vars());
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  exponents h_exp = R->exponents_make();
  int h_alpha, g_alpha;
  int h_deg = wt->gbvector_weight(f.f);
  while (!R->gbvector_is_zero(h.f))
    {
      if (M2_gbTrace == 3) emit_wrapped(".");
      POLY g;
      R->gbvector_get_lead_exponents(F, h.f, h_exp);
      int h_comp = h.f->comp;

      if (M2_gbTrace >= 4)
        {
          buffer o;
          o << "\nreducing ";
          R->gbvector_text_out(o, F, h.f);
          emit(o.str());
        }

      if (find_good_divisor(h_exp,
                            h_comp,
                            h_deg,
                            h_alpha,
                            g,
                            g_alpha))  // sets these three values
        {
          if (M2_gbTrace >= 4)
            {
              buffer o;
              o << "  h_alpha " << h_alpha << " g_alpha "
                << g_alpha;  // << " reducing using ";
              // R->gbvector_text_out(o,F,g.f);
              // o << newline;
              emit(o.str());
            }
          if (g_alpha > h_alpha)
            {
              if (head.next != 0)
                {
                  // In this case, we can't reduce the tail without
                  // risking an infinite loop.  So we declare ourselves done
                  // Attach the rest of h.f to frem
                  frem->next = h.f;
                  break;
                }
              // place h into T1, and store its (value,deg,alpha) values.
              // store_in_table copies h
              store_in_table(h, h_exp, h_comp, h_alpha);
              if (M2_gbTrace == 3) emit_wrapped("x");
              if (M2_gbTrace == 4) emit("\nstored result\n");
              h_deg += g_alpha - h_alpha;
              h_exp = R->exponents_make();
            }
          R->gbvector_reduce_lead_term(
              F, Fsyz, head.next, h.f, h.fsyz, g.f, g.fsyz, use_denom, denom);
        }
      else
        {
          frem->next = h.f;
          frem = frem->next;
          h.f = h.f->next;
          frem->next = 0;
        }
    }

  f.f = head.next;
  f.fsyz = h.fsyz;
  R->exponents_delete(h_exp);
  reset_table();
}

void ReducedGB_Field_Local::remainder(gbvector *&f,
                                      bool use_denom,
                                      ring_elem &denom)
{
  if (f == 0) return;

  T1 = MonomialTable::make(R->n_vars());
  gbvector *zero = 0;
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h;
  h.f = f;
  h.fsyz = NULL;
  exponents h_exp = R->exponents_make();
  int h_alpha, g_alpha;
  int h_deg = wt->gbvector_weight(f);
  while (!R->gbvector_is_zero(h.f))
    {
      if (M2_gbTrace == 3) emit_wrapped(".");
      POLY g;
      R->gbvector_get_lead_exponents(F, h.f, h_exp);
      int h_comp = h.f->comp;

      if (M2_gbTrace >= 4)
        {
          buffer o;
          o << "\nreducing ";
          R->gbvector_text_out(o, F, h.f);
          emit(o.str());
        }

      if (find_good_divisor(h_exp,
                            h_comp,
                            h_deg,
                            h_alpha,
                            g,
                            g_alpha))  // sets these three values
        {
          if (M2_gbTrace >= 4)
            {
              buffer o;
              o << "  h_alpha " << h_alpha << " g_alpha "
                << g_alpha;  // << " reducing using ";
              // R->gbvector_text_out(o,F,g.f);
              // o << newline;
              emit(o.str());
            }
          if (g_alpha > h_alpha)
            {
              if (head.next != 0)
                {
                  // In this case, we can't reduce the tail without
                  // risking an infinite loop.  So we declare ourselves done
                  // Attach the rest of h.f to frem
                  frem->next = h.f;
                  break;
                }
              // place h into T1, and store its (value,deg,alpha) values.
              store_in_table(h, h_exp, h_comp, h_alpha);
              if (M2_gbTrace == 3) emit_wrapped("x");
              if (M2_gbTrace == 4) emit("\nstored result\n");
              h_deg += g_alpha - h_alpha;
              h_exp = R->exponents_make();
            }
          R->gbvector_reduce_lead_term(
              F, Fsyz, head.next, h.f, zero, g.f, zero, use_denom, denom);
        }
      else
        {
          frem->next = h.f;
          frem = frem->next;
          h.f = h.f->next;
          frem->next = 0;
        }
    }

  f = head.next;
  R->exponents_delete(h_exp);
  reset_table();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
