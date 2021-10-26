// Copyright 1997  Michael E. Stillman

#include "style.hpp"
#include "res-a2.hpp"
#include "hilb.hpp"
#include "text-io.hpp"
#include "comp-gb.hpp"
#include "matrix-con.hpp"
#include "interrupted.hpp"

void gb2_comp::setup(FreeModule *FFsyz,
                     stash *mi_stash0,
                     gb_node *ggens,
                     int lodeg,
                     int origsyz,
                     int lev,
                     int strategy)
{
  level = lev;
  int i;
  originalR = FFsyz->get_ring()->cast_to_PolynomialRing();
  if (originalR == NULL)
    {
      ERROR("internal error - ring is not a polynomial ring");
      assert(0);
    }
  GR = originalR->get_gb_ring();
  M = GR->get_flattened_monoid();
  K = GR->get_flattened_coefficients();

  mi_stash = mi_stash0;

  F = const_cast<FreeModule *>(ggens->output_free_module());

  gens = ggens;
  syz = NULL;

  Fsyz = const_cast<FreeModule *>(FFsyz);

  spairs = new s_pair_heap(M);

  n_gb = n_mingens = n_subring = 0;
  n_gb_first = 0;
  n_pairs = n_pairs_computed = 0;
  n_pairs_syz = n_pairs_usyz = 0;
  n_pairs_gb = n_pairs_zero = 0;
  n_pairs_gcd = n_pairs_hilb = 0;

  orig_syz = origsyz;  // Note: if orig_syz > 0, then Fsyz is
                       // completely set already.

  this_degree = lodeg;

  // We index into this using gbvector components (which are one greater than
  // the
  // FreeModule components
  monideals.push_back(0);
  for (i = 0; i < F->rank(); i++)
    {
      monideal_pair *p = new monideal_pair(originalR, mi_stash);
      monideals.push_back(p);
    }

  use_hilb = 0;
  hf_numgens_F = F->rank();
  hf_numgens_gb = 0;  // This will enable the initial computation of HF,
                      // if use_hilb is set.
  hf = hilb_comp::hilbertNumerator(F);
  n_gb_syz = 0;

  strategy_flags = strategy;
  state = STATE_GENS;
}

gb2_comp::gb2_comp(FreeModule *Fsyz0,
                   stash *mi_stash0,
                   gb_node *gens0,
                   int lodegree,
                   int origsyz,
                   int level0,
                   int strat)
{
  setup(Fsyz0, mi_stash0, gens0, lodegree, origsyz, level0, strat);
}
void gb2_comp::set_output(gb_node *p)
{
  syz = p;
  use_hilb = (strategy_flags & STRATEGY_USE_HILB) && (syz != NULL);
}
void gb2_comp::remove_pair(s_pair *&p)
{
  p->f = 0;  // these are not used in res-a2-gb algorithm except for temp space
  p->fsyz = 0;  // ditto
  p->first = NULL;
  p->second = NULL;
  p->next = NULL;
  M->remove(p->lcm);
  freemem(p);
  p = NULL;
}

gb2_comp::~gb2_comp()
{
  // remove all of the gbvector's in the gb_elems's and spairs.
  for (int i = 0; i < gb.size(); i++)
    {
      gb_elem *g = gb[i];
      GR->gbvector_remove(g->f);
      GR->gbvector_remove(g->fsyz);
    }
  these_pairs = 0;
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////

s_pair *gb2_comp::new_ring_pair(gb_elem *p, const int *lcm)
{
  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_RING;
  result->degree = M->primary_degree(lcm) + F->primary_degree(p->f->comp - 1);
  result->compare_num = 0;
  result->first = p;
  result->second = NULL;
  result->f = NULL;
  result->fsyz = NULL;

  result->lcm = M->make_new(lcm);
  return result;
}

s_pair *gb2_comp::new_s_pair(gb_elem *p, gb_elem *q, const int *lcm)
{
  // p and q should have 'f' field defined.
  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_PAIR;
  result->degree = M->primary_degree(lcm) + F->primary_degree(p->f->comp - 1);
  result->compare_num = 0;
  result->first = p;
  result->second = q;
  result->f = NULL;
  result->fsyz = NULL;

  result->lcm = M->make_new(lcm);
  return result;
}

//////////////////////////////////////////////
//  sorting the Groebner basis ///////////////
//////////////////////////////////////////////

int gb2_comp::gb_sort_partition(int lo, int hi)
{
  gb_elem *pivot = gb[lo];
  const int *pivot_monom = pivot->f->monom;
  int i = lo - 1;
  int j = hi + 1;
  for (;;)
    {
      do
        {
          j--;
        }
      while (M->compare(gb[j]->f->monom, pivot_monom) < 0);
      do
        {
          i++;
        }
      while (M->compare(gb[i]->f->monom, pivot_monom) > 0);

      if (i < j)
        {
          gb_elem *tmp = gb[j];
          gb[j] = gb[i];
          gb[i] = tmp;
        }
      else
        return j;
    }
}

void gb2_comp::gb_sort(int lo, int hi)
{
  if (lo < hi)
    {
      int q = gb_sort_partition(lo, hi);
      gb_sort(lo, q);
      gb_sort(q + 1, hi);
    }
}

void gb2_comp::find_pairs(gb_elem *p)
// compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
// (includes cases m * lead(p) = 0).
// Returns a list of new s_pair's.
{
  queue<Bag *> elems;
  Index<MonomialIdeal> j;
  intarray vplcm;
  int *find_pairs_m = M->make_one();
  int *f_m = M->make_one();
  int *find_pairs_lcm = newarray_atomic(int, M->n_vars());

  GR->gbvector_get_lead_monomial(F, p->f, f_m);
  if (GR->is_skew_commutative())
    {
      int *find_pairs_exp = newarray_atomic(int, M->n_vars());
      M->to_expvector(f_m, find_pairs_exp);

      for (int v = 0; v < GR->n_skew_commutative_vars(); v++)
        {
          int w = GR->skew_variable(v);
          if (find_pairs_exp[w] == 0) continue;

          find_pairs_exp[w]++;
          M->from_expvector(find_pairs_exp, find_pairs_lcm);
          find_pairs_exp[w]--;

          vplcm.shrink(0);
          M->to_varpower(find_pairs_lcm, vplcm);
          s_pair *q = new_ring_pair(p, find_pairs_lcm);
          elems.insert(new Bag(q, vplcm));
        }
      freemem(find_pairs_exp);
    }

  // Add in syzygies arising from a base ring

  if (originalR->is_quotient_ring())
    {
      for (int i = 0; i < originalR->n_quotients(); i++)
        {
          const Nterm *f = originalR->quotient_element(i);
          M->lcm(f->monom, f_m, find_pairs_lcm);
          vplcm.shrink(0);
          M->to_varpower(find_pairs_lcm, vplcm);
          s_pair *q = new_ring_pair(p, find_pairs_lcm);
          elems.insert(new Bag(q, vplcm));
        }
    }

  // Add in syzygies arising as s-pairs
  MonomialIdeal *mi1 = monideals[p->f->comp]->mi;
  for (Index<MonomialIdeal> i = mi1->first(); i.valid(); i++)
    {
      M->from_varpower((*mi1)[i]->monom().raw(), find_pairs_m);
      M->lcm(find_pairs_m, f_m, find_pairs_lcm);
      vplcm.shrink(0);
      M->to_varpower(find_pairs_lcm, vplcm);
      s_pair *q =
          new_s_pair(p,
                     reinterpret_cast<gb_elem *>((*mi1)[i]->basis_ptr()),
                     find_pairs_lcm);
      elems.insert(new Bag(q, vplcm));
    }

  // Add 'p' to the correct monideal
  intarray vp;
  M->to_varpower(f_m, vp);
  mi1->insert(new Bag(p, vp));

  // Now minimalize these elements, and insert them into
  // the proper degree.

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal *mi = new MonomialIdeal(originalR, elems, rejects, mi_stash);
  while (rejects.remove(b))
    {
      s_pair *q = reinterpret_cast<s_pair *>(b->basis_ptr());
      remove_pair(q);
      delete b;
    }

  int is_ideal2 = (F->rank() == 1 && orig_syz == 0);
  for (j = mi->first(); j.valid(); j++)
    {
      n_pairs++;
      s_pair *q = reinterpret_cast<s_pair *>((*mi)[j]->basis_ptr());
      if (is_ideal2 && q->syz_type == SPAIR_PAIR)
        {
          // MES: the following line is suspect, for Schreyer orders
          M->gcd(q->first->f->monom, q->second->f->monom, find_pairs_m);
          if (M->is_one(find_pairs_m))
            {
              n_pairs_gcd++;
              if (M2_gbTrace >= 8)
                {
                  buffer o;
                  o << "removed pair[" << q->first->me << " " << q->second->me
                    << "]" << newline;
                  emit(o.str());
                }
              remove_pair(q);
            }
          else
            spairs->insert(q);
        }
      else
        spairs->insert(q);
    }

  delete mi;
  // Remove the local variables
  M->remove(find_pairs_m);
  M->remove(f_m);
  freemem(find_pairs_lcm);
}

void gb2_comp::compute_s_pair(s_pair *p)
{
  if (p->f == NULL)
    {
      int *s = M->make_one();
      GR->gbvector_get_lead_monomial(F, p->first->f, s);
      M->divide(p->lcm, s, s);

      GR->gbvector_mult_by_term(
          F, Fsyz, GR->one(), s, p->first->f, p->first->fsyz, p->f, p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
        GR->gbvector_reduce_lead_term(
            F, Fsyz, 0, p->f, p->fsyz, p->second->f, p->second->fsyz);
      M->remove(s);
    }
}

void gb2_comp::gb_reduce(gbvector *&f, gbvector *&fsyz)
{
  if ((strategy_flags & STRATEGY_LONGPOLYNOMIALS) != 0)
    {
      gb_geo_reduce(f, fsyz);
      return;
    }
  gbvector head;
  gbvector *result = &head;
  result->next = 0;

  int *div_totalexp = newarray_atomic(int, M->n_vars());
  int count = 0;
  if (M2_gbTrace == 10)
    {
      buffer o;
      o << "reducing ";
      GR->gbvector_text_out(o, F, f);
      emit_line(o.str());
    }
  while (f != NULL)
    {
      Bag *b;
      GR->gbvector_get_lead_exponents(F, f, div_totalexp);
      if (originalR->is_quotient_ring() &&
          originalR->get_quotient_monomials()->search_expvector(div_totalexp,
                                                                b))
        {
          const gbvector *g = originalR->quotient_gbvector(b->basis_elem());
          GR->gbvector_reduce_lead_term(F, Fsyz, head.next, f, fsyz, g, 0);
          count++;
        }
      else if (monideals[f->comp]->mi_search->search_expvector(div_totalexp, b))
        {
          gb_elem *q = reinterpret_cast<gb_elem *>(b->basis_ptr());
          GR->gbvector_reduce_lead_term(
              F, Fsyz, head.next, f, fsyz, q->f, q->fsyz);
          count++;
          if (M2_gbTrace == 10)
            {
              buffer o;
              o << "  reduced by ";
              GR->gbvector_text_out(o, F, q->f);
              o << newline;
              o << "    giving ";
              GR->gbvector_text_out(o, F, f);
              o << newline;
              emit(o.str());
            }
        }
      else
        {
          result->next = f;
          f = f->next;
          result = result->next;
          result->next = 0;
        }
    }

  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << "." << count;
      emit_wrapped(o.str());
    }
  f = head.next;
  freemem(div_totalexp);
}

void gb2_comp::gb_geo_reduce(gbvector *&f, gbvector *&fsyz)
{
  gbvector head;
  gbvector *result = &head;
  result->next = 0;

  int *div_totalexp = newarray_atomic(int, M->n_vars());
  int count = 0;

  gbvectorHeap fb(GR, F);
  gbvectorHeap fsyzb(GR, Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);
  const gbvector *lead;
  while ((lead = fb.get_lead_term()) != NULL)
    {
      Bag *b;
      GR->gbvector_get_lead_exponents(F, lead, div_totalexp);
      if (originalR->is_quotient_ring() &&
          originalR->get_quotient_monomials()->search_expvector(div_totalexp,
                                                                b))
        {
          const gbvector *g = originalR->quotient_gbvector(b->basis_elem());
          GR->reduce_lead_term_heap(F,
                                    Fsyz,
                                    lead,
                                    div_totalexp,  // are these two needed
                                    head.next,
                                    fb,
                                    fsyzb,
                                    g,
                                    0);
          count++;
        }
      else if (monideals[lead->comp]->mi_search->search_expvector(div_totalexp,
                                                                  b))
        {
          gb_elem *q = reinterpret_cast<gb_elem *>(b->basis_ptr());
          GR->reduce_lead_term_heap(
              F, Fsyz, lead, div_totalexp, head.next, fb, fsyzb, q->f, q->fsyz);
          count++;
        }
      else
        {
          result->next = fb.remove_lead_term();
          result = result->next;
          result->next = 0;
        }
    }

  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << "." << count;
      emit_wrapped(o.str());
    }
  f = head.next;

  fsyz = fsyzb.value();
  freemem(div_totalexp);
}

void gb2_comp::reduce(gbvector *&f, gbvector *&fsyz) { gb_reduce(f, fsyz); }
void gb2_comp::flush_pairs()
{
  while (these_pairs != NULL)
    {
      n_pairs_hilb++;
      s_pair *p = these_pairs;
      these_pairs = p->next;
      remove_pair(p);
    }
}

#ifdef DEVELOPMENT
#warning " schreyer append: wrong if not encoded order"
#endif
void gb2_comp::schreyer_append(gbvector *f)
{
  if (orig_syz < 0)
    {
      int *d = originalR->degree_monoid()->make_one();
      GR->gbvector_multidegree(F, f, d);
      Fsyz->append_schreyer(d, f->monom, Fsyz->rank());
      originalR->degree_monoid()->remove(d);
    }
}

void gb2_comp::gb_insert(gbvector *f, gbvector *fsyz, int ismin)
{
  int *f_m = M->make_one();
  gbvector *bull = NULL;
  gb_elem *p = new gb_elem(f, fsyz, ismin);

  if (orig_syz < 0 && ismin)
    GR->gbvector_remove_content(p->f, bull);
  else
    GR->gbvector_remove_content(p->f, p->fsyz);

  if (M2_gbTrace >= 10)
    {
      buffer o;
      o << "inserting level " << level << " ";
      GR->gbvector_text_out(o, F, p->f);
      o << newline;
      emit(o.str());
    }
  if (ismin) n_mingens++;
  GR->gbvector_get_lead_monomial(F, p->f, f_m);

  if (M->in_subring(1, f_m)) n_subring++;
  // insert into p->f->comp->mi_search
  intarray vp;
  M->to_varpower(f_m, vp);
  monideals[p->f->comp]->mi_search->insert(new Bag(p, vp));
  gb.push_back(p);

  M->remove(f_m);

  // Now do auto-reduction of previous elements using this one.
  if (orig_syz >= 0 || !ismin)
    for (int i = n_gb_first; i < n_gb; i++)
      {
        // Now compute gb(i) := gb(i) - c gb(j), where
        // c in(gb(j)) is a term in gb(i).
        // Also compute change(i) -= c change(j).

        GR->gbvector_auto_reduce(F, Fsyz, gb[i]->f, gb[i]->fsyz, p->f, p->fsyz);
      }

  n_gb++;
}

int gb2_comp::get_pairs()
{
  int n = 0;
  s_pair head;
  s_pair *slast = &head;

  for (;;)
    {
      s_pair *p = spairs->remove();
      if (p == NULL) break;
      if (p->degree != this_degree)
        {
          spairs->put_back(p);
          break;
        }

      slast->next = p;
      slast = p;
      n++;
    }

  slast->next = NULL;
  these_pairs = head.next;
  total_pairs.append(this_degree);
  total_pairs.append(n);
  return n;
}
bool gb2_comp::s_pair_step()
// If no s-pairs left in the current degree,
// return false
// Otherwise, compute the current s-pair, reduce it, and
// dispatch the result.  Return true.
{
  if (use_hilb && n_gb_syz == 0) flush_pairs();
  if (these_pairs == NULL) return false;  // Done
  s_pair *p = these_pairs;
  these_pairs = these_pairs->next;

  n_pairs_computed++;
  compute_s_pair(p);

  gbvector *f = p->f;
  gbvector *fsyz = p->fsyz;
  p->f = NULL;
  p->fsyz = NULL;
  remove_pair(p);

  gb_reduce(f, fsyz);
  if (f != NULL)
    {
      gb_insert(f, fsyz, 0);
      n_gb_syz--;
      n_pairs_gb++;
      if (M2_gbTrace >= 3) emit_wrapped("m");
    }
  else if (fsyz != NULL && syz != NULL)
    {
      if (syz->receive_generator(fsyz, n_syz++, GR->one()))
        {
          n_gb_syz--;
          n_pairs_syz++;
          if (M2_gbTrace >= 3) emit_wrapped("z");
        }
      else
        {
          n_pairs_usyz++;
          if (M2_gbTrace >= 3) emit_wrapped("u");
        }
    }
  else
    {
      if (fsyz != NULL) GR->gbvector_remove(fsyz);
      n_pairs_zero++;
      if (M2_gbTrace >= 3) emit_wrapped("o");
    }
  return true;
}

///////////////////////////
// Hilbert function use ///
///////////////////////////

//---- Completion testing -----------------------------

bool gb2_comp::receive_generator(gbvector *f, int n, const ring_elem denom)
{
  bool isgen = false;
  // It is our duty to free 'f'...

  for (int i = monideals.size(); i <= F->rank(); i++)
    {
      monideal_pair *p = new monideal_pair(originalR, mi_stash);
      monideals.push_back(p);
    }

  gbvector *fsyz = NULL;
  if (orig_syz >= 0)
    {
      if (orig_syz > n)
        fsyz = GR->gbvector_term(
            Fsyz, denom, n + 1);  // Note the change in component number
      gb_reduce(f, fsyz);
      if (f == NULL)
        {
          if (fsyz != NULL && syz != NULL)
            syz->receive_generator(fsyz, n_syz++, GR->one());
        }
      else
        {
          isgen = true;
          gb_insert(f, fsyz, 1);
        }
    }
  else
    {
      gb_reduce(f, fsyz);
      GR->gbvector_remove(fsyz);
      GR->gbvector_remove_content(f, NULL);
      if (f != NULL)
        {
          // schreyer_append(f);
          isgen = true;
          // gb_insert(f,Fsyz->e_sub_i(n_mingens),1);
          // The fsyz part will be set at the end of the degree,
          // after sorting takes place, and after auto-reduction in
          // this degree.
          gb_insert(f, NULL, 1);
        }
    }
  return isgen;
}

void gb2_comp::end_degree()
{
  if ((strategy_flags & STRATEGY_SORT) != 0)
    {
      gb_sort(n_gb_first, n_gb - 1);  // This is the range of elements to sort.
    }
  for (int j = n_gb_first; j < n_gb; j++) gb[j]->me = j;
  if (orig_syz < 0)
    {
      for (int j = n_gb_first; j < n_gb; j++)
        if (gb[j]->is_min)
          {
            schreyer_append(gb[j]->f);
            gb[j]->fsyz = GR->gbvector_term(Fsyz, GR->one(), Fsyz->rank());
          }
    }

  // Now set the state so that we know we have finished here
  this_degree++;
  state = STATE_NEW_DEGREE;
}

enum ComputationStatusCode gb2_comp::calc_gb(int deg)
{
  enum ComputationStatusCode ret = COMP_DONE;
  if (this_degree > deg) return COMP_DONE;
  if (state == STATE_DONE) return COMP_DONE;  // This includes knowledge
  // that there will be no new generators.
  if (this_degree < deg)
    {
      // Now make sure that previous computations have been done:
      ret = calc_gens(deg - 1);
      if (ret != COMP_DONE) return ret;
    }
  // At this point, we have completely computed a GB with new gens
  // in this degree.  Depending on whether we stopped
  // prematurely, our state will be one of STATE_NEW_DEGREE,
  // STATE_GB, STATE_GENS.

  buffer o1;

  if (state == STATE_NEW_DEGREE)
    {
      if (use_hilb)
        {
          RingElement *hsyz;
          const PolynomialRing *DR = originalR->get_degree_ring();
          RingElement *h = RingElement::make_raw(DR, DR->zero());
          if (syz != NULL)
            {
              hsyz = syz->hilbertNumerator();
              if (hsyz == 0) return COMP_INTERRUPTED;
              //              o1 << "hsyz = "; hsyz->text_out(o);
              h = (*h) + (*hsyz);
            }
          RingElement *hf1 = hilbertNumerator();
          if (hf1 == 0) return COMP_INTERRUPTED;
          h = (*h) + (*hf1);
          RingElement *hF = hilb_comp::hilbertNumerator(F);
          if (hF == 0) return COMP_INTERRUPTED;
          h = (*h) - (*hF);

#if 0
//        o1 << "\nhf   = "; hf1->text_out(o1);
//        o1 << "\nhF   = "; hF->text_out(o1);
//        o1 << "\nh    = "; h->text_out(o1);
//        o1 << "\n";
//        emit(o1.str());
//        o1.reset();
#endif
          if (M2_gbTrace >= 1 && n_gb_syz != 0)
            {
              buffer o;
              o << "<WARNING: remaining nsyz+ngb = " << n_gb_syz << ">";
              emit(o.str());
            }

          n_gb_syz = hilb_comp::coeff_of(h, this_degree);
          if (error()) return COMP_ERROR;
        }

      // Compute new s-pairs
      for (int i = n_gb_first; i < n_gb; i++) find_pairs(gb[i]);

      state = STATE_GB;
      n_gb_first = n_gb;
      int npairs = get_pairs();
      if (M2_gbTrace >= 1 && npairs > 0)
        {
          buffer o;
          // Should only display this if there are some pairs.
          o << '[' << level << ',' << npairs;
          if (use_hilb) o << ",e" << n_gb_syz;  // << "," << n1 << "," << n2;
          o << ']';
          emit(o.str());
        }
    }

  if (state == STATE_GB)
    {
      ret = COMP_COMPUTING;
      // Now compute all the s-pairs here.
      for (;;)
        {
          if (ret != COMP_COMPUTING) break;
          if (system_interrupted())
            {
              ret = COMP_INTERRUPTED;
              break;
            }
          // Check ending conditions...
          if (!s_pair_step())
            {
              ret = COMP_DONE;
              state = STATE_GENS;
            }
        }
    }

  if (state == STATE_GENS && orig_syz > 0)
    {
      // Get the generators of the same degree, since these
      // may produce syzygies of this degree.
      ret = gens->calc_gb(deg);
      if (ret == COMP_DONE)
        {
          // Cleanup, go to next degree.
          end_degree();
        }
    }
  // MES: put out an endl if M2_gbTrace >= 1?

  // Is this where we should compute the HF again: for use by the
  // previous node... Do we really have to compute it twice per degree/level
  // node?

  return ret;
}

enum ComputationStatusCode gb2_comp::calc_gens(int deg)
{
  enum ComputationStatusCode ret;
  // First check whether we have done this:
  if (this_degree > deg) return COMP_DONE;

  // First make sure that we have a GB here:
  ret = calc_gb(deg);
  if (ret != COMP_DONE) return ret;

  // Go get the generators:
  ret = gens->calc_gb(deg);
  if (ret != COMP_DONE) return ret;

  end_degree();
  return COMP_DONE;
}

bool gb2_comp::is_done()
{
  return ((spairs->n_elems() == 0) && n_gb == n_gb_first);
}

////////////////////////////////
// Hilbert function computing //
////////////////////////////////

Matrix *gb2_comp::make_lead_term_matrix()
{
  MatrixConstructor result(F, gb.size());
  for (int i = 0; i < gb.size(); i++)
    {
      gb_elem *g = gb[i];
      gbvector *f = g->f;
      if (f == 0) continue;
      // Only grab the lead term, which should be non-null
      gbvector *fnext = f->next;
      f->next = 0;
      vec v = originalR->translate_gbvector_to_vec(F, f);
      f->next = fnext;
      result.set_column(i, v);
    }
  return result.to_matrix();
}

RingElement /* or null */ *gb2_comp::hilbertNumerator()
{
  if (hf_numgens_gb == n_gb && hf_numgens_F == F->rank())
    {
      return hf;
    }

  Matrix *hf_matrix = make_lead_term_matrix();
  hf = hilb_comp::hilbertNumerator(hf_matrix);

  // hf is NULL if the computation was interrupted
  if (hf == 0) return 0;

  hf_numgens_F = F->rank();
  hf_numgens_gb = n_gb;

  return hf;
}

//--- Obtaining matrices as output -------
Matrix *gb2_comp::min_gens_matrix()
{
  MatrixConstructor mat(F, Fsyz, 0);
  int j = 0;
  for (int i = 0; i < gb.size(); i++)
    if (gb[i]->is_min)
      mat.set_column(j++, originalR->translate_gbvector_to_vec(F, gb[i]->f));
  return mat.to_matrix();
}
Matrix *gb2_comp::get_matrix()
{
  if (orig_syz > 0)
    return gens->get_matrix();
  else
    return min_gens_matrix();
}

Matrix *gb2_comp::initial_matrix(int n)
{
  MatrixConstructor mat(F, 0);
  for (int i = 0; i < gb.size(); i++)
    {
      gbvector *tmp = GR->gbvector_lead_term(n, F, gb[i]->f);
      mat.append(originalR->translate_gbvector_to_vec(F, tmp));
      GR->gbvector_remove(tmp);
    }
  return mat.to_matrix();
}

Matrix *gb2_comp::gb_matrix()
{
  MatrixConstructor mat(F, 0);
  for (int i = 0; i < gb.size(); i++)
    mat.append(originalR->translate_gbvector_to_vec(F, gb[i]->f));
  return mat.to_matrix();
}

Matrix *gb2_comp::change_matrix()
{
  MatrixConstructor mat(Fsyz, 0);
  for (int i = 0; i < gb.size(); i++)
    mat.append(originalR->translate_gbvector_to_vec(Fsyz, gb[i]->fsyz));
  return mat.to_matrix();
}

void gb2_comp::debug_out(s_pair *q) const
{
  buffer o;
  debug_out(o, q);
  emit(o.str());
}
void gb2_comp::debug_out(buffer &o, s_pair *q) const
{
  if (q == NULL) return;
  o << "(" << q->compare_num << " ";
  if (q->first != NULL)
    o << q->first->me;
  else
    o << ".";
  o << " ";
  if (q->second != NULL)
    o << q->second->me;
  else
    o << ".";
  o << " ";
  M->elem_text_out(o, q->lcm);
  o << ") ";
}

// text_out and stats are the same routine, except that text_out returns
// a string, essentially, and stats displays its result directly.
void gb2_comp::text_out(buffer &o) const
{
  if (M2_gbTrace >= 4 && n_gb > 0)
    {
      int nmonoms = 0;
      int nchange = 0;
      for (int i = 0; i < gb.size(); i++)
        {
          nmonoms += GR->gbvector_n_terms(gb[i]->f);
          nchange += GR->gbvector_n_terms(gb[i]->fsyz);
        }
      o.put(n_gb, 5);
      o.put(" ");
      o.put(n_pairs, 5);
      o.put(" ");
      o.put(n_pairs_computed, 5);
      o.put(" ");
      o.put(n_pairs_gb, 5);
      o.put(" ");
      o.put(n_pairs_syz, 5);
      o.put(" ");
      o.put(n_pairs_zero, 5);
      o.put(" ");
      o.put(n_pairs_usyz, 5);
      o.put(" ");
      o.put(n_pairs_hilb, 5);
      o.put(" ");
      o.put(n_pairs_gcd, 5);
      o.put(" ");
      o.put(nmonoms, 5);
      o.put(" ");
      o.put(nchange, 5);
      o.put(newline);
    }

  spairs->text_out(o);
  if (M2_gbTrace >= 5 && M2_gbTrace % 2 == 1)
    for (int i = 0; i < gb.size(); i++)
      {
        o << i << '\t';
        GR->gbvector_text_out(o, F, gb[i]->f);
        o << newline;
      }
}

void gb2_comp::stats() const
{
  buffer o;
  if (M2_gbTrace >= 4 && n_gb > 0)
    {
      int nmonoms = 0;
      int nchange = 0;
      for (int i = 0; i < gb.size(); i++)
        {
          nmonoms += GR->gbvector_n_terms(gb[i]->f);
          nchange += GR->gbvector_n_terms(gb[i]->fsyz);
        }
      o.put(n_gb, 5);
      o.put(" ");
      o.put(n_pairs, 5);
      o.put(" ");
      o.put(n_pairs_computed, 5);
      o.put(" ");
      o.put(n_pairs_gb, 5);
      o.put(" ");
      o.put(n_pairs_syz, 5);
      o.put(" ");
      o.put(n_pairs_zero, 5);
      o.put(" ");
      o.put(n_pairs_usyz, 5);
      o.put(" ");
      o.put(n_pairs_hilb, 5);
      o.put(" ");
      o.put(n_pairs_gcd, 5);
      o.put(" ");
      o.put(nmonoms, 5);
      o.put(" ");
      o.put(nchange, 5);
      o.put(newline);
      emit(o.str());
      o.reset();
    }

  spairs->stats();
  if (M2_gbTrace >= 5 && M2_gbTrace % 2 == 1)
    {
      o << "free module is ";
      F->text_out(o);
      o << newline;
      for (int i = 0; i < gb.size(); i++)
        {
          o.reset();
          o << i << '\t';
          GR->gbvector_text_out(o, F, gb[i]->f);
          o << newline;
          emit(o.str());
        }
    }
  //  dfree(F);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
