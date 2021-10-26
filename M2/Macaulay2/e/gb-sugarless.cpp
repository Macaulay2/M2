// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "gb-sugarless.hpp"
#include "text-io.hpp"
#include "matrix-con.hpp"
#include "gbweight.hpp"
#include "interrupted.hpp"

// is_min field of gb_elem:
//  0 not a mingen (produced by an spair), not minimal gb elem
//  1 a mingen (trimmed gen), but not minimal gb elem
//  2 not mingen, but is a minimal gb element
//  3 mingen and minimal gb elem

static const int MINGEN_MASK = 0x1;
static const int MINGB_MASK = 0x2;

void GBinhom_comp::set_up0(const Matrix *m,
                           int csyz,
                           int nsyz,
                           M2_arrayint gb_weights)
{
  int i;
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      ERROR("ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  originalR = R;
  GR = R->get_gb_ring();
  weightInfo_ = new GBWeight(m->rows(), gb_weights);
  M = GR->get_flattened_monoid();
  K = GR->get_flattened_coefficients();

  spairs = new s_pair_heap(M);

  gb = gbLarge = new gb_elem;  // List head for the GB computed so far
  gb->next = NULL;             // (both minimal, and large GB's)
  gb->next_min = NULL;

  if (nsyz < 0 || nsyz > m->n_cols()) nsyz = m->n_cols();
  n_comps_per_syz = nsyz;

  F = m->rows();

  n_gb = n_subring = 0;
  n_pairs = n_computed = 0;
  last_gb_num = 0;
  n_saved_gcd = n_saved_lcm = 0;

  collect_syz = csyz;
  is_ideal = (F->rank() == 1 && csyz == 0);
  if (GR->is_weyl_algebra()) is_ideal = false;
  need_resize = 0;

  for (i = 0; i < F->rank(); i++) monideals.push_back(new MonomialIdeal(R));
}

void GBinhom_comp::set_up(const Matrix *m,
                          int csyz,
                          int nsyz,
                          M2_arrayint gb_weights,
                          int strat)
{
  strategy = strat;
  set_up0(m, csyz, nsyz, gb_weights);

  Fsyz = m->cols()->sub_space(n_comps_per_syz);

  minimal_gb = ReducedGB::create(originalR, F, Fsyz);
  minimal_gb_valid = true;

  syz = MatrixConstructor(Fsyz, 0);
  n_syz = 0;
  add_gens(0, m->n_cols() - 1, m);
}

void GBinhom_comp::inter_reduce(gb_elem *& /*gens*/)
{
  // MES
}

void GBinhom_comp::add_gens(int lo, int hi, const Matrix *m)
{
  // MES
  // First incorporate the new generators.
  // 2. inter-reduce them, so they have different lead terms
  // 3. insert them into gb, gbLarge
  // 4. each insertion will also call find_pairs.

  // MES: should we inter-reduce these first?  Does it matter?
  for (int i = hi; i >= lo; i--)
    {
      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(F, (*m)[i], denom);
      s_pair *p = new_gen(i, f, denom);
      if (p != NULL) spairs->insert(p);
      n_pairs++;
    }
}

GBinhom_comp *GBinhom_comp::create(const Matrix *m,
                                   M2_bool collect_syz,
                                   int n_rows_to_keep,
                                   M2_arrayint gb_weights,
                                   int strategy,
                                   M2_bool use_max_degree_limit,
                                   int max_degree_limit)
{
  const PolynomialRing *P = m->get_ring()->cast_to_PolynomialRing();
  if (P == 0 || P->getCoefficients()->is_ZZ())
    {
      ERROR("expected polynomial ring over a field");
      return 0;
    }
  GBinhom_comp *result =
      new GBinhom_comp(m, collect_syz, n_rows_to_keep, gb_weights, strategy);
  return result;
}

GBinhom_comp::GBinhom_comp(const Matrix *m,
                           int csyz,
                           int nsyz,
                           M2_arrayint gb_weights,
                           int strat)
{
  set_up(m, csyz, nsyz, gb_weights, strat);
}

void GBinhom_comp::remove_pair(s_pair *&p)
{
  GR->gbvector_remove(p->f);
  GR->gbvector_remove(p->fsyz);
  p->first = NULL;
  p->second = NULL;
  p->next = NULL;
  M->remove(p->lcm);
  freemem(p);
  p = NULL;
}

GBinhom_comp::~GBinhom_comp() {}
void GBinhom_comp::resize(int /*nbits*/)
// Resizes all (packed) monomials, and polynomials
// to work in at least the next degree.
{
  // MES
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////

s_pair *GBinhom_comp::new_var_pair(gb_elem *p, const int *lcm)
{
  return new_ring_pair(p, lcm);
}

s_pair *GBinhom_comp::new_ring_pair(gb_elem *p, const int *lcm)
{
  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_RING;
  result->degree = weightInfo_->monomial_weight(
      lcm,
      p->f->comp);  // M->primary_degree(lcm) + F->primary_degree(p->f->comp-1);
  result->compare_num = 0;
  result->first = p;
  result->second = NULL;
  result->f = NULL;
  result->fsyz = NULL;

  result->lcm = M->make_new(lcm);
  return result;
}

s_pair *GBinhom_comp::new_s_pair(gb_elem *p, gb_elem *q, const int *lcm)
{
  // p and q should have 'f' field defined.
  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_PAIR;
  result->degree = weightInfo_->monomial_weight(
      lcm,
      p->f->comp);  // M->primary_degree(lcm) + F->primary_degree(p->f->comp-1);
  result->compare_num = 0;
  result->first = p;
  result->second = q;
  result->f = NULL;
  result->fsyz = NULL;

  result->lcm = M->make_new(lcm);
  return result;
}

s_pair *GBinhom_comp::new_gen(int i, gbvector *f, ring_elem denom)
{
  gbvector *fsyz;

  if (i < n_comps_per_syz)
    fsyz = GR->gbvector_term(Fsyz, denom, i + 1);
  else
    fsyz = GR->gbvector_zero();

  if (GR->gbvector_is_zero(f))
    {
      if (!GR->gbvector_is_zero(fsyz))
        {
          vec fsyzvec = originalR->translate_gbvector_to_vec(Fsyz, fsyz);
          n_syz++;
          syz.append(fsyzvec);
        }
      return NULL;
    }

  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_GEN;
  result->degree = weightInfo_->gbvector_weight(f);
  result->compare_num = 0;
  result->first = NULL;
  result->second = NULL;
  result->f = f; /* NOTE THAT WE GRAB f */
  result->fsyz = fsyz;

  result->lcm = M->make_new(result->f->monom);

  return result;
}

int GBinhom_comp::mark_pair(gb_elem *p, gb_elem *q) const
{
  s_pair *r;
  for (r = p->pair_list; r != NULL; r = r->next_same)
    if (r->second == q)
      {
        if (r->compare_num >= 0)
          {
            r->compare_num = -1;
            if (M2_gbTrace >= 8)
              {
                buffer o;
                o << "---- removed pair ";
                debug_out(o, r);
                emit_line(o.str());
              }
            return 1;
          }
        else
          return 0;
      }
  for (r = q->pair_list; r != NULL; r = r->next_same)
    if (r->second == p)
      {
        if (r->compare_num >= 0)
          {
            r->compare_num = -1;
            if (M2_gbTrace >= 8)
              {
                buffer o;
                o << "---- removed pair ";
                debug_out(o, r);
                emit_line(o.str());
              }
            return 1;
          }
        else
          return 0;
      }
  return 0;
}
void GBinhom_comp::find_pairs(gb_elem *p)
// compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
// (includes cases m * lead(p) = 0).
// Returns a list of new s_pair's.
{
  queue<Bag *> elems;
  Index<MonomialIdeal> j;
  intarray vplcm;
  s_pair *q;
  int nvars = M->n_vars();
  int *f_m = M->make_one();
  int *f_m2 = M->make_one();
  int *find_pairs_lcm = newarray_atomic(int, nvars);
  int *find_pairs_mon = M->make_one();
  int *pi = newarray_atomic(int, nvars);
  int *pj = newarray_atomic(int, nvars);
  int *pij = newarray_atomic(int, nvars);

  GR->gbvector_get_lead_monomial(F, p->f, f_m);
  if (GR->is_skew_commutative())
    {
      int *find_pairs_exp = newarray_atomic(int, nvars);
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
          s_pair *q2 = new_var_pair(p, find_pairs_lcm);
          elems.insert(new Bag(q2, vplcm));
        }
      freemem(find_pairs_exp);
    }

// Add in syzygies arising from a base ring
#ifdef DEVELOPMENT
#warning "quotient ring stuff"
#endif
  if (originalR->is_quotient_ring())
    {
      for (int i = 0; i < originalR->n_quotients(); i++)
        {
          const gbvector *f = originalR->quotient_gbvector(i);
          M->lcm(f->monom, f_m, find_pairs_lcm);
          vplcm.shrink(0);
          M->to_varpower(find_pairs_lcm, vplcm);
          s_pair *q2 = new_ring_pair(p, find_pairs_lcm);
          elems.insert(new Bag(q2, vplcm));
        }
    }

  // Add in syzygies arising as s-pairs
  for (gb_elem *s = gb->next_min; s != NULL; s = s->next_min)
    {
      if (p->f->comp != s->f->comp) continue;

      GR->gbvector_get_lead_monomial(F, s->f, f_m2);
      M->lcm(f_m, f_m2, find_pairs_lcm);

      vplcm.shrink(0);
      M->to_varpower(find_pairs_lcm, vplcm);
      q = new_s_pair(p, s, find_pairs_lcm);
      elems.insert(new Bag(q, vplcm));
    }

  // Now minimalize these elements, and insert the minimal ones

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal mi(originalR, elems, rejects);
  while (rejects.remove(b))
    {
      s_pair *q2 = reinterpret_cast<s_pair *>(b->basis_ptr());
      remove_pair(q2);
      freemem(b);
    }

  s_pair head;
  s_pair *nextsame = &head;
  int len = 0;
  for (j = mi.first(); j.valid(); j++)
    {
      q = reinterpret_cast<s_pair *>(mi[j]->basis_ptr());
      nextsame->next = q;
      nextsame = q;
      len++;
      if (is_ideal && q->syz_type == SPAIR_PAIR)
        {
          M->gcd(q->first->f->monom, q->second->f->monom, find_pairs_mon);
          if (M->is_one(find_pairs_mon))
            {
              n_saved_gcd++;
              q->compare_num = -1;  // MES: change name of field!!
                                    // This means: don't compute spair.
              if (M2_gbTrace >= 8)
                {
                  buffer o;
                  o << "removed pair[" << q->first->me << " " << q->second->me
                    << "]";
                  emit_line(o.str());
                }
            }
        }
    }
  n_pairs += len;
  nextsame->next = NULL;
  p->pair_list = head.next;
  spairs->sort_list(p->pair_list);
  if (M2_gbTrace >= 8)
    {
      buffer o;
      for (q = p->pair_list; q != NULL; q = q->next)
        {
          o << "insert ";
          debug_out(o, q);
          o << newline;
        }
      emit(o.str());
    }
  for (q = p->pair_list; q != NULL; q = q->next) q->next_same = q->next;
  spairs->insert(p->pair_list, len);

  // remove those pairs (i,j) for which gcd(p:i, p:j) = 1
  // and for which (p,i), (p,j) are both in the previous list of add-ons.
  // MES: this does not catch all of the un-necessary pairs...
  // Also much optimization might be able to be done, as far as removing
  // keeping the 'correct' minimal generator of the lcms.

  for (s_pair *s1 = p->pair_list; s1 != NULL; s1 = s1->next_same)
    {
      if (s1->syz_type != SPAIR_PAIR) continue;

      GR->gbvector_get_lead_monomial(F, s1->second->f, f_m);

      M->divide(s1->lcm, f_m, pi);

      for (s_pair *t1 = s1->next_same; t1 != NULL; t1 = t1->next_same)
        {
          if (t1->syz_type != SPAIR_PAIR) continue;
          GR->gbvector_get_lead_monomial(F, t1->second->f, f_m);
          M->divide(t1->lcm, f_m, pj);
          M->gcd(pi, pj, pij);
          if (M->is_one(pij))
            {
              if (mark_pair(s1->second, t1->second))
                {
                  n_saved_lcm++;
                }
            }
        }
    }

  // Remove the local variables
  freemem(find_pairs_lcm);
  freemem(pi);
  freemem(pj);
  freemem(pij);
  M->remove(find_pairs_mon);
  M->remove(f_m);
  M->remove(f_m2);
}

void GBinhom_comp::compute_s_pair(s_pair *p)
{
  if (p->f == NULL)
    {
      int *s = M->make_one();
      M->divide(p->lcm, p->first->f->monom, s);

      GR->gbvector_mult_by_term(
          F, Fsyz, GR->one(), s, p->first->f, p->first->fsyz, p->f, p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
        GR->gbvector_reduce_lead_term(
            F, Fsyz, 0, p->f, p->fsyz, p->second->f, p->second->fsyz);
      M->remove(s);
    }
}

int GBinhom_comp::gb_reduce(gbvector *&f, gbvector *&fsyz)
{
  if ((strategy & STRATEGY_LONGPOLYNOMIALS) != 0) return gb_geo_reduce(f, fsyz);
  gbvector head;
  gbvector *result = &head;
  result->next = 0;
  gb_elem *q;

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
      GR->gbvector_get_lead_exponents(F, f, div_totalexp);
#ifdef DEVELOPMENT
#warning "quotient ring stuff"
#endif
      Bag *b;
      if (originalR->is_quotient_ring() &&
          originalR->get_quotient_monomials()->search_expvector(div_totalexp,
                                                                b))
        {
          const gbvector *g = originalR->quotient_gbvector(b->basis_elem());
          GR->gbvector_reduce_lead_term(F, Fsyz, head.next, f, fsyz, g, 0);
          count++;
        }
      else if (search(div_totalexp, f->comp, q))
        {
          GR->gbvector_reduce_lead_term(
              F, Fsyz, head.next, f, fsyz, q->f, q->fsyz);
          count++;
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
      emit(o.str());
    }

  f = head.next;
  freemem(div_totalexp);
  return 1;
}

int GBinhom_comp::gb_geo_reduce(gbvector *&f, gbvector *&fsyz)
{
  gb_elem *q;

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
      GR->gbvector_get_lead_exponents(F, lead, div_totalexp);
#ifdef DEVELOPMENT
#warning "quotient ring stuff"
#endif
      Bag *b;
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
      else if (search(div_totalexp, lead->comp, q))
        {
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
      emit(o.str());
    }
  f = head.next;

  fsyz = fsyzb.value();
  freemem(div_totalexp);
  return 1;
}

int GBinhom_comp::compare(const gb_elem *p, const gb_elem *q) const
{
  int cmp = M->compare(p->f->monom, q->f->monom);
  if (cmp == -1) return LT;
  if (cmp == 1) return GT;
  cmp = p->f->comp - q->f->comp;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}
int GBinhom_comp::search(const int *exp, int comp, gb_elem *&result)
{
  int nvars = M->n_vars();
  int *exp2;
  for (gb_elem *p = gbLarge->next; p != NULL; p = p->next)
    {
      if (p->f->comp != comp) continue;
      exp2 = p->lead_exp;
      int is_div = 1;
      for (int i = 0; i < nvars; i++)
        if (exp2[i] > exp[i])
          {
            is_div = 0;
            break;
          }
      if (is_div)
        {
          result = p;
          return 1;
        }
    }
  return 0;
}
void GBinhom_comp::gb_insert(gbvector *f, gbvector *fsyz, int minlevel)
{
  int *f_m = M->make_one();
  minlevel = (minlevel == 0 ? MINGB_MASK : MINGEN_MASK | MINGB_MASK);
  gb_elem *p = new gb_elem(f, fsyz, minlevel);
  p->me = last_gb_num++;
  p->lead_exp = newarray_atomic(int, M->n_vars());

  GR->gbvector_get_lead_monomial(F, p->f, f_m);
  GR->gbvector_remove_content(p->f, p->fsyz);

  M->to_expvector(f_m, p->lead_exp);
  if (M->in_subring(1, f_m)) n_subring++;

  // Next determine the new s pairs.  This also deletes unneeded pairs
  find_pairs(p);

  // Insert into the Groebner basis
  minimal_gb_valid = false;
  gb_elem *q = gbLarge;
  gb_elem *prevmin = gb;
  for (;;)
    {
      if (q->next == NULL || compare(p, q->next) == LT)
        {
          p->next = q->next;
          q->next = p;
          n_gb++;
          // Now place into the minimal list as well
          p->next_min = prevmin->next_min;
          prevmin->next_min = p;
          break;
        }
      else if (q->next->is_min & MINGB_MASK)
        prevmin = q->next;
      q = q->next;
    }

  M->remove(f_m);

  // At this point 'p' has been inserted.  Now we need to remove the
  // non-minimal elements.
  q = p;
  while (q->next_min != NULL)
    // MES: this loop would be a good place to put in auto-reduction?
    if (p->f->comp == q->next_min->f->comp &&
        M->divides(p->f->monom, q->next_min->f->monom))
      {
        gb_elem *tmp = q->next_min;
        q->next_min = tmp->next_min;
        tmp->next_min = NULL;
        tmp->is_min ^= MINGB_MASK;  // I.e. not in the minimal GB
        n_gb--;
      }
    else
      q = q->next_min;
}

int GBinhom_comp::s_pair_step(s_pair *p)
// If no s-pairs left in the current degree,
// return SPAIR_DONE.
// Otherwise, compute the current s-pair, reduce it, and
// dispatch the result.  Return one of the other SPAIR_*
// values.
{
  n_computed++;
  if (M2_gbTrace >= 8)
    {
      buffer o;
      o << "--- computing pair ";
      debug_out(o, p);
      o << " ----" << newline;
      emit(o.str());
    }
  int minlevel = (p->syz_type == SPAIR_GEN);
  int compute_pair = (p->compare_num >= 0);  // MES: change field name
  if (compute_pair)
    {
      compute_s_pair(p);

      if (!gb_reduce(p->f, p->fsyz)) return SPAIR_DEFERRED;
    }
  gbvector *f = p->f;
  gbvector *fsyz = p->fsyz;
  p->f = NULL;
  p->fsyz = NULL;
  if (p->first != NULL)
    {
      // Then 'p' should be the first element on the p->first->pair_list
      assert(p->first->pair_list == p);
      p->first->pair_list = p->next_same;
    }
  remove_pair(p);

  if (!compute_pair) return SPAIR_REMOVED;

  if (!GR->gbvector_is_zero(f))
    {
      gb_insert(f, fsyz, minlevel);
      if (M2_gbTrace >= 8)
        {
          buffer o;
          o << "  gb " << last_gb_num - 1 << " = ";
          GR->gbvector_text_out(o, F, f);
          emit_line(o.str());
        }
      return SPAIR_GB;
    }
  if (!GR->gbvector_is_zero(fsyz))
    {
      if (M2_gbTrace >= 8)
        {
          buffer o;
          o << "  syz = ";
          GR->gbvector_text_out(o, Fsyz, fsyz);
          emit_line(o.str());
        }
      if (collect_syz)
        {
          vec fsyzvec = originalR->translate_gbvector_to_vec(Fsyz, fsyz);
          n_syz++;
          syz.append(fsyzvec);
          return SPAIR_SYZ;
        }
      else
        GR->gbvector_remove(fsyz);
    }
  return SPAIR_ZERO;
}

//---- Completion testing -----------------------------

ComputationStatusCode GBinhom_comp::computation_complete() const
// Test whether the current computation is done.
{
  if (stop_.basis_element_limit > 0 && n_gb >= stop_.basis_element_limit)
    return COMP_DONE_GB_LIMIT;
  if (stop_.syzygy_limit > 0 && n_syz >= stop_.syzygy_limit)
    return COMP_DONE_SYZ_LIMIT;
  if (stop_.pair_limit > 0 && n_computed >= stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.subring_limit > 0 && n_subring >= stop_.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

//---- state machine (roughly) for the computation ----

void GBinhom_comp::start_computation()
{
  ComputationStatusCode is_done = COMP_COMPUTING;

  for (;;)
    {
      if (system_interrupted())
        {
          is_done = COMP_INTERRUPTED;
          break;
        }

      if (need_resize)
        {
          is_done = COMP_NEED_RESIZE;
          break;
        }

      is_done = computation_complete();
      if (is_done != COMP_COMPUTING) break;

      if (error())
        {
          is_done = COMP_ERROR;
          break;
        }
      s_pair *p = spairs->remove();
      if (p == NULL)
        {
          is_done = COMP_DONE;
          break;
        }
      int stype = s_pair_step(p);
      if (M2_gbTrace >= 3 && M2_gbTrace <= 7) switch (stype)
          {
            case SPAIR_GB:
              emit_wrapped("m");
              break;
            case SPAIR_SYZ:
              emit_wrapped("z");
              break;
            case SPAIR_ZERO:
              emit_wrapped("o");
              break;
            case SPAIR_REMOVED:
              emit_wrapped("r");
              break;
            default:
              emit_wrapped("ERROR");
              break;
          }
    }

  // MES: complete the reduction of the GB here
  if (M2_gbTrace >= 1) emit_line("");
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << "Number of pairs             = " << n_pairs << newline;
      o << "Number of gb elements       = " << n_gb << newline;
      o << "Number of gcd=1 pairs       = " << n_saved_gcd << newline;
      o << "Number of gcd tails=1 pairs = " << n_saved_lcm << newline;
      o << "Number of pairs computed    = " << n_computed << newline;
      emit(o.str());
    }
  set_status(is_done);
}

/*******************************
 ** Minimalization of the GB ***
 *******************************/
void GBinhom_comp::minimalize_gb()
{
  if (minimal_gb_valid) return;

  VECTOR(POLY) polys;

  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    {
      POLY g;
      g.f = q->f;
      g.fsyz = q->fsyz;
      polys.push_back(g);
    }

  minimal_gb->minimalize(polys);
  minimal_gb_valid = true;
}

//--- Reduction --------------------------
const Matrix /* or null */ *GBinhom_comp::matrix_remainder(const Matrix *m)
{
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
}

M2_bool GBinhom_comp::matrix_lift(const Matrix *m,
                                  const Matrix /* or null */ **result_remainder,
                                  const Matrix /* or null */ **result_quotient)
{
  minimalize_gb();
  return minimal_gb->matrix_lift(m, result_remainder, result_quotient);
}

int GBinhom_comp::contains(const Matrix *m)
// Return -1 if every column of 'm' reduces to zero.
// Otherwise return the index of the first column that
// does not reduce to zero.
{
  minimalize_gb();
  return minimal_gb->contains(m);
}

//--- Obtaining matrices as output -------

int GBinhom_comp::complete_thru_degree() const
// The computation is complete up through this degree.
{
#ifdef DEVELOPMENT
#warning "not set"
#endif
  return 0;
}

void GBinhom_comp::text_out(buffer &o) const
/* This displays statistical information, and depends on the
   M2_gbTrace value */
{
  stats();
}

const Matrix /* or null */ *GBinhom_comp::get_mingens()
{
  MatrixConstructor mat(F, 0);
  for (gb_elem *q = gb->next; q != NULL; q = q->next)
    if (q->is_min & MINGEN_MASK)
      mat.append(originalR->translate_gbvector_to_vec(F, q->f));
  return mat.to_matrix();
}

const Matrix /* or null */ *GBinhom_comp::get_initial(int nparts)
{
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
}

const Matrix /* or null */ *GBinhom_comp::get_parallel_lead_terms(M2_arrayint w)
{
  minimalize_gb();
  return minimal_gb->get_parallel_lead_terms(w);
}

const Matrix /* or null */ *GBinhom_comp::get_gb()
{
  minimalize_gb();
  //  fprintf(stderr, "-- done with GB -- \n");
  return minimal_gb->get_gb();
}

const Matrix /* or null */ *GBinhom_comp::get_change()
{
  minimalize_gb();
  return minimal_gb->get_change();
}

const Matrix /* or null */ *GBinhom_comp::get_syzygies()
{
#ifdef DEVELOPMENT
#warning \
    "this is not correct: this grabs the vectors, and so can't be called twice"
#endif
  return syz.to_matrix();
}

void GBinhom_comp::debug_out(s_pair *q) const
{
  buffer o;
  debug_out(o, q);
  emit(o.str());
}

void GBinhom_comp::debug_out(buffer &o, s_pair *q) const
{
  if (q == NULL) return;
  int *m = M->make_one();
  o << "(";
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
  if (q->first != NULL)
    {
      M->divide(q->lcm, q->first->f->monom, m);
      M->elem_text_out(o, m);
      o << ' ';
    }
  if (q->second != NULL)
    {
      M->divide(q->lcm, q->second->f->monom, m);
      M->elem_text_out(o, m);
      o << ' ';
    }
  M->elem_text_out(o, q->lcm);
  M->remove(m);
  if (q->compare_num < 0) o << " marked";
  o << ") ";
}

void GBinhom_comp::debug_pairs_out(gb_elem *p) const
{
  buffer o;
  debug_pairs_out(o, p);
  emit(o.str());
}
void GBinhom_comp::debug_pairs_out(buffer &o, gb_elem *p) const
{
  s_pair *q;
  int n = 0;
  for (q = p->pair_list; q != NULL; q = q->next_same)
    {
      debug_out(o, q);
      n++;
      if (n % 10 == 0) o << newline;
    }
  o << newline;
}

void GBinhom_comp::debug_pairs() const
{
  buffer o;
  debug_pairs(o);
  emit(o.str());
}
void GBinhom_comp::debug_pairs(buffer &o) const
{
  for (gb_elem *p = gbLarge->next; p != NULL; p = p->next)
    debug_pairs_out(o, p);

  for (int i = 0; i < NHEAP; i++)
    {
      s_pair *q = spairs->debug_list(i);
      if (q == NULL) continue;
      o << "---- pairs in bin " << i << " -----" << newline;
      int n = 0;
      for (; q != NULL; q = q->next)
        {
          debug_out(o, q);
          n++;
          if (n % 10 == 0) o << newline;
        }
      o << newline;
    }
}
void GBinhom_comp::stats() const
{
  spairs->stats();
  buffer o;
  if (M2_gbTrace >= 5 && M2_gbTrace % 2 == 1)
    {
      int i = 0;
      for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
        {
          o << i << '\t';
          i++;
          GR->gbvector_text_out(o, F, q->f);
          o << newline;
        }
    }
  emit(o.str());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
