// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "gbinhom.hpp"
#include "geovec.hpp"
#include "text_io.hpp"

extern char system_interrupted;
extern int comp_printlevel;

stash *GBinhom_comp::mystash;

void GBinhom_comp::set_up0(const Matrix &m, int csyz, int nsyz)
{
  int i;
  R = m.get_ring()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      gError << "ring is not a polynomial ring";
      // MES: throw an error here.
      assert(0);
    }
  M = R->Nmonoms();

  spairs = new s_pair_heap(M);

  gb = gbLarge = new gb_elem;		// List head for the GB computed so far
  gb->next = NULL;			// (both minimal, and large GB's)
  gb->next_min = NULL;

  if (nsyz < 0 || nsyz > m.n_cols())
    nsyz = m.n_cols();
  n_comps_per_syz = nsyz;

  F = m.rows();
  bump_up(F);

  n_gb = n_subring = 0;
  n_pairs = n_computed = 0;
  last_gb_num = 0;
  n_saved_gcd = n_saved_lcm = 0;

  collect_syz = csyz;
  is_ideal = (F->rank() == 1 && csyz == 0);
  if (R->cast_to_WeylAlgebra() != 0)
    is_ideal = false;
  need_resize = 0;

  for (i=0; i<F->rank(); i++)
    monideals.append(MonomialIdeal(R));

}

void GBinhom_comp::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  strategy = strat;
  set_up0(m, csyz, nsyz);

  Fsyz = m.cols()->sub_space(n_comps_per_syz);  
  bump_up(Fsyz);
  syz = Matrix(Fsyz);

  add_gens(0, m.n_cols()-1, m);
}

void GBinhom_comp::inter_reduce(gb_elem *&/*gens*/)
{
  // MES
}

void GBinhom_comp::add_gens(int lo, int hi, const Matrix &m)
{
  // MES
  // First incorporate the new generators.
  // 2. inter-reduce them, so they have different lead terms
  // 3. insert them into gb, gbLarge
  // 4. each insertion will also call find_pairs.

  // MES: should we inter-reduce these first?  Does it matter?
  for (int i=hi; i>=lo; i--)
    {
      s_pair *p = new_gen(i, m[i]);
      if (p != NULL)
	spairs->insert(p);
      n_pairs++;
    }
}

void GBinhom_comp::force(const Matrix &m, const Matrix &gb, const Matrix &mchange,
		    const Matrix &msyz)
{
  int csyz = (msyz.n_cols() > 0);
  set_up0(m, csyz, mchange.n_rows());

  Fsyz = mchange.rows();
  bump_up(Fsyz);
  syz = msyz;

  for (int i=0; i<gb.n_cols(); i++)
    {
      vec f = F->copy(gb[i]);
      vec fsyz = Fsyz->copy(mchange[i]);
      gb_insert(f,fsyz,1);
    }
}

GBinhom_comp::GBinhom_comp(const Matrix &m, int csyz, int nsyz, int strat)
  : gb_comp(COMP_GBINHOM)
{
  set_up(m, csyz, nsyz, strat);
}

GBinhom_comp::GBinhom_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
		 const Matrix &syz)
  : gb_comp(COMP_GBINHOM)
{
  force(m, gb, mchange, syz);
}

void GBinhom_comp::remove_pair(s_pair *& p)
{
  F->remove(p->f);
  Fsyz->remove(p->fsyz);
  p->first = NULL;
  p->second = NULL;
  p->next = NULL;
  M->remove(p->lcm);
  delete p;
  p = NULL;
}

GBinhom_comp::~GBinhom_comp()
{
  // remove any remaining s-pairs
  s_pair *p;
  while ((p = spairs->remove()) != NULL)
    remove_pair(p);
  delete spairs;

  // remove the gb_elem's
  while (gbLarge != NULL)
    {
      gb_elem *tmp = gbLarge;
      gbLarge = tmp->next;

      // remove gb_elem fields, and itself
      F->remove(tmp->f);
      Fsyz->remove(tmp->fsyz);
      delete [] tmp->lead_exp;
      delete tmp;
    }

  // Finally, decrement ref counts
  bump_down(F);
  bump_down(Fsyz);
}

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
  return new_ring_pair(p,lcm);
}

s_pair *GBinhom_comp::new_ring_pair(gb_elem *p, const int *lcm)
{
  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_RING;
  result->degree = M->primary_degree(lcm) + F->primary_degree(p->f->comp);
  result->compare_num = 0;
  result->first = p;
  result->second = NULL;
  result->f = NULL;
  result->fsyz = NULL;

  result->lcm = M->make_new(lcm);
  M->mult(result->lcm, F->base_monom(p->f->comp), result->lcm);
  return result;
}

s_pair *GBinhom_comp::new_s_pair(gb_elem *p, gb_elem *q, const int *lcm)
{
  // p and q should have 'f' field defined.
  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_PAIR;
  result->degree = M->primary_degree(lcm) + F->primary_degree(p->f->comp);
  result->compare_num = 0;
  result->first = p;
  result->second = q;
  result->f = NULL;
  result->fsyz = NULL;

  result->lcm = M->make_new(lcm);
  M->mult(result->lcm, F->base_monom(p->f->comp), result->lcm);

  return result;
}

s_pair *GBinhom_comp::new_gen(int i, const vec f)
{
  vec fsyz;

  if (i < n_comps_per_syz)
    fsyz = Fsyz->e_sub_i(i);
  else
    fsyz = NULL;

  if (F->is_zero(f))
    {
      if (!Fsyz->is_zero(fsyz))
	syz.append(fsyz);
      return NULL;
    }

  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_GEN;
  result->degree = F->primary_degree(f);
  result->compare_num = 0;
  result->first = NULL;
  result->second = NULL;
  result->f = F->copy(f);
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
	    if (comp_printlevel >= 8)
	      {
		buffer o;
		o << "---- removed pair ";
		debug_out(o,r);
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
	    if (comp_printlevel >= 8)
	      {
		buffer o;
		o << "---- removed pair ";
		debug_out(o,r);
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
  int *find_pairs_exp = new int[nvars];
  int *find_pairs_lcm = new int[nvars];
  int *find_pairs_mon = M->make_one();
  int *pi = new int [nvars];
  int *pj = new int [nvars];
  int *pij = new int [nvars];

  M->divide(p->f->monom, F->base_monom(p->f->comp), f_m);
  if (M->is_skew())
    {
      int *skewvars = new int[M->n_vars()];
      M->to_expvector(f_m, find_pairs_exp);
      int nskew = M->exp_skew_vars(find_pairs_exp, skewvars);
      
      // Add in syzygies arising from exterior variables
      for (int v=0; v < nskew; v++)
	{
	  int w = skewvars[v];

	  find_pairs_exp[w]++;
	  M->from_expvector(find_pairs_exp, find_pairs_lcm);
	  find_pairs_exp[w]--;
	      
	  vplcm.shrink(0);
	  M->to_varpower(find_pairs_lcm, vplcm);
	  s_pair *q = new_var_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q, vplcm));
	}
      delete [] skewvars;
    }

  // Add in syzygies arising from a base ring

  if (R->is_quotient_ring())
    {
      const MonomialIdeal &Rideal = R->get_quotient_monomials();
      for (j = Rideal.first(); j.valid(); j++)
	{
	  Nterm * f = (Nterm *) Rideal[j]->basis_ptr();
	  M->lcm(f->monom, f_m, find_pairs_lcm);
	  vplcm.shrink(0);
	  M->to_varpower(find_pairs_lcm, vplcm);
	  q = new_ring_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q, vplcm));
	}
    }

  // Add in syzygies arising as s-pairs
  for (gb_elem *s = gb->next_min; s != NULL; s = s->next_min)
    {
      if (p->f->comp != s->f->comp) continue;

      M->divide(s->f->monom, F->base_monom(p->f->comp), s->f->monom);
      M->lcm(f_m, s->f->monom, find_pairs_lcm);
      M->mult(s->f->monom, F->base_monom(p->f->comp), s->f->monom);      

      vplcm.shrink(0);
      M->to_varpower(find_pairs_lcm, vplcm);
      q = new_s_pair(p, s, find_pairs_lcm);
      elems.insert(new Bag(q, vplcm));
    }

  // Now minimalize these elements, and insert the minimal ones

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal mi(R, elems, rejects);
  while (rejects.remove(b))
    {
      s_pair *q = (s_pair *) b->basis_ptr();
      remove_pair(q);
      delete b;
    }

  s_pair head;
  s_pair *nextsame = &head;
  int len = 0;
  for (j = mi.first(); j.valid(); j++)
    {
      q = (s_pair *) mi[j]->basis_ptr();
      nextsame->next = q;
      nextsame = q;
      len++;
      if (is_ideal && q->syz_type == SPAIR_PAIR)
	{
	  M->gcd(q->first->f->monom, q->second->f->monom, find_pairs_mon);
	  if (M->is_one(find_pairs_mon))
	    {
	      n_saved_gcd++;
	      q->compare_num = -1; // MES: change name of field!!
				    // This means: don't compute spair.
	      if (comp_printlevel >= 8)
		{
		  buffer o;
		  o << "removed pair[" << q->first->me << " " 
		    << q->second->me << "]";
		  emit_line(o.str());
		}
	    }
	}
    }
  n_pairs += len;
  nextsame->next = NULL;
  p->pair_list = head.next;
  spairs->sort_list(p->pair_list);
  if (comp_printlevel >= 8)
    {
      buffer o;
      for (q = p->pair_list; q != NULL; q = q->next)
	{
	  o << "insert ";
	  debug_out(o,q);
	  o << newline;
	}
      emit(o.str());
    }
  for (q = p->pair_list; q != NULL; q = q->next)
    q->next_same = q->next;
  spairs->insert(p->pair_list, len);

  // remove those pairs (i,j) for which gcd(p:i, p:j) = 1
  // and for which (p,i), (p,j) are both in the previous list of add-ons.
  // MES: this does not catch all of the un-necessary pairs...
  // Also much optimization might be able to be done, as far as removing
  // keeping the 'correct' minimal generator of the lcms.

  for (s_pair *s1 = p->pair_list; s1 != NULL; s1 = s1->next_same)
    {    
      if (s1->syz_type != SPAIR_PAIR) continue;

      M->divide(s1->second->f->monom, F->base_monom(s1->second->f->comp), f_m);
      M->divide(s1->lcm, f_m, pi);
      for (s_pair *t1 = s1->next_same; t1 != NULL; t1 = t1->next_same)
	{
	  if (t1->syz_type != SPAIR_PAIR) continue;
	  M->divide(t1->second->f->monom, F->base_monom(t1->second->f->comp), f_m);
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
  delete [] find_pairs_exp;
  delete [] find_pairs_lcm;
  delete [] pi;
  delete [] pj;
  delete [] pij;
  M->remove(find_pairs_mon);
  M->remove(f_m);
}

void GBinhom_comp::compute_s_pair(s_pair *p)
{
  if (p->f == NULL)
    {
      int *s = M->make_one();
      ring_elem one = R->Ncoeffs()->from_int(1);
      M->divide(p->lcm, p->first->f->monom, s);

      p->f = F->imp_mult_by_term(one, s, p->first->f);
      p->fsyz = Fsyz->imp_mult_by_term(one, s, p->first->fsyz);
      if (Fsyz->is_quotient_ring) Fsyz->normal_form(p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
	{
	  ring_elem coeff;
	  F->imp_cancel_lead_term(p->f, p->second->f, coeff, s);
	  Fsyz->subtract_multiple_to(p->fsyz, coeff, s, p->second->fsyz);
	  R->Ncoeffs()->remove(coeff);
	}
      M->remove(s);
      R->Ncoeffs()->remove(one);
    }
}

int GBinhom_comp::gb_reduce(vec &f, vec &fsyz)
{
  if (((strategy & USE_GEOBUCKET) != 0) && !M->is_skew())
    return gb_geo_reduce(f,fsyz);
  vecterm head;
  vecterm *result = &head;
  ring_elem coeff;

  gb_elem *q;
  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());
  int count = 0;
  int *s = M->make_one();
  while (f != NULL)
    {
      Bag *b;
      M->divide(f->monom, F->base_monom(f->comp), s);
      M->to_expvector(s, div_totalexp);
      if (R->is_quotient_ring() 
	  && R->get_quotient_monomials().search_expvector(div_totalexp, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  F->imp_ring_cancel_lead_term(f, g, coeff, reduce_ndiv);
	  R->Ncoeffs()->remove(coeff);
	  count++;
	}
      //else if (monideals[f->comp].search_expvector(div_totalexp, b))
      else if (search(div_totalexp, f->comp, q))
	{
	  F->imp_cancel_lead_term(f, q->f, coeff, reduce_ndiv);
	  Fsyz->subtract_multiple_to(fsyz, coeff, reduce_ndiv, q->fsyz);
	  R->Ncoeffs()->remove(coeff);
	  count++;
	}
      else
	{
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
    }
  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }

  result->next = NULL;
  f = head.next;
  M->remove(s);
  return 1;
}
int GBinhom_comp::gb_geo_reduce(vec &f, vec &fsyz)
{
  vecterm head;
  vecterm *result = &head;

  gb_elem *q;
  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());
  int count = 0;

  vecHeap fb(F);
  vecHeap fsyzb(Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);
  const vecterm *lead;
  int *s = M->make_one();
  while ((lead = fb.get_lead_term()) != NULL)
    {
      Bag *b;
      M->divide(lead->monom, F->base_monom(lead->comp), s);
      M->to_expvector(s, div_totalexp);
      if (R->is_quotient_ring()
	  && R->get_quotient_monomials().search_expvector(div_totalexp, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  M->divide(lead->monom, g->monom, reduce_ndiv);
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  vecterm *h = F->imp_ring_mult_by_term(g, c, reduce_ndiv, lead->comp);
	  R->Ncoeffs()->remove(c);
	  fb.add(h);
	  count++;
	}
      else if (search(div_totalexp, lead->comp, q))
	{
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  M->divide(lead->monom, q->f->monom, reduce_ndiv);
	  vecterm *h = F->imp_mult_by_term(c, reduce_ndiv, q->f);
	  vecterm *hsyz = Fsyz->imp_mult_by_term(c, reduce_ndiv, q->fsyz);
	  R->Ncoeffs()->remove(c);
	  fb.add(h);		// Eats h
	  fsyzb.add(hsyz);	// Eats hsyz
	  count++;
	}
      else
	{
	  result->next = fb.remove_lead_term();
	  result = result->next;
	}
    }

  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }
  result->next = NULL;
  f = head.next;

  fsyz = fsyzb.value();
  M->remove(s);
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
      for (int i=0; i<nvars; i++)
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
void GBinhom_comp::gb_insert(vec f, vec fsyz, int forced)
{
  int *f_m = M->make_one();
  gb_elem *p = new gb_elem(f, fsyz, 1);
  p->me = last_gb_num++;
  p->lead_exp = new int[R->n_vars()];
  M->divide(p->f->monom, F->base_monom(p->f->comp), f_m);
  M->to_expvector(f_m, p->lead_exp);

  F->make_monic(p->f, p->fsyz);
  if (M->in_subring(1,f_m))
    n_subring++;

#if 0
  intarray vp;
  M->to_varpower(p->f->monom, vp);
  monideals[p->f->comp].insert(new Bag(p, vp));
#endif
  // Next determine the new s pairs.  This also deletes unneeded pairs
  if (!forced) find_pairs(p);

  // Insert into the Groebner basis
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
      else if (q->next->is_min)
	prevmin = q->next;
      q = q->next;
    }

  M->remove(f_m);
  if (forced) return;

  // At this point 'p' has been inserted.  Now we need to remove the 
  // non-minimal elements.
  q = p;
  while (q->next_min != NULL)
    // MES: this loop would be a good place to put in auto-reduction?
    if (p->f->comp == q->next_min->f->comp && M->divides(p->f->monom, q->next_min->f->monom))
      {
	gb_elem *tmp = q->next_min;
	q->next_min = tmp->next_min;
	tmp->next_min = NULL;
	tmp->is_min = 0;	// I.e. not in the minimal GB
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
  if (comp_printlevel >= 8)
    {
      buffer o;
      o << "--- computing pair ";
      debug_out(o,p);
      o << " ----" << newline;
      emit(o.str());
    }
  int compute_pair = (p->compare_num >= 0); // MES: change field name
  if (compute_pair)
    {
      compute_s_pair(p);
      
      if (!gb_reduce(p->f, p->fsyz))
	return SPAIR_DEFERRED;
      
    }
  vec f = p->f;
  vec fsyz = p->fsyz;
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

  if (f != NULL)
    {
      gb_insert(f, fsyz,0);	// The 0 means 'normal insertion'.
      if (comp_printlevel >= 8)
	{
	  buffer o;
	  o << "  gb " << last_gb_num-1 << " = ";
	  F->elem_text_out(o, f);
	  emit_line(o.str());
	}
      return SPAIR_GB;
    }
  if (fsyz != NULL)
    {
      if (comp_printlevel >= 8)
	{
	  buffer o;
	  o << "  syz = ";
	  F->elem_text_out(o, fsyz);
	  emit_line(o.str());
	}
      if (collect_syz)
	{
	  syz.append(fsyz);
	  return SPAIR_SYZ;
	}
      else
	Fsyz->remove(fsyz);
    }
  return SPAIR_ZERO;
}

//---- Completion testing -----------------------------

int GBinhom_comp::computation_complete(int stop_gb, 
				       int stop_syz, 
				       int stop_pairs,
				       int stop_subring)
     // Test whether the current computation is done.
{
  if (stop_gb > 0 && n_gb >= stop_gb) return COMP_DONE_GB_LIMIT;
  if (stop_syz > 0 && syz.n_cols() >= stop_syz) return COMP_DONE_SYZ_LIMIT;
  if (stop_pairs > 0 && n_computed >= stop_pairs) return COMP_DONE_PAIR_LIMIT;
  if (stop_subring > 0 && n_subring >= stop_subring) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

//---- state machine (roughly) for the computation ----

int GBinhom_comp::calc(const int * /*deg*/, const intarray &stop)
{
  if (stop.length() != 7) 
    {
      gError << "inappropriate stop conditions for GB computation";
      return COMP_ERROR;
    }
  int stop_gb = stop[0]; //ngb
  int stop_syz = stop[1]; //nsyz
  int stop_pairs = stop[2]; //npairs
  int stop_subring = stop[5]; //subring limit
  int is_done = COMP_COMPUTING;
  
  for (;;)
    {
      system_spincursor();
      if (system_interrupted) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}

      if (need_resize)
	{
	  is_done = COMP_NEED_RESIZE;
	  break;
	}

      is_done = computation_complete(stop_gb, stop_syz, stop_pairs, stop_subring);
      if (is_done != COMP_COMPUTING) break;

      if (error())
	{
	  gError << error_message();
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
      if (comp_printlevel >= 3 && comp_printlevel <= 7)
	switch (stype)
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
  if (comp_printlevel >= 1) emit_line("");
  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "Number of pairs             = " << n_pairs << newline;
      o << "Number of gb elements       = " << n_gb << newline;
      o << "Number of gcd=1 pairs       = " << n_saved_gcd << newline;
      o << "Number of gcd tails=1 pairs = " << n_saved_lcm << newline;
      o << "Number of pairs computed    = " << n_computed << newline;
      emit(o.str());
    }
  return is_done;
}

//--- Reduction --------------------------
Matrix GBinhom_comp::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols());
  lift = Matrix(Fsyz, m.cols());
  if (m.n_rows() != F->rank()) {
       gError << "expected matrices to have same number of rows";
       return red;
  }
  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = F->copy(m[i]);
      vec fsyz = NULL;

      gb_reduce(f, fsyz);
      Fsyz->negate_to(fsyz);
      red[i] = f;
      lift[i] = fsyz;
    }
  return red;
}

Vector GBinhom_comp::reduce(const Vector &v, Vector &lift)
{
  if (!v.free_of()->is_equal(F))
    {
      gError << "reduce: vector is in incorrect free module";
      return Vector(F, NULL);
    }
  vec f = F->copy(v.get_value());
  vec fsyz = NULL;

  gb_reduce(f, fsyz);
  Fsyz->negate_to(fsyz);

  lift = Vector(Fsyz, fsyz);
  return Vector(F, f);
}

int GBinhom_comp::contains(const Matrix &m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = F->translate(m.rows(),m[i]);
      vec fsyz = NULL;
      gb_reduce(f, fsyz);
      Fsyz->remove(fsyz);
      if (f != NULL)
	{
	  F->remove(f);
	  return i;
	}
    }
  return -1;
}
bool GBinhom_comp::is_equal(const gb_comp * /*q*/)
{
  gError << "== not yet implemented for inhomogeneous GB's";
  return false;
}

//--- Obtaining matrices as output -------
Matrix GBinhom_comp::min_gens_matrix()
{
  Matrix result(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    if (q->is_min)
      result.append(F->copy(q->f));
  return result;
}

Matrix GBinhom_comp::initial_matrix(int n)
{
  Matrix result(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result.append(F->lead_term(n, q->f));
  return result;
}

Matrix GBinhom_comp::gb_matrix()
{
  Matrix result(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result.append(F->copy(q->f));
  return result;
}

Matrix GBinhom_comp::change_matrix()
{
  Matrix result(Fsyz);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result.append(Fsyz->copy(q->fsyz));
  return result;
}

Matrix GBinhom_comp::syz_matrix()
{
  return syz;
}

void GBinhom_comp::debug_out(s_pair *q) const
{
  buffer o;
  debug_out(o,q);
  emit(o.str());
}

void GBinhom_comp::debug_out(buffer &o, s_pair *q) const
{
  if (q == NULL) return;
  int *m = M->make_one();
  o << "(";
  if (q->first != NULL) o << q->first->me; else o << ".";
  o << " ";
  if (q->second != NULL) o << q->second->me; else o << ".";
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
  if (q->compare_num < 0)
    o << " marked";
  o << ") ";
}

void GBinhom_comp::debug_pairs_out(gb_elem *p) const
{
  buffer o;
  debug_pairs_out(o,p);
  emit(o.str());
}
void GBinhom_comp::debug_pairs_out(buffer &o, gb_elem *p) const
{
  s_pair *q;
  int n = 0;
  for (q = p->pair_list; q != NULL; q = q->next_same)
    {
      debug_out(o,q);
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
    debug_pairs_out(o,p);

  for (int i=0; i<NHEAP; i++)
    {
      s_pair *q = spairs->debug_list(i);
      if (q == NULL) continue;
      o << "---- pairs in bin " << i << " -----" << newline;
      int n = 0;
      for ( ; q != NULL; q = q->next)
	{
	  debug_out(o,q);
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
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    {
      int i = 0;
      for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
	{
	  o << i << '\t';
	  i++;
	  F->elem_text_out(o, q->f);
	  o << newline;
	}
    }
  emit(o.str());
}

#if 0
void step()
{
  while ((p = L.remove()) != NULL)
    {
      calc_s_pair(p);
      if (T.reduce(p))		// Returns 1 iff reduction of 'p' is finished
	if (p->f != NULL)
	  {
	    L.update(S,p->f);
	    S.insert(p);
	    T.insert(p);
	    S.remove_redundants();
	  }
	else if (p->fsyz != NULL)
	  {
	    syz.insert(p->fsyz);
	    delete p;
	  }
    }
  S.inter_reduce();		// Reduce tails
}
#endif


