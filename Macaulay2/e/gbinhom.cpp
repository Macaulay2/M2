// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "gbinhom.hpp"
#include "text_io.hpp"
#include "vector.hpp"

extern char system_interrupted;
extern int gbTrace;

void GBinhom_comp::set_up0(const Matrix *m, int csyz, int nsyz)
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
  M = GR->get_flattened_monoid();
  K = GR->get_flattened_coefficients();

  spairs = new s_pair_heap(M);

  gb = gbLarge = new gb_elem;		// List head for the GB computed so far
  gb->next = NULL;			// (both minimal, and large GB's)
  gb->next_min = NULL;

  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  n_comps_per_syz = nsyz;

  F = m->rows();

  n_gb = n_subring = 0;
  n_pairs = n_computed = 0;
  last_gb_num = 0;
  n_saved_gcd = n_saved_lcm = 0;

  collect_syz = csyz;
  is_ideal = (F->rank() == 1 && csyz == 0);
  if (GR->is_weyl_algebra())
    is_ideal = false;
  need_resize = 0;

  for (i=0; i<F->rank(); i++)
    monideals.append(new MonomialIdeal(R));

}

void GBinhom_comp::set_up(const Matrix *m, int csyz, int nsyz, int strat)
{
  strategy = strat;
  set_up0(m, csyz, nsyz);

  Fsyz = m->cols()->sub_space(n_comps_per_syz);  
  syz = new Matrix(Fsyz);

  add_gens(0, m->n_cols()-1, m);
}

void GBinhom_comp::inter_reduce(gb_elem *&/*gens*/)
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
  for (int i=hi; i>=lo; i--)
    {
      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(F,(*m)[i], denom);
      s_pair *p = new_gen(i, f, denom);
      if (p != NULL)
	spairs->insert(p);
      n_pairs++;
    }
}

void GBinhom_comp::force(const Matrix *m, const Matrix *gb0, const Matrix *mchange,
		    const Matrix *msyz)
{
  int csyz = (msyz->n_cols() > 0);
  set_up0(m, csyz, mchange->n_rows());

  Fsyz = mchange->rows();
  syz = const_cast<Matrix *>(msyz);

  for (int i=0; i<gb0->n_cols(); i++)
    {
      if ((*gb0)[i] == NULL) continue;
      ring_elem denom1, denom2, u, v;
      gbvector *f = originalR->translate_gbvector_from_vec(F, (*gb0)[i], denom1);
      gbvector *fsyz = originalR->translate_gbvector_from_vec(Fsyz, (*mchange)[i], denom2);
      K->syzygy(denom1,denom2,u,v);
      GR->gbvector_mult_by_coeff_to(f,u);
      K->negate_to(v);
      GR->gbvector_mult_by_coeff_to(fsyz,v);
      gb_insert(f,fsyz,1);
    }
}

GBinhom_comp::GBinhom_comp(const Matrix *m, int csyz, int nsyz, int strat)
{
  set_up(m, csyz, nsyz, strat);
}

GBinhom_comp::GBinhom_comp(const Matrix *m, const Matrix *gb0, const Matrix *mchange, 
		 const Matrix *syz0)
{
  force(m, gb0, mchange, syz0);
}

void GBinhom_comp::remove_pair(s_pair *& p)
{
  GR->gbvector_remove(p->f);
  GR->gbvector_remove(p->fsyz);
  p->first = NULL;
  p->second = NULL;
  p->next = NULL;
  M->remove(p->lcm);
  delete p;
  p = NULL;
}

GBinhom_comp::~GBinhom_comp()
{
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
  return result;
}

s_pair *GBinhom_comp::new_gen(int i, gbvector * f, ring_elem denom)
{
  gbvector *fsyz;

  if (i < n_comps_per_syz)
    fsyz = GR->gbvector_term(Fsyz,denom,i);
  else
    fsyz = GR->gbvector_zero();

  if (GR->gbvector_is_zero(f))
    {
      if (!GR->gbvector_is_zero(fsyz))
	{
	  vec fsyzvec = originalR->translate_gbvector_to_vec(Fsyz,fsyz);
	  syz->append(fsyzvec);
	}
      return NULL;
    }

  s_pair *result = new s_pair;
  result->next = NULL;
  result->syz_type = SPAIR_GEN;
  result->degree = GR->gbvector_degree(F,f);
  result->compare_num = 0;
  result->first = NULL;
  result->second = NULL;
  result->f = f;  /* NOTE THAT WE GRAB f */
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
	    if (gbTrace >= 8)
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
	    if (gbTrace >= 8)
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
  int *f_m2 = M->make_one();
  int *find_pairs_exp = new int[nvars];
  int *find_pairs_lcm = new int[nvars];
  int *find_pairs_mon = M->make_one();
  int *pi = new int [nvars];
  int *pj = new int [nvars];
  int *pij = new int [nvars];

  GR->gbvector_get_lead_monomial(F, p->f, f_m);
  if (GR->is_skew_commutative())
    {
      M->to_expvector(f_m, find_pairs_exp);

      for (int v=0; v<GR->n_skew_commutative_vars(); v++)
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
    }

  // Add in syzygies arising from a base ring
#warning "quotient ring stuff"
#if 0
  if (GR->is_quotient_ring())
    {
      for (int i=0; i<GR->n_quotients(); i++)
	{
	  const gbvector * f = GR->quotient_element(i);
	  M->lcm(f->monom, f_m, find_pairs_lcm);
	  vplcm.shrink(0);
	  M->to_varpower(find_pairs_lcm, vplcm);
	  s_pair *q2 = new_ring_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q2, vplcm));
	}
    }
#endif

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
  MonomialIdeal mi(originalR->get_flattened_ring(), elems, rejects);
  while (rejects.remove(b))
    {
      s_pair *q2 = (s_pair *) b->basis_ptr();
      remove_pair(q2);
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
	      if (gbTrace >= 8)
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
  if (gbTrace >= 8)
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
  delete [] find_pairs_exp;
  delete [] find_pairs_lcm;
  delete [] pi;
  delete [] pj;
  delete [] pij;
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

      GR->gbvector_mult_by_term(F,Fsyz,
				GR->one(), s,
				p->first->f, 
				p->first->fsyz,
				p->f, 
				p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
	GR->gbvector_reduce_lead_term(F,Fsyz,0,p->f,p->fsyz,
				      p->second->f, p->second->fsyz);
      M->remove(s);
    }
}

int GBinhom_comp::gb_reduce(gbvector * &f, gbvector * &fsyz)
{
  if ((strategy & STRATEGY_LONGPOLYNOMIALS) != 0)
    return gb_geo_reduce(f,fsyz);
  gbvector head;
  gbvector *result = &head;
  result->next = 0;
  gb_elem *q;

  int *div_totalexp = new int[M->n_vars()];
  int *reduce_ndiv = new int[M->n_vars()];
  int count = 0;
  if (gbTrace == 10)
    {
      buffer o;
      o << "reducing ";
      GR->gbvector_text_out(o,F,f);
      emit_line(o.str());
    }
  while (f != NULL)
    {
      GR->gbvector_get_lead_exponents(F, f, div_totalexp);
#warning "quotient ring stuff"
#if 0
      Bag *b;
      if (GR->is_quotient_ring() 
	  && GR->get_quotient_monomials()->search_expvector(div_totalexp, b))
	{
	  gbvector *g = (gbvector *) b->basis_ptr();
	  GR->gbvector_reduce_lead_term(F,Fsyz,head.next,f,fsyz,g,0);
	  count++;
	}
      else 
#endif
if (search(div_totalexp, f->comp, q))
	{
	  GR->gbvector_reduce_lead_term(F,Fsyz,head.next,f,fsyz,q->f,q->fsyz);
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
  if (gbTrace >= 4)
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }

  f = head.next;
  delete [] div_totalexp;
  delete [] reduce_ndiv;
  return 1;
}

int GBinhom_comp::gb_geo_reduce(gbvector * &f, gbvector * &fsyz)
{
  gb_elem *q;

  gbvector head;
  gbvector *result = &head;
  result->next = 0;
  int *div_totalexp = new int[M->n_vars()];
  int *reduce_ndiv = new int[M->n_vars()];
  int count = 0;

  gbvectorHeap fb(GR,F);
  gbvectorHeap fsyzb(GR,Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);
  const gbvector *lead;

  while ((lead = fb.get_lead_term()) != NULL)
    {
      GR->gbvector_get_lead_exponents(F, lead, div_totalexp);
#warning "quotient ring stuff"
#if 0
      Bag *b;
      if (GR->is_quotient_ring()
	  && GR->get_quotient_monomials()->search_expvector(div_totalexp, b))
	{
	  gbvector *g = (gbvector *) b->basis_ptr();
	  GR->reduce_lead_term_heap(F,Fsyz,
				    lead, div_totalexp, // are these two needed
				    result,fb,fsyzb,
				    g,0);
	  count++;
	}
      else 
#endif
if (search(div_totalexp, lead->comp, q))
	{
	  GR->reduce_lead_term_heap(F,Fsyz,
				    lead, div_totalexp,
				    result,fb,fsyzb,
				    q->f,q->fsyz);
	  count++;
	}
      else
	{
	  result->next = fb.remove_lead_term();
	  result = result->next;
	  result->next = 0;
	}
    }

  if (gbTrace >= 4)
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }
  f = head.next;

  fsyz = fsyzb.value();
  delete [] div_totalexp;
  delete [] reduce_ndiv;
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
void GBinhom_comp::gb_insert(gbvector * f, gbvector * fsyz, int forced)
{
  int *f_m = M->make_one();
  gb_elem *p = new gb_elem(f, fsyz, 1);
  p->me = last_gb_num++;
  p->lead_exp = new int[M->n_vars()];

  GR->gbvector_get_lead_monomial(F,p->f,f_m);
  GR->gbvector_remove_content(p->f, p->fsyz);

  M->to_expvector(f_m, p->lead_exp);
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
  if (gbTrace >= 8)
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
  gbvector * f = p->f;
  gbvector * fsyz = p->fsyz;
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
      gb_insert(f, fsyz,0);	// The 0 means 'normal insertion'.
      if (gbTrace >= 8)
	{
	  buffer o;
	  o << "  gb " << last_gb_num-1 << " = ";
	  GR->gbvector_text_out(o, F, f);
	  emit_line(o.str());
	}
      return SPAIR_GB;
    }
  if (!GR->gbvector_is_zero(fsyz))
    {
      if (gbTrace >= 8)
	{
	  buffer o;
	  o << "  syz = ";
	  GR->gbvector_text_out(o, Fsyz,fsyz);
	  emit_line(o.str());
	}
      if (collect_syz)
	{
	  vec fsyzvec = originalR->translate_gbvector_to_vec(Fsyz,fsyz);
	  syz->append(fsyzvec);
	  return SPAIR_SYZ;
	}
      else
	GR->gbvector_remove(fsyz);
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
  if (stop_syz > 0 && syz->n_cols() >= stop_syz) return COMP_DONE_SYZ_LIMIT;
  if (stop_pairs > 0 && n_computed >= stop_pairs) return COMP_DONE_PAIR_LIMIT;
  if (stop_subring > 0 && n_subring >= stop_subring) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

//---- state machine (roughly) for the computation ----

int GBinhom_comp::calc(const int * /*deg*/, const intarray &stop)
{
  if (stop.length() != 7) 
    {
      ERROR("inappropriate stop conditions for GB computation");
      return COMP_ERROR;
    }
  int stop_gb = stop[0]; //ngb
  int stop_syz = stop[1]; //nsyz
  int stop_pairs = stop[2]; //npairs
  int stop_subring = stop[5]; //subring limit
  int is_done = COMP_COMPUTING;
  
  for (;;)
    {
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
	  ERROR(error_message());
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
      if (gbTrace >= 3 && gbTrace <= 7)
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
  if (gbTrace >= 1) emit_line("");
  if (gbTrace >= 4)
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
Matrix *GBinhom_comp::reduce(const Matrix *m, Matrix *&lift)
{
  if (m->n_rows() != F->rank()) {
       ERROR("expected matrices to have same number of rows");
       return 0;
  }
  Matrix *red = new Matrix(m->rows(), m->cols());
  lift = new Matrix(Fsyz, m->cols());
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector * f = originalR->translate_gbvector_from_vec(F, (*m)[i], denom);
      gbvector * fsyz = GR->gbvector_zero();

      gb_reduce(f, fsyz);

      vec fv = originalR->translate_gbvector_to_vec_denom(F, f, denom);
      K->negate_to(denom);
      vec fsyzv = originalR->translate_gbvector_to_vec_denom(Fsyz,fsyz, denom);
      (*red)[i] = fv;
      (*lift)[i] = fsyzv;
    }
  return red;
}

int GBinhom_comp::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(F,(*m)[i], denom);
      K->remove(denom);
      gbvector *fsyz = NULL;
      gb_reduce(f, fsyz);
      GR->gbvector_remove(fsyz);
      if (f != NULL)
	{
	  GR->gbvector_remove(f);
	  return i;
	}
    }
  return -1;
}

//--- Obtaining matrices as output -------
Matrix *GBinhom_comp::min_gens_matrix()
{
  Matrix *result = new Matrix(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    if (q->is_min)
      result->append(originalR->translate_gbvector_to_vec(F,q->f));
  return result;
}

Matrix *GBinhom_comp::initial_matrix(int n)
{
  Matrix *result = new Matrix(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    {
      gbvector *f = GR->gbvector_lead_term(n, F, q->f);
      result->append(originalR->translate_gbvector_to_vec(F,f));
    }
  return result;
}

Matrix *GBinhom_comp::gb_matrix()
{
  Matrix *result = new Matrix(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result->append(originalR->translate_gbvector_to_vec(F,q->f));
  return result;
}

Matrix *GBinhom_comp::change_matrix()
{
  Matrix *result = new Matrix(Fsyz);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result->append(originalR->translate_gbvector_to_vec(Fsyz,q->fsyz));
  return result;
}

Matrix *GBinhom_comp::syz_matrix()
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
  if (gbTrace >= 5 && gbTrace % 2 == 1)
    {
      int i = 0;
      for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
	{
	  o << i << '\t';
	  i++;
	  GR->gbvector_text_out(o, F,q->f);
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



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
