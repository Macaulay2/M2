// Copyright 1997  Michael E. Stillman

#include "style.hpp"
#include "gb2.hpp"
#include "hilb.hpp"
#include "text_io.hpp"

void gb2_comp::setup(FreeModule *FFsyz,
		     gb_node *ggens,
		     int lodeg,
		     int origsyz, 
		     int lev,
		     int strategy)
{
  level = lev;
  int i;
  const PolynomialRing *R = FFsyz->get_ring()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      ERROR("internal error - ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  GR = R->get_gb_ring();
  M = GR->get_flattened_monoid();
  K = GR->get_flattened_coefficients();

  F = (FreeModule *) ggens->output_free_module();

  gens = ggens;
  syz = NULL;

  Fsyz = (FreeModule *) FFsyz;

  spairs = new s_pair_heap(M);

  gbmatrix = new Matrix(F);

  n_gb = n_mingens = n_subring = 0;
  n_gb_first = 0;
  n_pairs = n_pairs_computed = 0;
  n_pairs_syz = n_pairs_usyz = 0;
  n_pairs_gb = n_pairs_zero = 0;
  n_pairs_gcd = n_pairs_hilb = 0;

  orig_syz = origsyz;		// Note: if orig_syz > 0, then Fsyz is 
				// completely set already.

  this_degree = lodeg;

  for (i=0; i<F->rank(); i++)
    {
      monideal_pair *p = new monideal_pair(GR->get_flattened_ring());
      monideals.append(p);
    }

  use_hilb = 0;
  n_hf = -1;			// This will enable the initial computation of HF.
				// If use_hilb is set.
  hf_comp = NULL;
  n_gb_syz = -1;

  strategy_flags = strategy;
  state = STATE_GENS;
}

gb2_comp::gb2_comp(FreeModule *Fsyz,
		   gb_node *gens,
		   int lodegree,
		   int origsyz,
		   int level,
		   int strat)
{
  setup(Fsyz,gens,lodegree,origsyz,level,strat);
}
void gb2_comp::set_output(gb_node *p) 
{
  syz = p;
  use_hilb = (strategy_flags & USE_HILB) && (syz != NULL);
}
void gb2_comp::remove_pair(s_pair *& p)
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

gb2_comp::~gb2_comp()
{
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////

s_pair *gb2_comp::new_ring_pair(gb_elem *p, const int *lcm)
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

s_pair *gb2_comp::new_s_pair(gb_elem *p, gb_elem *q, const int *lcm)
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


//////////////////////////////////////////////
//  sorting the Groebner basis ///////////////
//////////////////////////////////////////////

int gb2_comp::gb_sort_partition(int lo, int hi)
{
  gb_elem *pivot = gb[lo];
  const int *pivot_monom = pivot->f->monom;
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
#if 0
      do { j--; }
      while (M->compare(gb[j]->f->monom, pivot_monom) > 0);
      do { i++; }
      while (M->compare(gb[i]->f->monom, pivot_monom) < 0);
#endif
      do { j--; }
      while (M->compare(gb[j]->f->monom, pivot_monom) < 0);
      do { i++; }
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
      gb_sort(q+1, hi);
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
  int *find_pairs_exp = new int[M->n_vars()];
  int *find_pairs_lcm = new int[M->n_vars()];

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
	  s_pair *q = new_ring_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q, vplcm));
	}
    }

  // Add in syzygies arising from a base ring

  if (GR->is_quotient_ring())
    {
      for (int i=0; i<GR->n_quotients(); i++)
	{
	  const gbvector * f = GR->quotient_element(i);
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
      s_pair *q = new_s_pair(p, (gb_elem *)(*mi1)[i]->basis_ptr(), find_pairs_lcm);
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
  MonomialIdeal *mi = new MonomialIdeal(R, elems, rejects);
  while (rejects.remove(b))
    {
      s_pair *q = (s_pair *) b->basis_ptr();
      remove_pair(q);
      delete b;
    }

  int is_ideal = (F->rank() == 1 && orig_syz == 0);
  for (j = mi->first(); j.valid(); j++)
    {
      n_pairs++;
      s_pair *q = (s_pair *) (*mi)[j]->basis_ptr();
      if (is_ideal && q->syz_type == SPAIR_PAIR)
	{
	  // MES: the following line is suspect, for Schreyer orders
	  M->gcd(q->first->f->monom, q->second->f->monom, find_pairs_m);
	  if (M->is_one(find_pairs_m))
	    {
	      n_pairs_gcd++;
	      if (comp_printlevel >= 8)
		{
		  buffer o;
		  o << "removed pair[" << q->first->me << " " 
		    << q->second->me << "]" << newline;
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

  // Remove the local variables
  M->remove(find_pairs_m);
  M->remove(f_m);
  delete [] find_pairs_exp;
  delete [] find_pairs_lcm;
}

void gb2_comp::compute_s_pair(s_pair *p)
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
	GR->gbvector_reduce_lead_term(F,Fsyz,
				      0,p->f,p->fsyz,
				      p->second->f, p->second->fsyz);
      M->remove(s);
    }
}

void gb2_comp::gb_reduce(gbvector * &f, gbvector * &fsyz)
{
  if ((strategy_flags & USE_GEOBUCKET) != 0)
    {
      gb_geo_reduce(f,fsyz);
      return;
    }
  gbvector head;
  gbvector *result = &head;
  result->next = 0;

  int *div_totalexp = new int[M->n_vars()];
  int *reduce_ndiv = new int[M->n_vars()];
  int count = 0;
  if (comp_printlevel == 10)
    {
      buffer o;
      o << "reducing ";
      GR->gbvector_text_out(o,F,f);
      emit_line(o.str());
    }
  while (f != NULL)
    {
      Bag *b;
      GR->gbvector_get_lead_exponents(F, f, div_totalexp);
      if (GR->is_quotient_ring() 
	  && GR->get_quotient_monomials()->search_expvector(div_totalexp, b))
	{
	  gbvector *g = (gbvector *) b->basis_ptr();
	  GR->gbvector_reduce_lead_term(F,Fsyz,head.next,f,fsyz,g,0);
	  count++;
	}
      else if (monideals[f->comp]->mi_search->search_expvector(div_totalexp, b))
	{
	  gb_elem *q = (gb_elem *) b->basis_ptr();
	  GR->gbvector_reduce_lead_term(F,Fsyz,head.next,f,fsyz,q->f,q->fsyz);
	  count++;
	  if (comp_printlevel == 10)
	    {
	      buffer o;
	      o << "  reduced by ";
	      GR->gbvector_text_out(o,F,q->f);
	      o << newline;
	      o << "    giving ";
	      GR->gbvector_text_out(o,F,f);
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

  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "." << count;
      emit_wrapped(o.str());
    }
  f = head.next;
  delete [] div_totalexp;
  delete [] reduce_ndiv;
}

void gb2_comp::gb_geo_reduce(gbvector * &f, gbvector * &fsyz)
{
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
      Bag *b;
      GR->gbvector_get_lead_exponents(F, lead, div_totalexp);

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
      else if (monideals[lead->comp]->mi_search->search_expvector(div_totalexp, b))
	{
	  gb_elem *q = (gb_elem *) b->basis_ptr();
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

  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "." << count;
      emit_wrapped(o.str());
    }
  f = head.next;

  fsyz = fsyzb.value();
  delete [] div_totalexp;
  delete [] reduce_ndiv;
}

void gb2_comp::reduce(gbvector * &f, gbvector * &fsyz)
{
  gb_reduce(f,fsyz);
}

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

void gb2_comp::schreyer_append(gbvector * f)
{
  if (orig_syz < 0)
    {
      int *d = R->degree_monoid()->make_one();
      // F->degree(f, d); // MES Aug 2002
      //      Ssyz->append(f->monom, Fsyz->rank());
      // MES: Aug 2002 the previous line needs us to keep Ssyz
      Fsyz->append(d);
      R->degree_monoid()->remove(d);
    }
}
  
void gb2_comp::gb_insert(gbvector * f, gbvector * fsyz, int ismin)
{
  int *f_m = M->make_one();
  gbvector * bull = NULL;
  gb_elem *p = new gb_elem(f, fsyz, ismin);
  ring_elem denom;

  if (orig_syz < 0 && ismin)
    GR->gbvector_remove_content(p->f, bull, denom);
  else
    GR->gbvector_remove_content(p->f, p->fsyz, denom);

  if (ismin) n_mingens++;
  GR->gbvector_get_lead_monomial(F,p->f,f_m);

  if (M->in_subring(1,f_m))
    n_subring++;
  // insert into p->f->comp->mi_search
  intarray vp;
  M->to_varpower(f_m, vp);
  monideals[p->f->comp]->mi_search->insert(new Bag(p, vp));
  gb.append(p);
  M->remove(f_m);
  // Now we must be a bit careful about this next, but we only want one
  // copy of a GB element, since the whole thing can be quite large.
  // Just make sure that when the GB is deleted at the end, that the 'f'
  // field of the gb_elem's is not removed.

#if 0
  // MES Aug 2002: what to do with this gbmatrix?
  gbmatrix->append(p->f);
#endif

  // Now do auto-reduction of previous elements using this one.
  // MES: possible fix: only do this if the current element is not minimal
  if (orig_syz >= 0 || !ismin)
    for (int i=n_gb_first; i<n_gb; i++)
      {
	// Now compute gb(i) := gb(i) - c gb(j), where
	// c in(gb(j)) is a term in gb(i).
	// Also compute change(i) -= c change(j).
	
	GR->gbvector_auto_reduce(F,Fsyz,
				 gb[i]->f, gb[i]->fsyz, 
				 p->f, p->fsyz);
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
  if (use_hilb && n_gb_syz == 0)
    flush_pairs();
  if (these_pairs == NULL) return false; // Done
  s_pair *p = these_pairs;
  these_pairs = these_pairs->next;

  n_pairs_computed++;
  compute_s_pair(p);
  
  gbvector * f = p->f;
  gbvector * fsyz = p->fsyz;
  p->f = NULL;
  p->fsyz = NULL;
  remove_pair(p);

  gb_reduce(f, fsyz);
  if (f != NULL)
    {
      gb_insert(f, fsyz, 0);
      n_gb_syz--;
      n_pairs_gb++;
      if (comp_printlevel >= 3) emit_wrapped("m");
    }
  else if (fsyz != NULL && syz != NULL)
    {
      if (syz->receive_generator(fsyz,n_syz++,GR->one()))
	{
	  n_gb_syz--;
	  n_pairs_syz++;
	  if (comp_printlevel >= 3) emit_wrapped("z");
	}
      else
	{
	  n_pairs_usyz++;
	  if (comp_printlevel >= 3) emit_wrapped("u");
	}
    }
  else
    {
      if (fsyz != NULL) GR->gbvector_remove(fsyz);
      n_pairs_zero++;
      if (comp_printlevel >= 3) emit_wrapped("o");
    }
  return true;
}

///////////////////////////
// Hilbert function use ///
///////////////////////////


//---- Completion testing -----------------------------

int gb2_comp::computation_complete(int stop_gb, int /*stop_syz*/, 
				  int /*stop_codim*/,
				  int stop_pairs, int /*stop_min_gens*/,
				  int stop_subring)
     // Test whether the current computation is done.
     // Return COMP_DONE_DEGREE_LIMIT, COMP_DONE, COMP_DONE_GB_LIMIT, COMP_DONE_SYZ_LIMIT,
     // COMP_DONE_PAIR_LIMIT, COMP_DONE_CODIM, COMP_DONE_MIN_GENS, or
     // (if not done) COMP_COMPUTING.
{
  if (stop_gb > 0 && n_gb >= stop_gb) return COMP_DONE_GB_LIMIT;
  //  if (stop_syz > 0 && syz->value().n_cols() >= stop_syz) return COMP_DONE_SYZ_LIMIT;
  if (stop_pairs > 0 && n_pairs_computed >= stop_pairs) return COMP_DONE_PAIR_LIMIT;
  //if (stop_codim > 0 && ...) return COMP_DONE_CODIM;
  if (stop_subring > 0 && n_subring >= stop_subring) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

bool gb2_comp::receive_generator(gbvector *f, int n, const ring_elem denom)
{
  bool isgen = false;
  // It is our duty to free 'f'...

  for (int i=monideals.length(); i<F->rank(); i++)
    {
      monideal_pair *p = new monideal_pair(R);
      monideals.append(p);
    }

  gbvector * fsyz = NULL;
  if (orig_syz >= 0)
    {
      if (orig_syz > n)
	fsyz = GR->gbvector_term(Fsyz,denom,n);
      gb_reduce(f,fsyz);
      if (f == NULL)
	{
	  if (fsyz != NULL && syz != NULL)
	    syz->receive_generator(fsyz, n_syz++, GR->one());
	}
      else
	{
	  isgen = true;
	  gb_insert(f,fsyz,1);
	}
    }
  else
    {
      ring_elem denom;
      gb_reduce(f,fsyz);
      GR->gbvector_remove(fsyz);
      GR->gbvector_remove_content(f,NULL,denom);
      if (f != NULL)
	{
	  //schreyer_append(f);
	  isgen = true;
	  //gb_insert(f,Fsyz->e_sub_i(n_mingens),1);
	  // The fsyz part will be set at the end of the degree,
	  // after sorting takes place, and after auto-reduction in
	  // this degree.
	  gb_insert(f,NULL,1);
	}
    }
  return isgen;
}

void gb2_comp::end_degree()
{
  if ((strategy_flags & USE_SORT) != 0)
    {
      gb_sort(n_gb_first, n_gb-1); // This is the range of elements to sort.
    }
  for (int j=n_gb_first; j < n_gb; j++)
    gb[j]->me = j;
  if (orig_syz < 0)
    {
      for (int j=n_gb_first; j < n_gb; j++)
	if (gb[j]->is_min)
	  {
	    schreyer_append(gb[j]->f);
	    gb[j]->fsyz = GR->gbvector_term(Fsyz,GR->one(),Fsyz->rank()-1);
	  }
    }

  // Now set the state so that we know we have finished here
  this_degree++;
  state = STATE_NEW_DEGREE;
}

int gb2_comp::calc_gb(int deg, const intarray &stop)
{
  int ret = COMP_DONE;
  if (this_degree > deg) return COMP_DONE;
  if (state == STATE_DONE) return COMP_DONE; // This includes knowledge
				// that there will be no new generators.
  if (this_degree < deg) 
    {
      // Now make sure that previous computations have been done:
      ret = calc_gens(deg-1, stop);
      if (ret != COMP_DONE) return ret;
    }
  // At this point, we have completely computed a GB with new gens
  // in this degree.  Depending on whether we stopped
  // prematurely, our state will be one of STATE_NEW_DEGREE,
  // STATE_GB, STATE_GENS.

  int n1,n2;
  if (state == STATE_NEW_DEGREE)
    {
      if (use_hilb)
	{
	  //int n1,n2;
	  if (syz != NULL)
	    {
	      ret = syz->hilbertNumeratorCoefficient(this_degree, n1);
	      if (ret != COMP_DONE)
		{
		  return ret;
		}
	    }
	  else 
	    n1 = 0;
	  ret = hilbertNumeratorCoefficient(this_degree, n2);
	  if (ret != COMP_DONE)
	    {
	      return ret;
	    }
	  n_gb_syz = n1 + n2;
	}

      // Compute new s-pairs
      for (int i=n_gb_first; i<n_gb; i++)
	find_pairs(gb[i]);

      state = STATE_GB;
      n_gb_first = n_gb;
      int npairs = get_pairs();
      if (comp_printlevel >= 1 && npairs > 0)
	{
	  buffer o;
	  // Should only display this if there are some pairs.
	  o << '[' << level << ',' << npairs;
	  if (use_hilb)
	    o << ",e" << n_gb_syz << "," << n1 << "," << n2;
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
	  system_spincursor();
	  if (system_interrupted) 
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
      ret = gens->calc_gb(deg, stop);
      if (ret == COMP_DONE)
	{
	  // Cleanup, go to next degree.
	  end_degree();
	}
    }
  // MES: put out an endl if comp_printlevel >= 1?

  // Is this where we should compute the HF again: for use by the 
  // previous node... Do we really have to compute it twice per degree/level
  // node?

  return ret;
}

int gb2_comp::calc_gens(int deg, const intarray &stop)
{
  int ret;
  // First check whether we have done this:
  if (this_degree > deg) return COMP_DONE;

  // First make sure that we have a GB here:
  ret = calc_gb(deg, stop);
  if (ret != COMP_DONE) return ret;

  // Go get the generators:
  ret = gens->calc_gb(deg, stop);
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

int gb2_comp::hilbertNumerator(RingElement *&result)
{
  // It is possible that the computation was not completed before.
  if (n_hf == n_gb)
    {
      // No more is needed to be computed
      result = hf;
      return COMP_DONE;
    }

  if (hf_comp == NULL)
    hf_comp = new hilb_comp(R->HilbertRing(), gbmatrix);

  int retval = hf_comp->calc(-1);
  if (retval != COMP_DONE) return retval;
  hf = hf_comp->value();
  result = hf;
  delete hf_comp;
  hf_comp = NULL;
  n_hf = n_gb;
  return COMP_DONE;
}

int gb2_comp::hilbertNumeratorCoefficient(int deg, int &result)
{
  RingElement *f;
  int ret = hilbertNumerator(f);
  if (ret != COMP_DONE) return ret;
  result = hilb_comp::coeff_of(f, deg);
  return COMP_DONE;
}

//--- Obtaining matrices as output -------
Matrix *gb2_comp::min_gens_matrix()
{
  Matrix *result = new Matrix(F,Fsyz);
  int j = 0;
  for (int i=0; i<gb.length(); i++)
    if (gb[i]->is_min)
      (*result)[j++] = GR->gbvector_to_vec(F,gb[i]->f);
  return result;
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
  Matrix *result = new Matrix(F);
  for (int i=0; i<gb.length(); i++)
    {
      gbvector *tmp = GR->gbvector_lead_term(n, F, gb[i]->f);
      result->append(GR->gbvector_to_vec(F,tmp));
      GR->gbvector_remove(tmp);
    }
  return result;
}

Matrix *gb2_comp::gb_matrix()
{
  Matrix *result = new Matrix(F);
  for (int i=0; i<gb.length(); i++)
    result->append(GR->gbvector_to_vec(F,gb[i]->f));
  return result;
}

Matrix *gb2_comp::change_matrix()
{
  Matrix *result = new Matrix(Fsyz);
  for (int i=0; i<gb.length(); i++)
    result->append(GR->gbvector_to_vec(Fsyz,gb[i]->fsyz));
  return result;
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
  if (q->first != NULL) o << q->first->me; else o << ".";
  o << " ";
  if (q->second != NULL) o << q->second->me; else o << ".";
  o << " ";
  M->elem_text_out(o, q->lcm);
  o << ") ";
}

void gb2_comp::stats() const
{
  buffer o;
  if (comp_printlevel >= 4 && n_gb > 0)
    {
      int nmonoms = 0;
      int nchange = 0;
      for (int i=0; i<gb.length(); i++)
	{
	  nmonoms += GR->gbvector_n_terms(gb[i]->f);
	  nchange += GR->gbvector_n_terms(gb[i]->fsyz);
	}
      o.put(n_gb, 5);              o.put(" ");
      o.put(n_pairs, 5);           o.put(" ");
      o.put(n_pairs_computed, 5);  o.put(" ");
      o.put(n_pairs_gb, 5);        o.put(" ");
      o.put(n_pairs_syz, 5);       o.put(" ");
      o.put(n_pairs_zero, 5);      o.put(" ");
      o.put(n_pairs_usyz, 5);      o.put(" ");
      o.put(n_pairs_hilb, 5);      o.put(" ");
      o.put(n_pairs_gcd, 5);       o.put(" ");
      o.put(nmonoms, 5);           o.put(" ");
      o.put(nchange, 5);           o.put(newline);
      emit(o.str());
      o.reset();
    }

  spairs->stats();
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    for (int i=0; i<gb.length(); i++)
      {
	o.reset();
	o << i << '\t';
	GR->gbvector_text_out(o, F, gb[i]->f);
	o << newline;
	emit(o.str());
      }

}


