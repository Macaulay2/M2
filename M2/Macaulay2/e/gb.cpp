// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "gb.hpp"
#include "hilb.hpp"
#include "geovec.hpp"
#include "text_io.hpp"

int comp_printlevel = 0;

extern ring_elem hilb(const Matrix &M, const Ring *RR);

stash *GB_comp::mystash;

void GB_comp::set_up0(const Matrix &m, int csyz, int nsyz)
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
  gens = new s_pair_heap(M);

  gbmatrix = Matrix(m.rows());

  if (nsyz < 0 || nsyz > m.n_cols())
    nsyz = m.n_cols();
  n_comps_per_syz = nsyz;

  F = m.rows();
  bump_up(F);

  ar_i = ar_j = np_i = -1;

  n_gb = n_mingens = n_subring = 0;
  n_pairs = n_computed = n_saved_gcd = 0;
  n_gens_left = 0;

  collect_syz = csyz;
  is_ideal = (F->rank() == 1 && csyz == 0);
  if (R->cast_to_WeylAlgebra() != 0)
    is_ideal = false;

  use_hilb = false;
  n_saved_hilb = 0;
  n_in_degree = 0;
  hilb_step = 0;
  hf_comp = NULL;

  // set local variables for certain time-critical routines

  this_degree = F->lowest_primary_degree() - 1;

  for (i=0; i<F->rank(); i++)
    {
      monideal_pair *p = new monideal_pair(R);
      monideals.append(p);
    }
}

void GB_comp::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  int i;
  strategy = strat;

  set_up0(m, csyz, nsyz);

  Fsyz = m.cols()->sub_space(n_comps_per_syz);  
  bump_up(Fsyz);
  syz = Matrix(Fsyz);

  state = GB_COMP_NEWDEGREE;

  for (i=0; i<m.n_cols(); i++)
    {
      s_pair *p = new_gen(i, m[i]);
      if (p != NULL)
	{
	  gens->insert(p);
	  n_gens_left++;
	}
    }
}

void GB_comp::force(const Matrix &m, const Matrix &gb, const Matrix &mchange,
		    const Matrix &msyz)
{
  int csyz = (msyz.n_cols() > 0);
  set_up0(m, csyz, mchange.n_rows());

  Fsyz = mchange.rows();
  bump_up(Fsyz);
  syz = msyz;

  state = GB_COMP_DONE;

  for (int i=0; i<gb.n_cols(); i++)
    {
      if (gb[i] == NULL) continue;
      vec f = F->copy(gb[i]);
      vec fsyz = Fsyz->copy(mchange[i]);
      gb_insert(f,fsyz,0);
    }
}

GB_comp::GB_comp(const Matrix &m, int csyz, int nsyz, int strat)
  : gb_comp(COMP_GB)
{
  set_up(m, csyz, nsyz, strat);
}

GB_comp::GB_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
		 const Matrix &syz)
  : gb_comp(COMP_GB)
{
  force(m, gb, mchange, syz);
}

GB_comp::GB_comp(const Matrix &m, int csyz, int nsyz, 
		 RingElement hf, int strat)
  : gb_comp(COMP_GB)
{
  // MES
  set_up(m, csyz, nsyz, strat);
  hf_orig = hf;
  hf_diff = RingElement(hf.get_ring());
  use_hilb = true;
  hilb_step = 1;
}

void GB_comp::remove_pair(s_pair *& p)
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

GB_comp::~GB_comp()
{
  int i;

  // remove spairs
  s_pair *p;
  while ((p = gens->remove()) != NULL)
    remove_pair(p);
  while ((p = spairs->remove()) != NULL)
    remove_pair(p);
  delete spairs;
  delete gens;

  // remove the gb
  for (i=0; i<gb.length(); i++)
    {
      // Don't remove the 'f' field of 'gb[i]', since this is also pointed
      // to by 'gbmatrix'.
      gb[i]->f = NULL;
      Fsyz->remove(gb[i]->fsyz);
      delete gb[i];
    }

  // remove the monideals: we don't need to delete the s_pairs
  // contained in each one, since they exist in the 'gb' array.
  for (i=0; i<monideals.length(); i++)
    delete monideals[i];

  // remove the hilbert computation, if needed.  We don't need to remove the
  // ring elements hf_orig, hf_diff, since they are reference counted.

  // Finally, decrement ref counts (monoids are not ref counted currently)
  delete hf_comp;
  bump_down(F);
  bump_down(Fsyz);
}

void GB_comp::resize(int /*nbits*/)
     // Resizes all (packed) monomials, and polynomials
     // to work in at least the next degree.
{
  // MES
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////

s_pair *GB_comp::new_var_pair(gb_elem *p, const int *lcm)
{
  return new_ring_pair(p,lcm);
}

s_pair *GB_comp::new_ring_pair(gb_elem *p, const int *lcm)
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

s_pair *GB_comp::new_s_pair(gb_elem *p, gb_elem *q, const int *lcm)
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

  result->lcm = M->make_one();
  M->lcm(p->f->monom, q->f->monom, result->lcm);

  return result;
}

s_pair *GB_comp::new_gen(int i, const vec f)
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

//////////////////////////////////////////////
//  sorting the Groebner basis ///////////////
//////////////////////////////////////////////

int GB_comp::gb_sort_partition(int lo, int hi)
{
  gb_elem *pivot = gb[lo];
  const int *pivot_monom = pivot->f->monom;
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (M->compare(gb[j]->f->monom, pivot_monom) > 0);

      do { i++; }
      while (M->compare(gb[i]->f->monom, pivot_monom) < 0);

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

void GB_comp::gb_sort(int lo, int hi)
{
  if (lo < hi)
    {
      int q = gb_sort_partition(lo, hi);
      gb_sort(lo, q);
      gb_sort(q+1, hi);
    }
}

void GB_comp::find_pairs(gb_elem *p)
  // compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
  // (includes cases m * lead(p) = 0).
  // Returns a list of new s_pair's.
{
  queue<Bag *> elems;
  Index<MonomialIdeal> j;
  intarray vplcm;
  int *find_pairs_m = M->make_one();
  int *find_pairs_exp = new int[M->n_vars()];
  int *find_pairs_lcm = new int[M->n_vars()];

  if (M->is_skew())
    {
      int *skewvars = new int[M->n_vars()];
      M->to_expvector(p->f->monom, find_pairs_exp);
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
	  M->lcm(f->monom, p->f->monom, find_pairs_lcm);
	  vplcm.shrink(0);
	  M->to_varpower(find_pairs_lcm, vplcm);
	  s_pair *q = new_ring_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q, vplcm));
	}
    }
  // Add in syzygies arising as s-pairs
  MonomialIdeal &mi1 = monideals[p->f->comp]->mi;
  for (Index<MonomialIdeal> i = mi1.first(); i.valid(); i++)
    {
      M->from_varpower(mi1[i]->monom().raw(), find_pairs_m);
      M->lcm(find_pairs_m, p->f->monom, find_pairs_lcm);
      vplcm.shrink(0);
      M->to_varpower(find_pairs_lcm, vplcm);
      s_pair *q = new_s_pair(p, (gb_elem *)mi1[i]->basis_ptr(), find_pairs_lcm);
      elems.insert(new Bag(q, vplcm));
    }

  // Add 'p' to the correct monideal
  intarray vp;
  M->to_varpower(p->f->monom, vp);
  mi1.insert(new Bag(p, vp));

  // Now minimalize these elements, and insert them into
  // the proper degree.

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal mi(R, elems, rejects);
  while (rejects.remove(b))
    {
      s_pair *q = (s_pair *) b->basis_ptr();
      remove_pair(q);
      delete b;
    }
  for (j = mi.first(); j.valid(); j++)
    {
      s_pair *q = (s_pair *) mi[j]->basis_ptr();
      if (is_ideal && q->syz_type == SPAIR_PAIR)
	{
	  M->gcd(q->first->f->monom, q->second->f->monom, find_pairs_m);
	  if (M->is_one(find_pairs_m))
	    {
	      n_saved_gcd++;
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
  delete [] find_pairs_exp;
  delete [] find_pairs_lcm;
}

void GB_comp::compute_s_pair(s_pair *p)
{
  if (p->f == NULL)
    {
      int *s = M->make_one();
      ring_elem one = R->Ncoeffs()->from_int(1);
      M->divide(p->lcm, p->first->f->monom, s);
      //p->f = F->mult_by_monomial(s, p->first->f);
      //p->fsyz = Fsyz->mult_by_monomial(s, p->first->fsyz);
      p->f = F->imp_mult_by_term(one, s, p->first->f);
      p->fsyz = Fsyz->imp_mult_by_term(one, s, p->first->fsyz);
      if (Fsyz->is_quotient_ring) Fsyz->normal_form(p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
	{
	  ring_elem coeff;
	  F->imp_cancel_lead_term(p->f, p->second->f, coeff, s);
	  Fsyz->subtract_multiple_to(p->fsyz, coeff, s, p->second->fsyz);
	  R->Ncoeffs()->remove(coeff);

	  //ring_elem a = R->Ncoeffs()->from_int(1);
	  //M->divide(p->lcm, p->second->f->monom, s);
	  //F->imp_subtract_multiple_to(p->f, a, s, p->second->f);
	  //Fsyz->subtract_multiple_to(p->fsyz, a, s, p->second->fsyz);
	  //R->Ncoeffs()->remove(a);
	}
      M->remove(s);
      R->Ncoeffs()->remove(one);
    }
}

void GB_comp::gb_reduce(vec &f, vec &fsyz)
{
  if (((strategy & USE_GEOBUCKET) != 0) && !M->is_skew())
    {
      gb_geo_reduce(f,fsyz);
      return;
    }
  vecterm head;
  vecterm *result = &head;
  ring_elem coeff;

  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());
  int count = 0;
  while (f != NULL)
    {
      Bag *b;
      M->to_expvector(f->monom, div_totalexp);
      if (R->is_quotient_ring() 
	  && R->get_quotient_monomials().search_expvector(div_totalexp, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  F->imp_ring_cancel_lead_term(f, g, coeff, reduce_ndiv);
	  R->Ncoeffs()->remove(coeff);
	  //M->divide(f->monom, g->monom, reduce_ndiv);
	  //F->imp_subtract_ring_multiple_to(f, f->coeff, reduce_ndiv, g);
	  count++;
	}
      else if (monideals[f->comp]->mi_search.search_expvector(div_totalexp, b))
	{
	  gb_elem *q = (gb_elem *) b->basis_ptr();
	  F->imp_cancel_lead_term(f, q->f, coeff, reduce_ndiv);
	  Fsyz->subtract_multiple_to(fsyz, coeff, reduce_ndiv, q->fsyz);
	  R->Ncoeffs()->remove(coeff);

	  //ring_elem c = f->coeff;
	  //M->divide(f->monom, q->f->monom, reduce_ndiv);
	  //Fsyz->subtract_multiple_to(fsyz, c, reduce_ndiv, q->fsyz);
	  //F->subtract_multiple_to(f, c, reduce_ndiv, q->f);
	  // NOTE!! the above 'c' is in 'f', so make sure not to use it
	  // after this last line!
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
      emit_wrapped(o.str());
    }
  result->next = NULL;
  f = head.next;
}

void GB_comp::gb_geo_reduce(vec &f, vec &fsyz)
{
  vecterm head;
  vecterm *result = &head;

  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());
  int count = 0;

  vecHeap fb(F);
  vecHeap fsyzb(Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);
  const vecterm *lead;
  while ((lead = fb.get_lead_term()) != NULL)
    {
      Bag *b;
      M->to_expvector(lead->monom, div_totalexp);
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
      else if (monideals[lead->comp]->mi_search.search_expvector(div_totalexp, b))
	{
	  gb_elem *q = (gb_elem *) b->basis_ptr();
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
      emit_wrapped(o.str());
    }
  result->next = NULL;
  f = head.next;

  fsyz = fsyzb.value();
}

void GB_comp::flush_pairs(int deg)
{
  s_pair *p;
  while ((p = spairs->remove()) != NULL)
    if (p->degree != deg) 
      {
	spairs->put_back(p);
	break;
      }
    else
      {
	n_saved_hilb++;
	remove_pair(p);
      }
  while ((p = gens->remove()) != NULL)
    if (p->degree != deg) 
      {
	gens->put_back(p);
	break;
      }
    else
      {
	n_saved_hilb++;
	remove_pair(p);
      }
}

void GB_comp::gb_insert(vec f, vec fsyz, int ismin)
{
  gb_elem *p = new gb_elem(f, fsyz, ismin);

  F->make_monic(p->f, p->fsyz);
  if (ismin)
    {
      n_mingens++;
      p->is_min = 1;
    }
  if (M->in_subring(1,p->f->monom))
    n_subring++;
  // insert into p->f->comp->mi_search
  intarray vp;
  M->to_varpower(p->f->monom, vp);
  monideals[p->f->comp]->mi_search.insert(new Bag(p, vp));
  n_gb++;
  gb.append(p);

  // Now we must be a bit careful about this next, but we only want one
  // copy of a GB element, since the whole thing can be quite large.
  // Just make sure that when the GB is deleted at the end, that the 'f'
  // field of the gb_elem's is not removed.

  gbmatrix.append(p->f);

  if (use_hilb)
    {
      hilb_step = 1;
      if (--n_in_degree == 0) flush_pairs(this_degree);
    }
}

int GB_comp::s_pair_step()
     // If no s-pairs left in the current degree, 
     // return SPAIR_DONE.
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return one of the other SPAIR_*
     // values.
{
  s_pair *p = spairs->remove();
  if (p == NULL) return SPAIR_DONE;
  if (p->degree != this_degree) 
    {
      spairs->put_back(p);
      return SPAIR_DONE;
    }

  n_computed++;
  compute_s_pair(p);
  
  vec f = p->f;
  vec fsyz = p->fsyz;
  p->f = NULL;
  p->fsyz = NULL;
  remove_pair(p);

  gb_reduce(f, fsyz);
  if (f != NULL)
    {
      gb_insert(f, fsyz, 0);
      return SPAIR_GB;
    }
  if (fsyz != NULL)
    {
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

int GB_comp::gen_step()
     // If no gens left in the current degree, 
     // return SPAIR_DONE.
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return one of the other SPAIR_*
     // values.
{
  s_pair *p = gens->remove();
  if (p == NULL) return SPAIR_DONE;
  if (p->degree != this_degree) 
    {
      gens->put_back(p);
      return SPAIR_DONE;
    }

  n_computed++;
  n_gens_left--;

  compute_s_pair(p);
  
  vec f = p->f;
  vec fsyz = p->fsyz;
  p->f = NULL;
  p->fsyz = NULL;
  remove_pair(p);

  gb_reduce(f, fsyz);
  if (f != NULL)
    {
      gb_insert(f, fsyz, 1);	// 1 = minimal generator
      return SPAIR_MINGEN;
    }
  if (fsyz != NULL)
    {
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

bool GB_comp::auto_reduce_step()
     // Using ar_i, ar_j, reduce the gb element ar_i wrt ar_j.
     // Increment ar_i, ar_j as needed. If done, return false.
{
  if (ar_j >= n_gb)
    {
      ar_i++;
      ar_j = ar_i + 1;
      if (ar_j >= n_gb) return false;
    }
  // Now compute gb(i) := gb(i) - c gb(j), where
  // c in(gb(j)) is a term in gb(i).
  // Also compute change(i) -= c change(j).
  
  F->auto_reduce(Fsyz, gb[ar_i]->f, gb[ar_i]->fsyz, 
			   gb[ar_j]->f, gb[ar_j]->fsyz);
  ar_j++;
  return true;
}

bool GB_comp::new_pairs_step()
     // Compute the new s-pairs associated to the given gb element.
     // Increment 'np_i'.  If done with all pairs in this 
     // degree, return false.
{
  if (np_i >= n_gb) return false;
  find_pairs(gb[np_i]);
  np_i++;
  return true;
}

//---- Hilbert function use ---------------------------

int GB_comp::coeff_of(const RingElement &h, int deg) const
{
  // This is a bit of a kludge of a routine.  The idea is to loop through
  // all the terms of the polynomial h, expand out the exponent, and to add
  // up the small integer values of the coefficients of those that have exp[0]=deg.
  const PolynomialRing *P = h.get_ring()->cast_to_PolynomialRing();

  int *exp = new int[P->n_vars()];
  int result = 0;
  for (Nterm *f = h.get_value(); f!=NULL; f=f->next)
    {
      P->Nmonoms()->to_expvector(f->monom, exp);
      if (exp[0] < deg) 
	{
	  ERROR("Incorrect Hilbert function given");
	}
      else if (exp[0] == deg)
	{
	  int n = P->Ncoeffs()->coerce_to_int(f->coeff);
	  result += n;
	}
	
    }
  delete [] exp;
  return result;
}

//---- Completion testing -----------------------------

int GB_comp::computation_complete(const int * /* stop_degree */,
				  int stop_gb, 
				  int stop_syz, 
				  int stop_pairs,
				  int /* stop_codim */,
				  int stop_min_gens,
				  int stop_subring)
     // Test whether the current computation is done.
     // Return COMP_DONE_DEGREE_LIMIT, COMP_DONE, COMP_DONE_GB_LIMIT, COMP_DONE_SYZ_LIMIT,
     // COMP_DONE_PAIR_LIMIT, COMP_DONE_CODIM, COMP_DONE_MIN_GENS, or
     // (if not done) COMP_COMPUTING.
{
  if (state == GB_COMP_DONE) 
    {
#if 0
      if (stop_degree != NULL && n_computed != n_pairs)
	{
	  state = GB_COMP_NEWDEGREE;
	  return COMP_DONE_DEGREE_LIMIT;
	}
#endif
      return COMP_DONE;
    }
  if (stop_gb > 0 && n_gb >= stop_gb) return COMP_DONE_GB_LIMIT;
  if (stop_syz > 0 && syz.n_cols() >= stop_syz) return COMP_DONE_SYZ_LIMIT;
  if (stop_pairs > 0 && n_computed >= stop_pairs) return COMP_DONE_PAIR_LIMIT;
  //if (stop_codim > 0 && ...) return COMP_DONE_CODIM;
  if (stop_min_gens && n_gens_left == 0) return COMP_DONE_MIN_GENS;
  if (stop_subring > 0 && n_subring >= stop_subring) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

int GB_comp::next_degree()
{
  s_pair *p, *q;
  int result = 0;
  p = spairs->remove();
  q = gens->remove();
  if (p != NULL)
    {
      result = p->degree;
      if (q != NULL && q->degree < p->degree)
	result = q->degree;
    }
  else if (q != NULL)
    result = q->degree;
  else
    assert(0);
  if (p != NULL) spairs->put_back(p);
  if (q != NULL) gens->put_back(q);
  return result;
}
//---- state machine (roughly) for the computation ----

int GB_comp::calc(const int *deg, const intarray &stop)
{
  if (stop.length() != 7) 
    {
      gError << "inappropriate stop conditions for GB computation";
      return COMP_ERROR;
    }
  const int *stop_degree = deg;
  int stop_gb = stop[0]; //ngb
  int stop_syz = stop[1]; //nsyz
  int stop_pairs = stop[2]; //npairs
  int stop_codim = stop[3]; //cod
  int stop_min_gens = stop[4]; //do_min
  int stop_subring = stop[5]; //#elems in (first) subring
  int is_done = COMP_COMPUTING;
  
  for (;;)
    {
      if (is_done != COMP_COMPUTING) break;
      is_done = computation_complete(stop_degree,
				     stop_gb, stop_syz, stop_pairs, 
				     stop_codim, stop_min_gens, stop_subring);
      if (is_done != COMP_COMPUTING) break;
      system_spincursor();
      if (system_interrupted) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}
      
      switch (state) 
	{
	case GB_COMP_NEWDEGREE:
	  if (spairs->n_elems() == 0 && gens->n_elems() == 0)
	    {
	      state = GB_COMP_DONE;
	      is_done = COMP_DONE;
	      break;
	    }
	  this_degree = next_degree();
	  if (stop_degree && this_degree > *stop_degree)
	    {
	      is_done = COMP_DONE_DEGREE_LIMIT;
	      break;
	    }


	  if (use_hilb)
	    {
	      if (hilb_step > 0)
		{
		  if (hilb_step == 1)
		    {
		      // Set up the Hilbert function computation
		      delete hf_comp;
		      hf_comp = new hilb_comp(hf_orig.get_ring()->cast_to_PolynomialRing(), 
					      gbmatrix);
		      hilb_step = 2;
		    }
		  // recompute Hilbert function
		  if (hf_comp->calc(-1) == COMP_INTERRUPTED)
		    {
		      is_done = COMP_INTERRUPTED;
		      break;
		    }
		  
		  // At this point we have a completed Hilbert polynomial
		  RingElement h = hf_comp->value();
		  hf_diff = h - hf_orig;
		  hilb_step = 0;
		}
	      n_in_degree = coeff_of(hf_diff, this_degree);
	      if (n_in_degree == 0) flush_pairs(this_degree);
	    }
	  if (comp_printlevel >= 1)
	    {
	      buffer o;
	      o << '{' << this_degree << '}';
	      o << '(';
	      if (use_hilb) 
		o << n_in_degree << ',';
	      o << spairs->n_elems() << ')';
	      emit_wrapped(o.str());
	    }

	  // Set state information for auto reduction, new pairs
	  ar_i = n_gb;
	  ar_j = ar_i + 1;
	  np_i = n_gb;
	  state = GB_COMP_S_PAIRS;
	  break;
	  
	case GB_COMP_S_PAIRS:
	  if (comp_printlevel < 2)
	    {
	      if (s_pair_step() == SPAIR_DONE) 
		state = GB_COMP_GENS;
	    }
	  else switch (s_pair_step()) 
	    {
	    case SPAIR_MINGEN:
	      emit_wrapped("g");
	      break;
	    case SPAIR_GB:
	      emit_wrapped("m");
	      break;
	    case SPAIR_SYZ:
	      emit_wrapped("z");
	      break;
	    case SPAIR_ZERO:
	      emit_wrapped("o");
	      break;
	    case SPAIR_DONE:
	      state = GB_COMP_GENS;
	      break;
	    default:
	      emit_wrapped("ERROR");
	      break;
	    }
	  break;

	case GB_COMP_GENS:
	  if (comp_printlevel < 2)
	    {
	      if (gen_step() == SPAIR_DONE) 
		state = GB_COMP_AUTO_REDUCE;
	    }
	  else switch (gen_step()) 
	    {
	    case SPAIR_MINGEN:
	      emit_wrapped("g");
	      break;
	    case SPAIR_SYZ:
	      emit_wrapped("z");
	      break;
	    case SPAIR_ZERO:
	      emit_wrapped("o");
	      break;
	    case SPAIR_DONE:
	      state = GB_COMP_AUTO_REDUCE;
	      break;
	    default:
	      emit_wrapped("ERROR");
	      break;
	    }
	  break;
	  
	case GB_COMP_AUTO_REDUCE:
	  if (!auto_reduce_step()) 
	    {
	      state = GB_COMP_NEWPAIRS;
	      if ((strategy & USE_SORT) != 0)
		{
		  gb_sort(np_i, n_gb-1); // This is the range of elements to sort.
		}
	      for (int j=np_i; j < n_gb; j++)
		gb[j]->me = j;
	    }
	  break;
	  
	case GB_COMP_NEWPAIRS:
	  if (!new_pairs_step()) state = GB_COMP_NEWDEGREE;
	  break;
	  
	case GB_COMP_DONE:
	  break;

	case GB_COMP_NEED_RESIZE:
          is_done = COMP_NEED_RESIZE;
	  break;
	}
    }
  if (comp_printlevel >= 1) emit_line("");
  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "Number of pairs             = " << n_pairs << newline;
      o << "Number of gb elements       = " << n_gb << newline;
      o << "Number of gcd=1 pairs       = " << n_saved_gcd << newline;
      o << "Number of pairs computed    = " << n_computed << newline;
      emit(o.str());
    }
  return is_done;
}

//--- Reduction --------------------------
Matrix GB_comp::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols(), m.degree_shift());
  lift = Matrix(Fsyz, m.cols());
  if (m.n_rows() != F->rank()) {
       gError << "expected matrices to have same number of rows";
       return red;
  }
  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = F->translate(m.rows(),m[i]);
      vec fsyz = NULL;

      gb_reduce(f, fsyz);
      Fsyz->negate_to(fsyz);
      red[i] = f;
      lift[i] = fsyz;
    }
  return red;
}

int GB_comp::contains(const Matrix &m)
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
bool GB_comp::is_equal(const gb_comp *q)
{
  if (kind() != q->kind()) return false;
  GB_comp *that = (GB_comp *) q;
  if (this->F->rank() != that->F->rank()) return false;

  // Loop through every GB element: in each monideal[i]->mi_search
  for (int i=0; i<F->rank(); i++)
    {
      Index<MonomialIdeal> j1 = this->monideals[i]->mi_search.first();
      Index<MonomialIdeal> j2 = that->monideals[i]->mi_search.first();
      for (;j1.valid() && j2.valid();j1++, j2++)
	{
	  gb_elem *f1 = (gb_elem *) (this->monideals[i]->mi_search)[j1]->basis_ptr();
	  gb_elem *f2 = (gb_elem *) (that->monideals[i]->mi_search)[j2]->basis_ptr();
	  if (!F->is_equal(f1->f,f2->f))
	    return false;
	}
      if (j1.valid() || j2.valid())
	return false;
    }
  return true;
}

Vector GB_comp::reduce(const Vector &v, Vector &lift)
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

//--- Obtaining matrices as output -------
Matrix GB_comp::min_gens_matrix()
{
  Matrix result(F);
  for (int i=0; i<gb.length(); i++)
    if (gb[i]->is_min)
      result.append(F->copy(gb[i]->f));
  return result;
}

Matrix GB_comp::initial_matrix(int n)
{
  Matrix result(F);
  for (int i=0; i<gb.length(); i++)
    result.append(F->lead_term(n, gb[i]->f));
  return result;
}

Matrix GB_comp::gb_matrix()
{
  return gbmatrix;
//  Matrix result(F);
//  for (int i=0; i<gb.length(); i++)
//    result.append(F->copy(gb[i]->f));
//  return result;
}

Matrix GB_comp::change_matrix()
{
  Matrix result(Fsyz);
  for (int i=0; i<gb.length(); i++)
    result.append(Fsyz->copy(gb[i]->fsyz));
  return result;
}

Matrix GB_comp::syz_matrix()
{
  return syz;
}

void GB_comp::debug_out(s_pair *q) const
{
  if (q == NULL) return;
  buffer o;
  o << "(" << q->compare_num << " ";
  if (q->first != NULL) o << q->first->me; else o << ".";
  o << " ";
  if (q->second != NULL) o << q->second->me; else o << ".";
  o << " ";
  M->elem_text_out(o, q->lcm);
  o << ") ";
  emit_wrapped(o.str());
}

void GB_comp::stats() const
{
  buffer o;
  o << "# pairs computed = " << n_computed << newline;
  emit(o.str());
  o.reset();
  spairs->stats();
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    for (int i=0; i<gb.length(); i++)
      {
	o << i << '\t';
	F->elem_text_out(o, gb[i]->f);
	o << newline;
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


