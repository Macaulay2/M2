// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "gbZZ.hpp"
#include "geovec.hpp"
#include "text_io.hpp"

extern char system_interrupted;
extern int comp_printlevel;

stash *GBZZ_comp::mystash;

void GBZZ_comp::set_up0(const Matrix &m, int csyz, int nsyz)
{
  int i;
  R = m.Ring_of()->cast_to_PolynomialRing();
  if (R == NULL)
    {
      gError << "ring is not a polynomial ring";
      // MES: throw an error here.
      assert(0);
    }
  M = R->Nmonoms();

  REDUCE_EXP = new int[M->n_vars()];
  REDUCE_DIV = M->make_one();

  spairs = new s_pair_set(F,Fsyz,Gsyz);

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

  // set local variables for certain time-critical routines

  this_degree = F->lowest_primary_degree() - 1;

  for (i=0; i<F->rank(); i++)
    {
      monideal_pair *p = new monideal_pair(R);
      monideals.append(p);
    }
}

void GBZZ_comp::set_up(const Matrix &m, int csyz, int nsyz, int strat)
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
      gen_pair *p = new_gen(i, m[i]);
      if (p != NULL)
	{
	  spairs->insert(p);
	  n_gens_left++;
	}
    }
}

void GBZZ_comp::force(const Matrix &m, const Matrix &gb, const Matrix &mchange,
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
      vec f = F->copy(gb[i]);
      vec fsyz = Fsyz->copy(mchange[i]);
      gb_insert(f,fsyz,0);
    }
}

GBZZ_comp::GBZZ_comp(const Matrix &m, int csyz, int nsyz, int strat)
  : gb_comp(COMP_GB)
{
  set_up(m, csyz, nsyz, strat);
}

GBZZ_comp::GBZZ_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
		 const Matrix &syz)
  : gb_comp(COMP_GB)
{
  force(m, gb, mchange, syz);
}

void GBZZ_comp::remove_pair(S_pair *& p)
{
  Gsyz->remove(p->fsyz);
  p->next = NULL;
  delete p;
  p = NULL;
}

void GBZZ_comp::remove_gen(gen_pair *& p)
{
  F->remove(p->f);
  Fsyz->remove(p->fsyz);
  p->next = NULL;
  delete p;
  p = NULL;
}

GBZZ_comp::~GBZZ_comp()
{
  int i;

  // remove spairs
  delete spairs;

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
  bump_down(F);
  bump_down(Fsyz);
}

void GBZZ_comp::resize(int /*nbits*/)
     // Resizes all (packed) monomials, and polynomials
     // to work in at least the next degree.
{
  // MES
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////

S_pair *GBZZ_comp::new_ring_pair(GB_elem *p, const int *lcm)
{
  vec fsyz = Gsyz->e_sub_i(p->me);
  // MESXX: Multiply 'f' by the extra stuff in the ring element.
  return new S_pair(fsyz);
}

S_pair *GBZZ_comp::new_s_pair(GB_elem *p, GB_elem *q, const int *lcm)
{
  vec fsyz = Gsyz->e_sub_i(p->me); // MESXX: then multiply by monomial
  vec gsyz = Gsyz->e_sub_i(q->me); // MESXX: then multiply by monomial
  Gsyz->add_to(fsyz, gsyz);
  return new S_pair(fsyz);
}

gen_pair *GBZZ_comp::new_gen(int i, const vec f)
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

  return new gen_pair(f,fsyz);
}

//////////////////////////////////////////////
//  sorting the Groebner basis ///////////////
//////////////////////////////////////////////

int GBZZ_comp::gb_sort_partition(int lo, int hi)
{
  GB_elem *pivot = gb[lo];
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
	  GB_elem *tmp = gb[j];
	  gb[j] = gb[i];
	  gb[i] = tmp;
	}
      else
	return j;
    }
}

void GBZZ_comp::gb_sort(int lo, int hi)
{
  if (lo < hi)
    {
      int q = gb_sort_partition(lo, hi);
      gb_sort(lo, q);
      gb_sort(q+1, hi);
    }
}

void GBZZ_comp::find_pairs(GB_elem *p)
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
	  S_pair *q = new_ring_pair(p, find_pairs_lcm);
	  elems.insert(new Bag(q, vplcm));
	}
      delete [] skewvars;
    }

  // Add in syzygies arising from a base ring

  if (F->is_quotient_ring)
    for (j = R->Rideal.first(); j.valid(); j++)
      {
	Nterm * f = (Nterm *) R->Rideal[j]->basis_ptr();
	M->lcm(f->monom, p->f->monom, find_pairs_lcm);
	vplcm.shrink(0);
	M->to_varpower(find_pairs_lcm, vplcm);
	S_pair *q = new_ring_pair(p, find_pairs_lcm);
	elems.insert(new Bag(q, vplcm));
      }

  // Add in syzygies arising as s-pairs
  MonomialIdeal &mi1 = monideals[p->f->comp]->mi;
  for (Index<MonomialIdeal> i = mi1.first(); i.valid(); i++)
    {
      M->from_varpower(mi1[i]->monom().raw(), find_pairs_m);
      M->lcm(find_pairs_m, p->f->monom, find_pairs_lcm);
      vplcm.shrink(0);
      M->to_varpower(find_pairs_lcm, vplcm);
      S_pair *q = new_s_pair(p, (GB_elem *)mi1[i]->basis_ptr(), find_pairs_lcm);
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
      S_pair *q = (S_pair *) b->basis_ptr();
      remove_pair(q);
      delete b;
    }
  for (j = mi.first(); j.valid(); j++)
    {
      S_pair *q = (S_pair *) mi[j]->basis_ptr();
      if (is_ideal && q->fsyz->next != NULL) // last condition: unclear for k=ZZ?
	{
	  // MESXX: check gcd of correct terms...
	  M->gcd(q->fsyz->monom, q->fsyz->next->monom, find_pairs_m);
	  if (M->is_one(find_pairs_m))
	    {
	      n_saved_gcd++;
	      if (comp_printlevel >= 8)
		{
		  buffer o;
		  o << "removed pair[" << q->fsyz->comp << " " 
		    << q->fsyz->next->comp << "]" << newline;
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

void GBZZ_comp::compute_s_pair(S_pair *p, vec &f, vec &fsyz)
{
  int *s = M->make_one();
  f = NULL;
  fsyz = NULL;
  for (vec t = p->fsyz; t != NULL; t = t->next)
    {
      GB_elem *g = gb[t->comp];
      M->divide(t->monom, g->f->monom, s);
      vec f1 = F->imp_mult_by_term(t->coeff, s, g->f);
      vec fsyz1 = Fsyz->mult_by_term(t->coeff, s, g->fsyz);
      if (Fsyz->is_quotient_ring) Fsyz->normal_form(fsyz1);
      F->add_to(f, f1);
      Fsyz->add_to(fsyz, fsyz1);
    }
  M->remove(s);
}

void GBZZ_comp::gb_reduce(vec &f, vec &fsyz)
{
  if (((strategy & USE_GEOBUCKET) != 0) && !M->is_skew())
    {
      gb_geo_reduce(f,fsyz);
      return;
    }
  vecterm head;
  vecterm *result = &head;
  ring_elem coeff;

  // REDUCE_EXP (exponent vector), REDUCE_DIV (element of M) are 
  // set in GBZZ_comp::GBZZ_comp.

  int count = 0;
  while (f != NULL)
    {
      Bag *b;
      M->to_expvector(f->monom, REDUCE_EXP);
      if (F->is_quotient_ring && R->Rideal.search_expvector(REDUCE_EXP, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  F->imp_ring_cancel_lead_term(f, g, coeff, REDUCE_DIV);
	  R->Ncoeffs()->remove(coeff);
	  count++;
	}
      else if (monideals[f->comp]->mi_search.search_expvector(REDUCE_EXP, b))
	{
	  GB_elem *q = (GB_elem *) b->basis_ptr();
	  F->imp_cancel_lead_term(f, q->f, coeff, REDUCE_DIV);
	  Fsyz->subtract_multiple_to(fsyz, coeff, REDUCE_DIV, q->fsyz);
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
}

void GBZZ_comp::gb_geo_reduce(vec &f, vec &fsyz)
{
  vecterm head;
  vecterm *result = &head;

  // REDUCE_EXP (exponent vector), REDUCE_DIV (element of M) are 
  // set in GBZZ_comp::GBZZ_comp.
  int count = 0;

  geobucket fb(F);
  geobucket fsyzb(Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);
  vecterm *lead;
  while ((lead = fb.remove_lead_term()) != NULL)
    {
      Bag *b;
      M->to_expvector(lead->monom, REDUCE_EXP);
      if (F->is_quotient_ring && R->Rideal.search_expvector(REDUCE_EXP, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  M->divide(lead->monom, g->monom, REDUCE_DIV);
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  vecterm *h = F->imp_ring_mult_by_term(g->next, c, REDUCE_DIV, lead->comp);
	  F->remove(lead);
	  R->Ncoeffs()->remove(c);
	  fb.add(h);
	  count++;
	}
      else if (monideals[lead->comp]->mi_search.search_expvector(REDUCE_EXP, b))
	{
	  GB_elem *q = (GB_elem *) b->basis_ptr();
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  M->divide(lead->monom, q->f->monom, REDUCE_DIV);
	  vecterm *h = F->imp_mult_by_term(c, REDUCE_DIV, q->f->next);
	  vecterm *hsyz = Fsyz->imp_mult_by_term(c, REDUCE_DIV, q->fsyz);
	  F->remove(lead);
	  R->Ncoeffs()->remove(c);
	  fb.add(h);		// Eats h
	  fsyzb.add(hsyz);	// Eats hsyz
	  count++;
	}
      else
	{
	  result->next = lead;
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
}

void GBZZ_comp::gb_insert(vec f, vec fsyz, int ismin)
{
  GB_elem *p = new GB_elem(f, fsyz, ismin);

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
}

bool GBZZ_comp::s_pair_step()
     // If no s-pairs left in the current degree, 
     // return false.
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return true.
{
  vec f, fsyz;

  S_pair *p = spairs->next_pair();
  if (p == NULL) return false;  // Done

  n_computed++;
  compute_s_pair(p, f, fsyz);	// Sets f, fsyz from p.
  remove_pair(p);

  gb_reduce(f, fsyz);
  if (f != NULL)
    {
      gb_insert(f, fsyz, 0);
      if (comp_printlevel >= 3) emit("m");
      return true;
    }
  if (fsyz != NULL)
    {
      if (collect_syz)
	{
	  syz.append(fsyz);
	  if (comp_printlevel >= 3) emit("z");
	  return true;
	}
      else
	Fsyz->remove(fsyz);
    }
  if (comp_printlevel >= 3) emit("o");
  return true;
}

bool GBZZ_comp::gen_step()
     // If no gens left in the current degree, 
     // return false;
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return true.
{
  gen_pair *p = spairs->next_gen();
  if (p == NULL) return false;	// Done.

  n_computed++;
  n_gens_left--;

  vec f = p->f;
  vec fsyz = p->fsyz;
  p->f = NULL;
  p->fsyz = NULL;
  remove_gen(p);

  gb_reduce(f, fsyz);
  if (f != NULL)
    {
      gb_insert(f, fsyz, 1);	// 1 = minimal generator
      if (comp_printlevel >= 3) emit("g");
      return true;
    }
  if (fsyz != NULL)
    {
      if (collect_syz)
	{
	  syz.append(fsyz);
	  if (comp_printlevel >= 3) emit("z");
	  return true;
	}
      else
	Fsyz->remove(fsyz);
    }
  if (comp_printlevel >= 3) emit("o");
  return true;
}

bool GBZZ_comp::auto_reduce_step()
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

bool GBZZ_comp::new_pairs_step()
     // Compute the new s-pairs associated to the given gb element.
     // Increment 'np_i'.  If done with all pairs in this 
     // degree, return false.
{
  if (np_i >= n_gb) return false;
  find_pairs(gb[np_i]);
  np_i++;
  return true;
}

//---- Completion testing -----------------------------

int GBZZ_comp::computation_complete(const int * /* stop_degree */,
				  int stop_gb, int stop_syz, 
				  int /*stop_codim*/,
				  int stop_pairs, int stop_min_gens,
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

//---- state machine (roughly) for the computation ----

int GBZZ_comp::calc(const int *deg, const intarray &stop)
{
  int n_in_degree;

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
      if (system_interrupted) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}
      
      switch (state) 
	{
	case GB_COMP_NEWDEGREE:
	  n_in_degree = spairs->next_degree(this_degree);
	  if (n_in_degree == 0)
	    {
	      state = GB_COMP_DONE;
	      is_done = COMP_DONE;
	      break;
	    }

	  if (stop_degree && this_degree > *stop_degree)
	    {
	      is_done = COMP_DONE_DEGREE_LIMIT;
	      break;
	    }

	  if (comp_printlevel >= 1)
	    {
	      buffer o;
	      o << '{' << this_degree << '}';
	      o << '(';
	      o << n_in_degree << ')';
	      emit(o.str());
	    }

	  // Set state information for auto reduction, new pairs
	  ar_i = n_gb;
	  ar_j = ar_i + 1;
	  np_i = n_gb;
	  state = GB_COMP_S_PAIRS;
	  break;
	  
	case GB_COMP_S_PAIRS:
	  if (!s_pair_step())
	    state = GB_COMP_GENS;
	  break;

	case GB_COMP_GENS:
	  if (!gen_step())
	    state = GB_COMP_AUTO_REDUCE;
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
Matrix GBZZ_comp::reduce(const Matrix &m, Matrix &lift)
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

int GBZZ_comp::contains(const Matrix &m)
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
bool GBZZ_comp::is_equal(const gb_comp *q)
{
  if (kind() != q->kind()) return false;
  GBZZ_comp *that = (GBZZ_comp *) q;
  if (this->F->rank() != that->F->rank()) return false;

  // Loop through every GB element: in each monideal[i]->mi_search
  for (int i=0; i<F->rank(); i++)
    {
      Index<MonomialIdeal> j1 = this->monideals[i]->mi_search.first();
      Index<MonomialIdeal> j2 = that->monideals[i]->mi_search.first();
      for (;j1.valid() && j2.valid();j1++, j2++)
	{
	  GB_elem *f1 = (GB_elem *) (this->monideals[i]->mi_search)[j1]->basis_ptr();
	  GB_elem *f2 = (GB_elem *) (that->monideals[i]->mi_search)[j2]->basis_ptr();
	  if (!F->is_equal(f1->f,f2->f))
	    return false;
	}
      if (j1.valid() || j2.valid())
	return false;
    }
  return true;
}

Vector GBZZ_comp::reduce(const Vector &v, Vector &lift)
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
Matrix GBZZ_comp::min_gens_matrix()
{
  Matrix result(F);
  for (int i=0; i<gb.length(); i++)
    if (gb[i]->is_min)
      result.append(F->copy(gb[i]->f));
  return result;
}

Matrix GBZZ_comp::initial_matrix(int n)
{
  Matrix result(F);
  for (int i=0; i<gb.length(); i++)
    result.append(F->lead_term(n, gb[i]->f));
  return result;
}

Matrix GBZZ_comp::gb_matrix()
{
  return gbmatrix;
//  Matrix result(F);
//  for (int i=0; i<gb.length(); i++)
//    result.append(F->copy(gb[i]->f));
//  return result;
}

Matrix GBZZ_comp::change_matrix()
{
  Matrix result(Fsyz);
  for (int i=0; i<gb.length(); i++)
    result.append(Fsyz->copy(gb[i]->fsyz));
  return result;
}

Matrix GBZZ_comp::syz_matrix()
{
  return syz;
}

void GBZZ_comp::debug_out(S_pair *q) const
{
  if (q == NULL) return;
  buffer o;
  Gsyz->elem_text_out(o, q->fsyz);
  emit(o.str());
}

void GBZZ_comp::stats() const
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


