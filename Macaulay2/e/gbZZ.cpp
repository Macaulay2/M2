// Copyright 1996  Michael E. Stillman

#include "style.hpp"
#include "gbZZ.hpp"
#include "geovec.hpp"
#include "text_io.hpp"
#include "termideal.hpp"

extern char system_interrupted;
extern int comp_printlevel;

stash *GB_elem::mystash;
stash *GBZZ_comp::mystash;

void GBZZ_comp::set_up0(const Matrix &m, int csyz, int nsyz)
{
  int i;
  A = m.Ring_of()->cast_to_PolynomialRing();
  if (A == NULL)
    {
      gError << "ring is not a polynomial ring";
      // MES: throw an error here.
      assert(0);
    }
  R = A->get_base_poly_ring();	// The polynomial ring of which A is a quotient.
  if (R == NULL) R = A;
  K = R->Ncoeffs();
  M = R->Nmonoms();
  one = K->from_int(1);

  Rsyz = A->get_Rsyz();

  gbmatrix = Matrix(m.rows());
  Gsyz = R->make_FreeModule();

  F = m.rows();
  bump_up(F);


  if (nsyz < 0 || nsyz > m.n_cols())
    nsyz = m.n_cols();
  n_comps_per_syz = nsyz;


  ar_i = ar_j = np_i = -1;

  n_gb = n_mingens = n_subring = 0;
  n_pairs = n_computed = n_saved_gcd = 0;
  n_gens_left = 0;

  collect_syz = csyz;
  is_ideal = (F->rank() == 1 && csyz == 0);

  this_degree = F->lowest_primary_degree() - 1;

  for (i=0; i<F->rank(); i++)
    termideals.append(new TermIdeal(A,Gsyz));
}

void GBZZ_comp::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  vec f, fsyz;
  int i;
  strategy = strat;

  set_up0(m, csyz, nsyz);

  Fsyz = m.cols()->sub_space(n_comps_per_syz);  
  bump_up(Fsyz);
  syz = Matrix(Fsyz);

  spairs = new s_pair_set(F,Fsyz,Gsyz,Rsyz);

  state = GB_COMP_NEWDEGREE;

  for (i=0; i<m.n_cols(); i++)
    if (new_generator(i, m[i], f, fsyz))  // Possibly creates a syzygy.
      {
	spairs->insert_generator(f, fsyz); // Consumes f,fsyz.
	n_gens_left++;
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
  spairs = NULL;

  for (int i=0; i<gb.n_cols(); i++)
    {
      if (gb[i] == NULL) continue;
      vec f = F->copy(gb[i]);
      vec fsyz = Fsyz->copy(mchange[i]);
      insert_gb_element(f,fsyz,GB_NOT_MINIMAL);
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

  for (i=0; i<termideals.length(); i++)
    delete termideals[i];

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

bool GBZZ_comp::new_generator(int i, const vec m, vec &f, vec &fsyz)
{
  if (i < n_comps_per_syz)
    fsyz = Fsyz->e_sub_i(i);
  else
    fsyz = NULL;

  if (F->is_zero(m))
    {
      insert_syzygy(fsyz);
      return false;
    }
  else
    {
      f = F->copy(m);
      return true;
    }
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

//////////////////////////////////////////////
//  Finding new S-pairs //////////////////////
//////////////////////////////////////////////

void GBZZ_comp::find_pairs(GB_elem *p)
  // compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
  // (includes cases m * lead(p) = 0).
  // Returns a list of new s_pair's.
{
  queue<tagged_term *> elems;
  int j;
  intarray vplcm;
  int *find_pairs_m = M->make_one();
  int *find_pairs_exp = new int[M->n_vars()];
  int *find_pairs_lcm = new int[M->n_vars()];

  if (M->is_skew())
    {
      int *skewvars = new int[M->n_vars()];
      M->to_expvector(p->f->monom, find_pairs_exp);
      int nskew = M->exp_skew_vars(find_pairs_exp, skewvars);
      for (int i=0; i<M->n_vars(); i++) 
	find_pairs_exp[i] = 0;
      // Add in syzygies arising from exterior variables
      for (int v=0; v < nskew; v++)
	{
	  int w = skewvars[v];

	  find_pairs_exp[w]++;
	  M->from_expvector(find_pairs_exp, find_pairs_lcm);
	  find_pairs_exp[w]--;

	  // Add in the monomial syzygy (x_w * g_i), for p = g_i.
	  vec vsyz = Gsyz->term(p->me, one, find_pairs_lcm);
	  elems.insert(new tagged_term(K->copy(vsyz->coeff), 
				       M->make_new(vsyz->monom), 
				       vsyz, 
				       NULL));
	}
      delete [] skewvars;
    }

  // Add in syzygies arising from a base ring
  int *mon1 = M->make_one();
  int *mon2 = M->make_one();
  ring_elem g, g1, g2;

  if (F->is_quotient_ring)
    for (j=0; j<A->get_quotient_elem_length(); j++)
      {
	Nterm * f = (Nterm *) A->get_quotient_elem(j);

	M->monsyz(f->monom, p->f->monom, mon2, mon1);
	g = K->gcd(f->coeff, p->f->coeff);
	g1 = K->divide(f->coeff, g);
	g2 = K->divide(p->f->coeff, g);
	// K->negate_to(g2);// This is not needed, since negation is handled ahead of time
	// for ring elements.
	vec gsyz = Gsyz->term(p->me, g1, mon1);
	vec rsyz = Rsyz->term(j, g2, mon2);
	elems.insert(new tagged_term(g1, M->make_new(mon1), gsyz, rsyz));
	K->remove(g);
	K->remove(g2);
      }

  // Add in syzygies arising as s-pairs
  for (j=0; j<p->me; j++)
    {
      if (gb[j]->f->comp != p->f->comp)
	continue;
      vec f = gb[j]->f;
      M->monsyz(p->f->monom, f->monom, mon1, mon2);
      g = K->gcd(f->coeff, p->f->coeff);
      g1 = K->divide(f->coeff, g);
      g2 = K->divide(p->f->coeff, g);
      K->negate_to(g2); 
      vec gsyz = Gsyz->term(p->me, g1, mon1);
      vec gsyz2 = Gsyz->term(j, g2, mon2);
      Gsyz->add_to(gsyz, gsyz2);
      elems.insert(new tagged_term(g1, M->make_new(mon1), gsyz, NULL));
      K->remove(g);
      K->remove(g2);
    }

  M->remove(mon1);
  M->remove(mon2);
  // Now minimalize these elements, and insert them into
  // the proper degree.

  TermIdeal *ti = TermIdeal::make_termideal(R,Gsyz,elems);  
                                // Removes the elements which are
  				// not minimal.

  for (cursor_TermIdeal k(ti); k.valid(); ++k)
    {
      tagged_term *t = *k;
      vec gsyz = t->_gsyz;
      vec rsyz = t->_rsyz;
      t->_gsyz = NULL;
      t->_rsyz = NULL;

      // We want to insert these s-pairs, after checking some
      // criteria.
      // MESXX: what criteria?
      
      // Insert the element (gsyz,rsyz) as an s pair.
      spairs->insert_s_pair(gsyz, rsyz);
    }
  delete ti;
  // Remove the local variables
  M->remove(find_pairs_m);
  delete [] find_pairs_exp;
  delete [] find_pairs_lcm;
}

//////////////////////////////////////////////
//  Reduction ////////////////////////////////
//////////////////////////////////////////////

void GBZZ_comp::apply_gb_elements(vec &f, vec &fsyz, vec gsyz) const
{
  // f in F
  // fsyz in Fsyz
  // gsyz in Gsyz
  // Modify f, fsyz using the GB elements in gsyz.
  //int *s = M->make_one();
  for (vec t = gsyz; t != NULL; t = t->next)
    {
      GB_elem *g = gb[t->comp];
      //M->divide(t->monom, g->f->monom, s);
      int *s = t->monom;
      ring_elem c = K->negate(t->coeff);
      vec f1 = F->imp_mult_by_term(c, s, g->f);
      vec fsyz1 = Fsyz->mult_by_term(c, s, g->fsyz);
      if (Fsyz->is_quotient_ring) Fsyz->normal_form(fsyz1);
      F->add_to(f, f1);
      Fsyz->add_to(fsyz, fsyz1);
      K->remove(c);
    }
  //M->remove(s);
}
#if 0
void GBZZ_comp::apply_ring_elements(vec &f, vec rsyz) const
{
  // f in F
  // rsyz in Rsyz
  // Modify f using the ring elements in rsyz, applied to component 'x'.
  if (f == NULL) emit("apply_ring_elements: Oh oh!");
  int x = f->comp;
  for (vec t = rsyz; t != NULL; t = t->next)
    {
      ring_elem r = quotient_ideal[t->comp];
      vec f1 = F->imp_ring_mult_by_term(r, t->coeff, t->monom, x);
      F->add_to(f, f1);
    }
}
#endif
void GBZZ_comp::compute_s_pair(vec gsyz, vec rsyz,
			       vec &f, vec &fsyz) const
{
  f = NULL;
  fsyz = NULL;
  if (gsyz != NULL) apply_gb_elements(f, fsyz, gsyz);
  if (rsyz != NULL) F->apply_quotient_ring_elements(f, rsyz);
}

bool GBZZ_comp::gb_reduce(vec &f, vec &fsyz) const
{
  if (((strategy & USE_GEOBUCKET) != 0) && !M->is_skew())
    {
      // gb_geo_reduce(f,fsyz);
      return true;
    }
  vecterm head;
  vecterm *result = &head;

  bool ret = true;
  int count = 0;
  while (f != NULL)
    {
      vec gsyz, rsyz;
      bool reduces = termideals[f->comp]->search(f->coeff, f->monom,
						 gsyz, rsyz);
      if (rsyz != NULL)	
	{
	  F->apply_quotient_ring_elements(f, rsyz);
	  Rsyz->remove(rsyz);
	}
      if (gsyz != NULL) 
	{
	  apply_gb_elements(f,fsyz, gsyz);
	  Gsyz->remove(gsyz);
	}
      if (!reduces)
	{
	  ret = false;
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
      else 
	count++;
    }
  if (comp_printlevel >= 4)
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }
  result->next = NULL;
  f = head.next;
  return ret;
}

//////////////////////////////////////////////
//  New GB elements, syzygies ////////////////
//////////////////////////////////////////////

void GBZZ_comp::insert_gb_element(vec f, vec fsyz, bool ismin)
{
  GB_elem *p = new GB_elem(f, fsyz, ismin);

  // MESXX
  // Makemonic, if over a field.  But here, let's just make the
  // term positive.  If p->f->coeff < 0 then negate p->f, p->fsyz.

  if (ismin)
    {
      n_mingens++;
      p->is_min = 1;
    }
  if (M->in_subring(1,p->f->monom))   // MESXX: also determine if this is a minimal gen in subring.
    n_subring++;


  n_gb++;
  gb.append(p);

  TermIdeal *ti = termideals[p->f->comp];
  vec gsyz = Gsyz->e_sub_i(gb.length()-1);

  ti->insert_minimal(new tagged_term(K->copy(p->f->coeff),  // MESXX: what should be copied here?
				     M->make_new(p->f->monom),
				     gsyz,
				     NULL));

  // Now we must be a bit careful about this next, but we only want one
  // copy of a GB element, since the whole thing can be quite large.
  // Just make sure that when the GB is deleted at the end, that the 'f'
  // field of the gb_elem's is not removed.

  gbmatrix.append(p->f);
  int i = gbmatrix.n_cols() - 1;
  Gsyz->append(gbmatrix.cols()->degree(i));
}

bool GBZZ_comp::insert_syzygy(vec fsyz)
{
  if (fsyz != NULL)
    {
      if (collect_syz)
	{
	  syz.append(fsyz);
	  return true;
	}
      else
	Fsyz->remove(fsyz);
    }
  return false;
}

//////////////////////////////////////////////
//  Computation steps ////////////////////////
//////////////////////////////////////////////

void GBZZ_comp::handle_element(vec f, vec fsyz, bool maybe_minimal)
{
  while (true)
    {
      gb_reduce(f, fsyz);
      if (f == NULL) break;
      // The following sees whether f->monom() itself already is a GB
      // lead monomial.  If it is, it returns the integer location of this,
      // and resets the coefficient for this monomial to be the given one.
      // If a negative value is returned, then this means that the lead monomial
      // is not already a GB lead term.
      int i = termideals[f->comp]->replace_minimal(f->coeff, f->monom);
      if (i < 0) break;
      // At this point, we have a new lead coefficient for this monomial.
      // We need to replace gb[i] with (f,fsyz,maybe_minimal), and then
      // re-reduce the element we just took out of the GB.

      if (comp_printlevel >= 5)
	{
	  buffer o;
	  o << "replaced " << i << " with ";
	  Gsyz->elem_text_out(o, f);
	  o << newline;
	  emit(o.str());
	}

      swap(gb[i]->f, f);
      swap(gb[i]->fsyz,fsyz);
      swap(gb[i]->is_min, maybe_minimal);
      gbmatrix[i] = gb[i]->f;  // This must ALWAYS match gb[i]->f !!
    }
  
  if (f != NULL)
    {
      insert_gb_element(f, fsyz, maybe_minimal);
      if (comp_printlevel >= 3) emit("m");
    }
  else if (insert_syzygy(fsyz))
    {
      if (comp_printlevel >= 3) emit("z");
    }
  else if (comp_printlevel >= 3) emit("o");
}

bool GBZZ_comp::s_pair_step()
     // If no s-pairs left in the current degree, 
     // return false.
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return true.
{
  vec gsyz;			// in Gsyz.
  vec rsyz;			// in Rsyz.
  vec f;			// in F.
  vec fsyz;			// in Fsyz.

  if (!spairs->next_s_pair(gsyz, rsyz))
    return false;		// No more pairs in this degree.

  if (comp_printlevel >= 5)
    {
      buffer o;
      o << "s pair ";
      Gsyz->elem_text_out(o, gsyz);
      if (rsyz != NULL)
	{
	  o << " ring ";
	  Rsyz->elem_text_out(o,rsyz);
	}
      o << newline;
      emit(o.str());
    }

  n_computed++;
  compute_s_pair(gsyz, rsyz, f, fsyz);	// Sets f, fsyz from gsyz, rsyz.
  Gsyz->remove(gsyz);
  Rsyz->remove(rsyz);

  handle_element(f, fsyz, GB_NOT_MINIMAL);
  return true;
}

bool GBZZ_comp::gen_step()
     // If no gens left in the current degree, 
     // return false;
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return true.
{
  vec f, fsyz;
  if (!spairs->next_generator(f,fsyz))
    return false;		// No more in this degree.

  n_computed++;
  n_gens_left--;

  if (comp_printlevel >= 5)
    {
      buffer o;
      o << "gen ";
      F->elem_text_out(o, f);
      o << newline;
      emit(o.str());
    }

  handle_element(f, fsyz, GB_MAYBE_MINIMAL);
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
  
  F->auto_reduce_coeffs(Fsyz, gb[ar_i]->f, gb[ar_i]->fsyz, 
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

//////////////////////////////////////////////
//  Completion testing ///////////////////////
//////////////////////////////////////////////

int GBZZ_comp::computation_complete(const int * /* stop_degree */,
				    int stop_gb, 
				    int stop_syz, 
				    int stop_pairs, 
				    int /*stop_codim*/,
				    int stop_min_gens,
				    int stop_subring)
     // Test whether the current computation is done.
     // Return COMP_DONE_DEGREE_LIMIT, COMP_DONE, COMP_DONE_GB_LIMIT, COMP_DONE_SYZ_LIMIT,
     // COMP_DONE_PAIR_LIMIT, COMP_DONE_CODIM, COMP_DONE_MIN_GENS, or
     // (if not done) COMP_COMPUTING.
{
  if (state == GB_COMP_DONE)  return COMP_DONE;
  if (stop_gb > 0 && n_gb >= stop_gb) return COMP_DONE_GB_LIMIT;
  if (stop_syz > 0 && syz.n_cols() >= stop_syz) return COMP_DONE_SYZ_LIMIT;
  if (stop_pairs > 0 && n_computed >= stop_pairs) return COMP_DONE_PAIR_LIMIT;
  //if (stop_codim > 0 && ...) return COMP_DONE_CODIM;
  if (stop_min_gens && n_gens_left == 0) return COMP_DONE_MIN_GENS;
  if (stop_subring > 0 && n_subring >= stop_subring) return COMP_DONE_SUBRING_LIMIT;
  return COMP_COMPUTING;
}

//////////////////////////////////////////////
//  Main computation loop ////////////////////
//////////////////////////////////////////////

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
  emit_line("Should not be using GBZZ_comp::is_equal");
  return false;
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



#if 0
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
#endif
