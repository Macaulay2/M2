// Copyright 1996  Michael E. Stillman

#include "style.hh"
#include "Ngb.hh"

extern char system_interrupted;
extern int comp_printlevel;

extern ring_elem hilb(const Matrix &M, const Ring *RR);

stash *MonomialIdeal_pair::mystash;
stash *NGB_comp::mystash;
stash *Ns_pair::mystash;
stash *Ns_degree::mystash;

void NGB_comp::set_up(const Matrix &m, int csyz, int nsyz)
{
  int i;
  gens = m;
  R = m.Ring_of()->cast_to_poly_ring();
  if (R == NULL)
    {
      *gError << "ring is not a polynomial ring";
      // MES: throw an error here.
      assert(0);
    }
  M = R->Nmonoms();
  n_degrees = degree_monoid()->n_vars();
  state = GB_COMP_NEWDEGREE;
  this_degree = NULL;
  ar_i = ar_j = np_i = -1;
  this_pair = NULL;

  next_compare_num = 0;
  n_gb = n_mingens = 0;
  n_pairs = n_computed = n_saved_gcd = n_saved_hilb = 0;
  n_gens_left = m.n_cols();

  stop_degree = NULL;
  
  collect_syz = csyz;
  use_hilb = 0;
  
  // set local variables for certain time-critical routines
  find_pairs_m = M->make_one();
  find_pairs_exp = a_exp.alloc(M->n_vars());
  find_pairs_lcm = a_lcm.alloc(M->n_vars());

  if (nsyz < 0 || nsyz > m.n_cols())
    nsyz = m.n_cols();
  n_comps_per_syz = nsyz;
  const FreeModule *F = m.cols()->sub_space(nsyz);  
    
  syz = Matrix(F);

  if (m.n_rows() > 0)
    lowest_degree = m.rows()->degree(0)[0];
  else 
    lowest_degree = 0;
  for (i=0; i<m.n_rows(); i++)
    {
      MonomialIdeal_pair *p = new MonomialIdeal_pair(R);
      monideals.append(p);

      if (m.rows()->degree(i)[0] < lowest_degree)
	lowest_degree = m.rows()->degree(i)[0];
    }

  for (i=0; i<m.n_rows(); i++)
    {
      Ns_pair *p = new_base(i);
      base_components.append(p);
    }

  for (i=0; i<m.n_cols(); i++)
    {
      Ns_pair *p = new_gen(i);
      if (p != NULL)
	insert_s_pair(p);
    }
}

NGB_comp::NGB_comp(const Matrix &m, int csyz, int nsyz)
  : gb_comp(COMP_NGB)
{
  set_up(m, csyz, nsyz);
}

NGB_comp::NGB_comp(const Matrix &/*m*/, const Matrix &/*gb*/, 
		   const Matrix &/*mchange*/)
  : gb_comp(COMP_NGB)
{
  // MES
}

NGB_comp::NGB_comp(const Matrix &/*m*/, const Matrix &/*gb*/, 
		   const Matrix &/*mchange*/, 
		   const Matrix &/*syz*/)
  : gb_comp(COMP_NGB)
{
  // MES
}

NGB_comp::NGB_comp(const Matrix &/*m*/, int /*collect_syz*/, int /*n_syz*/, 
		 const ring_elem /*hf*/)
  : gb_comp(COMP_NGB)
{
  // MES
}

NGB_comp::~NGB_comp()
{
  // MES
}

//--- Degree control ---------//
int * NGB_comp::multi_degree(const Ns_pair *q) const
{
  int *result = degree_monoid()->make_one();
  M->multi_degree(q->lcm, result);
  if (q->f != NULL)
    degree_monoid()->mult(result, gens.rows()->degree(q->f->comp), result);
  else if (q->first != NULL && q->first->f != NULL)
    degree_monoid()->mult(result, gens.rows()->degree(q->first->f->comp), result);
  else
    assert(0);
  return result;
}

const int *NGB_comp::current_degree() const
{
  if (this_degree == NULL) return NULL;
  return this_degree->this_deg;
}

int NGB_comp::degree_set_ok(const Ns_degree *p, const int * /*deg*/) const
{
  // Check that 'p' has elements in it.
  // Later we must make sure that this element is in the
  // positive cone of possible weights.
  return p->num_left > 0;
}

Ns_degree *NGB_comp::next_degree(const int *deg)
{
  int top = s_pairs.length();
  if (deg != NULL && deg[0] < top+lowest_degree)
    top = deg[0] - lowest_degree;

  for (int i=0; i<top; i++)
    {
      Ns_degree *p = s_pairs[i];
      if (p == NULL) continue;
      while (p->next != NULL 
	     && !degree_set_ok(p->next, deg)) 
	p = p->next;

      if (p->next != NULL) return p->next;
    }
  return NULL;
}

int NGB_comp::next_level()
     // Updates the state to the next degree.
     // In particular, this sets: 'this_degree'.
     // Also sets: ar_i, ar_j (auto reduction state)
     // and: np_i (new pairs state).
     // The 'spair' and 'gen' state is either set here, or after sort_pairs.
     // Return value is one of GB_COMP_DONE, GB_COMP_SORTPAIRS, GB_COMP_RESIZE_MONOMIALS
{
  this_degree = next_degree(stop_degree);
  if (this_degree == NULL) return GB_COMP_DONE;
  if (comp_printlevel >= 1)
    {
      degree_monoid()->elem_text_out(cerr, current_degree());
      cerr << '(' << this_degree->num_pairs << ')';
    }

  // MES: is this following line the correct condition?
  if (M->max_degree() < current_degree()[0] - lowest_degree)
    return GB_COMP_RESIZE_MONOMIALS;
  ar_i = n_gb;
  ar_j = ar_i + 1;
  np_i = n_gb;
  return GB_COMP_SORTPAIRS;
}

void NGB_comp::resize(int /*nbits*/)
     // Resizes all (packed) monomials, and polynomials
     // to work in at least the next degree.
{
  // MES
}

//////////////////////////////////////////////
//  Sorting //////////////////////////////////
//////////////////////////////////////////////

int NGB_comp::compare_pairs(Ns_pair *f, Ns_pair *g) const
{
  int cmp = M->compare(f->lcm, g->lcm);
  if (cmp != 0) return cmp;
  cmp = f->first->compare_num - g->first->compare_num;
  if (cmp < 0) return 1;
  if (cmp > 0) return -1;
  return 0;
}

Ns_pair *NGB_comp::merge_pairs(Ns_pair *f, Ns_pair *g) const
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  Ns_pair head;
  Ns_pair *result = &head;
  while (1)
    switch (compare_pairs(f, g))
      {
      case 1:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case -1:
      case 0:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
      }
}

void NGB_comp::sort_pairs(Ns_pair *& p) const
{
  if (p == NULL || p->next == NULL) return;
  Ns_pair *p1 = NULL;
  Ns_pair *p2 = NULL;
  while (p != NULL)
    {
      Ns_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_pairs(p1);
  sort_pairs(p2);
  p = merge_pairs(p1, p2);
}

//---- S-pair management --------------------------------//
Ns_pair *NGB_comp::new_var_pair(Ns_pair * /*p*/, const int * /*lcm*/)
{
  // MES
  return NULL;
}

Ns_pair *NGB_comp::new_ring_pair(Ns_pair *p, const int *lcm)
{
  Ns_pair *result = new Ns_pair;
  result->next = NULL;
  result->syz_type = SPAIR_RING;
  result->first = p;
  result->second = NULL;
  result->f = NULL;
  result->fsyz = NULL;
  result->is_min = 0;

  result->lcm = M->make_new(lcm);

  return result;
}

Ns_pair *NGB_comp::new_s_pair(Ns_pair *p, Ns_pair *q, const int * /*lcm*/)
{
  // p and q should have 'f' field defined.
  Ns_pair *result = new Ns_pair;
  result->next = NULL;
  result->syz_type = SPAIR_PAIR;
  result->first = p;
  result->second = q;
  result->f = NULL;
  result->fsyz = NULL;
  result->is_min = 0;

  result->lcm = M->make_one();
  M->lcm(p->f->monom, q->f->monom, result->lcm);

  return result;
}
Ns_pair *NGB_comp::new_base(int i)
{
  Ns_pair *result = new Ns_pair;
  result->next = NULL;
  result->syz_type = SPAIR_BASE;
  result->first = NULL;
  result->second = NULL;
  result->f = NULL;
  result->fsyz = NULL;
  result->compare_num = i;
  result->lcm = M->make_one();
  result->is_min = 1;
  return result;
}

Ns_pair *NGB_comp::new_gen(int i)
{
  vec fsyz;

  if (i < n_comps_per_syz)
    fsyz = syz.rows()->e_sub_i(i);
  else
    fsyz = NULL;

  if (gens.rows()->is_zero(gens[i]))
    {
      syz.append(fsyz, gens.cols()->degree(i));
      return NULL;
    }

  Ns_pair *result = new Ns_pair;
  result->next = NULL;
  result->syz_type = SPAIR_GEN;
  result->second = NULL;
  result->f = gens.rows()->copy(gens[i]);
  result->first = base_components[result->f->comp];
  result->fsyz = fsyz;
  result->lcm = M->make_new(result->f->monom);
  result->is_min = -1;

  return result;
}

static int ints_same(int n, const int *a, const int *b)
{
  for (int i=0; i<n; i++)
    if (*a++ != *b++) return 0;
  return 1;
}

Ns_degree *NGB_comp::get_degree_set(int *&deg)
     // Find the given degree_set, if it exists.
     // If it doesn't, create it, and return it.
     // Consume 'deg'
{
  int d = deg[0] - lowest_degree;
  int e = d - s_pairs.length();
  if (e >= 0)
    for (int i=0; i<e; i++)
      s_pairs.append((Ns_degree *)NULL);

  if (s_pairs[d] == NULL)
    s_pairs[d] = new Ns_degree;

  Ns_degree *p;
  for (p = s_pairs[d]; p->next != NULL; p = p->next)
    if (ints_same(n_degrees, deg, p->next->this_deg))
      {
	degree_monoid()->remove(deg);
	return p->next;
      }

  p->next = new Ns_degree;
  p->next->this_deg = deg;
  return p->next;
}

void NGB_comp::insert_s_pair(Ns_pair *p)
{
  // First we must determine the multi-degree,
  // and then find the correct s_degree,
  // and finally place it onto the list.

  int *deg = multi_degree(p);
  Ns_degree *q = get_degree_set(deg);
  q->num_pairs++;
  q->num_left++;
  p->next = q->first;
  q->first = p;
  n_pairs++;
}

void NGB_comp::find_pairs(Ns_pair *p)
  // compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
  // (includes cases m * lead(p) = 0).
  // Returns a list of new s_pair's.
{
  queue<Bag *> elems;
  intarray vplcm;
  Index<MonomialIdeal> j;
#if 0
  if (R->exterior_vars.length() > 0)
    {
      M->divide(p->lcm, p->f->monom, find_pairs_m);
      M->to_expvector(find_pairs_m, find_pairs_exp);
      
      // Add in syzygies arising from exterior variables
      for (int v=0; v < R->exterior_vars.length(); v++)
	{
	  int w = R->exterior_vars[v];
	  if (find_pairs_exp[w] > 0)
	    {
	      find_pairs_exp[w]++;
	      M->from_expvector(find_pairs_exp, find_pairs_lcm);
	      find_pairs_exp[w]--;
	      
	      vplcm.shrink(0);
	      M->to_varpower(find_pairs_lcm, vplcm);
	      Ns_pair *q = new_var_pair(p, find_pairs_lcm);
	      elems.insert(new Bag(q, vplcm));
	    }
	}
    }
#endif
  // Add in syzygies arising from a base ring

  if (gens.rows()->is_quotient_ring)
    for (j = R->Rideal.first(); j.valid(); j++)
      {
	Nterm * f = (Nterm *) R->Rideal[j]->basis_ptr();
	M->lcm(f->monom, p->f->monom, find_pairs_lcm);
	vplcm.shrink(0);
	M->to_varpower(find_pairs_lcm, vplcm);
	Ns_pair *q = new_ring_pair(p, find_pairs_lcm);
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
      Ns_pair *q = new_s_pair(p, (Ns_pair *)mi1[i]->basis_ptr(), find_pairs_lcm);
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
      Ns_pair *q = (Ns_pair *) b->basis_ptr();
      M->remove(q->lcm);  // MES: more should probably be removed...
      delete q;
      delete b;
    }
  for (j = mi.first(); j.valid(); j++)
    {
      Ns_pair *q = (Ns_pair *) mi[j]->basis_ptr();
      insert_s_pair(q);		// In ideal case: don't insert some of these.
    }
}


//---- Handling s-pairs and generators ------------------//

void NGB_comp::sort_new_elems()
{
  // MES
}

void NGB_comp::compute_s_pair(Ns_pair *p)
{
  if (p->syz_type != SPAIR_GEN)
    {
      intarray ai, aj;
      int *si = ai.alloc(M->monomial_size());
      int *sj = aj.alloc(M->monomial_size());
      M->divide(p->lcm, p->first->f->monom, si);
      p->f = gens.rows()->mult_by_monomial(si, p->first->f);
      p->fsyz = syz.rows()->mult_by_monomial(si, p->first->fsyz);
      if (syz.rows()->is_quotient_ring) syz.rows()->normal_form(p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
	{
	  ring_elem a = R->Ncoeffs()->from_int(1);
	  M->divide(p->lcm, p->second->f->monom, sj);
	  gens.rows()->imp_subtract_multiple_to(p->f, a, sj, p->second->f);
	  syz.rows()->subtract_multiple_to(p->fsyz, a, sj, p->second->fsyz);
	  R->Ncoeffs()->remove(a);
	}
      if (comp_printlevel >= 8) 
	{
	  debug_out(p);
	  cerr << " ";
	}
    }
}

void NGB_comp::gb_reduce(vec &f, vec &fsyz)
{
  Nvecterm head;
  Nvecterm *result = &head;

  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());

  while (f != NULL)
    {
      Bag *b;
      M->to_expvector(f->monom, div_totalexp);
      if (gens.rows()->is_quotient_ring && R->Rideal.search_expvector(div_totalexp, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  M->divide(f->monom, g->monom, reduce_ndiv);
	  gens.rows()->imp_subtract_ring_multiple_to(f, f->coeff, reduce_ndiv, g);
	}
      else if (monideals[f->comp]->mi_search.search_expvector(div_totalexp, b))
	{
	  Ns_pair *q = (Ns_pair *) b->basis_ptr();
	  ring_elem c = f->coeff;
	  M->divide(f->monom, q->f->monom, reduce_ndiv);
	  gens.rows()->subtract_multiple_to(f, c, reduce_ndiv, q->f);
	  syz.rows()->subtract_multiple_to(fsyz, c, reduce_ndiv, q->fsyz);
	}
      else
	{
	  result->next = f;
	  f = f->next;
	  result = result->next;
	}
    }

  result->next = NULL;
  f = head.next;
}

void NGB_comp::gb_insert(Ns_pair *p, int ismin)
{
  gens.rows()->make_monic(p->f, p->fsyz);
  if (ismin)
    {
      n_mingens++;
      p->is_min = 1;
    }
  // insert into p->f->comp->mi_search
  intarray vp;
  M->to_varpower(p->f->monom, vp);
  monideals[p->f->comp]->mi_search.insert(new Bag(p, vp));
  n_gb++;
  gb.append(p);
}

int NGB_comp::s_pair_step()
     // If no s-pairs left in the current degree, 
     // return SPAIR_DONE.
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return one of the other SPAIR_*
     // values.
{
  if (this_pair == NULL) return SPAIR_DONE;
  int is_min = this_pair->syz_type == SPAIR_GEN;
  this_degree->num_left--;
  n_computed++;
  if (is_min) n_gens_left--;

  compute_s_pair(this_pair);

  Ns_pair *p = this_pair;
  this_pair = this_pair->next;

  gb_reduce(p->f, p->fsyz);
  if (comp_printlevel >= 9)
    {
      gens.rows()->Nelem_text_out(cerr, p->f);
      cerr << endl;
      if (p->fsyz != NULL)
	{
	  syz.rows()->Nelem_text_out(cerr, p->fsyz);
	  cerr << endl;
	}
    }
  if (p->f != NULL)
    {
      gb_insert(p, is_min);
      if (is_min) 
	return SPAIR_MINGEN;
      return SPAIR_GB;
    }
  if (p->fsyz != NULL)
    {
      if (collect_syz)
	{
	  syz.append(p->fsyz, current_degree());
	  //delete p;
	  return SPAIR_SYZ;
	}
      //delete p;
    }
  
  return SPAIR_ZERO;
}

int NGB_comp::auto_reduce_step()
     // Using ar_i, ar_j, reduce the gb element ar_i wrt ar_j.
     // Increment ar_i, ar_j as needed. If done, return 0.
{
  if (ar_j >= n_gb)
    {
      ar_i++;
      ar_j = ar_i + 1;
      if (ar_j >= n_gb) return 0;
    }
  // Now compute gb(i) := gb(i) - c gb(j), where
  // c in(gb(j)) is a term in gb(i).
  // Also compute change(i) -= c change(j).
  
  gens.rows()->auto_reduce(syz.rows(), elem(ar_i)->f, elem(ar_i)->fsyz, 
			   elem(ar_j)->f, elem(ar_j)->fsyz);
  ar_j++;
  return 1;
}

int NGB_comp::new_pairs_step()
     // Compute the new s-pairs associated to the given gb element.
     // Increment 'np_i'.  If done with all pairs in this 
     // degree, return 0.
{
  if (np_i >= n_gb) return 0;
  find_pairs(elem(np_i));
  np_i++;
  return 1;
}

//---- Completion testing -----------------------------

int NGB_comp::computation_complete()
     // Test whether the current computation is done.
     // Return 1 if so, else 0.
{
  if (state == GB_COMP_DONE) 
    {
      if (stop_degree != NULL && n_computed != n_pairs)
	{
	  state = GB_COMP_NEWDEGREE;
	  return COMP_DONE_DEGREE_LIMIT;
	}
      return COMP_DONE;
    }
  if (stop_gb > 0 && n_gb >= stop_gb) return COMP_DONE_GB_LIMIT;
  if (stop_syz > 0 && syz.n_cols() >= stop_syz) return COMP_DONE_SYZ_LIMIT;
  if (stop_pairs > 0 && n_computed >= stop_pairs) return COMP_DONE_PAIR_LIMIT;
  //if (stop_codim > 0 && ...) return COMP_DONE_CODIM;
  if (stop_min_gens && n_gens_left == 0) return COMP_DONE_MIN_GENS;
  return COMP_COMPUTING;
}

//---- state machine (roughly) for the computation ----

void NGB_comp::step()
{
  switch (state) 
    {
    case GB_COMP_NEWDEGREE:
      state = next_level();	// returns GB_COMP_DONE, GB_COMP_SORTPAIRS, or
				// GB_COMP_RESIZE_MONOMIALS.  Also sets
				// "this_degree".
      break;

    case GB_COMP_RESIZE_MONOMIALS:
      if (resize_nbits != -1) 
	{
	  resize(resize_nbits);
	  state = GB_COMP_SORTPAIRS;
	}
      break;

    case GB_COMP_SORTPAIRS:
      sort_pairs(this_degree->first);
      for (Ns_pair *p = this_degree->first; p != NULL; p=p->next)
	p->compare_num = next_compare_num++;
      this_pair = this_degree->first;
      state = GB_COMP_S_PAIRS;
      break;

    case GB_COMP_S_PAIRS:
      if (comp_printlevel < 2 || comp_printlevel >= 8)
	{
	  if (s_pair_step() == SPAIR_DONE) 
	    state = GB_COMP_AUTO_REDUCE;
	}
      else switch (s_pair_step()) 
	{
	case SPAIR_MINGEN:
	  cerr << "g";
	  break;
	case SPAIR_GB:
	  cerr << "m";
	  break;
	case SPAIR_SYZ:
	  cerr << "z";
	  break;
	case SPAIR_ZERO:
	  cerr << "o";
	  break;
	case SPAIR_DONE:
	  state = GB_COMP_AUTO_REDUCE;
	  break;
	}
      break;

    case GB_COMP_AUTO_REDUCE:
      if (!auto_reduce_step()) 
	{
	  state = GB_COMP_NEWPAIRS;
	  sort_new_elems();
	}
      break;

    case GB_COMP_NEWPAIRS:
      if (!new_pairs_step()) state = GB_COMP_NEWDEGREE;
      break;

    case GB_COMP_DONE:
      break;
    }
}

int NGB_comp::calc(const int *deg, const intarray &stop)
{
  if (stop.length() != 6) 
    {
      *gError << "inappropriate stop conditions for GB computation";
      return COMP_ERROR;
    }
  stop_degree = deg;
  stop_gb = stop[0]; //ngb
  stop_syz = stop[1]; //nsyz
  stop_pairs = stop[2]; //npairs
  stop_codim = stop[3]; //cod
  stop_min_gens = stop[4]; //do_min

  int is_done;
  for (;;)
    {
      is_done = computation_complete();
      if (is_done != COMP_COMPUTING) break;
      if (system_interrupted) 
	{
	  if (comp_printlevel >= 1) cerr << endl;
	  return COMP_INTERRUPTED;
	}
      step();
    }
  if (comp_printlevel >= 1) cerr << endl;
  return is_done;
}

//--- Reduction --------------------------//
Matrix NGB_comp::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols());
  lift = Matrix(syz.rows(), m.cols());
  for (int i=0; i<m.n_cols(); i++)
    {
      vec f = gens.rows()->copy(m[i]);
      vec fsyz = NULL;

      gb_reduce(f, fsyz);
      syz.rows()->negate_to(fsyz);
      red[i] = f;
      lift[i] = fsyz;
    }
  return red;
}

Vector NGB_comp::reduce(const Vector &v, Vector &lift)
{
  if (!v.free_of()->is_equal(gens.rows()))
    {
      *gError << "reduce: vector is in incorrect free module";
      return Vector(gens.rows(), NULL);
    }
  vec f = gens.rows()->copy(v.get_value());
  vec fsyz = NULL;

  gb_reduce(f, fsyz);
  syz.rows()->negate_to(fsyz);

  lift = Vector(syz.rows(), fsyz);
  return Vector(gens.rows(), f);
}

//--- Obtaining matrices as output -------//
Matrix NGB_comp::min_gens_matrix()
{
  Matrix result(gens.rows());
  for (int i=0; i<gb.length(); i++)
    if (gb[i]->is_min)
      result.append(gens.rows()->copy(gb[i]->f));
  return result;
}

Matrix NGB_comp::initial_matrix(int n)
{
  Matrix result(gens.rows());
  for (int i=0; i<gb.length(); i++)
    result.append(gens.rows()->lead_term(n, gb[i]->f));
  return result;
}

Matrix NGB_comp::gb_matrix()
{
  Matrix result(gens.rows());
  for (int i=0; i<gb.length(); i++)
    result.append(gens.rows()->copy(gb[i]->f));
  return result;
}

Matrix NGB_comp::change_matrix()
{
  Matrix result(syz.rows());
  for (int i=0; i<gb.length(); i++)
    result.append(syz.rows()->copy(gb[i]->fsyz));
  return result;
}

Matrix NGB_comp::syz_matrix()
{
  return syz;
}

void NGB_comp::debug_out(Ns_pair *q) const
{
  if (q == NULL) return;
  cerr << "(" << q->compare_num << " ";
  if (q->first != NULL) cerr << q->first->compare_num; else cerr << ".";
  cerr << " ";
  if (q->second != NULL) cerr << q->second->compare_num; else cerr << ".";
  cerr << " ";
  M->elem_text_out(cerr, q->lcm);
  cerr << ") ";
}

void NGB_comp::stats() const
{
  int i;
  int total_pairs = 0;
  int total_left = 0;
  for (i=0; i<s_pairs.length(); i++)
    {
      Ns_degree *p = s_pairs[i];
      if (p == NULL) continue;
      while ((p = p->next) != NULL)
	{
	  degree_monoid()->elem_text_out(cerr, p->this_deg);
	  cerr << '\t'  << p->num_pairs << '\t' << p->num_left << endl;
	  total_pairs += p->num_pairs;
	  total_left += p->num_left;
	}
    }
  cerr << "------------------------------------------" << endl;
  cerr << '\t' << total_pairs << '\t' << total_left << endl << endl;
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    for (i=0; i<gb.length(); i++)
      {
	cerr << i << '\t';
	gens.rows()->Nelem_text_out(cerr, gb[i]->f);
	cerr << endl;
      }

  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    for (i=0; i<s_pairs.length(); i++)
      {
	Ns_degree *p = s_pairs[i];
	if (p == NULL) continue;
	while ((p = p->next) != NULL)
	  {
	    cerr << "---- degree ";
	    degree_monoid()->elem_text_out(cerr, p->this_deg);
	    cerr << "----"  << p->num_pairs << " ---- " << p->num_left << " ----" << endl;
	    for (Ns_pair *q = p->first; q!=NULL; q=q->next)
	      debug_out(q);
	  }
	
      }
}
