// Copyright 1996  Michael E. Stillman

#include "style.hh"
#include "gbinhom2.hh"
#include "geobucket.hh"
extern char system_interrupted;
extern int comp_printlevel;

stash *GBinhom2_comp::mystash;

void GBinhom2_comp::set_up0(const Matrix &m, int csyz, int nsyz)
{
  int i;
  R = m.Ring_of()->cast_to_poly_ring();
  if (R == NULL)
    {
      *gError << "ring is not a polynomial ring";
      // MES: throw an error here.
      assert(0);
    }
  M = R->Nmonoms();

  F = m.rows();
  bump_up(F);

  spairs = new s_pair_heap(M);

  for (i=0; i<F->rank(); i++)
    {
      largeGB.append(new TermIdeal(R));
      minimalGB.append(new TermIdeal(R));
    }

  if (nsyz < 0 || nsyz > m.n_cols())
    nsyz = m.n_cols();
  n_comps_per_syz = nsyz;

  n_gb = n_subring = 0;
  n_pairs = n_computed = 0;
  last_gb_num = 0;
  n_saved_gcd = n_saved_lcm = 0;

  collect_syz = csyz;
  is_ideal = (F->rank() == 1 && csyz == 0);
  need_resize = 0;
}

void GBinhom2_comp::set_up(const Matrix &m, int csyz, int nsyz, int strat)
{
  strategy = strat;
  set_up0(m, csyz, nsyz);

  Fsyz = m.cols()->sub_space(n_comps_per_syz);  
  bump_up(Fsyz);
  syz = Matrix(Fsyz);

  add_gens(0, m.n_cols()-1, m);
}

void GBinhom2_comp::inter_reduce(gb_elem *&/*gens*/)
{
  // MES
}

void GBinhom2_comp::add_gens(int lo, int hi, const Matrix &m)
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

void GBinhom2_comp::force(const Matrix &m, const Matrix &gb, const Matrix &mchange,
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

GBinhom2_comp::GBinhom2_comp(const Matrix &m, int csyz, int nsyz, int strat)
{
  set_up(m, csyz, nsyz, strat);
}

GBinhom2_comp::GBinhom2_comp(const Matrix &m, const Matrix &gb, const Matrix &mchange, 
		 const Matrix &syz)
{
  force(m, gb, mchange, syz);
}

void GBinhom2_comp::remove_pair(s_pair *& p)
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

GBinhom2_comp::~GBinhom2_comp()
{
  // remove any remaining s-pairs
  s_pair *p;
  while ((p = spairs->remove()) != NULL)
    remove_pair(p);
  delete spairs;

  // remove the gb_elem's
  // MES: remove the elements in largeGB including gb_elem's they point to
  for (int i=0; i<F->rank(); i++)
    {
      for (monterm *q = largeGB[i]->first(); q != NULL; q = q->next)
	{
	  gb_elem *tmp = q->bag;
	  F->remove(tmp->f);
	  Fsyz->remove(tmp->fsyz);
	  delete [] tmp->lead_exp;
	  delete tmp;
	}
      delete largeGB[i];
      delete minimalGB[i];
    }
  
  // Finally, decrement ref counts
  bump_down(F);
  bump_down(Fsyz);
}

void GBinhom2_comp::resize(int /*nbits*/)
     // Resizes all (packed) monomials, and polynomials
     // to work in at least the next degree.
{
  // MES
}

//////////////////////////////////////////////
//  s pair construction //////////////////////
//////////////////////////////////////////////

s_pair *GBinhom2_comp::new_var_pair(gb_elem * /*p*/, const int * /*lcm*/)
{
  // MES
  return NULL;
}

s_pair *GBinhom2_comp::new_ring_pair(gb_elem *p, const int *lcm)
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

s_pair *GBinhom2_comp::new_s_pair(gb_elem *p, gb_elem *q, const int *lcm)
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

s_pair *GBinhom2_comp::new_gen(int i, const vec f)
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

int GBinhom2_comp::mark_pair(gb_elem *p, gb_elem *q) const
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
		cerr << "---- removed pair ";
		debug_out(r);
		cerr << endl;
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
		cerr << "---- removed pair ";
		debug_out(r);
		cerr << endl;
	      }
	    return 1;
	  }
	else
	  return 0;
      }
  return 0;
}
void GBinhom2_comp::find_pairs(gb_elem *p)
  // compute min gen set of {m | m lead(p) is in (p1, ..., pr, f1, ..., fs)}
  // (includes cases m * lead(p) = 0).
  // Returns a list of new s_pair's.
{
  queue<Bag *> elems;
  intarray vplcm;
  s_pair *q;
  int nvars = M->n_vars();
  int *find_pairs_exp = new int[nvars];
  int *find_pairs_lcm = new int[nvars];
  int *find_pairs_mon = new int[nvars];
  int *pi = new int [nvars];
  int *pj = new int [nvars];
  int *pij = new int [nvars];
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
	      s_pair *q = new_var_pair(p, find_pairs_lcm);
	      elems.insert(new Bag(q, vplcm));
	    }
	}
    }
#endif
  // Add in syzygies arising from a base ring

  if (F->is_quotient_ring)
    for (Index<MonomialIdeal> j = R->Rideal.first(); j.valid(); j++)
      {
	Nterm * f = (Nterm *) R->Rideal[j]->basis_ptr();
	M->lcm(f->monom, p->f->monom, find_pairs_lcm);
	vplcm.shrink(0);
	M->to_varpower(find_pairs_lcm, vplcm);
	q = new_ring_pair(p, find_pairs_lcm);
	elems.insert(new Bag(q, vplcm));
      }

  // Add in syzygies arising as s-pairs
  for (monterm *r = minimalGB[p->f->comp]->first(); r != NULL; r = r->next)
    {
      gb_elem *s = r->bag;
      M->lcm(p->f->monom, s->f->monom, find_pairs_lcm);
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
  for (Index<MonomialIdeal> j = mi.first(); j.valid(); j++)
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
		  cerr << "removed pair[" << q->first->me << " " 
		    << q->second->me << "]" << endl;;
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
      for (q = p->pair_list; q != NULL; q = q->next)
	{
	  cerr << "insert ";
	  debug_out(q);
	  cerr << endl;
	}
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
      M->divide(s1->lcm, s1->second->f->monom, pi);
      for (s_pair *t1 = s1->next_same; t1 != NULL; t1 = t1->next_same)
	{
	  if (t1->syz_type != SPAIR_PAIR) continue;
	  M->divide(t1->lcm, t1->second->f->monom, pj);
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
  delete [] find_pairs_mon;
  delete [] pi;
  delete [] pj;
  delete [] pij;
}

void GBinhom2_comp::compute_s_pair(s_pair *p)
{
  if (p->f == NULL)
    {
      int *s = M->make_one();
      M->divide(p->lcm, p->first->f->monom, s);
      p->f = F->mult_by_monomial(s, p->first->f);
      p->fsyz = Fsyz->mult_by_monomial(s, p->first->fsyz);
      if (Fsyz->is_quotient_ring) Fsyz->normal_form(p->fsyz);
      if (p->syz_type == SPAIR_PAIR)
	{
	  ring_elem a = R->Ncoeffs()->from_int(1);
	  M->divide(p->lcm, p->second->f->monom, s);
	  F->imp_subtract_multiple_to(p->f, a, s, p->second->f);
	  Fsyz->subtract_multiple_to(p->fsyz, a, s, p->second->fsyz);
	  R->Ncoeffs()->remove(a);
	}
      M->remove(s);
    }
}

int GBinhom2_comp::gb_reduce(vec &f, vec &fsyz)
{
  if ((strategy & USE_GEOBUCKET) != 0) 
    return gb_geo_reduce(f,fsyz);
  Nvecterm head;
  Nvecterm *result = &head;

  monterm *s;
  gb_elem *q;
  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());
  int count = 0;
  while (f != NULL)
    {
      Bag *b;
      M->to_expvector(f->monom, div_totalexp);
      if (F->is_quotient_ring && R->Rideal.search_expvector(div_totalexp, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  M->divide(f->monom, g->monom, reduce_ndiv);
	  F->imp_subtract_ring_multiple_to(f, f->coeff, reduce_ndiv, g);
	  count++;
	}
      else if (largeGB[f->comp]->find_divisor(div_totalexp, s))
	{
	  q = s->bag;
	  ring_elem c = f->coeff;
	  M->divide(f->monom, q->f->monom, reduce_ndiv);
	  Fsyz->subtract_multiple_to(fsyz, c, reduce_ndiv, q->fsyz);
	  F->subtract_multiple_to(f, c, reduce_ndiv, q->f);
	  // Don't remove c: it is (or was) part of f!
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
    cerr << "." << count;

  result->next = NULL;
  f = head.next;
  return 1;
}
int GBinhom2_comp::gb_geo_reduce(vec &f, vec &fsyz)
{
  Nvecterm head;
  Nvecterm *result = &head;

  monterm *s;
  gb_elem *q;
  intarray a_totalexp, a_reduce_ndiv;
  int *div_totalexp = a_totalexp.alloc(M->n_vars());
  int *reduce_ndiv = a_reduce_ndiv.alloc(M->n_vars());
  int count = 0;

  geobucket fb(F);
  geobucket fsyzb(Fsyz);
  fb.add(f);
  fsyzb.add(fsyz);
  Nvecterm *lead;
  while ((lead = fb.remove_lead_term()) != NULL)
    {
      Bag *b;
      M->to_expvector(lead->monom, div_totalexp);
      if (F->is_quotient_ring && R->Rideal.search_expvector(div_totalexp, b))
	{
	  Nterm *g = (Nterm *) b->basis_ptr();
	  M->divide(lead->monom, g->monom, reduce_ndiv);
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  Nvecterm *h = F->imp_ring_mult_by_term(g->next, c, reduce_ndiv, lead->comp);
	  F->remove(lead);
	  R->Ncoeffs()->remove(c);
	  fb.add(h);
	  count++;
	}
      else if (largeGB[lead->comp]->find_divisor(div_totalexp, s))
	{
	  q = s->bag;
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  M->divide(lead->monom, q->f->monom, reduce_ndiv);
	  Nvecterm *h = F->imp_mult_by_term(c, reduce_ndiv, q->f->next);
	  Nvecterm *hsyz = Fsyz->imp_mult_by_term(c, reduce_ndiv, q->fsyz);
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
    cerr << "." << count;
  result->next = NULL;
  f = head.next;

  fsyz = fsyzb.value();
  return 1;
}

void GBinhom2_comp::gb_insert(vec f, vec fsyz, int forced)
{
  gb_elem *p = new gb_elem(f, fsyz, 1);
  p->me = last_gb_num++;
  p->lead_exp = new int[R->n_vars()];
  M->to_expvector(f->monom, p->lead_exp);

  F->make_monic(p->f, p->fsyz);
  if (M->in_subring(1,p->f->monom))
    n_subring++;

  // Next determine the new s pairs.  This also deletes unneeded pairs
  if (!forced) find_pairs(p);

  largeGB[f->comp]->insert(p);
  minimalGB[f->comp]->insert_w_deletions(p);
  // MES: modify n_gb...
  // MES: and also remove the monterms in 'p'.
}

int GBinhom2_comp::s_pair_step(s_pair *p)
     // If no s-pairs left in the current degree, 
     // return SPAIR_DONE.
     // Otherwise, compute the current s-pair, reduce it, and
     // dispatch the result.  Return one of the other SPAIR_*
     // values.
{
  n_computed++;
  if (comp_printlevel >= 8)
    {
      cerr << "--- computing pair ";
      debug_out(p);
      cerr << " ----" << endl;
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
	  cerr << "  gb " << last_gb_num-1 << " = ";
	  F->Nelem_text_out(cerr, f);
	  cerr << endl;
	}
      return SPAIR_GB;
    }
  if (fsyz != NULL)
    {
      if (comp_printlevel >= 8)
	{
	  cerr << "  syz = ";
	  F->Nelem_text_out(cerr, fsyz);
	  cerr << endl;
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

int GBinhom2_comp::computation_complete(int stop_gb, 
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

int GBinhom2_comp::calc(const int */*deg*/, const intarray &stop)
{
  if (stop.length() != 7) 
    {
      *gError << "inappropriate stop conditions for GB computation";
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
	    cerr << "m";
	    break;
	  case SPAIR_SYZ:
	    cerr << "z";
	    break;
	  case SPAIR_ZERO:
	    cerr << "o";
	    break;
	  case SPAIR_REMOVED:
	    cerr << "r";
	    break;
	  default:
	    cerr << "ERROR";
	    break;
	  }
    }
  
  // MES: complete the reduction of the GB here
  if (comp_printlevel >= 1) cerr << endl;
  if (comp_printlevel >= 4)
    {
      cerr << "Number of pairs             = " << n_pairs << endl;
      cerr << "Number of gb elements       = " << n_gb << endl;
      cerr << "Number of gcd=1 pairs       = " << n_saved_gcd << endl;
      cerr << "Number of gcd tails=1 pairs = " << n_saved_lcm << endl;
      cerr << "Number of pairs computed    = " << n_computed << endl;
    }
  return is_done;
}

//--- Reduction --------------------------
Matrix GBinhom2_comp::reduce(const Matrix &m, Matrix &lift)
{
  Matrix red(m.rows(), m.cols());
  lift = Matrix(Fsyz, m.cols());
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

Vector GBinhom2_comp::reduce(const Vector &v, Vector &lift)
{
  if (!v.free_of()->is_equal(F))
    {
      *gError << "reduce: vector is in incorrect free module";
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
Matrix GBinhom2_comp::min_gens_matrix()
{
  Matrix result(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    if (q->is_min)
      result.append(F->copy(q->f));
  return result;
}

Matrix GBinhom2_comp::initial_matrix(int n)
{
  Matrix result(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result.append(F->lead_term(n, q->f));
  return result;
}

Matrix GBinhom2_comp::gb_matrix()
{
  Matrix result(F);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result.append(F->copy(q->f));
  return result;
}

Matrix GBinhom2_comp::change_matrix()
{
  Matrix result(Fsyz);
  for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
    result.append(Fsyz->copy(q->fsyz));
  return result;
}

Matrix GBinhom2_comp::syz_matrix()
{
  return syz;
}

void GBinhom2_comp::debug_out(s_pair *q) const
{
  if (q == NULL) return;
  int *m = M->make_one();
  cerr << "(";
  if (q->first != NULL) cerr << q->first->me; else cerr << ".";
  cerr << " ";
  if (q->second != NULL) cerr << q->second->me; else cerr << ".";
  cerr << " ";
  if (q->first != NULL)
    {
      M->divide(q->lcm, q->first->f->monom, m);
      M->elem_text_out(cerr, m);
      cerr << ' ';
    }
  if (q->second != NULL)
    {
      M->divide(q->lcm, q->second->f->monom, m);
      M->elem_text_out(cerr, m);
      cerr << ' ';
    }
  M->elem_text_out(cerr, q->lcm);
  M->remove(m);
  if (q->compare_num < 0)
    cerr << " marked";
  cerr << ") ";
}

void GBinhom2_comp::debug_pairs_out(gb_elem *p) const
{
  s_pair *q;
  int n = 0;
  for (q = p->pair_list; q != NULL; q = q->next_same)
    {
      debug_out(q);
      n++;
      if (n % 10 == 0) cerr << endl;
    }
  cerr << endl;

}
void GBinhom2_comp::debug_pairs() const
{
  for (gb_elem *p = gbLarge->next; p != NULL; p = p->next)
    debug_pairs_out(p);

  for (int i=0; i<NHEAP; i++)
    {
      s_pair *q = spairs->debug_list(i);
      if (q == NULL) continue;
      cerr << "---- pairs in bin " << i << " -----" << endl;
      int n = 0;
      for ( ; q != NULL; q = q->next)
	{
	  debug_out(q);
	  n++;
	  if (n % 10 == 0) cerr << endl;
	}
      cerr << endl;
    }
}
void GBinhom2_comp::stats() const
{
  spairs->stats();
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    {
      int i = 0;
      for (gb_elem *q = gb->next_min; q != NULL; q = q->next_min)
	{
	  cerr << i << '\t';
	  i++;
	  F->Nelem_text_out(cerr, q->f);
	  cerr << endl;
	}
    }
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


