/* Copyright 2003, Michael E. Stillman */

#include "comp.hpp"
#include "gbA.hpp"
#include "text_io.hpp"
#include <functional>
#include <algorithm>

#include "matrix.hpp"
#include "matrixcon.hpp"
#include "polyring.hpp"
#include "newdelete.hpp"
#include "relem.hpp"
#include "hilb.hpp"

/*************************
 * Initialization ********
 *************************/

gbA * gbA::create(
		  const Matrix *m,
		  M2_bool collect_syz,
		  int n_rows_to_keep,
		  int strategy,
		  M2_bool use_max_degree_limit,
		  int max_degree_limit)
{
  gbA *result = new gbA;
  result->initialize(m, collect_syz, n_rows_to_keep, strategy);
  return result;
}

void gbA::initialize(const Matrix *m, int csyz, int nsyz, int strat)
{
  const PolynomialRing *origR = m->get_ring()->cast_to_PolynomialRing();
  if (origR == NULL)
    {
      ERROR("ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  originalR = origR;
  R = origR->get_gb_ring();

  _nvars = R->get_flattened_monoid()->n_vars();
  _coeff_type = origR->coefficient_type();
  _n_fraction_vars = origR->n_fraction_vars();

  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  _n_rows_per_syz = nsyz;

  _F = m->rows();
  _Fsyz = m->cols()->sub_space(_n_rows_per_syz);  

  _first_in_degree = 0;
  _n_syz = 0;
  _n_pairs_computed = 0;
  _n_gens_left = 0;
  _n_subring = 0;

  _strategy = strat;
  _collect_syz = csyz;
  _is_ideal = (_F->rank() == 1 && csyz == 0);
  if (R->is_weyl_algebra())
    _is_ideal = false;

  _use_hilb = false;
  _hilb_new_elems = false;
  _hilb_n_in_degree = 0;
  _n_saved_hilb = 0;
  _hf_orig = 0;
  _hf_diff = 0;
  _hilb_matrix = 0;

  // set local variables for certain time-critical routines

  _this_degree = _F->lowest_primary_degree() - 1;
  _complete_thru_this_degree = _this_degree;
  set_status(COMP_NOT_STARTED);

  _stats_nreductions = 0;
  _stats_ntail = 0;
  _stats_npairs = 0;
  _stats_ngb = 0;
  _stats_ngcd1 = 0;

  // ZZZZ split
  lookup = 0;
  lookupZZ = 0;
  if (over_ZZ())
    lookupZZ = MonomialTableZZ::make(R->n_vars());
  else
    lookup = MonomialTable::make(R->n_vars());

  if (over_ZZ())
    minimal_gb = new MinimalGB_ZZ(R,_F,_Fsyz);
  else
    minimal_gb = new MinimalGB_Field(R,_F,_Fsyz);
  minimal_gb_valid = true;
  _EXP = R->exponents_make();

  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(_F,(*m)[i], denom);
      spair *p = new_gen(i, f, denom);
      if (p != NULL)
	{
	  spair_set_insert(p);
	  _n_gens_left++;
	}
    }
}

gbA::spair *gbA::new_gen(int i, gbvector *f, ring_elem denom)
{
  gbvector *fsyz;

  if (i < _n_rows_per_syz)
    fsyz = R->gbvector_term(_Fsyz,denom,i+1);
  else
    fsyz = R->gbvector_zero();

  if (R->gbvector_is_zero(f))
    {
      if (!R->gbvector_is_zero(fsyz))
	{
	  //vec fsyzvec = _GR->gbvector_to_vec(_Fsyz,fsyz);
	  collect_syzygy(fsyz);
	}
      return NULL;
    }

  POLY g;
  g.f = f;
  g.fsyz = fsyz;

  return spair_make_gen(g);
}

/*************************
 * GB removal ************
 *************************/

// We might not have to do ANYTHING here, since the garbage collector
// will free everything up for us...
gbA::~gbA()
{
}

/*************************
 * Exponent handling *****
 *************************/

static void exponents_lcm(int nvars, 
			  int dega, 
			  exponents a, 
			  exponents b, 
			  exponents result, 
			  int &result_degree)
{
  int i;
  int deg = dega;
  for (i=0; i<nvars; i++)
    {
      int diff = b[i] - a[i];
      if (diff <= 0)
	result[i] = a[i];
      else
	{
	  result[i] = b[i];
	  deg += diff;
	}
    }
  result_degree = deg;
}


static bool exponents_equal(int nvars, exponents a, exponents b)
{
  for (int i=0; i<nvars; i++)
    if (a[i] != b[i]) return false;
  return true;
}

static bool exponents_less_than(int nvars, exponents a, exponents b)
{
  for (int i=0; i<nvars; i++)
    {
      if (a[i] < b[i]) return true;
      if (a[i] > b[i]) return false;
    }
  return false;
}

/*************************
 * gbelem handling *******
 *************************/

gbA::gbelem *gbA::gbelem_make(gbvector *f,  // grabs f
			      gbvector *fsyz, // grabs fsyz
			      gbelem_type minlevel,
			      int deg)
{
  gbelem *g = new gbelem;
  g->g.f = f;
  g->g.fsyz = fsyz;
  g->lead = R->exponents_make();
  R->gbvector_get_lead_exponents(_F, f, g->lead);
  g->deg = deg;
  g->alpha = deg - R->gbvector_term_weight(_F,f);
  g->minlevel = minlevel;
  return g;
}

/*************************
 * SPair handling ********
 *************************/

gbA::spair *gbA::spair_node()
{
  spair *result = new spair;
  result->next = 0;
  return result;
}

void gbA::spair_delete(spair *&p)
{
  // MES: delete the exponent first?
  deleteitem(p);
}

gbA::spair *gbA::spair_make(int i, int j)
{
  gbelem *g1 = gb[i];
  gbelem *g2 = gb[j];
  exponents exp1 = g1->lead;
  exponents exp2 = g2->lead;
  spair *result = spair_node();
  result->next = 0;
  result->type = SPAIR_SPAIR;
  result->lcm = R->exponents_make();
    exponents_lcm(_nvars, g1->deg, exp1, exp2, result->lcm, result->deg);
  result->x.pair.i = i;
  result->x.pair.j = j;

  return result;
}

gbA::spair *gbA::spair_make_gcd_ZZ(int i, int j)
{
  spair *result = spair_make(i,j);
  result->type = SPAIR_GCD_ZZ;
  return result;
}

gbA::spair *gbA::spair_make_gen(POLY f)
{
  assert(f.f != 0);
  exponents exp1 = R->exponents_make();
  R->gbvector_get_lead_exponents(_F, f.f, exp1);
  int deg = R->gbvector_degree(_F, f.f);
  spair *result = spair_node();
  result->next = 0;
  result->type = SPAIR_GEN;
  result->deg = deg;
  result->lcm = exp1;
  result->x.f = f;

  return result;
}

gbA::spair *gbA::spair_make_skew(int i, int v)
{
  spair *result;
  int j;
  gbelem *g1 = gb[i];
  exponents exp1 = g1->lead;
  exponents exp2 = R->exponents_make();
  for (j=0; j<_nvars; j++)
    exp2[j] = 0;
  exp2[v] = 2;
  result = spair_node();
  result->next = 0;
  result->type = SPAIR_SKEW;
  result->lcm = exp2;
    exponents_lcm(_nvars, g1->deg, exp1, exp2, exp2, result->deg);
    // note: result is being placed into exp2, from the input exp2.
    // This is OK, I hope.
  result->x.pair.i = i;
  result->x.pair.j = v;

  return result;
}

gbA::spair *gbA::spair_make_ring(int i, int j)
{
  /* This requires that j indexes into the gb array somewhere. */
  spair *result = spair_make(i,j);
  result->type = SPAIR_RING;

  return result;
}

void gbA::spair_text_out(buffer &o, spair *p)
{
  char s[100]; // enough room for all of the non polynomial cases.
  switch (p->type) {
  case SPAIR_GCD_ZZ:
    sprintf(s, "spairgcd(%d,%d)", p->x.pair.i, p->x.pair.j);
    o << s;
    sprintf(s, " deg(%d)", p->deg);
    o << s;
    o << " lcm[";
    for (int i=0; i<_nvars+2; i++)
      {
	sprintf(s, "%d ", p->lcm[i]);
	o << s;
      }
    o << "]";
    break;
  case SPAIR_SPAIR:
    sprintf(s, "spair(%d,%d)", p->x.pair.i, p->x.pair.j);
    o << s;
    sprintf(s, " deg(%d)", p->deg);
    o << s;
    o << " lcm[";
    for (int i=0; i<_nvars+2; i++)
      {
	sprintf(s, "%d ", p->lcm[i]);
	o << s;
      }
    o << "]";
    break;
  case SPAIR_GEN:
    o << "gen ";
    R->gbvector_text_out(o, _F, p->f());
    break;
  case SPAIR_ELEM:
    o << "elem ";
    R->gbvector_text_out(o, _F, p->f());
    break;
  case SPAIR_RING:
    sprintf(s, "rpair(%d,%d)", p->x.pair.i, p->x.pair.j);
    o << s;
    break;
  case SPAIR_SKEW:
    sprintf(s, "skewpair(%d,%d)", p->x.pair.i, p->x.pair.j);
    o << s;
    break;
  default:
    o << "unknown pair";
    break;
  }
  o << newline;
}

/*************************
 * S-pair heuristics *****
 *************************/

bool gbA::pair_not_needed(spair *p, gbelem *m)
{
  /* Check the criterion: in(m) divides lcm(p).
   * If so: check if lcm(p1,m) == lcm(p)  (if so, return false)
   *        check if lcm(p2,m) == lcm(p)  (if so, return false)
   * If still here, return true.
   */
  int i, first, second;
  bool firstok;
  exponents mexp, lcm, p1exp, p2exp;
  if (p->type != SPAIR_SPAIR && p->type != SPAIR_RING) return false;
  mexp = m->lead;
  lcm = p->lcm;
  if (gbelem_COMPONENT(m) != 
      spair_COMPONENT(p)) 
    return false;

  first = p->x.pair.i;
  second = p->x.pair.j;
  p1exp = gb[first]->lead;
  p2exp = gb[second]->lead; /* If a ring pair, this should index into gb array */

  for (i=0; i<_nvars; i++)
    if (mexp[i] > lcm[i]) return false;
      
  firstok = false;
  for (i=0; i<_nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p1exp[i] == lcm[i]) continue;
      firstok = true;
      break;
    }
  if (!firstok) return false;
  for (i=0; i<_nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p2exp[i] == lcm[i]) continue;
      return true;
    }
  return false;
}

void gbA::remove_unneeded_pairs(int id)
{
  /* Removes all pairs from C->S that are not needed */
  spair head;
  spair *p = &head;
  gbelem *m = gb[id];

  head.next = S.heap;
  while (p->next != 0)
    if (pair_not_needed(p->next, m))
      {
	spair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
	if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
	  {
	    buffer o;
	    o << "removing unneeded ";
	    spair_text_out(o, tmp);
	    emit_line(o.str());
	  }
	spair_delete(tmp);
	S.nelems--;
      }
  else
    p = p->next;
  S.heap = head.next;
}

bool gbA::is_gcd_one_pair(spair *p)
{
  int i,j;
  exponents e1, e2;
  if (p->type != SPAIR_SPAIR) return false;
  i = p->x.pair.i;
  j = p->x.pair.j;
  e1 = gb[i] -> lead;
  e2 = gb[j] -> lead;
  for (i=0; i<_nvars; i++)
    if (e1[i] > 0 && e2[i] > 0)
      return false;
  return true;
}

gbA::spairs::iterator gbA::choose_pair(gbA::spairs::iterator first, 
				       gbA::spairs::iterator next)
{
  /* a is an array of spair's, and a[first], ..., a[next-1] all have the
     same lcm, which is a minimal monomial generator of all such lcm's.
     Our goal is to choose a nice one, and throw away the others.
     We return one spair, and delete the rest.
  */
  if (next == first+1) return first;
  return first; /* MES: really do something here... */
}

struct spair_sorter : public binary_function<gbA::spair *,gbA::spair *,bool> {
  int nvars;
  spair_sorter(int nv) : nvars(nv) {}
#if 0
  bool operator()(gbA::spair *a, gbA::spair *b)
    {
      /* Compare using degree, then type, then lcm */
      int cmp = a->deg - b->deg;
      if (cmp < 0) return true;
      if (cmp > 0) return false;
      cmp = a->type - b->type;
      if (cmp < 0) return true;
      if (cmp > 0) return false;
      return !exponents_greater(nvars,a->lcm, b->lcm);
    }
#endif
  bool operator()(gbA::spair *a, gbA::spair *b)
    {
      /* Compare using degree, then type, then lcm */
      bool result;
      int cmp = a->deg - b->deg;
      if (cmp < 0) result = true;
      else if (cmp > 0) result = false;
      else {
	cmp = a->type - b->type;
	if (cmp < 0) result = true;
	else if (cmp > 0) result = false;
	else result = exponents_less_than(nvars,a->lcm, b->lcm);
      }
      return result;
    }

};

// ZZZZ split
void gbA::minimalize_pairs_non_ZZ(spairs &new_set)
     /* new_set: array of spair*  */
{
#if 0
  spairs keep_for_now;
  emit("--minimalize pairs--\n");
  for (int i=0; i<new_set.size(); i++) {
    keep_for_now.push_back(new_set[i]);
    debug_spair(new_set[i]);
  }
#endif
  sort(new_set.begin(), new_set.end(), spair_sorter(_nvars));
  MonomialTable *montab = MonomialTable::make(_nvars);

  //  array_sort(new_set, (compareFcn)spair_compare, 0);
  spairs::iterator first = new_set.begin();
  spairs::iterator next = first;
  spairs::iterator end = new_set.end();
  for ( ; first != end; first = next)
    {
      next = first+1;
      spair *me = *first;
      while (next != end)
	{
	  spair *p = *next;
	  if (!exponents_equal(_nvars, me->lcm, p->lcm)) break;
	  next++;
	}
      /* At this point: [first,next) is the range of equal monomials */
      
      int inideal = montab->find_divisors(1, me->lcm, 1);
      if (inideal == 0)
	{
	  spairs::iterator t = choose_pair(first, next);
	  spair *p = *t;
	  if (_is_ideal && is_gcd_one_pair(p))
	    {
	      _stats_ngcd1++;
	      if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
		{
		  buffer o;
		  o << "removing spair because of gcd: ";
		  spair_text_out(o, p);
		  emit_line(o.str());
		}
	      spair_delete(p);
	    }
	  else
	    {
	      spair_set_insert(p);
	      montab->insert(p->lcm, 1, 0);
	    }
	  *t = 0;
	}
    }

  deleteitem(montab);
  for (spairs::iterator i = new_set.begin(); i != new_set.end(); i++)
    spair_delete(*i);
}

void gbA::minimalize_pairs_ZZ(spairs &new_set)
{
  // Prune down the set of spairs to a 'minimal' set.  For each one, we 
  // need to add in a "gcd" combination spair as well.

  vector<mpz_ptr,gc_alloc> coeffs;
  vector<mpz_ptr,gc_alloc> coeffs2;
  vector<exponents,gc_alloc> exps;
  vector<int,gc_alloc> comps;
  vector<int,gc_alloc> positions;
  
  coeffs.reserve(gb.size());
  coeffs2.reserve(gb.size());
  exps.reserve(gb.size());
  comps.reserve(gb.size());

  for (vector<spair *,gc_alloc>::iterator i = new_set.begin(); i != new_set.end(); i++)
    {
      spair *a = *i;
      exps.push_back(a->lcm);
      comps.push_back(1); /* This is not needed here, as all of these 
                             have the same component */
      /* Now get the coefficient */
      /* This is the lcm divided by the lead coeff, but it depends on the kind of spair */
      if (a->type == SPAIR_SKEW)
	coeffs.push_back(MPZ_VAL(globalZZ->one()));
      else 
	{
	  /* */
	  gbvector *f1 = gb[a->x.pair.i]->g.f;
	  gbvector *f2 = gb[a->x.pair.j]->g.f;
	  ring_elem u,v;
	  globalZZ->syzygy(f1->coeff, f2->coeff, u, v);
	  coeffs.push_back(MPZ_VAL(u));
	  coeffs2.push_back(MPZ_VAL(v));
	}
    }  

  MonomialTableZZ::find_weak_generators(_nvars, coeffs, exps, comps, positions);

  for (vector<int,gc_alloc>::iterator i = positions.begin(); i != positions.end(); i++)
    {
      // Insert this spair, and also the corresponding gcd one.
      spair *p = new_set[*i];
      spair_set_insert(p);
      mpz_ptr u = coeffs[*i];
      mpz_ptr v = coeffs2[*i];
      if (p->type != SPAIR_SKEW && mpz_cmpabs_ui(u,1) && mpz_cmpabs_ui(v,1))
	{
	  spair *p2 = spair_make_gcd_ZZ(p->x.pair.i, p->x.pair.j);
	  spair_set_insert(p2);
	}
    }
}

void gbA::minimalize_pairs(spairs &new_set)
{
  if (over_ZZ())
    minimalize_pairs_ZZ(new_set);
  else
    minimalize_pairs_non_ZZ(new_set);
}

void gbA::update_pairs(int id)
{
  gbelem *r = gb[id];
  int x = gbelem_COMPONENT(r);

  /* Step 1.  Remove un-needed old pairs */
  remove_unneeded_pairs(id);
  
  /* Step 2.  Collect new pairs */
  spairs new_set;

  /* Step 2a: */
  if (R->is_skew_commutative())
    {
      for (int i=0; i<R->n_skew_commutative_vars(); i++)
	if (r->lead[R->skew_variable(i)] > 0)
	  {
	    spair *s = spair_make_skew(id,i);
	    new_set.push_back(s);
	  }
    }
  /* Step 2b: pairs from ring elements, or 'in stone' elements */
  for (int i=0; i<_first_gb_element; i++)
    {
      spair *s = spair_make_ring(id,i);
      new_set.push_back(s);
    }
  /* Step 2c. pairs from the vectors themselves */
  /* Loop through the minimal GB elements and form the s-pair */
  for (int i=_first_gb_element; i<id; i++)
    {
      gbelem *g = gb[i];
      if (g->minlevel <= ELEM_MIN_GB && gbelem_COMPONENT(g) == x)
	{
	  spair *s = spair_make(id,i);
	  new_set.push_back(s);
	}
    }

  /* Step 3. Minimalize this set */
  minimalize_pairs(new_set); /* Modifies new_set, inserts minimal pairs into S */
}

/*************************
 * S-pair sets ***********
 *************************/


gbA::SPairSet::SPairSet()
  : nelems(0), 
    n_in_degree(0), 
    heap(0), 
    this_set(0), 
    n_computed(0)
{
}

#if 0
gbA::SPairSet::~SPairSet()
{
  spair *set = heap;
  while (!set)
    {
      spair *tmp = set;
      set = set->next;
      spair_delete(tmp);
    }
  set = this_set;
  while (!set)
    {
      spair *tmp = set;
      set = set->next;
      spair_delete(tmp);
    }
}
#endif

void gbA::spair_set_insert(gbA::spair *p)
  /* Insert a LIST of s pairs into S */
{
  while (p != 0)
    {
      spair *tmp = p;
      p = p->next;
      S.nelems++;
      tmp->next = S.heap;
      S.heap = tmp;
    }
}
 
gbA::spair *gbA::spair_set_next()
  /* Removes the next element of the current degree, returning NULL if none left */
{
  spair *result;
  if (!S.this_set) return 0;

  result = S.this_set;
  S.this_set = S.this_set->next;
  result->next = 0;
  S.nelems--;
  S.n_in_degree--;
  S.n_computed++;
  return result;
}

int gbA::spair_set_determine_next_degree(int &nextdegree)
{
  spair *p;
  int nextdeg;
  int len = 1;
  if (S.heap == 0) return 0;
  nextdeg = S.heap->deg;
  for (p = S.heap->next; p!=0; p=p->next)
    if (p->deg > nextdeg) 
      continue;
    else if (p->deg < nextdeg)
      {
	len = 1;
	nextdeg = p->deg;
      }
  else
    len++;
  nextdegree = nextdeg;
  return len;
}

int gbA::spair_set_prepare_next_degree(int &nextdegree)
  /* Finds the next degree to consider, returning the number of spairs in that degree */
{
  S.this_set = 0;
  int len = spair_set_determine_next_degree(nextdegree);
  if (len == 0) return 0;

  spair head;
  spair *p;
  head.next = S.heap;
  p = &head;
  while (p->next != 0)
    if (p->next->deg != nextdegree)
      p = p->next;
    else
      {
	spair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = S.this_set;
	S.this_set = tmp;
      }
  S.heap = head.next;
  S.n_in_degree = len;

  /* Now sort 'this_set'. */
  spairs_sort(len, S.this_set);
  //  G->spairs_reverse(this_set);
  return len;
}

void gbA::spairs_reverse(spair *&ps)
{
  spair *reversed = 0;
  spair *p = ps;
  while (p != 0)
    {
      spair *tmp = p;
      p = p->next;
      tmp->next = reversed;
      reversed = tmp;
    }
  ps = reversed;
}

/* Sorting a list of spairs */
void gbA::spairs_sort(int len, spair *&ps)
{
  spairs a; // array of spair's
  a.reserve(len);
  for (spair *p = ps; p != 0; p=p->next)
    a.push_back(p);

  sort(a.begin(), a.end(), spair_sorter(_nvars));

  spairs::iterator j = a.begin();

  for (spairs::iterator i = j+1; i != a.end(); j=i, i++)
    (*j)->next = *i;
  (*j)->next = 0;

  ps = a[0];
}



/****************************************
 * Polynomial arithmetic and reduction **
 ****************************************/


void gbA::compute_s_pair(spair *p)
{
  POLY f,g;
  if (gbTrace >= 5)
    {
      buffer o;
      spair_text_out(o,p);
      emit_line(o.str());
    }
  if (p->type > SPAIR_SKEW) return;
  f = gb[p->x.pair.i]->g;
  if (p->type == SPAIR_SKEW)
    {
#if 0
      // MES: need to implement skew_poly...
      gbvector *g1 = R->skew_poly(p->x.pair.j);
      R->gbvector_mult_by_term(_F,_Fsyz,
			       R->one(), g1->monom,
			       f.f, f.fsyz,
			       p->f(), p->fsyz());
#endif
    }
  else if (p->type == SPAIR_GCD_ZZ)
    {
      g = gb[p->x.pair.j]->g;
      R->gbvector_combine_lead_terms_ZZ(_F, _Fsyz, 
					f.f, f.fsyz,
					g.f,g.fsyz,
					p->f(),
					p->fsyz());
    }
  else
    {
      g = gb[p->x.pair.j]->g;
      R->gbvector_cancel_lead_terms(_F, _Fsyz, 
				    f.f, f.fsyz,
				    g.f,g.fsyz,
				    p->f(),
				    p->fsyz());
    }
  p->type = SPAIR_ELEM;
  if (gbTrace >= 5)
    {
      buffer o;
      o << "    ";
      R->gbvector_text_out(o, _F, p->f());
      emit_line(o.str());
    }
}


// ZZZZ split
bool gbA::reduce(spair *p)
{
  /* Returns false iff we defer computing this spair. */
  /* If false is returned, this routine has grabbed the spair 'p'. */
  int count = 0;
  compute_s_pair(p); /* Changes the type, possibly */
  if (gbTrace == 10)
    {
      buffer o;
      o << "reducing ";
      R->gbvector_text_out(o, _F, p->f());
      emit_line(o.str());
    }
  //  exponents _EXP = R->exponents_make();
  while (!R->gbvector_is_zero(p->f()))
    {
      int alpha,w;
      R->gbvector_get_lead_exponents(_F, p->f(), _EXP);
      int x = p->f()->comp;
      if (over_ZZ())
	{
	  mpz_ptr c = MPZ_VAL(p->f()->coeff);
	  w = find_good_term_divisor_ZZ(c,_EXP,x,_this_degree,alpha);

	  // If w < 0, then no divisor was found.  Is there a GB element of
	  // the same degree as this one, and with the same exponent vector?
	  // If so, use gcdextended to find (g,u,v), 
	  if (w < 0)
	  {
	    MonomialTableZZ::mon_term *t = lookupZZ->find_exact_monomial(_EXP,
									 x,
									 _first_in_degree);
	    if (t != 0)
	      {
		// f <-- u*p+v*f (same with syz versions), need to change lookupZZ too?
		// p <-- c*p-d*f
		gbelem *g = gb[t->_val];
		if (gbTrace == 10)
		  {
		    buffer o;
		    o << "swapping GB element\n    ";
		    R->gbvector_text_out(o, _F, p->f());
		    o << "\n    and ";
		    R->gbvector_text_out(o, _F, g->g.f);
		    o << "\n  giving\n    ";
		    emit(o.str());
		  }
		R->gbvector_replace_2by2_ZZ(_F, _Fsyz, p->f(), p->fsyz(), g->g.f, g->g.fsyz);
		lookupZZ->change_coefficient(t, MPZ_VAL(g->g.f->coeff));
		if (gbTrace == 10)
		  {
		    buffer o;
		    R->gbvector_text_out(o, _F, p->f());
		    o << "\n    and ";
		    R->gbvector_text_out(o, _F, g->g.f);
		    o << "\n";
		    emit(o.str());
		  }
		continue;
	      }
	  }
	}
      else
	{
	  w = find_good_divisor(_EXP,x,_this_degree, alpha); 
	}

	
      // replaced alpha, g.
      if (w < 0) break;
      count++;
      if (alpha > 0)
	{
	  POLY h;
	  h.f = R->gbvector_copy(p->x.f.f);
	  h.fsyz = R->gbvector_copy(p->x.f.fsyz);
	  insert(h,ELEM_NON_MIN_GB);
	  if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
	    {
	      buffer o;
	      o << "deferring A spair ";
	      spair_text_out(o,p);
	      emit_line(o.str());
	    }
	}
      POLY g = gb[w]->g;

      if (over_ZZ())
	{
	  R->gbvector_reduce_lead_term_ZZ(_F,_Fsyz,p->f(), p->fsyz(),
					  g.f, g.fsyz);
	}
      else
	{
	  R->gbvector_reduce_lead_term(_F, _Fsyz,
				       0,
				       p->f(), p->fsyz(), /* modifies these */
				       g.f, g.fsyz);
	}
      _stats_nreductions++;
      if (gbTrace == 10)
	{
	  buffer o;
	  o << "  reducing by ";
	  R->gbvector_text_out(o, _F, g.f);
	  o << newline << "    giving ";
	  R->gbvector_text_out(o, _F, p->f());
	  emit_line(o.str());
	}
      if (R->gbvector_is_zero(p->f())) break;
      if (alpha > 0)
	{
	  p->deg += alpha;
	  if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
	    {
	      buffer o;
	      o << "deferring B spair ";
	      spair_text_out(o,p);
	      emit_line(o.str());
	    }
	  spair_set_insert(p);
	  R->exponents_delete(_EXP);
	  return false;
	}
    }
  if (gbTrace == 3) 
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }
  //R->exponents_delete(_EXP);
  return true;
}

/***********************
 * gbasis routines *****
 ***********************/

int gbA::find_good_monomial_divisor_ZZ(
				       mpz_ptr c,
				       exponents e,
				       int x,
				       int degf, 
				       int &result_alpha)
{
  // Get all of the term divisors.
  // Choose one with the smallest alpha.
  int i, alpha, newalpha, ealpha;
  int n = 0;

  vector<MonomialTableZZ::mon_term *,gc_alloc> divisors;
  ealpha = degf - R->exponents_weight(e);

  /* First search for ring divisors */
#if 0
  /* MES: removed until ringtable is functional */
  n += ringtable->find_divisors(-1, e, 1, &divisors);
#endif

  /* Next search for GB divisors */
  n += lookupZZ->find_monomial_divisors(-1, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    return -1;
  MonomialTableZZ::mon_term *t = divisors[0];
  gbelem *tg = gb[t->_val];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=1; i<n; i++)
      {
	t = divisors[i];
	tg = gb[t->_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  break;
	} else if (newalpha < alpha) alpha = newalpha;
      }
  result_alpha = alpha;
  return t->_val;
}

int gbA::find_good_term_divisor_ZZ(
				   mpz_ptr c,
				   exponents e,
				   int x,
				   int degf, 
				   int &result_alpha)
{
  // Get all of the term divisors.
  // Choose one with the smallest alpha.
  int i, alpha, newalpha, ealpha;
  int n = 0;

  vector<MonomialTableZZ::mon_term *,gc_alloc> divisors;
  ealpha = degf - R->exponents_weight(e);

  /* First search for ring divisors */
#if 0
  /* MES: removed until ringtable is functional */
  n += ringtable->find_divisors(-1, e, 1, &divisors);
#endif

  /* Next search for GB divisors */
  n += lookupZZ->find_term_divisors(-1, c, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    return -1;
  MonomialTableZZ::mon_term *t = divisors[0];
  gbelem *tg = gb[t->_val];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=1; i<n; i++)
      {
	t = divisors[i];
	tg = gb[t->_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  break;
	} else if (newalpha < alpha) alpha = newalpha;
      }
  result_alpha = alpha;
  return t->_val;
}

int gbA::find_good_divisor(exponents e,
			   int x,
			   int degf, 
			   int &result_alpha)
  // Returns an integer w.
  // if w >=0: gb[w]'s lead term divides [e,x].
  // if w<0: no gb[w] has lead term dividing [e,x].
{
  int i, alpha, newalpha, ealpha;
  int n = 0;

  vector<MonomialTable::mon_term *,gc_alloc> divisors;
  ealpha = degf - R->exponents_weight(e);

  /* First search for ring divisors */
#if 0
  /* MES: removed until ringtable is functional */
  n += ringtable->find_divisors(-1, e, 1, &divisors);
#endif

  /* Next search for GB divisors */
  n += lookup->find_divisors(-1, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    return -1;
  MonomialTable::mon_term *t = divisors[0];
  gbelem *tg = gb[t->_val];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=1; i<n; i++)
      {
	t = divisors[i];
	tg = gb[t->_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  break;
	} else if (newalpha < alpha) alpha = newalpha;
      }
  result_alpha = alpha;
  return t->_val;
}

void gbA::remainder(POLY &f, int degf, bool use_denom, ring_elem &denom)
{
  if (over_ZZ())
    remainder_ZZ(f,degf,use_denom,denom);
  else
    remainder_non_ZZ(f,degf,use_denom,denom);
}

void gbA::remainder_ZZ(POLY &f, int degf, bool use_denom, ring_elem &denom)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  int count = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      R->gbvector_get_lead_exponents(_F, h.f, _EXP);
      int x = h.f->comp;
      int w = find_good_monomial_divisor_ZZ(MPZ_VAL(h.f->coeff),_EXP,x,degf,  alpha);
        // replaced alpha, g.
      if (w < 0 || alpha > 0)
	{
	  frem->next = h.f;
	  frem = frem->next;
	  h.f = h.f->next;
	  frem->next = 0;
	}
      else
	{
	  POLY g = gb[w]->g;
	  if (!R->gbvector_reduce_lead_term_ZZ(_F, _Fsyz,
					       h.f, h.fsyz,
					       g.f, g.fsyz))
	    {
	      // This term is still there, so we must move it to result.
	      frem->next = h.f;
	      frem = frem->next;
	      h.f = h.f->next;
	      frem->next = 0;
	    }
	  count++;
	  //	  _stats_ntail++;
	  if (gbTrace == 10)
	    {
	      buffer o;
	      o << "  tail reducing by ";
	      R->gbvector_text_out(o,_F,g.f);
	      o << "\n    giving ";
	      R->gbvector_text_out(o,_F,h.f);
	      emit_line(o.str());
	    }
	  
	}
    }
  h.f = head.next;
  // Negate these if needed
  if (mpz_sgn(MPZ_VAL(h.f->coeff)) < 0)
    {
      R->gbvector_mult_by_coeff_to(h.f, globalZZ->minus_one());
      R->gbvector_mult_by_coeff_to(h.fsyz, globalZZ->minus_one());
    }
  f.f = h.f;
  f.fsyz = h.fsyz;
  if (gbTrace == 3)
    {
      buffer o;
      o << "," << count;
      emit(o.str());
    }
}

void gbA::remainder_non_ZZ(POLY &f, int degf, bool use_denom, ring_elem &denom)
  // find the remainder of f = [g,gsyz] wrt the GB,
  // i.e. replace f with h[h,hsyz], st
  // base not ZZ:
  //    h = f - sum(a_i * g_i),  in(f) not in in(G)
  //    hsyz = fsyz - sum(a_i * gsyz_i)
  //    denom is unchanged
  // base is ZZ:
  //    h = c*f - sum(a_i * g_i), in(f) not in in(G),
  //    hsyz = c*fsyz - sum(a_i * gsyz_i)
  //    but a_i,h are all polynomials with ZZ coefficients (not QQ).
  //    denom *= c
  // (Here: G = (g_i) is the GB, and a_i are polynomials generated
  // during division).
  // c is an integer, and is returned as 'denom'.
  // Five issues:
  // (a) if gcd(c, coeffs(f)) becomes > 1, can we divide
  //     c, f, by this gcd? If so, how often do we do this?
  // (b) do we reduce by any element of the GB, or only those whose
  //     sugar degree is no greater than degf?
  // (c) can we exclude an element of the GB from the g_i?
  //     (for use in auto reduction).
  // (d) can we reduce by the minimal GB instead of the original GB?
  //     ANSWER: NO.  Instead, use a routine to make a new GB.
  // (e) Special handling of quotient rings: none needed.
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  int count = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      R->gbvector_get_lead_exponents(_F, h.f, _EXP);
      int x = h.f->comp;
      int w = find_good_divisor(_EXP,x,degf,  alpha);
        // replaced alpha, g.
      if (w < 0 || alpha > 0)
	{
	  frem->next = h.f;
	  frem = frem->next;
	  h.f = h.f->next;
	  frem->next = 0;
	}
      else
	{
	  POLY g = gb[w]->g;
	  R->gbvector_reduce_lead_term(_F, _Fsyz,
				       head.next,
				       h.f, h.fsyz,
				       g.f, g.fsyz,
				       use_denom, denom);
	  count++;
	  //	  _stats_ntail++;
	  if (gbTrace == 10)
	    {
	      buffer o;
	      o << "  tail reducing by ";
	      R->gbvector_text_out(o,_F,g.f);
	      o << "\n    giving ";
	      R->gbvector_text_out(o,_F,h.f);
	      emit_line(o.str());
	    }
	  
	}
    }
  h.f = head.next;
  R->gbvector_remove_content(h.f, h.fsyz, use_denom, denom);
  f.f = h.f;
  f.fsyz = h.fsyz;
  if (gbTrace == 3)
    {
      buffer o;
      o << "," << count;
      emit(o.str());
    }
}

/********************
 ** State machine ***
 ********************/

// ZZZZ split
void gbA::auto_reduce_by(int id)
{
  /* Loop backwards while degree doesn't change */
  /* Don't change quotient ring elements */
  gbelem *me = gb[id];
  for (int i=id-1; i>=_first_gb_element; i--)
    {
      gbelem *g = gb[i];
      if (g->deg < me->deg) return;
      if (gbTrace == 10)
	{
	  buffer o;
	  o << "auto reduce " << id << " by " << i;
	  emit_line(o.str());
	}
      if (over_ZZ())
	{
	  R->gbvector_auto_reduce_ZZ(_F, _Fsyz,
				  g->g.f, g->g.fsyz, // these are modified
				  me->g.f, me->g.fsyz);
	}
      else
	{
	  R->gbvector_auto_reduce(_F, _Fsyz,
				  g->g.f, g->g.fsyz, // these are modified
				  me->g.f, me->g.fsyz);
	}
    }
}

void gbA::insert(POLY f, int minlevel)
{
  /* Reduce this element as far as possible.  This either removes content, 
     makes it monic, or at least negates it so the lead coeff is positive. */
  ring_elem junk;
  remainder(f,_this_degree,false,junk);

  _stats_ngb++;

  //  int me = G->insert(f.f, f.fsyz, (gbelem_type)minlevel, _this_degree);

#warning "this cast should not be needed"
  gbelem *g = gbelem_make(f.f, f.fsyz, static_cast<gbelem_type>(minlevel), _this_degree);
  minimal_gb_valid = false;
  int me = gb.size();
  gb.push_back(g);
  
  int x = g->g.f->comp;

  // ZZZZ split
  if (over_ZZ())
    lookupZZ->insert(MPZ_VAL(g->g.f->coeff),g->lead,x,me);
  else
    lookup->insert(g->lead, x, me);

  if (gbTrace >= 4)
    {
      char s[100];
      buffer o;
      sprintf(s, "%sinserting element %d (minimal %d): ",wrapping_prefix,me,minlevel);
      o << s;
      R->gbvector_text_out(o,_F,g->g.f);
      o << "\n" << wrapping_prefix;
      R->gbvector_text_out(o,_Fsyz,g->g.fsyz);
      emit_line(o.str());
    }
  if (minlevel <= ELEM_MIN_GB)
    update_pairs(me);

  auto_reduce_by(me);

  if (_hilb_matrix)
    {
      // _hilb_matrix is non-zero if codim test is set, or _use_hilb is set
      // Make the element 1 * m * comp, as an element of _hilb_matrix->get_ring().
      // and append it as the last column of _hilb_matrix.

      ring_elem a = originalR->get_flattened_ring()->term(originalR->Ncoeffs()->one(), g->g.f->monom);
      _hilb_matrix->append_column(0);
      _hilb_matrix->set_entry(g->g.f->comp-1,_hilb_matrix->n_cols()-1, a);
      
      if (_use_hilb)
	{
	  _hilb_new_elems = true;
	  if (--_hilb_n_in_degree == 0) flush_pairs();
	}
      else
	{
	  // codim test is set.  Compute the codimension now.
	}
    }

  if (gbTrace >= 10)
    {
      lookupZZ->showmontable();
      showgb();
    }
}

void gbA::collect_syzygy(gbvector *f)
{
  _syz.push_back(f);
  _n_syz++;

}

void gbA::handle_elem(POLY f, int minlevel)
{
  if (!R->gbvector_is_zero(f.f))
    {
      insert(f,minlevel);
      if (gbTrace == 3)
	emit("m");
    }
  else if (!R->gbvector_is_zero(f.fsyz))
    {
      /* This is a syzygy */
      collect_syzygy(f.fsyz);
      if (gbTrace == 3)
	emit("z");
    }
  else
    {
      if (gbTrace == 3)
	emit("o");
    }
}

bool gbA::s_pair_step()
{
  int minlevel;
  spair *p = spair_set_next();
  if (!p) return false;

  _stats_npairs++;
  minlevel = (p->type == SPAIR_GEN ? 0 : 1); /* MES: Fix this line */

  if (reduce(p)) /* i.e. if the reduction is not deferred */
    {
      POLY f = p->x.f;
      spair_delete(p);

      handle_elem(f,minlevel);
    }
  return true;
}

enum ComputationStatusCode gbA::computation_is_complete()
{
  // This handles everything but _Stop.always, _Stop.degree_limit
  if (_Stop.basis_element_limit > 0 && gb.size() > _Stop.basis_element_limit) 
    return COMP_DONE_GB_LIMIT;
  if (_Stop.syzygy_limit > 0 && _n_syz > _Stop.syzygy_limit)
    return COMP_DONE_SYZ_LIMIT;
  if (_Stop.pair_limit > 0 && _n_pairs_computed > _Stop.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (_Stop.just_min_gens && _n_gens_left == 0)
    return COMP_DONE_MIN_GENS;
  if (_Stop.subring_limit > 0 && _n_subring > _Stop.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (_Stop.use_codim_limit)
    {
      // Compute the codimension
      int c = 0;
      //int c = codim_of_lead_terms();
      if (c >= _Stop.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

void gbA::start_computation()
{
  int npairs;
  enum ComputationStatusCode is_done = COMP_COMPUTING;

  for (;;)
    {
      if (system_interrupted) 
	{
	  is_done = COMP_INTERRUPTED;
	  break;
	}

      is_done = computation_is_complete();
      if (is_done != COMP_COMPUTING) break;

      /* If we need to move to the next degree, do it. */
      if (S.n_in_degree  == 0)
	{
	  if (_hilb_new_elems)
	    {
	      // Recompute h, _hf_diff
	      RingElement *h = hilb_comp::hilbertNumerator(_hilb_matrix);
	      if (h == 0)
		{
		  is_done = COMP_INTERRUPTED;
		  break;
		}
	      _hf_diff = (*h) - (*_hf_orig);
	      _hilb_new_elems = false;
	    }

	  int old_degree = _this_degree;
	  npairs = spair_set_prepare_next_degree(_this_degree); // sets _this_degree
	  if (old_degree < _this_degree)
	    _first_in_degree = gb.size();
	  _complete_thru_this_degree = _this_degree-1;
	  if (npairs == 0)
	    {
	      is_done = COMP_DONE;
	      break;
	    }
	  if (_Stop.stop_after_degree && _this_degree > _Stop.degree_limit->array[0])
	    {
	      is_done = COMP_DONE_DEGREE_LIMIT;
	      break;
	    }

	  if (_use_hilb)
	    {
	      _hilb_n_in_degree = hilb_comp::coeff_of(_hf_diff, _this_degree);
	      if (_hilb_n_in_degree == 0) flush_pairs();
	    }

	  if (gbTrace >= 1)
	    {
	      buffer o;
	      o << '{' << _this_degree << '}';
	      o << '(';
	      if (_use_hilb) 
		o << _hilb_n_in_degree << ',';
	      o << npairs << ')';
	      emit_wrapped(o.str());
	    }
#if 0
	  if (gbTrace >= 1)
	    {
	      char s[100];
	      sprintf(s, "%sDEGREE %d (npairs %d)\n%s", wrapping_prefix, _this_degree, npairs,wrapping_prefix);
	      emit(s);
	    }
#endif
	}

      s_pair_step();
    }
  //  return is_done;
  set_status(is_done);
  //  showgb();
}


/*******************************
 ** Minimalization of the GB ***
 *******************************/
#if 0
void gbA::poly_auto_reduce_ZZ(vector<POLY,gc_alloc> &mat)
{
  MonomialTableZZ *T = MonomialTableZZ::make(_nvars);
  vector<POLY,gc_alloc> polys;

  for (int i=0; i<mat.size(); i++)
    {
      remainder_by_ZZ(_F,_Fsyz,
		      mat[i],
		      polys, T);
      R->gbvector_get_lead_exponents(_F, mat[i].f, _EXP);
      T->insert(MPZ_VAL(mat[i].f->coeff), _EXP, mat[i].f->comp, i);
      polys.push_back(mat[i]);
    }
}

void gbA::poly_auto_reduce(vector<POLY,gc_alloc> &mat)
{
  if (over_ZZ())
    {
      poly_auto_reduce_ZZ(mat);
      return;
    }
  for (vector<POLY,gc_alloc>::iterator i = mat.begin(); i != mat.end(); i++)
    //    for (vector<POLY,gc_alloc>::iterator j = mat.begin(); j != i; j++)
    for (vector<POLY,gc_alloc>::iterator j = i-1; j >= mat.begin(); j--)
      {
	R->gbvector_auto_reduce(_F,_Fsyz,
				(*i).f, (*i).fsyz,
				(*j).f, (*j).fsyz);
      }
}

struct gbelem_sorter : public binary_function<int,int,bool> {
  GBRing *R;
  const FreeModule *F;
  const vector<gbA::gbelem *,gc_alloc> &gb;
  gbelem_sorter(GBRing *R0,
		const FreeModule *F0,
		const vector<gbA::gbelem *,gc_alloc> &gb0)
    : R(R0), F(F0), gb(gb0) {}
  bool operator()(int xx, int yy) {
    gbvector *x = gb[xx]->g.f;
    gbvector *y = gb[yy]->g.f;
    return R->gbvector_compare(F,x,y) == LT;
  }
};
#endif
void gbA::minimalize_gb()
{
  if (minimal_gb_valid) return;

  vector<POLY,gc_alloc> polys;
  for (vector<gbelem *,gc_alloc>::iterator i = gb.begin(); i != gb.end(); i++)
    {
      //      if ((*i)->minlevel <= ELEM_MIN_GB)
	{
	  polys.push_back((*i)->g);
	}
    }


  minimal_gb->minimalize(polys);


  minimal_gb_valid = true;
#if 0
  // Place into _minimal_gb a sorted minimal GB
  vector<exponents,gc_alloc> exps;
  vector<int,gc_alloc> comps;
  vector<int,gc_alloc> positions;
  exps.reserve(gb.size());
  comps.reserve(gb.size());
  positions.reserve(gb.size());

  for (vector<gbelem *,gc_alloc>::iterator i = gb.begin(); i != gb.end(); i++)
    {
      //      if ((*i)->minlevel <= ELEM_MIN_GB)
	{
	  exponents e = (*i)->lead;
	  exps.push_back(e);
	  int x = (*i)->g.f->comp; // component of this element
	  comps.push_back(x);
	}
    }

  if (over_ZZ())
    {
      vector<mpz_ptr,gc_alloc> coeffs;
      for (vector<gbelem *,gc_alloc>::iterator i = gb.begin(); i != gb.end(); i++)
	coeffs.push_back(MPZ_VAL((*i)->g.f->coeff));

      MonomialTableZZ::find_strong_generators(_nvars,
					      coeffs, 
					      exps,
					      comps,
					      positions);
    }
  else
    MonomialTable::minimalize(_nvars,
			      exps,
			      comps,
			      false,
			      positions);

  if (gbTrace >= 10)
    {
      buffer o;
      for (unsigned int i=0; i<positions.size(); i++)
	{
	  o << i << '\t';
	  R->gbvector_text_out(o, _F, gb[positions[i]]->g.f);
	  o << newline;
	}
      emit_line(o.str());
    }

  // Now sort 'positions'.
  sort(positions.begin(), positions.end(), gbelem_sorter(R,_F,gb));

  for (vector<int,gc_alloc>::iterator i = positions.begin(); i != positions.end(); i++)
    {
      // possibly first copy gb[*i]->g...
      minimal_gb.push_back(gb[*i]->g);
    }

  if (gbTrace >= 10)
    {
      buffer o;
      for (unsigned int i=0; i<minimal_gb.size(); i++)
	{
	  o << i << '\t';
	  R->gbvector_text_out(o, _F, minimal_gb[i].f);
	  o << newline;
	}
      emit(o.str());
    }

  
  poly_auto_reduce(minimal_gb);

  minimal_gb_valid = true;
#endif
}

/*******************************
 ** Hilbert function routines **
 *******************************/

void gbA::flush_pairs()
{
  spair *p;
  while ((p = spair_set_next()) != 0)
    {
      _n_saved_hilb++;
      spair_delete(p);
    }
}

/*************************
 ** Top level interface **
 *************************/

ComputationOrNull *gbA::set_hilbert_function(const RingElement *hf)
{
  // TODO Problems here:
  //  -- check that the ring is correct
  //  -- if the computation has already been started, this will fail
  //     So probably an error should be given, and 0 returned in this case.

  _hf_orig = hf;
  _hf_diff = RingElement::make_raw(hf->get_ring(), ZERO_RINGELEM);
  _use_hilb = true;
  _hilb_new_elems = true;

  _hilb_matrix = const_cast<Matrix *>(Matrix::make(_F, 0, 0, true));
  return this;
}

const MatrixOrNull *gbA::get_gb()
{
  minimalize_gb();
  const vector<POLY,gc_alloc> & mingb = minimal_gb->get();
  MatrixConstructor mat(_F,0,false/*not mutable*/);
  int j=0;
  for (vector<POLY,gc_alloc>::const_iterator i = mingb.begin(); i != mingb.end(); i++)
    {
      fprintf(stderr, "%d ", j++);
      if (j % 20 == 0) fprintf(stderr,"\n");

      vec v = originalR->translate_gbvector_to_vec(_F, (*i).f);
#if 0
      buffer o;
      o << "element " << j++ << " ";
      R->gbvector_text_out(o, F, (*i).f);
      o << "\n  vec   ";
      F->elem_text_out(o, v);
      emit_line(o.str());
#endif
      mat.append(v);
      //      mat.append(originalR->translate_gbvector_to_vec(F, (*i).f));
    }
  return mat.to_matrix();
}

const MatrixOrNull *gbA::get_mingens()
{
  MatrixConstructor mat(_F,0,false/*not mutable*/);
  for (vector<gbelem *,gc_alloc>::iterator i = gb.begin(); i != gb.end(); i++)
    if ((*i)->minlevel <= ELEM_TRIMMED)
      mat.append(originalR->translate_gbvector_to_vec(_F, (*i)->g.f));
  return mat.to_matrix();

}

const MatrixOrNull *gbA::get_change()
{
  minimalize_gb();
  const vector<POLY,gc_alloc> & mingb = minimal_gb->get();
  MatrixConstructor mat(_Fsyz,0,false/*not mutable*/);
  for (vector<POLY,gc_alloc>::const_iterator i = mingb.begin(); i != mingb.end(); i++)
    mat.append(originalR->translate_gbvector_to_vec(_Fsyz, (*i).fsyz));
  return mat.to_matrix();
}

const MatrixOrNull *gbA::get_syzygies()
{
  // The (non-minimal) syzygy matrix
  MatrixConstructor mat(_Fsyz, 0, false /* not mutable */);
  for (vector<gbvector *,gc_alloc>::iterator i = _syz.begin(); i != _syz.end(); i++)
    mat.append(originalR->translate_gbvector_to_vec(_Fsyz, *i));
  return mat.to_matrix();
}

const MatrixOrNull *gbA::get_initial(int nparts)
{
  minimalize_gb();
  const vector<POLY,gc_alloc> & mingb = minimal_gb->get();
  MatrixConstructor mat(_F,0,false/*not mutable*/);
  for (vector<POLY,gc_alloc>::const_iterator i = mingb.begin(); i != mingb.end(); i++)
    {
      gbvector *f = R->gbvector_lead_term(nparts, _F, (*i).f);
      mat.append(originalR->translate_gbvector_to_vec(_F, f));
    }
  return mat.to_matrix();

}

const MatrixOrNull *gbA::matrix_remainder(const Matrix *m)
{
  if (m->get_ring() != originalR)
    {
      ERROR("expected matrix over the same ring");
      return 0;
    }

  if (m->n_rows() != _F->rank()) {
       ERROR("expected matrices to have same number of rows");
       return 0;
  }

#warning "NEEDS WORK!!!"

  MatrixConstructor red(m->rows(), m->cols(), false, m->degree_shift());
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = originalR->translate_gbvector_from_vec(_F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, 
		R->gbvector_degree(m->rows(), g.f), 
		true, denom);

      vec fv = originalR->translate_gbvector_to_vec_denom(_F, g.f, denom);
      // MES: what about g.fsyz??
      red.set_column(i, fv);
    }
  return red.to_matrix();
}

void gbA::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
  if (m->get_ring() != originalR)
    {
      ERROR("expected matrix over the same ring");
      *result_remainder = 0;
      *result_quotient = 0;
      return;
    }
  if (m->n_rows() != _F->rank()) {
       ERROR("expected matrices to have same number of rows");
      *result_remainder = 0;
      *result_quotient = 0;
      return;
  }

  MatrixConstructor mat_remainder(m->rows(), m->cols(), false, m->degree_shift());
  MatrixConstructor mat_quotient(_Fsyz, m->cols(), false);

  const Ring *K = R->get_flattened_coefficients();
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = originalR->translate_gbvector_from_vec(_F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, 
		R->gbvector_degree(m->rows(), g.f), 
		true, denom);

      vec fv = originalR->translate_gbvector_to_vec_denom(_F, g.f, denom);
      K->negate_to(denom);
      vec fsyzv = originalR->translate_gbvector_to_vec_denom(_Fsyz,g.fsyz, denom);
      mat_remainder.set_column(i, fv);
      mat_quotient.set_column(i, fsyzv);
    }
  *result_remainder = mat_remainder.to_matrix();
  *result_quotient = mat_quotient.to_matrix();
}

int gbA::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  if (m->get_ring() != originalR)
    {
      ERROR("expected matrix over the same ring");
      return -2;
    }

  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom, junk;
      POLY g;
      g.f = originalR->translate_gbvector_from_vec(_F,(*m)[i], denom);
      g.fsyz = NULL;
      remainder(g, 
		R->gbvector_degree(m->rows(), g.f),
		false, junk);
      R->gbvector_remove(g.fsyz);
      if (g.f != NULL)
	{
	  R->gbvector_remove(g.f);
	  return i;
	}
    }
  return -1;
}

int gbA::gb_complete_thru_degree()
  // The computation is complete up through this degree.
{
  return _complete_thru_this_degree;
}

void gbA::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
  o << "# pairs computed = " << _n_pairs_computed << newline;
  if (gbTrace >= 5 && gbTrace % 2 == 1)
    for (unsigned int i=0; i<gb.size(); i++)
      {
	o << i << '\t';
	R->gbvector_text_out(o, _F, gb[i]->g.f);
	o << newline;
      }
}

void gbA::debug_spair(spair *p)
{
  buffer o;
  spair_text_out(o, p);
  emit(o.str());
}

void gbA::debug_spairs(spair *spairlist)
{
  spair *p = spairlist;
  while (p != 0)
    {
    debug_spair(p);
    p = p->next;
  }
}

void gbA::debug_spair_array(spairs &spairlist)
{
  for (int i=0; i<spairlist.size(); i++)
    debug_spair(spairlist[i]);
}

void gbA::showgb()
{
  buffer o;
  for (unsigned int i=0; i<gb.size(); i++)
    {
      o << i << '\t';
      R->gbvector_text_out(o, _F, gb[i]->g.f);
      o << newline;
    }
  emit(o.str());
}

#if 0
void gbA::remainder_by(const FreeMoudle *F,
		       const FreeModule *Fsyz,
		       POLY &f,
		       const vector<POLY,gc_alloc> &polys,
		       MonomialTable *T,
		       bool use_denom,
		       ring_elem &denom)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      R->gbvector_get_lead_exponents(_F, h.f, _EXP);
      int x = h.f->comp;
      int nmatches = T->find_divisors(1,_EXP,x,divisors);
        // replaced alpha, g.
      if (nmatches < 0)
	{
	  frem->next = h.f;
	  frem = frem->next;
	  h.f = h.f->next;
	  frem->next = 0;
	}
      else
	{
	  POLY g = polys[0]->g;
	  R->gbvector_reduce_lead_term(_F, _Fsyz,
				       head.next,
				       h.f, h.fsyz,
				       g.f, g.fsyz,
				       use_denom, denom);
	}
    }
  h.f = head.next;
  R->gbvector_remove_content(h.f, h.fsyz, use_denom, denom);
  f.f = h.f;
  f.fsyz = h.fsyz;
}


void gbA::remainder_by_ZZ(const FreeModule *F,
			  const FreeModule *Fsyz,
			  POLY &f,
			  const vector<POLY,gc_alloc> &polys,
			  MonomialTableZZ *T)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      R->gbvector_get_lead_exponents(_F, h.f, _EXP);
      int x = h.f->comp;
      int w = T->find_smallest_coeff_divisor(_EXP,x); // gives smallest coeff
      // of all elements dividing _EXP*x
      if (w >= 0)
	{
	  POLY g = polys[w];
	  if (R->gbvector_reduce_lead_term_ZZ(_F, _Fsyz,
					      h.f, h.fsyz,
					      g.f, g.fsyz))
	    continue;
	}
      frem->next = h.f;
      frem = frem->next;
      h.f = h.f->next;
      frem->next = 0;
    }
  h.f = head.next;
  f.f = h.f;
  f.fsyz = h.fsyz;
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
