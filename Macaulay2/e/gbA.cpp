/* Copyright 2003, Michael E. Stillman */

#include "comp.hpp"
#include "gbA.hpp"
#include "text_io.hpp"
#include <functional>
#include <algorithm>

#include "matrix.hpp"
#include "polyring.hpp"

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
  emit_line("Creating gbA");
  gbA *result = new gbA;
  result->initialize(m, collect_syz, n_rows_to_keep, strategy);
  return result;
}

void gbA::initialize(const Matrix *m, int csyz, int nsyz, int strat)
{
  _strategy = strat;

  initialize0(m, csyz, nsyz);

  _Fsyz = m->cols()->sub_space(_n_rows_per_syz);  

  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *f = R->gbvector_from_vec(_F,(*m)[i], denom);
      spair *p = new_gen(i, f, denom);
      if (p != NULL)
	{
	  spair_set_insert(p);
	  _n_gens_left++;
	}
    }
}

void gbA::initialize0(const Matrix *m, int csyz, int nsyz)
{
  const PolynomialRing *origR = m->get_ring()->cast_to_PolynomialRing();
  if (origR == NULL)
    {
      ERROR("ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  _originalR = origR;
  R = origR->get_gb_ring();

  _first_gb_element = 0; // MES: correct??
  _nvars = R->get_flattened_monoid()->n_vars();
  lookup = MonomialTable::make(_nvars);
  ringtable = MonomialTable::make(_nvars); // MES: grab from R.
  _minimal_gb_valid = true;
  
  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  _n_rows_per_syz = nsyz;

  _F = m->rows();

  _n_syz = 0;
  _n_pairs_computed = 0;
  _n_gens_left = 0;
  _n_subring = 0;

  _collect_syz = csyz;
  _is_ideal = (_F->rank() == 1 && csyz == 0);
  if (R->is_weyl_algebra())
    _is_ideal = false;

#if 0
  _use_hilb = false;
  _hilb_new_elems = false;
  _hilb_n_in_degree = 0;
  _n_saved_hilb = 0;
#endif

  // set local variables for certain time-critical routines

  _this_degree = _F->lowest_primary_degree() - 1;

  _stats_nreductions = 0;
  _stats_ntail = 0;
  _stats_npairs = 0;
  _stats_ngb = 0;
  _stats_ngcd1 = 0;

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
  for (i=-1; i<=nvars; i++)
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

static bool exponents_greater(int nvars, exponents a, exponents b)
{
  for (int i=0; i<nvars; i++)
    {
      if (a[i] < b[i]) return false;
      if (a[i] > b[i]) return true;
    }
  return false;
}

exponents gbA::exponents_make()
{
  int *e = new int[_nvars+1]; // length is nvars+1, add 1 to result.
  return e+1;
}

void gbA::exponents_delete(exponents e)
{
  e--;
  delete [] e;
}

void gbA::lead_exponents(gbvector *f, exponents e)
  // This is only defined for f in _F
  // 
{
  R->gbvector_get_lead_exponents(_F, f, e);
  e[-1] = f->comp;
  // Now maybe set the sugar degree too?
}

/*************************
 * SPair handling ********
 *************************/

gbA::gbelem *gbA::gbelem_make(POLY f, int minlevel, int deg, int me)
{
  gbelem *g = new gbelem;
  g->g = f;
  g->lead = exponents_make();
  lead_exponents(f.f, g->lead);
  g->deg = deg;
  g->alpha = deg - R->exponents_weight(g->lead);
  g->minlevel = minlevel;
  g->me = me;
  return g;
}

gbA::spair *gbA::spair_node()
{
  spair *result = new spair;
  result->next = 0;
  return result;
}

void gbA::spair_delete(spair *&p)
{
  // MES: delete the exponent first?
  delete p;
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
  result->lcm = exponents_make();
    exponents_lcm(_nvars, g1->deg, exp1, exp2, result->lcm, result->deg);
  result->x.pair.i = i;
  result->x.pair.j = j;

  return result;
}

gbA::spair *gbA::spair_make_gen(POLY f)
{
  assert(f.f != 0);
  exponents exp1 = exponents_make();
  lead_exponents(f.f, exp1);
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
  exponents exp2 = exponents_make();
  exp2[-1] = 0;
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
  /* This requires that j indexes into the G array somewhere (negative is fine) */
  spair *result = spair_make(i,j);
  result->type = SPAIR_RING;

  return result;
}

void gbA::spair_text_out(buffer &o, spair *p)
{
  char s[100]; // enough room for all of the non polynomial cases.
  switch (p->type) {
  case SPAIR_SPAIR:
    sprintf(s, "spair(%d,%d)", p->x.pair.i, p->x.pair.j);
    o << s;
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
  p2exp = gb[second]->lead; /* If a ring or skew pair, this should index into GB array */

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
	if ((comp_printlevel & PRINT_SPAIR_TRACKING) != 0)
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
  spair_sorter(int nvars) : nvars(nvars) {}
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
};

void gbA::minimalize_pairs(spairs &new_set)
     /* new_set: array of spair*  */
{
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
      
      bool inideal = montab->find_divisors(1, me->lcm, 1);
      if (!inideal)
	{
	  spairs::iterator t = choose_pair(first, next);
	  spair *p = *t;
	  if (_is_ideal && is_gcd_one_pair(p))
	    {
	      _stats_ngcd1++;
	      if ((comp_printlevel & PRINT_SPAIR_TRACKING) != 0)
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

  delete montab;
  for (spairs::iterator i = new_set.begin(); i != new_set.end(); i++)
    spair_delete(*i);
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
      gbelem *g = gb[i];
      int gx = gbelem_COMPONENT(g);
      if (gx == 0 || gx == x) // MES: this should just be gx == 0??
	{
	  spair *s = spair_make_ring(id,i);
	  new_set.push_back(s);
	}
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
  if (comp_printlevel >= 5)
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
  if (comp_printlevel >= 5)
    {
      buffer o;
      o << "    ";
      R->gbvector_text_out(o, _F, p->f());
      emit_line(o.str());
    }
}

bool gbA::reduce(spair *p)
{
  /* Returns false iff we defer computing this spair. */
  /* If false is returned, this routine has grabbed the spair 'p'. */
  int count = 0;
  compute_s_pair(p); /* Changes the type, possibly */
  if (comp_printlevel == 10)
    {
      buffer o;
      o << "reducing ";
      R->gbvector_text_out(o, _F, p->f());
      emit_line(o.str());
    }
  while (!R->gbvector_is_zero(p->f()))
    {
      int alpha;
      POLY g;
      int v = find_good_divisor(p->x.f,_this_degree, alpha,g); 
      // replaced alpha, g.
      if (v == 0) break;
      count++;
      if (alpha > 0)
	{
	  // MES: this next line needs to be fixed: does it need to be copied?
	  // YES YES!!
	  insert(p->x.f,ELEM_NON_MIN_GB);
	  if ((comp_printlevel % PRINT_SPAIR_TRACKING) != 0)
	    {
	      buffer o;
	      o << "deferring A spair ";
	      spair_text_out(o,p);
	      emit_line(o.str());
	    }
	}
      R->gbvector_reduce_lead_term(_F, _Fsyz,
				   0,
				   p->f(), p->fsyz(), /* modifies these */
				   g.f, g.fsyz);
      _stats_nreductions++;
      if (comp_printlevel == 10)
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
	  if ((comp_printlevel % PRINT_SPAIR_TRACKING) != 0)
	    {
	      buffer o;
	      o << "deferring B spair ";
	      spair_text_out(o,p);
	      emit_line(o.str());
	    }
	  spair_set_insert(p);
	  return false;
	}
    }
  if (comp_printlevel == 3) 
    {
      buffer o;
      o << "." << count;
      emit(o.str());
    }
  return true;
}

void gbA::remainder(POLY &f, int degf)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  int count = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      POLY g;
      int v = find_good_divisor(h,degf,  alpha,g);
        // replaced alpha, g.
      if (v == 0 || alpha > 0)
	{
	  frem->next = h.f;
	  frem = frem->next;
	  h.f = h.f->next;
	  frem->next = 0;
	}
      else
	{
	  R->gbvector_reduce_lead_term(_F, _Fsyz,
				       head.next,
				       h.f, h.fsyz,
				       g.f, g.fsyz);
	  count++;
	  _stats_ntail++;
	  if (comp_printlevel == 10)
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
  R->gbvector_remove_content(h.f, h.fsyz);
  f.f = h.f;
  f.fsyz = h.fsyz;
  if (comp_printlevel == 3)
    {
      buffer o;
      o << "," << count;
      emit(o.str());
    }
}

bool gbA::find_good_divisor(POLY f, int degf, int &result_alpha, POLY &result_g)
     /* Assumption: f.f is non-zero */
{
  int i, alpha, newalpha, ealpha;
  int n = 0;

  vector<MonomialTable::mon_term *> divisors;
  exponents e = exponents_make();
  lead_exponents(f.f, e);
  ealpha = degf - R->exponents_weight(e);
  int x = exponents_COMPONENT(e);

  /* First search for ring divisors */
  n += ringtable->find_divisors(-1, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookup->find_divisors(-1, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    {
      exponents_delete(e);
      return false;
    }
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
  result_g = gb[t->_val] -> g;

  exponents_delete(e);
  return true;
}

/********************
 ** State machine ***
 ********************/

void gbA::auto_reduce_by(int id)
{
  /* Loop backwards while degree doesn't change */
  gbelem *me = gb[id];
  for (int i=id-1; i>=0; i--)
    {
      gbelem *g = gb[i];
      if (g->deg < me->deg) return;
      R->gbvector_auto_reduce(_F, _Fsyz,
			      g->g.f, g->g.fsyz, // these are modified
			      me->g.f, me->g.fsyz);
    }
}

void gbA::insert(POLY f, int minlevel)
{
  /* Reduce this element as far as possible.  This removes content. */
  remainder(f,_this_degree);

  _stats_ngb++;

  gbelem *g = gbelem_make(f, minlevel, _this_degree, gb.size());
  _minimal_gb_valid = false;
  gb.push_back(g);
  
  int me = g->me;
  int x = exponents_COMPONENT(g->lead);
  lookup->insert(g->lead, x, me);

  if (comp_printlevel >= 5)
    {
      char s[100];
      buffer o;
      sprintf(s, "inserting element %d (minimal %d): ",me,minlevel);
      o << s;
      R->gbvector_text_out(o,_F,g->g.f);
      o << "\n";
      R->gbvector_text_out(o,_Fsyz,g->g.fsyz);
      emit_line(o.str());
    }
  if (minlevel <= ELEM_MIN_GB)
    update_pairs(me);

  auto_reduce_by(me);
}

void gbA::collect_syzygy(gbvector *f)
{
  _syz.push_back(f);
  _n_syz++;

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

      if (!R->gbvector_is_zero(f.f))
	{
	  insert(f,minlevel);
	  if (comp_printlevel == 3)
	    emit("m");
	}
      else if (!R->gbvector_is_zero(f.fsyz))
	{
	  /* This is a syzygy */
	  collect_syzygy(f.fsyz);
	  if (comp_printlevel == 3)
	    emit("z");
	}
      else
	{
	  if (comp_printlevel == 3)
	    emit("o");
	}
    }
  return true;
}

int gbA::computation_is_complete()
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

int gbA::compute()
{
  int npairs;
  int is_done = COMP_COMPUTING;

  for (;;)
    {
      system_spincursor();
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
	  npairs = spair_set_prepare_next_degree(_this_degree); // sets _this_degree

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
	  if (comp_printlevel >= 1)
	    {
	      char s[100];
	      sprintf(s, "DEGREE %d (npairs %d)\n", _this_degree, npairs);
	      emit(s);
	    }
	  /*	  if (C->this_degree == 8) return COMP_DONE; */
	}

      s_pair_step();
    }
  return is_done;
}

void gbA::poly_auto_reduce(vector<POLY> &mat)
{
  for (vector<POLY>::iterator i = mat.begin(); i != mat.end(); i++)
    for (vector<POLY>::iterator j = mat.begin(); j != i; j++)
      {
	R->gbvector_auto_reduce(_F,_Fsyz,
				(*i).f, (*i).fsyz,
				(*j).f, (*j).fsyz);
      }
}

struct gbelem_sorter : public binary_function<int,int,bool> {
  GBRing *R;
  const FreeModule *F;
  const vector<gbA::gbelem *> &gb;
  gbelem_sorter(GBRing *R,
		const FreeModule *F,
		const vector<gbA::gbelem *> &gb)
    : R(R), F(F), gb(gb) {}
  bool operator()(int xx, int yy) {
    gbvector *x = gb[xx]->g.f;
    gbvector *y = gb[yy]->g.f;
    return R->gbvector_compare(F,x,y) == LT;
  }
};

#if 0
void gbA::make_ring_gb(vector<gbelem *> &ring_gb, MonomialTable *&ring_table)
{
  // Given that the current GB has rank _F == 1, fill ring_gb (with copied elements).
  // This will be a minimal, reduced GB (in the non-local case), with each component 
  // of each monomial zero, and the ring_table points into this array.

  // We also assume, for the purpose of degree checking, that the gen of _F has degree 0.
  if (_F->rank() != 1) 
    {
      ring_table = make(_nvars);
      return;
    }

  // Now choose a minimal set of elements
  // Copy these
  // Auto-reduce these and sort them
  // Recreate the ring_table.

  // Place into _minimal_gb a sorted minimal GB
  vector<exponents> exps;
  vector<int> comps;
  vector<int> positions;
  exps.reserve(gb.size());
  comps.reserve(gb.size());
  positions.reserve(gb.size());

  for (vector<gbelem *>::iterator i = gb.begin(); i != gb.end(); i++)
    {
      if ((*i)->minlevel <= ELEM_MIN_GB)
	{
	  exps.push_back((*i)->lead);
	  comps.push_back(1);
	}
    }

  MonomialTable::minimalize(_nvars,
			    exps,
			    comps, // not really needed here...
			    false,
			    positions);

  // Now sort 'positions'.
  sort(positions.begin(), positions.end(), gbelem_sorter(R,_F,gb));

  for (vector<int>::iterator i = positions.begin(); i != positions.end(); i++)
    {
      // Make a new gbelem, with a copy of the GB element.
      POLY f;
      f.f = R->gbvector_copy(gb[*i]->g.f);
      f.fsyz = 0;
      int deg = XXX;
      gbelem *g = gbelem_make(f,ELEM_IN_STONE,deg,0);
      
      ring_gb.push_back(g);
    }

  poly_auto_reduce(_ring_gb);
}
#endif

void gbA::make_minimal_gb()
{
  if (_minimal_gb_valid) return;

  // Place into _minimal_gb a sorted minimal GB
  vector<exponents> exps;
  vector<int> comps;
  vector<int> positions;
  exps.reserve(gb.size());
  comps.reserve(gb.size());
  positions.reserve(gb.size());

  for (vector<gbelem *>::iterator i = gb.begin(); i != gb.end(); i++)
    {
      if ((*i)->minlevel <= ELEM_MIN_GB)
	{
	  exps.push_back((*i)->lead);
	  comps.push_back(exponents_COMPONENT((*i)->lead));
	}
    }

  // MES: if we modify _nvars below to only take those variables
  // which are not field vars (via a fraction field), we can obtain
  // a minimal GB in those cases.  Uniqueness is harder to compute though.

  MonomialTable::minimalize(_nvars,
			    exps,
			    comps,
			    false,
			    positions);

  // Now sort 'positions'.
  sort(positions.begin(), positions.end(), gbelem_sorter(R,_F,gb));

  for (vector<int>::iterator i = positions.begin(); i != positions.end(); i++)
    {
      // possibly first copy gb[*i]->g...
      _minimal_gb.push_back(gb[*i]->g);
    }

  poly_auto_reduce(_minimal_gb);

  _minimal_gb_valid = true;
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
#if 0
  _hf_orig = hf;
  _hf_diff = RingElement::make_raw(hf->get_ring(), (Nterm*)0);
  _use_hilb = true;
  _hilb_new_elems = true;
  return this;
#endif
  return 0;
}

const MatrixOrNull *gbA::get_matrix(int level, M2_bool minimize)
{
  // level 1, minimal:  mingens (or trimmed set of gens)
  // level 1, nonminimal: GB matrix
  // level 2, nonminimal: syz matrix (NOT a GB!!)

  // TODO
  if (level > 2 || (level == 2 && minimize))
    {
      ERROR("GB computation: matrix was not computed");
      return 0;
    }
  if (level == 2)
    {
      // The (non-minimal) syzygy matrix
      compute();
      Matrix *result = new Matrix(_Fsyz);
      for (vector<gbvector *>::iterator i = _syz.begin(); i != _syz.end(); i++)
	result->append(R->gbvector_to_vec(_Fsyz, *i));
      return result;
    }
  else if (minimize)
    {
      // return the minimal generators (or as minimal as possible?)
      compute();
      Matrix *result = new Matrix(_F);
      for (vector<gbelem *>::iterator i = gb.begin(); i != gb.end(); i++)
	if ((*i)->minlevel <= ELEM_TRIMMED)
	  result->append(R->gbvector_to_vec(_F, (*i)->g.f));
      return result;
    }
  else
    {
      // The (minimal) Groebner basis itself
      compute();
      make_minimal_gb();
      Matrix *result = new Matrix(_F);
      for (vector<POLY>::iterator i = _minimal_gb.begin(); i != _minimal_gb.end(); i++)
	  result->append(R->gbvector_to_vec(_F, (*i).f));
      return result;
    }
  return 0;
}

const MatrixOrNull *gbA::get_change(int level)
{
  if (level > 1)
    {
      ERROR("matrix not computed");
      return 0;
    }
  compute();
  make_minimal_gb();
  Matrix *result = new Matrix(_Fsyz);
  for (vector<POLY>::iterator i = _minimal_gb.begin(); i != _minimal_gb.end(); i++)
    result->append(R->gbvector_to_vec(_F, (*i).fsyz));
  return result;
}

const MatrixOrNull *gbA::get_leadterms(int nparts, int level)
{
  if (level > 1)
    {
      ERROR("matrix not computed");
      return 0;
    }
  compute();
  make_minimal_gb();
  Matrix *result = new Matrix(_F);
  for (vector<POLY>::iterator i = _minimal_gb.begin(); i != _minimal_gb.end(); i++)
    {
      gbvector *f = R->gbvector_lead_term(nparts, _F, (*i).f);
      result->append(R->gbvector_to_vec(_F, f));
    }
  return result;
}
  
const FreeModuleOrNull *gbA::get_free(int level, M2_bool minimal)
{
  if (level == 0) return _F;
  if (level > 1 || level < 0) 
    {
      ERROR("free module at level %d not computed", level);
      return 0;
    }
  compute();
#if 0
  if (minimal)
    return XXX;
  else
    return YYY;
#endif
}

const MatrixOrNull *gbA::matrix_remainder(int level,
					  const Matrix *m)
{
  if (level > 1)
    {
      ERROR("that Groebner basis not computed");
      return 0;
    }
  if (m->n_rows() != _F->rank()) {
       ERROR("expected matrices to have same number of rows");
       return 0;
  }
  compute();
  Matrix *red = new Matrix(m->rows(), m->cols(), m->degree_shift());
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = R->gbvector_from_vec(_F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, 0); // MES: this degree.  What should it be?

      vec fv = R->gbvector_to_vec_denom(_F, g.f, denom);
      // MES: what about g.fsyz??
      (*red)[i] = fv;
    }
  return red;
}

void gbA::matrix_lift(int level,
		 const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
  if (level > 1)
    {
      ERROR("that Groebner basis not computed");
      *result_remainder = 0;
      *result_quotient = 0;
    }
  if (m->n_rows() != _F->rank()) {
       ERROR("expected matrices to have same number of rows");
      *result_remainder = 0;
      *result_quotient = 0;
  }
  compute();
  *result_remainder = new Matrix(m->rows(), m->cols(), m->degree_shift());
  *result_quotient = new Matrix(_Fsyz, m->cols());
  const Ring *K = R->get_flattened_coefficients();
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = R->gbvector_from_vec(_F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, 0); // MES: this degree.  What should it be?

      vec fv = R->gbvector_to_vec_denom(_F, g.f, denom);
      K->negate_to(denom);
      vec fsyzv = R->gbvector_to_vec_denom(_Fsyz,g.fsyz, denom);
      (**result_remainder)[i] = fv;
      (**result_quotient)[i] = fsyzv;
    }
}

int gbA::contains(int level,
		       const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  if (level > 1)
    {
      ERROR("that Groebner basis not computed");
      return -2;
    }
  // Reduce each column of m one by one.
  compute();
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = R->gbvector_from_vec(_F,(*m)[i], denom);
      g.fsyz = NULL;
      remainder(g,0);
      R->gbvector_remove(g.fsyz);
      if (g.f != NULL)
	{
	  R->gbvector_remove(g.f);
	  return i;
	}
    }
  return -1;
}

int gbA::status(int * complete_up_through_this_degree,
		     int * complete_up_through_this_level)
  /* -1: error condition, and the error message is set.
     0: not made, and in fact it won't ever be done...
     1: not started,
     2: started, 
     3: stopped because of a stopping condition
     4: finished the computation completely
  */
{
  // TODO: what is this really supposed to do
  *complete_up_through_this_level = 1;
  *complete_up_through_this_degree = _this_degree-1;
  return -1; // TODO
}

int gbA::status_level(int level, 
			   M2_bool minimize,
			   int * complete_up_through_this_degree)
  /* Same return values */
{
  // TODO
}

const M2_arrayint gbA::betti(int type)
  /* 0: minimal betti numbers,
     1:
     2:
     3:
  */
{
  // TODO
}
  
void gbA::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     comp_printlevel value */
{
  o << "# pairs computed = " << _n_pairs_computed << newline;
  if (comp_printlevel >= 5 && comp_printlevel % 2 == 1)
    for (int i=0; i<gb.size(); i++)
      {
	o << i << '\t';
	R->gbvector_text_out(o, _F, gb[i]->g.f);
	o << newline;
      }
}


