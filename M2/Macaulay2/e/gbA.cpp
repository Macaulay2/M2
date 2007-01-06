/* Copyright 2003-2006, Michael E. Stillman */

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

#include "gbweight.hpp"
#include "reducedgb.hpp"

#define PrintingDegree 0x0001

/*************************
 * Initialization ********
 *************************/

exponents gbA::exponents_make()
{
  exponents result = reinterpret_cast<exponents>(lcm_stash->new_elem());
  return result;
}

gbA * gbA::create(const Matrix *m,
		  M2_bool collect_syz,
		  int n_rows_to_keep,
		  M2_arrayint gb_weights,
		  int strategy,
		  M2_bool use_max_degree_limit,
		  int max_degree_limit)
{
  gbA *result = new gbA;
  result->initialize(m, collect_syz, n_rows_to_keep, gb_weights, strategy);
  intern_GB(result);
  return result;
}

void gbA::initialize(const Matrix *m, int csyz, int nsyz, M2_arrayint gb_weights0, int strat)
{
  max_reduction_count = 10; 
     // 1 is best possible for 3-anderbuch!
     // 5 is: (114.64 sec, 494 MB)
     // 10 is best so far (125.33 sec, 527 MB virtual).  
     // 50 is faster/smaller than 100, and 1000 was awful, on 3-andersbuch
  const PolynomialRing *origR = m->get_ring()->cast_to_PolynomialRing();
  if (origR == NULL)
    {
      ERROR("ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  originalR = origR;
  R = origR->get_gb_ring();
  weightInfo_ = new GBWeight(m->rows(), gb_weights0);
  gb_weights = weightInfo_->get_weights();

  _nvars = R->get_flattened_monoid()->n_vars();
  _coeff_type = origR->coefficient_type();
  n_fraction_vars = origR->n_fraction_vars();

  spair_stash = new stash("gbA spairs", sizeof(spair));
  gbelem_stash = new stash("gbA elems", sizeof(gbelem));
  lcm_stash = new stash("gbA lead monoms", sizeof(int) * (R->n_vars()+2));

  if (nsyz < 0 || nsyz > m->n_cols())
    nsyz = m->n_cols();
  n_rows_per_syz = nsyz;

  _F = m->rows();
  _Fsyz = m->cols()->sub_space(n_rows_per_syz);  

  S = new SPairSet;
  first_in_degree = 0;
  n_syz = 0;
  n_pairs_computed = 0;
  n_gens_left = 0;
  n_subring = 0;

  _strategy = strat;
  _collect_syz = csyz;
  _is_ideal = (_F->rank() == 1 && csyz == 0);
  if (R->is_weyl_algebra())
    _is_ideal = false;

  use_hilb = false;
  hilb_new_elems = false;
  hilb_n_in_degree = 0;
  n_saved_hilb = 0;
  hf_orig = 0;
  hf_diff = 0;

  this_degree = _F->lowest_primary_degree() - 1;
  npairs = 0;
  complete_thru_this_degree = this_degree;
  set_status(COMP_NOT_STARTED);

  stats_nreductions = 0;
  stats_ntail = 0;
  stats_npairs = 0;
  stats_ngb = 0;
  stats_ngcd1 = 0;

  divisor_previous = -1;
  divisor_previous_comp = -1;

  // ZZZZ split
  lookup = 0;
  lookupZZ = 0;
  if (over_ZZ())
    lookupZZ = MonomialTableZZ::make(R->n_vars());
  else
    {
      lookup = MonomialTable::make(R->n_vars());
    }

  minimal_gb = ReducedGB::create(originalR,_F,_Fsyz);
  minimal_gb_valid = true;
  EXP_ = exponents_make();

  if (originalR->is_quotient_ring())
    {
      ringtable = originalR->get_quotient_MonomialTable();
      ringtableZZ = originalR->get_quotient_MonomialTableZZ();

      first_gb_element = originalR->n_quotients();
      for (int i=0; i<first_gb_element; i++)
	{
	  gbvector *f = const_cast<gbvector *>(originalR->quotient_gbvector(i));
	  gbelem *g = gbelem_ring_make(f);
	  gb.push_back(g);
	}
    }
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(_F,(*m)[i], denom);
      spair *p = new_gen(i, f, denom);
      if (p != NULL)
	{
	  spair_set_insert(p);
	  n_gens_left++;
	}
    }

  state = STATE_NEWDEGREE; // will be changed if hilb fcn is used
  np_i = first_gb_element;
  ar_i = first_gb_element;
  ar_j = ar_i+1;
  n_gb = first_gb_element;
}

gbA::spair *gbA::new_gen(int i, gbvector *f, ring_elem denom)
{
  gbvector *fsyz;

  if (i < n_rows_per_syz)
    fsyz = R->gbvector_term(_Fsyz,denom,i+1);
  else
    fsyz = R->gbvector_zero();

  if (R->gbvector_is_zero(f))
    {
      originalR->get_quotient_info()->gbvector_normal_form(_Fsyz, fsyz);
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
void gbA::remove_gb()
{
  // removes all allocated objects
  for (int i=first_gb_element; i<gb.size(); i++)
    {
      R->gbvector_remove(gb[i]->g.f);
      R->gbvector_remove(gb[i]->g.fsyz);
    }
  for (int i=0; i<gb.size(); i++)
    {
      lcm_stash->delete_elem(gb[i]->lead);
      gbelem_stash->delete_elem(gb[i]);
      gb[i] = 0;
    }
  delete minimal_gb; // will free its own gbvector's.
  for (int i=0; i<_syz.size(); i++)
    {
      R->gbvector_remove(_syz[i]);
      _syz[i] = 0;
    }
  delete lookup;
  delete lookupZZ;
  delete spair_stash;
  delete gbelem_stash;
  delete lcm_stash;
  // Also remove the SPAirSet...
}

gbA::~gbA()
{
  remove_gb();
}

/*************************
 * Exponent handling *****
 *************************/

static void exponents_lcm(int nvars, 
			  int dega, 
			  exponents a, 
			  exponents b, 
			  exponents result, 
			  M2_arrayint weights,
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
	  deg += diff * weights->array[i];
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

static bool exponents_divide(int nvars, exponents a, exponents b)
{
  for (int i=0; i<nvars; i++)
    if (a[i] > b[i]) return false;
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

gbA::gbelem *gbA::gbelem_ring_make(gbvector *f)
{
  int f_leadweight;
  gbelem *g = reinterpret_cast<gbelem *>(gbelem_stash->new_elem());
  g->g.f = f;
  g->g.fsyz = 0;
  g->lead = exponents_make();
  R->gbvector_get_lead_exponents(_F, f, g->lead);
  g->deg = weightInfo_->gbvector_weight(f, f_leadweight);
  g->alpha = g->deg - f_leadweight;
  g->size = R->gbvector_n_terms(f);
  g->minlevel = ELEM_IN_RING;
  return g;
}


gbA::gbelem *gbA::gbelem_make(gbvector *f,  // grabs f
			      gbvector *fsyz, // grabs fsyz
			      gbelem_type minlevel,
			      int deg)
{
  int f_wt, f_leadweight;
  gbelem *g = reinterpret_cast<gbelem *>(gbelem_stash->new_elem());
  g->g.f = f;
  g->g.fsyz = fsyz;
  g->lead = exponents_make();
  R->gbvector_get_lead_exponents(_F, f, g->lead);
  g->deg = deg;
  f_wt = weightInfo_->gbvector_weight(f, f_leadweight);
  //g->alpha = f_wt - f_leadweight; // DOESN"T PRODUCE CORRECT DEGREES
  g->alpha = deg - weightInfo_->gbvector_term_weight(f);
  g->size = R->gbvector_n_terms(f);
  g->minlevel = minlevel;
  return g;
}

/*************************
 * SPair handling ********
 *************************/

gbA::spair *gbA::spair_node()
{
  spair *result = reinterpret_cast<spair *>(spair_stash->new_elem());
  result->next = 0;
  return result;
}

void gbA::spair_delete(spair *&p)
{
  // MES: delete the exponent first?
  if (p == 0) return;
  if (p->type == SPAIR_GEN || p->type == SPAIR_ELEM)
    {
      R->gbvector_remove(p->x.f.f);
      R->gbvector_remove(p->x.f.fsyz);
    }
  lcm_stash->delete_elem(p->lcm);
  spair_stash->delete_elem(p);
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
    exponents_lcm(_nvars, g1->deg, exp1, exp2, result->lcm, gb_weights, result->deg);
    if (g2->alpha > g1->alpha)
      result->deg += g2->alpha - g1->alpha;
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
  exponents exp1 = exponents_make();
  R->gbvector_get_lead_exponents(_F, f.f, exp1);
  int deg = weightInfo_->gbvector_weight(f.f);
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
  int vvar = R->skew_variable(v);
  for (j=0; j<_nvars; j++)
    exp2[j] = 0;
  exp2[vvar] = 2;
  result = spair_node();
  result->next = 0;
  result->type = SPAIR_SKEW;
  result->lcm = exp2;
    exponents_lcm(_nvars, g1->deg, exp1, exp2, exp2, gb_weights, result->deg);
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
  if (over_ZZ()) return; 
  spair head;
  spair *p = &head;
  gbelem *m = gb[id];

  head.next = S->heap;
  while (p->next != 0)
    if (pair_not_needed(p->next, m))
      {
	spair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
	if (gbTrace >= 10)
	  {
	    buffer o;
	    o << "removing unneeded ";
	    spair_text_out(o, tmp);
	    emit_line(o.str());
	  }
	spair_delete(tmp);
	S->nelems--;
      }
  else
    p = p->next;
  S->heap = head.next;
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

struct spair_sorter : public std::binary_function<gbA::spair *,gbA::spair *,bool> {
  int nvars;
  spair_sorter(int nv) : nvars(nv) {}
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
//   spairs keep_for_now;
//   emit("--minimalize pairs--\n");
//   for (int i=0; i<new_set.size(); i++) {
//     keep_for_now.push_back(new_set[i]);
//     debug_spair(new_set[i]);
//   }
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
	      stats_ngcd1++;
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

  delete montab;
  for (spairs::iterator i = new_set.begin(); i != new_set.end(); i++)
    spair_delete(*i);
}

void gbA::minimalize_pairs_ZZ(spairs &new_set)
{
  // Prune down the set of spairs to a 'minimal' set.  For each one, we 
  // need to add in a "gcd" combination spair as well.

  VECTOR(mpz_ptr) coeffs;
  VECTOR(mpz_ptr) coeffs2;
  VECTOR(exponents) exps;
  VECTOR(int) comps;
  VECTOR(int) positions;
  
  coeffs.reserve(gb.size());
  coeffs2.reserve(gb.size());
  exps.reserve(gb.size());
  comps.reserve(gb.size());

  for (VECTOR(spair *)::iterator i = new_set.begin(); i != new_set.end(); i++)
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

  for (VECTOR(int)::iterator i = positions.begin(); i != positions.end(); i++)
    {
      // Insert this spair, and also the corresponding gcd one.
      spair *p = new_set[*i];
      if (gbTrace >= 4)
	{
	  buffer o;
	  spair_text_out(o, p);
	  emit_line(o.str());
	}
      spair_set_insert(p);
      mpz_ptr u = coeffs[*i];
      mpz_ptr v = coeffs2[*i];
      if (p->type != SPAIR_SKEW && mpz_cmpabs_ui(u,1) && mpz_cmpabs_ui(v,1))
	{
	  spair *p2 = spair_make_gcd_ZZ(p->x.pair.i, p->x.pair.j);
	  if (gbTrace >= 4)
	    {
	      buffer o;
	      spair_text_out(o, p2);
	      emit_line(o.str());
	    }
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
  for (int i=0; i<first_gb_element; i++)
    {
      spair *s = spair_make_ring(id,i);
      new_set.push_back(s);
    }
  /* Step 2c. pairs from the vectors themselves */
  /* Loop through the minimal GB elements and form the s-pair */
  for (int i=first_gb_element; i<id; i++)
    {
      gbelem *g = gb[i];
      if (g->minlevel != ELEM_NON_MIN_GB && gbelem_COMPONENT(g) == x)
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
    n_computed(0),
    spair_list(0),
    spair_last_deferred(0),
    gen_list(0),
    gen_last_deferred(0)
{
}

void gbA::remove_spair_list(spair *&set)
{
  while (!set)
    {
      spair *tmp = set;
      set = set->next;
      spair_delete(tmp);
    }
  set = 0;
}

void gbA::remove_SPairSet()
{
  remove_spair_list(S->heap);
  remove_spair_list(S->spair_list);
  remove_spair_list(S->spair_deferred_list.next);
  remove_spair_list(S->gen_list);
  remove_spair_list(S->gen_deferred_list.next);
  S->spair_last_deferred = 0;
  S->gen_last_deferred = 0;
}

void gbA::spair_set_insert(gbA::spair *p)
  /* Insert a LIST of s pairs into S */
{
  while (p != 0)
    {
      spair *tmp = p;
      p = p->next;
      S->nelems++;
      tmp->next = S->heap;
      S->heap = tmp;
    }
}

gbA::spair *gbA::spair_set_next()
  /* Removes the next element of the current degree, returning NULL if none left */
{
  spair *result = S->spair_list;
  if (result)
    {
      S->spair_list = result->next;
    }
  else
    {
      if (S->spair_deferred_list.next != 0)
	{
	  if (gbTrace >= 4)
	    {
	      emit_wrapped(" deferred pairs: ");
	    }
	  S->spair_list = S->spair_deferred_list.next;
	  S->spair_deferred_list.next = 0;
	  S->spair_last_deferred = &S->spair_deferred_list;
	  result = S->spair_list;
	  S->spair_list = result->next;
	}
      else
	{
	  // Now do the same for generators
	  result = S->gen_list;
	  if (result)
	    {
	      S->gen_list = result->next;
	    }
	  else
	    {
	      if (S->gen_deferred_list.next != 0)
		{
		  if (gbTrace >= 4)
		    {
		      emit_wrapped(" deferred gen pairs: ");
		    }
		  S->gen_list = S->gen_deferred_list.next;
		  S->gen_deferred_list.next = 0;
		  S->gen_last_deferred = &S->gen_deferred_list;
		  result = S->gen_list;
		  S->gen_list = result->next;
		}
	      else
		return 0;
	    }
	}
    }

  result->next = 0;
  S->nelems--;
  S->n_in_degree--;
  S->n_computed++;
  return result;
}

void gbA::spair_set_defer(spair *&p)
  // Defer the spair p until later in this same degree
  // The spair should have been reduced a number of times
  // already, so its type should be SPAIR_GEN or SPAIR_ELEM
{
  if (gbTrace >= 4) emit_wrapped("D");
  //  spair_delete(p); // ONLY FOR TESTING!! THIS IS INCORRECT!!
  //  return;
  S->n_in_degree++;
  if (p->type == SPAIR_GEN)
    {
      S->gen_last_deferred->next = p;
      S->gen_last_deferred = p;
    }
  else
    {
      S->spair_last_deferred->next = p;
      S->spair_last_deferred = p;
    }
}

int gbA::spair_set_determine_next_degree(int &nextdegree)
{
  spair *p;
  int nextdeg;
  int len = 1;
  if (S->heap == 0) return 0;
  nextdeg = S->heap->deg;
  for (p = S->heap->next; p!=0; p=p->next)
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
  S->spair_list = 0;
  S->spair_deferred_list.next = 0;
  S->spair_last_deferred = &S->spair_deferred_list;

  S->gen_list = 0;
  S->gen_deferred_list.next = 0;
  S->gen_last_deferred = &S->gen_deferred_list;
  
  int len = spair_set_determine_next_degree(nextdegree);
  if (len == 0) return 0;

  spair head;
  spair *p;
  head.next = S->heap;
  p = &head;
  while (p->next != 0)
    if (p->next->deg != nextdegree)
      p = p->next;
    else
      {
	spair *tmp = p->next;
	p->next = tmp->next;
	if (tmp->type == SPAIR_GEN)
	  {
	    tmp->next = S->gen_list;
	    S->gen_list = tmp;
	  }
	else
	  {
	    // All other types are on the spair list
	    tmp->next = S->spair_list;
	    S->spair_list = tmp;
	  }
      }
  S->heap = head.next;
  S->n_in_degree = len;

  /* Now sort 'spair_list' and 'gen_list'. */
  spairs_sort(len, S->spair_list);
  spairs_sort(len, S->gen_list);
  //  G->spairs_reverse(S->spair_list);
  //  G->spairs_reverse(S->gen_list);
  return len;
}

void gbA::spair_set_show_mem_usage()
{
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
  if (ps == 0 || ps->next == 0) return;
  if (len <= 1) return;
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
      const int *mon = R->skew_monomial_var(p->x.pair.j);
      R->gbvector_mult_by_term(_F,_Fsyz,
			       R->one(), mon,
			       f.f, f.fsyz,
			       p->f(), p->fsyz());
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
  int tmf, wt;
  int count = -1;
  compute_s_pair(p); /* Changes the type, possibly */
  if (gbTrace >= 10)
    {
      buffer o;
      o << "reducing ";
      R->gbvector_text_out(o, _F, p->f());
      emit_line(o.str());
    }
  while (!R->gbvector_is_zero(p->f()))
    {
      if (count++ > max_reduction_count)
      	{
      	  spair_set_defer(p);
      	  return false;
      	}
      if (gbTrace >= 5)
	{
	  if ((wt = weightInfo_->gbvector_weight(p->f(), tmf)) > this_degree)
	    {
	      buffer o;
	      o << "ERROR: degree of polynomial is too high: deg " <<  wt 
		<< " termwt " << tmf 
		<< " expectedeg " << this_degree
		<< newline;
	      emit(o.str());
	    }
	}

      int alpha,w;
      R->gbvector_get_lead_exponents(_F, p->f(), EXP_);
      int x = p->f()->comp;
      if (over_ZZ())
	{
	  mpz_ptr c = MPZ_VAL(p->f()->coeff);
	  w = find_good_term_divisor_ZZ(c,EXP_,x,this_degree,alpha);

	  // If w < 0, then no divisor was found.  Is there a GB element of
	  // the same degree as this one, and with the same exponent vector?
	  // If so, use gcdextended to find (g,u,v), 
	  if (w < 0 || alpha > 0)
	  {
	    MonomialTableZZ::mon_term *t = lookupZZ->find_exact_monomial(EXP_,
									 x,
									 first_in_degree);
	    if (t != 0)
	      {
		// f <-- u*p+v*f (same with syz versions), need to change lookupZZ too?
		// p <-- c*p-d*f
#ifdef DEVELOPMENT
#warning "quotient ring handling, lookupZZ needs to change too?"
#endif
		gbelem *g = gb[t->_val];
		if (gbTrace >= 10)
		  {
		    buffer o;
		    o << "swapping GB element\n    ";
		    R->gbvector_text_out(o, _F, p->f());
		    emit_line(o.str()); o.reset();
		    o << "    and ";
		    R->gbvector_text_out(o, _F, g->g.f);
		    emit_line(o.str()); o.reset();
		    o << "  giving";
		    emit_line(o.str());
		  }
		R->gbvector_replace_2by2_ZZ(_F, _Fsyz, p->f(), p->fsyz(), g->g.f, g->g.fsyz);
		// Before continuing, do remainder of g->g
		tail_remainder_ZZ(g->g, this_degree);
		lookupZZ->change_coefficient(t, MPZ_VAL(g->g.f->coeff));
		auto_reduce_by(t->_val);
		if (gbTrace >= 10)
		  {
		    buffer o;
		    R->gbvector_text_out(o, _F, p->f());
		    emit_line(o.str()); o.reset();
		    o << "    and ";
		    R->gbvector_text_out(o, _F, g->g.f);
		    emit_line(o.str());
		  }
		continue;
	      }
	  }
	}
      else
	{
	  w = find_good_divisor(EXP_,x,this_degree, alpha); 
	}

	
      // replaced alpha, g.
      if (w < 0) break;
      if (alpha > 0)
	{
	  POLY h;
	  h.f = R->gbvector_copy(p->x.f.f);
	  h.fsyz = R->gbvector_copy(p->x.f.fsyz);
	  new_insert(h,ELEM_NON_MIN_GB);
	  if (gbTrace >= 10)
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

      stats_nreductions++;
      if (gbTrace >= 10)
	{
	  buffer o;
	  o << "  reducing by ";
	  R->gbvector_text_out(o, _F, g.f);
	  emit_line(o.str()); o.reset();
	  o << "    giving ";
	  R->gbvector_text_out(o, _F, p->f());
	  emit_line(o.str());
	}
      if (R->gbvector_is_zero(p->f())) break;
      if (alpha > 0)
	{
	  p->deg += alpha;
	  if (gbTrace >= 10)
	    {
	      buffer o;
	      o << "deferring B spair old deg " << p->deg-alpha << " new deg " << p->deg << " ";
	      spair_text_out(o,p);
	      emit_line(o.str());
	    }
	  spair_set_insert(p);
	  return false;
	}
    }
  if (gbTrace >= 4) 
    {
      buffer o;
      o << "." << count;
      emit_wrapped(o.str());
    }
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

  VECTOR(MonomialTableZZ::mon_term *) divisors;
  ealpha = degf - weightInfo_->exponents_weight(e,x);


  /* First search for ring divisors */
  if (ringtableZZ)
    n += ringtableZZ->find_monomial_divisors(-1, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookupZZ->find_monomial_divisors(-1, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    return -1;
  int result = divisors[0]->_val;
  gbelem *tg = gb[result];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=1; i<n; i++)
      {
	int new_val = divisors[i]->_val;
	tg = gb[new_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  result = new_val;
	  break;
	} else if (newalpha < alpha) 
	  {
	    result = new_val;
	    alpha = newalpha;
	  }
      }
  result_alpha = alpha;
  return result;
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

  VECTOR(MonomialTableZZ::mon_term *) divisors;
  ealpha = degf - weightInfo_->exponents_weight(e,x);

  /* First search for ring divisors */
  if (ringtableZZ)
    n += ringtableZZ->find_term_divisors(-1, c, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookupZZ->find_term_divisors(-1, c, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    {
      result_alpha = 0;
      return -1;
    }
  int result = divisors[n-1]->_val;
  gbelem *tg = gb[result];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=n-2; i>=0; i--)
      {
	int new_val = divisors[i]->_val;
	tg = gb[new_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  result = new_val;
	  break;
	} else if (newalpha < alpha) 
	  {
	    result = new_val;
	    alpha = newalpha;
	  }
      }
  result_alpha = alpha;
  return result;
}

#if 0
// int gbA::find_good_divisor(exponents e,
// 			   int x,
// 			   int degf, 
// 			   int &result_alpha)
//   // Returns an integer w.
//   // if w >=0: gb[w]'s lead term divides [e,x].
//   // if w<0: no gb[w] has lead term dividing [e,x].
// {
//   int alpha, newalpha, ealpha;
//   int n = 0;
// 
//   VECTOR(MonomialTable::mon_term *) divisors;
//   ealpha = degf - weightInfo_->exponents_weight(e,x);
// 
// #ifdef DEVELOPMENT
// #warning "previous divisor code might not work with alpha..."
// #endif
//   if (divisor_previous >= 0 && x == divisor_previous_comp)
//     {
//       gbelem *tg = gb[divisor_previous];
//       alpha = tg->alpha - ealpha;
//       if (alpha <= 0 && exponents_divide(_nvars, tg->lead, e))
// 	{
// 	  result_alpha = 0;
// 	  return divisor_previous;
// 	}
//     }
//   /* First search for ring divisors */
//   if (ringtable)
//     n += ringtable->find_divisors(-1, e, 1, &divisors);
// 
//   /* Next search for GB divisors */
//   n += lookup->find_divisors(-1, e, x, &divisors);
// 
//   if (gbTrace >= 4 && n >= 2)
//     {
//       gbelem *tg = gb[divisors[n-1]->_val];
//       gbvector *f = tg->g.f;
//       int sz = tg->size;
//       if (sz >= 3)
// 	{
// 	  buffer o;
// 	  o << "\nndivisors " << n;
// 	  o << "\n  choices:";
// 	  for (int j=0; j<n; j++)
// 	    {
// 	      f = gb[divisors[j]->_val]->g.f;
// 	      o << "\n    size " << R->gbvector_n_terms(f);
// 	      o << " lead ";
// 	      f = R->gbvector_lead_term(-1,_F,f);
// 	      R->gbvector_text_out(o,_F,f);
// 	    }
// 	  o << "\n";
// 	  emit_wrapped(o.str());
// 	}
//     }
//   /* Now find the minimal alpha value */
//   if (n == 0) 
//     {
//       result_alpha = 0;
//       return -1;
//     }
//   int result = divisors[n-1]->_val;
//   gbelem *tg = gb[result];
//   alpha = tg->alpha - ealpha;
//   if (alpha <= 0) 
//     {
//       alpha = 0;
//       int minsz = R->gbvector_n_terms(tg->g.f);
//       for (int i=n-2; i>=0; i--)
// 	{
// 	  int new_val = divisors[i]->_val;
// 	  tg = gb[new_val];
// 	  int sz = R->gbvector_n_terms(tg->g.f);
// 	  if (sz < minsz)
// 	    {
// 	      if (tg->alpha <= ealpha)
// 		{
// 		  minsz = sz;
// 		  result = new_val;
// 		}
// 	    }
// 	}
//     }
//   else
//     //    for (i=1; i<n; i++)
//     for (int i=n-2; i>=0; i--)
//       {
// 	int new_val = divisors[i]->_val;
// 	tg = gb[new_val];
// 
// 
// 	newalpha = tg->alpha - ealpha;
// 	if (newalpha <= 0) {
// 	  alpha = 0;
// 	  result = new_val;
// 	  break;
// 	} else if (newalpha < alpha) 
// 	  {
// 	    result = new_val;
// 	    alpha = newalpha;
// 	  }
//       }
//   divisor_previous = result;
//   divisor_previous_comp = x;
//   result_alpha = alpha;
//   return result;
// }
#endif

int gbA::find_good_divisor(exponents e,
			   int x,
			   int degf, 
			   int &result_alpha)
  // Returns an integer w.
  // if w >=0: gb[w]'s lead term divides [e,x].
  // if w<0: no gb[w] has lead term dividing [e,x].
{
  int alpha, newalpha, ealpha;
  int n = 0;

  VECTOR(MonomialTable::mon_term *) divisors;
  ealpha = degf - weightInfo_->exponents_weight(e,x);

#ifdef DEVELOPMENT
#warning "previous divisor code might not work with alpha..."
#endif
  if (divisor_previous >= 0 && x == divisor_previous_comp)
    {
      gbelem *tg = gb[divisor_previous];
      alpha = tg->alpha - ealpha;
      if (alpha <= 0 && exponents_divide(_nvars, tg->lead, e))
	{
	  result_alpha = 0;
	  return divisor_previous;
	}
    }
  /* First search for ring divisors */
  if (ringtable)
    n += ringtable->find_divisors(-1, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookup->find_divisors(-1, e, x, &divisors);

  if (gbTrace >= 4 && n >= 2)
    {
      gbelem *tg = gb[divisors[n-1]->_val];
      gbvector *f = tg->g.f;
      int sz = tg->size;
      if (sz >= 3)
	{
	  buffer o;
	  o << "\nndivisors " << n;
	  o << "\n  choices:";
	  for (int j=0; j<n; j++)
	    {
	      f = gb[divisors[j]->_val]->g.f;
	      o << "\n    size " << R->gbvector_n_terms(f);
	      o << " lead ";
	      f = R->gbvector_lead_term(-1,_F,f);
	      R->gbvector_text_out(o,_F,f);
	    }
	  o << "\n";
	  emit_wrapped(o.str());
	}
    }
  /* Now find the minimal alpha value */
  if (n == 0) 
    {
      result_alpha = 0;
      return -1;
    }
  int result = divisors[n-1]->_val;
  gbelem *tg = gb[result];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    {
      alpha = 0;
      int minsz = tg->size;
      for (int i=n-2; i>=0; i--)
	{
	  int new_val = divisors[i]->_val;
	  tg = gb[new_val];
	  int sz = tg->size;
	  if (sz < minsz)
	    {
	      if (tg->alpha <= ealpha)
		{
		  minsz = sz;
		  result = new_val;
		}
	    }
	}
    }
  else
    //    for (i=1; i<n; i++)
    for (int i=n-2; i>=0; i--)
      {
	int new_val = divisors[i]->_val;
	tg = gb[new_val];


	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  result = new_val;
	  break;
	} else if (newalpha < alpha) 
	  {
	    result = new_val;
	    alpha = newalpha;
	  }
      }
  divisor_previous = result;
  divisor_previous_comp = x;
  result_alpha = alpha;
  return result;
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
      R->gbvector_get_lead_exponents(_F, h.f, EXP_);
      int x = h.f->comp;
      int w = find_good_monomial_divisor_ZZ(MPZ_VAL(h.f->coeff),EXP_,x,degf,  alpha);
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
	  //	  stats_ntail++;
	  if (gbTrace >= 10)
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
  if (h.f != 0 && mpz_sgn(MPZ_VAL(h.f->coeff)) < 0)
    {
      R->gbvector_mult_by_coeff_to(h.f, globalZZ->minus_one());
      R->gbvector_mult_by_coeff_to(h.fsyz, globalZZ->minus_one());
    }
  f.f = h.f;
  f.fsyz = h.fsyz;
  if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
    {
      buffer o;
      o << "number of reduction steps was " << count;
      emit_line(o.str());
    }
  else if (gbTrace >= 4)
    {
      buffer o;
      o << "," << count;
      emit_wrapped(o.str());
    }
}

void gbA::tail_remainder_ZZ(POLY &f, int degf)
{
  gbvector head;
  gbvector *frem = &head;
  int count = 0;
  POLY h = f;

  frem->next = h.f;
  frem = frem->next;
  h.f = h.f->next;
  frem->next = 0;

  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      R->gbvector_get_lead_exponents(_F, h.f, EXP_);
      int x = h.f->comp;
      int w = find_good_monomial_divisor_ZZ(MPZ_VAL(h.f->coeff),EXP_,x,degf,  alpha);
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
	  //	  stats_ntail++;
	  if (gbTrace >= 10)
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
  if (h.f != 0 && mpz_sgn(MPZ_VAL(h.f->coeff)) < 0)
    {
      R->gbvector_mult_by_coeff_to(h.f, globalZZ->minus_one());
      R->gbvector_mult_by_coeff_to(h.fsyz, globalZZ->minus_one());
    }
  f.f = h.f;
  f.fsyz = h.fsyz;
  if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
    {
      buffer o;
      o << "number of reduction steps was " << count;
      emit_line(o.str());
    }
  else if (gbTrace >= 4)
    {
      buffer o;
      o << "," << count;
      emit_wrapped(o.str());
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
      R->gbvector_get_lead_exponents(_F, h.f, EXP_);
      int x = h.f->comp;
      int w = find_good_divisor(EXP_,x,degf,  alpha);
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
	  //	  stats_ntail++;
	  if (gbTrace >= 10)
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
  if ((gbTrace & PRINT_SPAIR_TRACKING) != 0)
    {
      buffer o;
      o << "number of reduction steps was " << count;
      emit_line(o.str());
    }
  else if (gbTrace >= 4)
    {
      buffer o;
      o << "," << count;
      emit_wrapped(o.str());
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
  int a = me->alpha;  // Only auto reduce those that are of the same degree
                      // and not a higher alpha level
  for (int i=gb.size()-1; i>=first_gb_element; i--)
    {
      if (i == id) continue;
      gbelem *g = gb[i];
      if (g->deg < me->deg) return;
      if (g->alpha < a) continue;
      if (gbTrace >= 10)
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

void gbA::insert(POLY f, gbelem_type minlevel)
{
  /* Reduce this element as far as possible.  This either removes content, 
     makes it monic, or at least negates it so the lead coeff is positive. */
  ring_elem junk;

  //DEBUG BLOCK  int fwt;
  //  int fdeg = weightInfo_->gbvector_weight(f.f, fwt);
  //  fprintf(stderr, "inserting GB element %d, thisdeg %d deg %d alpha %d\n", 
  //	  gb.size(), 
  //	  this_degree,
  //	  fdeg,
  //	  fdeg-fwt);

  remainder(f,this_degree,false,junk);

  //  fdeg = weightInfo_->gbvector_weight(f.f, fwt);
  //  fprintf(stderr, "    after remainder deg %d alpha %d\n", 
  //	  fdeg,
  //	  fdeg-fwt);

  stats_ngb++;

  gbelem *g = gbelem_make(f.f, f.fsyz, minlevel, this_degree);
  minimal_gb_valid = false;
  int me = gb.size();
  gb.push_back(g);

  // In a encoded Schreyer order, the following line might miss subring elements.
  // But it at least won't be incorrect...
  if (R->get_flattened_monoid()->in_subring(1,f.f->monom))
    n_subring++;
  
  int x = g->g.f->comp;

  if (over_ZZ())
    lookupZZ->insert(MPZ_VAL(g->g.f->coeff),g->lead,x,me);
  else
    lookup->insert(g->lead, x, me);

  if (gbTrace >= 5)
    {
      char s[100];
      buffer o;
      sprintf(s, "inserting element %d (minimal %d): ",me,minlevel);
      o << s;
      R->gbvector_text_out(o,_F,g->g.f);
      emit_line(o.str()); o.reset();
      o << "                          syzygy : ";
      R->gbvector_text_out(o,_Fsyz,g->g.fsyz);
      emit_line(o.str());
    }
  if (minlevel != ELEM_NON_MIN_GB)
    update_pairs(me);

  auto_reduce_by(me);

  //  for (int i=0; i<gb.size(); i++)
  //    {
  //      fdeg = weightInfo_->gbvector_weight(gb[i]->g.f, fwt);
  //      fprintf(stderr, "    after auto reduce gb %d deg %d actualdeg %d alpha %d\n", 
  //	      i, 
  //	      gb[i]->deg,
  //	      fdeg,
  //	      fdeg-fwt);
  //    }

  if (use_hilb)
    {
      hilb_new_elems = true;
      if (--hilb_n_in_degree == 0) flush_pairs();
    }
  else
    {
#ifdef DEVELOPMENT
#warning "todo: codimension stop condition"
#endif
      // codim test is set.  Compute the codimension now.
    }

  if (gbTrace >= 10)
    {
      //      lookupZZ->showmontable();
      showgb();
    }
}

void gbA::collect_syzygy(gbvector *f)
{
  _syz.push_back(f);
  n_syz++;

  if (gbTrace >= 10)
    {
      buffer o;
      o << " new syzygy : ";
      R->gbvector_text_out(o,_Fsyz,f);
      emit_line(o.str());
    }
}

void gbA::handle_elem(POLY f, gbelem_type minlevel)
{
  if (!R->gbvector_is_zero(f.f))
    {
      insert(f,minlevel);
      if (gbTrace == 3)
	emit_wrapped("m");
    }
  else 
    {
      originalR->get_quotient_info()->gbvector_normal_form(_Fsyz, f.fsyz);
      if (!R->gbvector_is_zero(f.fsyz))
	{
	  /* This is a syzygy */
	  collect_syzygy(f.fsyz);
	  if (gbTrace == 3)
	    emit_wrapped("z");
	}
      else
	{
	  if (gbTrace == 3)
	    emit_wrapped("o");
	}
    }
}

bool gbA::s_pair_step()
{
  spair *p = spair_set_next();
  if (!p) return false;

  stats_npairs++;

  if (reduce(p)) /* i.e. if the reduction is not deferred */
    {
      gbelem_type minlevel = (p->type == SPAIR_GEN ? ELEM_POSSIBLE_MINGEN : ELEM_MIN_GB);
      if (p->type == SPAIR_GEN)
	n_gens_left--;
      POLY f = p->x.f;
      p->x.f.f = 0;
      p->x.f.fsyz = 0;
      spair_delete(p);

      handle_elem(f,minlevel);
    }
  return true;
}

// new version

void gbA::new_insert(POLY f, gbelem_type minlevel)
{
  /* Reduce this element as far as possible.  This either removes content, 
     makes it monic, or at least negates it so the lead coeff is positive. */
  ring_elem junk;

  //DEBUG BLOCK  int fwt;
  //  int fdeg = weightInfo_->gbvector_weight(f.f, fwt);
  //  fprintf(stderr, "inserting GB element %d, thisdeg %d deg %d alpha %d\n", 
  //	  gb.size(), 
  //	  this_degree,
  //	  fdeg,
  //	  fdeg-fwt);

  remainder(f,this_degree,false,junk);

  //  fdeg = weightInfo_->gbvector_weight(f.f, fwt);
  //  fprintf(stderr, "    after remainder deg %d alpha %d\n", 
  //	  fdeg,
  //	  fdeg-fwt);

  stats_ngb++;

  gbelem *g = gbelem_make(f.f, f.fsyz, minlevel, this_degree);
  minimal_gb_valid = false;
  int me = gb.size();
  gb.push_back(g);
  n_gb++;
  int x = g->g.f->comp;

  // In a encoded Schreyer order, the following line might miss subring elements.
  // But it at least won't be incorrect...
  if (R->get_flattened_monoid()->in_subring(1,g->g.f->monom))
    n_subring++;

  if (over_ZZ())
    lookupZZ->insert(MPZ_VAL(g->g.f->coeff),g->lead,x,me);
  else
    lookup->insert(g->lead, x, me);

  if (gbTrace >= 5)
    {
      char s[100];
      buffer o;
      sprintf(s, "inserting element %d (minimal %d): ",me,minlevel);
      o << s;
      R->gbvector_text_out(o,_F,g->g.f);
      emit_line(o.str()); o.reset();
      o << "                          syzygy : ";
      R->gbvector_text_out(o,_Fsyz,g->g.fsyz);
      emit_line(o.str());
    }

  auto_reduce_by(me);

  if (use_hilb)
    {
      hilb_new_elems = true;
      if (--hilb_n_in_degree == 0) flush_pairs();
    }
  else
    {
#ifdef DEVELOPMENT
#warning "todo: codimension stop condition"
#endif
      // codim test is set.  Compute the codimension now.
    }

  if (gbTrace >= 10)
    {
      //      lookupZZ->showmontable();
      showgb();
    }
}

bool gbA::process_spair(spair *p)
{
  stats_npairs++;

  if (!reduce(p)) return true;

  gbelem_type minlevel = (p->type == SPAIR_GEN ? ELEM_POSSIBLE_MINGEN : ELEM_MIN_GB);
  if (p->type == SPAIR_GEN) n_gens_left--;
  POLY f = p->x.f;
  p->x.f.f = 0;
  p->x.f.fsyz = 0;
  spair_delete(p);


  if (!R->gbvector_is_zero(f.f))
    {
      new_insert(f,minlevel);
      if (gbTrace == 3)	emit_wrapped("m");
    }
  else 
    {
      originalR->get_quotient_info()->gbvector_normal_form(_Fsyz, f.fsyz);
      if (!R->gbvector_is_zero(f.fsyz))
	{
	  /* This is a syzygy */
	  collect_syzygy(f.fsyz);
	  if (gbTrace == 3) emit_wrapped("z");
	}
      else
	{
	  if (gbTrace == 3) emit_wrapped("o");
	}
    }
  return true;
}

ComputationStatusCode gbA::computation_is_complete()
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && gb.size() >= stop_.basis_element_limit) 
    return COMP_DONE_GB_LIMIT;
  if (stop_.syzygy_limit > 0 && n_syz >= stop_.syzygy_limit)
    return COMP_DONE_SYZ_LIMIT;
  if (stop_.pair_limit > 0 && n_pairs_computed >= stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.just_min_gens && n_gens_left == 0)
    return COMP_DONE_MIN_GENS;
  if (stop_.subring_limit > 0 && n_subring >= stop_.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (stop_.use_codim_limit)
    {
      // Compute the codimension
      int c = 0;
      //int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit)
	return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

Matrix *gbA::make_lead_term_matrix()
{
  MatrixConstructor result(_F,0);
  ring_elem one = originalR->Ncoeffs()->one();
  for (int i=first_gb_element; i<gb.size(); i++)
    {
      gbelem *g = gb[i];
      if (g->minlevel != ELEM_NON_MIN_GB)
	{
	  gbvector *f = g->g.f;
	  assert(f != 0);
	  // Only grab the lead term, which should be non-null
	  gbvector *fnext = f->next;
	  f->next = 0;
	  vec v = originalR->translate_gbvector_to_vec(_F, f);
	  f->next = fnext;
	  result.append(v);
	}
    }
  return result.to_matrix();
}

// new code
void gbA::do_computation()
{
  ComputationStatusCode ret;
  spair *p;

  // initial state is STATE_NEWDEGREE

  if (stop_.always_stop) return; // don't change status

  if ((ret = computation_is_complete()) != COMP_COMPUTING)
    {
      set_status(ret);
      return;
    }

  for (;;)
    {
      if (stop_.stop_after_degree && this_degree > stop_.degree_limit->array[0])
	{
	  // Break out now if we don't have anything else to compute in this degree.
	  set_status(COMP_DONE_DEGREE_LIMIT);
	  return;
	}
      if (gbTrace & PrintingDegree)
	{
	}

      switch(state) {

      case STATE_NEWPAIRS:
	// Loop through all of the new GB elements, and
	// compute spairs.  Start at np_i
	// np_i is initialized at the beginning, and also here.
	    while (np_i < n_gb)
	      {
		if (system_interruptedFlag)
		  {
		    set_status(COMP_INTERRUPTED);
		    return;
		  }
		if (gb[np_i]->minlevel != ELEM_NON_MIN_GB)
		  update_pairs(np_i);
		np_i++;
	      }
	    state = STATE_HILB;
	    
      case STATE_HILB:
	// If we are using hilbert function tracking:
	// Recompute the Hilbert function if new GB elements have been added
	
	    if (hilb_new_elems)
	      {
		// Recompute h, hf_diff
		Matrix *hf = make_lead_term_matrix();
		RingElement *h = hilb_comp::hilbertNumerator(hf);
		if (h == 0)
		  {
		    set_status(COMP_INTERRUPTED);
		    return;
		  }
		hf_diff = (*h) - (*hf_orig);
		hilb_new_elems = false;
	      }
	    state = STATE_NEWDEGREE;
	    
      case STATE_NEWDEGREE:
	// Get the spairs and generators for the next degree
	
	    if (S->n_in_degree == 0)
	      {
		int old_degree = this_degree;
		npairs = spair_set_prepare_next_degree(this_degree); // sets this_degree
		if (old_degree < this_degree)
		  first_in_degree = gb.size();
		complete_thru_this_degree = this_degree-1;
		if (npairs == 0)
		  {
		    state = STATE_DONE;
		    set_status(COMP_DONE);
		    return;
		  }
		if (stop_.stop_after_degree && this_degree > stop_.degree_limit->array[0])
		  {
		    set_status(COMP_DONE_DEGREE_LIMIT);
		    return;
		  }
		if (use_hilb)
		  {
		    hilb_n_in_degree = hilb_comp::coeff_of(hf_diff, this_degree);
		    if (hilb_n_in_degree == 0) flush_pairs();
		  }
	      }
	    if (gbTrace >= 1)
	      {
		buffer o;
		o << '{' << this_degree << '}';
		o << '(';
		if (use_hilb) 
		  o << hilb_n_in_degree << ',';
		o << npairs << ')';
		emit_wrapped(o.str());
	      }
	    ar_i = n_gb;
	    ar_j = ar_i+1;
	    state = STATE_SPAIRS;
	    
      case STATE_SPAIRS:
      case STATE_GENS:
	// Compute the spairs for this degree
	
	    while ((p = spair_set_next()) != 0)
	      {
		process_spair(p);
		npairs--;
		n_pairs_computed++;

		if ((ret = computation_is_complete()) != COMP_COMPUTING)
		  {
		    set_status(ret);
		    return;
		  }
		
		if (system_interruptedFlag)
		  {
		    set_status(COMP_INTERRUPTED);
		    return;
		  }
	      }
	    state = STATE_AUTOREDUCE;
	    // or state = STATE_NEWPAIRS
	    
      case STATE_AUTOREDUCE:
	// This is still possibly best performed when inserting a new element
	// Perform the necessary or desired auto-reductions

	    while (ar_i < n_gb)
	      {
		while (ar_j < n_gb)
		  {
		    if (system_interruptedFlag)
		      {
			set_status(COMP_INTERRUPTED);
			return;
		      }
		    if (over_ZZ())
		      {
			R->gbvector_auto_reduce_ZZ(_F, _Fsyz,
						   gb[ar_i]->g.f, gb[ar_i]->g.fsyz, 
						   gb[ar_j]->g.f, gb[ar_j]->g.fsyz);
		      }
		    else
		      {
			R->gbvector_auto_reduce(_F, _Fsyz,
						gb[ar_i]->g.f, gb[ar_i]->g.fsyz, 
						gb[ar_j]->g.f, gb[ar_j]->g.fsyz);
		      }
		    ar_j++;
		  }
		ar_i++;
		ar_j = ar_i+1;
	      }
	    state = STATE_NEWPAIRS;
	    break;
	    
      case STATE_DONE:
	return;
      }
  }
}



void gbA::start_computation()
{
  do_computation();
  if (gbTrace >= 1) show_mem_usage();
  return;
}


/*******************************
 ** Minimalization of the GB ***
 *******************************/
void gbA::minimalize_gb()
{
  if (minimal_gb_valid) return;

  VECTOR(POLY) polys;
  for (int i=first_gb_element; i<gb.size(); i++)
    {
      if (gb[i]->minlevel != ELEM_NON_MIN_GB)
	polys.push_back(gb[i]->g);
    }

  minimal_gb->minimalize(polys);
  minimal_gb_valid = true;
}

/*******************************
 ** Hilbert function routines **
 *******************************/

void gbA::flush_pairs()
{
  spair *p;
  while ((p = spair_set_next()) != 0)
    {
      n_saved_hilb++;
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

  // We may only use the Hilbert function if syzygies are not being collected
  // since otherwise we will miss syzygies

  if (!_collect_syz)
    {
      hf_orig = hf;
      hf_diff = RingElement::make_raw(hf->get_ring(), ZERO_RINGELEM);
      use_hilb = true;
      hilb_new_elems = true;
      state = STATE_HILB;
    }

  return this;
}

const MatrixOrNull *gbA::get_gb()
{
  minimalize_gb();
  //  fprintf(stderr, "-- done with GB -- \n");
  return minimal_gb->get_gb();
}

const MatrixOrNull *gbA::get_mingens()
{
  MatrixConstructor mat(_F,0);
  for (VECTOR(gbelem *)::iterator i = gb.begin(); i != gb.end(); i++)
    if ((*i)->minlevel == ELEM_POSSIBLE_MINGEN)
      mat.append(originalR->translate_gbvector_to_vec(_F, (*i)->g.f));
  return mat.to_matrix();

}

const MatrixOrNull *gbA::get_change()
{
  minimalize_gb();
  return minimal_gb->get_change();
}

const MatrixOrNull *gbA::get_syzygies()
{
  // The (non-minimal) syzygy matrix
  MatrixConstructor mat(_Fsyz, 0);
  for (VECTOR(gbvector *)::iterator i = _syz.begin(); i != _syz.end(); i++)
    {
      mat.append(originalR->translate_gbvector_to_vec(_Fsyz, *i));
    }
  return mat.to_matrix();
}

const MatrixOrNull *gbA::get_initial(int nparts)
{
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
}

const MatrixOrNull *gbA::matrix_remainder(const Matrix *m)
{
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
}

void gbA::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
  minimalize_gb();
  return minimal_gb->matrix_lift(m, result_remainder, result_quotient);
}

int gbA::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  minimalize_gb();
  return minimal_gb->contains(m);
}

int gbA::complete_thru_degree() const
  // The computation is complete up through this degree.
{
  return complete_thru_this_degree;
}

void gbA::text_out(buffer &o)
  /* This displays statistical information, and depends on the
     gbTrace value */
{
  o << "# pairs computed = " << n_pairs_computed << newline;
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
  emit_line(o.str());
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
  o << "Groebner basis, " << gb.size() << " elements";
  emit_line(o.str()); o.reset();
  for (unsigned int i=0; i<gb.size(); i++)
    {
      o << "    " << i << '\t' << "deg " << gb[i]->deg << '\t' 
        << "alpha " << gb[i]->alpha << '\t'
        << "min " << gb[i]->minlevel << '\t';
      R->gbvector_text_out(o, _F, gb[i]->g.f);
      emit_line(o.str()); o.reset();
    }
}

void gbA::show_mem_usage()
{
  long nmonoms = 0;
  for (int i=0; i<gb.size(); i++)
    {
      nmonoms += R->gbvector_n_terms(gb[i]->g.f);
      nmonoms += R->gbvector_n_terms(gb[i]->g.fsyz);
    }
  printf("\nnumber of (nonminimal) gb elements = %d\n", (int)gb.size());
  printf("number of monomials                = %ld\n", nmonoms);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
