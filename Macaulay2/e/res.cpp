// Copyright 1996.  Michael E. Stillman

#include "respoly.hpp"
#include "res.hpp"

stash *res_pair::mystash;
stash *res_degree::mystash;
stash *res_level::mystash;
stash *res_comp::mystash;

extern char system_interrupted;
extern int comp_printlevel;

//////////////////////////////////////////////
//  Initialization of a computation  /////////
//////////////////////////////////////////////

void res_comp::initialize(Matrix mat, 
			  int LengthLimit,
			  int /*strategy*/)
{
  int i;

  P = mat.Ring_of()->cast_to_poly_ring();
  assert(P != NULL);
  R = new res_poly((PolynomialRing *)P);
  M = P->Nmonoms();
  K = P->Ncoeffs();
  bump_up(P);
  bump_up(K);
  generator_matrix = mat;

  for (i=0; i<=LengthLimit; i++)
    resn.append(new res_level);

  max_degree = M->max_degree();
  length_limit = LengthLimit;
  if (mat.n_rows() > 0)
    {
      lodegree = mat.rows()->primary_degree(0);
      for (i=1; i<mat.n_rows(); i++)
	if (lodegree > mat.rows()->primary_degree(i))
	  lodegree = mat.rows()->primary_degree(i);
    }
  else
    lodegree = 0;

  for (i=0; i<mat.n_cols(); i++)
    if (lodegree > mat.cols()->primary_degree(i) - 1)
      lodegree = mat.cols()->primary_degree(i) - 1;

  hidegree = lodegree;
  n_level = 2;
  n_degree = lodegree;

  component_number = 0;
  next_me_number = 0;
  for (i=0; i<mat.n_rows(); i++)
    {
      res_pair *p = new res_pair;

      p->me = component_number++;
      next_me_number++;	// this and 'component_number' should still be equal here.
      p->compare_num = i;

      p->base_monom = M->make_new(mat.rows()->base_monom(i));
      p->mi = MonomialIdeal(P);
      p->syz_type = SYZ_MINIMAL;
      p->base_comp = p;
      base_components.append(p);
      search_mi.append(MonomialIdeal(P));

      int d = mat.rows()->primary_degree(i);
      res_degree *pairs = make_degree_set(0, d);
      p->next = pairs->first;
      pairs->first = p;
      pairs->npairs++;
      resn[0]->npairs++;
      npairs++;
    }

  nleft = 0;
  npairs = 0;
  nminimal = 0;

  for (i=0; i<mat.n_cols(); i++)
    if (mat[i] != NULL)
      {
	res_pair *p = new_res_pair(i); // Makes a generator 'pair'
	int d = mat.cols()->primary_degree(i);
	res_degree *pairs = make_degree_set(1, d-1);
	p->next = pairs->next_gen;
	pairs->next_gen = p;
	pairs->nleft++;
	pairs->npairs++;
	resn[1]->nleft++;
	resn[1]->npairs++;
	nleft++;
	npairs++;
      }

  for (i=0; i<base_components.length(); i++)
    {
      res_pair *p = base_components[i];
      p->compare_num = i;
    }

  REDUCE_exp = new int[P->n_vars()];
  REDUCE_mon = M->make_one();
  PAIRS_mon = M->make_one();
  MINIMAL_mon = M->make_one();
}

res_comp::res_comp(Matrix m, 
		   int LengthLimit, 
		   int strategy)
{
  initialize(m, LengthLimit, strategy);
}

void res_comp::remove_res_pair(res_pair *p)
{
  if (p == NULL) return;
  R->remove(p->syz);
  R->remove(p->stripped_syz);
  M->remove(p->base_monom);
  delete p;
}

void res_comp::remove_res_degree(res_degree *p)
{
  if (p == NULL) return;
  while (p->first != NULL)
    {
      res_pair *tmp = p->first;
      p->first = tmp->next;
      remove_res_pair(tmp);
    }
  delete p;
}

void res_comp::remove_res_level(res_level *lev)
{
  if (lev == NULL) return;
  int i;
  for (i=0; i<lev->bin.length(); i++)
    {
      res_degree *pairs = lev->bin[i];
      remove_res_degree(pairs);
    }
  delete lev;
}

res_comp::~res_comp()
{
  int i;
  for (i=0; i<resn.length(); i++)
    remove_res_level(resn[i]);

  delete [] REDUCE_exp;
  M->remove(REDUCE_mon);
  M->remove(PAIRS_mon);
  M->remove(MINIMAL_mon);

  // base_components have all been removed by this point
  // Since they appear in resn[0].

  bump_down(P);
  bump_down(K);
  delete R;
}
//////////////////////////////////////////////
//  Data structure insertion and access  /////
//////////////////////////////////////////////

res_degree *res_comp::make_degree_set(int level, int deg)
     // If there is such a res_degree, return it.
     // If not, first create it, and then return it.
{
  int i;
  if (level >= resn.length())
    {
      // Create new res_levels
      for (i=resn.length(); i<=level; i++)
	resn.append(new res_level);
    }
  res_level *lev = resn[level];
  
  deg -= lodegree;
  assert(deg >= 0);		// This would be an internal error
  if (deg >= lev->bin.length())
    {
      // Create new res_degrees
      for (i=lev->bin.length(); i<=deg; i++)
	lev->bin.append(new res_degree);
      if (deg+lodegree > hidegree) hidegree = deg+lodegree;
    }

  return lev->bin[deg];
}

res_degree *res_comp::get_degree_set(int level, int d) const
    // find (level,d) pair set, where 'd' is the slanted degree
{
  if (level < 0 || level >= resn.length())
    return NULL;
  res_level *lev = resn[level];
  d -= lodegree;
  if (d < 0 || d >= lev->bin.length())
    return NULL;
  return lev->bin[d];
}

res_pair *res_comp::new_res_pair(int syztype, res_pair *first, res_pair *second)
{
  res_pair *result = new res_pair;
  result->me = component_number++;
  result->base_monom = M->make_one();
  result->first = first;
  result->second = second;
  result->base_comp = first->base_comp;
  result->syz_type = syztype;
  result->mi = MonomialIdeal(P);
  return result;
}

res_pair *res_comp::new_res_pair(int i)
{
  res_pair *p = new res_pair; // Fills in almost all fields
  p->syz_type = SYZ_GEN;
  p->me = component_number++;
  p->compare_num = i;
  p->syz = R->from_vector(base_components, generator_matrix[i]);
  p->base_monom = M->make_new(p->syz->monom);
  p->base_comp = p->syz->comp;
  p->first = p->base_comp;
  p->mi = MonomialIdeal(P);

  return p;
}

res_pair *res_comp::new_res_pair(int syztype, resterm *f)
{
  // This is a level 1 pair.
  res_pair *p = new res_pair; // Fills in almost all fields
  p->me = component_number++;
  p->syz_type = syztype;
  p->syz = f;
  p->base_monom = M->make_new(f->monom);
  p->base_comp = f->comp->base_comp;
  p->first = f->comp;
  p->mi = MonomialIdeal(P);

  return p;
}

int res_comp::degree(const res_pair *p) const
{
  int result = M->primary_degree(p->base_monom);
  result += generator_matrix.rows()->primary_degree(p->base_comp->me);
  return result;
}

void res_comp::multi_degree(const res_pair *p, int *deg) const
{
  // MES: Is this correct?
  M->multi_degree(p->base_monom, deg);
  degree_monoid()->mult(deg, generator_matrix.rows()->degree(p->base_comp->me), deg);
}

void res_comp::insert_res_pair(int level, res_pair *p)
{
  // First determine the degree of 'p':
  int deg = degree(p) - level;

  res_degree *pairs = make_degree_set(level, deg);
  p->next = pairs->first;
  pairs->first = p;
  
  if (level > 1)
    {
      resn[level]->npairs++;
      resn[level]->nleft++;
      pairs->nleft++;
      pairs->npairs++;
      npairs++;
      nleft++;
    }
  else
    {
      intarray vp;
      M->to_varpower(p->syz->monom, vp);
      search_mi[p->syz->comp->me].insert_minimal(new Bag(p, vp));
    }
}


//////////////////////////////////////////////
//  Sorting //////////////////////////////////
//////////////////////////////////////////////

static int compare_type = 0;
static int EXP1[1000], EXP2[1000];
int res_comp::compare_res_pairs(res_pair *f, res_pair *g) const
{
  int cmp, df, dg, i;
//  if (f->compare_num < g->compare_num) return 1;
//  if (f->compare_num > g->compare_num) return -1;
  // MES: what to do if we obtain equality? Here is one way:
  switch (compare_type) {
  case 1:
    // Compare using descending lexicographic order
    // on the usual set of variables
    M->to_expvector(f->base_monom, EXP1);
    M->to_expvector(g->base_monom, EXP2);
    for (i=0; i<M->n_vars(); i++)
      {
	if (EXP1[i] < EXP2[i]) return -1;
	if (EXP1[i] > EXP2[i]) return 1;
      }
    return 0;
  case 2:
    // Compare using ascending lexicographic order
    // on the usual set of variables
    M->to_expvector(f->base_monom, EXP1);
    M->to_expvector(g->base_monom, EXP2);
    for (i=0; i<M->n_vars(); i++)
      {
	if (EXP1[i] < EXP2[i]) return 1;
	if (EXP1[i] > EXP2[i]) return -1;
      }
    return 0;
  case 3:
    // Compare using descending lexicographic order
    // on the reversed set of variables
    M->to_expvector(f->base_monom, EXP1);
    M->to_expvector(g->base_monom, EXP2);
    for (i=M->n_vars()-1; i>=0; i--)
      {
	if (EXP1[i] < EXP2[i]) return -1;
	if (EXP1[i] > EXP2[i]) return 1;
      }
    return 0;
  case 4:
    // Compare using ascending lexicographic order
    // on the reversed set of variables
    M->to_expvector(f->base_monom, EXP1);
    M->to_expvector(g->base_monom, EXP2);
    for (i=M->n_vars()-1; i>=0; i--)
      {
	if (EXP1[i] < EXP2[i]) return 1;
	if (EXP1[i] > EXP2[i]) return -1;
      }
    return 0;
  case 5:
    // The original method, but with degree added, since 
    // the new skeleton code doesn't just sort elements of
    // the same degree.
    df = degree(f);
    dg = degree(g);
    if (df > dg) return -1;
    if (df < dg) return 1;
    cmp = M->compare(f->base_monom, g->base_monom);
    if (cmp != 0) return cmp;
    cmp = f->first->compare_num - g->first->compare_num;
    if (cmp < 0) return 1;
    if (cmp > 0) return -1;
    return 0;
  default:
    cmp = M->compare(f->base_monom, g->base_monom);
    if (cmp != 0) return cmp;
    cmp = f->first->compare_num - g->first->compare_num;
    if (cmp < 0) return 1;
    if (cmp > 0) return -1;
    return 0;
  }
  return 0;
}

res_pair *res_comp::merge_res_pairs(res_pair *f, res_pair *g) const
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  res_pair head;
  res_pair *result = &head;
  while (1)
    switch (compare_res_pairs(f, g))
      {
      case 0:
      case -1:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case 1:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
	//      case 0:
	//	assert(0);
      }
}

void res_comp::sort_res_pairs(res_pair *& p) const
{
  // These elements are sorted in ascending 'me' values
  if (p == NULL || p->next == NULL) return;
  res_pair *p1 = NULL;
  res_pair *p2 = NULL;
  while (p != NULL)
    {
      res_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_res_pairs(p1);
  sort_res_pairs(p2);
  p = merge_res_pairs(p1, p2);
}

int res_comp::sort_value(res_pair *p, const int *sort_order) const
{
  M->to_expvector(p->base_monom, REDUCE_exp);
  int result = 0;
  for (int i=0; i<P->n_vars(); i++)
    result += REDUCE_exp[i] * sort_order[i];
  return result;
}

void res_comp::sort_gens(res_degree *pairs)
{
  if (pairs == NULL) return;
  res_pair *p = pairs->next_gen;
  if (p == NULL || p->next == NULL) return;
//  for (res_pair *q = p; q!=NULL; q=q->next)
//    q->compare_num = sort_value(q, s_pair_order); // MES: to be written

  sort_res_pairs(pairs->next_gen);
}

void res_comp::sort_pairs(int level, int deg)
{
  res_degree *pairs = get_degree_set(level, deg);
  if (pairs == NULL) return;
  if (pairs->is_sorted) return;
  res_pair *p = pairs->first;
  if (p != NULL && p->next != NULL)
    sort_res_pairs(pairs->first);

  pairs->next_pair = pairs->first;
  pairs->next_new_pair = pairs->first;
  pairs->is_sorted = 1;

  set_compare_nums(level, deg);
}


//////////////////////////////////////////////
//  Sorting Compare nums /////////////////////
//////////////////////////////////////////////

int res_comp::compare_compares(res_pair *f, res_pair *g) const
{
  int cmp = f->first->compare_num - g->first->compare_num;
  if (cmp < 0) return 1;
  if (cmp > 0) return -1;
  cmp = f->me - g->me;
  if (cmp < 0) return 1;
  if (cmp > 0) return -1;
  return 0;
}

res_pair *res_comp::merge_compares(res_pair *f, res_pair *g) const
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  res_pair head;
  res_pair *result = &head;
  while (1)
    switch (compare_compares(f, g))
      {
      case -1:
	result->next_compare = g;
	result = result->next_compare;
	g = g->next_compare;
	if (g == NULL) 
	  {
	    result->next_compare = f;
	    return head.next_compare;
	  }
	break;
      case 1:
	result->next_compare = f;
	result = result->next_compare;
	f = f->next_compare;
	if (f == NULL) 
	  {
	    result->next_compare = g; 
	    return head.next_compare;
	  }
	break;
      case 0:
	assert(0);
      }
}

void res_comp::sort_compares(res_pair *& p) const
{
  // These elements are sorted in ascending 'me' values
  if (p == NULL || p->next_compare == NULL) return;
  res_pair *p1 = NULL;
  res_pair *p2 = NULL;
  while (p != NULL)
    {
      res_pair *tmp = p;
      p = p->next_compare;
      tmp->next_compare = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next_compare;
      tmp->next_compare = p2;
      p2 = tmp;
    }

  sort_compares(p1);
  sort_compares(p2);
  p = merge_compares(p1, p2);
}

void res_comp::set_compare_nums(int level, int deg)
     // Set ALL of the compare_num fields at this level.
     // This will possibly change previous compare_num fields,
     // but that should not (!) affect the monomial order...
{
  if (level == 0) return;

  // This assumes that the pairs at level 'level' are ordered already
  // and that all previous pairs at this level (i.e. of lower degree)
  // are sorted on the 'next_compare' field, in ascending order

  // First, place the new pairs onto a 'next_compare' list, sort, and then merge
  // them into the previous list, using (first->compare_num, me) in ascending order.

  res_degree *pairs = get_degree_set(level, deg);
  if (pairs == NULL) return;

  res_pair *p;
  res_pair *compare_num_list = pairs->first;
  for (p = pairs->first; p != NULL; p = p->next)
    {
      p->next_compare = p->next;
      p->me = next_me_number++;
    }
  sort_compares(compare_num_list);
  resn[level]->compare_num_list = merge_compares(resn[level]->compare_num_list, compare_num_list);

  int next = 0;
  for (p = resn[level]->compare_num_list; p != NULL; p = p->next_compare)
    p->compare_num = next++;

}
//////////////////////////////////////////////
//  Creation of new pairs ////////////////////
//////////////////////////////////////////////

void res_comp::new_pairs(res_pair *p)
    // Create and insert all of the pairs which will have lead term 'p'.
    // This also places 'p' into the appropriate monomial ideal

    // Assumption: 'p' lies in a sorted list among its own degree/level
    // and only pairs with elements before this in the sorting order
    // will be considered.
{
  Index<MonomialIdeal> j;
  queue<Bag *> elems;
  intarray vp;			// This is 'p'.
  intarray thisvp;

  if (comp_printlevel >= 10)
    cerr << "Computing pairs with first = " << p->me << endl;
  M->divide(p->base_monom, p->first->base_monom, PAIRS_mon);
  M->to_varpower(PAIRS_mon, vp);

  // First add in syzygies arising from exterior variables
  // At the moment, there are none of this sort.

  if (M->is_skew())
    {
      intarray vplcm;
      intarray find_pairs_vp;

      int *skewvars = new int[M->n_vars()];
      varpower::to_ntuple(M->n_vars(), vp.raw(), find_pairs_vp);
      int nskew = M->exp_skew_vars(find_pairs_vp.raw(), skewvars);
      
      // Add in syzygies arising from exterior variables
      for (int v=0; v < nskew; v++)
	{
	  int w = skewvars[v];

	  thisvp.shrink(0);
	  varpower::var(w,1,thisvp);
	  Bag *b = new Bag(NULL, thisvp);
	  elems.insert(b);
	}
      // Remove the local variables
      delete [] skewvars;
    }

  // Second, add in syzygies arising from the base ring, if any
  // The baggage of each of these is NULL
  if (P->base_ring != NULL)
    for (j = P->Rideal.first(); j.valid(); j++)
      {
	// Compute (P->quotient_ideal->monom : p->monom)
	// and place this into a varpower and Bag, placing
	// that into 'elems'
	thisvp.shrink(0);
	varpower::divide(P->Rideal[j]->monom().raw(), vp.raw(), thisvp);
	if (varpower::is_equal(P->Rideal[j]->monom().raw(), thisvp.raw()))
	  continue;
	Bag *b = new Bag(NULL, thisvp);
	elems.insert(b);
      }
  
  // Third, add in syzygies arising from previous elements of this same level
  // The baggage of each of these is their corresponding res_pair

  MonomialIdeal &mi_orig = p->first->mi;
  for (j = mi_orig.first(); j.valid(); j++)
    {
      Bag *b = new Bag(mi_orig[j]->basis_ptr());
      varpower::divide(mi_orig[j]->monom().raw(), vp.raw(), b->monom());
      elems.insert(b);
    }

  // Make this monomial ideal, and then run through each minimal generator
  // and insert into the proper degree. (Notice that sorting does not
  // need to be done yet: only once that degree is about to begin.

  mi_orig.insert_minimal(new Bag(p, vp));

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal mi(P, elems, rejects);
  while (rejects.remove(b))
    delete b;

  if (comp_printlevel>= 11) mi.debug_out(1);

  for (j = mi.first(); j.valid(); j++)
    {
      res_pair *second = (res_pair *) mi[j]->basis_ptr();
      res_pair *q = new_res_pair(SYZ_S_PAIR, p, second);
      // That set most fields except base_monom:
      M->from_varpower(mi[j]->monom().raw(), q->base_monom);
      M->mult(q->base_monom, p->base_monom, q->base_monom);
      insert_res_pair(n_level, q);
    }
}

//////////////////////////////////////////////
//  S-pairs and reduction ////////////////////
//////////////////////////////////////////////

int res_comp::find_ring_divisor(const int *exp, ring_elem &result) const
     // If 'exp' is divisible by a ring lead term, then 1 is returned,
     // and result is set to be that ring element.
     // Otherwise 0 is returned.
{
  if (P->base_ring == NULL) return 0;
  Bag *b;
  if (!P->Rideal.search_expvector(exp, b))
    return 0;
  result = (Nterm *) b->basis_ptr();
  return 1;
}

resterm *res_comp::s_pair(res_pair *p) const
    // This sets p->syz to be the one or two term syzygy, and
    // returns this value multiplied out.
    // Care is of course taken with the Schreyer order
{
  p->syz = R->new_term(K->from_int(1), p->base_monom, p->first);
  int *si = M->make_one();
  M->divide(p->base_monom, p->first->base_monom, si);
  resterm *result = R->mult_by_monomial(p->first->syz, si);
  ring_elem one = K->from_int(1);
  if (p->second != NULL)
    {
      p->syz->next = R->new_term(K->from_int(-1), p->base_monom, p->second);
      M->divide(p->base_monom, p->second->base_monom, si);
      R->subtract_multiple_to(result, one, si, p->second->syz);
    }
  M->remove(si);
  K->remove(one);
  return result;
}

res_pair *res_comp::reduce(resterm * &f, resterm * &fsyz, resterm * &pivot)
     // Reduce f, placing the reduction history in fsyz.
     // Returns NULL if f reduces to 0, otherwise 
     // returns the res_pair at the previous level (f will "fill in" that pair), and
     // place a pointer to the corresponding term in "pivot".
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  resterm *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);

  res_pair *q;
  ring_elem rg;
  Bag *b;

  while (f != NULL)
    {
      M->divide(f->monom, f->comp->base_monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
	{
	  // Subtract off f, leave fsyz alone
	  Nterm *r = rg;
	  M->divide(f->monom, r->monom, REDUCE_mon);
	  R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
	}
      else if (f->comp->mi.search_expvector(REDUCE_exp, b))
	{
	  q = (res_pair *) (b->basis_ptr());
	  lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
	  lastterm = lastterm->next;
	  pivot = lastterm;
	  if (q->syz_type == SYZ_S_PAIR) return q; // i.e. not computed yet
	  M->divide(f->monom, q->syz->monom, REDUCE_mon);
	  R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
	}
      else
	{
	  // level 1: monomial not seen yet
	  q = new_res_pair(SYZ_S_PAIR, f);
	  insert_res_pair(1, q);
	  lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
	  lastterm = lastterm->next;
	  return q;
	}
    }
  return NULL;
}

// MES: Uugh.... This should not be a separate routine....?

res_pair *res_comp::reduce_level_one(resterm * &f, resterm * &fsyz, resterm * &pivot)
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  resterm *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);

  res_pair *q;
  ring_elem rg;
  Bag *b;

  while (f != NULL)
    {
      M->divide(f->monom, f->comp->base_monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
	{
	  // Subtract off f, leave fsyz alone
	  Nterm *r = rg;
	  M->divide(f->monom, r->monom, REDUCE_mon);
	  R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
	}
      else if (search_mi[f->comp->me].search_expvector(REDUCE_exp, b))
	{
	  q = (res_pair *) (b->basis_ptr());
	  lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
	  lastterm = lastterm->next;
	  pivot = lastterm;
	  if (q->syz_type == SYZ_S_PAIR) return q; // i.e. not computed yet
	  M->divide(f->monom, q->syz->monom, REDUCE_mon);
	  R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
	}
      else
	{
	  // level 1: monomial not seen yet
	  q = new_res_pair(SYZ_S_PAIR, f);
	  insert_res_pair(1, q);
	  lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
	  lastterm = lastterm->next;
	  pivot = lastterm;
	  return q;
	}
    }
  return NULL;
}

void res_comp::reduce_gen(resterm * &f) const
{
  res_pair *q;
  ring_elem rg;
  Bag *b;

  while (f != NULL)
    {
      M->divide(f->monom, f->comp->base_monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
	{
	  // Subtract off f, leave fsyz alone
	  Nterm *r = rg;
	  M->divide(f->monom, r->monom, REDUCE_mon);
	  R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
	}
      else if (search_mi[f->comp->me].search_expvector(REDUCE_exp, b))
	{
	  q = (res_pair *) (b->basis_ptr());
	  M->divide(f->monom, q->syz->monom, REDUCE_mon);
	  R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
	}
      else
	break;			// MES: possibly auto reduce further...
    }
}

//////////////////////////////////////////////
//  Toplevel calculation and state machine ///
//////////////////////////////////////////////

#define DO(CALL) {int result = CALL; if (result != COMP_COMPUTING) return result;}

static int SyzygyLimit;
static int PairLimit;

int res_comp::calc(const int *DegreeLimit, 
		   int LengthLimit, 
		   int ArgSyzygyLimit,
		   int ArgPairLimit,
		   int /*SyzLimitValue*/,
		   int /*SyzLimitLevel*/,
		   int /*SyzLimitDegree*/)
{
  SyzygyLimit = ArgSyzygyLimit;
  PairLimit = ArgPairLimit;

  if (LengthLimit >= 0)
    {
      if (length_limit < LengthLimit)
	{
	  *gError << "resolution: cannot increase maximum level";
	  return COMP_ERROR;
	}
      else
	length_limit = LengthLimit;
    }

  DO(gens(lodegree));
  DO(pairs(1,lodegree));		// MES: Probably not needed...

  for ( ; n_degree <= hidegree; n_degree++, n_level = 2)
    {
      if (DegreeLimit != NULL && *DegreeLimit < n_degree)
	return COMP_DONE_DEGREE_LIMIT;
      if (comp_printlevel >= 1) cerr << '{' << n_degree << '}';

      if (n_level == 2)
	{
	  DO(reductions(2,n_degree));
	  DO(gens(n_degree+1));
	  DO(pairs(1,n_degree+1));
	  n_level = 3;
	}

      for ( ; n_level <= length_limit+1; n_level++)
	{
	  if (comp_printlevel >= 1) cerr << '.';

	  DO(pairs(n_level-1, n_degree));
	  DO(pairs(n_level-1, n_degree+1));
	  DO(reductions(n_level, n_degree));
	}
    }
  return COMP_DONE;
}
int res_comp::gens(int deg)
{
  if (comp_printlevel >= 5) cerr << "gens(" << deg << ")" << endl;
  // preconditions: reductions(2,deg), gens(deg-1)
  res_pair *p;
  res_degree *pairs = get_degree_set(1, deg);
  if (pairs != NULL)
    {
      while ((p = pairs->next_gen) != NULL)
	{
	  pairs->next_gen = p->next;
	  handle_gen(p);	// Consumes 'p'
	  
	  pairs->nleft--;
	  resn[1]->nleft--;
	  nleft--;
	  if (PairLimit >= 0 && npairs-nleft >= PairLimit)
	    return COMP_DONE_PAIR_LIMIT;
	  if (SyzygyLimit >= 0 && nminimal >= SyzygyLimit)
	    return COMP_DONE_SYZYGY_LIMIT;
	  if (system_interrupted) return COMP_INTERRUPTED;
	}
      
      sort_pairs(1, deg); // Sort the level 1 GB elements
    }
  return COMP_COMPUTING;
}

int res_comp::pairs(int level, int deg)
{
  // Preconditions: pairs should be sorted
  // Cases: level=1: gens(deg), pairs(1,deg-1)
  //        level=2: pairs(1,deg)
  //        level>2: pairs(level-1,deg), pairs(level-2,deg+1)
  if (comp_printlevel >= 5) cerr << "pairs(" << level << ", " << deg << ")" << endl;
  res_pair *p;
  sort_pairs(level, deg);
  res_degree *pairs = get_degree_set(level, deg);
  if (pairs != NULL)
    {
      while ((p = pairs->next_new_pair) != NULL)
	{
	  pairs->next_new_pair = p->next;
	  new_pairs(p);
	  if (system_interrupted) return COMP_INTERRUPTED;
	}
    }
  return COMP_COMPUTING;
}

int res_comp::reductions(int level, int deg)
{
  res_pair *p;
  if (comp_printlevel >= 5) cerr << "reductions(" << level << ", " << deg << ")" << endl;
  sort_pairs(level, deg);
  res_degree *pairs = get_degree_set(level, deg);
  if (pairs != NULL)
    while ((p = pairs->next_pair) != NULL)
      {
	pairs->next_pair = p->next;
	handle_pair(p);
	
	pairs->nleft--;
	resn[level]->nleft--;
	nleft--;
	if (PairLimit >= 0 && npairs-nleft >= PairLimit)
	  return COMP_DONE_PAIR_LIMIT;
	if (SyzygyLimit >= 0 && nminimal >= SyzygyLimit)
	  return COMP_DONE_SYZYGY_LIMIT;
	if (system_interrupted) return COMP_INTERRUPTED;
      }
  return COMP_COMPUTING;
}


void res_comp::handle_gen(res_pair *p)
{
  reduce_gen(p->syz);
  if (p->syz != NULL)
    {
      R->make_monic(p->syz);
      M->copy(p->syz->monom, p->base_monom);
      p->first = p->syz->comp;
      p->second = NULL;
      p->syz_type = SYZ_MINIMAL;
      p->base_comp = p->syz->comp->base_comp;  // MES: added 7/11/97
      insert_res_pair(1, p);
      p->minimal_me = resn[1]->nminimal++;
      nminimal++;
      if (comp_printlevel >= 2) cerr << 'z';
    }
  else
    {
      remove_res_pair(p);
      if (comp_printlevel >= 2) cerr << 'o';
    }
}
void res_comp::handle_pair(res_pair *p)
{
  if (p->syz_type == SYZ_NOT_NEEDED) return;

  resterm *f = s_pair(p);
  res_pair *q;
  if (n_level == 2)
    q = reduce_level_one(f, p->syz, p->pivot_term);
  else
    q = reduce(f, p->syz, p->pivot_term);
  
  if (f == NULL)
    {
      // minimal syzygy
      p->syz_type = SYZ_MINIMAL;
      p->minimal_me = resn[n_level]->nminimal++;
      nminimal++;
      if (comp_printlevel >= 2) cerr << 'z';
    }
  else 
    {
      R->make_monic(f);
      p->syz_type = SYZ_NOT_MINIMAL;
      
      // non-minimal syzygy
      q->syz = f;
      q->syz_type = SYZ_NOT_NEEDED;
      if (comp_printlevel >= 2) cerr << 'm';
      // MES: need to decrement nleft for 'q'.
    }
}
#if 0
void res_comp::skeleton()
  // Compute the skeleton of the resolution
  // Currently: this is just used for debugging
{
  int level, deg;

  for (level=1; level < resn.length(); level++)
    {
      n_level = level+1;
      for (deg=0; deg < resn[level]->bin.length(); deg++)
	{
	  deg += lodegree;
	  res_degree *p = get_degree_set(level, deg);
	  if (p == NULL) continue;
	  if (level == 1)
	    {
	      // Get the generators back into the game
	      p->first = p->next_gen;
	      p->next_gen = NULL;
	    }
	  pairs(level, deg);
	}
    }
}
#endif

#include "res_aux.cpp"

//////////////////////////////////////
// Skeleton construction: test code //
//////////////////////////////////////

void res_comp::skeleton_init(array<res_pair *> &reslevel)
{
  int i;

  // Do level 0
  res_pair *pp = NULL;
  for (i=base_components.length()-1; i>=0; i--)
    {
      res_pair *p = base_components[i];
      p->next = pp;
      pp = p;
    }
  reslevel.append(pp);

  // Do level 1
  pp = NULL;
  for (i=0; i<generator_matrix.n_cols(); i++)
    if (generator_matrix[i] != NULL)
      {
	res_pair *p = new_res_pair(i); // Makes a generator 'pair'
	p->next = pp;
	pp = p;
      }
  reslevel.append(pp);
}

void res_comp::skeleton_pairs(res_pair *&result, res_pair *p)
    // Create and insert all of the pairs which will have lead term 'p'.
    // This also places 'p' into the appropriate monomial ideal
{
  Index<MonomialIdeal> j;
  queue<Bag *> elems;
  intarray vp;			// This is 'p'.
  intarray thisvp;

  if (comp_printlevel >= 10)
    cerr << "Computing pairs with first = " << p->me << endl;
  M->divide(p->base_monom, p->first->base_monom, PAIRS_mon);
  M->to_varpower(PAIRS_mon, vp);

  // First add in syzygies arising from exterior variables
  // At the moment, there are none of this sort.

  if (M->is_skew())
    {
      intarray vplcm;
      intarray find_pairs_vp;

      int *skewvars = new int[M->n_vars()];
      varpower::to_ntuple(M->n_vars(), vp.raw(), find_pairs_vp);
      int nskew = M->exp_skew_vars(find_pairs_vp.raw(), skewvars);
      
      // Add in syzygies arising from exterior variables
      for (int v=0; v < nskew; v++)
	{
	  int w = skewvars[v];

	  thisvp.shrink(0);
	  varpower::var(w,1,thisvp);
	  Bag *b = new Bag(NULL, thisvp);
	  elems.insert(b);
	}
      // Remove the local variables
      delete [] skewvars;
    }

  // Second, add in syzygies arising from the base ring, if any
  // The baggage of each of these is NULL
  if (P->base_ring != NULL)
    for (j = P->Rideal.first(); j.valid(); j++)
      {
	// Compute (P->quotient_ideal->monom : p->monom)
	// and place this into a varpower and Bag, placing
	// that into 'elems'
	thisvp.shrink(0);
	varpower::divide(P->Rideal[j]->monom().raw(), vp.raw(), thisvp);
	if (varpower::is_equal(P->Rideal[j]->monom().raw(), thisvp.raw()))
	  continue;
	Bag *b = new Bag(NULL, thisvp);
	elems.insert(b);
      }
  
  // Third, add in syzygies arising from previous elements of this same level
  // The baggage of each of these is their corresponding res_pair

  MonomialIdeal &mi_orig = p->first->mi;
  for (j = mi_orig.first(); j.valid(); j++)
    {
      Bag *b = new Bag(mi_orig[j]->basis_ptr());
      varpower::divide(mi_orig[j]->monom().raw(), vp.raw(), b->monom());
      elems.insert(b);
    }

  // Make this monomial ideal, and then run through each minimal generator
  // and insert into the proper degree. (Notice that sorting does not
  // need to be done yet: only once that degree is about to begin.

  mi_orig.insert_minimal(new Bag(p, vp));

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal mi(P, elems, rejects);
  while (rejects.remove(b))
    delete b;

  if (comp_printlevel>= 11) mi.debug_out(1);

  for (j = mi.first(); j.valid(); j++)
    {
      res_pair *second = (res_pair *) mi[j]->basis_ptr();
      res_pair *q = new_res_pair(SYZ_S_PAIR, p, second);
      // That set most fields except base_monom:
      M->from_varpower(mi[j]->monom().raw(), q->base_monom);
      M->mult(q->base_monom, p->base_monom, q->base_monom);
      result->next = q;
      result = q;
    }
}

int res_comp::skeleton_maxdegree(const array<res_pair *> &reslevel)
{
  int result = lodegree;
  for (int level=0; level < reslevel.length(); level++)
    {
      for (res_pair *p = reslevel[level]; p != NULL; p = p->next)
	{
	  int d = degree(p);
	  if (d-level > result)
	    result = d-level;
	}
    }
  return result;
}

void res_comp::skeleton_stats(const array<res_pair *> &reslevel)
{
  int level, i, d;
  int maxlevel = reslevel.length()-1;
  int maxdegree = skeleton_maxdegree(reslevel); // max slanted degree
  int *bettis = new int[(maxlevel+1)*(maxdegree+1)];
  for (i=(maxlevel+1)*(maxdegree+1)-1; i>=0; i--)
    bettis[i] = 0;

  for (level=0; level < reslevel.length(); level++)
    {
      for (res_pair *p = reslevel[level]; p != NULL; p = p->next)
	{
	  int d = degree(p);
	  d -= level;
	  d -= lodegree;
	  bettis[level + (maxlevel+1)*d] += 1;
	}
    }
  
  // Now make an intarray so that we may just use betti_display
  intarray betti;
  betti.append(lodegree);
  betti.append(maxdegree);
  betti.append(maxlevel);
  for (d=0; d<=maxdegree-lodegree; d++)
    for (level=0; level<=maxlevel; level++)
      betti.append(bettis[level + (maxlevel+1)*d]);

  betti_display(cerr, betti);
  delete [] bettis;

  for (level=0; level <= maxlevel; level++)
    {
      cerr << "---- level " << level << " ----" << endl;
      for (res_pair *p = reslevel[level]; p != NULL; p = p->next)
	{
	  int d = degree(p);
	  cerr << setw(4) << d << ' ';
	  text_out(p);
	}
    }
}

void res_comp::skeleton(int strategy)
  // Compute the skeleton of the resolution
  // Currently: this is just used for debugging
{
  int level;
  array<res_pair *> reslevel;

  // First, set reslevel[0], reslevel[1].
  skeleton_init(reslevel);

  // Now loop through each level, until the length limit is hit,
  // or there are no new pairs

  for (level=1; level < reslevel.length(); level++)
    {
      // Sort the pairs in the current level:
      res_pair *pp = reslevel[level];
      if (pp == NULL) break;

      compare_type = strategy;
      sort_res_pairs(pp);
      reslevel[level] = pp;
      compare_type = 0;

      // Now compute the pairs at the next level
      res_pair head, *ptrhead;
      head.next = NULL;
      ptrhead = &head;
      for (res_pair *p = pp; p != NULL; p = p->next)
	skeleton_pairs(ptrhead, p);
      reslevel.append(head.next);
    }

  // Now display the skeleton and stats on it
  skeleton_stats(reslevel);
}
