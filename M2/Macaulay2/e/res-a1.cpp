// Copyright 1996.  Michael E. Stillman

#include "res-a1-poly.hpp"
#include "res-a1.hpp"
#include "text-io.hpp"
#include "interrupted.hpp"
//////////////////////////////////////////////
//  Initialization of a computation  /////////
//////////////////////////////////////////////

void res_comp::initialize(const Matrix *mat, int LengthLimit, int /*strategy*/)
{
  int i;

  P = mat->get_ring()->cast_to_PolynomialRing();
  assert(P != NULL);
  R = new res_poly(const_cast<PolynomialRing *>(P));
  M = P->getMonoid();
  K = P->getCoefficientRing();
  generator_matrix = mat;

  // These next two lines may be added next (5/2/06)
  //  res_degree_stash      = new stash("resDegree", sizeof(res_degree));
  //  res_level_stash       = new stash("resLevel", sizeof(res_level));
  res_pair_stash = new stash("respair", sizeof(res_pair));
  mi_stash = new stash("res minodes", sizeof(Nmi_node));

  for (i = 0; i <= LengthLimit; i++) resn.push_back(new res_level);

  max_degree = M->max_degree();
  length_limit = LengthLimit;
  if (mat->n_rows() > 0)
    {
      lodegree = mat->rows()->primary_degree(0);
      for (i = 1; i < mat->n_rows(); i++)
        if (lodegree > mat->rows()->primary_degree(i))
          lodegree = mat->rows()->primary_degree(i);
    }
  else
    lodegree = 0;

  for (i = 0; i < mat->n_cols(); i++)
    if (lodegree > mat->cols()->primary_degree(i) - 1)
      lodegree = mat->cols()->primary_degree(i) - 1;

  hidegree = lodegree;
  n_level = 2;
  n_degree = lodegree;

  component_number = 0;
  next_me_number = 0;
  const SchreyerOrder *S = mat->rows()->get_schreyer_order();
  for (i = 0; i < mat->n_rows(); i++)
    {
      res_pair *p = new_res_pair();

      p->me = component_number++;
      p->minimal_me = p->me;
      next_me_number++;  // this and 'component_number' should still be equal
                         // here.
      p->compare_num = i;

      if (S == 0)
        p->base_monom = M->make_one();
      else
        p->base_monom = M->make_new(S->base_monom(i));
      p->mi = new MonomialIdeal(P, mi_stash);
      p->syz_type = SYZ_MINIMAL;
      p->base_comp = p;
      base_components.push_back(p);
      search_mi.push_back(new MonomialIdeal(P, mi_stash));

      int d = mat->rows()->primary_degree(i);
      res_degree *mypairs = make_degree_set(0, d);
      p->next = mypairs->first;
      mypairs->first = p;
      mypairs->npairs++;
      resn[0]->npairs++;
      npairs++;
    }

  nleft = 0;
  npairs = 0;
  nminimal = 0;

  for (i = 0; i < mat->n_cols(); i++)
    if ((*mat)[i] != NULL)
      {
        res_pair *p = new_res_pair(i);  // Makes a generator 'pair'
        int d = mat->cols()->primary_degree(i);
        res_degree *mypairs = make_degree_set(1, d - 1);
        p->next = mypairs->next_gen;
        mypairs->next_gen = p;
        mypairs->nleft++;
        mypairs->npairs++;
        resn[1]->nleft++;
        resn[1]->npairs++;
        nleft++;
        npairs++;
      }

  for (i = 0; i < base_components.size(); i++)
    {
      res_pair *p = base_components[i];
      p->compare_num = i;
    }

  exp_size = EXPONENT_BYTE_SIZE(P->n_vars());
  monom_size = MONOMIAL_BYTE_SIZE(M->monomial_size());

  compare_type = 0;
}

res_comp::res_comp(const Matrix *m, int LengthLimit, int strategy)
{
  initialize(m, LengthLimit, strategy);
}

res_comp::~res_comp()
{
  int i;
  for (i = 0; i < resn.size(); i++) remove_res_level(resn[i]);

  for (i = 0; i < search_mi.size(); i++) delete search_mi[i];

  delete res_pair_stash;
  delete mi_stash;
  delete R;

  // base_components have all been removed by this point
  // Since they appear in resn[0].
}

void res_comp::remove_res_pair(res_pair *p)
{
  if (p == NULL) return;
  delete p->mi;
  R->remove(p->syz);
  R->remove(p->stripped_syz);
  M->remove(p->base_monom);
  res_pair_stash->delete_elem(p);
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
  deleteitem(p);
}

void res_comp::remove_res_level(res_level *lev)
{
  if (lev == NULL) return;
  int i;
  for (i = 0; i < lev->bin.size(); i++)
    {
      res_degree *mypairs = lev->bin[i];
      remove_res_degree(mypairs);
    }
  deleteitem(lev);
}

//////////////////////////////////////////////
//  Data structure insertion and access  /////
//////////////////////////////////////////////

res_degree *res_comp::make_degree_set(int level, int deg)
// If there is such a res_degree, return it.
// If not, first create it, and then return it.
{
  int i;
  if (level >= resn.size())
    {
      // Create new res_levels
      for (i = resn.size(); i <= level; i++) resn.push_back(new res_level);
    }
  res_level *lev = resn[level];

  deg -= lodegree;
  assert(deg >= 0);  // This would be an internal error
  if (deg >= lev->bin.size())
    {
      // Create new res_degrees
      for (i = lev->bin.size(); i <= deg; i++)
        lev->bin.push_back(new res_degree);
      if (deg + lodegree > hidegree) hidegree = deg + lodegree;
    }

  return lev->bin[deg];
}

res_degree *res_comp::get_degree_set(int level, int d) const
// find (level,d) pair set, where 'd' is the slanted degree
{
  if (level < 0 || level >= resn.size()) return NULL;
  res_level *lev = resn[level];
  d -= lodegree;
  if (d < 0 || d >= lev->bin.size()) return NULL;
  return lev->bin[d];
}

res_pair *res_comp::new_res_pair()
{
  res_pair *result = reinterpret_cast<res_pair *>(res_pair_stash->new_elem());
  result->me = 0;
  result->compare_num = 0;
  result->base_monom = NULL;
  result->next = NULL;
  result->first = NULL;
  result->second = NULL;
  result->base_comp = NULL;
  result->syz_type = 0;
  result->mi2 = NULL;
  result->next_mi = NULL;
  result->syz = NULL;
  result->minimal_me = 0;
  result->pivot_term = NULL;
  result->stripped_syz = NULL;
  return result;
}

res_pair *res_comp::new_res_pair(int syztype, res_pair *first, res_pair *second)
{
  res_pair *result = new_res_pair();
  result->me = component_number++;
  result->base_monom = M->make_one();
  result->first = first;
  result->second = second;
  result->base_comp = first->base_comp;
  result->syz_type = syztype;
  result->mi = new MonomialIdeal(P, mi_stash);
  return result;
}

res_pair *res_comp::new_res_pair(int i)
{
  res_pair *p = new_res_pair();  // Fills in almost all fields
  p->syz_type = SYZ_GEN;
  p->me = component_number++;
  p->compare_num = i;
  p->syz = R->from_vector(base_components, (*generator_matrix)[i]);
  p->base_monom = M->make_new(p->syz->monom);
  p->base_comp = p->syz->comp;
  p->first = p->base_comp;
  p->mi = new MonomialIdeal(P, mi_stash);

  return p;
}

res_pair *res_comp::new_res_pair(int syztype, resterm *f)
{
  // This is a level 1 pair.
  res_pair *p = new_res_pair();  // Fills in almost all fields
  p->me = component_number++;
  p->syz_type = syztype;
  p->syz = f;
  p->base_monom = M->make_new(f->monom);
  p->base_comp = f->comp->base_comp;
  p->first = f->comp;
  p->mi = new MonomialIdeal(P, mi_stash);

  return p;
}

int res_comp::degree(const res_pair *p) const
{
  int result = M->primary_degree(p->base_monom);
  result += generator_matrix->rows()->primary_degree(p->base_comp->me);
  return result;
}

void res_comp::multi_degree(const res_pair *p, int *deg) const
{
  // MES: Is this correct?
  M->multi_degree(p->base_monom, deg);
  M->degree_monoid()->mult(
      deg, generator_matrix->rows()->degree(p->base_comp->me), deg);
}

void res_comp::insert_res_pair(int level, res_pair *p)
{
  // First determine the degree of 'p':
  int deg = degree(p) - level;

  res_degree *mypairs = make_degree_set(level, deg);
  p->next = mypairs->first;
  mypairs->first = p;

  if (level > 1)
    {
      resn[level]->npairs++;
      resn[level]->nleft++;
      mypairs->nleft++;
      mypairs->npairs++;
      npairs++;
      nleft++;
    }
  else
    {
      intarray vp;
      M->to_varpower(p->syz->monom, vp);
      search_mi[p->syz->comp->me]->insert_minimal(new Bag(p, vp));
    }
}

//////////////////////////////////////////////
//  Sorting //////////////////////////////////
//////////////////////////////////////////////

int res_comp::compare_res_pairs(res_pair *f, res_pair *g) const
{
  exponents EXP1, EXP2;
  int cmp, df, dg, i;
  //  if (f->compare_num < g->compare_num) return 1;
  //  if (f->compare_num > g->compare_num) return -1;
  // MES: what to do if we obtain equality? Here is one way:
  switch (compare_type)
    {
      case 1:
        // Compare using descending lexicographic order
        // on the usual set of variables
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        M->to_expvector(f->base_monom, EXP1);
        M->to_expvector(g->base_monom, EXP2);
        for (i = 0; i < M->n_vars(); i++)
          {
            if (EXP1[i] < EXP2[i]) return -1;
            if (EXP1[i] > EXP2[i]) return 1;
          }
        return 0;
      case 2:
        // Compare using ascending lexicographic order
        // on the usual set of variables
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        M->to_expvector(f->base_monom, EXP1);
        M->to_expvector(g->base_monom, EXP2);
        for (i = 0; i < M->n_vars(); i++)
          {
            if (EXP1[i] < EXP2[i]) return 1;
            if (EXP1[i] > EXP2[i]) return -1;
          }
        return 0;
      case 3:
        // Compare using descending lexicographic order
        // on the reversed set of variables
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        M->to_expvector(f->base_monom, EXP1);
        M->to_expvector(g->base_monom, EXP2);
        for (i = M->n_vars() - 1; i >= 0; i--)
          {
            if (EXP1[i] < EXP2[i]) return -1;
            if (EXP1[i] > EXP2[i]) return 1;
          }
        return 0;
      case 4:
        // Compare using ascending lexicographic order
        // on the reversed set of variables
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        M->to_expvector(f->base_monom, EXP1);
        M->to_expvector(g->base_monom, EXP2);
        for (i = M->n_vars() - 1; i >= 0; i--)
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
  while (1) switch (compare_res_pairs(f, g))
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
          //      assert(0);
      }
}

void res_comp::sort_res_pairs(res_pair *&p) const
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
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  M->to_expvector(p->base_monom, REDUCE_exp);
  int result = 0;
  for (int i = 0; i < P->n_vars(); i++) result += REDUCE_exp[i] * sort_order[i];
  return result;
}

void res_comp::sort_gens(res_degree *mypairs)
{
  if (mypairs == NULL) return;
  res_pair *p = mypairs->next_gen;
  if (p == NULL || p->next == NULL) return;
  //  for (res_pair *q = p; q!=NULL; q=q->next)
  //    q->compare_num = sort_value(q, s_pair_order); // MES: to be written

  sort_res_pairs(mypairs->next_gen);
}

void res_comp::sort_pairs(int level, int deg)
{
  res_degree *mypairs = get_degree_set(level, deg);
  if (mypairs == NULL) return;
  if (mypairs->is_sorted) return;
  res_pair *p = mypairs->first;
  if (p != NULL && p->next != NULL) sort_res_pairs(mypairs->first);

  mypairs->next_pair = mypairs->first;
  mypairs->next_new_pair = mypairs->first;
  mypairs->is_sorted = 1;

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
  while (1) switch (compare_compares(f, g))
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

void res_comp::sort_compares(res_pair *&p) const
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
  // them into the previous list, using (first->compare_num, me) in ascending
  // order.

  res_degree *mypairs = get_degree_set(level, deg);
  if (mypairs == NULL) return;

  res_pair *p;
  res_pair *compare_num_list = mypairs->first;
  for (p = mypairs->first; p != NULL; p = p->next)
    {
      p->next_compare = p->next;
      p->me = next_me_number++;
    }
  sort_compares(compare_num_list);
  resn[level]->compare_num_list =
      merge_compares(resn[level]->compare_num_list, compare_num_list);

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
  intarray vp;  // This is 'p'.
  intarray thisvp;

  monomial PAIRS_mon = ALLOCATE_MONOMIAL(monom_size);

  if (M2_gbTrace >= 10)
    {
      buffer o;
      o << "Computing pairs with first = " << p->me << newline;
      emit(o.str());
    }
  M->divide(p->base_monom, p->first->base_monom, PAIRS_mon);
  M->to_varpower(PAIRS_mon, vp);

  // First add in syzygies arising from exterior variables
  // At the moment, there are none of this sort.

  if (P->is_skew_commutative())
    {
      int *exp = newarray_atomic(int, M->n_vars());
      varpower::to_ntuple(M->n_vars(), vp.raw(), exp);

      int nskew = P->n_skew_commutative_vars();
      for (int v = 0; v < nskew; v++)
        {
          int w = P->skew_variable(v);
          if (exp[w] > 0)
            {
              thisvp.shrink(0);
              varpower::var(w, 1, thisvp);
              Bag *b = new Bag(static_cast<void *>(0), thisvp);
              elems.insert(b);
            }
        }
      deletearray(exp);
    }

  // Second, add in syzygies arising from the base ring, if any
  // The baggage of each of these is NULL
  if (P->is_quotient_ring())
    {
      const MonomialIdeal *Rideal = P->get_quotient_monomials();
      for (j = Rideal->first(); j.valid(); j++)
        {
          // Compute (P->quotient_ideal->monom : p->monom)
          // and place this into a varpower and Bag, placing
          // that into 'elems'
          thisvp.shrink(0);
          varpower::quotient((*Rideal)[j]->monom().raw(), vp.raw(), thisvp);
          if (varpower::is_equal((*Rideal)[j]->monom().raw(), thisvp.raw()))
            continue;
          Bag *b = new Bag(static_cast<void *>(0), thisvp);
          elems.insert(b);
        }
    }
  // Third, add in syzygies arising from previous elements of this same level
  // The baggage of each of these is their corresponding res_pair

  MonomialIdeal *mi_orig = p->first->mi;
  for (j = mi_orig->first(); j.valid(); j++)
    {
      Bag *b = new Bag((*mi_orig)[j]->basis_ptr());
      varpower::quotient((*mi_orig)[j]->monom().raw(), vp.raw(), b->monom());
      elems.insert(b);
    }

  // Make this monomial ideal, and then run through each minimal generator
  // and insert into the proper degree. (Notice that sorting does not
  // need to be done yet: only once that degree is about to begin.

  mi_orig->insert_minimal(new Bag(p, vp));

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal *mi = new MonomialIdeal(P, elems, rejects, mi_stash);
  while (rejects.remove(b)) delete b;

  if (M2_gbTrace >= 11) mi->debug_out(1);

  for (j = mi->first(); j.valid(); j++)
    {
      res_pair *second = reinterpret_cast<res_pair *>((*mi)[j]->basis_ptr());
      res_pair *q = new_res_pair(SYZ_S_PAIR, p, second);
      // That set most fields except base_monom:
      M->from_varpower((*mi)[j]->monom().raw(), q->base_monom);
      M->mult(q->base_monom, p->base_monom, q->base_monom);
      insert_res_pair(n_level, q);
    }
  delete mi;
}

//////////////////////////////////////////////
//  S-pairs and reduction ////////////////////
//////////////////////////////////////////////

int res_comp::find_ring_divisor(const int *exp, ring_elem &result) const
// If 'exp' is divisible by a ring lead term, then 1 is returned,
// and result is set to be that ring element.
// Otherwise 0 is returned.
{
  if (!P->is_quotient_ring()) return 0;
  Bag *b;
  if (!P->get_quotient_monomials()->search_expvector(exp, b)) return 0;
  result.poly_val = P->quotient_element(b->basis_elem());
  return 1;
}

resterm *res_comp::s_pair(res_pair *p) const
// This sets p->syz to be the one or two term syzygy, and
// returns this value multiplied out.
// Care is of course taken with the Schreyer order
{
  p->syz = R->new_term(K->from_long(1), p->base_monom, p->first);
  int *si = M->make_one();
  M->divide(p->base_monom, p->first->base_monom, si);
  resterm *result = R->mult_by_monomial(p->first->syz, si);
  ring_elem one = K->from_long(1);
  if (p->second != NULL)
    {
      p->syz->next = R->new_term(K->from_long(-1), p->base_monom, p->second);
      M->divide(p->base_monom, p->second->base_monom, si);
      R->subtract_multiple_to(result, one, si, p->second->syz);
    }
  M->remove(si);
  K->remove(one);
  return result;
}

res_pair *res_comp::reduce(resterm *&f, resterm *&fsyz, resterm *&pivot)
// Reduce f, placing the reduction history in fsyz.
// Returns NULL if f reduces to 0, otherwise
// returns the res_pair at the previous level (f will "fill in" that pair), and
// place a pointer to the corresponding term in "pivot".
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

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
      else if (f->comp->mi->search_expvector(REDUCE_exp, b))
        {
          q = reinterpret_cast<res_pair *>(b->basis_ptr());
          lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
          lastterm = lastterm->next;
          pivot = lastterm;
          if (q->syz_type == SYZ_S_PAIR) return q;  // i.e. not computed yet
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

res_pair *res_comp::reduce_level_one(resterm *&f,
                                     resterm *&fsyz,
                                     resterm *&pivot)
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

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
      else if (search_mi[f->comp->me]->search_expvector(REDUCE_exp, b))
        {
          q = reinterpret_cast<res_pair *>(b->basis_ptr());
          lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
          lastterm = lastterm->next;
          pivot = lastterm;
          if (q->syz_type == SYZ_S_PAIR) return q;  // i.e. not computed yet
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

void res_comp::reduce_gen(resterm *&f) const
{
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

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
      else if (search_mi[f->comp->me]->search_expvector(REDUCE_exp, b))
        {
          q = reinterpret_cast<res_pair *>(b->basis_ptr());
          M->divide(f->monom, q->syz->monom, REDUCE_mon);
          R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
        }
      else
        break;  // MES: possibly auto reduce further...
    }
}

//////////////////////////////////////////////
//  Toplevel calculation and state machine ///
//////////////////////////////////////////////

bool res_comp::stop_conditions_ok()
{
  if (stop_.length_limit != 0 && stop_.length_limit->len > 0)
    {
      if (length_limit < stop_.length_limit->array[0])
        {
          ERROR("resolution: cannot increase maximum level");
          return false;
        }
      else
        length_limit = stop_.length_limit->array[0];
    }

  return true;
}

int res_comp::complete_thru_degree() const { return n_degree - 1; }
//#define DO(CALL) {int result = CALL; if (result != COMP_COMPUTING) return
// result;}
#define DO(CALL)                              \
  {                                           \
    enum ComputationStatusCode result = CALL; \
    if (result != COMP_COMPUTING)             \
      {                                       \
        set_status(result);                   \
        return;                               \
      }                                       \
  }

#if 0
// int res_comp::calc(const int *DegreeLimit,
//                 int LengthLimit,
//                 int ArgSyzygyLimit,
//                 int ArgPairLimit,
//                 int /*SyzLimitValue*/,
//                 int /*SyzLimitLevel*/,
//                 int /*SyzLimitDegree*/)
#endif
void res_comp::start_computation()
{
  if (status() == COMP_DONE) return;
  set_status(COMP_COMPUTING);
  DO(gens(lodegree));
  DO(pairs(1, lodegree));  // MES: Probably not needed...

  for (; n_degree <= hidegree; n_degree++, n_level = 2)
    {
      if (stop_.stop_after_degree && stop_.degree_limit->array[0] < n_degree)
        {
          set_status(COMP_DONE_DEGREE_LIMIT);
          return;
        }
      if (M2_gbTrace >= 1)
        {
          buffer o;
          o << '{' << n_degree << '}';
          emit_wrapped(o.str());
        }

      if (n_level == 2)
        {
          DO(reductions(2, n_degree));
          DO(gens(n_degree + 1));
          DO(pairs(1, n_degree + 1));
          n_level = 3;
        }

      for (; n_level <= length_limit + 1; n_level++)
        {
          if (M2_gbTrace >= 1) emit_wrapped(".");

          DO(pairs(n_level - 1, n_degree));
          DO(pairs(n_level - 1, n_degree + 1));
          DO(reductions(n_level, n_degree));
        }
    }
  if (M2_gbTrace >= 6)
    {
      buffer o;
      text_out(o);
      emit(o.str());
    }
  set_status(COMP_DONE);
}

enum ComputationStatusCode res_comp::gens(int deg)
{
  if (M2_gbTrace >= 5)
    {
      buffer o;
      o << "gens(" << deg << ")" << newline;
      emit(o.str());
    }
  // preconditions: reductions(2,deg), gens(deg-1)
  res_pair *p;
  res_degree *mypairs = get_degree_set(1, deg);
  if (mypairs != NULL)
    {
      while ((p = mypairs->next_gen) != NULL)
        {
          mypairs->next_gen = p->next;
          handle_gen(p);  // Consumes 'p'

          mypairs->nleft--;
          resn[1]->nleft--;
          nleft--;
          if (stop_.pair_limit > 0 && npairs - nleft >= stop_.pair_limit)
            return COMP_DONE_PAIR_LIMIT;
          if (stop_.syzygy_limit > 0 && nminimal >= stop_.syzygy_limit)
            return COMP_DONE_SYZYGY_LIMIT;
          if (system_interrupted()) return COMP_INTERRUPTED;
        }

      sort_pairs(1, deg);  // Sort the level 1 GB elements
    }
  return COMP_COMPUTING;
}

enum ComputationStatusCode res_comp::pairs(int level, int deg)
{
  // Preconditions: pairs should be sorted
  // Cases: level=1: gens(deg), pairs(1,deg-1)
  //        level=2: pairs(1,deg)
  //        level>2: pairs(level-1,deg), pairs(level-2,deg+1)
  if (M2_gbTrace >= 5)
    {
      buffer o;
      o << "pairs(" << level << ", " << deg << ")" << newline;
      emit(o.str());
    }
  res_pair *p;
  sort_pairs(level, deg);
  res_degree *mypairs = get_degree_set(level, deg);
  if (mypairs != NULL)
    {
      while ((p = mypairs->next_new_pair) != NULL)
        {
          mypairs->next_new_pair = p->next;
          new_pairs(p);
          if (system_interrupted()) return COMP_INTERRUPTED;
        }
    }
  return COMP_COMPUTING;
}

enum ComputationStatusCode res_comp::reductions(int level, int deg)
{
  res_pair *p;
  if (M2_gbTrace >= 5)
    {
      buffer o;
      o << "reductions(" << level << ", " << deg << ")" << newline;
      emit(o.str());
    }
  sort_pairs(level, deg);
  res_degree *mypairs = get_degree_set(level, deg);
  if (mypairs != NULL)
    while ((p = mypairs->next_pair) != NULL)
      {
        mypairs->next_pair = p->next;
        handle_pair(p);

        mypairs->nleft--;
        resn[level]->nleft--;
        nleft--;
        if (stop_.pair_limit > 0 && npairs - nleft >= stop_.pair_limit)
          return COMP_DONE_PAIR_LIMIT;
        if (stop_.syzygy_limit > 0 && nminimal >= stop_.syzygy_limit)
          return COMP_DONE_SYZYGY_LIMIT;
        if (system_interrupted()) return COMP_INTERRUPTED;
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
      if (M2_gbTrace >= 2) emit_wrapped("z");
    }
  else
    {
      remove_res_pair(p);
      if (M2_gbTrace >= 2) emit_wrapped("o");
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
      if (M2_gbTrace >= 2) emit_wrapped("z");
    }
  else
    {
      R->make_monic(f);
      p->syz_type = SYZ_NOT_MINIMAL;

      // non-minimal syzygy
      q->syz = f;
      q->syz_type = SYZ_NOT_NEEDED;
      if (M2_gbTrace >= 2) emit_wrapped("m");
      // MES: need to decrement nleft for 'q'.
    }
}
#if 0
// void res_comp::skeleton()
//   // Compute the skeleton of the resolution
//   // Currently: this is just used for debugging
// {
//   int level, deg;
//
//   for (level=1; level < resn.size(); level++)
//     {
//       n_level = level+1;
//       for (deg=0; deg < resn[level]->bin.length(); deg++)
//      {
//        deg += lodegree;
//        res_degree *p = get_degree_set(level, deg);
//        if (p == NULL) continue;
//        if (level == 1)
//          {
//            // Get the generators back into the game
//            p->first = p->next_gen;
//            p->next_gen = NULL;
//          }
//        pairs(level, deg);
//      }
//     }
// }
#endif

#include "matrix-con.hpp"

int res_comp::n_pairs(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;
  return p->npairs;
}

int res_comp::n_left(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;

  int result = 0;
  for (res_pair *q = p->first; q != NULL; q = q->next)
    if (q->syz_type == SYZ_S_PAIR || q->syz_type == SYZ_GEN) result++;

  return result;
}

int res_comp::n_minimal(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;
  int result = 0;
  for (res_pair *q = p->first; q != NULL; q = q->next)
    if (q->syz_type == SYZ_MINIMAL) result++;

  return result;
}

int res_comp::n_monoms(int lev, int d) const
{
  res_degree *p = get_degree_set(lev, d);
  if (p == NULL) return 0;
  int result = 0;
  for (res_pair *q = p->first; q != NULL; q = q->next)
    result += R->n_terms(q->syz);

  return result;
}

int res_comp::max_level() const { return resn.size(); }
int res_comp::low_degree() const { return lodegree; }
int res_comp::high_degree() const { return hidegree; }
M2_arrayint res_comp::get_betti(int type) const
{
  int lo = low_degree();
  int hi = high_degree();
  int len = (type == 0 ? length_limit : max_level());

  int *bettis;
  betti_init(lo, hi, len, bettis);

  for (int d = lo; d <= hi; d++)
    for (int lev = 0; lev <= len; lev++)
      {
        int val = 0;
        switch (type)
          {
            case 0:
              val = n_minimal(lev, d);
              break;
            case 1:
              val = n_pairs(lev, d);
              break;
            case 2:
              val = n_left(lev, d);
              break;
            case 3:
              val = n_monoms(lev, d);
              break;
            case 4:
              ERROR(
                  "cannot use Minimize=>true unless "
                  "res(...,FastNonminimal=>true) was used");
              return 0;
            default:
              val = -1;
              break;
          }
        bettis[lev + (len + 1) * (d - lo)] = val;
      }

  M2_arrayint result = betti_make(lo, hi, len, bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res_comp::betti_skeleton() const { return get_betti(1); }
M2_arrayint res_comp::betti_remaining() const { return get_betti(2); }
M2_arrayint res_comp::betti_minimal() const { return get_betti(0); }
M2_arrayint res_comp::betti_nmonoms() const { return get_betti(3); }
void res_comp::text_out(buffer &o, const res_pair *p) const
{
  res_pair *a = p->first;
  res_pair *b = p->second;  // possibly NULL
  o << p->me << ' ';
  if (a != NULL)
    o << a->me << ' ';
  else
    o << ". ";
  if (b != NULL)
    o << b->me << ' ';
  else
    o << ". ";

  o << p->compare_num << ' ';

  switch (p->syz_type)
    {
      case SYZ_S_PAIR:
        o << "PR";
        break;
      case SYZ_GEN:
        o << "GN";
        break;
      case SYZ_MINIMAL:
        o << "SZ";
        break;
      case SYZ_NOT_MINIMAL:
        o << "GB";
        break;
      case SYZ_NOT_NEEDED:
        o << "NO";
        break;
      default:
        break;
    }

#if 0
//   if (p->mi_exists)
#endif
  o << "[mi: " << p->mi->length() << "]";
#if 0
//   else
//     {
//       res_pair *q = p->next_div;
//       int n = 0;
//       while (q != NULL) { n++; q = q->next_div; }
//       o << "[midiv: " << n << "]";
//     }
#endif
  M->elem_text_out(o, p->base_monom);
  if (M2_gbTrace >= 3)
    {
      // Display the vector
      o << " syz: ";
      R->elem_text_out(o, p->syz);
    }
  o << newline;
}

void res_comp::text_out(const res_pair *p) const
{
  buffer o;
  text_out(o, p);
  emit(o.str());
}

void res_comp::stats() const
{
  buffer o;
  text_out(o);
  emit(o.str());
}
void res_comp::text_out(buffer &o) const
{
  o << "level/degree = " << n_level << '/' << n_degree << newline;
  o << "--- The total number of pairs in each level/slanted degree -----"
    << newline;
  M2_arrayint a = betti_skeleton();
  betti_display(o, a);
  o << "--- The number of pairs left to compute ------------------------"
    << newline;
  a = betti_remaining();
  betti_display(o, a);
  o << "--- (Lower bounds of) the minimal betti numbers ----------" << newline;
  a = betti_minimal();
  betti_display(o, a);
  if (M2_gbTrace >= 1)
    {
      o << "--- Number of monomials  ---------------------------------"
        << newline;
      a = betti_nmonoms();
      betti_display(o, a);
    }

  // If the printlevel is high enough, display each element
  if (M2_gbTrace >= 2)
    for (int lev = 0; lev < resn.size(); lev++)
      {
        o << "---- level " << lev << " ----" << newline;
        for (int i = 0; i < resn[lev]->bin.size(); i++)
          {
            res_degree *mypairs = resn[lev]->bin[i];
            if (mypairs == NULL) continue;
            for (res_pair *p = mypairs->first; p != NULL; p = p->next)
              {
                o.put(i, 4);
                o << ' ';
                text_out(o, p);
              }
          }
      }
}

const FreeModule *res_comp::free_of(int i) const
{
  FreeModule *result;
  result = P->make_Schreyer_FreeModule();
  if (i < 0 || i >= resn.size()) return result;
  int *deg = P->degree_monoid()->make_one();
  int n = 0;
  res_level *lev = resn[i];
  for (int j = 0; j < lev->bin.size(); j++)
    {
      res_degree *mypairs = lev->bin[j];
      for (res_pair *p = mypairs->first; p != NULL; p = p->next)
        {
          multi_degree(p, deg);
          result->append_schreyer(deg, p->base_monom, p->compare_num);
          p->minimal_me = n++;
        }
    }
  P->degree_monoid()->remove(deg);

  return result;
}
const FreeModule *res_comp::minimal_free_of(int i) const
{
  FreeModule *result;
  if (i == 0) return generator_matrix->rows();
  result = P->make_FreeModule();
  if (i < 0 || i > length_limit) return result;
  int *deg = P->degree_monoid()->make_one();
  int nminimals = 0;
  res_level *lev = resn[i];
  for (int j = 0; j < lev->bin.size(); j++)
    {
      res_degree *mypairs = lev->bin[j];
      for (res_pair *p = mypairs->first; p != NULL; p = p->next)
        if (p->syz_type == SYZ_MINIMAL)
          {
            multi_degree(p, deg);
            result->append(deg);
            p->minimal_me = nminimals++;
          }
    }
  P->degree_monoid()->remove(deg);

  return result;
}

Matrix *res_comp::make(int level) const
{
  const FreeModule *F = free_of(level - 1);
  const FreeModule *G = free_of(level);
  MatrixConstructor result(F, G, NULL);

  int n = 0;
  if (G == 0) return result.to_matrix();
  res_level *lev = resn[level];
  for (int j = 0; j < lev->bin.size(); j++)
    {
      res_degree *mypairs = lev->bin[j];
      for (res_pair *p = mypairs->first; p != NULL; p = p->next)
        result.set_column(n++, R->to_vector(p->syz, F));
    }
  return result.to_matrix();
}

//////////////////////////////////////////////
//  Minimal resolutions //////////////////////
//////////////////////////////////////////////

void res_comp::reduce_minimal(int x,
                              resterm *&f,
                              VECTOR(res_pair *)& elems) const
{
  monomial MINIMAL_mon = ALLOCATE_MONOMIAL(monom_size);

  // Reduce any components of 'f' that correspond to non minimal syzygies.
  const resterm *tm;

  for (int i = x - 1; i >= 0; i--)
    {
      res_pair *p = elems[i];
      if (p->syz_type == SYZ_NOT_MINIMAL)
        while ((tm = R->component_occurs_in(p->pivot_term->comp, f)) != NULL)
          {
            // Subtract the proper multiple to f.  f = ... + c m e_y + ...
            // and                                 p = ... + d n e_y
            // where n|m.  So we want to compute f -= c/d m/n p.
            ring_elem c =
                K->divide(tm->coeff, p->pivot_term->coeff);  // exact division
            // MES: is the following line actually needed?
            M->divide(tm->monom, p->pivot_term->monom, MINIMAL_mon);
            if (p->stripped_syz == NULL) p->stripped_syz = R->strip(p->syz);
            R->subtract_multiple_to(f, c, MINIMAL_mon, p->stripped_syz);
          }
    }
}

Matrix *res_comp::make_minimal(int i) const
{
  const FreeModule *F = minimal_free_of(i - 1);
  const FreeModule *G = minimal_free_of(i);
  MatrixConstructor result(F, G, NULL);
  if (i < 0 || i > length_limit) return result.to_matrix();
  VECTOR(res_pair *) elems;

  res_level *lev = resn[i];
  for (int j = 0; j < lev->bin.size(); j++)
    for (res_pair *p = lev->bin[j]->first; p != NULL; p = p->next)
      elems.push_back(p);

  int thisx = 0;
  for (int x = 0; x < elems.size(); x++)
    {
      res_pair *p = elems[x];
      if (p->syz_type == SYZ_MINIMAL)
        {
          if (p->stripped_syz == NULL)
            {
              p->stripped_syz = R->strip(p->syz);
              reduce_minimal(x, p->stripped_syz, elems);
            }
          result.set_column(thisx++, R->to_vector(p->stripped_syz, F, 1));
        }
    }
  return result.to_matrix();
}

//////////////////////////////////////
// Skeleton construction: test code //
//////////////////////////////////////


void res_comp::skeleton_init(VECTOR(res_pair *)& reslevel)
{
  // Do level 0
  res_pair *pp = NULL;
  for (auto p = base_components.rbegin(); p != base_components.rend(); ++p)
    {
      (*p)->next = pp;
      pp = *p;
    }
  reslevel.push_back(pp);

  // Do level 1
  pp = NULL;
  for (auto i = 0; i < generator_matrix->n_cols(); i++)
    if ((*generator_matrix)[i] != NULL)
      {
        res_pair *p = new_res_pair(i);  // Makes a generator 'pair'
        p->next = pp;
        pp = p;
      }
  reslevel.push_back(pp);
}

void res_comp::skeleton_pairs(res_pair *&result, res_pair *p)
// Create and insert all of the pairs which will have lead term 'p'.
// This also places 'p' into the appropriate monomial ideal
{
  Index<MonomialIdeal> j;
  queue<Bag *> elems;
  intarray vp;  // This is 'p'.
  intarray thisvp;

  monomial PAIRS_mon = ALLOCATE_MONOMIAL(monom_size);

  if (M2_gbTrace >= 10)
    {
      buffer o;
      o << "Computing pairs with first = " << p->me << newline;
      emit(o.str());
    }
  M->divide(p->base_monom, p->first->base_monom, PAIRS_mon);
  M->to_varpower(PAIRS_mon, vp);

  // First add in syzygies arising from exterior variables
  // At the moment, there are none of this sort.

  if (P->is_skew_commutative())
    {
      int *exp = newarray_atomic(int, M->n_vars());
      varpower::to_ntuple(M->n_vars(), vp.raw(), exp);

      int nskew = P->n_skew_commutative_vars();
      for (int v = 0; v < nskew; v++)
        {
          int w = P->skew_variable(v);
          if (exp[w] > 0)
            {
              thisvp.shrink(0);
              varpower::var(w, 1, thisvp);
              Bag *b = new Bag(static_cast<void *>(0), thisvp);
              elems.insert(b);
            }
        }
      deletearray(exp);
    }

  // Second, add in syzygies arising from the base ring, if any
  // The baggage of each of these is NULL
  if (P->is_quotient_ring())
    {
      const MonomialIdeal *Rideal = P->get_quotient_monomials();
      for (j = Rideal->first(); j.valid(); j++)
        {
          // Compute (P->quotient_ideal->monom : p->monom)
          // and place this into a varpower and Bag, placing
          // that into 'elems'
          thisvp.shrink(0);
          varpower::quotient((*Rideal)[j]->monom().raw(), vp.raw(), thisvp);
          if (varpower::is_equal((*Rideal)[j]->monom().raw(), thisvp.raw()))
            continue;
          Bag *b = new Bag(static_cast<void *>(0), thisvp);
          elems.insert(b);
        }
    }

  // Third, add in syzygies arising from previous elements of this same level
  // The baggage of each of these is their corresponding res_pair

  MonomialIdeal *mi_orig = p->first->mi;
  for (j = mi_orig->first(); j.valid(); j++)
    {
      Bag *b = new Bag((*mi_orig)[j]->basis_ptr());
      varpower::quotient((*mi_orig)[j]->monom().raw(), vp.raw(), b->monom());
      elems.insert(b);
    }

  // Make this monomial ideal, and then run through each minimal generator
  // and insert into the proper degree. (Notice that sorting does not
  // need to be done yet: only once that degree is about to begin.

  mi_orig->insert_minimal(new Bag(p, vp));

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal *mi = new MonomialIdeal(P, elems, rejects);
  while (rejects.remove(b)) delete b;

  if (M2_gbTrace >= 11) mi->debug_out(1);

  for (j = mi->first(); j.valid(); j++)
    {
      res_pair *second = reinterpret_cast<res_pair *>((*mi)[j]->basis_ptr());
      res_pair *q = new_res_pair(SYZ_S_PAIR, p, second);
      // That set most fields except base_monom:
      M->from_varpower((*mi)[j]->monom().raw(), q->base_monom);
      M->mult(q->base_monom, p->base_monom, q->base_monom);
      result->next = q;
      result = q;
    }

  delete mi;
}

int res_comp::skeleton_maxdegree(const VECTOR(res_pair *)& reslevel)
{
  int result = lodegree;
  for (int level = 0; level < reslevel.size(); level++)
    {
      for (res_pair *p = reslevel[level]; p != NULL; p = p->next)
        {
          int d = degree(p);
          if (d - level > result) result = d - level;
        }
    }
  return result;
}

void res_comp::skeleton_stats(const VECTOR(res_pair *)& reslevel)
{
  buffer o;
  int level;
  int maxlevel = reslevel.size() - 1;
  int maxdegree = skeleton_maxdegree(reslevel);  // max slanted degree
  int *bettis = newarray_atomic_clear(int, (maxlevel + 1) * (maxdegree + 1));
  for (level = 0; level < reslevel.size(); level++)
    {
      for (res_pair *p = reslevel[level]; p != NULL; p = p->next)
        {
          int d = degree(p);
          d -= level;
          d -= lodegree;
          bettis[level + (maxlevel + 1) * d] += 1;
        }
    }

  // Now make an arrayint so that we may just use betti_display
  int lo = lodegree;
  int hi = maxdegree;
  int len = maxlevel;
  int totallen = 3 + (hi - lo + 1) * (len + 1);
  M2_arrayint betti = M2_makearrayint(totallen);

  betti->array[0] = lo;
  betti->array[1] = hi;
  betti->array[2] = len;
  int next = 3;

  for (int d = 0; d <= maxdegree - lodegree; d++)
    for (level = 0; level <= maxlevel; level++)
      betti->array[next++] = bettis[level + (maxlevel + 1) * d];

  betti_display(o, betti);
  deletearray(bettis);

  for (level = 0; level <= maxlevel; level++)
    {
      o << "---- level " << level << " ----" << newline;
      for (res_pair *p = reslevel[level]; p != NULL; p = p->next)
        {
          int d = degree(p);
          o.put(d, 4);
          o << ' ';
          text_out(o, p);
        }
    }
  emit(o.str());
}

void res_comp::skeleton(int strategy)
// Compute the skeleton of the resolution
// Currently: this is just used for debugging
{
  int level;
  VECTOR(res_pair *) reslevel;

  // First, set reslevel[0], reslevel[1].
  skeleton_init(reslevel);

  // Now loop through each level, until the length limit is hit,
  // or there are no new pairs

  for (level = 1; level < reslevel.size(); level++)
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
      for (res_pair *p = pp; p != NULL; p = p->next) skeleton_pairs(ptrhead, p);
      reslevel.push_back(head.next);
    }

  // Now display the skeleton and stats on it
  skeleton_stats(reslevel);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
