// Copyright 1996.  Michael E. Stillman

#include "res-a0-poly.hpp"
#include "res-a0.hpp"
#include "geobucket.hpp"
#include "buffer.hpp"
#include "text-io.hpp"
#include "interrupted.hpp"
#include "betti.hpp"

using respolyHeap = geobucket<const res2_poly, res2term *>;

bool res2_comp::stop_conditions_ok()
{
  if (stop_.length_limit != 0 && stop_.length_limit->len > 0)
    {
    }

  return true;
}

int res2_comp::complete_thru_degree() const
{
  int lo = hidegree + 1;
  int len = resn.size() - 1;
  for (int lev = 0; lev <= len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
        {
          if (p->syz_type != SYZ2_S_PAIR) continue;
          int d = p->degree;
          if (d < lo) lo = d;
        }
    }
  return lodegree + lo - 1;
}

enum ComputationStatusCode res2_comp::skeleton(int level)
// Compute the skeleton of the resolution
// returns COMP_COMPUTING or COMP_INTERRUPTED
{
  res2_pair *p;
  if (resn[level]->state != RES_SKELETON) return COMP_COMPUTING;
  // If we are new here, next_pairs will be null, so we should sort
  if (resn[level]->next_pair == NULL)
    {
      sort_skeleton(resn[level]->pairs);
      int n = 0;
      for (p = resn[level]->pairs; p != NULL; p = p->next)
        {
          p->me = n++;
          p->pair_num = p->me;
        }
      resn[level]->next_pair = resn[level]->pairs;
    }

  // Now compute the pairs at the next level
  for (;;)
    {
      p = resn[level]->next_pair;
      if (p == NULL) break;
      resn[level]->next_pair = p->next;
      // The following will only insert pairs of degree > topdegree
      // so this routine may be used to increase the degree bound
      // note: also need to redo monomial ideals...
      new_pairs(p);
      if (system_interrupted()) return COMP_INTERRUPTED;
    }
  resn[level]->state = RES_MONORDER;
  return COMP_COMPUTING;
}

void res2_comp::increase_level(int newmax)
{
  for (int i = resn.size(); i <= newmax + 2; i++)
    {
      res2_level *p = new res2_level;
      p->pairs = NULL;
      p->next_pair = NULL;
      p->state = RES_SKELETON;
      p->npairs = 0;
      p->nleft = 0;
      p->nminimal = 0;
      p->nthrown = 0;
      resn.push_back(p);
    }
  length_limit = newmax;
}

enum ComputationStatusCode res2_comp::do_all_pairs(int level, int degree)
{
  // First compute all pairs necessary for doing this (level,degree)
  // and then actually compute the ones in that degree.
  if (level <= 0 || level > length_limit + 1) return COMP_COMPUTING;
  //  if (degree <= 0 || degree > hidegree) return COMP_COMPUTING;
  if (level < resn.size() && (resn[level]->next_pair == NULL ||
                                degree < resn[level]->next_pair->degree))
    return COMP_COMPUTING;
  enum ComputationStatusCode ret = do_all_pairs(level, degree - 1);
  if (ret != COMP_COMPUTING) return ret;
  ret = do_all_pairs(level + 1, degree - 1);
  if (ret != COMP_COMPUTING) return ret;
  ret = do_pairs(level, degree);
  return ret;
}

enum ComputationStatusCode res2_comp::do_pairs(int level, int degree)
{
  // Find the first pair in this degree, and count the number of
  // pairs to compute in this degree.
  // If this number is 0, continue
  res2_pair *p;

  if (M2_gbTrace >= 2)
    {
      p = resn[level]->next_pair;
      int nelems = 0;
      while (p != NULL && p->degree == degree)
        {
          if (p->syz_type == SYZ2_S_PAIR || p->syz_type == SYZ2_MAYBE_MINIMAL)
            nelems++;
          p = p->next;
        }
      if (nelems > 0)
        {
          buffer o;
          o << "[" << degree << " " << level << " " << nelems << "]";
          emit(o.str());
        }
    }
  for (p = resn[level]->next_pair; p != NULL; p = p->next)
    {
      if (p->degree != degree) break;
      resn[level]->next_pair = p->next;
      if (p->syz_type == SYZ2_S_PAIR || p->syz_type == SYZ2_MAYBE_MINIMAL)
        {
          handle_pair(p);
          if (stop_.pair_limit > 0 && npairs - nleft >= stop_.pair_limit)
            return COMP_DONE_PAIR_LIMIT;
          // if (--PairLimit == 0) return COMP_DONE_PAIR_LIMIT;
          if (stop_.syzygy_limit > 0 && nminimal >= stop_.syzygy_limit)
            return COMP_DONE_SYZYGY_LIMIT;
        }
      if (system_interrupted()) return COMP_INTERRUPTED;
    }
  return COMP_COMPUTING;
}

enum ComputationStatusCode res2_comp::do_pairs_by_level(int level)
{
  // The pairs should be sorted in ascending monomial order

  res2_pair *p;

  if (M2_gbTrace >= 2)
    {
      int nelems = resn[level]->nleft;
      if (nelems > 0)
        {
          buffer o;
          o << "[lev " << nelems << ']';
          emit(o.str());
        }
    }
  for (p = resn[level]->next_pair; p != NULL; p = p->next)
    {
      resn[level]->next_pair = p->next;
      handle_pair_by_level(p);
      if (stop_.pair_limit > 0 && npairs - nleft >= stop_.pair_limit)
        return COMP_DONE_PAIR_LIMIT;
      //      if (--PairLimit == 0) return COMP_DONE_PAIR_LIMIT;
      if (system_interrupted()) return COMP_INTERRUPTED;
    }

  if (do_by_level == 2)
    {
      // The following is experimental
      // Remove any term that cannot appear as a lead term.
      // This destroys the resolution as we go, so in general, we would have
      // to place this information elsewhere.
      int nmonoms = 0;
      int nkilled = 0;
      for (p = resn[level]->pairs; p != NULL; p = p->next)
        {
          res2term *f = p->syz;
          res2term head;
          res2term *g = &head;
          for (f = p->syz; f != NULL; f = f->next)
            {
              if (f->comp->mi->length() > 0)
                {
                  g->next = R->new_term(K->copy(f->coeff), f->monom, f->comp);
                  g = g->next;
                  nmonoms++;
                }
              else
                nkilled++;
            }
          g->next = NULL;
          p->pivot_term = head.next;
        }
      if (M2_gbTrace >= 3)
        {
          buffer o;
          o << "[kept " << nmonoms << " killed " << nkilled << "]";
          emit(o.str());
        }
    }
  return COMP_COMPUTING;
}

enum ComputationStatusCode res2_comp::do_pairs_by_degree(int level, int degree)
{
  // Find the first pair in this degree, and count the number of
  // pairs to compute in this degree.
  // If this number is 0, continue
  res2_pair *p;

  if (M2_gbTrace >= 2 && level > 1)
    {
      p = resn[level]->next_pair;
      int nelems = 0;
      while (p != NULL && p->degree == degree)
        {
          nelems++;
          p = p->next;
        }
      if (nelems > 0)
        {
          buffer o;
          o << '[' << nelems << ']';
          emit(o.str());
        }
    }
  for (p = resn[level]->next_pair; p != NULL; p = p->next)
    {
      if (p->degree != degree) break;
      resn[level]->next_pair = p->next;
      if (p->syz_type == SYZ2_S_PAIR || p->syz_type == SYZ2_NOT_NEEDED ||
          (p->syz_type == SYZ2_MAYBE_MINIMAL && level < length_limit + 1))
        {
          handle_pair_by_degree(p);
          if (stop_.pair_limit > 0 && npairs - nleft >= stop_.pair_limit)
            return COMP_DONE_PAIR_LIMIT;
          // if (--PairLimit == 0) return COMP_DONE_PAIR_LIMIT;
          if (stop_.syzygy_limit > 0 && nminimal >= stop_.syzygy_limit)
            return COMP_DONE_SYZYGY_LIMIT;
        }
      if (system_interrupted()) return COMP_INTERRUPTED;
    }
  return COMP_COMPUTING;
}

#define DO(CALL)                              \
  {                                           \
    enum ComputationStatusCode result = CALL; \
    if (result != COMP_COMPUTING)             \
      {                                       \
        set_status(result);                   \
        return;                               \
      }                                       \
  }

void res2_comp::start_computation()
{
  int n, level;
  res2_pair *p;

  if (status() == COMP_DONE) return;
  set_status(COMP_COMPUTING);

  if (stop_.length_limit->len > 0)
    {
      if (length_limit < stop_.length_limit->array[0])
        // this also sets length_limit
        increase_level(stop_.length_limit->array[0]);
      else
        length_limit = stop_.length_limit->array[0];
    }
  int compute_level = COMPUTE_RES;

  for (level = 1; level <= length_limit + 1; level++) DO(skeleton(level));

  if (M2_gbTrace >= 3)
    {
      buffer o;
      o << "--- The total number of pairs in each level/slanted degree -----"
        << newline;
      M2_arrayint a = betti_skeleton();
      betti_display(o, a);
      emit(o.str());
    }

  // The skeleton routine sets the following:
  // hidegree: highest slanted degree found so far
  // a flag for each level as to whether the level has new elements
  //    since the last time the pairs were sorted for reduction

  if (compute_level <= COMPUTE_SKELETON)
    {
      set_status(COMP_DONE);
      return;
    }

  // Set the monomial order for each level, and then prepare for
  // reductions
  for (level = 1; level <= length_limit + 1; level++)
    {
      if (resn[level]->state != RES_MONORDER) continue;
      // The sort will use compare_num's from syz->comp->compare_num
      // and will use these numbers to break ties:
      for (n = 0, p = resn[level]->pairs; p != NULL; p = p->next, n++)
        p->compare_num = n;
      sort_monorder(resn[level]->pairs);
      for (n = 0, p = resn[level]->pairs; p != NULL; p = p->next, n++)
        p->compare_num = n;
      sort_reduction(resn[level]->pairs);
      resn[level]->state = RES_REDUCTIONS;
      resn[level]->next_pair = resn[level]->pairs;
    }

  for (level = 1; level <= length_limit + 1; level++)
    {
      resn[level]->next_pair = resn[level]->pairs;
    }

  // Here: possibly compute the resolution of the monomial ideal first.

  if (compute_level <= COMPUTE_MONOMIAL_RES)
    {
      set_status(COMP_DONE);
      return;
    }

  if (do_by_level != 0)
    {
      for (level = 1; level <= length_limit; level++)
        DO(do_pairs_by_level(level));
      set_status(COMP_DONE);
      return;
    }

  if (do_by_degree)
    {
      for (int deg = 0; deg <= hidegree; deg++)
        {
          if (stop_.stop_after_degree &&
              stop_.degree_limit->array[0] - lodegree < deg)
            //    if (DegreeLimit != NULL && deg > *DegreeLimit - lodegree)
            {
              set_status(COMP_DONE_DEGREE_LIMIT);
              return;
            }
          if (M2_gbTrace >= 1)
            {
              buffer o;
              o << '{' << deg + lodegree << '}';
              emit(o.str());
            }
          for (level = 1; level <= length_limit; level++)
            DO(do_pairs_by_degree(level, deg));
        }
      set_status(COMP_DONE);
      return;
    }

  // Now do all of the reductions
  for (int deg = 0; deg <= hidegree; deg++)
    {
      if (stop_.stop_after_degree &&
          stop_.degree_limit->array[0] - lodegree < deg)
        // if (DegreeLimit != NULL && deg > *DegreeLimit - lodegree)
        {
          set_status(COMP_DONE_DEGREE_LIMIT);
          return;
        }
      if (M2_gbTrace >= 1)
        {
          buffer o;
          o << '{' << deg + lodegree << '}';
          emit(o.str());
        }
      for (level = 1; level <= length_limit + 1; level++)
        {
          DO(do_all_pairs(level, deg));
          if (level > projdim) break;
        }
    }
#if 0
//   //This is the older, working version...
//   // Now do all of the reductions
//   for (int deg=0; deg<=hidegree; deg++)
//     {
//       if (DegreeLimit != NULL && deg > *DegreeLimit - lodegree)
//      {
//        set_status(COMP_DONE_DEGREE_LIMIT);
//        return;
//      }
//       if (M2_gbTrace >= 1)
//      {
//        buffer o;
//        o << '{' << deg+lodegree << '}';
//        emit(o.str());
//      }
//       for (level=1; level<=length_limit+1; level++)
//      {
//        DO(do_pairs(level,deg));
//      }
//     }
#endif
  set_status(COMP_DONE);
}

//////////////////////////////////////////////
//  Initialization of a computation  /////////
//////////////////////////////////////////////

void res2_comp::initialize(const Matrix *mat,
                           int LengthLimit,
                           bool UseDegreeLimit,
                           int SlantedDegreeLimit,
                           int SortStrategy)
{
  set_status(COMP_COMPUTING);
  P = mat->get_ring()->cast_to_PolynomialRing();
  assert(P != NULL);
  R = new res2_poly(const_cast<PolynomialRing *>(P));
  M = P->getMonoid();
  K = P->getCoefficientRing();
  generator_matrix = mat;

  exp_size = EXPONENT_BYTE_SIZE(P->n_vars());
  monom_size = MONOMIAL_BYTE_SIZE(M->monomial_size());

  res2_pair_stash = new stash("res2pair", sizeof(res2_pair));
  mi_stash = new stash("res2 minodes", sizeof(Nmi_node));

  length_limit = -3;  // The resolution is always kept at least in range
                      // 0 .. length_limit + 2.
  increase_level(LengthLimit);

  projdim = 0;
  max_mon_degree = M->max_degree();

  if (mat->n_rows() > 0)
    {
      lodegree = mat->rows()->primary_degree(0);
      for (auto i = 1; i < mat->n_rows(); i++)
        if (lodegree > mat->rows()->primary_degree(i))
          lodegree = mat->rows()->primary_degree(i);
    }
  else
    lodegree = 0;

  // This can't actually set lodegree, can it?
  for (auto i = 0; i < mat->n_cols(); i++)
    if (lodegree > mat->cols()->primary_degree(i) - 1)
      lodegree = mat->cols()->primary_degree(i) - 1;

  hidegree = 0;  // hidegree is an offset from 'lodegree'.

  have_degree_limit = UseDegreeLimit;
  hard_degree_limit = SlantedDegreeLimit;

  nleft = 0;
  npairs = 0;
  nminimal = 0;
  total_reduce_count = 0;

  n_ones = 0;
  n_unique = 0;
  n_others = 0;

  // Do level 0
  next_component = 0;
  for (auto i = 0; i < mat->n_rows(); i++)
    {
      res2_pair *p = new_base_res2_pair(i);
      base_components.push_back(p);
    }
  for (auto p = base_components.rbegin(); p != base_components.rend(); ++p)
    insert_pair(*p);

  // Do level 1
  for (auto i = 0; i < generator_matrix->n_cols(); i++)
    if ((*generator_matrix)[i] != NULL)
      {
        res2_pair *p = new_res2_pair(i);  // Makes a generator 'pair'
        insert_pair(p);
      }

  // Set variables for compare_res2_pairs
  compare_type = COMPARE_LEX;
  compare_use_degree = 0;
  compare_use_descending = 1;
  compare_use_reverse = 0;

  skeleton_sort = SortStrategy & 63;
  reduction_sort = (SortStrategy >> 6) & 63;
  reduction_sort |= FLAGS_DEGREE;  // ALWAYS do by increasing degree.
  do_by_level = (unsigned char)(SortStrategy & FLAGS_LEVEL ? 1 : 0);
  if (SortStrategy & FLAGS_LEVEL_STRIP) do_by_level = 2;
  do_by_degree = (unsigned char)(SortStrategy & FLAGS_DEGREELEVEL ? 1 : 0);
  auto_reduce = (SortStrategy & FLAGS_AUTO) >> SHIFT_AUTO;
  use_respolyHeaps = (unsigned char)(SortStrategy & FLAGS_GEO ? 1 : 0);

  if (do_by_degree) do_by_level = 0;
  if (M2_gbTrace >= 3)
    {
      buffer o;
      o << "auto-reduce level = " << auto_reduce << newline;
      if (do_by_level)
        {
          o << "computing resolution level by level" << newline;
          if (do_by_level == 2) o << "with strip optimization" << newline;
        }
      if (do_by_degree)
        o << "computing resolution degree by slanted degree" << newline;
      if (use_respolyHeaps) o << "using heap based reduction" << newline;
      o << "skeleton order = ";
      display_order(o, skeleton_sort);
      o << "reduction sort = ";
      display_order(o, reduction_sort);
      emit(o.str());
    }
}

void res2_comp::display_order(buffer &o, int sortval) const
{
  o << "[";
  switch (sortval & FLAGS_SORT)
    {
      case COMPARE_LEX:
        o << "LEX";
        break;
      case COMPARE_LEX_EXTENDED:
        o << "LEX1";
        break;
      case COMPARE_LEX_EXTENDED2:
        o << "LEX2";
        break;
      case COMPARE_ORDER:
        o << "ORDER";
        break;
      default:
        o << "bad order";
        break;
    }
  if (sortval & FLAGS_DEGREE) o << " (ascending degree first)";
  if (sortval & FLAGS_DESCEND)
    o << " descend";
  else
    o << " ascend";
  if ((sortval & FLAGS_REVERSE) && ((sortval & FLAGS_SORT) != COMPARE_ORDER))
    o << " reverse";
  o << "]" << newline;
}

res2_comp::res2_comp(const Matrix *m,
                     int LengthLimit,
                     bool UseDegreeLimit,
                     int SlantedDegreeLimit,
                     int SortStrategy)
{
  initialize(m, LengthLimit, UseDegreeLimit, SlantedDegreeLimit, SortStrategy);
}

void res2_comp::remove_res2_pair(res2_pair *p)
{
  if (p == NULL) return;
  R->remove(p->syz);
  delete p->mi;
  deleteitem(p);
}

void res2_comp::remove_res2_level(res2_level *lev)
{
  if (lev == NULL) return;
  while (lev->pairs != NULL)
    {
      res2_pair *p = lev->pairs;
      lev->pairs = p->next;
      remove_res2_pair(p);
    }
  deleteitem(lev);
}

res2_comp::~res2_comp()
{
  int i;
  for (i = 0; i < resn.size(); i++) remove_res2_level(resn[i]);

  delete res2_pair_stash;
  delete mi_stash;
  delete R;
}
//////////////////////////////////////////////
//  Data structure insertion and access  /////
//////////////////////////////////////////////

res2_pair *res2_comp::new_res2_pair(res2_pair *first,
                                    res2_pair * /* second */,
                                    const int *basemon)
{
  res2_pair *p = new res2_pair;
  p->next = NULL;
  p->me = next_component++;
  p->pair_num = p->me;
  p->syz_type = SYZ2_S_PAIR;
  p->level = (unsigned char)(first->level + 1);
  p->degree = (short unsigned int)(M->primary_degree(basemon) + first->degree -
                                   M->primary_degree(first->syz->monom) - 1);
  p->compare_num = 0;  // Will be set after pairs are done
  p->syz = R->new_term(K->from_long(1), basemon, first);
#if 0
//   if (second != NULL)
//     p->syz->next = R->new_term(K->from_long(-1), basemon, second);
#endif
  p->mi = new MonomialIdeal(P, mi_stash);
  p->pivot_term = NULL;

  return p;
}

res2_pair *res2_comp::new_base_res2_pair(int i)
{
  res2_pair *p = new res2_pair;
  p->next = NULL;
  p->me = next_component++;
  p->pair_num = p->me;
  p->syz_type = SYZ2_MINIMAL;
  p->level = 0;
  p->degree = (short unsigned int)(generator_matrix->rows()->primary_degree(i) -
                                   lodegree);
  p->compare_num = i;
  int *m = M->make_one();
  p->syz = R->new_term(K->from_long(1), m, p);  // circular link...
  M->remove(m);
  p->mi = new MonomialIdeal(P, mi_stash);
  p->pivot_term = NULL;
  return p;
}

res2_pair *res2_comp::new_res2_pair(int i)
{
  res2_pair *p = new res2_pair;
  p->next = NULL;
  p->me = next_component++;
  p->pair_num = p->me;
  p->syz_type = SYZ2_S_PAIR;
  p->level = 1;
  p->degree = (short unsigned int)(generator_matrix->cols()->primary_degree(i) -
                                   1 - lodegree);
  p->compare_num = 0;
  p->syz = R->from_vector(base_components, (*generator_matrix)[i]);
  p->mi = new MonomialIdeal(P, mi_stash);
  p->pivot_term = NULL;
  return p;
}

void res2_comp::insert_pair(res2_pair *q)
{
  // This is where we insert the pair:
  // If the degree is not in the range lodegree..hard_degree_limit+1
  // then don't insert it.
  if (q->level >= 1)
    {
      if (have_degree_limit)
        {
          if (q->degree + lodegree > hard_degree_limit + 1)
            {
              resn[q->level]->nthrown++;
              remove_res2_pair(q);
              return;
            }
          if (q->degree <= hard_degree_limit && q->degree > hidegree)
            hidegree = q->degree;
        }
      else if (q->degree > hidegree)
        hidegree = q->degree;
    }

  q->next = resn[q->level]->pairs;
  resn[q->level]->pairs = q;
  npairs++;
  resn[q->level]->npairs++;
  if (q->syz_type == SYZ2_S_PAIR)
    {
      nleft++;
      resn[q->level]->nleft++;
    }
  else
    {
      nminimal++;
      resn[q->level]->nminimal++;
    }
}

void res2_comp::multi_degree(const res2_pair *p, int *deg) const
{
  const res2_pair *q;
  M->multi_degree(p->syz->monom, deg);
  for (q = p; q->level != 0; q = q->syz->comp)
    ;
  degree_monoid()->mult(deg, generator_matrix->rows()->degree(q->me), deg);
}

//////////////////////////////////////////////
//  Sorting //////////////////////////////////
//////////////////////////////////////////////

int res2_comp::compare_res2_pairs(res2_pair *f, res2_pair *g) const
{
  // Lots of different orders appear here, controlled by the above
  // static variables.
  // if compare(f,g) returns -1, this says place g BEFORE f on the list.
  exponents EXP1, EXP2, EXP3, EXP4;
  int cmp, df, dg;

  if (compare_use_degree)
    {
      df = f->degree;
      dg = g->degree;
      if (df > dg) return -1;
      if (df < dg) return 1;
    }

  switch (compare_type)
    {
      case COMPARE_MONORDER:
        cmp = f->syz->comp->compare_num - g->syz->comp->compare_num;
        if (cmp < 0) return 1;
        if (cmp > 0)
          return -1;  // Lower compare num's should be on list earlier
        cmp = f->compare_num - g->compare_num;
        if (cmp < 0) return 1;
        if (cmp > 0) return -1;
        return 0;
      case COMPARE_LEX:
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        M->to_expvector(f->syz->monom, EXP1);
        M->to_expvector(g->syz->monom, EXP2);
        if (compare_use_reverse)
          {
            for (int i = M->n_vars() - 1; i >= 0; i--)
              {
                if (EXP1[i] < EXP2[i]) return -compare_use_descending;
                if (EXP1[i] > EXP2[i]) return compare_use_descending;
              }
          }
        else
          {
            for (int i = 0; i < M->n_vars(); i++)
              {
                if (EXP1[i] < EXP2[i]) return -compare_use_descending;
                if (EXP1[i] > EXP2[i]) return compare_use_descending;
              }
          }
      // Fall through to COMPARE_ORDER
      case COMPARE_ORDER:
        cmp = M->compare(f->syz->monom, g->syz->monom);
        if (cmp != 0) return compare_use_descending * cmp;
        cmp = f->syz->comp->compare_num - g->syz->comp->compare_num;
        if (cmp < 0) return -compare_use_descending;
        if (cmp > 0) return compare_use_descending;
        return 0;
      case COMPARE_LEX_EXTENDED:
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        while (f->level >= 1)
          {
            M->to_expvector(f->syz->monom, EXP1);
            M->to_expvector(g->syz->monom, EXP2);
            if (compare_use_reverse)
              {
                for (int i = M->n_vars() - 1; i >= 0; i--)
                  {
                    if (EXP1[i] < EXP2[i]) return -compare_use_descending;
                    if (EXP1[i] > EXP2[i]) return compare_use_descending;
                  }
              }
            else
              {
                for (int i = 0; i < M->n_vars(); i++)
                  {
                    if (EXP1[i] < EXP2[i]) return -compare_use_descending;
                    if (EXP1[i] > EXP2[i]) return compare_use_descending;
                  }
              }
            // Go down one level
            f = f->syz->comp;
            g = g->syz->comp;
          }
        return 0;
      case COMPARE_LEX_EXTENDED2:
        EXP1 = ALLOCATE_EXPONENTS(exp_size);
        EXP2 = ALLOCATE_EXPONENTS(exp_size);
        EXP3 = ALLOCATE_EXPONENTS(exp_size);
        EXP4 = ALLOCATE_EXPONENTS(exp_size);
        M->to_expvector(f->syz->monom, EXP1);
        M->to_expvector(g->syz->monom, EXP2);
        if (compare_use_reverse)
          {
            for (int i = M->n_vars() - 1; i >= 0; i--)
              {
                if (EXP1[i] < EXP2[i]) return -compare_use_descending;
                if (EXP1[i] > EXP2[i]) return compare_use_descending;
              }
          }
        else
          {
            for (int i = 0; i < M->n_vars(); i++)
              {
                if (EXP1[i] < EXP2[i]) return -compare_use_descending;
                if (EXP1[i] > EXP2[i]) return compare_use_descending;
              }
          }
        while (f->level >= 1)
          {
            // Go down one level
            M->to_expvector(f->syz->monom, EXP1);
            M->to_expvector(g->syz->monom, EXP2);
            f = f->syz->comp;
            g = g->syz->comp;
            M->to_expvector(f->syz->monom, EXP3);
            M->to_expvector(g->syz->monom, EXP4);
            if (compare_use_reverse)
              {
                for (int i = M->n_vars() - 1; i >= 0; i--)
                  {
                    if (EXP1[i] - EXP3[i] < EXP2[i] - EXP4[i])
                      return -compare_use_descending;
                    if (EXP1[i] - EXP3[i] > EXP2[i] - EXP4[i])
                      return compare_use_descending;
                  }
              }
            else
              {
                for (int i = 0; i < M->n_vars(); i++)
                  {
                    if (EXP1[i] - EXP3[i] < EXP2[i] - EXP4[i])
                      return -compare_use_descending;
                    if (EXP1[i] - EXP3[i] > EXP2[i] - EXP4[i])
                      return compare_use_descending;
                  }
              }
          }
        return 0;
      default:
        return 0;
    }
}

res2_pair *res2_comp::merge_res2_pairs(res2_pair *f, res2_pair *g) const
{
  if (g == NULL) return f;
  if (f == NULL) return g;
  res2_pair head;
  res2_pair *result = &head;
  while (1) switch (compare_res2_pairs(f, g))
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

void res2_comp::sort_res2_pairs(res2_pair *&p) const
{
  // These elements are sorted in ascending 'me' values
  if (p == NULL || p->next == NULL) return;
  res2_pair *p1 = NULL;
  res2_pair *p2 = NULL;
  while (p != NULL)
    {
      res2_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_res2_pairs(p1);
  sort_res2_pairs(p2);
  p = merge_res2_pairs(p1, p2);
}

void res2_comp::sort_skeleton(res2_pair *&p)
{
  compare_type = (skeleton_sort & FLAGS_SORT);
  compare_use_descending = (skeleton_sort & FLAGS_DESCEND ? 1 : -1);
  compare_use_reverse = (skeleton_sort & FLAGS_REVERSE);
  compare_use_degree = (skeleton_sort & FLAGS_DEGREE);
  sort_res2_pairs(p);
}
void res2_comp::sort_monorder(res2_pair *&p)
{
  // Rest of the compare_* don't matter in this case, except degree
  compare_type = COMPARE_MONORDER;
  compare_use_degree = 0;
  sort_res2_pairs(p);
}
void res2_comp::sort_reduction(res2_pair *&p)
{
  compare_type = (reduction_sort & FLAGS_SORT);
  compare_use_descending = (reduction_sort & FLAGS_DESCEND ? 1 : -1);
  compare_use_reverse = (reduction_sort & FLAGS_REVERSE);
  compare_use_degree = 1;
  sort_res2_pairs(p);
}

int res2_comp::sort_value(res2_pair *p, const int *sort_order) const
{
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  M->to_expvector(p->syz->monom, REDUCE_exp);
  int result = 0;
  for (int i = 0; i < P->n_vars(); i++) result += REDUCE_exp[i] * sort_order[i];
  return result;
}

//////////////////////////////////////////////
//  Creation of new pairs ////////////////////
//////////////////////////////////////////////

void res2_comp::new_pairs(res2_pair *p)
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
      o << "Computing pairs with first = " << p->pair_num << newline;
      emit(o.str());
    }
  M->divide(p->syz->monom, p->syz->comp->syz->monom, PAIRS_mon);
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
  // The baggage of each of these is their corresponding res2_pair

  MonomialIdeal *mi_orig = p->syz->comp->mi;
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
  MonomialIdeal mi(P, elems, rejects);
  while (rejects.remove(b)) delete b;

  if (M2_gbTrace >= 11) mi.debug_out(1);

  int *m = M->make_one();
  for (j = mi.first(); j.valid(); j++)
    {
      res2_pair *second = reinterpret_cast<res2_pair *>(mi[j]->basis_ptr());
      M->from_varpower(mi[j]->monom().raw(), m);
      M->mult(m, p->syz->monom, m);

      res2_pair *q = new_res2_pair(p, second, m);
      insert_pair(q);
    }
}
//////////////////////////////////////////////
//  S-pairs and reduction ////////////////////
//////////////////////////////////////////////

int res2_comp::find_ring_divisor(const int *exp, ring_elem &result) const
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

int res2_comp::find_divisor(const MonomialIdeal *mi,
                            const int *exp,
                            res2_pair *&result)
{
  // Find all the possible matches, use some criterion for finding the best...
  res2_pair *p;
  VECTOR(Bag *) bb;
  mi->find_all_divisors(exp, bb);
  if (bb.size() == 0) return 0;
  result = reinterpret_cast<res2_pair *>((bb[0]->basis_ptr()));
  // Now search through, and find the best one.  If only one, just return it.
  if (M2_gbTrace >= 5)
    if (mi->length() > 1)
      {
        buffer o;
        o << ":" << mi->length() << "." << bb.size() << ":";
        emit(o.str());
      }
  if (bb.size() == 1)
    {
      if (mi->length() == 1)
        n_ones++;
      else
        n_unique++;
      return 1;
    }
  n_others++;
  //  int n = R->n_terms(result->syz);

  unsigned int lowest = result->pair_num;
  for (int i = 1; i < bb.size(); i++)
    {
      p = reinterpret_cast<res2_pair *>(bb[i]->basis_ptr());
      if (p->pair_num < lowest)
        {
          lowest = p->pair_num;
          result = p;
        }
#if 0
//       int nt = R->n_terms(p->syz);
//       if (nt < n)
//      {
//        n = nt;
//        result = p;
//      }
#endif
    }
  return 1;
}

res2term *res2_comp::s_pair(res2term *f) const
{
  res2term *result = NULL;
  int *si = M->make_one();
  while (f != NULL)
    {
      M->divide(f->monom, f->comp->syz->monom, si);
      res2term *h = R->mult_by_term(f->comp->syz, f->coeff, si);
      R->add_to(result, h);
      //      R->subtract_multiple_to(result, f->coeff, si, f->comp->syz);
      f = f->next;
    }
  M->remove(si);
  return result;
}

res2_pair *res2_comp::reduce(res2term *&f,
                             res2term *&fsyz,
                             res2term *&pivot,
                             res2_pair *)
// Reduce f, placing the reduction history in fsyz.
// Returns NULL if f reduces to 0, otherwise
// returns the res2_pair at the previous level (f will "fill in" that pair), and
// place a pointer to the corresponding term in "pivot".
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  res2term *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);

  res2_pair *q;
  ring_elem rg;
  //  Bag *b;

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while (f != NULL)
    {
      M->divide(f->monom, f->comp->syz->monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
        {
          // Subtract off f, leave fsyz alone
          Nterm *r = rg;
          M->divide(f->monom, r->monom, REDUCE_mon);
          R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
          total_reduce_count++;
          count++;
        }
      //      else if (f->comp->mi.search_expvector(REDUCE_exp, b))
      else if (find_divisor(f->comp->mi, REDUCE_exp, q))
        {
          // q = (res2_pair *) (b->basis_ptr());
          lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
          lastterm = lastterm->next;
          pivot = lastterm;
          if (q->syz_type == SYZ2_S_PAIR || q->syz_type == SYZ2_MAYBE_MINIMAL)
            {
              if (M2_gbTrace >= 4)
                {
                  buffer o;
                  o << count;
                  emit_wrapped(o.str());
                }
              return q;  // i.e. not computed yet
            }
          M->divide(f->monom, q->syz->monom, REDUCE_mon);
          R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
          total_reduce_count++;
          count++;
        }
      else
        {
          res2term *tmp = f;
          f = f->next;
          tmp->next = NULL;
          R->remove(tmp);
        }
    }
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
  return NULL;
}

res2_pair *res2_comp::reduce2(res2term *&f,
                              res2term *&fsyz,
                              res2term *&pivot,
                              res2_pair *)
// Reduce f, placing the reduction history in fsyz.
// Returns NULL if f reduces to 0, otherwise
// returns the res2_pair at the previous level (f will "fill in" that pair), and
// place a pointer to the corresponding term in "pivot".
// 'p' is just here for auto-reduction...
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  res2term *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);
  res2term head;
  res2term *red = &head;
  res2_pair *result = NULL;
  res2_pair *q;
  ring_elem rg;

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while (f != NULL)
    {
      M->divide(f->monom, f->comp->syz->monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
        {
          // Subtract off f, leave fsyz alone
          Nterm *r = rg;
          M->divide(f->monom, r->monom, REDUCE_mon);
          R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
          total_reduce_count++;
          count++;
        }
      else if (find_divisor(f->comp->mi, REDUCE_exp, q))
        {
          if (q->syz_type == SYZ2_S_PAIR || q->syz_type == SYZ2_MAYBE_MINIMAL)
            {
              if (result == NULL || auto_reduce >= 2)
                {
                  lastterm->next =
                      R->new_term(K->negate(f->coeff), f->monom, q);
                  lastterm = lastterm->next;
                  if (result == NULL)
                    {
                      // Only do this for the first non-computed pair
                      pivot = lastterm;
                      result = q;
                    }
                  else if (auto_reduce == 2)
                    {
                      // Keep track of this element for later reduction
                      // Store it with q.  Now for a bit of a hack:
                      // We store it in the 'pivot_term' field, since
                      // it cannot have been set yet, and since we don't want
                      // to waste space.
                      auto_reduce_node *au = new auto_reduce_node;
                      au->next =
                          reinterpret_cast<auto_reduce_node *>(q->pivot_term);
                      au->p = pivot->comp;
                      au->pivot = lastterm;
                      q->pivot_term = reinterpret_cast<res2term *>(au);
                    }
                  if (auto_reduce == 0)
                    {
                      // Time to leave: 'red' has nothing in it
                      // with this option, and 'f' is set correctly.
                      return result;
                    }
                }
              red->next = f;
              red = red->next;
              f = f->next;
              continue;
            }
          else
            {
              lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
              lastterm = lastterm->next;
              M->divide(f->monom, q->syz->monom, REDUCE_mon);
              R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
              total_reduce_count++;
              count++;
            }
        }
      else
        {
          red->next = f;
          red = red->next;
          f = f->next;
        }
    }
  red->next = NULL;
  f = head.next;
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
  return result;
}

// The 'respolyHeap' version of reduction

res2_pair *res2_comp::reduce3(res2term *&f,
                              res2term *&fsyz,
                              res2term *&pivot,
                              res2_pair *)
// Reduce f, placing the reduction history in fsyz.
// Returns NULL if f reduces to 0, otherwise
// returns the res2_pair at the previous level (f will "fill in" that pair), and
// place a pointer to the corresponding term in "pivot".
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  res2term *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);
  res2term head;
  res2term *red = &head;
  res2_pair *result = NULL;
  res2_pair *q;
  ring_elem rg;
  respolyHeap fb(R);  // No bucket is needed for fsyz, since we
                      // only append elements to the end of fsyz.
  fb.add(f);
  f = NULL;
  res2term *lead;
  //  Bag *b;

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while ((lead = fb.remove_lead_term()) != NULL)
    {
      M->divide(lead->monom, lead->comp->syz->monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
        {
          // Subtract off f, leave fsyz alone
          Nterm *r = rg;
          M->divide(lead->monom, r->monom, REDUCE_mon);
          ring_elem c = K->negate(lead->coeff);
          res2term *h =
              R->ring_mult_by_term(r->next, c, REDUCE_mon, lead->comp);
          R->remove(lead);
          K->remove(c);
          fb.add(h);
          total_reduce_count++;
          count++;
        }
      else if (find_divisor(lead->comp->mi, REDUCE_exp, q))
        {
          // q = (res2_pair *) (b->basis_ptr());
          if (q->syz_type == SYZ2_S_PAIR || q->syz_type == SYZ2_MAYBE_MINIMAL)
            {
              if (result == NULL)
                {
                  lastterm->next =
                      R->new_term(K->negate(lead->coeff), lead->monom, q);
                  lastterm = lastterm->next;
                  pivot = lastterm;
                  result = q;
                }
              red->next = lead;
              red = red->next;
              continue;
            }
          else
            {
              ring_elem c = K->negate(lead->coeff);
              M->divide(lead->monom, q->syz->monom, REDUCE_mon);
              res2term *h = R->mult_by_term(q->syz->next, c, REDUCE_mon);
              lastterm->next = R->new_term(c, lead->monom, q);
              lastterm = lastterm->next;
              R->remove(lead);
              fb.add(h);
              total_reduce_count++;
              count++;
            }
        }
      else
        {
          red->next = lead;
          red = red->next;
        }
    }
  red->next = NULL;
  f = head.next;
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
  return result;
}

res2_pair *res2_comp::reduce4(res2term *&f,
                              res2term *&fsyz,
                              res2term *&pivot,
                              res2_pair *p)
// Reduce f, placing the reduction history in fsyz.
// Returns NULL if f reduces to 0, otherwise
// returns the res2_pair at the previous level (f will "fill in" that pair), and
// place a pointer to the corresponding term in "pivot".
// 'p' is just here for auto-reduction...
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  res2term *lastterm = fsyz;
  while (lastterm->next != NULL) lastterm = lastterm->next;

  res2term head;
  res2term *red = &head;
  res2_pair *result = NULL;
  res2_pair *q;
  ring_elem rg;

  int count = total_reduce_count;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while (f != NULL)
    {
      res2term *lead = f;
      f = f->next;
      lead->next = NULL;
      M->divide(lead->monom, lead->comp->syz->monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
        {
          // Subtract off f, leave fsyz alone
          Nterm *r = rg;
          M->divide(lead->monom, r->monom, REDUCE_mon);
          ring_elem c = K->negate(lead->coeff);
          res2term *h =
              R->ring_mult_by_term(r->next, c, REDUCE_mon, lead->comp);
          R->remove(lead);
          K->remove(c);
          R->add_to(f, h);
          total_reduce_count++;
        }
      else if (find_divisor(lead->comp->mi, REDUCE_exp, q))
        {
          if (q->degree == p->degree + 1)
            // if (q->syz_type == SYZ2_S_PAIR)
            {
              lastterm->next =
                  R->new_term(K->negate(lead->coeff), lead->monom, q);
              lastterm = lastterm->next;
              if (result == NULL && q->syz_type == SYZ2_S_PAIR)
                {
                  // Only do this for the first non-computed pair
                  // Question: do we really need to keep this information
                  // around?
                  pivot = lastterm;
                  result = q;
                }
              red->next = lead;
              red = red->next;
              continue;
            }
          else
            {
              ring_elem c = K->negate(lead->coeff);
              M->divide(lead->monom, q->syz->monom, REDUCE_mon);
              res2term *h = R->mult_by_term(q->syz->next, c, REDUCE_mon);
              lastterm->next = R->new_term(c, lead->monom, q);  // grabs 'c'.
              lastterm = lastterm->next;
              R->remove(lead);
              R->add_to(f, h);
              total_reduce_count++;
            }
        }
      else
        {
          red->next = lead;
          red = red->next;
        }
    }
  red->next = NULL;
  f = head.next;
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << (total_reduce_count - count);
      emit_wrapped(o.str());
    }
  return result;
}

res2_pair *res2_comp::reduce_by_level(res2term *&f, res2term *&fsyz)
// Reduce f, placing the reduction history in fsyz.
// Returns NULL if f reduces to 0, otherwise
// returns the res2_pair at the previous level (f will "fill in" that pair), and
// place a pointer to the corresponding term in "pivot".
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  res2term *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);

  res2_pair *q;
  ring_elem rg;

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while (f != NULL)
    {
      M->divide(f->monom, f->comp->syz->monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
        {
          // Subtract off f, leave fsyz alone
          Nterm *r = rg;
          M->divide(f->monom, r->monom, REDUCE_mon);
          R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
          total_reduce_count++;
          count++;
        }
      else if (find_divisor(f->comp->mi, REDUCE_exp, q))
        {
          lastterm->next = R->new_term(K->negate(f->coeff), f->monom, q);
          lastterm = lastterm->next;
          M->divide(f->monom, q->syz->monom, REDUCE_mon);
          R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->pivot_term);
          total_reduce_count++;
          count++;
        }
      else
        {
          res2term *tmp = f;
          f = f->next;
          tmp->next = NULL;
          R->remove(tmp);
        }
    }
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
  return NULL;
}

res2_pair *res2_comp::reduce_heap_by_level(res2term *&f, res2term *&fsyz)
{
  // 'lastterm' is used to append the next monomial to fsyz->syz
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  res2term *lastterm = (fsyz->next == NULL ? fsyz : fsyz->next);
  res2_pair *q;
  ring_elem rg;
  respolyHeap fb(R);  // No bucket is needed for fsyz, since we
                      // only append elements to the end of fsyz.
  fb.add(f);
  f = NULL;
  res2term *lead;

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while ((lead = fb.remove_lead_term()) != NULL)
    {
      M->divide(lead->monom, lead->comp->syz->monom, REDUCE_mon);
      M->to_expvector(REDUCE_mon, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, rg))
        {
          // Subtract off f, leave fsyz alone
          Nterm *r = rg;
          M->divide(lead->monom, r->monom, REDUCE_mon);
          ring_elem c = K->negate(lead->coeff);
          res2term *h =
              R->ring_mult_by_term(r->next, c, REDUCE_mon, lead->comp);
          R->remove(lead);
          K->remove(c);
          fb.add(h);
          total_reduce_count++;
          count++;
        }
      else if (find_divisor(lead->comp->mi, REDUCE_exp, q))
        {
          ring_elem c = K->negate(lead->coeff);
          M->divide(lead->monom, q->syz->monom, REDUCE_mon);
          res2term *h = R->mult_by_term(q->pivot_term->next, c, REDUCE_mon);
          lastterm->next = R->new_term(c, lead->monom, q);
          lastterm = lastterm->next;
          R->remove(lead);
          fb.add(h);
          total_reduce_count++;
          count++;
        }
      else
        {
          R->remove(lead);
        }
    }
  f = NULL;
  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
  return NULL;
}

//////////////////////////////////////////////
//  Toplevel calculation and state machine ///
//////////////////////////////////////////////

void res2_comp::do_auto_reductions(res2_pair *p, auto_reduce_node *au)
// For each node in 'au', remove the specified multiple of 'p->syz'.
{
  buffer o;
  while (au != NULL)
    {
      auto_reduce_node *a = au;
      au = au->next;
      o << "auto reduction: " << newline << "    ";
      R->elem_text_out(p->syz);
      o << newline << "    ";
      R->elem_text_out(a->p->syz);
      o << newline << "    by coeff = ";
      K->elem_text_out(o, a->pivot->coeff);
      ring_elem c = K->negate(a->pivot->coeff);
      res2term *h = R->mult_by_coefficient(p->syz, c);
      K->remove(c);
      R->add_to(a->p->syz, h);
      o << newline << "    result = ";
      R->elem_text_out(a->p->syz);
      o << newline;
      deleteitem(a);
    }
  emit(o.str());
}

void res2_comp::handle_pair(res2_pair *p)
{
  nleft--;
  resn[p->level]->nleft--;

  // For level 1: any non-marked GB elements in this degree are
  // minimal generators, so we need to mark them as such
  if (p->level == 1)
    {
      p->syz_type = SYZ2_MINIMAL;
      if (M2_gbTrace >= 2) emit_wrapped("z");
      if (projdim == 0) projdim = 1;
      nminimal++;
      return;
    }

  res2term *f = s_pair(p->syz);
  res2_pair *q;

  // This is used only for full auto reduction type 2.
  auto_reduce_node *au = reinterpret_cast<auto_reduce_node *>(p->pivot_term);

  if (use_respolyHeaps)
    q = reduce3(f, p->syz, p->pivot_term, p);
  else if (auto_reduce >= 1)
    q = reduce2(f, p->syz, p->pivot_term, p);
  else
    q = reduce(f, p->syz, p->pivot_term, p);

  // Auto reduction: here is where we 'modify' the
  // other elements by 'p'.
  if (auto_reduce == 2)
    {
      do_auto_reductions(p, au);
    }
  else if (auto_reduce == 3)
    {
    }

  if (f == NULL)
    {
      // minimal syzygy
      if (p->level == length_limit + 1)
        {
          p->syz_type = SYZ2_MAYBE_MINIMAL;
        }
      else
        {
          p->syz_type = SYZ2_MINIMAL;
          nminimal++;
          if (p->level > projdim) projdim = p->level;
        }
      if (M2_gbTrace >= 2) emit_wrapped("z");
    }
  else
    {
      R->make_monic(f);
      p->syz_type = SYZ2_NOT_MINIMAL;

      // non-minimal syzygy
      R->remove(q->syz);
      q->syz = f;
      q->syz_type = SYZ2_NOT_NEEDED;

      // Auto reduction: here is where we modify the
      // other elements by 'q'.
      if (auto_reduce == 2)
        {
          do_auto_reductions(
              q, reinterpret_cast<auto_reduce_node *>(q->pivot_term));
          q->pivot_term = NULL;
        }
      else if (auto_reduce == 3)
        {
        }

      if (M2_gbTrace >= 2) emit_wrapped("m");
    }
}

void res2_comp::handle_pair_by_level(res2_pair *p)
{
  nleft--;
  resn[p->level]->nleft--;

  // level 1 is easy: just mark as minimal
  if (p->level == 1)
    {
      p->syz_type = SYZ2_MINIMAL;
      if (M2_gbTrace >= 2) emit_wrapped("z");
      return;
    }

  res2term *f = s_pair(p->syz);
  if (do_by_level == 2)
    {
      if (use_respolyHeaps)
        reduce_heap_by_level(f, p->syz);
      else
        reduce_by_level(f, p->syz);
    }
  else
    reduce(f, p->syz, p->pivot_term, p);

  if (f == NULL)
    {
      p->syz_type = SYZ2_MINIMAL;
      if (M2_gbTrace >= 2) emit_wrapped("z");
    }
  else
    {
      // This should not happen at all!!
      buffer o;
      o << "handle pair: should not be here!";
      o << "p->syz == ";
      R->elem_text_out(o, p->syz);
      emit(o.str());
    }
}

void res2_comp::handle_pair_by_degree(res2_pair *p)
{
  nleft--;
  resn[p->level]->nleft--;

  // For level 1: any non-marked GB elements in this degree are
  // minimal generators, so we need to mark them as such
  if (p->level == 1)
    {
      if (p->syz_type != SYZ2_NOT_NEEDED)
        {
          p->syz_type = SYZ2_MINIMAL;
          if (M2_gbTrace >= 2) emit_wrapped("z");
          nminimal++;
        }
      return;
    }

  res2term *f = s_pair(p->syz);
  res2_pair *q = reduce4(f, p->syz, p->pivot_term, p);
  // In this version of 'reduce', the resulting value of 'f' is irrelevant.
  // And in fact the routine should probably read:
  //  res2_pair *q = reduce4(p->syz, p->pivot_term);
  if (q == NULL)
    {
      if (p->syz_type != SYZ2_NOT_NEEDED)
        {
          // minimal syzygy
          p->syz_type = SYZ2_MINIMAL;
          nminimal++;
          resn[p->level]->nminimal++;
          if (M2_gbTrace >= 2) emit_wrapped("z");
        }
      else
        {
          if (M2_gbTrace >= 2) emit_wrapped("o");
        }
    }
  else
    {
      p->syz_type = SYZ2_NOT_MINIMAL;
      q->syz_type = SYZ2_NOT_NEEDED;
      if (M2_gbTrace >= 2) emit_wrapped("m");
    }
}

//////////// res-a0-aux /////////////////////
#include "matrix-con.hpp"

M2_arrayint res2_comp::betti_skeleton() const
{
  int lo = lodegree;
  int hi = lo + hidegree;
  int len = resn.size() - 1;
  BettiDisplay B(lo, hi, len);

  for (int lev = 0; lev <= len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
        {
          int d = p->degree;
          B.entry(d, lev)++;
        }
    }
  return B.getBetti();
}

M2_arrayint res2_comp::betti_remaining() const
{
  int lo = lodegree;
  int hi = lo + hidegree;
  int len = resn.size() - 1;
  int *bettis;
  betti_init(lo, hi, len, bettis);
  for (int lev = 0; lev <= len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
        {
          if (p->syz_type != SYZ2_S_PAIR) continue;
          int d = p->degree;
          bettis[lev + (len + 1) * d]++;
        }
    }
  M2_arrayint result = betti_make(lo, hi, len, bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::betti_minimal() const
// Negative numbers represent upper bounds
{
  int lo = lodegree;
  int hi = lo + hidegree;
  int len = resn.size() - 1;
  int *bettis;
  betti_init(lo, hi, len, bettis);
  for (int lev = 0; lev <= len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
        {
          if (p->syz_type != SYZ2_MINIMAL) continue;
          int d = p->degree;
          bettis[lev + (len + 1) * d]++;
        }
    }
  M2_arrayint result = betti_make(lo, hi, len, bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::betti_nmonoms() const
{
  int lo = lodegree;
  int hi = lo + hidegree;
  int len = resn.size() - 1;
  int *bettis;
  betti_init(lo, hi, len, bettis);
  for (int lev = 0; lev <= len; lev++)
    {
      for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
        {
          int d = p->degree;
          bettis[lev + (len + 1) * d] += R->n_terms(p->syz);
        }
    }
  M2_arrayint result = betti_make(lo, hi, len, bettis);
  deletearray(bettis);
  return result;
}

M2_arrayint res2_comp::get_betti(int type) const
{
  switch (type)
    {
      case 0:
        return betti_minimal();
      case 1:
        return betti_skeleton();
      case 2:
        return betti_remaining();
      case 3:
        return betti_nmonoms();
      case 4:
        ERROR(
            "cannot use Minimize=>true unless res(...,FastNonminimal=>true) "
            "was used");
        return 0;
    }
  ERROR("received unknown betti type");
  return 0;
}

void res2_comp::text_out(const res2_pair *p) const
{
  buffer o;
  text_out(o, p);
  emit(o.str());
}
void res2_comp::text_out(buffer &o, const res2_pair *p) const
{
  res2_pair *a, *b;

  a = p->syz->comp;
  if (p->syz->next == NULL)
    b = NULL;
  else
    b = p->syz->next->comp;

  o << p->me << ' ';
  o << p->level << ' ' << p->degree << ' ';
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
      case SYZ2_S_PAIR:
        o << "PR";
        break;
      case SYZ2_MINIMAL:
        o << "SZ";
        break;
      case SYZ2_NOT_MINIMAL:
        o << "GB";
        o << "(pivot " << p->pivot_term->comp->me << ")";
        break;
      case SYZ2_NOT_NEEDED:
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
//       res2_pair *q = p->next_div;
//       int n = 0;
//       while (q != NULL) { n++; q = q->next_div; }
//       o << "[midiv: " << n << "]";
//     }
#endif
  M->elem_text_out(o, p->syz->monom);
  o << " [" << R->n_terms(p->syz) << "] ";
  if (M2_gbTrace >= 4)
    {
      // Display the vector
      o << " syz: ";
      R->elem_text_out(o, p->syz);
    }
  o << newline;
}

void res2_comp::stats() const
{
  buffer o;
  text_out(o);
  emit(o.str());
}

void res2_comp::text_out(buffer &o) const
{
  o << "--- The total number of pairs in each level/slanted degree -----"
    << newline;
  M2_arrayint a = betti_skeleton();
  betti_display(o, a);
  //  o << "--- The number of pairs left to compute ------------------------" <<
  //  newline;
  //  a.shrink(0);
  //  betti_remaining(a);
  //  betti_display(o, a);
  o << "--- (Lower bounds of) the minimal betti numbers ----------" << newline;
  a = betti_minimal();
  betti_display(o, a);
  if (M2_gbTrace >= 1)
    {
      o << "-- #Reduction steps = " << total_reduce_count << "--"
        << " ones " << n_ones << " unique " << n_unique << " others "
        << n_others << " ----" << newline;
      o << "--- Number of monomials  ---------------------------------"
        << newline;
      a = betti_nmonoms();
      betti_display(o, a);
    }

  // If the printlevel is high enough, display each element
  if (M2_gbTrace >= 2)
    for (int lev = 0; lev < resn.size(); lev++)
      {
        if (resn[lev]->pairs == NULL) continue;
        o << "---- level " << lev << " ----" << newline;
        for (res2_pair *p = resn[lev]->pairs; p != NULL; p = p->next)
          text_out(o, p);
      }
}

FreeModule *res2_comp::free_of(int i) const
{
  FreeModule *result;
  result = P->make_Schreyer_FreeModule();
  if (i < 0 || i >= resn.size()) return result;

  int *deg = degree_monoid()->make_one();
  int n = 0;
  for (res2_pair *p = resn[i]->pairs; p != NULL; p = p->next)
    {
      multi_degree(p, deg);
      result->append_schreyer(deg, p->syz->monom, p->compare_num);
      p->me = n++;
    }
  degree_monoid()->remove(deg);
  return result;
}
FreeModule *res2_comp::minimal_free_of(int i) const
{
  FreeModule *result;
  result = P->make_Schreyer_FreeModule();
  if (i < 0 || i >= resn.size() - 1) return result;
  if (do_by_level > 0) return free_of(i);

  int *deg = degree_monoid()->make_one();
  int n = 0;
  for (res2_pair *p = resn[i]->pairs; p != NULL; p = p->next)
    if (p->syz_type == SYZ2_MINIMAL)
      {
        multi_degree(p, deg);
        result->append_schreyer(deg, p->syz->monom, p->compare_num);
        p->me = n++;
      }
  degree_monoid()->remove(deg);
  return result;
}

Matrix *res2_comp::make(int level) const
{
  const FreeModule *F = free_of(level - 1);
  const FreeModule *G = free_of(level);
  MatrixConstructor result(F, G, NULL);
  //  Matrix *result = new Matrix(free_of(level-1), free_of(level));

  int n = 0;
  if (G->rank() == 0) return result.to_matrix();
  for (res2_pair *p = resn[level]->pairs; p != NULL; p = p->next)
    result.set_column(n++, R->to_vector(p->syz, F));
  return result.to_matrix();
}

//////////////////////////////////////////////
//  Minimal resolutions //////////////////////
//////////////////////////////////////////////

void res2_comp::reduce_minimal(int x,
                               res2term *&f,
                               VECTOR(res2_pair *)& elems,
                               VECTOR(res2term *)& stripped) const
{
  // Reduce any components of 'f' that correspond to non minimal syzygies.
  monomial MINIMAL_mon = ALLOCATE_MONOMIAL(monom_size);
  const res2term *tm;

  for (int i = x - 1; i >= 0; i--)
    {
      res2_pair *p = elems[i];
      if (p->syz_type == SYZ2_NOT_MINIMAL)
        while ((tm = R->component_occurs_in(p->pivot_term->comp, f)) != NULL)
          {
            // Subtract the proper multiple to f.  f = ... + c m e_y + ...
            // and                                 p = ... + d n e_y
            // where n|m.  So we want to compute f -= c/d m/n p.
            ring_elem c =
                K->divide(tm->coeff, p->pivot_term->coeff);  // exact division
            // MES: is the following line actually needed?
            M->divide(tm->monom, p->pivot_term->monom, MINIMAL_mon);
            if (stripped[p->me] == NULL) stripped[p->me] = R->strip(p->syz);
            R->subtract_multiple_to(f, c, MINIMAL_mon, stripped[p->me]);
          }
    }
}

Matrix *res2_comp::make_minimal(int i) const
{
  const FreeModule *F = minimal_free_of(i - 1);
  const FreeModule *G = minimal_free_of(i);
  MatrixConstructor result(F, G, NULL);
  if (i <= 0 || i >= resn.size() - 1) return result.to_matrix();
  if (do_by_level > 0) return make(i);

  VECTOR(res2_pair *) elems;
  VECTOR(res2term *) stripped;

  int n = 0;
  for (res2_pair *p = resn[i]->pairs; p != NULL; p = p->next)
    {
      p->me = n++;
      elems.push_back(p);
      stripped.push_back(static_cast<res2term *>(NULL));
    }

  int thisx = 0;
  for (int x = 0; x < elems.size(); x++)
    {
      res2_pair *p = elems[x];
      if (p->syz_type == SYZ2_MINIMAL)
        {
          if (stripped[p->me] == NULL)
            {
              stripped[p->me] = R->strip(p->syz);
              reduce_minimal(x, stripped[p->me], elems, stripped);
            }
          result.set_column(thisx++, R->to_vector(stripped[p->me], F, 1));
        }
    }

  return result.to_matrix();
}

/////////////////////////////
// Minimalization routines //
/////////////////////////////

///////////////////////////////////////////////////////
// pivot -- apply a specific pivot to the resolution //
//                                                   //
// modifies the resolution                           //
///////////////////////////////////////////////////////

#if 0
// Matrix res2_comp::reduce_mod_vars(int level) const
// {
//   // Set all variables to 0, but only take columns marked
//   // as SZ or GB, not NO.  The matrix returned is over K.
//
//   // Set 'me' values for level 'level' and 'level-1'.
//
//   FreeModule *rows = K->make_FreeModule();
//   FreeModule *cols = K->make_FreeModule();
//   Matrix result(rows, cols);
//   for (res_pair *p = resn[level]->pairs; p != NULL; p = p->next)
//     {
//       if (p->syz_type == SYZ2_MINIMAL || SYZ2_NOT_MINIMAL)
//      {
//        result[next++] = reduce_mod_vars(result.rows(), p->syz);
//      }
//     }
//   return result;
// }
// void res2_comp::set_pivots(int level)
// {
//   // Determines the status of each element (SZ, GB, NO)
//   // (given the status of each element of higher level).
//   // Also this sets the pivot_term of each element at this level
//
//   // m = reduce_mod_vars(level)
//   // now reduce this matrix, using change of basis.
//   // for each column of this matrix, we will set a column to SYZ2_NOT_MINIMAL,
//   // a row to SYZ2_NOT_NEEDED, and the columns pivot_term to the chosen pivot.
// }
//
// void res2_comp::pivot(int level, int r, int c)
// {
//   res2_pair *p;
//
//   // First find the specific pivot column
//   for (p = resn[level]->pairs; p!=NULL; p=p->next)
//     if (p->me == c)
//       {
//      pivot_pair = p;
//      break;
//       }
//
//   // Now find the pivot ring element
//   term head;
//   term *f = &head;
//   for (tm = pivot_pair->syz; tm != NULL; tm=tm->next)
//     {
//       if (tm->comp->me == r)
//      {
//        pivot_row = tm->comp;
//
//        // Grab this element
//        f->next = P->term(tm->coeff, tm->monom);
//        f = f->next;
//        M->divide(f->monom, pivot_row->syz->monom, f->monom);
//      }
//     }
//   f->next = NULL;
//   f = head.next;
//
//   // Now loop through the columns of the level th matrix,
//   // replacing each column (other than column c) with
//   // elem(level,i) = f*elem(level,i) - c*v,
//   // where c = elem(level,r,i).
//   for (res2_pair *p = resn[level]->pairs; p!=NULL; p=p->next)
//     {
//       if (p == pivot_pair) continue;
//       res2term *w = p->syz;
//       if (R->get_coefficient(w, pivot_row, g))
//      {
//        P->negate_to(g);
//        res2term *h1 = R->mult(f,w);
//        res2term *h2 = R->mult(g,pivot_pair->syz);
//        R->add_to(h1,h2);
//        P->remove(g);
//        R->remove(p->syz);
//        p->syz = h1;
//      }
//     }
//
//   // Remove every occurrence of row 'c' in level 'level+1'.
//   // We save these up, to remove all at once, using 'strip_matrix'.
//   pivot_pair->syz_type = SYZ2_NOT_MINIMAL;
//   pivot_row->syz_type = SYZ2_NOT_NEEDED;
//
//   // We will never use this column again, so remove it:
//   R->remove(pivot_pair->syz);
//   pivot_pair->syz = NULL;    // This is a bit dangerous, since
//                           // many routines use this as if it must be nonNULL...
// }
//
// void res2_comp::strip_matrix(int level)
// {
//   // Remove all rows which are marked as SYZ2_NOT_MINIMAL
//   for (res2_pair *p = resn[level]->pairs; p != NULL; p=p->next)
//     {
//       res2term *f = strip(p->syz);
//       R->remove(p->syz);
//       p->syz = f;
//     }
// }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
