
/* Copyright 2003-2009, Michael E. Stillman */

#include "gb-test1.hpp"
#include "text-io.hpp"
#include <functional>
#include <algorithm>

#include "matrix.hpp"
#include "matrix-con.hpp"
#include "polyring.hpp"
#include "newdelete.hpp"
#include "relem.hpp"
#include "hilb.hpp"

#include "gbweight.hpp"
#include "reducedgb.hpp"

#include "monsort.hpp"

#include "interrupted.hpp"

#define PrintingDegree 0x0001

/*************************
 * Initialization ********
 *************************/

exponents gbB::exponents_make()
{
  exponents result = reinterpret_cast<exponents>(lcm_stash->new_elem());
  return result;
}

gbB *gbB::create(const Matrix *m,
                 M2_bool collect_syz,
                 int n_rows_to_keep,
                 M2_arrayint gb_weights,
                 int strategy,
                 M2_bool use_max_degree_limit,
                 int max_degree_limit,
                 int max_reduction_count)
{
  gbB *result = new gbB;
  result->initialize(m,
                     collect_syz,
                     n_rows_to_keep,
                     gb_weights,
                     strategy,
                     max_reduction_count);
  return result;
}

void gbB::initialize(const Matrix *m,
                     int csyz,
                     int nsyz,
                     M2_arrayint gb_weights0,
                     int strat,
                     int max_reduction_count0)
{
  // max_reduction_count: default was 10
  // 1 is best possible for 3-anderbuch!
  // 5 is: (114.64 sec, 494 MB)
  // 10 is best so far (125.33 sec, 527 MB virtual).
  // 50 is faster/smaller than 100, and 1000 was awful, on 3-andersbuch

  max_reduction_count = max_reduction_count0;

  const PolynomialRing *origR = m->get_ring()->cast_to_PolynomialRing();
  if (origR == NULL)
    {
      ERROR("ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  originalR = origR;
  R = origR->get_gb_ring();
  weightInfo = new GBWeight(m->rows(), gb_weights0);
  gb_weights = weightInfo->get_weights();

  nvars = R->get_flattened_monoid()->n_vars();

  spair_stash = new stash("gbB spairs", sizeof(spair));
  gbelem_stash = new stash("gbB elems", sizeof(gbelem));
  exp_size = EXPONENT_BYTE_SIZE(nvars + 2);
  lcm_stash = new stash("gbB lead monoms", exp_size);

  if (nsyz < 0 || nsyz > m->n_cols()) nsyz = m->n_cols();
  n_rows_per_syz = nsyz;

  F = m->rows();
  Fsyz = m->cols()->sub_space(n_rows_per_syz);

  S = new SPairSet;
  first_in_degree = 0;
  n_syz = 0;
  n_pairs_computed = 0;
  n_gens_left = 0;
  n_subring = 0;

  _strategy = strat;
  _collect_syz = csyz;
  _is_ideal = (F->rank() == 1 && csyz == 0);
  if (R->is_weyl_algebra()) _is_ideal = false;

  hilbert = 0;
  n_saved_hilb = 0;

  this_degree = F->lowest_primary_degree() - 1;
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

  lookup = MonomialTable::make(nvars);

  minimal_gb = ReducedGB::create(originalR, F, Fsyz);
  minimal_gb_valid = true;

  if (originalR->is_quotient_ring())
    {
      ringtable = originalR->get_quotient_MonomialTable();

      first_gb_element = originalR->n_quotients();
      for (int i = 0; i < first_gb_element; i++)
        {
          gbvector *f = const_cast<gbvector *>(originalR->quotient_gbvector(i));
          gbelem *g = gbelem_ring_make(f);
          gb.push_back(g);
        }
    }
  for (int i = 0; i < m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *f = originalR->translate_gbvector_from_vec(F, (*m)[i], denom);
      spair *p = new_gen(i, f, denom);
      if (p != NULL)
        {
          spair_set_insert(p);
          n_gens_left++;
        }
    }

  state = STATE_NEWDEGREE;  // will be changed if hilb fcn is used
  np_i = first_gb_element;
  ar_i = first_gb_element;
  ar_j = ar_i + 1;
  n_gb = first_gb_element;
}

gbB::spair *gbB::new_gen(int i, gbvector *f, ring_elem denom)
{
  gbvector *fsyz;

  if (i < n_rows_per_syz)
    fsyz = R->gbvector_term(Fsyz, denom, i + 1);
  else
    fsyz = R->gbvector_zero();

  if (R->gbvector_is_zero(f))
    {
      originalR->get_quotient_info()->gbvector_normal_form(Fsyz, fsyz);
      if (!R->gbvector_is_zero(fsyz))
        {
          // vec fsyzvec = _GR->gbvector_to_vec(Fsyz,fsyz);
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
void gbB::remove_gb()
{
  // removes all allocated objects
  for (int i = first_gb_element; i < gb.size(); i++)
    {
      R->gbvector_remove(gb[i]->g.f);
      R->gbvector_remove(gb[i]->g.fsyz);
    }
  for (int i = 0; i < gb.size(); i++)
    {
      lcm_stash->delete_elem(gb[i]->lead);
      gbelem_stash->delete_elem(gb[i]);
      gb[i] = 0;
    }
  delete minimal_gb;  // will free its own gbvector's.
  for (int i = 0; i < _syz.size(); i++)
    {
      R->gbvector_remove(_syz[i]);
      _syz[i] = 0;
    }
  delete lookup;
  delete spair_stash;
  delete gbelem_stash;
  delete lcm_stash;
  // Also remove the SPAirSet...
}

gbB::~gbB() { remove_gb(); }
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
// can handle the case when a == result or b == result
{
  int i;
  int deg = dega;
  for (i = 0; i < nvars; i++)
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
  for (int i = 0; i < nvars; i++)
    if (a[i] != b[i]) return false;
  return true;
}

static bool exponents_divide(int nvars, exponents a, exponents b)
{
  for (int i = 0; i < nvars; i++)
    if (a[i] > b[i]) return false;
  return true;
}

static bool exponents_less_than(int nvars, exponents a, exponents b)
{
  for (int i = 0; i < nvars; i++)
    {
      if (a[i] < b[i]) return true;
      if (a[i] > b[i]) return false;
    }
  return false;
}

/*************************
 * gbelem handling *******
 *************************/

gbB::gbelem *gbB::gbelem_ring_make(gbvector *f)
{
  int f_leadweight;
  gbelem *g = reinterpret_cast<gbelem *>(gbelem_stash->new_elem());
  g->g.f = f;
  g->g.fsyz = 0;
  g->lead = exponents_make();
  R->gbvector_get_lead_exponents(F, f, g->lead);
  g->deg = weightInfo->gbvector_weight(f, f_leadweight);
  g->gap = g->deg - f_leadweight;
  g->reduced_deg = g->deg;
  g->reduced_gap = g->gap;
  g->size = R->gbvector_n_terms(f);
  g->minlevel = ELEMB_IN_RING;
  return g;
}

gbB::gbelem *gbB::gbelem_make(gbvector *f,     // grabs f
                              gbvector *fsyz,  // grabs fsyz
                              gbelem_type minlevel,
                              int deg)
{
  int f_wt, f_leadweight;
  gbelem *g = reinterpret_cast<gbelem *>(gbelem_stash->new_elem());
  g->g.f = f;
  g->g.fsyz = fsyz;
  g->lead = exponents_make();
  R->gbvector_get_lead_exponents(F, f, g->lead);
  g->deg = deg;
  f_wt = weightInfo->gbvector_weight(f, f_leadweight);
  g->gap =
      deg -
      f_leadweight;  // used to be: deg - weightInfo->gbvector_term_weight(f);
  g->reduced_deg = f_wt;
  g->reduced_gap = f_wt - f_leadweight;
  g->size = R->gbvector_n_terms(f);
  g->minlevel = minlevel;
  return g;
}

void gbB::gbelem_text_out(buffer &o, int i, int nterms) const
{
  gbelem_type minlevel = gb[i]->minlevel;
  bool ismingen = (minlevel & ELEMB_MINGEN);
  bool ismingb = (minlevel & ELEMB_MINGB);
  if (ismingb)
    o << "GB elem: ";
  else
    o << "reducer: ";
  o << "g" << i << " = ";
  R->gbvector_text_out(o, F, gb[i]->g.f, nterms);
  o << " ["
    << "gap " << gb[i]->gap << " size " << gb[i]->size << " deg " << gb[i]->deg
    << " rgap " << gb[i]->reduced_gap << " rdeg " << gb[i]->reduced_deg;
  if (ismingen) o << " mingen";
  o << "]";
}

/*************************
 * SPair handling ********
 *************************/

gbB::spair *gbB::spair_node()
{
  spair *result = reinterpret_cast<spair *>(spair_stash->new_elem());
  result->next = 0;
  return result;
}

void gbB::spair_delete(spair *&p)
{
  if (p == 0) return;
  if (p->type == SPAIR_GEN || p->type == SPAIR_ELEM)
    {
      R->gbvector_remove(p->x.f.f);
      R->gbvector_remove(p->x.f.fsyz);
    }
  R->gbvector_remove(p->lead_of_spoly);
  lcm_stash->delete_elem(p->lcm);
  spair_stash->delete_elem(p);
}

gbB::spair *gbB::spair_make(int i, int j)
{
  gbelem *g1 = gb[i];
  gbelem *g2 = gb[j];
  exponents exp1 = g1->lead;
  exponents exp2 = g2->lead;
  spair *result = spair_node();
  result->next = 0;
  result->type = SPAIR_SPAIR;
  result->lcm = exponents_make();
  exponents_lcm(
      nvars, g1->deg, exp1, exp2, result->lcm, gb_weights, result->deg);
  if (g2->gap > g1->gap) result->deg += g2->gap - g1->gap;
  result->x.pair.i = i;
  result->x.pair.j = j;

  return result;
}

gbB::spair *gbB::spair_make_gen(POLY f)
{
  assert(f.f != 0);
  exponents exp1 = exponents_make();
  R->gbvector_get_lead_exponents(F, f.f, exp1);
  int deg = weightInfo->gbvector_weight(f.f);
  spair *result = spair_node();
  result->next = 0;
  result->type = SPAIR_GEN;
  result->deg = deg;
  result->lcm = exp1;
  result->x.f = f;

  return result;
}

gbB::spair *gbB::spair_make_skew(int i, int v)
{
  spair *result;
  int j;
  gbelem *g1 = gb[i];
  exponents exp1 = g1->lead;
  exponents exp2 = exponents_make();
  int vvar = R->skew_variable(v);
  for (j = 0; j < nvars; j++) exp2[j] = 0;
  exp2[vvar] = 2;
  result = spair_node();
  result->next = 0;
  result->type = SPAIR_SKEW;
  result->lcm = exp2;
  exponents_lcm(nvars, g1->deg, exp1, exp2, exp2, gb_weights, result->deg);
  result->x.pair.i = i;
  result->x.pair.j = v;

  return result;
}

gbB::spair *gbB::spair_make_ring(int i, int j)
{
  /* This requires that j indexes into the gb array somewhere. */
  spair *result = spair_make(i, j);
  result->type = SPAIR_RING;

  return result;
}

void gbB::spair_text_out(buffer &o, spair *p)
{
  char s[100];  // enough room for all of the non polynomial cases.
  switch (p->type)
    {
      case SPAIR_SPAIR:
        sprintf(s, "spair(g%d,g%d):", p->x.pair.j, p->x.pair.i);
        o << s;
        sprintf(s, " deg %d", p->deg);
        o << s;
        o << " lcm exponents [";
        for (int i = 0; i < nvars + 2; i++)
          {
            sprintf(s, "%d ", p->lcm[i]);
            o << s;
          }
        o << "]";
        break;
      case SPAIR_GEN:
        o << "generator ";
        R->gbvector_text_out(o, F, p->f(), 3);
        break;
      case SPAIR_ELEM:
        o << "elem ";
        R->gbvector_text_out(o, F, p->f(), 3);
        break;
      case SPAIR_RING:
        sprintf(s, "rpair(%d,%d)", p->x.pair.i, p->x.pair.j);
        o << s;
        break;
      case SPAIR_SKEW:
        sprintf(s, "skewpair(g%d,g%d)", p->x.pair.j, p->x.pair.i);
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

static unsigned long ncalls = 0;
static unsigned long nloops = 0;
static unsigned long nsaved_unneeded = 0;
bool gbB::pair_not_needed(spair *p, gbelem *m)
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
  if (gbelem_COMPONENT(m) != spair_COMPONENT(p)) return false;

  first = p->x.pair.i;
  second = p->x.pair.j;
  p1exp = gb[first]->lead;
  p2exp =
      gb[second]->lead; /* If a ring pair, this should index into gb array */

  ncalls++;
  for (i = 0; i < nvars; i++, nloops++)
    if (mexp[i] > lcm[i]) return false;

  firstok = false;
  for (i = 0; i < nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p1exp[i] == lcm[i]) continue;
      firstok = true;
      break;
    }
  if (!firstok) return false;
  for (i = 0; i < nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p2exp[i] == lcm[i]) continue;
      return true;
    }
  return false;
}

void gbB::remove_unneeded_pairs(int id)
{
  /* Removes all pairs from C->S that are not needed */
  spair head;
  spair *p = &head;
  gbelem *m = gb[id];

  head.next = S->heap;
  while (p->next != 0)
    if (pair_not_needed(p->next, m))
      {
        nsaved_unneeded++;
        spair *tmp = p->next;
        p->next = tmp->next;
        tmp->next = 0;
        if (M2_gbTrace >= 10)
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

bool gbB::is_gcd_one_pair(spair *p)
{
  int i, j;
  exponents e1, e2;
  if (p->type != SPAIR_SPAIR) return false;
  i = p->x.pair.i;
  j = p->x.pair.j;
  e1 = gb[i]->lead;
  e2 = gb[j]->lead;
  for (i = 0; i < nvars; i++)
    if (e1[i] > 0 && e2[i] > 0) return false;
  return true;
}

gbB::spairs::iterator gbB::choose_pair(gbB::spairs::iterator first,
                                       gbB::spairs::iterator next)
{
  /* a is an array of spair's, and a[first], ..., a[next-1] all have the
     same lcm, which is a minimal monomial generator of all such lcm's.
     Our goal is to choose a nice one, and throw away the others.
     We return one spair, and delete the rest.
  */
  if (next == first + 1) return first;
  return first; /* MES: really do something here... */
}

namespace {
struct spair_sorter
    : public std::binary_function<gbB::spair *, gbB::spair *, bool>
{
  int nvars;
  spair_sorter(int nv) : nvars(nv) {}
  bool operator()(gbB::spair *a, gbB::spair *b)
  {
    /* Compare using degree, then type, then lcm */
    bool result;
    int cmp = a->deg - b->deg;
    if (cmp < 0)
      result = true;
    else if (cmp > 0)
      result = false;
    else
      {
        cmp = a->type - b->type;
        if (cmp < 0)
          result = true;
        else if (cmp > 0)
          result = false;
        else
          result = exponents_less_than(nvars, a->lcm, b->lcm);
      }
    return result;
  }
};
}; // unnamed namespace

class SPolySorterB
{
 public:
  typedef gbB::spair *value;

 private:
  const FreeModule *F;
  GBRing *R;
  long ncmps;

 public:
  int compare(value a, value b)
  {
    // returns: LT if a < b, EQ if a == b, GT if a > b.
    ncmps++;
    /* Compare using degree, then type, then lead term of spoly */
    int result;
    int cmp = a->deg - b->deg;
    if (cmp < 0)
      result = GT;
    else if (cmp > 0)
      result = LT;
    else
      {
        gbvector *a1 = (a->type > gbB::SPAIR_SKEW ? a->f() : a->lead_of_spoly);
        gbvector *b1 = (b->type > gbB::SPAIR_SKEW ? b->f() : b->lead_of_spoly);
        if (a1 == 0)
          {
            if (b1 == 0)
              result = EQ;
            else
              result = LT;
          }
        else
          {
            if (!b1)
              result = GT;
            else
              result = R->gbvector_compare(F, a1, b1);
          }
      }
    return result;
  }

  SPolySorterB(GBRing *R0, const FreeModule *F0) : F(F0), R(R0), ncmps(0) {}
  long ncomparisons() const { return ncmps; }
  ~SPolySorterB() {}
};

void gbB::minimalize_pairs(spairs &new_set)
/* new_set: array of spair*  */
{
  std::stable_sort(new_set.begin(), new_set.end(), spair_sorter(nvars));
  MonomialTable *montab = MonomialTable::make(nvars);

  //  array_sort(new_set, (compareFcn)spair_compare, 0);
  spairs::iterator first = new_set.begin();
  spairs::iterator next = first;
  spairs::iterator end = new_set.end();
  for (; first != end; first = next)
    {
      next = first + 1;
      spair *me = *first;
      while (next != end)
        {
          spair *p = *next;
          if (!exponents_equal(nvars, me->lcm, p->lcm)) break;
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
              if ((M2_gbTrace & PRINT_SPAIR_TRACKING) != 0)
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
              if (M2_gbTrace >= 4)
                {
                  buffer o;
                  o << "    new ";
                  spair_text_out(o, p);
                  emit_line(o.str());
                }
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

void gbB::update_pairs(int id)
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
      for (int i = 0; i < R->n_skew_commutative_vars(); i++)
        if (r->lead[R->skew_variable(i)] > 0)
          {
            spair *s = spair_make_skew(id, i);
            new_set.push_back(s);
          }
    }
  /* Step 2b: pairs from ring elements, or 'in stone' elements */
  for (int i = 0; i < first_gb_element; i++)
    {
      spair *s = spair_make_ring(id, i);
      new_set.push_back(s);
    }
  /* Step 2c. pairs from the vectors themselves */
  /* Loop through the minimal GB elements and form the s-pair */
  for (int i = first_gb_element; i < id; i++)
    {
      gbelem *g = gb[i];
      if ((g->minlevel & ELEMB_MINGB) && gbelem_COMPONENT(g) == x)
        {
          spair *s = spair_make(id, i);
          new_set.push_back(s);
        }
    }

  /* Step 3. Minimalize this set */
  minimalize_pairs(
      new_set); /* Modifies new_set, inserts minimal pairs into S */
}

/*************************
 * S-pair sets ***********
 *************************/

gbB::SPairSet::SPairSet()
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

void gbB::remove_spair_list(spair *&set)
{
  while (!set)
    {
      spair *tmp = set;
      set = set->next;
      spair_delete(tmp);
    }
  set = 0;
}

void gbB::remove_SPairSet()
{
  remove_spair_list(S->heap);
  remove_spair_list(S->spair_list);
  remove_spair_list(S->spair_deferred_list.next);
  remove_spair_list(S->gen_list);
  remove_spair_list(S->gen_deferred_list.next);
  S->spair_last_deferred = 0;
  S->gen_last_deferred = 0;
}

void gbB::spair_set_insert(gbB::spair *p)
/* Insert a LIST of s pairs into S */
{
  while (p != 0)
    {
      spair_set_lead_spoly(p);
      spair *tmp = p;
      p = p->next;
      S->nelems++;
      tmp->next = S->heap;
      S->heap = tmp;
    }
}

gbB::spair *gbB::spair_set_next()
/* Removes the next element of the current degree, returning NULL if none left
 */
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
          if (M2_gbTrace >= 4)
            {
              emit_line("considering deferred pairs: ");
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
                  if (M2_gbTrace >= 4)
                    {
                      emit_line("  deferred gen pairs: ");
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

void gbB::spair_set_defer(spair *&p)
// Defer the spair p until later in this same degree
// The spair should have been reduced a number of times
// already, so its type should be SPAIR_GEN or SPAIR_ELEM
{
  if (M2_gbTrace == 15)
    {
      emit_line("    deferred by reduction count");
    }
  else if (M2_gbTrace >= 4)
    emit_wrapped("D");
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

int gbB::spair_set_determine_next_degree(int &nextdegree)
{
  spair *p;
  int nextdeg;
  int len = 1;
  if (S->heap == 0) return 0;
  nextdeg = S->heap->deg;
  for (p = S->heap->next; p != 0; p = p->next)
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

int gbB::spair_set_prepare_next_degree(int &nextdegree)
/* Finds the next degree to consider, returning the number of spairs in that
 * degree */
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

void gbB::spair_set_show_mem_usage() {}
void gbB::spairs_reverse(spair *&ps)
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
void gbB::spairs_sort(int len, spair *&ps)
{
  if (ps == 0 || ps->next == 0) return;
  if (len <= 1) return;
  spairs a;  // array of spair's
  spairs b;  // these are the ones which are uncomputed, but whose lead_of_spoly
             // is 0.
  a.reserve(len);
  for (spair *p = ps; p != 0; p = p->next)
    {
      if ((p->type > gbB::SPAIR_SKEW) || p->lead_of_spoly)
        a.push_back(p);
      else
        b.push_back(p);
    }

  SPolySorterB SP(R, F);
  QuickSorter<SPolySorterB>::sort(&SP, &a[0], a.size());

  int asize = INTSIZE(a);
  int bsize = INTSIZE(b);

  if (asize > 0)
    {
      ps = a[0];
      for (int i = 1; i < asize; i++) a[i - 1]->next = a[i];
    }
  else if (bsize > 0)
    {
      ps = b[0];
      // debugging// fprintf(stderr, "bsize is %d\n",bsize);
    }
  else
    {
      ps = 0;
      return;
    }

  if (asize > 0) a[asize - 1]->next = (bsize > 0 ? b[0] : 0);
  if (bsize > 0)
    {
      for (int i = 1; i < bsize; i++) b[i - 1]->next = b[i];
      b[bsize - 1]->next = 0;
    }
}

/****************************************
 * Polynomial arithmetic and reduction **
 ****************************************/

void gbB::spair_set_lead_spoly(spair *p)
{
  gbvector *ltsyz = 0;
  POLY f, g;
  if (p->type > SPAIR_SKEW)
    {
      R->gbvector_remove(p->lead_of_spoly);
      p->lead_of_spoly = 0;
      return;
    }
  f = gb[p->x.pair.i]->g;
  if (p->type == SPAIR_SKEW)
    {
      const int *mon = R->skew_monomial_var(p->x.pair.j);
      R->gbvector_mult_by_term(
          F, Fsyz, R->one(), mon, f.f, 0, p->lead_of_spoly, ltsyz);
    }
  else
    {
      g = gb[p->x.pair.j]->g;
      R->gbvector_cancel_lead_terms(
          F, Fsyz, f.f, 0, g.f, 0, p->lead_of_spoly, ltsyz);
    }
  if (p->lead_of_spoly != 0)
    {
      gbvector *tmp = p->lead_of_spoly->next;
      p->lead_of_spoly->next = 0;
      R->gbvector_remove(tmp);
    }
}

void gbB::compute_s_pair(spair *p)
{
  POLY f, g;
  if (M2_gbTrace >= 5 && M2_gbTrace != 15)
    {
      buffer o;
      spair_text_out(o, p);
      emit_line(o.str());
    }
  if (p->type > SPAIR_SKEW) return;
  R->gbvector_remove(p->lead_of_spoly);
  p->lead_of_spoly = 0;
  f = gb[p->x.pair.i]->g;
  if (p->type == SPAIR_SKEW)
    {
      const int *mon = R->skew_monomial_var(p->x.pair.j);
      R->gbvector_mult_by_term(
          F, Fsyz, R->one(), mon, f.f, f.fsyz, p->f(), p->fsyz());
    }
  else
    {
      g = gb[p->x.pair.j]->g;
      R->gbvector_cancel_lead_terms(
          F, Fsyz, f.f, f.fsyz, g.f, g.fsyz, p->f(), p->fsyz());
    }
  p->type = SPAIR_ELEM;
  if (M2_gbTrace >= 5 && M2_gbTrace != 15)
    {
      buffer o;
      o << "    ";
      R->gbvector_text_out(o, F, p->f());
      emit_line(o.str());
    }
}

bool gbB::reduceit(spair *p)
{
  /* Returns false iff we defer computing this spair. */
  /* If false is returned, this routine has grabbed the spair 'p'. */

  exponents EXP = ALLOCATE_EXPONENTS(exp_size);

  int tmf, wt;
  int count = -1;
  if (M2_gbTrace == 15)
    {
      buffer o;
      o << "considering ";
      spair_text_out(o, p);
      o << " : ";
      emit_line(o.str());
    }
  compute_s_pair(p); /* Changes the type, possibly */

  while (!R->gbvector_is_zero(p->f()))
    {
      if (count++ > max_reduction_count)
        {
          spair_set_defer(p);
          return false;
        }
      if (M2_gbTrace >= 5)
        {
          if ((wt = weightInfo->gbvector_weight(p->f(), tmf)) > this_degree)
            {
              buffer o;
              o << "ERROR: degree of polynomial is too high: deg " << wt
                << " termwt " << tmf << " expectedeg " << this_degree
                << newline;
              emit(o.str());
            }
        }

      int gap, w;
      R->gbvector_get_lead_exponents(F, p->f(), EXP);
      int x = p->f()->comp;
      w = find_good_divisor(EXP, x, this_degree, gap);

      // replaced gap, g.
      if (w < 0) break;
      if (false && gap > 0)
        {
          POLY h;
          h.f = R->gbvector_copy(p->x.f.f);
          h.fsyz = R->gbvector_copy(p->x.f.fsyz);
          insert_gb(h, (p->type == SPAIR_GEN ? ELEMB_MINGEN : 0));
        }
      POLY g = gb[w]->g;

      R->gbvector_reduce_lead_term(F,
                                   Fsyz,
                                   0,
                                   p->f(),
                                   p->fsyz(), /* modifies these */
                                   g.f,
                                   g.fsyz);

      stats_nreductions++;
      if (M2_gbTrace == 15)
        {
          buffer o;
          o << "    reducing by g" << w;
          o << ", yielding ";
          R->gbvector_text_out(o, F, p->f(), 3);
          emit_line(o.str());
        }
      if (R->gbvector_is_zero(p->f())) break;
      if (gap > 0)
        {
          p->deg += gap;
          if (M2_gbTrace == 15)
            {
              buffer o;
              o << "    deferring to degree " << p->deg;
              emit_line(o.str());
            }
          spair_set_insert(p);
          return false;
        }
    }
  if (M2_gbTrace >= 4 && M2_gbTrace != 15)
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

int gbB::find_good_divisor(exponents e, int x, int degf, int &result_gap)
// Returns an integer w.
// if w >=0: gb[w]'s lead term divides [e,x].
// if w<0: no gb[w] has lead term dividing [e,x].
{
  int n = 0;
  int gap;
  int egap = degf - weightInfo->exponents_weight(e, x);

  VECTOR(MonomialTable::mon_term *) divisors;

  if (divisor_previous >= 0 && x == divisor_previous_comp)
    {
      gbelem *tg = gb[divisor_previous];
      gap = tg->reduced_gap - egap;
      if (gap <= 0 && exponents_divide(nvars, tg->lead, e))
        {
          result_gap = 0;
          return divisor_previous;
        }
    }

  /* First search for ring divisors */
  if (ringtable) n += ringtable->find_divisors(-1, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookup->find_divisors(-1, e, x, &divisors);

  if (M2_gbTrace == 15 && n >= 2)
    {
      gbelem *tg = gb[divisors[n - 1]->_val];
      int sz = tg->size;
      if (sz >= 0)  // was 3, why??
        {
          buffer o;
          o << "    reducers: ";
          for (int j = 0; j < n; j++) o << "g" << divisors[j]->_val << " ";
          emit_line(o.str());
        }
    }
  /* Now find the minimal gap value */
  if (n == 0)
    {
      result_gap = 0;
      return -1;
    }

  int newgap;
  int result = divisors[n - 1]->_val;
  gbelem *tg = gb[result];
  gap = tg->reduced_gap - egap;
  if (gap <= 0)
    {
      gap = 0;
      int minsz = tg->size;
      for (int i = n - 2; i >= 0; i--)
        {
          int new_val = divisors[i]->_val;
          tg = gb[new_val];
          int sz = tg->size;
          if (sz < minsz)
            {
              if (tg->reduced_gap <= egap)
                {
                  minsz = sz;
                  result = new_val;
                }
            }
        }
    }
  else
    //    for (i=1; i<n; i++)
    for (int i = n - 2; i >= 0; i--)
      {
        int new_val = divisors[i]->_val;
        tg = gb[new_val];

        newgap = tg->reduced_gap - egap;
        if (newgap <= 0)
          {
            gap = 0;
            result = new_val;
            break;
          }
        else if (newgap < gap)
          {
            result = new_val;
            gap = newgap;
          }
      }
  divisor_previous = result;
  divisor_previous_comp = x;
  result_gap = gap;
  return result;
}

void gbB::remainder(POLY &f, int degf, bool use_denom, ring_elem &denom)
// find the remainder of f = [g,gsyz] wrt the GB,
// i.e. replace f with h[h,hsyz], st
//    h = f - sum(a_i * g_i),  in(f) not in in(G)
//    hsyz = fsyz - sum(a_i * gsyz_i)
//    denom is unchanged
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
  exponents EXP = ALLOCATE_EXPONENTS(exp_size);

  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  int count = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int gap;
      R->gbvector_get_lead_exponents(F, h.f, EXP);
      int x = h.f->comp;
      int w = find_good_divisor(EXP, x, degf, gap);
      // replaced gap, g.
      if (w < 0 || gap > 0)
        {
          frem->next = h.f;
          frem = frem->next;
          h.f = h.f->next;
          frem->next = 0;
        }
      else
        {
          POLY g = gb[w]->g;
          R->gbvector_reduce_lead_term(
              F, Fsyz, head.next, h.f, h.fsyz, g.f, g.fsyz, use_denom, denom);
          count++;
          //      stats_ntail++;
          if (M2_gbTrace >= 10)
            {
              buffer o;
              o << "  tail reducing by ";
              R->gbvector_text_out(o, F, g.f, 2);
              o << "\n    giving ";
              R->gbvector_text_out(o, F, h.f, 3);
              emit_line(o.str());
            }
        }
    }
  h.f = head.next;
  R->gbvector_remove_content(h.f, h.fsyz, use_denom, denom);
  f.f = h.f;
  f.fsyz = h.fsyz;
  if ((M2_gbTrace & PRINT_SPAIR_TRACKING) != 0)
    {
      buffer o;
      o << "number of reduction steps was " << count;
      emit_line(o.str());
    }
  else if (M2_gbTrace >= 4 && M2_gbTrace != 15)
    {
      buffer o;
      o << "," << count;
      emit_wrapped(o.str());
    }
}

/********************
 ** State machine ***
 ********************/

void gbB::auto_reduce_by(int id)
{
  /* Loop backwards while degree doesn't change */
  /* Don't change quotient ring elements */
  gbelem *me = gb[id];
  int a = me->gap;  // Only auto reduce those that are of the same degree
                    // and not a higher gap level
  for (int i = INTSIZE(gb) - 1; i >= first_gb_element; i--)
    {
      if (i == id) continue;
      gbelem *g = gb[i];
      if (g->deg < me->deg) return;
      if (g->gap < a) continue;
      if (M2_gbTrace >= 10)
        {
          buffer o;
          o << "  auto reduce " << i << " by " << id;
          emit_line(o.str());
        }
      R->gbvector_auto_reduce(F,
                              Fsyz,
                              g->g.f,
                              g->g.fsyz,  // these are modified
                              me->g.f,
                              me->g.fsyz);
    }
}

void gbB::collect_syzygy(gbvector *f)
{
  _syz.push_back(f);
  n_syz++;

  if (M2_gbTrace >= 10)
    {
      buffer o;
      o << " new syzygy : ";
      R->gbvector_text_out(o, Fsyz, f, 3);
      emit_line(o.str());
    }
}

void gbB::insert_gb(POLY f, gbelem_type minlevel)
{
  /* Reduce this element as far as possible.  This either removes content,
     makes it monic, or at least negates it so the lead coeff is positive. */
  ring_elem junk;

  // DEBUG BLOCK  int fwt;
  //  int fdeg = weightInfo->gbvector_weight(f.f, fwt);
  //  fprintf(stderr, "inserting GB element %d, thisdeg %d deg %d gap %d\n",
  //      gb.size(),
  //      this_degree,
  //      fdeg,
  //      fdeg-fwt);

  remainder(f, this_degree, false, junk);

  //  fdeg = weightInfo->gbvector_weight(f.f, fwt);
  //  fprintf(stderr, "    after remainder deg %d gap %d\n",
  //      fdeg,
  //      fdeg-fwt);

  stats_ngb++;

  // Complete hack for getting bug fix to get test/isSubset.m2 to work again for
  // 1.3:
  // over ZZ, always make gb elements non reducers...

  gbelem *g = gbelem_make(f.f, f.fsyz, minlevel, this_degree);
  minimal_gb_valid = false;
  int me = INTSIZE(gb);
  gb.push_back(g);
  n_gb++;
  int x = g->g.f->comp;

  // In a encoded Schreyer order, the following line might miss subring
  // elements.
  // But it at least won't be incorrect...
  if (R->get_flattened_monoid()->in_subring(1, g->g.f->monom)) n_subring++;

  lookup->insert(g->lead, x, me);

  if (M2_gbTrace == 15)
    {
      buffer o;
      o << "    new ";
      gbelem_text_out(o, INTSIZE(gb) - 1);
      emit_line(o.str());
    }
  else if (M2_gbTrace >= 5)
    {
      char s[100];
      buffer o;
      sprintf(s, "new-inserting element %d (minimal %d): ", me, minlevel);
      o << s;
      R->gbvector_text_out(o, F, g->g.f);
      emit_line(o.str());
      o.reset();
      o << "                          syzygy : ";
      R->gbvector_text_out(o, Fsyz, g->g.fsyz);
      emit_line(o.str());
    }

  auto_reduce_by(me);

  if (hilbert)
    {
      bool is_last_elem = hilbert->addMonomial(g->lead, x);
      if (is_last_elem) flush_pairs();
    }
  else
    {
#ifdef DEVELOPMENT
#warning "todo: codimension stop condition"
#endif
      // codim test is set.  Compute the codimension now.
    }

  if (M2_gbTrace >= 10)
    {
      //      show();
    }
}

bool gbB::process_spair(spair *p)
{
  stats_npairs++;

  bool not_deferred = reduceit(p);

  if (!not_deferred) return true;

  gbelem_type minlevel =
      (p->type == SPAIR_GEN ? ELEMB_MINGEN : 0) | ELEMB_MINGB;

  if (p->type == SPAIR_GEN) n_gens_left--;
  POLY f = p->x.f;
  p->x.f.f = 0;
  p->x.f.fsyz = 0;
  spair_delete(p);

  if (!R->gbvector_is_zero(f.f))
    {
      insert_gb(f, minlevel);
      if (M2_gbTrace == 3) emit_wrapped("m");
    }
  else
    {
      originalR->get_quotient_info()->gbvector_normal_form(Fsyz, f.fsyz);
      if (!R->gbvector_is_zero(f.fsyz))
        {
          /* This is a syzygy */
          collect_syzygy(f.fsyz);
          if (M2_gbTrace == 3) emit_wrapped("z");
        }
      else
        {
          if (M2_gbTrace == 3) emit_wrapped("o");
        }
    }
  return true;
}

ComputationStatusCode gbB::computation_is_complete()
{
  // This handles everything but stop_.always, stop_.degree_limit
  if (stop_.basis_element_limit > 0 && gb.size() >= stop_.basis_element_limit)
    return COMP_DONE_GB_LIMIT;
  if (stop_.syzygy_limit > 0 && n_syz >= stop_.syzygy_limit)
    return COMP_DONE_SYZ_LIMIT;
  if (stop_.pair_limit > 0 && n_pairs_computed >= stop_.pair_limit)
    return COMP_DONE_PAIR_LIMIT;
  if (stop_.just_min_gens && n_gens_left == 0) return COMP_DONE_MIN_GENS;
  if (stop_.subring_limit > 0 && n_subring >= stop_.subring_limit)
    return COMP_DONE_SUBRING_LIMIT;
  if (stop_.use_codim_limit)
    {
      // Compute the codimension
      int c = 0;
      // int c = codim_of_lead_terms();
      if (c >= stop_.codim_limit) return COMP_DONE_CODIM;
    }
  return COMP_COMPUTING;
}

// new code
void gbB::do_computation()
{
  ComputationStatusCode ret;
  spair *p;

  // initial state is STATE_NEWDEGREE

  if (stop_.always_stop) return;  // don't change status

  if ((ret = computation_is_complete()) != COMP_COMPUTING)
    {
      set_status(ret);
      return;
    }

  if (M2_gbTrace == 15)
    {
      emit_line("[gb]");
    }
  else if (M2_gbTrace >= 1)
    {
      emit_wrapped("[gb]");
    }
  for (;;)
    {
      if (stop_.stop_after_degree && this_degree > stop_.degree_limit->array[0])
        {
          // Break out now if we don't have anything else to compute in this
          // degree.
          set_status(COMP_DONE_DEGREE_LIMIT);
          return;
        }
      if (M2_gbTrace & PrintingDegree)
        {
        }

      switch (state)
        {
          case STATE_NEWPAIRS:
            // Loop through all of the new GB elements, and
            // compute spairs.  Start at np_i
            // np_i is initialized at the beginning, and also here.
            while (np_i < n_gb)
              {
                if (system_interrupted())
                  {
                    set_status(COMP_INTERRUPTED);
                    return;
                  }
                if (gb[np_i]->minlevel & ELEMB_MINGB) update_pairs(np_i);
                np_i++;
              }
            state = STATE_NEWDEGREE;

          case STATE_NEWDEGREE:
            // Get the spairs and generators for the next degree

            if (S->n_in_degree == 0)
              {
                int old_degree = this_degree;
                npairs = spair_set_prepare_next_degree(
                    this_degree);  // sets this_degree
                if (old_degree < this_degree) first_in_degree = INTSIZE(gb);
                complete_thru_this_degree = this_degree - 1;
                if (npairs == 0)
                  {
                    state = STATE_DONE;
                    set_status(COMP_DONE);
                    return;
                  }
                if (stop_.stop_after_degree &&
                    this_degree > stop_.degree_limit->array[0])
                  {
                    set_status(COMP_DONE_DEGREE_LIMIT);
                    return;
                  }
                if (hilbert)
                  {
                    if (!hilbert->setDegree(this_degree))
                      {
                        if (error())
                          set_status(COMP_ERROR);
                        else
                          set_status(COMP_INTERRUPTED);
                        return;
                      }
                  }
              }
            if (M2_gbTrace == 15)
              {
                buffer o;
                o << "DEGREE " << this_degree;
                o << ", number of spairs = " << npairs;
                if (hilbert)
                  o << ", expected number in this degree = "
                    << hilbert->nRemainingExpected();
                emit_line(o.str());
              }
            else if (M2_gbTrace >= 1)
              {
                buffer o;
                o << '{' << this_degree << '}';
                o << '(';
                if (hilbert) o << hilbert->nRemainingExpected() << ',';
                o << npairs << ')';
                emit_wrapped(o.str());
              }
            ar_i = n_gb;
            ar_j = ar_i + 1;
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

                if (system_interrupted())
                  {
                    set_status(COMP_INTERRUPTED);
                    return;
                  }
              }
            state = STATE_AUTOREDUCE;
          // or state = STATE_NEWPAIRS

          case STATE_AUTOREDUCE:
            // This is still possibly best performed when inserting a new
            // element
            // Perform the necessary or desired auto-reductions
            while (ar_i < n_gb)
              {
                while (ar_j < n_gb)
                  {
                    if (system_interrupted())
                      {
                        set_status(COMP_INTERRUPTED);
                        return;
                      }
                    R->gbvector_auto_reduce(F,
                                            Fsyz,
                                            gb[ar_i]->g.f,
                                            gb[ar_i]->g.fsyz,
                                            gb[ar_j]->g.f,
                                            gb[ar_j]->g.fsyz);
                    ar_j++;
                  }
                ar_i++;
                ar_j = ar_i + 1;
              }
            state = STATE_NEWPAIRS;
            break;

          case STATE_DONE:
            return;
        }
    }
}

void gbB::start_computation()
{
  ncalls = 0;
  nloops = 0;
  nsaved_unneeded = 0;
  do_computation();
  if (M2_gbTrace >= 1)
    {
      show_mem_usage();
      if (M2_gbTrace >= 3)
        {
          buffer o;
          o << "ncalls = " << ncalls;
          emit_line(o.str());
          o.reset();
          o << "nloop = " << nloops;
          emit_line(o.str());
          o.reset();
          o << "nsaved = " << nsaved_unneeded;
          emit_line(o.str());
        }
      if (M2_gbTrace >= 15) show();
    }
}

/*******************************
 ** Minimalization of the GB ***
 *******************************/
void gbB::minimalize_gb()
{
  if (minimal_gb_valid) return;

  delete minimal_gb;
  minimal_gb = ReducedGB::create(originalR, F, Fsyz);

  VECTOR(POLY) polys;
  for (int i = first_gb_element; i < gb.size(); i++)
    {
      if (gb[i]->minlevel & ELEMB_MINGB) polys.push_back(gb[i]->g);
    }

  minimal_gb->minimalize(polys);
  minimal_gb_valid = true;
}

/*******************************
 ** Hilbert function routines **
 *******************************/

void gbB::flush_pairs()
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

Computation /* or null */ *gbB::set_hilbert_function(const RingElement *hf)
{
  // TODO Problems here:
  //  -- check that the ring is correct
  //  -- if the computation has already been started, this will fail
  //     So probably an error should be given, and 0 returned in this case.

  // We may only use the Hilbert function if syzygies are not being collected
  // since otherwise we will miss syzygies

  if (!_collect_syz) hilbert = new HilbertController(F, hf);
  return this;
}

const Matrix /* or null */ *gbB::get_gb()
{
  minimalize_gb();
  //  fprintf(stderr, "-- done with GB -- \n");
  return minimal_gb->get_gb();
}

const Matrix /* or null */ *gbB::get_mingens()
{
  MatrixConstructor mat(F, 0);
  for (VECTOR(gbelem *)::iterator i = gb.begin(); i != gb.end(); i++)
    if ((*i)->minlevel & ELEMB_MINGEN)
      mat.append(originalR->translate_gbvector_to_vec(F, (*i)->g.f));
  return mat.to_matrix();
}

const Matrix /* or null */ *gbB::get_change()
{
  minimalize_gb();
  return minimal_gb->get_change();
}

const Matrix /* or null */ *gbB::get_syzygies()
{
  // The (non-minimal) syzygy matrix
  MatrixConstructor mat(Fsyz, 0);
  for (VECTOR(gbvector *)::iterator i = _syz.begin(); i != _syz.end(); i++)
    {
      mat.append(originalR->translate_gbvector_to_vec(Fsyz, *i));
    }
  return mat.to_matrix();
}

const Matrix /* or null */ *gbB::get_initial(int nparts)
{
  minimalize_gb();
  return minimal_gb->get_initial(nparts);
}

const Matrix /* or null */ *gbB::get_parallel_lead_terms(M2_arrayint w)
{
  minimalize_gb();
  return minimal_gb->get_parallel_lead_terms(w);
}

const Matrix /* or null */ *gbB::matrix_remainder(const Matrix *m)
{
  minimalize_gb();
  return minimal_gb->matrix_remainder(m);
}

M2_bool gbB::matrix_lift(const Matrix *m,
                         const Matrix /* or null */ **result_remainder,
                         const Matrix /* or null */ **result_quotient)
{
  minimalize_gb();
  return minimal_gb->matrix_lift(m, result_remainder, result_quotient);
}

int gbB::contains(const Matrix *m)
// Return -1 if every column of 'm' reduces to zero.
// Otherwise return the index of the first column that
// does not reduce to zero.
{
  minimalize_gb();
  return minimal_gb->contains(m);
}

int gbB::complete_thru_degree() const
// The computation is complete up through this degree.
{
  return complete_thru_this_degree;
}

void gbB::text_out(buffer &o) const
/* This displays statistical information, and depends on the
   M2_gbTrace value */
{
  o << "# pairs computed = " << n_pairs_computed << newline;
  if (M2_gbTrace >= 5 && M2_gbTrace % 2 == 1)
    for (unsigned int i = 0; i < gb.size(); i++)
      {
        o << i << '\t';
        R->gbvector_text_out(o, F, gb[i]->g.f);
        o << newline;
      }
}

void gbB::debug_spair(spair *p)
{
  buffer o;
  spair_text_out(o, p);
  emit_line(o.str());
}

void gbB::debug_spairs(spair *spairlist)
{
  spair *p = spairlist;
  while (p != 0)
    {
      debug_spair(p);
      p = p->next;
    }
}

void gbB::debug_spair_array(spairs &spairlist)
{
  for (int i = 0; i < spairlist.size(); i++) debug_spair(spairlist[i]);
}

void gbB::show() const
{
  buffer o;
  o << "Groebner basis, " << gb.size() << " elements";
  emit_line(o.str());
  o.reset();
  for (unsigned int i = 0; i < gb.size(); i++)
    {
      gbelem_text_out(o, i);
      emit_line(o.str());
      o.reset();
    }
}

void gbB::show_mem_usage()
{
  buffer o;

  long nmonoms = 0;
  for (int i = 0; i < gb.size(); i++)
    {
      nmonoms += R->gbvector_n_terms(gb[i]->g.f);
      nmonoms += R->gbvector_n_terms(gb[i]->g.fsyz);
    }
  emit_line(o.str());
  o << "number of (nonminimal) gb elements = " << gb.size();
  emit_line(o.str());
  o.reset();
  o << "number of monomials                = " << nmonoms;
  emit_line(o.str());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// indent-tabs-mode: nil
// End:
