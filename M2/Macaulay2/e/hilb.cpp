// Copyright 1996 Michael E. Stillman

#include "hilb.hpp"

#include <assert.h>   // for assert
#include <stdio.h>    // for fprintf, stderr
#include <algorithm>  // for max, stable_sort
#include <cstdlib>    // for NULL, abort
#include <limits>     // for numeric_limits
#include <utility>    // for pair, make_pair
#include <vector>     // for vector

#include "ExponentList.hpp"    // for index_varpower, varpower, const_v...
#include "ExponentVector.hpp"  // for exponents
#include "M2mem.h"             // for freemem
#include "buffer.hpp"          // for buffer
#include "error.h"             // for ERROR
#include "freemod.hpp"         // for FreeModule
#include "int-bag.hpp"         // for Bag, int_bag
#include "interrupted.hpp"     // for system_interrupted
#include "matrix.hpp"          // for Matrix
#include "mem.hpp"             // for stash
#include "monideal.hpp"        // for MonomialIdeal, operator!=, Nmi_node
#include "monoid.hpp"          // for Monoid
#include "polyring.hpp"        // for PolynomialRing
#include "relem.hpp"           // for RingElement
#include "ring.hpp"            // for Ring

int partition_table::representative(int x)
{
  int i = x;
  while (dad[i] >= 0) i = dad[i];
  int j = x;
  while (j != i)
    {
      int t = dad[j];
      dad[j] = i;
      j = t;
    }
  return i;
}

int partition_table::merge_in(int x, int y)
// Returns the label representative of the merge of the two sets
{
  int t, newtop;
  int i = x;
  int j = y;
  while (dad[i] >= 0) i = dad[i];
  while (dad[j] >= 0) j = dad[j];
  if (i == j) return i;
  n_sets--;
  if (dad[i] > dad[j])
    {
      newtop = j;
      dad[newtop] += dad[i];
      dad[i] = newtop;
    }
  else
    {
      newtop = i;
      dad[newtop] += dad[j];
      dad[j] = newtop;
    }
  while (dad[x] >= 0)
    {
      t = x;
      x = dad[x];
      dad[t] = newtop;
    }
  while (dad[y] >= 0)
    {
      t = y;
      y = dad[y];
      dad[t] = newtop;
    }

  return newtop;
}

void partition_table::merge_in(const_varpower m)
// merge in the varpower monomial 'm'.
{
  index_varpower i = m;
  int var1 = i.var();
  occurs[var1] = 1;
  ++i;
  for (; i.valid(); ++i)
    {
      var1 = merge_in(var1, i.var());
      occurs[i.var()] = 1;
    }
}

partition_table::partition_table(int nvars, stash *mi_stash0)
    : n_vars(nvars),
      n_sets(nvars),
      adad(nvars),
      aoccurs(nvars),
      mi_stash(mi_stash0)
{
  dad = adad.data();
  occurs = aoccurs.data();
  for (int i = 0; i < nvars; i++)
    {
      dad[i] = -1;
      occurs[i] = 0;
    }
}

void partition_table::reset(int nvars)
{
  n_vars = nvars;
  n_sets = nvars;
  for (int i = 0; i < nvars; i++)
    {
      dad[i] = -1;
      occurs[i] = 0;
    }
}
void partition_table::partition(MonomialIdeal *&I,
                                gc_vector<MonomialIdeal*>& result)
// consumes and frees I
{
  int k;
  reset(I->topvar() + 1);
  // Create the sets
  for (Bag& a : *I)
    if (n_sets > 1)
      merge_in(a.monom().data());
    else
      break;

  if (n_sets == 1)
    {
      result.push_back(I);
      return;
    }

  int this_label = -1;
  n_sets = 0;
  for (k = 0; k < n_vars; k++)
    if (occurs[k] && dad[k] < 0)
      {
        dad[k] = this_label--;
        n_sets++;
      }

  if (n_sets == 1)
    {
      result.push_back(I);
      return;
    }

  int first = result.size();
  for (k = 0; k < n_sets; k++)
    result.push_back(new MonomialIdeal(I->get_ring(), mi_stash));

  // Now partition the monomials
  Bag *b;
  while (I->remove(b))
    {
      int v = varpower::topvar(b->monom().data());
      int loc = -1 - dad[representative(v)];
      result[first + loc]->insert_minimal(b);
    }

  delete I;
}

#define MAX_EXP (1 << (8 * sizeof(int) - 2))
static int popular_var(const MonomialIdeal &I,
                       int &npure,
                       exponents_t pure,
                       int &nhits,
                       const_varpower &non_pure_power,
                       int &exp_of_popular)
// Return the variable which occurs the most often in non pure monomials,
// placing the number of monomials in which it occurs into 'nhits'.
// At the same time, set 'pure' and 'npure'.  If there is a non pure
// power monomial, set non_pure_power to this varpower monomial.
{
  int k;
  npure = 0;
  int nvars = I.topvar() + 1;
  for (k = 0; k < nvars; k++) pure[k] = -1;

  exponents_t hits = newarray_atomic_clear(int, nvars);
  exponents_t minnonzero = newarray_atomic(int, nvars);
  for (k = 0; k < nvars; k++) hits[k] = 0;
  for (k = 0; k < nvars; k++) minnonzero[k] = MAX_EXP;

  non_pure_power = nullptr;

  for (Bag& a : I)
    {
      const_varpower m = a.monom().data();
      index_varpower j = m;
      assert(j.valid());  // The monomial cannot be '1'.
      int v = j.var();
      int e = j.exponent();
      ++j;
      if (j.valid())
        {
          non_pure_power = m;
          for (; j.valid(); ++j)
            {
              v = j.var();
              e = j.exponent();
              if (minnonzero[v] > e) minnonzero[v] = e;
              hits[v]++;
            }
        }
      else
        {
          if (pure[v] == -1)
            {
              npure++;
              pure[v] = e;
            }
          else if (pure[v] > e)
            pure[v] = e;
        }
    }

  int popular = 0;
  for (k = 1; k < nvars; k++)
    if (hits[k] > hits[popular]) popular = k;
  nhits = hits[popular];
  exp_of_popular = minnonzero[popular];
  freemem(hits);
  freemem(minnonzero);
  return popular;
}

static int find_pivot(const MonomialIdeal &I,
                      int &npure,
                      exponents_t pure,
                      gc_vector<int> &m)
// If I has a single monomial which is a non pure power, return 0,
// and set 'pure', and 'npure'.  If I has at least two non pure
// monomials, choose a monomial 'm', not in I, but at least of degree 1,
// which is suitable for divide and conquer in Hilbert function
// computation.
{
  int nhits;
  int exp_of_v;
  const_varpower vp;
  int v = popular_var(I, npure, pure, nhits, vp, exp_of_v);

  // The following will take some tweaking...
  // Some possibilities: take just this variable, take gcd of 2 or 3
  // elements containing this variable (choose first 3, last 3, or randomly).

  // For now, let's just take this variable, if there is more than one
  // non pure power.
  if (npure >= I.size() - 1)
    {
      assert(vp != NULL);
      varpower::copy(vp, m);
      return 0;
    }
  varpower::var(v, exp_of_v, m);
  return 1;
}

static void iquotient_and_sum(MonomialIdeal &I,
                              const_varpower m,
                              MonomialIdeal *&quot,
                              MonomialIdeal *&sum,
                              stash *mi_stash)
{
  gc_vector<Bag*> elems;
  sum = new MonomialIdeal(I.get_ring(), mi_stash);
  quot = new MonomialIdeal(I.get_ring(), mi_stash);
  Bag *bmin = new Bag();
  varpower::copy(m, bmin->monom());
  sum->insert_minimal(bmin);
  for (Bag& a : I)
    {
      Bag *b = new Bag();
      varpower::quotient(a.monom().data(), m, b->monom());
      if (varpower::divides(m, a.monom().data()))
        quot->insert_minimal(b);
      else
        {
          sum->insert_minimal(new Bag(0, a.monom()));
          elems.push_back(b);
        }
    }

  // Now we sort items in 'elems' so that we can insert as min gens into 'quot'
  std::vector<std::pair<int, int>> degs_and_indices;
  int count = 0;
  for (auto& b : elems)
    {
      int deg = varpower::simple_degree(b->monom().data());
      degs_and_indices.push_back(std::make_pair(deg, count));
      ++count;
    }
  std::stable_sort(degs_and_indices.begin(), degs_and_indices.end());

  for (auto p : degs_and_indices)
    {
      Bag* b = elems[p.second];
      Bag* b1; // not used here...
      if (quot->search(b->monom().data(), b1))
        delete b;
      else
        quot->insert_minimal(b);
    }
}

void hilb_comp::next_monideal()
{
  reset();
  MonomialIdeal *I = input_mat->make_monideal(this_comp);
  // The above line adds the squares of the variables which are skew variables
  // into the monomial ideal.  This allows Hilbert functions of such rings
  // to be computed as usual.
  part_table.partition(I, current->monids);
  current->i = current->monids.size() - 1;
  current->first_sum = current->i + 1;  // This part is not used at top level
}
void hilb_comp::reset()
{
  depth = 0;
  if (current == nullptr)
    {
      current = new hilb_step;
      current->up = current->down = nullptr;
      current->h0 = R->from_long(0);
      current->h1 = R->from_long(0);
    }
  else
    while (current->up != nullptr) current = current->up;

  R->remove(current->h0);  // This line should not be needed...
  R->remove(current->h1);
  current->h0 = R->from_long(0);  // This top level h0 is not used
  current->h1 = R->from_long(1);
}
hilb_comp::hilb_comp(const PolynomialRing *RR, const Matrix *m)
    : S(m->get_ring()->cast_to_PolynomialRing()),
      R(RR),
      M(S->getMonoid()),
      D(S->degree_monoid()),
      mi_stash(new stash("hilb mi", sizeof(Nmi_node))),
      input_mat(m),
      this_comp(0),
      n_components(m->n_rows()),
      current(nullptr),
      part_table(S->n_vars(), mi_stash)
{
  assert(D == R->getMonoid());
  one = R->getCoefficientRing()->from_long(1);
  minus_one = R->getCoefficientRing()->from_long(-1);
  LOCAL_deg1 = D->make_one();

  result_poincare = R->from_long(0);
  nsteps = 0;
  maxdepth = 0;
  nideal = 0;
  nrecurse = 0;

  // Collect the 'this_comp' monideal of the input matrix
  // Set up the computation for that
  next_monideal();
}

hilb_comp::hilb_comp(const PolynomialRing *RR, const MonomialIdeal *I)
    : S(I->get_ring()->cast_to_PolynomialRing()),
      R(RR),
      M(S->getMonoid()),
      D(S->degree_monoid()),
      mi_stash(new stash("hilb mi", sizeof(Nmi_node))),
      input_mat(nullptr),
      this_comp(0),
      n_components(1),
      current(nullptr),
      part_table(S->n_vars(), mi_stash)
{
  assert(D == R->getMonoid());
  one = R->getCoefficientRing()->from_long(1);
  minus_one = R->getCoefficientRing()->from_long(-1);
  LOCAL_deg1 = D->make_one();

  result_poincare = R->from_long(0);
  nsteps = 0;
  maxdepth = 0;
  nideal = 0;
  nrecurse = 0;

  reset();
  MonomialIdeal *copyI = I->copy();
  part_table.partition(copyI, current->monids);
  current->i = current->monids.size() - 1;
  current->first_sum = current->i + 1;  // This part is not used at top level
}

hilb_comp::~hilb_comp()
{
  // free 'current' (which is most of the stuff here...)
  while (current != nullptr)
    {
      hilb_step *p = current;
      current = current->down;

      R->remove(p->h0);
      R->remove(p->h1);
      for (int i = 0; i < p->monids.size(); i++) delete p->monids[i];
      delete p;
    }

  R->remove(result_poincare);
  R->getCoefficientRing()->remove(one);
  R->getCoefficientRing()->remove(minus_one);
  D->remove(LOCAL_deg1);
  delete mi_stash;
}

int hilb_comp::calc(int n_steps)
// Possible return values: COMP_DONE, COMP_INTERRUPTED.
{
  if (n_components == 0) return COMP_DONE;
  if (n_steps >= 0)
    {
      int calc_nsteps = nsteps + n_steps;
      while (calc_nsteps-- > 0)
        {
          int result = step();
          if (result == COMP_DONE) return COMP_DONE;
          if (system_interrupted()) return COMP_INTERRUPTED;
        }
      return COMP_DONE_STEPS;
    }
  else
    for (;;)
      {
        int result = step();
        if (result == COMP_DONE) return COMP_DONE;
        if (system_interrupted()) return COMP_INTERRUPTED;
      }
}

int hilb_comp::step()
// Possible return values: COMP_DONE, COMP_COMPUTING
{
  nsteps++;
  if (current->i == current->first_sum)
    {
      current->h0 = current->h1;
      current->h1 = R->from_long(1);
    }
  if (current->i >= 0)
    {
      // If the i th monomial ideal is a 'special case', simply compute its
      // value
      // Otherwise, find pivot, and quotient/sum creating next level down
      // (possibly make new hilb_step, and increment max_depth if needed)
      // and set current = current->down
      do_ideal(current->monids[current->i]);  // consumes this monomial ideal
    }
  else
    {
      // At this point, all Hilbert functions at this level have been
      // computed, so add the values, and place one step up
      R->add_to(current->h0, current->h1);
      ring_elem f = current->h0;
      current->h0 = R->from_long(0);
      current->h1 = R->from_long(0);
      current->monids.clear();
      if (current->up == nullptr)
        {
          if (input_mat)
            {
              ring_elem tmp =
                  R->make_flat_term(one, input_mat->rows()->degree(this_comp));
              R->mult_to(f, tmp);
              R->remove(tmp);
            }
          R->add_to(result_poincare, f);
          this_comp++;

          // Now check if we have any more components
          if (this_comp >= n_components) return COMP_DONE;
          // otherwise go on to the next component:

          next_monideal();
          return COMP_COMPUTING;
        }
      current = current->up;
      R->mult_to(current->h1, f);
      current->i--;
      R->remove(f);
    }
  return COMP_COMPUTING;
}

void hilb_comp::recurse(MonomialIdeal *&I, const_varpower pivot_vp)
{
  depth++;
  if (depth > maxdepth) maxdepth = depth;
  nrecurse++;
  if (current->down == nullptr)
    {
      current->down = new hilb_step;  // MES: is this ok?
      current->down->up = current;
      current->down->down = nullptr;
    }
  current = current->down;
  current->h0 = R->from_long(0);
  M->degree_of_varpower(pivot_vp, LOCAL_deg1);
  current->h1 = R->make_flat_term(one, LOCAL_deg1);  // t^(deg vp)
  MonomialIdeal *quot, *sum;
  iquotient_and_sum(*I, pivot_vp, quot, sum, mi_stash);
  delete I;
  part_table.partition(sum, current->monids);
  current->first_sum = current->monids.size() - 1;
  part_table.partition(quot, current->monids);
  current->i = current->monids.size() - 1;
}

void hilb_comp::do_ideal(MonomialIdeal *I)
{
  // This either will multiply to current->h1, or it will recurse down
  // Notice that one, and minus_one are global in this class.
  // LOCAL_deg1, LOCAL_vp are scratch variables defined in this class
  nideal++;
  ring_elem F = R->from_long(1);
  ring_elem G;
  int len = I->size();
  if (len <= 2)
    {
      // len==1: set F to be 1 - t^(deg m), where m = this one element
      // len==2: set F to be 1 - t^(deg m1) - t^(deg m2) + t^(deg lcm(m1,m2))
      M->degree_of_varpower(I->first_elem(), LOCAL_deg1);
      G = R->make_flat_term(minus_one, LOCAL_deg1);
      R->add_to(F, G);

      if (len == 2)
        {
          M->degree_of_varpower(I->second_elem(), LOCAL_deg1);
          G = R->make_flat_term(minus_one, LOCAL_deg1);
          R->add_to(F, G);
          varpower::lcm(I->first_elem(), I->second_elem(), LOCAL_vp);
          M->degree_of_varpower(LOCAL_vp.data(), LOCAL_deg1);
          G = R->make_flat_term(one, LOCAL_deg1);
          R->add_to(F, G);
        }
      delete I;
    }
  else
    {
      int npure;
      gc_vector<int> pivot;
      gc_vector<int> pure_a(I->topvar() + 1);
      exponents_t pure = pure_a.data();
      if (!find_pivot(*I, npure, pure, pivot))
        {
          // set F to be product(1-t^(deg x_i^e_i))
          //              - t^(deg pivot) product((1-t^(deg x_i^(e_i - m_i)))
          // where e_i >= 1 is the smallest number s.t. x_i^(e_i) is in I,
          // and m_i = exponent of x_i in pivot element (always < e_i).
          M->degree_of_varpower(pivot.data(), LOCAL_deg1);
          G = R->make_flat_term(one, LOCAL_deg1);
          for (index_varpower i = pivot.data(); i.valid(); ++i)
            if (pure[i.var()] != -1)
              {
                ring_elem H = R->from_long(1);
                D->power(M->degree_of_var(i.var()), pure[i.var()], LOCAL_deg1);
                ring_elem tmp = R->make_flat_term(minus_one, LOCAL_deg1);
                R->add_to(H, tmp);
                R->mult_to(F, H);
                R->remove(H);

                H = R->from_long(1);
                D->power(M->degree_of_var(i.var()),
                         pure[i.var()] - i.exponent(),
                         LOCAL_deg1);
                tmp = R->make_flat_term(minus_one, LOCAL_deg1);
                R->add_to(H, tmp);
                R->mult_to(G, H);
                R->remove(H);
              }
          R->subtract_to(F, G);
          delete I;
        }
      else
        {
          // This is the one case in which we recurse down
          R->remove(F);
          recurse(I, pivot.data());
          return;
        }
    }
  R->mult_to(current->h1, F);
  R->remove(F);
  current->i--;
}

int hilb_comp::is_done() const
{
  return (current != nullptr && current->up == nullptr);
}

RingElement *hilb_comp::value()
{
  if (!is_done())
    {
      ERROR("Hilbert function computation not complete");
      return nullptr;
    }
  RingElement *result = RingElement::make_raw(R, R->copy(result_poincare));
  return result;
}

void hilb_comp::stats() const
{
  buffer o;
  o << "--- Hilbert Function Statistics---------------------" << newline;
  o << "#steps        = " << nsteps << newline;
  o << "Max depth     = " << maxdepth << newline;
  o << "current depth = " << depth << newline;
  o << "#ideal        = " << nideal << newline;
  o << "#recurse      = " << nrecurse << newline;

  hilb_step *p = current;
  int d = depth;
  while (p != nullptr)
    {
      o << "----- depth " << d << " -------------" << newline;
      o << "  " << p->monids.size() << " monomial ideals total" << newline;
      o << "  " << p->i << " = current location" << newline;
      o << "  " << p->first_sum + 1 << " sum monomial ideals" << newline;
      o << "  h0 = ";
      R->elem_text_out(o, p->h0);
      o << newline;
      o << "  h1 = ";
      R->elem_text_out(o, p->h1);
      o << newline;
      for (int i = 0; i < p->monids.size(); i++)
        {
          o << "  ---- monomial ideal ---------------" << newline;
          o << "  ";
          p->monids[i]->text_out(o);
          o << newline;
        }
      p = p->up;
      d--;
    }
}
#if 0
// int hilb_comp::hilbertSeries(const Matrix *M, RingElement *&result)
// {
//   const PolynomialRing *P = M->get_ring()->get_degree_ring();
//   hilb_comp *hf = new hilb_comp(P,M);
//   int retval = hf->calc(-1);
//   if (retval != COMP_DONE) return 1;
//   result = hf->value();
//   freemem(hf);
//   return 0;
// }
#endif
RingElement *hilb_comp::hilbertNumerator(const Matrix *M)
/* This routine computes the numerator of the Hilbert series
   for coker leadterms(M), using the degrees of the rows of M.
   NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
  const PolynomialRing *P = M->get_ring()->get_degree_ring();
  if (P == nullptr) return nullptr;
  hilb_comp *hf = new hilb_comp(P, M);
  int retval = hf->calc(-1);
  if (retval != COMP_DONE) return nullptr;
  RingElement *result = hf->value();
  delete hf;
  return result;
}

RingElement *hilb_comp::hilbertNumerator(const FreeModule *F)
{
  const Matrix *Fmatrix = Matrix::make(F, 0, nullptr);
  RingElement *result = hilbertNumerator(Fmatrix);
  delete Fmatrix;
  return result;
}

RingElement /* or null */ *hilb_comp::hilbertNumerator(const MonomialIdeal *I)
/* This routine computes the numerator of the Hilbert series
   for coker I.   NULL is returned if the ring is not appropriate for
   computing Hilbert series, or the computation was interrupted. */
{
  const PolynomialRing *P = I->get_ring()->get_degree_ring();
  if (P == nullptr) return nullptr;
  hilb_comp *hf = new hilb_comp(P, I);
  int retval = hf->calc(-1);
  if (retval != COMP_DONE) return nullptr;
  RingElement *result = hf->value();
  delete hf;
  return result;
}

int hilb_comp::coeff_of(const RingElement *h, int deg)
{
  // This is a bit of a kludge of a routine.  The idea is to loop through
  // all the terms of the polynomial h, expand out the exponent, and to add
  // up the small integer values of the coefficients of those that have
  // exp[0]=deg.
  const PolynomialRing *P = h->get_ring()->cast_to_PolynomialRing();

  exponents_t exp = newarray_atomic(int, P->n_vars());
  int result = 0;
  for (Nterm& f : h->get_value())
    {
      P->getMonoid()->to_expvector(f.monom, exp);
      if (exp[0] < deg)
        {
          ERROR("incorrect Hilbert function given");
          fprintf(
              stderr,
              "internal error: incorrect Hilbert function given, aborting\n");
          fprintf(
              stderr,
              "exp[0]: %d   deg: %d\n", exp[0], deg);    
          abort();
        }
      else if (exp[0] == deg)
        {
          std::pair<bool, long> res =
              P->getCoefficientRing()->coerceToLongInteger(f.coeff);
          assert(res.first &&
                 std::abs(res.second) < std::numeric_limits<int>::max());
          int n = static_cast<int>(res.second);
          result += n;
        }
    }
  freemem(exp);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
