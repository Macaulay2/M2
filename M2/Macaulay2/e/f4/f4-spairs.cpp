// Copyright 2005-2021 Michael E. Stillman

#include "f4/f4-spairs.hpp"

#include "mem.hpp"            // for stash
#include "style.hpp"          // for INTSIZE

#include <gc/gc_allocator.h>  // for gc_allocator
#include <stdint.h>           // for int32_t
#include <stdio.h>            // for fprintf, stderr, size_t
#include <algorithm>          // for stable_sort
#include <vector>             // for vector, vector<>::iterator

F4SPairSet::F4SPairSet(const MonomialInfo *M0, const gb_array &gb0)
    : M(M0), gb(gb0), heap(nullptr), this_set(nullptr), nsaved_unneeded(0)
{
  max_varpower_size = 2 * M->n_vars() + 1;

  spair *used_to_determine_size = nullptr;
  mSPairSizeInBytes = sizeofspair(used_to_determine_size, M->max_monomial_size());
}

F4SPairSet::~F4SPairSet()
{
  // Deleting the stash deletes all memory used here
  // PS, VP are deleted automatically.
  M = nullptr;
  heap = nullptr;
  this_set = nullptr;
}

spair *F4SPairSet::make_spair(spair_type type, int deg, int i, int j)
{
  spair *result = reinterpret_cast<spair*>(new char[mSPairSizeInBytes]);
  //  spair *result = new spair;
  result->next = nullptr;
  result->type = type;
  result->deg = deg;
  result->i = i;
  result->j = j;
  return result;
}

void F4SPairSet::insert_spair(pre_spair *p, int me)
{
  int j = p->j;
  int deg = p->deg1 + gb[me]->deg;
  // int me_component = M->get_component(gb[me]->f.monoms);

  spair *result = make_spair(F4_SPAIR_SPAIR, deg, me, j);

  M->from_varpower_monomial(p->quot, 0, result->lcm);
  M->unchecked_mult(result->lcm, gb[me]->f.monoms, result->lcm);

  result->next = heap;
  heap = result;
}

void F4SPairSet::delete_spair(spair *p) { delete p; }
void F4SPairSet::insert_generator(int deg, packed_monomial lcm, int col)
{
  spair *p = make_spair(F4_SPAIR_GEN, deg, col, -1);
  M->copy(lcm, p->lcm);
  p->next = heap;
  heap = p;
}

bool F4SPairSet::pair_not_needed(spair *p, gbelem *m)
{
  if (p->type != F4_SPAIR_SPAIR && p->type != F4_SPAIR_RING) return false;
  if (M->get_component(p->lcm) != M->get_component(m->f.monoms)) return false;
  return M->unnecessary(
      m->f.monoms, gb[p->i]->f.monoms, gb[p->j]->f.monoms, p->lcm);
}

int F4SPairSet::remove_unneeded_pairs()
{
  // Loop through every spair, determine if it can be jettisoned
  // and do so.  Return the number removed.

  // MES: Check the ones in this_set? Probably not needed...
  spair head;
  spair *p = &head;
  gbelem *m = gb[gb.size() - 1];
  int nremoved = 0;

  head.next = heap;
  while (p->next != nullptr)
    if (pair_not_needed(p->next, m))
      {
        nremoved++;
        spair *tmp = p->next;
        p->next = tmp->next;
        tmp->next = nullptr;
        delete_spair(tmp);
      }
    else
      p = p->next;
  heap = head.next;
  return nremoved;
}

int F4SPairSet::determine_next_degree(int &result_number)
{
  spair *p;
  int nextdeg;
  int len = 1;
  if (heap == nullptr)
    {
      result_number = 0;
      return 0;
    }
  nextdeg = heap->deg;
  for (p = heap->next; p != nullptr; p = p->next)
    if (p->deg > nextdeg)
      continue;
    else if (p->deg < nextdeg)
      {
        len = 1;
        nextdeg = p->deg;
      }
    else
      len++;
  result_number = len;
  return nextdeg;
}

int F4SPairSet::prepare_next_degree(int max, int &result_number)
// Returns the (sugar) degree being done next, and collects all (or at
// most 'max', if max>0) spairs in this lowest degree.
// Returns the degree, sets result_number.
{
  this_set = nullptr;
  int result_degree = determine_next_degree(result_number);
  if (result_number == 0) return 0;
  if (max > 0 && max < result_number) result_number = max;
  int len = result_number;
  spair head;
  spair *p;
  head.next = heap;
  p = &head;
  while (p->next != nullptr)
    if (p->next->deg != result_degree)
      p = p->next;
    else
      {
        spair *tmp = p->next;
        p->next = tmp->next;
        tmp->next = this_set;
        this_set = tmp;
        len--;
        if (len == 0) break;
      }
  heap = head.next;
  return result_degree;
}

spair *F4SPairSet::get_next_pair()
// get the next pair in this degree (the one 'prepare_next_degree' set up')
// returns 0 if at the end
{
  spair *result;
  if (!this_set) return nullptr;

  result = this_set;
  this_set = this_set->next;
  result->next = nullptr;
  return result;
}

int F4SPairSet::find_new_pairs(bool remove_disjoints)
// returns the number of new pairs found
{
  nsaved_unneeded += remove_unneeded_pairs();
  int len = construct_pairs(remove_disjoints);
  return len;
}

void F4SPairSet::display_spair(spair *p)
// A debugging routine which displays an spair
{
  if (p->type == F4_SPAIR_SPAIR)
    {
      fprintf(stderr, "[%d %d deg %d lcm ", p->i, p->j, p->deg);
      M->show(p->lcm);
      fprintf(stderr, "\n");
    }
  else
    {
      fprintf(stderr, "unknown type\n");
    }
}

void F4SPairSet::display()
// A debugging routine which displays the spairs in the set
{
  fprintf(stderr, "spair set\n");
  for (spair *p = heap; p != nullptr; p = p->next)
    {
      fprintf(stderr, "   ");
      display_spair(p);
    }
  fprintf(stderr, "current set\n");
  for (spair *p = this_set; p != nullptr; p = p->next)
    {
      fprintf(stderr, "   ");
      display_spair(p);
    }
}

////////////////////////////////
// Construction of new spairs //
////////////////////////////////

pre_spair *F4SPairSet::create_pre_spair(int j)
{
  // Steps:
  //  a. allocate the space for the pre_spair and the varpower monomial
  //  b. compute the quotient and the degree
  pre_spair *result = PS.allocate();
  result->quot = VP.reserve(max_varpower_size);
  result->j = j;
  result->type = F4_SPAIR_SPAIR;
  M->quotient_as_vp(gb[j]->f.monoms,
                    gb[gb.size() - 1]->f.monoms,
                    result->quot,
                    result->deg1,
                    result->are_disjoint);
  int len = static_cast<int>(varpower_monomials::length(result->quot));
  VP.intern(len);
  return result;
}

void insert_pre_spair(std::vector<std::vector<pre_spair *>> & bins, pre_spair *p)
{
  int d = p->deg1;
  if (d >= bins.size()) bins.resize(d + 1);
  bins[d].push_back(p);
}

long PreSPairSorter::ncmps = 0;

int F4SPairSet::construct_pairs(bool remove_disjoints)
{
  if (gb.size() == 0) return 0;

  VP.reset();
  PS.reset();
  gbelem *me = gb[gb.size() - 1];
  int me_component = static_cast<int>(M->get_component(me->f.monoms));

  std::vector<std::vector<pre_spair *>> bins;

  // Loop through each element of gb, and create the pre_spair
  for (int i = 0; i < gb.size() - 1; i++)
    {
      if (gb[i]->minlevel == ELEM_NON_MIN_GB) continue;
      if (me_component != M->get_component(gb[i]->f.monoms)) continue;
      pre_spair *p = create_pre_spair(i);
      insert_pre_spair(bins, p);
    }

  ////////////////////////////
  // Now minimalize the set //
  ////////////////////////////
  MonomialLookupTable *montab = new MonomialLookupTable(M->n_vars());

  PreSPairSorter C;
  int n_new_pairs = 0;
  for (int i = 0; i < bins.size(); i++)
    {
      if (bins[i].size() == 0) continue;
      // First sort the monomials of this degree

      std::stable_sort(bins[i].begin(), bins[i].end(), C);

      // Loop through each degree and potentially insert...
      auto first = bins[i].begin();
      auto next = first;
      auto end = bins[i].end();
      for (; first != end; first = next)
        {
          next = first + 1;
          pre_spair *chosen = *first;
          while (next != end)
            {
              pre_spair *p = *next;
              if (!varpower_monomials::is_equal(chosen->quot, p->quot)) break;
              next++;
            }
          /* At this point: [first,next) is the range of equal monomials */

          int32_t junk;
          bool inideal = montab->find_one_divisor_vp(0, chosen->quot, junk);
          if (!inideal)
            {
              // MES: Maybe choose another of the equal monomials...
              montab->insert_minimal_vp(0, chosen->quot, 0);
              // The following condition is that gcd is not one
              if (!remove_disjoints || !chosen->are_disjoint)
                {
                  insert_spair(chosen, INTSIZE(gb) - 1);
                  n_new_pairs++;
                }
            }
        }
    }
  delete montab;

  return n_new_pairs;
}

#if 0
// testing mathic and mathicgb routines...
class TestPairQueueConfiguration
{
private:
  // What should be here?
  
public:
  TestPairQueueConfiguration(const gb_array& gb,
                     XXX
                     );
  using PairData = MonomialInfo::OrderedMonomial;
  void computePairData(
                       size_t col,
                       size_t row,
                       PairData & m, // allocated space?
                       ) const;

  using CompareResult = bool;
  bool compare(
               size_t colA,
               size_t rowA,
               MonomialInfo::ConstOrderedMonomial a,
               size_t colB,
               size_t rowB,
               MonomialInfo::ConstOrderedMonomial b) const
  {
    // What to change this test code to?
    const auto cmp = orderMonoid().compare(*a, *b);
    if (cmp == GT)
      return true;
    if (cmp == LT)
      return false;
      
    const bool aRetired = mBasis.retired(rowA) || mBasis.retired(colA);
    const bool bRetired = mBasis.retired(rowB) || mBasis.retired(colB);
    if (aRetired || bRetired)
      return !bRetired;
      
    if (mPreferSparseSPairs) {
      const auto termCountA =
        mBasis.basisElement(colA).termCount() +
        mBasis.basisElement(rowA).termCount();
      const auto termCountB =
        mBasis.basisElement(colB).termCount() +
        mBasis.basisElement(rowB).termCount();
      if (termCountA > termCountB)
        return true;
      if (termCountA < termCountB)
        return false;
    }
    return colA + rowA > colB + rowB;
  }

  bool cmpLessThan(bool v) const {return v;}
};

class TestSPairs
{
private:
  mathic::PairQueue<TestPairQueueConfiguration> mPairQueue;

public:
  TestSPairs(gb_poly& currentGroebnerBasis);

  ~TestSPairs() {} // anything here?

  
};
#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  indent-tabs-mode: nil
//  End:
