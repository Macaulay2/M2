// Copyright 2004 Michael E. Stillman

#include "linalgGB.hpp"
#include "SPairSet.h"
#include "monoms.h"
#include <functional>
#include <algorithm>
#include "MonomialTable.h"
#include "MonomialOps.h"

SPairSet::SPairSet(MonomialSet *H0)
  : H(H0),
    heap(0),
    this_set(0)
{
}

SPairSet::~SPairSet()
{
#warning "write deallocator"
  // Remove the spairs
  // TO BE WRITTEN
}

SPairSet::spair *SPairSet::make_spair(int deg, 
				      monomial lcm, 
				      monomial first_monom,
				      int first_gb_num,
				      monomial second_monom,
				      int second_gb_num)
{
  spair *result = new spair;
  result->next = 0;
  result->type = SPAIR_SPAIR;
  result->deg = deg;
  result->lcm = lcm;
  result->s.spair.first_monom = first_monom;
  result->s.spair.second_monom = second_monom;
  result->s.spair.first_gb_num = first_gb_num;
  result->s.spair.second_gb_num = second_gb_num;
  return result;
}

SPairSet::spair *SPairSet::make_spair_gen(int deg, monomial lcm, int col)
{
  spair *result = new spair;
  result->next = 0;
  result->type = SPAIR_GEN;
  result->deg = deg;
  result->lcm = lcm;
  result->s.poly.column = col;
  return result;
}

int SPairSet::remove_unneeded_pairs()
  // returns the number of pairs removed

{
  // Loop through every spair, determine if it can be jettisoned
  // and do so.  Return the number removed.

  // Check the ones in this_set?
#warning "unneeded pairs"
  return 0;
}

int SPairSet::determine_next_degree(int &result_number)
{
  spair *p;
  int nextdeg;
  int len = 1;
  if (heap == 0) 
    {
      result_number = 0;
      return 0;
    }
  nextdeg = heap->deg;
  for (p = heap->next; p!=0; p=p->next)
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

int SPairSet::prepare_next_degree(int max, int &result_number)
  // Returns the (sugar) degree being done next, and collects all (or at
  // most 'max', if max>0) spairs in this lowest degree.
  // Returns the degree, sets result_number.
{
  this_set = 0;
  int result_degree = determine_next_degree(result_number);
  if (result_number == 0) return 0;
  if (max > 0 && max < result_number)
    result_number = max;
  int len = result_number;
  spair head;
  spair *p;
  head.next = heap;
  p = &head;
  while (p->next != 0)
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

SPairSet::spair *SPairSet::get_next_pair()
  // get the next pair in this degree (the one 'prepare_next_degree' set up')
  // returns 0 if at the end
{
  spair *result;
  if (!this_set) return 0;

  result = this_set;
  this_set = this_set->next;
  result->next = 0;
  return result;
}

void SPairSet::insert(SPairSet::spair *p)
{
  p->next = heap;
  heap = p;
}
int SPairSet::find_new_pairs(const gb_array &gb,
			     bool remove_disjoints)
  // returns the number of new pairs found
{
  remove_unneeded_pairs();
  int len = SPairConstructor::make(H,this,gb,remove_disjoints);
  return len;
}

void SPairSet::display_spair(spair *p)
  // A debugging routine which displays an spair
{
  if (p->type == SPAIR_SPAIR)
    {
      fprintf(stderr,"[%d %d deg %d lcm ", 
	      p->s.spair.first_gb_num,
	      p->s.spair.second_gb_num,
	      p->deg);
      monomial_elem_text_out(stderr,p->lcm);
      fprintf(stderr," first ");
      monomial_elem_text_out(stderr,p->s.spair.first_monom);
      fprintf(stderr," second ");
      monomial_elem_text_out(stderr,p->s.spair.second_monom);
      fprintf(stderr,"\n");
    }
  else
    {
      fprintf(stderr, "unknown type\n");
    }
}

void SPairSet::display()
  // A debugging routine which displays the spairs in the set
{
  fprintf(stderr,"spair set\n");
  for (spair *p = heap; p != 0; p=p->next)
    {
      fprintf(stderr, "   ");
      display_spair(p);
    }
  fprintf(stderr,"current set\n");
  for (spair *p = this_set; p != 0; p=p->next)
    {
      fprintf(stderr, "   ");
      display_spair(p);
    }
}

////////////////////////////////
// Construction of new spairs //
////////////////////////////////

SPairConstructor::pre_spair *SPairConstructor::create_pre_spair(int i)
{
  // Construct the pre_spair (gb.size()-1, i)
  monomial m0 = gb[gb.size()-1]->f.monoms[0];
  monomial m1 = gb[i]->f.monoms[0];
  uninterned_monomial m = MonomialOps::quotient(B, m1, m0);
#if 0
  int len = MONOMIAL_LENGTH(m1);
  monomial m = B.reserve(len);
  monomial_quotient(m1, m0, m);
  B.intern(MONOMIAL_LENGTH(m));
#endif
  pre_spair *result = new pre_spair;
  result->next = 0;
  result->quot = m;
  result->first_gb_num = i;
  result->deg1 = monomial_simple_degree(m);
  result->deg2 = monomial_simple_degree(m1);
  return result;
}

SPairConstructor::SPairConstructor(MonomialSet* H0,
				   SPairSet *S0,
				   const gb_array &gb0,
				   bool remove_disjoints0)
  : H(H0),
    S(S0),
    gb(gb0),
    remove_disjoints(remove_disjoints0)
{
}

struct pre_spair_sorter : public binary_function<SPairConstructor::pre_spair *,
			                         SPairConstructor::pre_spair *,
                                                 bool>
{
  pre_spair_sorter() {}
  bool operator()(SPairConstructor::pre_spair *a, 
		  SPairConstructor::pre_spair *b)
  {
    /* Compare using degree, then quotient, then degree of other spair */
    /* should implement "<" */
    bool result;
    int cmp = a->deg1 - b->deg1;
    if (cmp < 0) result = true;
    else if (cmp > 0) result = false;
    else {
      int cmp2 = monomial_compare(a->quot, b->quot);
      if (cmp2 < 0) result = true;
      else if (cmp2 >= 0) result = false;
    }
    return result;
  }
};

void SPairConstructor::send_spair(pre_spair *p)
{
  monomial quot1;
  H->find_or_insert(p->quot, quot1);
  int i = gb.size()-1;
  int j = p->first_gb_num;
  int deg = monomial_simple_degree(quot1) + gb[i]->deg;
  monomial lcm = MonomialOps::mult(H,gb[i]->f.monoms[0], quot1);
  monomial first = quot1;
  monomial second = MonomialOps::quotient(H,
					  gb[i]->f.monoms[0],
					  gb[j]->f.monoms[0]);
  SPairSet::spair *result = SPairSet::make_spair(deg, lcm, first, i, second, j);

  // Now ship off to the spair set
  S->insert(result);
}

int SPairConstructor::construct_pairs()
{
  if (gb.size() == 0) return 0; // NOT VALID if quotient ring.
  typedef vector<pre_spair *,gc_allocator<pre_spair*> > spairs;
  spairs new_set;

  // Loop through each element of gb, and create the pre_spair
  for (int i=0; i<gb.size()-1; i++)
    {
      if (!gb[i]->is_minimal) continue;
      // Normally: need to check the component here
      pre_spair *p = create_pre_spair(i);
      new_set.push_back(p);
    }

  /////////////////////////////////////////////////////
  // Sort these so that ones of lowest degree are first, all ones with the
  // same quot monomial come together, and any disjoint ones come first.
  /////////////////////////////////////////////////////
  sort(new_set.begin(), new_set.end(), pre_spair_sorter());

  ////////////////////////////
  // Now minimalize the set //
  ////////////////////////////
  MonomialLookupTable *montab = new MonomialLookupTable;

  spairs::iterator first = new_set.begin();
  spairs::iterator next = first;
  spairs::iterator end = new_set.end();
  int n_new_pairs = 0;
  for ( ; first != end; first = next)
    {
      next = first+1;
      pre_spair *me = *first;
      while (next != end)
	{
	  pre_spair *p = *next;
	  if (!monomial_equal(me->quot, p->quot)) break;
	  next++;
	}
      /* At this point: [first,next) is the range of equal monomials */
      
      tagged_monomial *junk;
      int inideal = montab->search(me->quot, junk);
      if (inideal == 0)
	{
	  spairs::iterator t = first;// Maybe make a better choice?
	  pre_spair *p = *t;
	  montab->insert_minimal(new tagged_monomial(p->quot,0));
	  // The following condition is that gcd is not one
	  if (!remove_disjoints || p->deg1 != p->deg2)
	    {
	      send_spair(p);
	      n_new_pairs++;
	    }
	  *t = 0;
	}
    }

  deleteitem(montab);

  return n_new_pairs;
  ///////////////////////////////////////
  // Now collect the minimal ones, and //
  // make spairs out of them           //
  // Intern all of the monomials       //
  ///////////////////////////////////////
}

int SPairConstructor::make(MonomialSet* H0,
			   SPairSet *S0,
			   const gb_array &gb0,
			   bool remove_disjoints0)
{
  SPairConstructor C(H0,S0,gb0,remove_disjoints0);
  return C.construct_pairs();
}



// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
