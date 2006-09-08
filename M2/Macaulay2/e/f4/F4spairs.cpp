// Copyright 2005 Michael E. Stillman

#include "F4spairs.hpp"

F4SPairSet::F4SPairSet(MonomialInfo *M0)
  : M(M0),
    heap(0),
    this_set(0)
{
  spair_stash = new stash("spairs",sizeof(spair));
}

F4SPairSet::~F4SPairSet()
{
#ifdef DEVELOPMENT
#warning "write deallocator"
#endif
  // Remove the spairs
  // TO BE WRITTEN
}

spair *F4SPairSet::make_spair(int deg, 
			      packed_monomial lcm, 
			      int i,
			      int j)
{
  spair *result = reinterpret_cast<spair *>(spair_stash->new_elem());
  result->next = 0;
  result->type = F4_SPAIR_SPAIR;
  result->deg = deg;
  result->lcm = lcm;
  result->i = i;
  result->j = j;
  return result;
}

void F4SPairSet::insert_spair(int deg, 
			      packed_monomial lcm, 
			      int i,
			      int j)
{
  spair *p = make_spair(deg, lcm, i, j);
  p->next = heap;
  heap = p;
}

void F4SPairSet::delete_spair(spair *p)
{
  if (p->type == F4_SPAIR_GEN)
    gen_stash->delete_elem(p);
  else
    spair_stash->delete_elem(p);
}

spair *F4SPairSet::make_spair_gen(int deg, packed_monomial lcm, int col)
{
  spair *result = reinterpret_cast<spair *>(gen_stash->new_elem());
  result->next = 0;
  result->type = F4_SPAIR_GEN;
  result->deg = deg;
  result->lcm = lcm;
  result->i = col;
  return result;
}

void F4SPairSet::insert_generator(int deg, packed_monomial lcm, int col)
{
  spair *p = make_spair_gen(deg, lcm, col);
  p->next = heap;
  heap = p;
}

bool F4SPairSet::pair_not_needed(spair *p, gbelem *m, const gb_array &gb)
{
  if (p->type != F4_SPAIR_SPAIR && p->type != F4_SPAIR_RING) return false;
  return M->unnecessary(m->f.monom_space, 
			gb[p->i]->f.monom_space,
			gb[p->j]->f.monom_space,
			p->lcm);
}

int F4SPairSet::remove_unneeded_pairs(const gb_array &gb)
{
  // Loop through every spair, determine if it can be jettisoned
  // and do so.  Return the number removed.

  // MES: Check the ones in this_set? Probably not needed...
  spair head;
  spair *p = &head;
  gbelem *m = gb[gb.size()-1];
  int nremoved = 0;

  head.next = heap;
  while (p->next != 0)
    if (pair_not_needed(p->next, m, gb))
      {
	nremoved++;
	spair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
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

int F4SPairSet::prepare_next_degree(int max, int &result_number)
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

spair *F4SPairSet::get_next_pair()
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

int F4SPairSet::find_new_pairs(const gb_array &gb,
			       bool remove_disjoints)
  // returns the number of new pairs found
{
  remove_unneeded_pairs(gb);

  // Steps:
  //  1. Create an array of varpower_monomial's, which are
  //     the quotients of gb[i].monom_space by gb[last].monom_space
  //     BUT: as varpower's.  Only add in the ones corresponding to
  //     this component.  (But also add in the ones coming from
  //     quotients and from skew commuting variables.
  //  2. Sort these by degree, and then by some monomial order in each degree
  //  3. Minimalize this set.  Don't bother keeping the monomial lookup table...
  //       However: when considering a monomial, there may be several the same.
  //       How to choose the best one?
  //  4. For each of these minimal ones, make an s-pair andplace it into the heap.
  
  //  int len = SPairConstructor::make(MI,this,gb,remove_disjoints);
  //return len;
  return 0;
}

void F4SPairSet::display_spair(spair *p)
  // A debugging routine which displays an spair
{
  if (p->type == F4_SPAIR_SPAIR)
    {
      fprintf(stderr,"[%d %d deg %d lcm ", 
	      p->i,
	      p->j,
	      p->deg);
      M->show(p->lcm);
      fprintf(stderr,"\n");
    }
  else
    {
      fprintf(stderr, "unknown type\n");
    }
}

void F4SPairSet::display()
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

void F4SPairConstructor::find_quot(packed_monomial a,
				   packed_monomial b,
				   varpower_monomial result,
				   int & deg,
				   bool & are_disjoint)
{
}

pre_spair *
F4SPairConstructor::create_pre_spair(int i)
{
  // Steps: 
  //  a. allocate the space for the pre_spair and the varpower monomial
  //  b. compute the quotient and the degree
  pre_spair *result = P.allocate();
  result->quot = B.reserve(max_varpower_size);
  result->first_gb_num = i;
  result->type = F4_SPAIR_SPAIR;
  find_quot(gb[i]->f.monom_space, 
	    gb[gb.size()-1]->f.monom_space, 
	    result->quot, 
	    result->deg1, 
	    result->are_disjoint);
  B.intern(varpower_monomials::length(result->quot));
  return result;
}

void F4SPairConstructor::send_spair(pre_spair *p)
{
  int i = gb.size()-1;
  int j = p->first_gb_num;
  int deg = p->deg1 + gb[i]->deg;

  packed_monomial lcm = B.reserve(M->max_monomial_size());
  packed_monomial first = B.reserve(M->max_monomial_size());
  M->from_varpower_monomial(p->quot, me_component, first);
  M->unchecked_mult(first, gb[j]->f.monom_space, lcm);
  B.intern(M->monomial_size(lcm));

  S->insert_spair(deg, lcm, i, j);
}

F4SPairConstructor::F4SPairConstructor(MonomialInfo *M0,
				       F4SPairSet *S0,
				       const gb_array &gb0,
				       bool remove_disjoints0)
  : M(M0),
    S(S0),
    gb(gb0),
    remove_disjoints(remove_disjoints0),
    me(gb[gb.size()-1])
{
  me_component = M->get_component(me->f.monom_space);
  max_varpower_size = 2 * M->n_vars() + 1;
}

void F4SPairConstructor::insert_pre_spair(VECTOR(VECTOR(pre_spair *) *) &bins, pre_spair *p)
{
  int d = p->deg1;
  if (d >= bins.size())
    for (int i=bins.size(); i<=d; i++)
      bins.push_back(NULL);
  if (bins[d] == NULL)
    bins[d] = new VECTOR(pre_spair *);
  bins[d]->push_back(p);
}

int F4SPairConstructor::construct_pairs()
{
  if (gb.size() == 0) return 0; // NOT VALID if quotient ring.
  typedef VECTOR(pre_spair *) spairs;

  VECTOR( VECTOR(pre_spair *) *) bins;


  // Loop through each element of gb, and create the pre_spair
  for (int i=0; i<gb.size()-1; i++)
    {
      if (gb[i]->minlevel == ELEM_NON_MIN_GB) continue;
      if (me_component != M->get_component(gb[i]->f.monom_space)) continue;
      // Normally: need to check the component here
      pre_spair *p = create_pre_spair(i);
      insert_pre_spair(bins, p);
    }

  /////////////////////////////////////////////////////
  // Sort these so that ones of lowest degree are first, all ones with the
  // same quot monomial come together, and any disjoint ones come first.
  /////////////////////////////////////////////////////
  //  sort(new_set.begin(), new_set.end(), pre_spair_sorter<gb_array>());

  ////////////////////////////
  // Now minimalize the set //
  ////////////////////////////
  MonomialLookupTable *montab = new MonomialLookupTable;

  int n_new_pairs = 0;
  for (int i=0; i<bins.size(); i++)
    {
      // First sort the monomials of this degree

      // Loop through each degree and potentially insert...
      spairs::iterator first = bins[i]->begin();
      spairs::iterator next = first;
      spairs::iterator end = bins[i]->end();
      for ( ; first != end; first = next)
	{
	  next = first+1;
	  pre_spair *chosen = *first;
	  while (next != end)
	    {
	      pre_spair *p = *next;
	      if (!varpower_monomials::equal(chosen->quot, p->quot)) break;
	      next++;
	    }
	  /* At this point: [first,next) is the range of equal monomials */

	  int junk;
	  bool inideal = montab->find_one_divisor_vp(0, chosen->quot, junk);
	  if (!inideal)
	    {
	      // MES: Maybe choose another of the equal monomials...
	      montab->insert_minimal_vp(0, chosen->quot, 0);
	      // The following condition is that gcd is not one
	      if (!remove_disjoints || !chosen->are_disjoint)
		{
		  send_spair(chosen);
		  n_new_pairs++;
		}
	    }
	}
    }
  delete montab;
      
  return n_new_pairs;
  ///////////////////////////////////////
  // Now collect the minimal ones, and //
  // make spairs out of them           //
  // Intern all of the monomials       //
  ///////////////////////////////////////
}

int F4SPairConstructor::make(MonomialInfo *M0,
			     F4SPairSet *S0,
			     const gb_array &gb0,
			     bool remove_disjoints0)
{
  F4SPairConstructor C(M0,S0,gb0,remove_disjoints0);
  return C.construct_pairs();
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
