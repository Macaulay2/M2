// Copyright 2000  Michael E. Stillman

#include "EEspairs.hpp"
#include "ntuple.hpp"
#include "text_io.hpp"
//stash *es_pair::mystash;


///////////////////////
// ESPairLookupTable //
///////////////////////
// This class is simply used to select a minimal set of spairs.

class ESPairLookupTable
{
  // This structure is only used to find a minimal set
  // of spairs: one inserts elements one degree at a time, in increasing
  // degree order.
  // For each element of a new degree: one must check ALL elements
  // of lower degree, I guess.  Then choosing a unique pair
  // of each lcm must be done by the user, after which
  // the user appends these elements.
  struct node
  {
    node *next;
    es_pair *elem;
    unsigned int mask;
  };

  int nvars;
  node *table;  // This has a list head.
  node *last;
public:
  ESPairLookupTable(int nvars, es_pair *pairs);
  ~ESPairLookupTable();
  bool find_divisor(const exponent_vector *exp, es_pair *&result) const;
  void append(es_pair *&p);
  void append_list(es_pair *&pairs);
  es_pair *value();  // Returns a list of es_pair's.
};

ESPairLookupTable::ESPairLookupTable(int nvars, es_pair *pairs)
  : nvars(nvars), table(0), last(0)
{
  table = new node;  // A list header
  table->next = 0;
  last = table;
  append_list(pairs);
}
ESPairLookupTable::~ESPairLookupTable()
{
  last = 0;
  delete table;
  table = 0;
}
void ESPairLookupTable::append(es_pair *&p)
{
  if (p == 0) return;
  node *t = new node;
  t->elem = p;
  t->mask = ntuple::mask(nvars, p->lead_exponents());
  t->next = 0;
  last->next = t;
  last = t;
  p = 0;
}
void ESPairLookupTable::append_list(es_pair *&pairs)
{
  while (pairs != 0) {
    es_pair *tmp = pairs;
    pairs = pairs->next;
    append(tmp);
  }
}
bool ESPairLookupTable::find_divisor(const exponent_vector *exp, 
				     es_pair *&result) const
{
  unsigned int expmask = ~(ntuple::mask(nvars,exp));

  for (node *p = table->next; p != 0; p = p->next)
    if ((expmask & p->mask) == 0)
      {
	if (ntuple::divides(nvars, p->elem->lead_exponents(), exp))
	  {
	    result = p->elem;
	    return true;
	  }
      }
  return false;
}
es_pair *ESPairLookupTable::value()
  // Returns a list of es_pair's.
{
  es_pair head;
  es_pair *q = &head;
  node *p = table->next;
  table->next = 0;
  last = table;
  while (p != 0)
    {
      node *tmp;
      tmp = p;
      p = p->next;
      q->next = tmp->elem;
      q = q->next;
      delete tmp;
    }
  return head.next;
}

///////////////////////
// EEspairs routines //
///////////////////////

SPairSet::SPairSet()
  : heap(0), _nelems(0)
{
}

void SPairSet::insert(es_pair *p)
  // Insert a list of pairs.
{
  while (p != 0)
    {
      es_pair *tmp = p;
      p = p->next;
      _nelems++;
      tmp->next = heap;
      heap = tmp;
    }
}

int SPairSet::next_degree(int &nextdeg) const
  // Returns number to be done in nextdeg.
{
  if (heap == 0) return 0;
  int len = 1;
  nextdeg = heap->degree();
  for (es_pair *p = heap->next; p!=0; p=p->next)
    if (p->degree() > nextdeg) 
      continue;
    else if (p->degree() < nextdeg)
      {
	len = 1;
	nextdeg = p->degree();
      }
  else
    len++;
  return len;
}

int SPairSet::get_next_degree(int &deg)
// Return in 'elems' an unsorted list of elements in the lowest degree.
// This lowest degree is set in 'lodeg', and the number of pairs in the
// list is returned.
{
  this_set = 0;
  int len = next_degree(deg);
  if (len == 0) return 0;
  es_pair head;
  head.next = heap;
  es_pair *p = &head;
  while (p->next != 0)
    if (p->next->degree() != deg)
      p = p->next;
    else
      {
	es_pair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = this_set;
	this_set = tmp;
      }
  heap = head.next;
  _nelems -= len;
  _ndegree += len;

  // Now sort 'this_set'.
  sort_pairs(this_set);

  return len;
}

es_pair *SPairSet::remove_smallest()
{
  if (this_set == 0) 
    return 0;
  else {
    es_pair *result = this_set;
    this_set = this_set->next;
    result->next = 0;
    _ndegree--;
    return result;
  }
}

void SPairSet::stats(buffer &o)
  // Displays some statistics about this set.
{
  // TODO: keep track of the number of pairs in each degree.
  // display them here.
  // ALSO: what sets of pairs have been removed.
}

bool SPairSet::is_gcd_one_pair(es_pair *p) const
{
  if (p->type() != SP_SYZ) return false;
  const exponent_vector *e1 = p->first_exponents();
  const exponent_vector *e2 = p->second_exponents();
  for (int i=0; i<nvars; i++)
    if (e1[i] > 0 && e2[i] > 0)
      return false;
  return true;
}

void SPairSet::remove_pairs(es_pair *&pair_list) const
{
  while (pair_list != 0)
    {
      es_pair *tmp = pair_list;
      pair_list = tmp->next;
      MEM->remove_pair(tmp); // This does not remove any egb_elem's
    }
}

int SPairSet::compare_pairs(es_pair *f, es_pair *g) const
{
  // These pairs all have the same degree lcm's.
  int cmp = ntuple::lex_compare(nvars, f->lead_exponents(), g->lead_exponents());
  return cmp;
}

es_pair *SPairSet::merge_pairs(es_pair *f, es_pair *g) const
{
  // Sort in ascending degree order, then ascending monomial order
  if (g == NULL) return f;
  if (f == NULL) return g;
  es_pair head;
  es_pair *result = &head;
  while (1)
    switch (compare_pairs(f, g))
      {
      case LT:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f;
	    return head.next;
	  }
	break;
      case GT:
      case EQ:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    return head.next;
	  }
	break;
      }
}

void SPairSet::sort_pairs(es_pair *&p) const
{
  if (p == NULL || p->next == NULL) return;
  es_pair *p1 = NULL;
  es_pair *p2 = NULL;
  while (p != NULL)
    {
      es_pair *tmp = p;
      p = p->next;
      tmp->next = p1;
      p1 = tmp;

      if (p == NULL) break;
      tmp = p;
      p = p->next;
      tmp->next = p2;
      p2 = tmp;
    }

  sort_pairs(p1);
  sort_pairs(p2);
  p = merge_pairs(p1, p2);
}

void SPairSet::choose_nice_pair(es_pair *&p)
  // All of the pairs in the list should have the LCM.
  // This  routine chooses exactly one of them.
{
  if (comp_printlevel >= 5)
    {
      int len = 0; 
      for (es_pair *a = p; a!=0; a=a->next) len++;
      if (len > 1)
	{
	  buffer o;
	  if (comp_printlevel >= 7)
	    {
	      o << "choice of spair: ";
	      for (es_pair *q = p; q!= 0; q = q->next)
		{
		  o << "    ";
		  MEM->spair_out(o, q);
		}
	    }
	  else
	    o << "sp" << len;
	  emit(o.str());
	}
    }
  if (p->next == 0) return;
  if (is_ideal)
    {
      // See if one is a gcd 1 pair.
      // If so, keep that one.
      if (is_gcd_one_pair(p))
	{
	  es_pair *rest = p->next;
	  p->next = 0;
	  remove_pairs(rest);
	  return;
	}
      for (es_pair *q = p; q->next != 0; q = q->next)
	{
	  if (is_gcd_one_pair(q->next))
	    {
	      es_pair *answer = q->next;
	      q->next = answer->next;
	      remove_pairs(p);
	      p = answer;
	      p->next = 0;
	      _n_saved_gcd_choice++;
	      return;
	    }
	}
    }
  es_pair *rest = p->next;
  p->next = 0;
  remove_pairs(rest);
}

void SPairSet::choose_unique_pairs(es_pair *&p)
{
  if (p == 0) return;
  es_pair head;
  head.next = 0;
  es_pair *last = &head;
  while (p != 0)
    {
      es_pair *first = p;
      es_pair *q = first;
      while (q->next != 0 && ntuple::lex_compare(nvars, p->lead_exponents(), q->next->lead_exponents()) == EQ)
	q = q->next;
      p = q->next;
      q->next = 0;
      choose_nice_pair(first);
      last->next = first;  // Other elements have been removed
      last = first;
    }
  p = head.next;
}

bool SPairSet::pair_not_needed(es_pair *p, egb_elem *m) const
{
  // Check the criterion: in(m) divides lcm(p).
  // If so: check if lcm(p1,m) == lcm(p)  (if so, return false)
  //        check if lcm(p2,m) == lcm(p)  (if so, return false)
  // If still here, return true.
  if (p->type() != SP_SYZ && p->type() != SP_RING) return false;
  if (p->lead_component() != m->lead_component()) return false;
  const int *mexp = m->lead_exponents();
  const int *lcm = p->lead_exponents();
  const int *p1exp = p->first_exponents();
  const int *p2exp;
  if (p->type() == SP_SYZ)      // WRONG FOR others kinds of pairs!!!
    p2exp = p->second_exponents();  
  else
    p2exp = p->second_exponents();  
  int i;
  for (i=0; i<nvars; i++)
    if (mexp[i] > lcm[i]) return false;
      
  bool firstok = false;
  for (i=0; i<nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p1exp[i] == lcm[i]) continue;
      firstok = true;
      break;
    }
  if (!firstok) return false;
  for (i=0; i<nvars; i++)
    {
      if (mexp[i] == lcm[i]) continue;
      if (p2exp[i] == lcm[i]) continue;
      return true;
    }
  return false;
}

void SPairSet::minimalize_pairs(es_pair *&p)
{
  int d;
  if (p == 0) return;
  array<es_pair *> bins;

  // All the pairs in the list 'p' must have the same component.
  // Step 1: Divide up by actual degree of the monomial:

  while (p != 0)
    {
      es_pair *tmp = p;
      p = p->next;
      int d = ntuple::degree(nvars,tmp->lead_exponents());
      if (d >= bins.length())
	{
	  for (int i=bins.length(); i<=d; i++)
	    bins.append(0);
	}
      tmp->next = bins[d];
      bins[d] = tmp;
    }

  // Step 2: 

  for (d=0; bins[d] == 0; d++);
  sort_pairs(bins[d]);
  choose_unique_pairs(bins[d]);
  ESPairLookupTable table(nvars, bins[d]);
  bins[d] = 0;

  for (d++; d<bins.length(); d++)
    {
      es_pair *a, *c;
      es_pair head;
      head.next = bins[d];
      bins[d] = 0;

      // Check divisibility by previous stuff.  If not minimal
      // remove that pair.
      es_pair *b = &head;
      while (b->next != 0)
	if (table.find_divisor(b->next->lead_exponents(), c))
	  {
	    // Remove element:
	    es_pair *tmp = b->next;
	    b->next = tmp->next;
	    MEM->remove_pair(tmp);
	  }
	else
	  b = b->next;
      a = head.next;

      // Sort the list of remaining elements
      sort_pairs(a);

      // Check for duplicates: Choose one.
      choose_unique_pairs(a);

      // Place onto the list of s-pairs (At the end).
      table.append_list(a);
    }


  // Step 3:
  p = table.value(); // This list of pairs.

}

void SPairSet::remove_unneeded(egb_elem *m)
{
  // remove un-needed old pairs
  // NOTE: we don't need to check the elements of the current degree?

  // TODO: what about this_set?
  es_pair head;
  head.next = heap;
  es_pair *p = &head;
  while (p->next != 0)
    if (pair_not_needed(p->next, m))
      {
	es_pair *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
	MEM->remove_pair(tmp);
	_n_saved_lcm++;
	_nelems--;
      }
  else
    p = p->next;
  heap = head.next;
}

void SPairSet::insert_only_minimals(es_pair *& new_set)
{

  minimalize_pairs(new_set);

  while (new_set != 0)
    {
      es_pair *tmp = new_set;
      new_set = new_set->next;
      tmp->next = 0;
      if (is_ideal && is_gcd_one_pair(tmp))
	{
	  // can we remove this pair??
	  _n_saved_gcd++;
	  MEM->remove_pair(tmp);
	}
      else
	{
	  insert(tmp);
	}	
    }
}
