#include "montable.hpp"
#include <functional>
#include <algorithm>

/********************/
/* Support routines */
/********************/

static bool exponents_equal(int nvars, exponents a, exponents b)
{
  for (int i=0; i<nvars; i++)
    if (a[i] != b[i]) return false;
  return true;
}

static bool exponents_greater(int nvars, exponents a, exponents b)
{
  for (int i=0; i<nvars; i++)
    {
      if (a[i] < b[i]) return false;
      if (a[i] > b[i]) return true;
    }
  return false;
}

static void exponents_show(FILE *fil, exponents exp, int nvars)
  /* This is only for debugging */
{
  fprintf(fil, "[");
  for (int i=0; i<nvars; i++)
    fprintf(fil, "%d ", exp[i]);
  fprintf(fil, "]");
}

static unsigned long monomial_mask(int nvars, exponents exp)
{
  unsigned long result = 0;
  int i,j;
  for (i=0, j=0; i<nvars; i++, j++)
    {
      if (j == 8*sizeof(long)) j=0;
      if (exp[i] > 0)
	result |= (1 << j);
    }
  return result;
}

/********************/
/* MonomialTable ****/
/********************/

MonomialTable::mon_term *MonomialTable::make_list_head()
{
  mon_term *t = new mon_term;
  t->_next = t->_prev = t;
  t->_val = -1;
  t->_lead = 0;
  return t;
}

MonomialTable *MonomialTable::make(int nvars)
{
  MonomialTable *result;
  result = new MonomialTable;
  result->_nvars = nvars;
  result->_count = 0;

  /* The first entry is a dummy entry.  Components 
     will always start at 1. */
  result->_head.push_back(0);

  return result;
}

MonomialTable::~MonomialTable()
{
  /* Loop through each component, and remove all mon_terms */
  for (unsigned int i=1; i<_head.size(); i++)
    {
      mon_term *t = _head[i];
      while (t->_next != t)
	{
	  mon_term *tmp = t->_next;
	  tmp->_prev->_next = tmp->_next;
	  tmp->_next->_prev = t;
	  delete tmp;
	}
      _head[i] = 0;
    }
  _count = 0;
}

int MonomialTable::find_divisors(int max,
				 exponents exp,
				 int comp,
				 vector< mon_term * > *result) const
{
  assert(comp >= 1);
  if (comp >= (int)_head.size()) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  int nmatches = 0;
  unsigned long expmask = ~(monomial_mask(_nvars, exp));

  for (t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      {
	bool is_div = 1;
	for (i=0; i<_nvars; i++)
	  if (exp[i] < t->_lead[i])
	    {
	      is_div = 0;
	      break;
	    }
	if (is_div)
	  {
	    nmatches++;
	    if (result != 0) result->push_back(t);
	    if (max >= 0 && nmatches >= max) break;
	  }
      }
  return nmatches;
}

MonomialTable::mon_term *MonomialTable::find_exact(exponents exp, int comp) const
{
  if (comp >= (int)_head.size()) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  unsigned long expmask = monomial_mask(_nvars, exp);

  for (t = head->_next; t != head; t = t->_next)
    if (expmask == t->_mask)
      {
	bool is_eq = 1;
	for (i=0; i<_nvars; i++)
	  if (exp[i] != t->_lead[i])
	    {
	      is_eq = 0;
	      break;
	    }
	if (is_eq)
	  return t;
      }
  return 0;
}

void MonomialTable::insert(exponents exp, int comp, int id)
{
  /* Insert 'exp' into the monomial table.  These are kept sorted in ascending order
     in some order (lex order?).  No element is ever removed.
  */

  if (comp >= (int)_head.size())
    {
      for (int i=_head.size(); i <= comp; i++)
	_head.push_back(make_list_head());
    }

  mon_term *head = _head[comp];
  mon_term *t;

  /* Make a new mon_term including exp */
  mon_term *newterm = new mon_term;
  newterm->_lead = exp;
  newterm->_mask = monomial_mask(_nvars,exp);
  newterm->_val = id;

  _count++;

  /* Find where to put it */
  for (t=head; t->_next != head; t = t->_next)
    {
      if (exponents_greater(_nvars, newterm->_lead, t->_next->_lead))
	{
	  /* Time to insert newterm, right between t, t->next */
	  break;
	}
    }	  

  /* The actual insertion */
  newterm->_next = t->_next;
  newterm->_prev = t;
  t->_next->_prev = newterm;
  t->_next = newterm;
}

/****************************
 * Minimalization ***********
 ****************************/

struct sorter : public binary_function<exponents,exponents,bool> {
  int nvars;
  const vector<exponents> &exps;
  const vector<int> &comps;
  sorter(int nvars, 
	 const vector<exponents> &exps,
	 const vector<int> &comps) 
    : nvars(nvars), exps(exps), comps(comps) {}
  bool operator()(int x, int y) {
    exponents xx = exps[x];
    exponents yy = exps[y];
    for (int i=0; i<nvars; i++)
      if (xx[i] < yy[i]) return true;
    if (comps[x] < comps[y]) return true;
    return false;
  }
};

void MonomialTable::minimalize(int nvars, 
			       const vector<exponents> &exps,
			       const vector<int> &comps,
			       bool keep_duplicates, 
			       vector<int> &result_positions)
{
  /* Step 1: Sort an intarray into ascending order.  In this order, if e divides f, then e should appear
     before f. Don't actually change 'exp'.  Need a special compare routine.  */

  /* Step 2: Make a monomial table. */

  /* Step 3: Loop through each element in the intarray.  If the exponent is not in the
     monomial ideal, put that index into the result, and insert into the monomial ideal.
     If it is in the monomial ideal, go on. */

  /* Step alternate3: If ALL minimal elements are to be taken. (e.g. if [1,1,0] is
     minimal, but occurs more than once, then keep all occurrences of [1,1,0]. */

  /* Step 4: Remove the monomial table.  Note that the exp vectors should not 
     be recreated. */

  MonomialTable *T;

  vector<int> positions;
  positions.reserve(exps.size());
  for (unsigned int i=0; i<exps.size(); i++)
    positions.push_back(i);

  /* The following sorts in ascending lex order, considering the component and the
     inhomogeneous part of the exponent vector */
  sort(positions.begin(), positions.end(), sorter(nvars,exps,comps));

  T = MonomialTable::make(nvars);

  vector<int>::iterator first, end;
  first = positions.begin();
  end = positions.end();
  while (first != end)
    {
      vector<int>::iterator next = first+1;
      exponents this_exp = exps[*first];
      int comp = comps[*first];
      while (next != end)
	{
	  if (!exponents_equal(nvars, this_exp, exps[*next])) break;
	  if (comp != comps[*next]) break;
	  next++;
	}
      if (T->find_divisors(1, this_exp, comp) == 0)
	{
	  /* We have a minimal element */

	  T->insert(this_exp, comp, *first);
	  result_positions.push_back(*first);
	  if (keep_duplicates)
	    while (++first != next)
	      result_positions.push_back(*first);
	}
      
      first = next;
      /* At this point: [first,next) is the range of equal monomials */
    }
  delete T;
}

MonomialTable *MonomialTable::make_minimal(int nvars, 
					   const vector<exponents> &exps,
					   const vector<int> &comps,
					   const vector<int> &vals,
					   vector<int> &rejects)
{
  MonomialTable *T;

  vector<int> positions;
  positions.reserve(exps.size());
  for (unsigned int i=0; i<exps.size(); i++)
    positions.push_back(i);

  /* The following sorts in ascending lex order, considering the component and the
     inhomogeneous part of the exponent vector */
  sort(positions.begin(), positions.end(), sorter(nvars,exps,comps));

  T = MonomialTable::make(nvars);

  vector<int>::iterator first, end, last_minimal;
  first = positions.begin();
  end = positions.end();
  last_minimal = first;
  while (first != end)
    {
      vector<int>::iterator next = first+1;
      exponents this_exp = exps[*first];
      int comp = comps[*first];
      while (next != end)
	{
	  if (!exponents_equal(nvars, this_exp, exps[*next])) break;
	  if (comp != comps[*next]) break;
	  rejects.push_back(*next);
	  next++;
	}
      if (T->find_divisors(1, this_exp, comp) == 0)
	{
	  /* We have a minimal element */

	  T->insert(this_exp, comp, vals[*first]);
	  *last_minimal++ = *first;
	}
      else
	rejects.push_back(*first);
      
      first = next;
      /* At this point: [first,next) is the range of equal monomials */
    }
  return T;
}

void MonomialTable::show(FILE *fil)
{
  mon_term *t,*head;
  /* Loop through each component, display monomials(val) 10 per line */
  fprintf(fil, "monomial table: %d vars, %d components, %d elements\n",
	  this->_nvars, (int)_head.size(), this->_count);
  for (unsigned i=1; i<_head.size(); i++)
    {
      head = this->_head[i];
      if (head->_next == head) continue;
      fprintf(fil,"  -- component %d --\n",i);
      for (t = head->_next; t != head; t = t->_next)
	{
	  exponents_show(fil,t->_lead,_nvars);
	  fprintf(fil," (%d)\n",t->_val);
	}
    }
  fprintf(fil,"\n");
}
