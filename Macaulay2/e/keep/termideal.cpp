// Copyright 1996  Michael E. Stillman

#include "termideal.hh"
#include "polyring.hh"

TermIdeal::TermIdeal(const Ring *RR)
{
  R = RR->cast_to_poly_ring();
  assert(R != NULL);
  M = R->Nmonoms();
  K = R->Ncoeffs();
  nvars = R->n_vars();
  one = K->from_int(1);
  bump_up(R);			// Don't bother bumping the others

  terms = new_monterm_head();
  count = 0;
}

TermIdeal::~TermIdeal()
{
  while (terms->next != terms)
    delete_monterm(terms->next);
  delete_monterm(terms);
  K->remove(one);
  bump_down(R);
}

int TermIdeal::monomial_mask(const int *exp) const
{
  int result = 0;
  int i,j;
  for (i=0, j=0; i<nvars; i++, j++)
    {
      if (j == 8*sizeof(int)) j=0;
      if (exp[i] > 0)
	result |= (1 << j);
    }
  return result;
}

monterm *TermIdeal::new_monterm(ring_elem coeff, const int *monom, gb_elem *bag) const
{
  monterm *result = new monterm;
  result->next = result->prev = NULL;
  result->coeff_is_one = (K->is_equal(coeff, one));
  result->exp = new int[nvars];
  M->to_expvector(monom, result->exp);
  result->coeff = coeff;
  result->monom = monom;
  result->bag = bag;
  return result;
}

monterm *TermIdeal::new_monterm_head() const
{
  monterm *result = new monterm;
  result->next = result->prev = result;
  result->coeff_is_one = 0;
  result->exp = NULL;
  result->coeff = (Nterm*)NULL;
  result->monom = NULL;
  result->bag = NULL;
  return result;
}

void TermIdeal::delete_monterm(monterm *&p) const
{
  if (p == NULL) return;
  if (p->next != NULL) p->next->prev = p->prev;
  if (p->prev != NULL) p->prev->next = p->next;
  delete [] p->exp;
  p = NULL;
}

void TermIdeal::link(monterm *s, monterm *t)
{
  // Place 't' in the list containing 's' BEFORE 's'.
  t->next = s;
  t->prev = s->prev;
  s->prev = t;
  t->prev->next = t;
}

void TermIdeal::unlink(monterm *s)
{
  s->next->prev = s->prev;
  s->prev->next = s->next;
  s->next = NULL;
  s->prev = NULL;
}

int TermIdeal::exp_divides(const int *e, const int *f) const
{
  for (int i=0; i<nvars; i++)
    if (e[i] > f[i]) return 0;
  return 1;
}

monterm *TermIdeal::insert_minimal(monterm *t)
{
  // Inserts 't' into the TermIdeal.  If the monomial is already a member,
  // then the old one is replaced with this new one.  This is one way to replace
  // terms with smaller coefficients.
  // Terms are sorted in increasing order.

  for (monterm *s = terms->next; s != terms; s=s->next)
    {
      int cmp = compare(s,t);
      if (cmp == LT)
	{
	  continue;
	}
      else if (cmp == GT)
	{
	  link(s,t);
	  count++;
	}
      else
	{
	  t->next = s->next;
	  t->prev = s->prev;
	  t->next->prev = t;
	  t->prev->next = t;
	  s->prev = NULL;
	  s->next = NULL;
	  return s;
	}
    }
  return NULL;
}


void TermIdeal::insert_w_deletions(monterm *t, queue<monterm *> &deletions)
{
  // Since the elements are stored in increasing degree order, we may 
  // insert 't', and then look for deletions from this point on
  monterm *s = insert_minimal(t);
  if (s != NULL) deletions.insert(s);
  for (s = t->next; s != terms; s=s->next)
    {
      if (exp_divides(t->exp, s->exp))
	{
	  unlink(s);
	  deletions.insert(s);
	}
    }
}

int TermIdeal::find_first(const int *exp, monterm *&result) const
{
  int expmask = ~(monomial_mask(exp));

  for (monterm *t = terms->next; t != terms; t=t->next)
    if ((expmask & t->expmask) == 0)
      {
	int is_div = 1;
	for (int i=0; i<nvars; i++)
	  if (exp[i] < t->exp[i])
	    {
	      is_div = 0;
	      break;
	    }
	if (is_div)
	  {
	    result = t;
	    return 1;
	  }
      }
  return 0;
}

