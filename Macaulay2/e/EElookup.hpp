// Copyright 2000  Michael E. Stillman
#ifndef _EElookup_hh_
#define _EElookup_hh_

#include "EEdefs.hpp"
#include "EEbasics.hpp"

class EGBLookupTable
{
  friend class iterator;
  struct node
  {
    node *next;
    egb_elem *elem;
    unsigned int mask;
  };
public:
  EGBLookupTable(const EInterface &II);

  ~EGBLookupTable();

  void insert(egb_elem *p);

  void insert(egb_elem *p, egb_elem * & replaced);
  // Makes no attempt to remove non-minimals.
  // BUT: will replace one with exactly the same lead term.
  // In this case, 'replaced' is the old element.  Otherwise,
  // 'replaced' is set to 0.
  // The elements are kept sorted in increasing monomial order.
  // If degree_first is true then (sugar) ascending degree is used first.

  void insert_w_deletions(egb_elem *p, array< egb_elem * > &nonminimals);

  void insert_w_deletions(egb_elem *p);

  bool find_divisor(const exponent_vector *exp, egb_elem * & result) const;
  // Finds the first divisor

  bool find_all_divisors(const exponent_vector *exp,array<egb_elem *> &result) const;
  // Finds all of the divisors.

  bool find_all_divisors_minimal_tdegree(const exponent_vector *exp, array<egb_elem *> &result) const;

  class iterator {
    node *p;
  public:
    iterator();
    iterator(node *p);

    bool valid() { return p != 0; }
    void operator++() { p = p->next; }
    egb_elem *operator*() { return p->elem; }
  };
};
class ERingTable
{
public:
  ERingTable(const EInterface &II);

  ~ERingTable();

  void insert(ering_elem *p);

  bool find_divisor(const exponent_vector *exp, ering_elem * & result) const;
  // Finds the first divisor

  bool find_all_divisors(const exponent_vector *exp,array<ering_elem *> &result) const;
  // Finds all of the divisors.

  bool find_all_divisors_minimal_tdegree(const exponent_vector *exp, array<ering_elem *> &result) const;
};




#if 0
// OLD, or not used code.

typedef EMonomialLookupTable<egb_elem> EGBLookupTable;

typedef EMonomialLookupTable<ering_elem> ERingTable;


template<class T>
class EMonomialLookupTable
{
  // Each instantiation must define a EMonomialLookupTable<T>::compare(T *a,T *b)
  // AND T should have a field named 'lcm' which is an exponent_vector *.
  struct node
  {
    node *next;
    T *elem;
    unsigned int mask;
  };

  const EInterface &I;
  int nvars;
  node *table;

  int compare(T *p, T *q) const;
  const exponent_vector *exponents(T *g) const;

public:
  EMonomialLookupTable(const EInterface &II);

  ~EMonomialLookupTable();

  void insert(T *p, T * & replaced);
  // Makes no attempt to remove non-minimals.
  // BUT: will replace one with exactly the same lead term.
  // In this case, 'replaced' is the old element.  Otherwise,
  // 'replaced' is set to 0.
  // The elements are kept sorted in increasing monomial order.
  // If degree_first is true then (sugar) ascending degree is used first.

  void insert_w_deletion(egb_elem *p, array< egb_elem * > &nonminimals);

  bool find_divisor(const exponent_vector *exp, T * & result) const;
  // Finds the first divisor

  bool find_all_divisors(const exponent_vector *exp,array<T *> &result) const;
  // Finds all of the divisors.

  bool find_all_divisors_minimal_tdegree(const exponent_vector *exp, array<egb_elem *> &result) const;

  class iterator {
    node *p;
  public:
    iterator() : p(0) {}
    iterator(node *p) : p(p) {}

    bool valid() { return p != 0; }
    void operator++() { p = p->next; }
    egb_elem *operator*() { return p->elem; }
  };

  iterator first() { return iterator(table); }
};


//////////////////////////
// EMonomialLookupTable //
//////////////////////////
template <class T>
EMonomialLookupTable<T>::EMonomialLookupTable(const EInterface &II)
  : I(II), 
    nvars(II.n_vars()),
    table(0)
{
  table = new node;
  table->next = 0;
}

template <class T>
EMonomialLookupTable<T>::~EMonomialLookupTable()
{
  // The callee should make sure that all egb_elem's have been removed first.
  while (table != 0)
    {
      node *tmp = table;
      table = table->next;
      tmp->next = 0;
      MEM->remove_gb_elem(tmp->elem);
      delete tmp;
    }
}

template <class T>
inline const exponent_vector *EMonomialLookupTable<T>::exponents(T *g) const
{
  return g->lcm;
}

template <class T>
void EMonomialLookupTable<T>::insert(T *g, T * & replaced)
{
  // 'table' is sorted in increasing monomial order (actual degree first)
  // So: search to the point where 'p' should be inserted, then remove elements
  // past that which are divisible by 'p'.

  replaced = 0;
  node *t = new node;
  t->next = 0;
  t->elem = g;
  const exponent_vector *exp = exponents(g);
  t->mask = ntuple::mask(nvars,exp);
  node *p;
  node head;
  head.next = table;
  for (p = &head; p->next != 0; p = p->next)
    {
      int cmp = compare(p->next->elem, g);
      if (cmp == LT) continue;
      if (cmp == EQ)
	{
	  // We replace these.
	  replaced = p->next->elem;
	  p->next->elem = g;
	}
      break;
    }
  t->next = p->next;
  p->next = t;
  table = head.next;
}

template <class T>
bool EMonomialLookupTable<T>::find_divisor(
	      const int *exp,
	      T * & result) const
{
  unsigned int expmask = ~(ntuple::mask(nvars,exp));

  for (node *p = table; p != 0; p = p->next)
    if ((expmask & p->mask) == 0)
      {
	if (ntuple::divides(nvars, exponents(p->elem), exp))
	  {
	    result = p->elem;
	    return true;
	  }
      }
  return false;
}

template <class T>
bool EMonomialLookupTable<T>::find_all_divisors(const int *exp, 
						   array<T *> &result) const
  // Return value: true means that a monomial divisor with lead coeff '1' was found.
{
  unsigned int expmask = ~(ntuple::mask(nvars,exp));

  for (node *p = table; p != 0; p = p->next)
    if ((expmask & p->mask) == 0)
      {
	if (ntuple::divides(nvars, exponents(p->elem), exp))
	  result.append(p->elem);
      }
  return (result.length() > 0);
}




















////////////////////////////////////////////////////////////////
template<class T>
class EMonomialLookupTable
{
  // This class keeps a sequence of T's.
  // The type T should have the following:
  //    t->lcm()
  //    t->degree()
  //    t->tdegree()
  // Way of comparing two monomials.

  // Each instantiation must define a EMonomialLookupTable<T>::compare(T *a,T *b)
  // AND T should have a field named 'lcm' which is an exponent_vector *.
  struct node
  {
    node *next;
    T *elem;
    unsigned int mask;
    int degree;  // This is the actual degree of the exponent vector
  };

  const EInterface &I;
  int nvars;
  node *table;

  int compare(T *p, T *q) const;
  const exponent_vector *exponents(T *g) const;

  node *next_divisor(node *start, int tdegree_max, const exponent_vector *exp, unsigned int expmask) const;
public:
  EMonomialLookupTable(const EInterface &II);

  ~EMonomialLookupTable();

  void insert(T *p, array<T *> &nonminimals);

  bool find_divisor(const exponent_vector *exp, T * & result) const;

  bool find_all_divisors(const exponent_vector *exp,array<T *> &result) const;

  //// iterator: loop through all elements ////
  class iterator {
    node *p;
  public:
    iterator() : p(0) {}
    iterator(node *p) : p(p) {}

    bool valid() { return p != 0; }
    void operator++() { p = p->next; }
    T *operator*() { return p->elem; }
  };

  iterator first() { return iterator(table); }

  //// divisor_iterator ////
  class divisor_iterator {
    EMonomialTable<T> *me;
    node *p;
    int tdegree_max;
    int expmask;
    const exponent_vector *exp;

    divisor_iterator() : p(0), expmask(0), exp(0) {}
    divisor_iterator(node *p, const exponent_vector *exp, unsigned int expmask) 
      : p(p), expmask(expmask), exp(exp)
      { }
  public:
    bool valid() { return p != 0; }
    void operator++() { p = me->next_divisor(p->next,tdegree_max,exp,expmask); }
    T *operator*() { return p->elem; }
  };

  divisor_iterator first_divisor(int tdegree_max, exponent_vector *exp)
    {
      unsigned int expmask = ~(ntuple::mask(nvars,exp));
      return divisor_iterator(this,table,tdegree_max,expmask,exp);
    }
};


//////////////////////////
// EMonomialLookupTable //
//////////////////////////
template <class T>
EMonomialLookupTable<T>::EMonomialLookupTable(const EInterface &II)
  : I(II), 
    nvars(II.n_vars()),
    table(0)
{
}

template <class T>
EMonomialLookupTable<T>::~EMonomialLookupTable()
{
  // The callee should make sure that all egb_elem's have been removed first.
  while (table != 0)
    {
      node *tmp = table;
      table = table->next;
      tmp->next = 0;
      tmp->elem = 0;  // THIS SHOULD HAVE BEEN REMOVED BY USER OF THIS ROUTINE.
      delete tmp;
    }
}

template <class T>
inline const exponent_vector *EMonomialLookupTable<T>::exponents(T *g) const
{
  return g->lcm;
}

template <class T>
node * EMonomialLookupTable<T>::insert(T *g)
{
  node *t = new node;
  t->next = 0;
  t->elem = g;
  const exponent_vector *exp = exponents(g);
  t->mask = ntuple::mask(nvars,exp);
  t->degree = ntuple::degree(nvars,exp);
  node *p;
  node head;
  head.next = table;
  for (p = &head; p->next != 0; p = p->next)
    {
      int cmp = compare(p->next->elem, g);
      if (cmp == LT) continue;
      // note that cmp == EQ should NOT happen
      break;
    }
  t->next = p->next;
  p->next = t;
  table = head.next;

  return t;
}

template <class T>
void EMonomialLookupTable<T>::insert(T *g)
{
  insert(g);
}

template <class T>
void EMonomialLookupTable<T>::insert_w_deletions(T *g, array<T *> &nonminimals)
{
  // 'table' is sorted in increasing monomial order (actual degree first)
  // So: search to the point where 'p' should be inserted.

  node *t = insert(g);

  // Now we must check and remove those which are divisible by 't'.
  node *p = t;
  while (p->next != 0)
    if (ntuple::divides(nvars, exp, exponents(p->next->elem)))
      {
	node *tmp = p->next;
	p->next = tmp->next;
	tmp->next = 0;
	nonminimals.append(tmp->elem);
	tmp->elem = 0;
	delete tmp;
      }
    else
      p = p->next;
}

template <class T>
node *EMonomialLookupTable<T>::next_divisor(node *start,
					    int tdegree_max, 
					    const exponent_vector *exp,
					    unsigned int expmask) const
{
  for (node *p = start; p != 0; p = p->next)
    if ((expmask & p->mask) == 0
	&& p->elem->tdegree <= tdegree_max
	&& ntuple::divides(nvars, exponents(p->elem), exp))
      {
	return p;
      }
  return 0;
}

template <class T>
bool EMonomialLookupTable<T>::find_divisor(
	      int tdegree_max,
	      const int *exp,
	      T * & result) const
{
  unsigned int expmask = ~(ntuple::mask(nvars,exp));
  node *p = first_divisor(tdegree_max, exp, expmask);
  return (p != 0);
}


template <class T>
bool EMonomialLookupTable<T>::find_all_divisors(
              int tdegree_max,
	      const int *exp, 
	      array<T *> &result) const
  // Return value: true means that a monomial divisor with lead coeff '1' was found.
{
  unsigned int expmask = ~(ntuple::mask(nvars,exp));

  for (node *p = table; p != 0; p = next_divisor(p->next, tdegree_max, exp, expmask))
    {
      result.append(p->elem);
    }
  return (result.length() > 0);
}



class EEMonomialLookupTable
{
  // This class keeps a sequence of T's.
  // The type T should have the following:
  //    t->lcm()
  //    t->degree()
  //    t->tdegree()
  // Way of comparing two monomials.
  struct node
  {
    node *next;
    egb_elem *elem;		// Includes ering_elem case...
    unsigned int mask;
    int degree;  // This is the actual degree of the exponent vector
  };
  
  const EInterface &I;

  int nvars;
  node *table;
public:
  EMonomialLookupTable(const EInterface &II);

  ~EMonomialLookupTable();

  void insert(egb_elem *p);

  void insert_w_deletion(egb_elem *p, vector< egb_elem * > &nonminimals);


  bool find_divisor(const exponent_vector *exp, egb_elem * & result) const;

  bool find_all_divisors(const exponent_vector *exp, vector<egb_elem *> &result) const;

  bool find_all_divisors_minimal_tdegree(const exponent_vector *exp, vector<egb_elem *> &result) const;

  class iterator {
    node *p;
  public:
    iterator() : p(0) {}
    iterator(node *p) : p(p) {}

    bool valid() { return p != 0; }
    void operator++() { p = p->next; }
    egb_elem *operator*() { return p->elem; }
  };

  iterator first() { return iterator(table); }
};
#endif
#endif
