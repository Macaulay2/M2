#include "ntuple.hpp"
#include "Einterface.hpp"
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
    int degree;  // This is the actual degree of the exponent vector
  };

  const EInterface &I;
  int nvars;
  node *table;

  int compare(T *p, T *q) const;
  const exponent_vector *exponents(T *g) const;
public:
  EMonomialLookupTable(const EInterface &II);

  ~EMonomialLookupTable();

  void insert(T *p, array<T *> &nonminimals);

  bool find_divisor(const exponent_vector *exp, T * & result) const;

  bool find_all_divisors(const exponent_vector *exp,array<T *> &result) const;

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
void EMonomialLookupTable<T>::insert(T *g, array<T *> &nonminimals)
{
  // 'table' is sorted in increasing monomial order (actual degree first)
  // So: search to the point where 'p' should be inserted, then remove elements
  // past that which are divisible by 'p'.

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

  // Now we must check and remove those which are divisible by 't'.
  p = t;
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
