// Copyright 1996  Michael E. Stillman
#ifndef _termideal_hh_
#define _termideal_hh_

#include "queue.hh"
#include "ring.hh"
#include "spair.hh"

struct monterm
{
  monterm * next;
  monterm * prev;

  int       coeff_is_one;
  int       expmask;
  int      *exp;

  const int *monom;
  ring_elem coeff;

  gb_elem * bag;

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class TermIdeal
{
  const Ring *K;		// A p.i.d or a field?
  const monoid *M;
  const PolynomialRing *R;
  int nvars;
  ring_elem one;

  monterm *terms;
  int count;


  int exp_divides(const int *e, const int *f) const;
  int monomial_mask(const int *exp) const;

  void link(monterm *s, monterm *t);
  void unlink(monterm *s);
public:
  TermIdeal(const Ring *R);
  ~TermIdeal();

  monterm *new_monterm(ring_elem coeff, const int *monom, gb_elem *bag) const;
  monterm *new_monterm_head() const;
  void delete_monterm(monterm *&t) const;

  // Comparison of terms
  int compare(monterm *s, monterm *t) const;

  // Insertion of new monomials.  
  monterm *insert_minimal(monterm *t);
  //int insert(monterm *&t);
  void insert_w_deletions(monterm *t, queue<monterm *> &deletions);

  // Removal


  // Finding divisors of monomials
  int find_first(const int *exp, monterm *&result) const;
  //int find_lowest(const int *exp, monterm *&result) const;
  //int find_all(const int *exp, queue<monterm *> &result) const;
  
  // Finding monomials which a given monomial divides
  //int find_divisee(const int *exp, queue<monterm *> &result);
  
  // Mark non-minimals

  // Find pairs (?)

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};
#endif


