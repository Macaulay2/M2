// Copyright 1996  Michael E. Stillman
#ifndef _termideal_hpp_
#define _termideal_hpp_

#include "queue.hpp"
#include "ring.hpp"
#include "newspair.hpp"

struct mon_term
{
  mon_term * next;
  mon_term * prev;

  int       coeff_is_one;
  int       expmask;
  int       degree;

  GB_elem * elem;		// We use the following fields:
				// f (for coeff, monom)
				// fsyz
				// lead_exp
				// f, fsyz may be modified by various routines
				// in this class.

  const ring_elem coeff() const { return elem->f->coeff; }
  const int *monom() const { return elem->f->monom; }
  const int *lead_exp() const { return elem->lead_exp; }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

class TermIdeal : public type
{
private:
  const Ring *K;		// A p.i.d or a field?
  const Monoid *M;
  const PolynomialRing *R;
  const FreeModule *F;		// free module for mon_term->elem->f
  const FreeModule *Fsyz;	// free module for mon_term->elem->fsyz
  int nvars;
  ring_elem one;

  mon_term *terms;
  int count;

private:
  int exp_divides(const int *e, const int *f) const;
  int monomial_mask(const int *exp) const;

  void link(mon_term *s, mon_term *t);
  void unlink(mon_term *s);

  GB_elem *new_gb_elem(const vec f, const vec fsyz, int me) const;
  mon_term *mult_mon_term(const int *m, mon_term *p) const;
  mon_term *new_mon_term(GB_elem *bag) const;
  mon_term *new_mon_term_head() const;
  void delete_mon_term(mon_term *&t) const;

  // Sorting an array of mon_terms
  int sort_compare(const mon_term *p, const mon_term *q) const;
  int sort_partition(mon_term *a[], int lo, int hi);
  void sort(mon_term *a[], int lo, int hi);

  mon_term *gcd(array<mon_term *> &elems, const int *m) const;
  void select_non_divisors(mon_term **a, int nelems, mon_term *g,
				    array<mon_term *> &result_divs) const;

  // Comparison of terms
  int compare(mon_term *s, mon_term *t) const;
public:
  TermIdeal(const FreeModule *F, const FreeModule *Fsyz);
				// It is valid for these to be NULL, if
				// there is no baggage...
  ~TermIdeal();

  // Creation
  void from_list(queue<mon_term *> &elems);
  static TermIdeal *make_termideal(const Matrix &m, int n);
  void append_to_matrix(Matrix m, int i) const;
  Matrix change_matrix() const;

  // Insertion of new monomials.
  mon_term *insert_minimal(mon_term *t);
  int insert(mon_term *&t); 
  void insert_w_deletions(mon_term *t, queue<mon_term *> &deletions);

  // Finding divisors of monomials
  int find_first(const int *exp, mon_term *&result) const;
  void find_all_divisors(const int *exp, array<mon_term *> &result) const;


//////////////////////////////////////////////
//  Input, output, infrastructure ////////////
//////////////////////////////////////////////
public:
  const Ring * Ring_of() const { return R; }

  void text_out(buffer &o) const;
  void bin_out(buffer &o) const;

  int                 length_of()           const { return count; }
  TermIdeal *         cast_to_TermIdeal()         { return this; }
  const TermIdeal *   cast_to_TermIdeal()   const { return this; }

  class_identifier class_id() const { return CLASS_TermIdeal; }
  type_identifier  type_id () const { return TY_TERMIDEAL; }
  const char * type_name   () const { return "TermIdeal"; }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#endif


