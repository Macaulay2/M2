#if 0
// // Copyright 1996  Michael E. Stillman
// #ifndef _termideal_hpp_
// #define _termideal_hpp_
// 
// #include "queue.hpp"
// #include "polyring.hpp"
// #include "newspair.hpp"
// #include "newdelete.hpp"
// 
// class cursor_TermIdeal;
// 
// const int TI_TERM = 1;
// const int TI_MONOMIAL = 2;
// const int TI_NONE = 3;
// 
// //////////////////////////////////
// // tagged_term ///////////////////
// //////////////////////////////////
// // Used in finding minimal 
// // generators of term ideals
// //////////////////////////////////
// 
// struct tagged_term
// {
//   ring_elem _coeff;
//   int *_monom;
//   gbvector *_gsyz;
//   int _homog_degree;  //(homo var)^(_homog_degree) * monom has degree
// 				//  equal to the sugar degree of this element.
// 
//   tagged_term(ring_elem c, int *m, gbvector *g)
//     : _coeff(c), _monom(m), _gsyz(g),
//     _homog_degree(0) {}
//   ~tagged_term() {}
// 
//   ring_elem coeff() const { return _coeff; }
//   const int *monom() const { return _monom; }
//   gbvector *gsyz() const { return _gsyz; }
// };
// 
// // mon_term
// // 
// struct mon_term : public our_new_delete
// {
//   mon_term * next;
//   mon_term * prev;
// 
//   tagged_term *t;
// 
//   bool      coeff_is_one;
//   int       expmask;
//   int *     _lead_exp;
// 
//   ring_elem coeff() const { return t->_coeff; }
//   const int *monom() const { return t->_monom; }
//   const int *lead_exp() const { return _lead_exp; }
// };
// 
// 
// class TermIdeal : public mutable_object
// {
//   friend class cursor_TermIdeal;
// private:
//   const Ring *K;		// A p.i.d or a field?
//   const Monoid *M;
//   const PolynomialRing *R;
//   const PolynomialRing *A;	// A = R/I
// 
//   GBRing *GR;
// 
//   const FreeModule *Gsyz;	// Free module over R.
// 
//   int nvars;
//   ring_elem one;
// 
//   mon_term *ring_terms;		// Taken from the ring.  Do not free these directly!
//   mon_term *terms;
//   int count;
// 
// private:
//   int exp_divides(const int *e, const int *f) const;
//   int monomial_mask(const int *exp) const;
// 
//   void link(mon_term *s, mon_term *t);
//   void unlink(mon_term *s);
// 
//   mon_term *new_mon_term_head() const;
// 
//   // Minimalize and insert
//   void from_list(queue<tagged_term *> &elems);
//   tagged_term *insert_minimal(tagged_term *t, mon_term *&new_t);
// 
//   // Sorting an array of mon_terms
//   int sort_compare(const tagged_term *p, const tagged_term *q) const;
//   int sort_partition(tagged_term *a[], int lo, int hi);
//   void sort(tagged_term *a[], int lo, int hi);
// 
//   tagged_term *gcd(array<tagged_term *> &elems, const int *m) const;
//   void select_non_divisors(tagged_term **a, int nelems, tagged_term *g,
// 				    array<tagged_term *> &result_divs) const;
// 
//   // Comparison of terms
//   int compare(mon_term *s, mon_term *t) const;
// 
//   // Finding divisors
//   bool find_all_divisors(const int *exp, array<tagged_term *> &result) const;
// public:
//   TermIdeal(GBRing *GR, const FreeModule *Gsyz);
// 				// It is valid for these to be NULL, if
// 				// there is no baggage...
//   ~TermIdeal();
// 
//   // Creation
//   static TermIdeal *make_termideal(const Matrix *m, int n);
//   static TermIdeal *make_termideal(GBRing *GR,
// 				   const FreeModule *Gsyz, 
//   				   queue<tagged_term *> &elems);
// 
//   static TermIdeal *make_ring_termideal(const PolynomialRing *R, 
// 					const array<ring_elem> &elems1,					
// 					const array<ring_elem> &elems2,
// 					array<ring_elem> &result);
//   // This routine takes a ring R, which should be a polynomial ring with ZZ as coefficients,
//   // NOT a quotient ring; and two sets of ring elements that together should form a GB.
//   // Returns a term ideal of all of these, which refers to the newly created 'result'.
// 
//   void append_to_matrix(Matrix *m, int i) const;
//   Matrix *change_matrix() const;
//   Matrix *ring_change_matrix() const;
// 
//   // Insertion of new monomials.  It is up to the callee to delete the returned value...
//   tagged_term *insert_minimal(tagged_term *t);
// 
//   void insert_w_deletions(tagged_term *t, queue<tagged_term *> &deletions);
// 
//   // Finding divisors of monomials
//   int replace_minimal(const ring_elem new_coeff, const int *mon);
// 
//   int search(const int *m, ring_elem &result_gcd, gbvector *&result_gsyz) const;
//     // Returns the number of divisors found.
// 
//   int search(const ring_elem c, const int *m, gbvector *&result_gsyz) const;
//     // return value is one of TI_TERM, TI_MONOMIAL, TI_NONE.
// 
//   int search(const ring_elem c, const int *m, 
// 	     ring_elem &termgcd, 
// 	     gbvector *&result_gsyz) const;
//     // return value is one of TI_TERM, TI_MONOMIAL, TI_NONE.
//     // termgcd is ONLY set if the return value is not TI_NONE.
// 
//   Matrix *search(const Matrix *m) const;
// 
//   // Creating and deleting "mon_terms"s
//   mon_term *new_mon_term(tagged_term *t) const;
//   void delete_mon_term(mon_term *&t) const;
//   void delete_tagged_term(tagged_term *&t) const;
//   
// //////////////////////////////////////////////
// //  Input, output, infrastructure ////////////
// //////////////////////////////////////////////
// public:
//   const Ring * get_ring() const { return R; }
// 
//   void text_out(buffer &o) const;
// };
// 
// class cursor_TermIdeal
// {
//   mon_term *head;
//   mon_term *t;
// public:
//   cursor_TermIdeal(TermIdeal *ti) : head(ti->terms), t(ti->terms->next) {}
//   bool valid() { return t != head; }
//   void operator++() { t = t->next; }
//   tagged_term *operator*() { return t->t; }
// };
// #endif
// 
// 
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
