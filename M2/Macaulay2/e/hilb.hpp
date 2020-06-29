// Copyright 1996 Michael E. Stillman

#ifndef _hilb_hh_
#define _hilb_hh_

// Computation of Hilbert functions via Bigatti's (et al) algorithm.

#include "monideal.hpp"
#include "matrix.hpp"
#include "polyring.hpp"

class RingElement;
class Matrix;
class MonomialIdeal;

class partition_table
// Partition a monomial ideal into several such that
// the graph of variables occuring in each is connected.
// Implemented using a union-find algorithm.
{
  int n_vars;
  int n_sets;
  intarray adad;
  intarray aoccurs;
  int *dad;
  int *occurs;

  int merge_in(int x, int y);
  void merge_in(const int *m);
  int representative(int x);

  stash *mi_stash;  // for all of the nodes in all of the monomial ideals
 public:
  partition_table(int nvars, stash *mi_stash0);
  ~partition_table() {}
  void reset(int nvars);
  void partition(MonomialIdeal *&I,
                 VECTOR(MonomialIdeal *)& result);  // Consumes I.
};

struct hilb_step : public our_new_delete
{
  hilb_step *up;
  hilb_step *down;
  int i;         // Which monomial ideal is next
  ring_elem h0;  // Hilbert function so far computed 'to the left'
  ring_elem h1;
  int first_sum;  // First monomial ideal which corresponds to the 'sum' part
  VECTOR(MonomialIdeal *) monids;  // The (partitoned) array of monomial ideals
};

/**
    @ingroup hilb

    @brief Computation of Hilbert functions
*/
class hilb_comp : public MutableEngineObject
{
  const PolynomialRing *S;  // This is the base ring of the monomial ideal
  const PolynomialRing *R;  // This is the output degree ring.
  const Monoid *M;          // S->getMonoid()
  const Monoid *D;          // R->getMonoid() == S->degree_monoid()

  stash *mi_stash;  // for all of the nodes in all of the monomial ideals

  // Collected values from the matrix
  const Matrix *input_mat;    // The input matrix
  ring_elem result_poincare;  // The result poincare polynomial from components
                              // 0..this_comp-1
  int this_comp;
  int n_components;  // If input_mat is not NULL,
                     // this is the number of rows

  // The recursion stack is the following:
  hilb_step *current;

  // Statistics
  int nsteps;
  int depth;
  int maxdepth;
  int nideal;
  int nrecurse;

  // Some useful precomputed values
  ring_elem one;
  ring_elem minus_one;

  // Local variables that are allocated once and for all
  // extreme care is needed for their use!
  int *LOCAL_deg1;  // An element of the degree monoid
  intarray LOCAL_vp;
  partition_table part_table;

  int step();  // Returns 0 when done
  void recurse(MonomialIdeal *&I, const int *pivot_vp);
  void do_ideal(MonomialIdeal *I);

 public:
  hilb_comp(const PolynomialRing *R, const Matrix *M);
  hilb_comp(const PolynomialRing *R, const MonomialIdeal *I);
  ~hilb_comp();

  void reset();
  void next_monideal();
  int calc(int nsteps);
  int is_done() const;
  RingElement *value();
  void stats() const;

  // static routines

  // If the coefficient in degree 'deg' is < 0, then
  // set an error, and return 0.  The caller MUST check this.
  static int coeff_of(const RingElement *h, int deg);

#if 0
//   static int hilbertSeries(const Matrix *M, RingElement * &result);
//   // A return of 0 means that the result can be used.  A non-zero return
//   // value means that the computation was interrupted, and so control should
//   // return to the user.
#endif

  static RingElement /* or null */ *hilbertNumerator(const Matrix *M);
  /* This routine computes the numerator of the Hilbert series
     for coker leadterms(M), using the degrees of the rows of M.
     NULL is returned if the ring is not appropriate for
     computing Hilbert series, or the computation was interrupted. */

  static RingElement *hilbertNumerator(const FreeModule *F);

  static RingElement /* or null */ *hilbertNumerator(const MonomialIdeal *I);
  /* This routine computes the numerator of the Hilbert series
     for coker I.   NULL is returned if the ring is not appropriate for
     computing Hilbert series, or the computation was interrupted. */
};

#if 0
// class HilbertComputation
// {
//   ring_elem hilbert(const MonomialIdeal *M);
//
//   ring_elem hilbert(const MonomialTable *M);
//
//   RingElement /* or null */ *hilbert(const Matrix *M);
//   // This one is pretty easy: loop through each component,
//   // make a monomial ideal, and compute its hilbert function,
//   // then multiply it by the degree of that row component.
//
//
// };
//
// class hilb_comp : public mutable_object
// {
//   const PolynomialRing *S;           // This is the base ring of the monomial ideal
//   const PolynomialRing *R;           // This is the output degree ring.
//   const Monoid *M;           // S->getMonoid()
//   const Monoid *D;           // R->getMonoid() == S->degree_monoid()
//
//   // Collected values from the matrix
//   const Matrix *input_mat;   // The input matrix
//   ring_elem result_poincare; // The result poincare polynomial from components
//                              // 0..this_comp-1
//   int this_comp;
//   int n_components;          // If input_mat is not NULL,
//                                 // this is the number of rows
//
//   // The recursion stack is the following:
//   hilb_step *current;
//
//   // Statistics
//   int nsteps;
//   int depth;
//   int maxdepth;
//   int nideal;
//   int nrecurse;
//
//   // Some useful precomputed values
//   ring_elem one;
//   ring_elem minus_one;
//
//   // Local variables that are allocated once and for all
//   // extreme care is needed for their use!
//   int *LOCAL_deg1; // An element of the degree monoid
//   intarray LOCAL_vp;
//   partition_table part_table;
//
//   int step();                        // Returns 0 when done
//   void recurse(MonomialIdeal * &I, const int *pivot_vp);
//   void do_ideal(MonomialIdeal * I);
//
// public:
//   hilb_comp(const PolynomialRing *R, const Matrix * M);
//   ~hilb_comp();
//
//   void reset();
//   void next_monideal();
//   int calc(int nsteps);
//   int is_done() const;
//   RingElement *value();
//   void stats() const;
//
//   // static routines
//   static int coeff_of(const RingElement *h, int deg);
//
// #if 0
// //   static int hilbertSeries(const Matrix *M, RingElement * &result);
// //   // A return of 0 means that the result can be used.  A non-zero return
// //   // value means that the computation was interrupted, and so control should
// //   // return to the user.
// #endif
//
//   static RingElement /* or null */ *hilbertNumerator(const Matrix *M);
//   /* This routine computes the numerator of the Hilbert series
//      for coker leadterms(M), using the degrees of the rows of M.
//      NULL is returned if the ring is not appropriate for
//      computing Hilbert series, or the computation was interrupted. */
//
//
// };
//
//
//
//
// class HilbertComputation
// {
// public:
//   HilbertComputation(const Ring *R,
//                   VECTOR(int) &comp,
//                   M2_arrayint wts);
//
//   insert_generator(const int *m, int comp);
//
//   RingElement /* or null */ * multDegreeHilbert(const FreeModule *F);
//   RingElement /* or null */ * hilbert(VECTOR(int) &comp);
//
//   // The following routines all use the singly graded degree ring.
//   static RingElement /* or null */ * hilbert(const Ring *R,
//                                   VECTOR(exponents) &exps,
//                                   VECTOR(int) &comps,
//                                   VECTOR(int) &comp_degs,
//                                   M2_arrayint wts);
//
//   static int codimension(const RingElement *hf);
//   static int coefficient(const RingElement *hf, int deg);
//   // Should this return (as an argument) an mpz_ptr instead?
//   static int degree(const RingElement *hf);
//   static int reduce_hilb_fcn(const RingElement *hf, RingElement *&result_hf);
//   // returns the codimension, and places hf/(1-t)^codim into result_hf.
//
//   static RingElement /* or null */ * multDegreeHilbert(const Ring *R,
//                                             VECTOR(exponents) &exps,
//                                             VECTOR(int) &comps,
//                                             const FreeModule *F);
//
//   // The following routines all use the singly graded degree ring.
//   static RingElement /* or null */ * hilbert(const Ring *R,
//                                   VECTOR(exponents) &exps,
//                                   VECTOR(int) &comps,
//                                   VECTOR(int) &comp_degs,
//                                   M2_arrayint wts);
//
//   static int codimension(const RingElement *hf);
//   static int coefficient(const RingElement *hf, int deg);
//   // Should this return (as an argument) an mpz_ptr instead?
//   static int degree(const RingElement *hf);
//   static int reduce_hilb_fcn(const RingElement *hf, RingElement *&result_hf);
//   // returns the codimension, and places hf/(1-t)^codim into result_hf.
// };
#endif

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
