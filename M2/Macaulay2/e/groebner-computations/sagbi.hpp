// Copyright 1997  Michael E. Stillman

#ifndef M2_GROEBNER_COMPUTATIONS_SAGBI_HH_
#define M2_GROEBNER_COMPUTATIONS_SAGBI_HH_

#include "matrices/matrix.hpp"
#include "groebner-computations/comp-gb.hpp"

/**
* A basic class that implements SAGBI bases (aka canonical subalgebra bases, or Khovanskii bases.)
*/
class sagbi
{
 public:

  /**
   * A subduction routine for a single ring element: unclear if/how this is ever used.
   */
  static ring_elem subduct(int numslots, /**< the number of blocks in a monomial order */
                           const PolyRing *R, /**< a polynomial ring */
                           ring_elem f, /**< the ring element to be subducted */
                           const RingMap *phi, /**< a ring map presenting an algebra */
                           GBComputation *J /**< a Groebner basis computation object */
                           ); 

  /**
   * A subduction routine for multiple ring elements in a matrix: unclear if/how this is ever used.
   */  
  static Matrix *subduct(int numparts, /**< the number of blocks in a monomial order */
                         const Matrix *m, /**< a matrix of ring elements to be subducted */
                         const RingMap *phi,/**< a ring map presenting an algebra */
                         GBComputation *J /**< a Groebner basis computation object */
                         );
    
  static ring_elem subduct1(int numslots,
                            const PolyRing *T, /**< this is the tensor ring (containing original variables for subalgebra and extra variables tagging algebra generators) */
                            const PolyRing *S, /**< polynomial ring containing original variables*/
                            ring_elem a, /**< this is the ring element to be subducteda*/
                            const RingMap *inclusionAmbient, /**< the inclusion map reprensenting a subalgebra */
                            const RingMap *fullSubstitution, /**< substitution map sending tag variables to generators */
                            const RingMap *substitutionInclusion, /**< combined   */
                            GBComputation *gbI, /**< Groebner basis of ideal in S encoding quotient ideal */
                            GBComputation *gbReductionIdeal /**< Groebner basis of ideal in T encoding generator reductions and lead term of I */
                            );

  static Matrix *subduct1(int numparts, 
                          const Ring *rawT, 
                          const Ring *rawS, 
                          const Matrix *m, 
                          const RingMap *inclusionAmbient, 
                          const RingMap *fullSubstitution,
                          const RingMap *substitutionInclusion, 
                          GBComputation *gbI, 
                          GBComputation *gbReductionIdeal);
};

#if 0
// #include "matrices/matrix.hpp"
// #include "computations/comp.hpp"
// #include "gb_comp.hpp"
//
//
// class pending_list
// {
//   const FreeModule *F;
//   int _n_held;
//   int _base_degree;
//   int _lo_degree;
//   array<Matrix *> pending;
// public:
//   pending_list(Matrix *m);
//   ~pending_list();
//
//   void insert(Matrix *m);  // removes m?
//   Matrix *take_lowest_matrix();
//   int lo_degree() { return _lo_degree; }
//   int n_left() { return _n_held; }
// };
//
// class sagbi_comp : public gb_comp
// {
// #if 0
// //   struct sagbi_elem {
// //     sagbi_elem *next;
// //     vec elem;
// //   };
// //
// //   int _n_iterations;
// //   int _max_degree;
// //   int _current_degree;
// //
// //   pending_list Pending;           // Over R
// //
// //   FreeModule *F;
// //   Matrix G;                       // Sagbi basis as so far computed, over R.
// //   PolynomialRing *RS;
// //   binomialGB_comp *J;
// //   RingMap *Gmap;
// //   RingMap *RtoRS;
// //   RingMap *RStoR;
// //
// //   void append_to_basis(Matrix &m);  // Adds to G, also modifies J,RS,...
// //   Matrix grab_lowest_degree();
// //   void row_reduce(Matrix &m);  // Modifies m.
// #endif
// public:
//   // creation
//   sagbi_comp(const Matrix *m);
//   ~sagbi_comp();
//
//   void enlarge(const Ring *R, int *wts);
//   void add_generators(const Matrix *m);
//   int calc(const int *deg, const intarray &stop_conditions);
//
//   Matrix *reduce(const Matrix *m, Matrix *&lift);
//
//   virtual int contains(const Matrix *m);
//   virtual bool is_equal(const gb_comp *q);
//
//   // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
//   Matrix *min_gens_matrix();
//   Matrix *initial_matrix(int n);
//   Matrix *gb_matrix();
//   Matrix *change_matrix();
//   Matrix *syz_matrix();
//   void stats() const;
//
// public:
//   sagbi_comp * cast_to_sagbi_comp() { return this; }
//   const sagbi_comp * cast_to_sagbi_comp() const { return this; }
// };
#endif
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
