// Copyright 2000  Michael E. Stillman
#ifndef _EEGB1_hh_
#define _EEGB1_hh_

#include "gb_comp.hpp"
#include "EEdefs.hpp"
#include "EEbasics.hpp"
#include "EEspairs.hpp"
#include "EEhilbtrack.hpp"
#include "EEGB.hpp"

// Notes:
// TODO:
// Life becomes much simpler if quotient ring elements are also vectors...
//    However, some new multiply routines might be needed in freemod.hpp...
// at the conclusion of the EGB1, it removes it, and save the GB?
// saving the minimal GB?
// forceGB: could land up here?
// what about where syzygies go?

class SyzygyCollector
{
  Matrix syz;
public:
  SyzygyCollector(FreeModule *F) : syz(Matrix(F)) {}
  ~SyzygyCollector() {}

  void eat(EVector v) { syz.schreyer_append(v); }
  Matrix value() { return syz; }
  int length() const { return syz.n_cols(); }
};


class EGB1 : public gb_comp
{
  const EInterface &I;
  int nvars;

  const PolynomialRing *R;  // k[x], or ZZ[x]
  const PolynomialRing *A;  // R/I
  const PolynomialRing *B;  // R, or QQ[x]/I  ?? Is this needed?

  const FreeModule *F;		// Over what ring?  Probably R.
  const FreeModule *Fsyz;	// Over A.

  EGBMemory *MEM;		// Handles memory for egb_elem,ering_elem,es_pair,
				// and exponent vectors.

  //// Main data structures for computing the GB and syzygies ////

  EGB *GB;			// Contains divisors and also keeps track of minimal GB
				// elements.

  SPairSet *SPAIRS;		// Maintains the elements of the current degree.
				// Handles sorting operations, and other spair list handling.

  SyzygyCollector *syz;

  //// Syzygy type ////
  bool collect_syz;
  int n_comps_per_syz;

  //// Hilbert function information ////
  bool update_hilb;
  HilbertTracker *hilb_tracker;	// null means not being used

  //// Auto-reduction ////
  array< egb_elem * > auto_loc;// Used only for auto-reduction.
				// these egb_elem *'s are not owned here: instead they are
				// in GB.
  //// Flags ////
  bool is_ideal;
  bool find_trimmed;
  bool find_subring_trimmed;

  //// Options which are useful to have handy ////
  bool ring_is_skew;
  bool ring_is_quotient;
  bool coeffs_QQ;
  bool is_homogeneous;
  bool is_well_ordering;

  // statistics information
  int n_subring;		// The number of elements in the subring found so far.

public:
  //TODO: denoms, hilbfcns, local
  // Creation of a GB computation object
  bool set_up(const Matrix &m, int csyz, int nsyz, int strategy);  //OK
  EGB1(const Matrix &m, int collect_syz, int n_syz, int strategy); // not quite OK: have 'make' routine...
  ~EGB1(); //TODO

  void use_Hilbert_function(const RingElement &hf); //OK

  /////////////////////
  // top level logic //
  /////////////////////

  int new_calc(const EStopConditions &stop); //TODO: alot done

  int is_computation_complete(const EStopConditions &stop) const; //OK, codim?

  void s_pair_step(es_pair *p); //not OK...

  void update_pairs(egb_elem *p); //OK, sort of.
    // Update the set of s-pairs

  ////////////////////////////
  // Support level routines //
  ////////////////////////////
  void insert_generator(int i, EVector f); //OK

  void collect_syzygy(EVector z); //OK

  void gb_insert(es_pair *&p); //OK

  void compute_s_pair(es_pair *p); //?? 2 versions...

  int gb_reduce(es_pair *p); //TODO

  int gb_reduce_tail(egb_elem *g); //TODO

  void auto_reduce_by(egb_elem *new_elem); //OK, TODO:denoms

  virtual int calc(const int *deg, const intarray &stop_conditions);

  void gb_reduce(EVector &f, EVector &fsyz) const; //TODO
  virtual void stats() const; //TODO
  virtual Matrix min_gens_matrix(); //TODO: iterator
  virtual Matrix gb_matrix();
  virtual Matrix syz_matrix();
  virtual Matrix change_matrix();
  virtual Matrix initial_matrix(int n=-1);

  virtual Matrix reduce(const Matrix &m, Matrix &result_lift);
  virtual Vector reduce(const Vector &v, Vector &result_lift);

  virtual int contains(const Matrix &m);
  virtual bool is_equal(const gb_comp *q);  //TODO

  virtual void moreGenerators(int lo, int hi, const Matrix &m); //OK
};


#endif
