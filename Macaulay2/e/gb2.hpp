// Copyright 1997  Michael E. Stillman
#ifndef _gb2_hh_
#define _gb2_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"
#include "hilb.hpp"
#include "spair.hpp"

#define STATE_DONE 0
#define STATE_NEW_DEGREE 1
#define STATE_HILB 2
#define STATE_GB 3
#define STATE_GENS 4

////////////////////////////
// Computation node types //
////////////////////////////

class gb_node
{
public:
  virtual ~gb_node() {}
  virtual void set_output(gb_node *p) = 0;

  // The following two routines return one of 
  // COMP_DONE, COMP_DONE_*, COMP_COMPUTING, COMP_INTERRUPTED

  virtual int calc_gb(int degree, const intarray &stop) = 0;
  virtual int calc_gens(int degree, const intarray &stop) = 0;

  virtual bool is_done() = 0;	// Returns true if computation is completely done,
				// if no other generators are received.

  virtual bool receive_generator(gbvector * f, int n, const ring_elem denom) = 0; 
  // returns true if f is minimal.  'denom' is only used in the 'origsyz>0'
  // case.

  // Reduction of a vector f (in correct free module), and its rep 'fsyz'.
  virtual void reduce(gbvector * &f, gbvector * &fsyz) = 0;

  // Hilbert functions of these nodes...
  // Both of these return 0 if computed, non-zero if interrupted.
  virtual int hilbertNumerator(RingElement *&result) = 0;
  virtual int hilbertNumeratorCoefficient(int deg, int &result) = 0;

  virtual int n_gb_elems() const = 0;
  virtual const FreeModule *output_free_module() = 0;
  virtual Matrix *min_gens_matrix() = 0;
  virtual Matrix *get_matrix() = 0;
  virtual Matrix *initial_matrix(int n) = 0;
  virtual Matrix *gb_matrix() = 0;
  virtual Matrix *change_matrix() = 0;
  virtual void stats() const = 0;
};

////////////////////////////
// Specific node types /////
////////////////////////////

class gb_emitter : public gb_node
{
  GBRing *GR;
  const Matrix *gens;
  gb_node *g;
  int this_degree;
  int n_left;
  int n_i;
  int n_in_degree;
  int n_gens;
  int *these;

  void flush();
  int start_degree(int deg);
public:
  gb_emitter(const Matrix *m);
  ~gb_emitter();
  virtual void set_output(gb_node *gg) { g = gg; }

  virtual int calc_gb(int degree, const intarray &stop);
  virtual int calc_gens(int degree, const intarray &stop);

  virtual bool is_done();

  virtual bool receive_generator(gbvector *, int, const ring_elem) { return false; }

  // Reduction of a vector f (in correct free module), and its rep 'fsyz'.
  virtual void reduce(gbvector * &, gbvector * &) {}

  virtual int hilbertNumerator(RingElement *&) { return 1; }
  virtual int hilbertNumeratorCoefficient(int, int &) { return 1; }

  virtual int n_gb_elems() const { return 0; }
  virtual const FreeModule *output_free_module() { return gens->rows(); }
  virtual Matrix *min_gens_matrix() { return new Matrix(gens->rows()); }
  virtual Matrix *get_matrix() { return (Matrix *)gens; }
  virtual Matrix *initial_matrix(int) { return new Matrix(gens->rows()); }
  virtual Matrix *gb_matrix() { return new Matrix(gens->rows()); }
  virtual Matrix *change_matrix() { return new Matrix(gens->rows()); }
  virtual void stats() const;
};

class gb2_comp : public gb_node
{
private:
  // Ring information
  GBRing *GR;
  const PolynomialRing *R;
  const Monoid *M;
  const Ring *K;

  FreeModule *F;
  FreeModule *Fsyz;	// This is a Schreyer module
  const SchreyerOrder *S; // If non-NULL, the Schreyer order for F.

  int level;			// what level is this?
  int state;			// STATE_NEW_DEGREE, STATE_GB, STATE_GENS, STATE_HILB
  int this_degree;
  int n_gb_first;		// First GB element in the current degree.
				// (or previous degree, if state = STATE_NEW_DEGREE)

  s_pair_heap *spairs;
  s_pair *these_pairs;
  intarray total_pairs;

  array<gb_elem *> gb;
  Matrix *gbmatrix;
  array<monideal_pair *> monideals; // baggage for each is 'gb_elem *'

  // Syzygies collected
  gb_node *syz;
  gb_node *gens;

  // statistics information, much is kept with the s_set
  int n_gb;
  int n_mingens;
  int n_subring;
  int n_syz;

  int n_pairs;			// Total number of pairs
  int n_pairs_computed;		// Number of pairs total computed (sum of next 6 integers)
  int n_pairs_syz;		// #pairs which produced non-zero syzygies
  int n_pairs_usyz;		// #pairs which produced zero syzygies, after reduction
  int n_pairs_gb;		// #pairs which produced gb elements
  int n_pairs_zero;		// #pairs which reduced to 0 (syz reduced to 0 too)
  int n_pairs_hilb;		// #pairs which were not done, due to HF info
  int n_pairs_gcd;		// #pairs which were not done, due to gcd=1 pairs

  // Syzygy type
  int orig_syz;			// >=0 means how many components to keep.
				// < 0 means compute syz's on minimal gens.

  char is_ideal;
  int strategy_flags;		// USE_GEOBUCKET, USE_SORT are the current flags used

  // Hilbert function information
  char use_hilb;
  hilb_comp *hf_comp;		// The HF computation, used to compute 'hf'.
				// In case of interrupts, we require that this
				// computation be restarted (reasoning: it is usually not
				// so bad, and it complicates the logic quite a bit).
  RingElement *hf;		// The Hilbert function, as so far computed
  int n_hf;			// The HF has been computed for this many GB elements.
				// (Used to determine whether to recompute HF).
  const RingElement *hf_orig;
  int n_gb_syz;
  int n_in_degree;		// The number of new elements that we expect to find
				// in this degree. <0 means we don't know how many.
private:
  void setup(FreeModule *Fsyz,
	     gb_node *gens,
	     int lodegree,
	     int origsyz, 
	     int level,
	     int strategy);

  // S-pair control
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  void remove_pair(s_pair *& p);

  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  void gb_reduce(gbvector * &f, gbvector * &fsyz);
  void gb_geo_reduce(gbvector * &f, gbvector * &fsyz);
  void gb_insert(gbvector * f, gbvector * fsyz, int ismin);

  int gb_sort_partition(int lo, int hi);
  void gb_sort(int lo, int hi);

  void flush_pairs();
  
  int computation_complete(int stop_gb, 
			   int stop_syz, 
			   int stop_codim,
			   int stop_pairs, 
			   int stop_min_gens,
			   int subring);

  void schreyer_append(gbvector *f);
  bool s_pair_step();
  int get_pairs();

public:
  gb2_comp(FreeModule *Fsyz,
	   gb_node *gens,
	   int lodegree,
	   int orig_syz,
	   int level,
	   int strategy);

  ~gb2_comp();
  virtual void set_output(gb_node *p);

  // Performing the computation
  int start_degree(int degree, int expected=-1);// Returns the number of pairs in this degree.
  int calc(const intarray &stop_conditions);
  // calc returns COMP_DONE_*, or COMP_DONE, or COMP_INTERRUPTED.
  bool receive_generator(gbvector * f, int n, const ring_elem denom);
  void end_degree();
  bool is_done();

  int calc_gb(int deg, const intarray &stop);
  int calc_gens(int deg, const intarray &stop);

  virtual void reduce(gbvector * &f, gbvector * &fsyz);

  virtual int hilbertNumerator(RingElement *&result);
  virtual int hilbertNumeratorCoefficient(int deg, int &result);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  int n_gb_elems() const { return n_gb; }
  const FreeModule *output_free_module() { return Fsyz; }
  Matrix *min_gens_matrix();
  Matrix *get_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();

  
  void debug_out(s_pair *q) const;
  void debug_out(buffer &o, s_pair *q) const;
  void stats() const;
};  

class gbres_comp : public mutable_object
{
private:
  GBRing *GR;
  int n_nodes;
  gb_node **nodes;

  int lo_degree;
  int strategy_flags;
private:
  void setup(const Matrix *m, int length, int origsyz, int strategy);
  
public:
  gbres_comp(const Matrix *m, int length, int orig_syz, int strategy);
  gbres_comp(const Matrix *m, int length, int orig_syz,
	  const RingElement *hf, int strategy);

  ~gbres_comp();
  // Performing the computation
  int calc(const int *deg, const intarray &stop_conditions);
  bool is_done();

  // reduction
  Matrix *reduce(const Matrix *m, Matrix *&lift);
  Vector *reduce(const Vector *v, Vector *&lift);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  void betti_minimal(intarray &result);

  FreeModule *free_module(int level);
  Matrix *min_gens_matrix(int level);
  Matrix *get_matrix(int level);
  Matrix *initial_matrix(int n, int level);
  Matrix *gb_matrix(int level);
  Matrix *change_matrix(int level);
  void stats() const;

  gbres_comp *cast_to_gbres_comp() { return this; }
};  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
