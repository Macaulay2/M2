// Copyright 1996 Michael E. Stillman.

#ifndef _gb_comp_hh_
#define _gb_comp_hh_

#include "hash.hpp"
#include "intarray.hpp"

class binomialGB_comp;
class sagbi_comp;
class Matrix;
class RingElement;
class PolynomialRing;

extern "C" char system_interrupted;
extern int gbTrace;

// The printlevel flags
const int PRINT_SPAIR_TRACKING=1024;

// The various kinds of GB computations
const int COMP_NGB = 10;
const int COMP_GB = 1;
const int COMP_GBINHOM = 2;
const int COMP_HERMITE = 3;
const int COMP_GAUSS = 4;
const int COMP_BINOMIAL_GB = 5;
const int COMP_SAGBI = 6;

// Possible strategy flags
const int USE_HILB = 1;
const int USE_GEOBUCKET = 8;
const int USE_SORT = 16;

// These are the possible states of a GB computation
const int GB_COMP_NEWDEGREE        = 1; 
const int GB_COMP_NEED_RESIZE      = 2;
const int GB_COMP_S_PAIRS          = 3;
const int GB_COMP_GENS             = 4;
const int GB_COMP_AUTO_REDUCE      = 5;
const int GB_COMP_NEWPAIRS         = 6;
const int GB_COMP_DONE             = 7;

// The following are the return values from s_pair_step,
const int SPAIR_DONE   = 0;
const int SPAIR_GB     = 1;
const int SPAIR_SYZ    = 2;
const int SPAIR_ZERO   = 3;
const int SPAIR_MINGEN = 4;
const int SPAIR_GEN    = 5;
const int SPAIR_PAIR   = 6;
const int SPAIR_RING   = 7;
const int SPAIR_REMOVED = 8;
const int SPAIR_DEFERRED = 9;

//--- To be removed soon -----------------------------------
// These are the possible states of a GB computation
const int GB_COMP_RESIZE_MONOMIALS = 10;
const int GB_COMP_SORTPAIRS        = 11;

// The following are the three levels of a Groebner basis.
const int SYZ_GB      = 0;
const int SYZ_CHANGE  = 2;  // MES these need to be changed!!
const int SYZ_KER     = 1;

// The following are the return values from s_pair_step,
// and gen_step.
const int SPAIR_NOT_MINIMAL = 8;
const int SPAIR_BASE   = 9;

// These are the types of ending conditions
const int STOP_DONE     = 1;
const int STOP_DEGREE   = 2;
const int STOP_NEW_ELEM = 3;
const int STOP_CODIM    = 4;
const int STOP_MIN_GENS = 5;
//--- above to be removed soon -----------------------------

struct StopConditions
{
  bool always_stop;
  bool stop_after_degree;
  M2_arrayint degree_limit; // Stop after completing this 'slanted' degree
  unsigned int basis_element_limit; // Number of gb elements
  unsigned int syzygy_limit;
  unsigned int pair_limit;
  bool use_codim_limit;
  unsigned int codim_limit;
  unsigned int subring_limit;
  M2_bool just_min_gens;
  M2_arrayint length_limit; // ignored for GB computations
};

class Computation : public mutable_object
// This is the base type for all Groebner basis and syzygy computations
{
protected:
  StopConditions _Stop;
  Computation();
  virtual ~Computation();
public:
  static ComputationOrNull *choose_gb(const Matrix *m,
				      M2_bool collect_syz,
				      int n_rows_to_keep,
				      M2_arrayint gb_degrees,
				      M2_bool use_max_degree,
				      int max_degree,
				      int algorithm,
				      int strategy);

  static ComputationOrNull *choose_res(const Matrix *m,
				  M2_bool resolve_cokernel,
				  int max_level,
				  M2_bool use_max_slanted_degree,
				  int max_slanted_degree,
				  int algorithm,
				  int strategy
				  );

  static ComputationOrNull * force(const Matrix *m,
				   const Matrix *gb,
				   const Matrix *change);

  ComputationOrNull *
  set_stop_conditions(M2_bool always_stop,
		      M2_bool stop_after_degree,
		      M2_arrayint degree_limit,
		      int basis_element_limit,
		      int syzygy_limit,
		      int pair_limit,
		      int codim_limit,
		      int subring_limit,
		      M2_bool just_min_gens,
		      M2_arrayint length_limit);

  virtual const PolynomialRing *get_ring() = 0;

  virtual ComputationOrNull *set_hilbert_function(const RingElement *h) = 0;

  virtual void compute() = 0;

  virtual const MatrixOrNull *get_matrix(int level, M2_bool minimize) = 0;

  virtual int kind() = 0;

  virtual int status(int * complete_up_through_this_degree,
		     int * complete_up_through_this_level) = 0;
  /* -1: error condition, and the error message is set.
     0: not made, and in fact it won't ever be done...
     1: not started,
     2: started, 
     3: stopped because of a stopping condition
     4: finished the computation completely
  */

  virtual int status_level(int level, 
			   M2_bool minimize,
			   int * complete_up_through_this_degree) = 0;
  /* Same return values */

  virtual const MatrixOrNull *get_change(int level) = 0;

  virtual const MatrixOrNull *get_leadterms(int nparts, int level) = 0;
  
  virtual const FreeModuleOrNull *get_free(int level, M2_bool minimal) = 0; 

  virtual const MatrixOrNull *matrix_remainder(int level,
					       const Matrix *m) = 0;

  virtual void matrix_lift(int level,
			   const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient
			   ) = 0;

  virtual int contains(int level,
		       const Matrix *m) = 0;

  virtual const M2_arrayint betti(int type) = 0;
  /* 0: minimal betti numbers,
     1:
     2:
     3:
  */
  
  virtual void text_out(buffer &o) = 0;
  /* This displays statistical information, and depends on the
     gbTrace value */
};

class GBComputation : public Computation
{
protected:
  // Values maintained to determine status of computation
  int _state;
  int _this_degree;

  // Values maintained to check stop conditions  
  int _n_gb;
  int _n_syz;
  int _n_pairs_computed;
  int _n_gens_left;
  int _n_subring;

protected:
  int computation_is_complete() const;

public:
};


class gb_comp : public mutable_object
{
protected:
  int _kind;  // GB_comp:1, GBinhom_comp:2, EGB1:13
  StopConditions Stop;
public:
  gb_comp(int k) : _kind(k) {}
  virtual ~gb_comp() {}

  int kind() const { return _kind; }

  static gb_comp *make(const Matrix *m, bool dosyz, int nsyz, int strategy);
  static gb_comp *make(const Matrix *m, bool dosyz, int nsyz, const RingElement *hf, int strategy);
  static gb_comp *force(const Matrix *gens, 
			const Matrix *gb, 
			const Matrix *change, 
			const Matrix *syz);

  void set_stop_conditions(StopConditions &S) { Stop = S; }

  virtual int calc(const int *deg, const intarray &stop) = 0;
  
  virtual void stats() const = 0;
  virtual Matrix *min_gens_matrix() = 0;
  virtual Matrix *gb_matrix() = 0;
  virtual Matrix *syz_matrix() = 0;
  virtual Matrix *change_matrix() = 0;
  virtual Matrix *initial_matrix(int n=-1) = 0;

  virtual Matrix *reduce(const Matrix *m, Matrix * &result_lift) = 0;

  virtual int contains(const Matrix *m) = 0;
  virtual bool is_equal(const gb_comp *q) = 0;

  // These can be overridden by the specific computation
  void text_out(buffer &o) const { o << "GB computation"; }

  virtual binomialGB_comp * cast_to_binomialGB_comp() { return 0; }
  virtual const binomialGB_comp * cast_to_binomialGB_comp() const { return 0; }
  virtual sagbi_comp * cast_to_sagbi_comp() { return 0; }
  virtual const sagbi_comp * cast_to_sagbi_comp() const { return 0; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
