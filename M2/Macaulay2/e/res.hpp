// Copyright 1996.  Michael E. Stillman

#ifndef _res_hh_
#define _res_hh_

#include "style.hpp"
#include "array.hpp"
#include "intarray.hpp"
#include "matrix.hpp"
#include "monideal.hpp"
#include "polyring.hpp"
#include "comp.hpp"

#include "respoly.hpp"

class res_pair;
class res_degree;
class res_level;
class res_comp;
class res_poly;

class res_degree
    // Collection of pairs all of the same degree
{
friend class res_comp;

  res_pair     *first;		// list of pairs in this degree, no list head

  res_pair     *next_new_pair;	// sort_pairs will set this to non-null
  res_pair     *next_pair;	// set to be first (when?)
  res_pair     *next_gen;	// this is a separate list of sorted generators

  int           is_sorted;	// If set, then the pairs have already been sorted
  int           npairs;
  int           nleft;
  int           nminimal;
public:
  res_degree()
    : first(NULL), next_new_pair(NULL), next_pair(NULL), next_gen(NULL), 
      is_sorted(0), npairs(0), nleft(0), nminimal(0)
	{}
  ~res_degree() {}
};

class res_level
    // Collection of pairs all at same syzygy level
{
friend class res_comp;

  array<res_degree *> bin;	// Bins for pairs sorted by (slanted) 
				// degree. So bin[d] refers to elements
				// of degree low_degree + level + d
  array<res_pair *> elems;

  res_pair *compare_num_list;
  int           npairs;
  int           nleft;
  int           nminimal;
public:
  res_level() : compare_num_list(NULL), npairs(0), nleft(0), nminimal(0) {}
};

class res_comp : public mutable_object
{
  // Base ring and input
  const PolynomialRing *P;
  res_poly *R;
  const Monoid *M;
  const Ring *K;
  const Matrix *generator_matrix;	// Input matrix of generators, possibly a GB, possibly not

  // The current state of the computation
  int n_level;			// Current level
  int n_degree;			// Current (slanted) degree

  array<res_level *> resn;	// The resolution itself

  // Degree and length limits, monomial size limit
  array<res_pair *> base_components;
  array<MonomialIdeal *>  search_mi;	// Used for new generators only...

  int lodegree;			// Base degree
  int hidegree;			// Highest (slanted) degree appearing
  int length_limit;		// May be downsized during the computation

  int max_degree;		// This is the largest degree than can be represented
				// as a least common multiple.  Any higher degree found
				// will cause the computation to exit with COMP_RESIZE

  // Statistics
  int next_me_number;
  int component_number;
  int nleft;
  int npairs;
  int nminimal;

  // Internal variables.  Used locally in a single routine
  int *REDUCE_exp;
  int *REDUCE_mon;
  int *PAIRS_mon;
  int *MINIMAL_mon;

  res_pair *elem(int lev, int n) const { return resn[lev]->elems[n]; }

  int find_ring_divisor(const int *exp, ring_elem &result) const;
  int find_divisor(const int *exp, res_pair *& result) const;
  res_pair *reduce(resterm *&f, resterm *&fsyz, resterm *&pivot);
  res_pair *reduce_level_one(resterm *&f, resterm *&fsyz, resterm *&pivot);
  void reduce_gen(resterm *&f) const;
  resterm *s_pair(res_pair *fsyz) const;

  int gens(int deg);
  int pairs(int level, int deg);
  int reductions(int level, int deg);

  void handle_pair(res_pair *p);
  void handle_gen(res_pair *p);
  
  void new_pairs(res_pair *p);

  int sort_value(res_pair *p, const int *sort_order) const;
  int compare_res_pairs(res_pair *f, res_pair *g) const;
  res_pair *merge_res_pairs(res_pair *f, res_pair *g) const;
  void sort_res_pairs(res_pair *& p) const;
  void sort_gens(res_degree *pairs);
  void sort_pairs(int level, int deg);

  int compare_compares(res_pair *f, res_pair *g) const;
  res_pair *merge_compares(res_pair *f, res_pair *g) const;
  void sort_compares(res_pair *& p) const;
  void set_compare_nums(int level, int deg);
  
  int degree(const res_pair *q) const;
  void multi_degree(const res_pair *q, int *result) const;

  res_degree *make_degree_set(int level, int deg);
  res_degree *get_degree_set(int level, int d) const;
  res_pair *new_res_pair(int i);
  res_pair *new_res_pair(int syztype, resterm *f);
  res_pair *new_res_pair(int syztype, res_pair *first, res_pair *second);
  void insert_res_pair(int level, res_pair *p);

//////////////////////////////////////////////
//  Initiating the computation ///////////////
//////////////////////////////////////////////
private:
  void remove_res_pair(res_pair *p);
  void remove_res_degree(res_degree *p);
  void remove_res_level(res_level *lev);

  void initialize(const Matrix *mat, 
		  int LengthLimit,
		  int strategy);

public:
  res_comp(const Matrix *m, 
	   int LengthLimit, 
	   int strategy);

  ~res_comp();

  void resize(const Ring *new_ring);

//////////////////////////////////////////////
//  Performing the calculation ///////////////
//////////////////////////////////////////////

  int calc(const int *DegreeLimit, 
	   int LengthLimit, 
	   int SyzygyLimit,
	   int PairLimit,
	   int SyzLimitValue,
	   int SyzLimitLevel,
	   int SyzLimitDegree);

  void skeleton_init(array<res_pair *> &reslevel);
  void skeleton_pairs(res_pair *&result, res_pair *p);
  int skeleton_maxdegree(const array<res_pair *> &reslevel);
  void skeleton_stats(const array<res_pair *> &reslevel);

  void skeleton(int strategy);

//////////////////////////////////////////////
//  Result matrices of the resolution ////////
//////////////////////////////////////////////
private:
  void reduce_minimal(int x, 
		      resterm *& f,
		      array<res_pair *> &elems) const;
public:

  FreeModule *free_of(int i) const;
  FreeModule *minimal_free_of(int i) const;
  Matrix *make(int i) const;
  Matrix *make_minimal(int i) const;

//////////////////////////////////////////////
//  Betti routines and numbers associated ////
//  with the resolution                   ////
//////////////////////////////////////////////
// Betti output is a flattened array of  /////
// length                                /////
// (high_degree() - low_degree() + 1)    /////
//    * (max_level() + 1)                /////
// The first row of the betti display is /////
// given first, then the second, etc     /////
//////////////////////////////////////////////

  int n_pairs(int lev, int d) const;
  int n_left(int lev, int d) const;
  int n_minimal(int lev, int d) const;
  int n_monoms(int lev, int d) const;

  int low_degree() const;
  int high_degree() const;
  int max_level() const;
  int regularity() const;

  void betti_skeleton(intarray &result) const;
  void betti_remaining(intarray &result) const;
  void betti_minimal(intarray &result) const;
  void betti_nmonoms(intarray &result) const;

//////////////////////////////////////////////
//  Debugging ////////////////////////////////
//////////////////////////////////////////////

  void text_out(const res_pair *p) const;
  void stats() const;

  void text_out(buffer &o, const res_pair *p) const;
  void stats(buffer &o) const;

//////////////////////////////////////////////
//  Infrastructure ///////////////////////////
//////////////////////////////////////////////

public:  
  const Ring   * get_ring() const { return P; }
  const Monoid  * Nmonoms() const { return M; }
  const Ring   * Ncoeffs() const { return K; }
  const Monoid  * degree_monoid() const { return P->degree_monoid(); }
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
