// Copyright 1996  Michael E. Stillman
#ifndef _gbinhom_hh_
#define _gbinhom_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"

#include "spair.hpp"

class GBinhom_comp : public gb_comp
{
private:
  // Ring information
  GBRing *GR;
  const PolynomialRing *originalR;
  const Monoid *M; // flattened monomials
  const Ring *K; // flattened coefficients

  const FreeModule *F;
  const FreeModule *Fsyz;

  s_pair_heap *spairs;

  gb_elem *gb;			// The minimal GB so far encountered
				// This is a list with a dummy head.

  gb_elem *gbLarge;

  array<MonomialIdeal *> monideals; // baggage for each is 'gb_elem *'
                              // This is the 'large' GB

  // Syzygies collected
  Matrix *syz;

  // statistics information
  int n_gb;
  int n_subring;
  int n_pairs;
  int n_computed;
  int last_gb_num;
  int n_saved_gcd;
  int n_saved_lcm;

  // Syzygy type
  int collect_syz;	// 0 or 1
  int n_comps_per_syz;
  char is_ideal;
  int strategy;

  int need_resize;
private:
  void set_up0(const Matrix *m, int csyz, int nsyz);
  void set_up(const Matrix *m, int csyz, int nsyz, int strategy);
  void force(const Matrix *m, const Matrix *gb, const Matrix *mchange, 
	  const Matrix *syz);

  // S-pair control
  s_pair *new_var_pair(gb_elem *p, const int *lcm);
  s_pair *new_ring_pair(gb_elem *p, const int *lcm);
  s_pair *new_s_pair(gb_elem *p, gb_elem *q, const int *lcm);
  s_pair *new_gen(int i, gbvector * f, ring_elem denom);
  void remove_pair(s_pair *& p);
  int mark_pair(gb_elem *p, gb_elem *q) const;

  int search(const int *exp, int comp, gb_elem *&result);
  int compare(const gb_elem *p, const gb_elem *q) const;
  void find_pairs(gb_elem *p);
  void compute_s_pair(s_pair *p);
  int gb_reduce(gbvector * &f, gbvector * &fsyz);
  int gb_geo_reduce(gbvector * &f, gbvector * &fsyz);
  void gb_insert(gbvector *f, gbvector *fsyz, int ismin);
  void inter_reduce(gb_elem *&gens);

  int computation_complete(int stop_gb, int stop_syz, 
			   int stop_pairs, int stop_subring);

  int s_pair_step(s_pair *p);

  void resize(int nbits);

public:
  // Forcing a GB
  GBinhom_comp(const Matrix *m, const Matrix *gb, const Matrix *mchange, 
	  const Matrix *syz);

  // An honest GB computation
  GBinhom_comp(const Matrix *m, int collect_syz, int n_syz, int strategy);
  ~GBinhom_comp();

  // Performing the computation
  int calc(const int *deg, const intarray &stop_conditions);

  // Adding generators
  void add_gens(int lo, int hi, const Matrix *m);

  // reduction
  Matrix *reduce(const Matrix *m, Matrix *&lift);
  Vector *reduce(const Vector *v, Vector *&lift);

  virtual int contains(const Matrix *m);
  virtual bool is_equal(const gb_comp *q);

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix *min_gens_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();
  Matrix *syz_matrix();
  void debug_out(s_pair *q) const;
  void debug_pairs_out(gb_elem *p) const;
  void debug_pairs() const;
  void debug_out(buffer &o, s_pair *q) const;
  void debug_pairs_out(buffer &o, gb_elem *p) const;
  void debug_pairs(buffer &o) const;

  void stats() const;
};  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
