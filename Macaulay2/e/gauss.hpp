// Copyright 1996  Michael E. Stillman
#ifndef _gauss_hh_
#define _gauss_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"

struct gm_elem
{
  gm_elem *next;
  int nterms;
  vec f;
  vec fsyz;
};

class GaussElimComputation : public gb_comp
{
private:
  int row;
  gm_elem **reduce_list;	// One list for each row
  gm_elem **gb_list;		// The GB elem (if any) with the given row index
				// as lead term.
  const Matrix *gens;			// This is the input
  Matrix *syz;

  int n_gb;
  int n_pairs, n_syz;

  int collect_syz;	// 0 or 1
  int n_comps_per_syz;

private:
  gm_elem *new_gen(int i);
  void remove_gm_elem(gm_elem *&p);
  void insert(gm_elem *p);

  void reduce(gm_elem *&p, gm_elem *q);
  void reduce(vec &f, vec &fsyz);
public:
  // An honest GB computation
  GaussElimComputation(const Matrix *m, int collect_syz, int n_syz);
  ~GaussElimComputation();

  // performing the computation
  int calc(const int *deg, const intarray &stop);  // 'deg' is ignored here

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix *min_gens_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();
  Matrix *syz_matrix();
  void stats() const;

  Matrix *reduce(const Matrix *m, Matrix *&lift);
  Vector *reduce(const Vector *v, Vector *&lift);

  virtual int contains(const Matrix *m);
  virtual bool is_equal(const gb_comp *q);
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
