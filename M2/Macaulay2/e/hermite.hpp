// Copyright 1996  Michael E. Stillman
#ifndef _hermite_hh_
#define _hermite_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "gb_comp.hpp"
#include "ZZ.hpp"

struct hm_elem
{
  hm_elem *next;
  mpz_t lead;
  vec f;
  vec fsyz;
};

class HermiteComputation : public gb_comp
{
private:
  int row;
  array<hm_elem *> initial;

  const Matrix *gens;			// This is the input

  hm_elem *GB_list;
  Matrix *syz;

  int n_gb;
  int collect_syz;	// 0 or 1
  int n_comps_per_syz;

private:
  hm_elem *new_gen(int i);
  void remove_hm_elem(hm_elem *&p);
  void insert(hm_elem *p);

  int compare_elems(hm_elem *f, hm_elem *g) const;
  hm_elem *merge(hm_elem *f, hm_elem *g);
  void sort(hm_elem *&p);
  void reduce(hm_elem *&p, hm_elem *q);
public:
  // An honest GB computation
  HermiteComputation(const Matrix *m, int collect_syz, int n_syz);
  ~HermiteComputation();

  // performing the computation
  int calc(const int *deg, const intarray &stop);  // 'deg' is ignored here

  // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
  Matrix *min_gens_matrix();
  Matrix *initial_matrix(int n);
  Matrix *gb_matrix();
  Matrix *change_matrix();
  Matrix *syz_matrix();
  void stats() const;

  void gb_reduce(vec &f, vec &fsyz) const;
  Matrix *reduce(const Matrix *m, Matrix *&lift);
  Vector *reduce(const Vector *v, Vector *&lift);

  virtual int contains(const Matrix *m);
  virtual bool is_equal(const gb_comp *q);
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
