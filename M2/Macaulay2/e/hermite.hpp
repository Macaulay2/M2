// Copyright 1996  Michael E. Stillman
#ifndef _hermite_hh_
#define _hermite_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp.hpp"
#include "comp_gb.hpp"
#include "ZZ.hpp"

struct hm_elem : public our_new_delete
{
  hm_elem *next;
  mpz_t lead;
  vec f;
  vec fsyz;
};

class HermiteComputation : public GBComputation
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

  void gb_reduce(vec &f, vec & /*fsyz*/) const;
  
  // performing the computation
  int calc(const int *deg, const intarray &stop);  // 'deg' is ignored here
public:
  // An honest GB computation
  HermiteComputation(const Matrix *m, int collect_syz, int n_syz);
  ~HermiteComputation();

  virtual void start_computation() = 0;

  virtual enum ComputationStatusCode gb_status(int *degree); 
  // The computation is complete up through this degree.

  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const MatrixOrNull *get_gb();

  virtual const MatrixOrNull *get_mingens();

  virtual const MatrixOrNull *get_change();

  virtual const MatrixOrNull *get_syzygies();

  virtual const MatrixOrNull *get_initial(int nparts);

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m);

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient);

  virtual int contains(const Matrix *m);

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o);
  // This displays statistical information, and depends on the
  // gbTrace value.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
