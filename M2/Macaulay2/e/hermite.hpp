// Copyright 1996  Michael E. Stillman
#ifndef _hermite_hh_
#define _hermite_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp-gb.hpp"
#include "ZZ.hpp"
#include <vector>

struct hm_elem : public our_new_delete
{
  hm_elem *next;
  mpz_t lead;
  vec f;
  vec fsyz;
};

/**
    @ingroup gb

    @brief Slow Hermite normal form computation over ZZ.  Replace.
*/
class HermiteComputation : public GBComputation
{
 private:
  int row;
  VECTOR(hm_elem *) initial;

  const Matrix *gens;  // This is the input

  hm_elem *GB_list;
  const FreeModule *Fsyz;
  VECTOR(vec) syz_list;

  int n_gb;
  int collect_syz;  // 0 or 1
  int n_comps_per_syz;

 private:
  hm_elem *new_gen(int i);
  void remove_hm_elem(hm_elem *&p);
  void insert(hm_elem *p);

  int compare_elems(hm_elem *f, hm_elem *g) const;
  hm_elem *merge(hm_elem *f, hm_elem *g);
  void sort(hm_elem *&p);
  void reduce(hm_elem *&p, hm_elem *q);

  void gb_reduce(vec &f) const;
  void gb_reduce(vec &f, vec &fsyz) const;

  virtual bool stop_conditions_ok() { return true; }
 public:
  // An honest GB computation
  HermiteComputation(const Matrix *m, int collect_syz, int n_syz);
  ~HermiteComputation();

  virtual void remove_gb() {}
  virtual void start_computation();

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.

  virtual Ring *get_ring() const { return globalZZ; }
  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const Matrix /* or null */ *get_gb();

  virtual const Matrix /* or null */ *get_mingens();

  virtual const Matrix /* or null */ *get_change();

  virtual const Matrix /* or null */ *get_syzygies();

  virtual const Matrix /* or null */ *get_initial(int nparts);

  ////////////////////////////////
  // Normal forms and lifting ////
  ////////////////////////////////

  virtual const Matrix /* or null */ *matrix_remainder(const Matrix *m);

  virtual M2_bool matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient);

  virtual int contains(const Matrix *m);

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o) const;
  // This displays statistical information, and depends on the
  // M2_gbTrace value.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
