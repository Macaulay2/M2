// Copyright 1996-2005  Michael E. Stillman
#ifndef _gauss_hh_
#define _gauss_hh_

#include "relem.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "comp-gb.hpp"

struct gm_elem : public our_new_delete
{
  gm_elem *next;
  int nterms;
  vec f;
  vec fsyz;
};

/**
    @ingroup gb

    @brief Gaussian elimination class.  To be rewritten.

    This class provides very slow Gaussian elimination functionality, and also
   only over
    (exact) fields.
*/
class GaussElimComputation : public GBComputation
{
 private:
  int row;
  gm_elem **reduce_list;  // One list for each row
  gm_elem **gb_list;      // The GB elem (if any) with the given row index
                          // as lead term.

  const Ring *R;       // Ring of 'gens'.  Should be a field.
  const Matrix *gens;  // This is the input
  const FreeModule *Fsyz;
  VECTOR(vec) syz_list;

  int n_gb;
  int n_pairs, n_syz;

  int collect_syz;  // 0 or 1
  int n_comps_per_syz;

 private:
  gm_elem *new_gen(int i);
  void remove_gm_elem(gm_elem *&p);
  void insert(gm_elem *p);

  void reduce(gm_elem *&p, gm_elem *q);
  void reduce(vec &f, vec &fsyz, bool tail_only = false);
  void reduce(vec &f);

 protected:
  virtual bool stop_conditions_ok() { return true; }
 public:
  // An honest GB computation
  GaussElimComputation(const Matrix *m, int collect_syz, int n_syz);
  ~GaussElimComputation();

  virtual void remove_gb() {}
  virtual void start_computation();

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.

  virtual const Ring *get_ring() const { return R; }
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
