/**
 * \author Micahel E. Stillman
 * \copyright Copyright 1999  Michael E. Stillman
*/
#ifndef M2_COMPUTATIONS_ESCHREYER_HPP_
#define M2_COMPUTATIONS_ESCHREYER_HPP_

#include "rings/polyring.hpp"
#include "groebner-computations/gbring.hpp"
#include "free-modules/schreyer-orders.hpp"
#include "matrices/matrix.hpp"
#include "monoid.hpp"
#include "computations/comp.hpp"

struct GBMatrix : public our_new_delete
{
  const FreeModule *F;  // target
  gc_vector<gbvector*> elems;

  GBMatrix(const Matrix *m);
  GBMatrix(const FreeModule *F);
  void append(gbvector *f);  // grabs f
  const FreeModule *get_free_module() const { return F; }
  Matrix *to_matrix();
};

class GBKernelComputation : public Computation
{
  // these three were virtual in class Computation
  bool stop_conditions_ok() { return true; }
  int complete_thru_degree() const { return 0; }
  void start_computation() {}
  const PolynomialRing *R;
  const Ring *K;
  GBRing *GR;
  const Monoid *M;
  const SchreyerOrder *SF;  ///< order for F.
  const SchreyerOrder *SG;  ///< order for G.
  const FreeModule *F;      ///< This is where the action is...
  const FreeModule *G;      ///< This is where the resulting syzygies live.
  // This MUST be a Schreyer free module compatible with the input!

  gc_vector<MonomialIdeal*> mi;   ///< Used in reduction.
  gc_vector<gbvector*> gb;        ///< This is the "stripped" GB.
  gc_vector<gbvector*> syzygies;  ///< This is basically the result.

  size_t exp_size; ///< byte size for allocating temp exp vectors on the stack
  size_t monom_size; ///< byte size for allocating monomials on the stack

  int n_ones;
  int n_unique;
  int n_others;
  int total_reduce_count;

  void new_pairs(int i);
  void strip_gb(const gc_vector<gbvector*> &m);
  void strip_gb(const GBMatrix *m);

  /**
   * This routine grabs 'c', and 'monom' should be the total monomial.
   */
  gbvector *make_syz_term(ring_elem c, const_monomial monom, int comp) const;

  bool find_ring_divisor(const_exponents exp, const gbvector *&result);

  /**
   * Returns the index of the least element in the monomial order which divides.
   */
  int find_divisor(const MonomialIdeal *mi, const_exponents exp, int &result);

  /**
   * removes every term of f which is not a lead term of some element of gb.
   */
  void wipe_unneeded_terms(gbvector *&f);

  gbvector *s_pair(gbvector *syz);

  /**
   * Reduces g to zero.  gsyz is real result.
   */
  void reduce(gbvector *&g, gbvector *&gsyz);

  /**
   * Reduces g to zero.  gsyz is real result.
   */
  void geo_reduce(gbvector *&g,
                  gbvector *&gsyz);  
 public:
  GBKernelComputation(const GBMatrix *m);

  virtual ~GBKernelComputation();

  int calc();

  GBMatrix *get_syzygies();

 public:
  GBKernelComputation *cast_to_GBKernelComputation() { return this; }
  const GBKernelComputation *cast_to_GBKernelComputation() const
  {
    return this;
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
