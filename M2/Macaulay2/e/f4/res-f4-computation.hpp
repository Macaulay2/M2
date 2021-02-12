/* Copyright 2014-2016, Michael E. Stillman */

#ifndef _res_f4_computation_hpp_
#define _res_f4_computation_hpp_

#include "res-poly-ring.hpp"

#include "polyring.hpp"
#include "../comp-res.hpp"

#include <memory>  // For std::unique_ptr

class SchreyerFrame;

class F4ResComputation : public ResolutionComputation
{
 private:
  F4ResComputation(const PolynomialRing* origR,
                   ResPolyRing* R,
                   const Matrix* gbmatrix,
                   int max_level);

 public:
  friend ResolutionComputation* createF4Res(const Matrix* groebnerBasisMatrix,
                                            int max_level,
                                            int strategy);

  virtual ~F4ResComputation();

  // Compute (if needed) enough to return the minimal Betti numbers
  // of the free resolution.
  // 'length_limit': value is infinity if it has length 0, else it is first
  // entry.
  // 'slanted_degree_limit': value is infinity if it has length 0, else it is
  // first entry.
  // then all returned values will be correct for
  M2_arrayint minimal_betti(M2_arrayint slanted_degree_limit,
                            M2_arrayint length_limit);

 public:
  MutableMatrix /* or null */* get_mutable_matrix(const Ring* R, int level);
  MutableMatrix /* or null */* get_mutable_matrix(const Ring* KK,
                                                  int slanted_degree,
                                                  int level);

 protected:
  // These functions override those in ResolutionComputation
  bool stop_conditions_ok()
  {
    // We ignore all stopping conditions except length_limit, degree_limit
    return true;
  }

  SchreyerFrame& frame() { return *mComp; }
  void start_computation();

  int complete_thru_degree() const;
  // The computation is complete up through this slanted degree.

  const Matrix /* or null */* get_matrix(int level);

  MutableMatrix /* or null */* get_matrix(int slanted_degree, int level);

  const FreeModule /* or null */* get_free(int level);

  M2_arrayint get_betti(int type) const;
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer& o) const;

 private:
  const PolynomialRing& mOriginalRing;
  const Matrix& mInputGroebnerBasis;
  std::unique_ptr<ResPolyRing> mRing;
  std::unique_ptr<SchreyerFrame> mComp;
};

ResolutionComputation* createF4Res(const Matrix* m,
                                   int max_level,
                                   int strategy);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
