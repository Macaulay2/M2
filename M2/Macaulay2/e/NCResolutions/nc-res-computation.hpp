/* Copyright 2014-2021, Michael E. Stillman */

#ifndef _nc_res_computation_hpp_
#define _nc_res_computation_hpp_

#include "comp-res.hpp"

class NCResComputation : public ResolutionComputation
{
 private:
  NCResComputation(const Matrix* gbmatrix,
                   int max_level) {}

 public:
  friend ResolutionComputation* createNCRes(const Matrix* groebnerBasisMatrix,
                                            int max_level,
                                            int strategy);

  virtual ~NCResComputation() {}

 protected:
  // These functions override those in ResolutionComputation
  bool stop_conditions_ok()
  {
    // We ignore all stopping conditions except length_limit, degree_limit
    return true;
  }

  void start_computation() {}

  int complete_thru_degree() const { return 0; }
  // The computation is complete up through this slanted degree.

  const Matrix /* or null */* get_matrix(int level) {return nullptr; }

  MutableMatrix /* or null */* get_matrix(int slanted_degree, int level) {return nullptr; }

  const FreeModule /* or null */* get_free(int level) {return nullptr; }

  M2_arrayint get_betti(int type) const { return nullptr; }
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer& o) const {}

#if 0

 private:
  const PolynomialRing& mOriginalRing;
  const Matrix& mInputGroebnerBasis;
  std::unique_ptr<ResPolyRing> mRing;
  std::unique_ptr<SchreyerFrame> mComp;

#endif

};

ResolutionComputation* createNCRes(const Matrix* m,
                                   int max_level,
                                   int strategy);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
