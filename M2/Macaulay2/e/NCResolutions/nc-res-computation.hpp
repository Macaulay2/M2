/* Copyright 2014-2021, Michael E. Stillman */

#ifndef _nc_res_computation_hpp_
#define _nc_res_computation_hpp_

#include "comp-res.hpp"
#include "NCAlgebras/FreeAlgebraQuotient.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"

class NCResComputation : public ResolutionComputation
{
 private:
  NCResComputation(const FreeAlgebraQuotient& ring,
                   const Matrix& gbMatrix,
                   int max_level);

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

  void start_computation() { 
    std::cout << "Starting computation." << std::endl;
  }

  int complete_thru_degree() const { return 0; }
  // The computation is complete up through this slanted degree.

  const Matrix /* or null */* get_matrix(int level) {
    if (level == 1) return &mInputGroebnerBasis;
    MatrixConstructor matCon(get_free(level-1), get_free(level));
    return matCon.to_matrix();
  }

  MutableMatrix /* or null */* get_matrix(int slanted_degree, int level) {return nullptr; }

  const FreeModule /* or null */* get_free(int level) {
    if (level == 0) return mInputGroebnerBasis.rows();
    if (level == 1) return mInputGroebnerBasis.cols();
    return mInputGroebnerBasis.get_ring()->make_FreeModule(0);
  }

  M2_arrayint get_betti(int type) const { return nullptr; }
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer& o) const {
    o << "Noncommutative resolution";
  }

 private:
  // something like a Schreyer frame which will be the free res object

  const FreeAlgebraQuotient& mRing;
  const Matrix& mInputGroebnerBasis;
  int mMaxLevel;
};

ResolutionComputation* createNCRes(const Matrix* m,
                                   int max_level,
                                   int strategy);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
