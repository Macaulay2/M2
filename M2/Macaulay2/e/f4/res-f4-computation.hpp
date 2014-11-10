/* Copyright 2014, Michael E. Stillman */

#ifndef _res_f4_computation_hpp_
#define _res_f4_computation_hpp_

#include "gausser.hpp"
#include "polyring.hpp"

#include "../comp-res.hpp"
#include "../exceptions.hpp"

class PolynomialRing;
class FreeModule;
class F4Res {};

class F4ResComputation : public ResolutionComputation
{
public:
  F4ResComputation(const PolynomialRing& R,
                   const FreeModule& F,
                   int max_level);

  virtual ~F4ResComputation();
protected:
  // These functions override those in ResolutionComputation
  bool stop_conditions_ok() { 
    // We ignore all stopping conditions except length_limit, degree_limit
    return true;
  }

  void start_computation() { }

  int complete_thru_degree() const { throw exc::engine_error("complete_thru_degree not implemented"); }
  // The computation is complete up through this degree.

  void remove_res() { delete mComp; mComp = nullptr; }

  const Matrix /* or null */ *get_matrix(int level) { throw exc::engine_error("get_matrix not implemented"); }

  const FreeModule /* or null */ *get_free(int level) { 
    if (level > 0) return mRing.make_FreeModule(0);
    return &mFreeModule;
    //throw exc::engine_error("get_free not implemented"); 
  }

  M2_arrayint get_betti(int type) const { throw exc::engine_error("get_betti not implemented"); }
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer &o) const {
    o << "F4 resolution computation" << newline;
  }

  // This displays statistical information, and depends on the
  // M2_gbTrace value.
private:
  const PolynomialRing& mRing;
  const FreeModule& mFreeModule;
  F4Res* mComp; // This is a pointer so that finalizers can more easily remove the data
};

ResolutionComputation* createF4Res(const Matrix *m,
                                   int max_level,
                                   int strategy
                                   );

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
