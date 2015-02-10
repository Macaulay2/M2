/* Copyright 2014, Michael E. Stillman */

#ifndef _res_f4_computation_hpp_
#define _res_f4_computation_hpp_

#include "res-gausser.hpp"
#include "polyring.hpp"

#include "../comp-res.hpp"
#include "../exceptions.hpp"
#include "res-f4.hpp"
#include <memory>
#include <iostream>

class PolynomialRing;
class FreeModule;
class F4Res;

class F4ResComputation : public ResolutionComputation
{
private:
  F4ResComputation(const PolynomialRing* R,
                   const Matrix* gbmatrix,
                   ResF4Mem* Mem,
                   const ResGausser* KK,
                   const MonomialInfo* MI,
                   int max_level);
public:
  friend ResolutionComputation* createF4Res(const Matrix *groebnerBasisMatrix,
                                            int max_level,
                                            int strategy
                                            );

  virtual ~F4ResComputation();
protected:
  // These functions override those in ResolutionComputation
  bool stop_conditions_ok() { 
    // We ignore all stopping conditions except length_limit, degree_limit
    return true;
  }

  SchreyerFrame& frame() { return mComp->frame(); }

  void start_computation()
  {
    std::cout << "F4ResComputation::start_computation() needs to be written" << std::endl;
    mComp->construct(2,3); // testing
    mComp->construct(2,4);
    mComp->construct(2,5);
    mComp->construct(3,4);
  }

  int complete_thru_degree() const { throw exc::engine_error("complete_thru_degree not implemented"); }
  // The computation is complete up through this degree.

  void remove_res() { mComp.reset(); mComp = nullptr; }

  const Matrix /* or null */ *get_matrix(int level);

  MutableMatrix /* or null */ *get_matrix(int level, int degree);

  const FreeModule /* or null */ *get_free(int level);

  M2_arrayint get_betti(int type) const { return mComp->getBetti(type); }
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer &o) const {
    o << "F4 resolution computation" << newline;
  }
private:
  const PolynomialRing& mOriginalRing;
  const ResPolyRing mRing;
  const Matrix& mInputGroebnerBasis;
  ResF4Mem* mMem;
  std::unique_ptr<F4Res> mComp;
  //  F4Res* mComp; // This is a pointer so that finalizers can more easily remove the data
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
