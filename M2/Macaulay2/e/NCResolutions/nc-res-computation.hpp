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
                   const Matrix& gbIdealMatrix,
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
    if (level == 1) return &mInputModuleGB;
    MatrixConstructor matCon(get_free(level-1), get_free(level));
    return matCon.to_matrix();
  }

  MutableMatrix /* or null */* get_matrix(int slanted_degree, int level) {return nullptr; }

  const FreeModule /* or null */* get_free(int level) {
    if (level == 0) return mInputModuleGB.rows();
    if (level == 1) return mInputModuleGB.cols();
    return mInputModuleGB.get_ring()->make_FreeModule(0);
  }

  M2_arrayint get_betti(int type) const { return nullptr; }
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer& o) const {
    o << "Noncommutative resolution";
  }

 private:
  // input information coming from M2 objects
  const FreeAlgebraQuotient& mRing;
  //const Matrix& mInputIdealGB; // probably a computation type from a partial GB.
  const Matrix& mInputModuleGB;  // maybe not a GB of the module either...
  int mMaxLevel;
};

#if 0
//  Data Types
class NCSchreyerResolution
{
  // internal information for this resolution computation
  std::vector<Level> mLevels;
  MemoryBlock mMemoryBlock; // where all the monomials are stored
  std::unordered_set<Monom> mAllMonomials; // or <int*>?
};

class Level
{
  std::vector<Element> mElements;
  // SchreyerOrder at this level
};

class Element
{
  ModulePoly mPoly;
  // degree
  // component of previous step
  // bool indicating that the element has been computed entirely?
};

class ModulePoly
{
  CoeffVector mCoeffVector;
  std::vector<int> mMonomials;
};

#endif

// to create the frame:

// 1. Fill in level 0 - (Take the degree information from the target mInputModGB)
// 2. Fill in level 1 - (This is really just the info on the entries of mInputModGB)
//                      This is more difficult the GB is not known out to that point yet.
// 3. Fill in level 2 - This requires us to find all the spairs on module elements vs
//                      elements of the ring Groebner basis
// 4. Rinse and repeat.

// creation of the frame
// creation of level0and1 (translation of matrix to our information)
// fillInSyzygies - fill in a level and degree of the frame.
// F4 matrix creation and reduction code
// take result in the frame and translate back
ResolutionComputation* createNCRes(const Matrix* m,
                                   int max_level,
                                   int strategy);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
