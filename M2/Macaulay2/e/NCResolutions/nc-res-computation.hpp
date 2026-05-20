/* Copyright 2014-2021, Michael E. Stillman */

#ifndef _nc_res_computation_hpp_
#define _nc_res_computation_hpp_

/**
 * @file NCResolutions/nc-res-computation.hpp
 * @brief `NCResComputation` --- placeholder free-resolution driver for modules over a `FreeAlgebraQuotient`.
 *
 * Declares the intended non-commutative counterpart of
 * `F4ResComputation`: a `ResolutionComputation` subclass that
 * **will** compute a free resolution of a right module presented
 * by its Gröbner basis over a `FreeAlgebraQuotient` ring. The
 * constructor is private; the friend factory
 * `createNCRes(groebnerBasisMatrix, max_level, strategy)` is the
 * only entry point, and the input matrix is expected to be a
 * (partial) GB of the module being resolved.
 *
 * This adapter is currently a **non-functional placeholder**.
 * `stop_conditions_ok()` unconditionally returns `true`,
 * `start_computation()` just prints "Starting computation." to
 * `std::cout`, `complete_thru_degree()` returns `0`,
 * `get_betti` and the `MutableMatrix`-shaped `get_matrix`
 * return `nullptr`, the `Matrix`-shaped `get_matrix(level)`
 * returns either the input module's GB at level 1 or an empty
 * `MatrixConstructor` matrix elsewhere, `text_out` emits the
 * fixed string "Noncommutative resolution", and even the
 * stored fields (`mInputModuleGB`; commented-out `mRing` /
 * `mMaxLevel`) reflect the unfinished plumbing. The companion
 * `nc-res-computation.cpp` is 30 lines and only defines the
 * constructor plus `createNCRes`.
 *
 * The expected per-level loop is sketched in the long comment
 * block at the bottom of the header: build levels 0 / 1 from
 * the input module, fill in syzygies degree-by-degree using
 * F4-style overlap reduction against both the ring's GB and the
 * earlier-level frame, and store each `(free module,
 * differential)` pair as the resolution grows. The `#if 0`
 * block sketches `NCSchreyerResolution` / `Level` / `Element` /
 * `ModulePoly` types the implementation is expected to grow
 * into; `NCResolutions/notes.txt` tracks the still-open design
 * questions.
 *
 * @see comp-res.hpp
 * @see FreeAlgebraQuotient.hpp
 * @see NCGroebner.hpp
 * @see schreyer-resolution/res-f4-computation.hpp
 */

#include "comp-res.hpp"
#include "NCAlgebras/FreeAlgebraQuotient.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"

/**
 * @brief `ResolutionComputation` subclass that builds a free resolution over
 * a `FreeAlgebraQuotient` (non-commutative).
 *
 * @details Constructor is private; the free function `createNCRes` (declared
 * friend) instantiates the computation from a GB-presented quotient
 * matrix, a `max_level` cap on homological degree, and a strategy
 * flag. The class then plugs into the engine's standard
 * `ResolutionComputation` driver loop (`start_computation`,
 * partial-progress accessors, etc.) so non-commutative resolutions
 * look the same to front-end callers as the commutative ones.
 */
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

  MutableMatrix /* or null */* get_matrix(int slanted_degree, int level)
  {
    (void) slanted_degree;
    (void) level;
    return nullptr;
  }

  const FreeModule /* or null */* get_free(int level) {
    if (level == 0) return mInputModuleGB.rows();
    if (level == 1) return mInputModuleGB.cols();
    return mInputModuleGB.get_ring()->make_FreeModule(0);
  }

  M2_arrayint get_betti(int type) const
  {
    (void) type;
    return nullptr;
  }
  // type is documented under rawResolutionBetti, in engine.h

  void text_out(buffer& o) const {
    o << "Noncommutative resolution";
  }

 private:
  // input information coming from M2 objects
  ////Add in once we want it:  const FreeAlgebraQuotient& mRing;
  //const Matrix& mInputIdealGB; // probably a computation type from a partial GB.
  const Matrix& mInputModuleGB;  // maybe not a GB of the module either...
  ////Add in once we want it:  int mMaxLevel;
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
  ElementArray mElementArray;
  std::vector<int> mComponents;
  std::vector<int> mMonomials;
  // above could be std::vector of Monoms, or maybe even together
  // a std::vector of a struct.
};

#endif

// to create the frame:

// 1. Fill in level 0 - (Take the degree information from the target mInputModGB)
// 2. Fill in level 1 - (This is really just the info on the entries of mInputModGB)
//                      This is more difficult the GB is not known out to that point yet.
// 3. Fill in level 2 - This requires us to find all the spairs on module elements vs
//                      elements of the ring Groebner basis
// 4. Rinse and repeat.

// detailed notes on a basic example
// consider the right ideal (yyx,yxx)R with R = QQ<|x,y|>/(x*y - y*x)

// Step 1:
// level 0: right module generated by e_1

// Step 2:
// level 1: f_1 --> e_1yyx, f_2 --> e_1yxx

// other gb elements? (e_1yyx) * y - (e_1 yy)*(x*y - y*x) = e_1 yyyx
//                    (e_1yxx) * y - (e_1 yx)*(x*y - y*x) = e_1 yxyx ~> e_1yyxx - (e_1yyx)*x = 0

// f_3 --> e_1 yyyx, etc (another f_j for each e_i y^j x for j >= 3)

// Level 2: 

// g_1 --> f_1.y = e_1.yyx.y  <~~> e_1.yyxy = e_1.yyyx, now have to add f_3 to level 1

// g_1 --> f_1.y = e_1.yyx.y  <~~> f_1.y - f_3   Claim: this element maps to zero under the previous map
// g_1 will have label: e_1.yyx.y      e_1yyxy - e_1yyyx = e_1yy.(xy - yx) = 0

// g_2 --> f_2.y = e_1.yxx.y  <~~> f_2.y - f_1.x    e_1yxxy - e1yyxx = 0
// g_2 will have label: e_1.yxx.y

// g_3 --> f_3.y = e_1.yyyx.y <~~> f_3.y - f_4  (etc)
// g_3 will have label: e_1.yyyx.y

// for a term, we have:  label - product of all leadTerms all the way back to level 0, e.g. e_1.yyyx.y (without the dots)

// to compare
// g_1.m_1; g_1 = f_1.v_1
// g_2.m_2; g_2 = f_2.v_2, f_1 and f_2 are further back etc

// first compare total monomials of g_1.m_1 and g_2.m_2. if there is a winner, take that term.
// if there is a tie at this stage, 

// example:
// level 0: e_1
// level 1: f_1 = e_1.xy, f_2 = e_1.y^3, f_3 = e_1.yx
// level 2: g_1 = f_1.xyy g_2 = f_1.xxy, g_3 = f_2.x

// How to compare f_i.m and f_j.n:  f_1.xxx vs f_2.xy
//             labels are e_1.xyxxx  e_1.yyyxy (compare here).  One is a winner.

// Level 3:
// we are done, because:
//  1. All lead terms of images of g's are in different f components, and
//  2. There are no overlaps with these lead terms with the ring.

// level degree table:
// degrees rows, levels columns (betti diagram in M2)
// (entries indicate order of computation)

// entries with the same letter may be computed at the same time

/*
    0     1     2     3        0    1    2    3
0   a     .     .     .                  pb   pc (or *during*)
1   .     b     c     d
2   .     c     d
3   .     d
*/

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
