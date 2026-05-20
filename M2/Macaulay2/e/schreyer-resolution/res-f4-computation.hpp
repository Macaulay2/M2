/* Copyright 2014-2021, Michael E. Stillman */

#ifndef _res_f4_computation_hpp_
#define _res_f4_computation_hpp_

/**
 * @file schreyer-resolution/res-f4-computation.hpp
 * @brief `F4ResComputation` --- top-level Schreyer-frame F4 free-resolution driver.
 *
 * Declares the `ResolutionComputation` subclass that owns a
 * `SchreyerFrame`, walks it homological level by level, and
 * reports Betti tables / free modules / differential matrices
 * back to the M2 interpreter. The constructor is private; the
 * friend factory `createF4Res(groebnerBasisMatrix, max_level,
 * strategy, numThreads, parallelizeByDegree)` is the only entry
 * point and requires the input matrix to already be a Gröbner
 * basis with respect to the chosen monomial order.
 * `minimal_betti(slanted_degree_limit, length_limit)` returns
 * the minimal Betti numbers within those caps, and the two
 * `get_mutable_matrix` overloads (whole-level and
 * `(slanted_degree, level)`-cell) expose Macaulay-matrix data
 * without forcing the full resolution to complete.
 *
 * Two `Ring`-flavoured pointers thread through the
 * implementation: a `const PolynomialRing& mOriginalRing` (the
 * user-facing ring) and a `std::unique_ptr<ResPolyRing> mRing`
 * (the resolution-tuned ring from `res-poly-ring.hpp`); the
 * split mirrors the legacy `PolynomialRing` / `GBRing`
 * separation on the GB side. Only `length_limit` and
 * `degree_limit` from `Computation`'s stop conditions are
 * honoured (`stop_conditions_ok()` returns `true`
 * unconditionally with a comment explaining why).
 * `parallelizeByDegree` is plumbed through to the
 * `SchreyerFrame` constructor as a single on/off flag --- when
 * true, `minimalBettiNumbers` builds and runs the
 * `DependencyGraph`; when false, it falls back to the serial
 * `computeRanks` path.
 *
 * @see comp-res.hpp
 * @see res-poly-ring.hpp
 * @see res-schreyer-frame.hpp
 * @see res-f4.hpp
 * @see res-dep-graph.hpp
 */

#include "res-poly-ring.hpp"
#include "polyring.hpp"
#include "comp-res.hpp"

#include <memory>  // For std::unique_ptr

class SchreyerFrame;

/**
 * @brief `ResolutionComputation` subclass that drives the F4 resolution
 * engine (`SchreyerFrame` + `F4Res`) from the engine-side API.
 *
 * @details Constructor is private; the free function `createF4Res` (declared
 * friend) instantiates it from a GB-presented input matrix plus
 * a `max_level` cap, thread count, and degree-parallel flag.
 * Owns the underlying `ResPolyRing` and the `SchreyerFrame` that
 * holds the in-progress resolution. The standard
 * `ResolutionComputation` driver loop walks the frame
 * `(level, slanted_degree)` cell by cell, calling into `F4Res`
 * to build and reduce the Macaulay matrix at each.
 */
class F4ResComputation : public ResolutionComputation
{
 private:
  F4ResComputation(const PolynomialRing* origR,
                   ResPolyRing* R,
                   const Matrix* gbmatrix,
                   int max_level,
                   int numThreads,
                   bool parallelizeByDegree);

 public:
  friend ResolutionComputation* createF4Res(const Matrix* groebnerBasisMatrix,
                                            int max_level,
                                            int strategy,
                                            int numThreads,
                                            bool parallelizeByDegree);

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
                                   int strategy,
                                   int numThreads,
                                   bool parallelizeByDegree);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
