#ifndef __res_f4_to_m2_interface_hpp_
#define __res_f4_to_m2_interface_hpp_

/**
 * @file schreyer-resolution/res-f4-m2-interface.hpp
 * @brief Conversion layer between engine `Matrix` / `vec` types and the F4-resolution internal types.
 *
 * Declares `ResF4toM2Interface`, a static-only helper class
 * that crosses the boundary between the user-facing `Matrix` /
 * `vec` / `FreeModule` / `PolynomialRing` vocabulary and the
 * resolution-tuned `ResPolynomial` / `ResPolyRing` /
 * `SchreyerFrame` vocabulary. `from_M2_vec` translates an
 * input `vec` (sparse `(component, coeff)` list over the M2
 * ring) into a `ResPolynomial`; `to_M2_vec`,
 * `to_M2_freemodule`, `to_M2_matrix`, and the two
 * `to_M2_MutableMatrix` variants walk back, including a variant
 * that returns a dense matrix over a `ZZ/p` coefficient ring
 * for a specific `(level, degree)` Macaulay submatrix.
 * `setDegreeZeroMap` extracts the degree-zero part of a level
 * for the minimal-Betti accelerator.
 *
 * `F4ResComputation` translates inputs once at startup and
 * outputs once on demand, so the conversion cost lives entirely
 * at the boundaries and never inside the inner reduction loop.
 * Counterpart to `f4/f4-m2-interface.hpp` on the GB side.
 *
 * @see res-poly-ring.hpp
 * @see res-schreyer-frame.hpp
 * @see res-f4-computation.hpp
 * @see f4/f4-m2-interface.hpp
 */

#include "ringelem.hpp"  // for vec
#include <utility>       // for pair
class FreeModule;
class Matrix;
class MutableMatrix;
class PolynomialRing;
class ResPolyRing;
class Ring;
class SchreyerFrame;
class ResPolynomial;
template <typename ACoeffRing> class DMat;

/**
 * @brief Static-method namespace bridging engine `Matrix` / `vec` values
 * and the resolution engine's `ResPolynomial` representation.
 *
 * @details Resolution-side counterpart of `F4toM2Interface`: every method
 * is `static`. Provides `from_M2_vec` / `to_M2_vec` and the
 * matrix-wide variants used by `F4ResComputation` when handing
 * input to the `SchreyerFrame` and reading the resulting
 * `ResPolynomial` syzygies back into engine-side `vec`s and
 * `Matrix*`es for the front end.
 */
class ResF4toM2Interface
{
 public:
  static void from_M2_vec(const ResPolyRing& R,
                          const FreeModule* F,
                          vec v,
                          ResPolynomial& result);

  static vec to_M2_vec(const ResPolyRing& R,
                       const ResPolynomial& f,
                       const FreeModule* F);

  static FreeModule* to_M2_freemodule(const PolynomialRing* R,
                                      SchreyerFrame& C,
                                      int lev);

  static FreeModule* to_M2_freemodule(const PolynomialRing* R,
                                      const FreeModule* F,
                                      SchreyerFrame& C,
                                      int lev);

  static Matrix* to_M2_matrix(SchreyerFrame& C,
                              int lev,
                              const FreeModule* tar,
                              const FreeModule* src);

  static MutableMatrix* to_M2_MutableMatrix(SchreyerFrame& C,
                                            const Ring* R,  // a polynomial
                                                            // ring, same monoid
                                                            // as C, coeffs
                                                            // allowed depend on
                                                            // ring of C.
                                            int lev);

  static MutableMatrix* to_M2_MutableMatrix(
      SchreyerFrame& C,
      const Ring* K,  // should be a ZZ/p ring.
      int lev,
      int degree);

  template <typename RingType>
  static double setDegreeZeroMap(SchreyerFrame& C,
                                 DMat<RingType>& result,
                                 int slanted_degree,
                                 int lev);

  static std::pair<Matrix*,double> setDegreeZeroMap(SchreyerFrame& C,
                                                    int slanted_degree,
                                                    int lev);

};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
