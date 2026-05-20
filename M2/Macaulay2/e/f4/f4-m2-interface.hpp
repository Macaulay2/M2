#ifndef __F4toM2interface_h_
#define __F4toM2interface_h_

/**
 * @file f4/f4-m2-interface.hpp
 * @brief `F4toM2Interface` --- static translators between engine `vec` / `Matrix` and F4's `GBF4Polynomial`.
 *
 * Declares the static-only `F4toM2Interface` namespace-class
 * that converts each direction across the F4 boundary. Ingest
 * side: `from_M2_vec` packs one engine `vec` into a single
 * `GBF4Polynomial`, and `from_M2_matrix` walks the columns of
 * an input `Matrix` and fills a `gb_array` of
 * `GBF4Polynomial`s for the F4 generator list. Egress side:
 * `to_M2_vec` unpacks one `GBF4Polynomial` back into an engine
 * `vec`, `to_M2_matrix` produces a `Matrix` from a
 * `gb_array` (used by the GB extraction path), and
 * `to_M2_MutableMatrix` reconstructs a `MutableMatrix` directly
 * from a Macaulay `coefficient_matrix` plus the generator and
 * basis arrays (used by the debug / introspection path).
 * `poly_set_degrees(VA, MI, wts, f, deg, alpha)` reads the
 * weight-vector degree `deg` and the homogenising degree
 * `alpha` of a packed polynomial for sugar / degree tracking
 * inside `F4GB`.
 *
 * Every method threads through a `VectorArithmetic *` (the
 * per-coefficient-ring fast path) and a `MonomialInfo *` (the
 * F4 packed-monomial layout) so the helpers stay stateless.
 * Both `f4-computation.cpp` (input ingestion + result vec
 * extraction) and `f4.cpp` itself (per-element vec extraction
 * during basis output and MutableMatrix reconstruction) call
 * into this interface.
 *
 * @see f4-computation.hpp
 * @see f4-types.hpp
 * @see moninfo.hpp
 */

#include "interface/m2-types.h"  // for M2_arrayint
#include "f4/f4-types.hpp"   // for gb_array, GBF4Polynomial (ptr only), coefficient_m...
#include "ringelem.hpp"      // for vec

class FreeModule;
class VectorArithmetic;
class Matrix;
class MonomialInfo;
class MutableMatrix;

/**
 * @brief Static-method namespace for translating between engine `Matrix` /
 * `vec` polynomials and the F4-internal `GBF4Polynomial` form.
 *
 * @details Every method is `static` --- the class holds no state. It bundles
 * the conversion glue (`poly_set_degrees`, `to_M2_vec`,
 * `from_M2_vec`, matrix-wide variants) that `F4Computation` uses
 * when handing input to `F4GB` and reading its output back into
 * the engine's `Matrix` / `MutableMatrix` types.
 */
class F4toM2Interface
{
 public:
  static void poly_set_degrees(const VectorArithmetic* VA,
                               const MonomialInfo *MI,
                               const M2_arrayint wts,
                               const GBF4Polynomial &f,
                               int &deg,
                               int &alpha);

  static void from_M2_vec(const VectorArithmetic* VA,
                          const MonomialInfo *MI,
                          const FreeModule *F,
                          vec v,
                          GBF4Polynomial &result);

  static vec to_M2_vec(const VectorArithmetic* VA,
                       const MonomialInfo *MI,
                       const GBF4Polynomial &f,
                       const FreeModule *F);

  static void from_M2_matrix(const VectorArithmetic* VA,
                             const MonomialInfo *MI,
                             const Matrix *m,
                             gb_array &result_polys);

  static Matrix *to_M2_matrix(const VectorArithmetic* VA,
                              const MonomialInfo *MI,
                              gb_array &polys,
                              const FreeModule *F);

  static MutableMatrix *to_M2_MutableMatrix(const VectorArithmetic* VA,
                                            coefficient_matrix *mat,
                                            gb_array &gens,
                                            gb_array &gb);
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  indent-tabs-mode: nil
//  End:
