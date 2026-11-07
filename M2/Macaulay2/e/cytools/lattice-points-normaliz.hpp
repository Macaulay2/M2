#ifndef _cytools_lattice_points_normaliz_hpp_
#define _cytools_lattice_points_normaliz_hpp_

#include <gmpxx.h>
#include <vector>

namespace M2::cytools {

struct LatticePointsNormalizResult {
  // Each inner vector is one lattice point of length `dim`.
  std::vector<std::vector<mpz_class>> points;
};

// Enumerate integer vectors x of length `dim` satisfying A*x <= b
// (componentwise), where A is m x dim and b has length m. The polytope
// must be bounded; an unbounded input is reported via std::runtime_error.
//
// Backed by libnormaliz::Cone<mpz_class> with Type::inhom_inequalities,
// so big integers are handled natively (no fits-in-int restriction).
//
// Convention is A*x <= b natively, matching the engine wrapper. (The
// box_enum-based sibling latticePoints() uses the inverted H*v >= rhs
// convention; see TODO in cytools/lattice_points.hpp about unifying.)
//
// Throws std::runtime_error on:
//   - shape mismatch (A.size() != b.size(), or row of A has length != dim)
//   - unbounded polyhedron (Normaliz reports the polytope as not computable)
//   - other libnormaliz failures (BadInputException, etc.)
//
// TODO: no user-facing options for now. Two libnormaliz knobs are worth
// surfacing later if needed: setVerbose(bool) (currently hard-coded false)
// for progress output on long runs, and GlobalTimeBound (double seconds)
// to abort runaway compute. Other libnormaliz options (Approximate,
// BottomDecomposition, BigInt, etc.) are advanced algorithm-selection flags
// whose defaults work; expose only on demand.
LatticePointsNormalizResult latticePointsNormaliz(
    int dim,
    const std::vector<std::vector<mpz_class>>& A,
    const std::vector<mpz_class>& b);

} // namespace M2::cytools

#endif
