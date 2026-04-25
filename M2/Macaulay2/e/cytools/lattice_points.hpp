#ifndef _cytools_lattice_points_hpp_
#define _cytools_lattice_points_hpp_

#include <vector>

namespace M2::cytools {

struct LatticePointsResult {
  // Each inner vector is one lattice point of length `dim`.
  std::vector<std::vector<int>> points;
  // Number of nodes visited in the search tree (diagnostic).
  long n_nodes = 0;
};

// Enumerate integer vectors `v` of length `dim` satisfying
//   H * v >= rhs  (componentwise)  and  |v_i| <= B  for all i.
//
// `H` has N_hyps rows of length `dim` (may be empty for pure box enumeration).
// `rhs` has length N_hyps (must match H.size(); may be empty if H is empty).
//
// Throws std::runtime_error if:
//   - dim > 256 (unsupported by underlying box_enum)
//   - the search produced more than max_N_out points
//   - the search visited more than max_N_nodes nodes
LatticePointsResult latticePoints(
    int dim,
    int B,
    const std::vector<std::vector<int>>& H,
    const std::vector<int>& rhs,
    long max_N_out,
    long max_N_nodes);

} // namespace M2::cytools

#endif
