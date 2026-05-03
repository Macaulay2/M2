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
// max_N_out is a soft cap: when reached, the search stops and the points
// found so far are returned. Detect truncation by comparing
// result.points.size() against max_N_out.
// Throws std::runtime_error if:
//   - dim > 256 (unsupported by underlying box_enum)
//   - the search visited more than max_N_nodes nodes (hard cap, since the
//     caller cannot easily distinguish "ran out of budget" from "done")
LatticePointsResult latticePoints(
    int dim,
    int B,
    const std::vector<std::vector<int>>& H,
    const std::vector<int>& rhs,
    long max_N_out,
    long max_N_nodes);

} // namespace M2::cytools

#endif
