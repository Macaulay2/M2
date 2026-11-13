#include "cytools/lattice_points.hpp"

#include <cstdint>
#include <stdexcept>
#include <string>

// Forward-declare the C entry point from box_enum.h with C linkage.
// We do NOT #include "box_enum.h" here because that header is C99 (uses
// `restrict` and VLAs in its declaration/body); it is compiled separately
// as C via cytools/box_enum.c.
extern "C" {
int _box_enum_c(
    int32_t* out,
    long* N_out,
    long* N_nodes,
    int dim,
    int B,
    int* H,
    int* rhs,
    int N_hyps,
    long max_N_out,
    long max_N_nodes);
}

namespace M2::cytools {

LatticePointsResult latticePoints(
    int dim,
    int B,
    const std::vector<std::vector<int>>& H,
    const std::vector<int>& rhs,
    long max_N_out,
    long max_N_nodes)
{
  const int n_hyps = static_cast<int>(H.size());
  if (static_cast<size_t>(n_hyps) != rhs.size())
    throw std::runtime_error(
        "latticePoints: H.size() must equal rhs.size()");
  for (const auto& row : H)
    if (static_cast<int>(row.size()) != dim)
      throw std::runtime_error(
          "latticePoints: every row of H must have length dim");

  // Flatten H row-major.
  std::vector<int> H_flat;
  H_flat.reserve(static_cast<size_t>(n_hyps) * static_cast<size_t>(dim));
  for (const auto& row : H)
    H_flat.insert(H_flat.end(), row.begin(), row.end());

  std::vector<int> rhs_copy = rhs; // mutable buffer for non-const C pointer
  std::vector<std::int32_t> out(static_cast<size_t>(max_N_out)
                                * static_cast<size_t>(dim));

  long n_out = 0;
  long n_nodes = 0;

  int status = _box_enum_c(
      out.data(),
      &n_out,
      &n_nodes,
      dim,
      B,
      H_flat.empty() ? nullptr : H_flat.data(),
      rhs_copy.empty() ? nullptr : rhs_copy.data(),
      n_hyps,
      max_N_out,
      max_N_nodes);

  if (status == -1)
    throw std::runtime_error(
        "rawLatticePoints: dim > 256 not supported by box_enum");
  // status == -2 (max_N_out reached) is intentionally NOT an error: return
  // the points collected so far and let the caller detect truncation by
  // comparing result.points.size() against max_N_out.
  if (status == -3)
    throw std::runtime_error(
        "rawLatticePoints: exceeded max_N_nodes = "
        + std::to_string(max_N_nodes) + " search nodes");
  if (status != 0 && status != -2)
    throw std::runtime_error(
        "rawLatticePoints: unknown box_enum status "
        + std::to_string(status));

  LatticePointsResult result;
  result.n_nodes = n_nodes;
  result.points.resize(static_cast<size_t>(n_out),
                       std::vector<int>(static_cast<size_t>(dim)));
  for (long i = 0; i < n_out; ++i)
    for (int j = 0; j < dim; ++j)
      result.points[static_cast<size_t>(i)][static_cast<size_t>(j)] =
          static_cast<int>(out[static_cast<size_t>(i) * dim + j]);

  return result;
}

} // namespace M2::cytools
