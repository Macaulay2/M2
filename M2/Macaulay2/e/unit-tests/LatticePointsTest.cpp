// Tests for the pure-C++ lattice-point enumerator
// `M2::cytools::latticePoints` (Kannan/box_enum, in cytools/lattice_points.hpp).
//
// Convention reminder: this helper enumerates v with H*v >= rhs (componentwise)
// and |v_i| <= B. The engine wrapper `rawLatticePoints` in interface/cone.cpp
// is what flips signs to expose the user-facing A*x <= b convention; that
// wrapper is exercised in a separate engine-side test group.

#include <set>
#include <stdexcept>
#include <vector>

#include <gtest/gtest.h>

#include "cytools/lattice_points.hpp"

using M2::cytools::latticePoints;
using M2::cytools::LatticePointsResult;

namespace {

// Brute-force enumeration of lattice points in [-B,B]^dim satisfying
// H * v >= rhs (componentwise). Returns a set for easy comparison against
// the box_enum-produced points.
std::set<std::vector<int>> bruteForce(
    int dim, int B,
    const std::vector<std::vector<int>>& H,
    const std::vector<int>& rhs)
{
  std::set<std::vector<int>> out;
  std::vector<int> v(dim, -B);
  while (true)
    {
      bool ok = true;
      for (size_t j = 0; j < H.size(); ++j)
        {
          long s = 0;
          for (int i = 0; i < dim; ++i)
            s += static_cast<long>(H[j][i]) * static_cast<long>(v[i]);
          if (s < rhs[j]) { ok = false; break; }
        }
      if (ok) out.insert(v);

      // increment v lexicographically over [-B,B]^dim
      int i = 0;
      while (i < dim && v[i] == B) { v[i] = -B; ++i; }
      if (i == dim) break;
      ++v[i];
    }
  return out;
}

std::set<std::vector<int>> asSet(const LatticePointsResult& r)
{
  return std::set<std::vector<int>>(r.points.begin(), r.points.end());
}

// Generous defaults for the search caps in tests where we expect the search
// to terminate well before either limit. 1<<24 nodes is plenty for the small
// problems below; the largest brute-force ground truth here is (2*3+1)^3 = 343.
constexpr long kBigN  = 1L << 20;
constexpr long kBigNN = 1L << 24;

}  // namespace

// ---------------------------------------------------------------------------
// Pure box enumeration (no hyperplane constraints).
// ---------------------------------------------------------------------------

TEST(LatticePoints, BoxOnly_dim2_B1)
{
  auto r = latticePoints(/*dim*/ 2, /*B*/ 1, {}, {}, kBigN, kBigNN);
  EXPECT_EQ(r.points.size(), 9u);  // (2*1+1)^2

  // Every lattice point in [-1,1]^2 should appear exactly once.
  std::set<std::vector<int>> expected;
  for (int x = -1; x <= 1; ++x)
    for (int y = -1; y <= 1; ++y) expected.insert({x, y});
  EXPECT_EQ(asSet(r), expected);
}

TEST(LatticePoints, BoxOnly_dim3_B2)
{
  auto r = latticePoints(3, 2, {}, {}, kBigN, kBigNN);
  EXPECT_EQ(r.points.size(), 125u);  // (2*2+1)^3
}

TEST(LatticePoints, BoxOnly_dim1_B0)
{
  // Degenerate: only the origin.
  auto r = latticePoints(1, 0, {}, {}, kBigN, kBigNN);
  ASSERT_EQ(r.points.size(), 1u);
  EXPECT_EQ(r.points[0], std::vector<int>{0});
}

// ---------------------------------------------------------------------------
// One-hyperplane half-spaces and small simplices.
// ---------------------------------------------------------------------------

TEST(LatticePoints, HalfSpace_xPlusY_geq_0)
{
  // x + y >= 0 inside [-2,2]^2: count 15 by hand.
  std::vector<std::vector<int>> H = {{1, 1}};
  std::vector<int> rhs = {0};
  auto r = latticePoints(2, 2, H, rhs, kBigN, kBigNN);
  EXPECT_EQ(r.points.size(), 15u);
  EXPECT_EQ(asSet(r), bruteForce(2, 2, H, rhs));
}

TEST(LatticePoints, Simplex_dim3_B3)
{
  // Standard simplex {x_i >= 0, x_1+x_2+x_3 <= 3}: count = C(6,3) = 20.
  // In H*v >= rhs form: x_i >= 0 and -x_1-x_2-x_3 >= -3.
  std::vector<std::vector<int>> H = {
      {1, 0, 0}, {0, 1, 0}, {0, 0, 1}, {-1, -1, -1}};
  std::vector<int> rhs = {0, 0, 0, -3};
  auto r = latticePoints(3, 3, H, rhs, kBigN, kBigNN);
  EXPECT_EQ(r.points.size(), 20u);
  EXPECT_EQ(asSet(r), bruteForce(3, 3, H, rhs));
}

TEST(LatticePoints, MixedSignsAndRhs)
{
  // 2x - y >= 3 and -x + 2y >= -1 inside [-3,3]^2. No magic count -- just
  // check the helper agrees with brute force. This pins down sign/rhs
  // handling in set_bounds without depending on a hand-computed answer.
  std::vector<std::vector<int>> H = {{2, -1}, {-1, 2}};
  std::vector<int> rhs = {3, -1};
  auto r = latticePoints(2, 3, H, rhs, kBigN, kBigNN);
  EXPECT_EQ(asSet(r), bruteForce(2, 3, H, rhs));
}

TEST(LatticePoints, Infeasible_xGeq1_AndXLeqMinus1)
{
  // x >= 1 AND x <= -1: empty intersection.
  std::vector<std::vector<int>> H = {{1}, {-1}};
  std::vector<int> rhs = {1, 1};
  auto r = latticePoints(1, 2, H, rhs, kBigN, kBigNN);
  EXPECT_EQ(r.points.size(), 0u);
  // TODO: assert r.n_nodes >= 1 here once box_enum.h is fixed upstream.
  // Currently when set_bounds returns 0 at the root, _box_enum_c jumps
  // straight to its end: label without ever executing `*N_nodes = 1`,
  // so n_nodes leaks the caller's initial value (0). Issue reported
  // to the author; one-line fix is to initialize *N_nodes at the top.
}

// ---------------------------------------------------------------------------
// Cap behavior:
//   max_N_out is a soft cap (return partial, no throw)
//   max_N_nodes is a hard cap (throw runtime_error)
// ---------------------------------------------------------------------------

TEST(LatticePoints, SoftCap_MaxNOut_ReturnsPartial)
{
  // No hyperplanes: full box has 441 points; ask for at most 10.
  const long cap = 10;
  auto r = latticePoints(2, 10, {}, {}, cap, kBigNN);
  EXPECT_EQ(r.points.size(), static_cast<size_t>(cap));
  // No exception thrown -- if we got here, the soft-cap behavior is intact.
}

TEST(LatticePoints, SoftCap_MaxNOut_ExactlyAtBound)
{
  // Exactly 9 points in [-1,1]^2; cap=9 should not trigger truncation.
  auto r = latticePoints(2, 1, {}, {}, /*max_N_out*/ 9, kBigNN);
  EXPECT_EQ(r.points.size(), 9u);
}

TEST(LatticePoints, HardCap_MaxNNodes_Throws)
{
  // Tiny node budget on a problem that expands the search tree past it.
  EXPECT_THROW(
      latticePoints(2, 10, {}, {}, kBigN, /*max_N_nodes*/ 5),
      std::runtime_error);
}

TEST(LatticePoints, DimTooLarge_Throws)
{
  // box_enum's MAX_SUPPORTED_DIM is 256.
  EXPECT_THROW(
      latticePoints(/*dim*/ 257, /*B*/ 0, {}, {}, kBigN, kBigNN),
      std::runtime_error);
}

// ---------------------------------------------------------------------------
// Input shape errors (caught in the C++ wrapper, before _box_enum_c).
// ---------------------------------------------------------------------------

TEST(LatticePoints, RhsLengthMismatch_Throws)
{
  std::vector<std::vector<int>> H = {{1, 0}, {0, 1}};
  std::vector<int> rhs = {0};  // length 1, should be 2
  EXPECT_THROW(
      latticePoints(2, 1, H, rhs, kBigN, kBigNN),
      std::runtime_error);
}

TEST(LatticePoints, HRowLengthMismatch_Throws)
{
  std::vector<std::vector<int>> H = {{1, 0, 0}};  // row length 3, dim is 2
  std::vector<int> rhs = {0};
  EXPECT_THROW(
      latticePoints(2, 1, H, rhs, kBigN, kBigNN),
      std::runtime_error);
}
