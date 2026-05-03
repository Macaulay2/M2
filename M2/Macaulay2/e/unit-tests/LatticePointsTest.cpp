// Tests for the pure-C++ lattice-point enumerator
// `M2::cytools::latticePoints` (Kannan/box_enum, in cytools/lattice_points.hpp)
// AND for the engine wrapper `rawLatticePoints` (interface/cone.h), which is
// the A*x <= b entry point used by Macaulay2 callers.
//
// Convention reminder: the cytools helper enumerates v with H*v >= rhs
// (componentwise) and |v_i| <= B; the engine wrapper flips signs to expose
// the user-facing A*x <= b convention. The two are exercised in separate
// gtest groups (LatticePoints / LatticePointsRaw).

#include <set>
#include <stdexcept>
#include <vector>

#include <gtest/gtest.h>

#include "cytools/lattice_points.hpp"

#include "ZZp.hpp"
#include "error.h"
#include "interface/cone.h"
#include "mat.hpp"
#include "matrix-con.hpp"
#include "matrix.hpp"
#include "ring.hpp"

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

// ===========================================================================
// Engine-wrapper tests for `rawLatticePoints` (interface/cone.h):
//   - input is two ZZ Matrix*, A (m x dim) and b (m x 1)
//   - output is a MutableMatrix* over ZZ with dim rows, one col per point
//   - convention is A*x <= b (the wrapper does the sign flip internally)
// These exercise the marshaling layer that the pure-C++ tests above bypass:
// Matrix construction, mpz extraction, ring/shape validation, MutableMatrix
// construction with globalZZ.
// ===========================================================================

namespace {

// Build a const Matrix* over globalZZ from a 2D vector of ints.
const Matrix* makeZZ(const std::vector<std::vector<int>>& rows)
{
  const int nrows = static_cast<int>(rows.size());
  const int ncols = nrows > 0 ? static_cast<int>(rows[0].size()) : 0;
  MatrixConstructor mat(globalZZ->make_FreeModule(nrows), ncols);
  for (int i = 0; i < nrows; ++i)
    for (int j = 0; j < ncols; ++j)
      mat.set_entry(i, j, globalZZ->from_long(rows[i][j]));
  return mat.to_matrix();
}

// Read all columns of a MutableMatrix as lattice points (rows = coords).
std::set<std::vector<int>> pointsAsSet(const MutableMatrix* M)
{
  std::set<std::vector<int>> out;
  const size_t dim = M->n_rows();
  const size_t n = M->n_cols();
  for (size_t c = 0; c < n; ++c)
    {
      std::vector<int> p(dim);
      for (size_t r = 0; r < dim; ++r)
        {
          ring_elem v;
          M->get_entry(r, c, v);
          p[r] = static_cast<int>(mpz_get_si(v.get_mpz()));
        }
      out.insert(std::move(p));
    }
  return out;
}

// Brute-force enumeration in A*x <= b form. Delegates to the H*v >= rhs
// brute-forcer above by negating: (A,b) for Ax <= b corresponds to (-A,-b)
// for Hv >= rhs. This keeps a single source-of-truth enumerator and also
// documents the sign-flip the engine wrapper performs internally.
std::set<std::vector<int>> bruteForceAxLeqB(
    int dim, int B,
    const std::vector<std::vector<int>>& A,
    const std::vector<int>& b)
{
  std::vector<std::vector<int>> H(A.size(), std::vector<int>(dim));
  std::vector<int> rhs(b.size());
  for (size_t i = 0; i < A.size(); ++i)
    {
      for (int j = 0; j < dim; ++j) H[i][j] = -A[i][j];
      rhs[i] = -b[i];
    }
  return bruteForce(dim, B, H, rhs);
}

// Read and clear the engine error flag. Returns the message that was set
// (and "" if no error was set).
std::string consumeEngineError()
{
  if (!error()) return "";
  return std::string(error_message());  // also clears the flag
}

}  // namespace

TEST(LatticePointsRaw, Simplex_dim2_B3)
{
  // x >= 0, y >= 0, x+y <= 3 in A*x <= b form:
  //   -x <= 0, -y <= 0, x+y <= 3.  Count = C(5,2) = 10.
  std::vector<std::vector<int>> A = {{-1, 0}, {0, -1}, {1, 1}};
  std::vector<int> b = {0, 0, 3};
  const Matrix* Am = makeZZ(A);
  const Matrix* bm = makeZZ({{0}, {0}, {3}});

  MutableMatrix* M = rawLatticePoints(Am, bm, /*B*/ 3, kBigN, kBigNN);
  ASSERT_NE(M, nullptr);
  EXPECT_EQ(M->n_rows(), 2u);
  EXPECT_EQ(M->n_cols(), 10u);
  EXPECT_EQ(pointsAsSet(M), bruteForceAxLeqB(2, 3, A, b));
}

TEST(LatticePointsRaw, HalfPlane_xPlusY_geq_0)
{
  // x + y >= 0 in [-2,2]^2, written A*x <= b:  -x - y <= 0.  Count = 15.
  std::vector<std::vector<int>> A = {{-1, -1}};
  std::vector<int> b = {0};
  const Matrix* Am = makeZZ(A);
  const Matrix* bm = makeZZ({{0}});

  MutableMatrix* M = rawLatticePoints(Am, bm, /*B*/ 2, kBigN, kBigNN);
  ASSERT_NE(M, nullptr);
  EXPECT_EQ(M->n_rows(), 2u);
  EXPECT_EQ(M->n_cols(), 15u);
  EXPECT_EQ(pointsAsSet(M), bruteForceAxLeqB(2, 2, A, b));
}

TEST(LatticePointsRaw, MatchesSmokeExample)
{
  // Same shape as the M2-side smoke test: x <= 2, y <= 2, x+y >= 0 in [-5,5]^2.
  std::vector<std::vector<int>> A = {{1, 0}, {0, 1}, {-1, -1}};
  std::vector<int> b = {2, 2, 0};
  const Matrix* Am = makeZZ(A);
  const Matrix* bm = makeZZ({{2}, {2}, {0}});

  MutableMatrix* M = rawLatticePoints(Am, bm, /*B*/ 5, kBigN, kBigNN);
  ASSERT_NE(M, nullptr);
  EXPECT_EQ(M->n_cols(), 15u);
  EXPECT_EQ(pointsAsSet(M), bruteForceAxLeqB(2, 5, A, b));
}

TEST(LatticePointsRaw, NonZZ_b_Errors)
{
  // b over a finite-field ring should be rejected with a clear engine error.
  const Matrix* Am = makeZZ({{1, 0}, {0, 1}});

  Ring* Fp = Z_mod::create(101);
  MatrixConstructor bcon(Fp->make_FreeModule(2), 1);
  bcon.set_entry(0, 0, Fp->from_long(2));
  bcon.set_entry(1, 0, Fp->from_long(2));
  const Matrix* bm = bcon.to_matrix();

  MutableMatrix* M = rawLatticePoints(Am, bm, 3, kBigN, kBigNN);
  EXPECT_EQ(M, nullptr);
  std::string msg = consumeEngineError();
  EXPECT_NE(msg.find("over ZZ"), std::string::npos) << "msg was: " << msg;
}

TEST(LatticePointsRaw, WrongShape_b_Errors)
{
  // b given as a 1x2 row matrix instead of 2x1 column matrix.
  const Matrix* Am = makeZZ({{1, 0}, {0, 1}});
  const Matrix* bm_row = makeZZ({{2, 2}});  // 1 row, 2 cols

  MutableMatrix* M = rawLatticePoints(Am, bm_row, 3, kBigN, kBigNN);
  EXPECT_EQ(M, nullptr);
  std::string msg = consumeEngineError();
  EXPECT_NE(msg.find("column matrix"), std::string::npos)
      << "msg was: " << msg;
}

TEST(LatticePointsRaw, BigInt_b_Errors)
{
  // b entry that doesn't fit in a C int.
  const Matrix* Am = makeZZ({{1, 0}, {0, 1}});
  MatrixConstructor bcon(globalZZ->make_FreeModule(2), 1);
  mpz_t big;
  mpz_init(big);
  mpz_ui_pow_ui(big, 2, 40);  // 2^40, well above INT_MAX
  bcon.set_entry(0, 0, globalZZ->from_int(big));
  bcon.set_entry(1, 0, globalZZ->from_long(0));
  mpz_clear(big);
  const Matrix* bm = bcon.to_matrix();

  MutableMatrix* M = rawLatticePoints(Am, bm, 3, kBigN, kBigNN);
  EXPECT_EQ(M, nullptr);
  std::string msg = consumeEngineError();
  EXPECT_NE(msg.find("does not fit"), std::string::npos)
      << "msg was: " << msg;
}
