// Written in 2021 by Mahrud Sayrafi

#include "interface/cone.h"

#include <M2/math-include.h>

#include "debug.hpp"
#include "interface/gmp-util.h"
#include "interface/matrix.h"
#include "matrices/matrix-con.hpp"
#include "matrices/matrix.hpp"
#include "ring-elements/ring-element.hpp"
#include "matrices/matrix-con.hpp"
#include "matrices/matrix.hpp"
#include "interface/mutable-matrix.h"
#include "mutable-matrices/mutablemat.hpp"
#include "util.hpp"

#include "cytools/lattice_points.hpp"
#include "cytools/cone-interior-point.hpp"
#include "interface/ring.h"
#include <libnormaliz/cone.h>
#include <stdexcept>
#include <vector>
typedef mpz_class Integer;

/**
 * \ingroup cones
 */

const Matrix /* or null */ *rawFourierMotzkin(const Matrix *C)
{
  try
    {
      // TODO: generalize the input type, in particular to allow lineality space
      const Ring *R = C->get_ring();
      const size_t c = C->n_cols();  // rank of ambient lattice
      const size_t r = C->n_rows();  // number of cone inequalities

      auto ineqs = libnormaliz::Matrix<Integer>(r, c);
      for (size_t i = 0; i < r; i++)
        for (size_t j = 0; j < c; j++)
	  // libnormaliz uses A*x >= 0, Macaulay2 uses A*x <= 0
          ineqs[i][j] = (-1) * static_cast<Integer>(C->elem(i, j).get_mpz());

      auto cone = libnormaliz::Cone<Integer>(libnormaliz::Type::inequalities, ineqs);
      auto rays = cone.getExtremeRays();
      size_t n = rays.size();  // number of extremal rays

      MatrixConstructor mat(R->make_FreeModule(n), c);
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < c; j++)
          {
            mpz_ptr z = newitem(__mpz_struct);
            mpz_init_set(z, rays[i][j].get_mpz_t());
            mpz_reallocate_limbs(z);
            mat.set_entry(i, j, ring_elem(z));
          }

      return mat.to_matrix();
  } catch (const exc::engine_error &e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const Matrix /* or null */ *rawHilbertBasis(const Matrix *C)
{
  try
    {
      // TODO: Check that C is over ZZ
      // TODO: for cones over QQ, lift to ZZ first
      // TODO: Normaliz also supports algebraic cones defined over
      // algebraic number fields embedded in RR
      const Ring *R = C->get_ring();
      const size_t c = C->n_cols();  // rank of ambient lattice
      const size_t r = C->n_rows();  // number of cone rays

      auto rays = libnormaliz::Matrix<Integer>(r, c);
      for (size_t i = 0; i < r; i++)
        for (size_t j = 0; j < c; j++)
          rays[i][j] = static_cast<Integer>(C->elem(i, j).get_mpz());

      auto cone = libnormaliz::Cone<Integer>(libnormaliz::Type::cone, rays);
      // cone.compute(libnormaliz::ConeProperty::HilbertBasis,
      //              libnormaliz::ConeProperty::DefaultMode);
      auto HB = cone.getHilbertBasis();
      size_t n = HB.size();  // number of basis elements

      MatrixConstructor mat(R->make_FreeModule(n), c);
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < c; j++)
          {
            mpz_ptr z = newitem(__mpz_struct);
            mpz_init_set(z, HB[i][j].get_mpz_t());
            mpz_reallocate_limbs(z);
            mat.set_entry(i, j, ring_elem(z));
          }

      return mat.to_matrix();
  } catch (const exc::engine_error &e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

// Keep this in sync with the typedef in Macaulay2/e/computeGV.hpp.
using CurveAndGVCollection =
    std::vector<std::pair<std::vector<int>, mpz_class>>;

// The following is in the file e/computeGV.{hpp,cpp}
extern int gvcompute(
    std::vector<std::vector<int>> input_curves,
    std::vector<std::vector<int>> lightcone_curves,
    std::vector<int> grading_vec,
    std::vector<std::vector<int>> Q,  // GLSM charge matrix
    std::vector<std::vector<int>> nef_partition,
    std::vector<std::vector<int>> intnums_list,  // intersection numbers
                     std::vector<int> input_settings,
    CurveAndGVCollection& result); // computation settings

auto decodeArrayArrayInt(M2_arrayint a) -> std::vector<std::vector<int>>
{
  // throw an error if the 
  auto nelems = a->array[0];
  std::vector<std::vector<int>> result;
  int next = 1;
  for (int i = 0; i < nelems; ++i)
    {
      result.emplace_back();
      int len = a->array[next];
      for (int j = 1; j <= len; ++j) result[i].push_back(a->array[next + j]);
      next += len + 1;
    }
  return result;
}

MutableMatrix *rawGVInvariants(M2_arrayint a,
                              M2_arrayint b,
                              M2_arrayint c,
                              M2_arrayint d, // missing e as that cannot be used in d-file...
                              M2_arrayint f,
                              M2_arrayint g,
                              M2_arrayint h)
{
  std::vector<std::vector<int>> input_curves {decodeArrayArrayInt(a)};
  std::vector<std::vector<int>> lightcone_curves {decodeArrayArrayInt(b)};
  std::vector<int> grading_vec { M2_arrayint_to_stdvector<int>(c) }; // c
  std::vector<std::vector<int>> Q {decodeArrayArrayInt(d)};  // GLSM charge matrix
  std::vector<std::vector<int>> nef_partition {decodeArrayArrayInt(f)};
  std::vector<std::vector<int>> intnums_list {decodeArrayArrayInt(g)};  // intersection numbers
  std::vector<int> input_settings {M2_arrayint_to_stdvector<int>(h)}; // {h};             // computation settings

  int h11 = input_curves[0].size();
  //  MatrixConstructor resultCurvesAndGVs(globalZZ->make_FreeModule(h11+1));

  CurveAndGVCollection curveandgvcollection;

  
  gvcompute(input_curves,
            lightcone_curves,
            grading_vec,
            Q,
            nef_partition,
            intnums_list,
            input_settings,
            curveandgvcollection);

  MutableMatrix *M =
    MutableMatrix::zero_matrix(globalZZ, h11 + 1, curveandgvcollection.size(), true);

  int col = -1;
  for (auto &k : curveandgvcollection)
    {
      col++;
      for (int r = 0; r < h11; ++r)
        {
          M->set_entry(r, col, globalZZ->from_long(k.first[r]));
        }
      M->set_entry(h11, col, globalZZ->from_int(k.second.get_mpz_t()));
    }
  return M;
  
  
  // std::cout << "# curves: " << curveandgvcollection.size() << std::endl;
  // for (auto &k : curveandgvcollection)
  //   {
  //     for (auto &a : k.first) { std::cout << a << " "; }
  //     gmp_printf("%Zd\n", &k.second);
  //   }
  // return nullptr;
//  return resultCurvesAndGVs.to_matrix();
}
// todo here:
// 1. add in translate functions (make the function...)
// 2. create the top level function, call the d level function.
// 3. deal with the output from gvcompute.

// User-facing convention is A * x <= b. The underlying box_enum function
// works with H * x >= rhs, so we negate A and b on the way in.
MutableMatrix *rawLatticePoints(const Matrix *A,
                                const Matrix *b,
                                int B,
                                long max_N_out,
                                long max_N_nodes)
{
  try
    {
      const size_t n_hyps = A->n_rows();
      const size_t dim = A->n_cols();

      if (b->get_ring() != globalZZ)
        {
          ERROR("rawLatticePoints: b must be a matrix over ZZ");
          return nullptr;
        }
      if (static_cast<size_t>(b->n_rows()) != n_hyps || b->n_cols() != 1)
        {
          ERROR("rawLatticePoints: b must be a column matrix with n_rows(A) rows");
          return nullptr;
        }

      // Marshal A entries to vector<vector<int>>, negating to convert
      // A*x <= b into (-A)*x >= -b for the box_enum convention.
      std::vector<std::vector<int>> Hvec(n_hyps, std::vector<int>(dim));
      for (size_t i = 0; i < n_hyps; i++)
        for (size_t j = 0; j < dim; j++)
          {
            mpz_srcptr z = A->elem(i, j).get_mpz();
            if (mpz_fits_sint_p(z) == 0)
              {
                ERROR("rawLatticePoints: entry of A does not fit in a C int");
                return nullptr;
              }
            Hvec[i][j] = -static_cast<int>(mpz_get_si(z));
          }

      std::vector<int> rhsvec(n_hyps);
      for (size_t i = 0; i < n_hyps; i++)
        {
          mpz_srcptr z = b->elem(i, 0).get_mpz();
          if (mpz_fits_sint_p(z) == 0)
            {
              ERROR("rawLatticePoints: entry of b does not fit in a C int");
              return nullptr;
            }
          rhsvec[i] = -static_cast<int>(mpz_get_si(z));
        }

      auto result = M2::cytools::latticePoints(
          static_cast<int>(dim), B, Hvec, rhsvec, max_N_out, max_N_nodes);

      // Output: rows = ambient coords (dim), cols = lattice points.
      const size_t n_points = result.points.size();
      MutableMatrix *M =
          MutableMatrix::zero_matrix(globalZZ, dim, n_points, /*dense*/ true);
      for (size_t i = 0; i < n_points; i++)
        for (size_t j = 0; j < dim; j++)
          M->set_entry(j, i, globalZZ->from_long(result.points[i][j]));
      return M;
  } catch (const std::runtime_error &e)
    {
      // catches both std::runtime_error from latticePoints() and
      // exc::engine_error (which derives from std::runtime_error)
      ERROR(e.what());
      return nullptr;
  }
}

// Macaulay2 convention: rows of A are inequalities A*x <= 0.
// The helper coneInteriorPoint() works with {x : A x >= 0}, so we negate.
//
// Return value is a 1-row MutableMatrix over RR(53):
//   if full-dimensional: 2 + n columns, [1, tStar, interior point]
//   else                : 2 + m columns, [0, tStar, dual certificate]
MutableMatrix /* or null */ *rawConeInteriorPoint(const Matrix *A)
{
  try
    {
      const size_t m = A->n_rows();  // number of inequalities
      const size_t n = A->n_cols();  // ambient dimension

      std::vector<int> Avec(m * n);
      for (size_t i = 0; i < m; i++)
        for (size_t j = 0; j < n; j++)
          {
            mpz_srcptr z = A->elem(i, j).get_mpz();
            if (mpz_fits_sint_p(z) == 0)
              {
                ERROR("rawConeInteriorPoint: entry of A does not fit in a C int");
                return nullptr;
              }
            Avec[i * n + j] = -static_cast<int>(mpz_get_si(z));
          }

      ConeResult cr = coneInteriorPoint(static_cast<int>(m),
                                        static_cast<int>(n),
                                        Avec);

      const Ring *RR = IM2_Ring_RRR(53);
      const std::vector<double> &tail =
          cr.fullDimensional ? cr.interiorPoint : cr.dualCert;
      MutableMatrix *M = MutableMatrix::zero_matrix(
          RR, 1, 2 + tail.size(), /*dense*/ true);

      ring_elem r;
      RR->from_double(cr.fullDimensional ? 1.0 : 0.0, r);
      M->set_entry(0, 0, r);
      RR->from_double(cr.tStar, r);
      M->set_entry(0, 1, r);
      for (size_t k = 0; k < tail.size(); k++)
        {
          RR->from_double(tail[k], r);
          M->set_entry(0, 2 + k, r);
        }

      return M;
  } catch (const exc::engine_error &e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

// struct ConeResult {
//     bool fullDimensional;
//     int rank;                        // dimension of the cone
//     std::vector<mpz_class> interior; // valid if fullDimensional: lattice point with Ax > 0
// };

// // A is m x n, row-major: cone is { x : A x >= 0 }.
// ConeResult coneInteriorPointNormaliz(int m, int n, const std::vector<mpz_class>& A) {
//     // Build the m x n matrix of inequality rows.
//     std::vector<std::vector<mpz_class>> ineqs(m, std::vector<mpz_class>(n));
//     for (int i = 0; i < m; ++i)
//         for (int j = 0; j < n; ++j)
//             ineqs[i][j] = A[i * n + j];

//     // Construct the cone from inequalities (H-representation).
//     libnormaliz::Cone<mpz_class> C(libnormaliz::Type::inequalities, ineqs);

//     // Ask for rank and (if full-dim) a witness of the interior.
//     C.compute(libnormaliz::ConeProperty::Rank,
//               libnormaliz::ConeProperty::IsPointed,
//               libnormaliz::ConeProperty::Generators);  // extreme rays, used below

//     ConeResult res;
//     res.rank = C.getRank();
//     res.fullDimensional = (res.rank == n);

//     if (res.fullDimensional) {
//         // Sum of extreme rays lies in the relative interior; since the cone
//         // is full-dimensional, that's the interior, and it's a lattice point.
//         const auto& rays = C.getExtremeRays();   // vector<vector<mpz_class>>
//         std::vector<mpz_class> x(n, 0);
//         for (const auto& r : rays)
//             for (int j = 0; j < n; ++j)
//                 x[j] += r[j];
//         res.interior = std::move(x);
//     }
//     return res;
// }
