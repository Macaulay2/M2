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

MutableMatrix *rawLatticePoints(const Matrix *H,
                                M2_arrayint rhs,
                                int B,
                                long max_N_out,
                                long max_N_nodes)
{
  try
    {
      const size_t n_hyps = H->n_rows();
      const size_t dim = H->n_cols();

      if (static_cast<size_t>(rhs->len) != n_hyps)
        {
          ERROR("rawLatticePoints: length of rhs must equal number of rows of H");
          return nullptr;
        }

      // Marshal H entries from ZZ matrix to vector<vector<int>>.
      std::vector<std::vector<int>> Hvec(n_hyps, std::vector<int>(dim));
      for (size_t i = 0; i < n_hyps; i++)
        for (size_t j = 0; j < dim; j++)
          {
            mpz_srcptr z = H->elem(i, j).get_mpz();
            if (mpz_fits_sint_p(z) == 0)
              {
                ERROR("rawLatticePoints: H entry does not fit in a C int");
                return nullptr;
              }
            Hvec[i][j] = static_cast<int>(mpz_get_si(z));
          }

      std::vector<int> rhsvec = M2_arrayint_to_stdvector<int>(rhs);

      auto result = M2::cytools::latticePoints(
          static_cast<int>(dim), B, Hvec, rhsvec, max_N_out, max_N_nodes);

      const size_t n_points = result.points.size();
      MutableMatrix *M =
          MutableMatrix::zero_matrix(globalZZ, n_points, dim, /*dense*/ true);
      for (size_t i = 0; i < n_points; i++)
        for (size_t j = 0; j < dim; j++)
          M->set_entry(i, j, globalZZ->from_long(result.points[i][j]));
      return M;
  } catch (const std::runtime_error &e)
    {
      // catches both std::runtime_error from latticePoints() and
      // exc::engine_error (which derives from std::runtime_error)
      ERROR(e.what());
      return nullptr;
  }
}
