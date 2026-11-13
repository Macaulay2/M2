#include "cytools/lattice-points-normaliz.hpp"

#include <libnormaliz/cone.h>
#include <libnormaliz/cone_property.h>
#include <libnormaliz/matrix.h>
#include <libnormaliz/normaliz_exception.h>

#include <stdexcept>
#include <string>

namespace M2::cytools {

LatticePointsNormalizResult latticePointsNormaliz(
    int dim,
    const std::vector<std::vector<mpz_class>>& A,
    const std::vector<mpz_class>& b)
{
  const size_t m = A.size();
  if (m != b.size())
    throw std::runtime_error(
        "latticePointsNormaliz: A.size() must equal b.size()");
  for (const auto& row : A)
    if (static_cast<int>(row.size()) != dim)
      throw std::runtime_error(
          "latticePointsNormaliz: every row of A must have length dim");

  // Normaliz Type::inhom_inequalities row [a | c] represents a.x + c >= 0.
  // For Ax <= b that is -A.x + b >= 0, so each row is [-A_i | b_i].
  libnormaliz::Matrix<mpz_class> ineqs(m, dim + 1);
  for (size_t i = 0; i < m; ++i)
    {
      for (int j = 0; j < dim; ++j) ineqs[i][j] = -A[i][j];
      ineqs[i][dim] = b[i];
    }

  try
    {
      libnormaliz::Cone<mpz_class> cone(
          libnormaliz::Type::inhom_inequalities, ineqs);
      cone.setVerbose(false);
      cone.compute(libnormaliz::ConeProperty::LatticePoints);

      // Reject unbounded polyhedra explicitly: libnormaliz doesn't throw
      // for inhom_inequalities with a nontrivial recession cone --- it
      // silently returns just the polytope's vertex lattice points,
      // which is misleading.
      if (cone.getRecessionRank() > 0)
        throw std::runtime_error(
            "latticePointsNormaliz: polyhedron is unbounded "
            "(recession rank > 0)");

      const auto& lps = cone.getLatticePoints();
      LatticePointsNormalizResult result;
      result.points.reserve(lps.size());
      for (const auto& row : lps)
        {
          // For inhom_inequalities, libnormaliz returns the n affine
          // coordinates of each polytope lattice point. Be defensive in
          // case a homogenizing 1 is appended: take the first `dim`.
          if (static_cast<int>(row.size()) < dim)
            throw std::runtime_error(
                "latticePointsNormaliz: unexpected row width from libnormaliz");
          result.points.emplace_back(row.begin(), row.begin() + dim);
        }
      return result;
  } catch (const libnormaliz::NotComputableException& e)
    {
      throw std::runtime_error(
          std::string("latticePointsNormaliz: not computable, likely an "
                      "unbounded polyhedron (")
          + e.what() + ")");
  } catch (const libnormaliz::BadInputException& e)
    {
      throw std::runtime_error(
          std::string("latticePointsNormaliz: bad input: ") + e.what());
  } catch (const libnormaliz::NormalizException& e)
    {
      throw std::runtime_error(
          std::string("latticePointsNormaliz: libnormaliz error: ") + e.what());
  }
}

} // namespace M2::cytools
