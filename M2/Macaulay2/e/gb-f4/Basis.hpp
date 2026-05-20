#pragma once

/**
 * @file gb-f4/Basis.hpp
 * @brief `newf4::Basis` --- evolving Gröbner basis container with `GBPolyStatus` per element.
 *
 * Declares the `Basis` class meant to hold the current Gröbner
 * basis (plus the original generators) while the refactored F4
 * driver runs. Storage is two parallel containers: a
 * `PolynomialList mPolynomialList` bound to a shared
 * `MonomialHashTable&` and `VectorArithmetic&`, and a
 * `std::vector<GBPolyStatus> mGBStatusList` recording the role
 * of each entry via the `Gen` / `MinGen` / `MinGB` /
 * `NonMinGB` / `Retired` enum (in-source comment flags the
 * statuses as "not a final choice"). Public surface is
 * currently constructor + read-only accessors
 * (`getPolynomialList`, `vectorArithmetic`, `getGBStatusList`);
 * the in-file comment lists the planned API ("wipe out a poly
 * from basis", "add poly... adjust other elements", "test
 * divisibility both ways") that hasn't been written yet.
 *
 * Modern counterpart of the legacy `f4/`-era `gb_array`: the
 * monomial side is keyed through `MonomialHashTable` indices
 * rather than packed-monomial pointers. The Gröbner-basis
 * reporters in `GBF4Interface.hpp` (`get_gb`, `get_mingens`,
 * `get_change`) override the legacy `GBComputation` virtuals
 * but currently `return nullptr` --- once the refactor lands
 * they will read off `GBPolyStatus` from this class to filter
 * the appropriate subset.
 *
 * @see MonomialHashTable.hpp
 * @see PolynomialList.hpp
 * @see GBF4Computation.hpp
 * @see MacaulayMatrix.hpp
 */

#include "MonomialHashTable.hpp"
#include "PolynomialList.hpp"
#include "../VectorArithmetic.hpp"

namespace newf4 {

// not a final choice of statuses
enum class GBPolyStatus { Gen, MinGen, MinGB, NonMinGB, Retired };

inline std::string toString(GBPolyStatus status) {
   switch (status) {
      case GBPolyStatus::Gen: return "Gen";
      case GBPolyStatus::MinGen: return "MinGen";
      case GBPolyStatus::MinGB: return "MinGB";
      case GBPolyStatus::NonMinGB: return "NonMinGB";
      case GBPolyStatus::Retired: return "Retired";
   };
   return "Undefined";
}

// Basis: keeping the Groebner basis (and original generators)
class Basis
{
private:
  std::vector<GBPolyStatus> mGBStatusList;
  PolynomialList mPolynomialList;

public:
  Basis(const VectorArithmetic& VA,
        MonomialHashTable& monHashTable)
    : mPolynomialList(VA, monHashTable)
  {
  }

  ~Basis() = default;

  const PolynomialList& getPolynomialList() const { return mPolynomialList; }
  const VectorArithmetic& vectorArithmetic() const { return mPolynomialList.vectorArithmetic(); }
  const std::vector<GBPolyStatus>& getGBStatusList() const { return mGBStatusList; }

  // What functionality do we need here?
  // wipe out a poly from basis
  // add poly to the basis (and adjust other elements whose lead terms are div by the new poly)
  // test divisibility both ways (monomial divisibility class)

  // accessor functions

};

}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
