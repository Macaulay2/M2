// Copyright 2026, The Macaulay2 Authors.

#include "util.hpp"
#include "unit-tests/util-polyring-creation.hpp"
#include "rings/weylalg.hpp"
#include "interface/ring.h"
#include "interface/aring.h"
#include "BasicPoly.hpp"
#include "BasicPolyList.hpp"
#include "monoid.hpp"
#include "free-modules/freemod.hpp"
#include "interface/groebner.h"
#include "groebner-computations/comp-gb.hpp"

// Creation of ring type objects
// coefficient rings
// degree ring (not degree monoid)
// monoid
// polynomialRing
// weylAlgebra
// exteriorAlgebra
// associativeAlgebra
// groebnerAlgebra


const Monoid* degreeMonoid(const std::vector<std::string>& names)
{
  std::vector<int> wts;
  for (int i=0; i<names.size(); i++)
    wts.push_back(-1);
  MonomialOrdering* mo = MonomialOrderings::join
    ({
      MonomialOrderings::Weights(wts),
      MonomialOrderings::GroupLex(names.size()),
      MonomialOrderings::PositionUp()
    });

  return Monoid::create(mo,
                        IM2_Ring_trivial_polyring()->cast_to_PolynomialRing(),
                        names,
                        {},
                        {});
}

const PolynomialRing* degreeRing(const std::vector<std::string>& names)
{
  auto degM = degreeMonoid(names);
  if (degM == nullptr) return nullptr;
  return PolyRing::create(globalZZ, degM);
}
const PolynomialRing* degreeRing(int ndegrees)
{
  assert(ndegrees == 1);
  return degreeRing({"T"});
}

// Monoids and monomial orderings

const Monoid* simpleMonoid(const std::vector<std::string>& names,
                           MonomialOrdering* monorder,
                           const PolynomialRing* degRing,
                           const std::vector<int>& degs,
                           const std::vector<int>& heft)
{
  // a few checks:
  // (#vars of degrees ring) * #vars == #degs
  // #heft == #gens degreesRing.
  // heft of each degree vector for each vector should be > 0, if heft is non-empty.

  const Monoid* M = Monoid::create(
                             monorder,
                             degRing,
                             names,
                             degs,
                             heft
                             );
}

                           
const PolynomialRing* simplePolynomialRing(const Ring* kk,
                                           const std::vector<std::string>& names,
                                           MonomialOrdering* monorder)
{
  // degrees are all set to 1. (degree ring has one variable)
  // heft is 1.

  // Now create the monomial order.  This one is a pain in the butt!
  std::vector<int> degs;
  for (int i=0; i<names.size(); i++) degs.push_back(1);
  std::vector<int> heft {1};

  const Monoid* M = Monoid::create(
                             monorder,
                             degreeRing(1),
                             names,
                             degs,
                             heft
                             );
  if (M == nullptr) return nullptr; // an error should have been constructed
  return PolyRing::create(kk, M);
}

const PolynomialRing* simplePolynomialRing(int p, const std::vector<std::string>& names)
{
  // if p is 0, use QQ.
  // degrees are all set to 1. (degree ring has one variable)
  // heft is 1.
  // monomial order is grevlex.

  const Ring *kk = (p > 0 ? rawARingZZpFlint(p) : IM2_Ring_QQ());
  if (kk == nullptr) return nullptr; // one of these routines would have made an error.

  MonomialOrdering* monorder = MonomialOrderings::join
    ({
      MonomialOrderings::GRevLex(names.size()),
      MonomialOrderings::PositionUp()
    });

  return simplePolynomialRing(kk, names, monorder);
}

const WeylAlgebra* simpleWeylAlgebra(long p,
                                     const std::vector<std::string> names,
                                     const std::vector<int> comms,
                                     const std::vector<int> derivs)
{
  // if p is 0, use QQ.
  // degrees are all set to 1. (degree ring has one variable)
  // heft is 1.
  // monomial order is grevlex.

  const Ring *kk = (p > 0 ? rawARingZZpFlint(p) : IM2_Ring_QQ());
  if (kk == nullptr) return nullptr; // one of these routines would have made an error.

  MonomialOrdering* monorder = MonomialOrderings::join
    ({
      MonomialOrderings::GRevLex(names.size()),
      MonomialOrderings::PositionUp()
    });

  int n = static_cast<int>(comms.size());
  std::vector<int> degs(2*n, -1);
  std::fill_n(degs.begin(), n, 1);

  const Monoid* M = Monoid::create(
                                   monorder,
                                   degreeRing(1),
                                   names,
                                   degs,
                                   {1});

  M2_arrayint derivs1 = stdvector_to_M2_arrayint(derivs);
  M2_arrayint comms1 = stdvector_to_M2_arrayint(comms);
  auto W = WeylAlgebra::create(kk,
                          M,
                          derivs1,
                          comms1,
                          -1);
  
  return W;
}


const Matrix* idealFromStrings(const PolynomialRing* R,
                               const std::vector<std::string>& polys)
{
  auto varnames = R->getMonoid()->variableNames();
  BasicPolyList bpList;
  for (const auto& s : polys)
    bpList.push_back(parseBasicPoly(s, varnames));
  return toMatrix(R->make_FreeModule(1), bpList);
}

const Matrix* computeGB(const Matrix* M)
{
  M2_arrayint weights = stdvector_to_M2_arrayint(std::vector<int>{});
  Computation* C = IM2_GB_make(M,
                               false,  // collect_syz
                               0,      // n_rows_to_keep
                               weights,
                               false,  // use_max_degree
                               0,      // max_degree
                               0,      // algorithm (default)
                               0,      // strategy (default)
                               10);    // max_reduction_count (engine default)
  if (C == nullptr) return nullptr;
  rawStartComputation(C);
  return rawGBGetMatrix(C);
}

const Ring* simpleQuotientRing(const PolynomialRing* R,
                               const std::vector<std::string>& generators)
{
  const Matrix* I = idealFromStrings(R, generators);
  if (I == nullptr) return nullptr;
  const Matrix* gb = computeGB(I);
  if (gb == nullptr) return nullptr;
  return PolynomialRing::create_quotient(R, gb);
}

// Local Variables:
// indent-tabs-mode: nil
// End:
