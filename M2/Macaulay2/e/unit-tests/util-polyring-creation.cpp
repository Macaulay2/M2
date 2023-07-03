#include "util-polyring-creation.hpp"

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
                        names,
                        IM2_Ring_trivial_polyring()->cast_to_PolynomialRing(),
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
                             names,
                             degreeRing(1),
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

  const Ring *kk = (p > 0 ? rawARingZZpFlint(p) : rawARingQQFlint());
  if (kk == nullptr) return nullptr; // one of these routines would have made an error.

  MonomialOrdering* monorder = MonomialOrderings::join
    ({
      MonomialOrderings::GRevLex(names.size()),
      MonomialOrderings::PositionUp()
    });

  return simplePolynomialRing(kk, names, monorder);
}


// Local Variables:
// indent-tabs-mode: nil
// End:
