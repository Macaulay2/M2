// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "Basis.hpp"

#include "PolyRing.hpp"
#include "Poly.hpp"
#include "MathicIO.hpp"
#include <ostream>
#include <istream>
#include <iostream>
#include <cctype>

MATHICGB_NAMESPACE_BEGIN

void Basis::insert(std::unique_ptr<Poly>&& p) {
  MATHICGB_ASSERT(p.get() != 0);
  MATHICGB_ASSERT(p->termsAreInDescendingOrder());
  mGenerators.push_back(std::move(p));
}

void Basis::sort() {
  const auto& monoid = ring().monoid();
  const auto cmp = [&monoid](
    const std::unique_ptr<Poly>& a,
    const std::unique_ptr<Poly>& b
  ) {
    return monoid.lessThan(a->leadMono(), b->leadMono());
  };
  std::sort(mGenerators.begin(), mGenerators.end(), cmp);
}

MATHICGB_NAMESPACE_END
