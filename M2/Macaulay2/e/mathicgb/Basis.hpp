// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_BASIS_GUARD
#define MATHICGB_BASIS_GUARD

#include "Poly.hpp"
#include "PolyRing.hpp"
#include <tuple>
#include <memory>
#include <algorithm>
#include <vector>

MATHICGB_NAMESPACE_BEGIN

class Poly;

/// A collection of polynomials. @todo: replace with just std::vector<Poly>.
// Really: a list of polynomials
// BUT ALSO maybe: includes memory areas for the polynomials?
class Basis {
public:
  Basis(const PolyRing &R) : mRing(R) {}
  Basis(Basis&& basis):
    mRing(basis.ring()), mGenerators(std::move(basis.mGenerators)) {}

  void insert(std::unique_ptr<Poly>&& p);

  const PolyRing& ring() const { return mRing; }

  const PolyRing *getPolyRing() const { return &mRing; }
  const std::vector< std::unique_ptr<Poly>>& viewGenerators() {
    return mGenerators;
  }
  const Poly *getPoly(size_t i) const {
    MATHICGB_ASSERT(i < size());
    return mGenerators[i].get();
  }
  size_t size() const {return mGenerators.size();}
  bool empty() const {return mGenerators.empty();}
  void reserve(size_t size) {mGenerators.reserve(size);}

  void sort();

private:
  Basis(const Basis&); // not available

  const PolyRing& mRing;
  std::vector< std::unique_ptr<Poly>> mGenerators;
};

MATHICGB_NAMESPACE_END
#endif
