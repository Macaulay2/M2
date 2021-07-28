// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "PolyBasis.hpp"

#include "Basis.hpp"

MATHICGB_NAMESPACE_BEGIN

PolyBasis::PolyBasis(
  const PolyRing& ring,
  std::unique_ptr<MonoLookup> monoLookup
):
  mRing(ring),
  mMonoLookup(std::move(monoLookup))
{
  MATHICGB_ASSERT(mMonoLookup.get() != 0);
  mMonoLookup->setBasis(*this);
}

PolyBasis::~PolyBasis() {
  EntryIter const stop = mEntries.end();
  for (EntryIter it = mEntries.begin(); it != stop; ++it) {
    if (it->retired)
      continue;
    MATHICGB_ASSERT(it->poly != 0);
    delete it->poly;
  }
}

std::unique_ptr<Basis> PolyBasis::initialIdeal() const {
  std::unique_ptr<Basis> basis(new Basis(mRing));
  size_t const basisSize = size();
  for (size_t gen = 0; gen != basisSize; ++gen) {
    if (!retired(gen) && leadMinimal(gen)) {
      std::unique_ptr<Poly> p(new Poly(mRing));
      p->append(1, leadMono(gen));
      basis->insert(std::move(p));
    }
  }
  basis->sort();
  return basis;
}

void PolyBasis::insert(std::unique_ptr<Poly> poly) {
  MATHICGB_ASSERT(poly.get() != 0);
  MATHICGB_ASSERT(!poly->isZero());
  poly->makeMonic();
  const size_t index = size();
  EntryIter const stop = mEntries.end();
  const auto lead = poly->leadMono();
#ifdef MATHICGB_DEBUG
  // lead monomials must be unique among basis elements
  for (EntryIter it = mEntries.begin(); it != stop; ++it) {
    if (it->retired)
      continue;
    MATHICGB_ASSERT(!monoid().equal(lead, it->poly->leadMono()));
  }
#endif

  // Update information about minimal lead terms.
  const bool leadMinimal = (divisor(lead) == static_cast<size_t>(-1));
  if (leadMinimal) {
    class MultipleOutput : public MonoLookup::EntryOutput {
    public:
      MultipleOutput(EntryCont& entries): mEntries(entries) {}
      virtual bool proceed(size_t index) {
        MATHICGB_ASSERT(index < mEntries.size());
        mEntries[index].leadMinimal = false;
        return true;
      }
    private:
      EntryCont& mEntries;
    };
    MultipleOutput out(mEntries);
    monoLookup().multiples(lead, out);
  }

  mMonoLookup->insert(lead, index);

  mEntries.push_back(Entry());
  Entry& entry = mEntries.back();
  entry.poly = poly.release();
  entry.leadMinimal = leadMinimal;

  MATHICGB_ASSERT(mEntries.back().poly != 0);
}

std::unique_ptr<Poly> PolyBasis::retire(size_t index) {
  MATHICGB_ASSERT(index < size());
  MATHICGB_ASSERT(!retired(index));
  mMonoLookup->remove(leadMono(index));
  std::unique_ptr<Poly> poly(mEntries[index].poly);
  mEntries[index].poly = 0;
  mEntries[index].retired = true;
  return poly;
}

std::unique_ptr<Basis> PolyBasis::toBasisAndRetireAll() {
  auto basis = make_unique<Basis>(ring());
  for (size_t i = 0; i < size(); ++i)
    if (!retired(i))
      basis->insert(retire(i));
  return basis;
}

size_t PolyBasis::divisor(ConstMonoRef mono) const {
  size_t index = monoLookup().divisor(mono);
  MATHICGB_ASSERT((index == static_cast<size_t>(-1)) ==
    (divisorSlow(mono) == static_cast<size_t>(-1)));
  MATHICGB_ASSERT(index == static_cast<size_t>(-1) ||
    monoid().divides(leadMono(index), mono));
  return index;
}

size_t PolyBasis::classicReducer(ConstMonoRef mono) const {
  const auto index = monoLookup().classicReducer(mono);
  MATHICGB_ASSERT((index == static_cast<size_t>(-1)) ==
    (divisorSlow(mono) == static_cast<size_t>(-1)));
  MATHICGB_ASSERT(index == static_cast<size_t>(-1) ||
    monoid().divides(leadMono(index), mono));
  return index;
}

size_t PolyBasis::divisorSlow(ConstMonoRef mono) const {
  const size_t stop = size();
  for (size_t i = 0; i != stop; ++i)
    if (!retired(i) && monoid().dividesWithComponent(leadMono(i), mono))
      return i;
  return static_cast<size_t>(-1);
}

bool PolyBasis::leadMinimalSlow(size_t index) const {
  MATHICGB_ASSERT(index < size());
  MATHICGB_ASSERT(!retired(index));
  const auto lead = leadMono(index);
  EntryCIter const skip = mEntries.begin() + index;
  EntryCIter const stop = mEntries.end();
  for (EntryCIter it = mEntries.begin(); it != stop; ++it) {
    if (it->retired)
      continue;
    const auto itLead = it->poly->leadMono();
    if (monoid().divides(itLead, lead) && it != skip)
      return false;
  }
  return true;
}

size_t PolyBasis::minimalLeadCount() const {
  // todo: use iterators
  size_t minCount = 0;
  const size_t stop = size();
  for (size_t i = 0; i != stop; ++i)
    if (!retired(i) && leadMinimal(i))
      ++minCount;
  return minCount;
}

size_t PolyBasis::maxIndexMinimalLead() const {
  // todo: use iterators
  size_t i = size() - 1;
  for (; i != static_cast<size_t>(-1); --i)
    if (!retired(i) && leadMinimal(i))
      break;
  return i;
}

size_t PolyBasis::monomialCount() const {
  size_t sum = 0;
  EntryCIter const stop = mEntries.end();
  for (EntryCIter it = mEntries.begin(); it != stop; ++it)
    if (!it->retired)
      sum += it->poly->termCount();
  return sum;
}

size_t PolyBasis::getMemoryUse() const {
  size_t sum = mEntries.capacity() * sizeof(mEntries.front());
  EntryCIter const stop = mEntries.end();
  for (EntryCIter it = mEntries.begin(); it != stop; ++it)
    if (!it->retired)
      sum += it->poly->getMemoryUse();
  return sum;
}

PolyBasis::Entry::Entry():
  poly(0),
  leadMinimal(0),
  retired(false),
  usedAsStartCount(0),
  usedAsReducerCount(0),
  possibleReducerCount(0),
  nonSignatureReducerCount(0) {}

MATHICGB_NAMESPACE_END
