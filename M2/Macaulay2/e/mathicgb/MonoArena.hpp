// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MONO_ARENA_GUARD
#define MATHICGB_MONO_ARENA_GUARD

#include "Range.hpp"
#include <algorithm>
#include <vector>

MATHICGB_NAMESPACE_BEGIN

/// Like a Monoid::MonoVector, except that references are not
/// invalidated when additional memory must be allocated.
///
/// Todo: Think of a better name. We can't deallocate at the end, so
/// MonoArena is not really an Arena.
template<class Monoid>
class MonoArena;

template<class M>
class MonoArena {
public:
  typedef M Monoid;
  typedef typename Monoid::Mono Mono;
  typedef typename Monoid::MonoRef MonoRef;
  typedef typename Monoid::ConstMonoRef ConstMonoRef;
  typedef typename Monoid::MonoPtr MonoPtr;
  typedef typename Monoid::ConstMonoPtr ConstMonoPtr;
  typedef typename Monoid::MonoVector MonoVector;

  typedef Flatten<typename std::vector<MonoVector>::const_iterator> const_iterator;

  // *** Constructors and assignment

  MonoArena(const Monoid& monoid) {
    mVectors.emplace_back(monoid);
  }

  MonoArena(const MonoArena& a): mVectors(a.mVectors) {}
  MonoArena(MonoArena&& a): mVectors(std::move(a.mVectors)) {}

  MonoArena& operator=(const MonoArena& a) {
    MATHICGB_ASSERT(monoid() == a.monoid());
    mVectors = a.mVectors;
    return *this;
  }

  MonoArena& operator=(MonoArena&& a) {
    MATHICGB_ASSERT(monoid() == a.monoid());
    mVectors = std::move(a.mVectors);
    return *this;      
  }


  // *** Iterators

  const_iterator begin() const {
    return makeFlatten(std::begin(mVectors), std::end(mVectors));
  }

  const_iterator end() const {
    return makeFlatten(std::end(mVectors), std::end(mVectors));
  }

  const_iterator cbegin() const {return begin();}
  const_iterator cend() const {return end();}


  // *** Size and capacity

  size_t size() const {
    auto count = size_t(0);
    for (const auto& v : mVectors)
      count += v.size();
    return count;
  }

  bool empty() const {
    MATHICGB_ASSERT(!mVectors.empty());
    return mVectors.front().empty();
  }


  // *** Element access

  ConstMonoRef front() const {
    MATHICGB_ASSERT(!empty());
    MATHICGB_ASSERT(!mVectors.front().empty());
    return mVectors.front().front();
  }

  MonoRef back() {
    MATHICGB_ASSERT(!empty());
    MATHICGB_ASSERT(!backVector().empty());
    return backVector().back();
  }

  ConstMonoRef back() const {
    MATHICGB_ASSERT(!empty());
    MATHICGB_ASSERT(!backVector().empty());
    return backVector().back();
  }


  // *** Modifiers

  /// Appends the identity.
  void push_back() {
    MATHICGB_ASSERT(debugAssertValid());

    ensureSpace();
    return;
    backVector().push_back();

    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(monoid().isIdentity(back()));
  }

  void push_back(ConstMonoRef mono) {
    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(monoid().debugValid(mono));

    ensureSpace();
    backVector().push_back(mono);

    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(monoid().equal(back(), mono));
  }

  template<class Monoid>
  void push_back(
    const Monoid& monoidMono,
    typename Monoid::ConstMonoRef mono
  ) {
    MATHICGB_ASSERT(monoidMono.debugValid(mono));
    MATHICGB_ASSERT(debugAssertValid());

    ensureSpace();
    backVector().push_back(monoidMono, mono);

    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(monoid().equal(monoidMono, mono, back()));
  }

  void swap(MonoArena& a) {
    MATHICGB_ASSERT(monoid() == a.monoid());
    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(a.debugAssertValid());

    mVectors.swap(a.mVectors);

    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(a.debugAssertValid());
  }

  void clear() {
    MATHICGB_ASSERT(debugAssertValid());

    auto last = std::end(mVectors);
    --last;
    last->clear();
    mVectors.erase(mVectors.begin(), last);

    MATHICGB_ASSERT(debugAssertValid());
  }


  // *** Other

  size_t memoryBytesUsed() const {
    auto sum = size_t(0);
    for (const auto& v : mVectors)
      sum += v.memoryBytesUsed();
    return sum;
  }

  const Monoid& monoid() const {return mVectors.back().monoid();}

  bool debugAssertValid() const {
#ifdef MATHICGB_DEBUG
    MATHICGB_ASSERT(!mVectors.empty());
    for (const auto& p : adjPairRange(mVectors)) {
      MATHICGB_ASSERT(p.first.monoid() == p.second.monoid());
      MATHICGB_ASSERT(p.first.capacity() <= p.second.capacity());
    }
#endif
    return true;
  }

private:
  /// Ensures that there is space for at least one more monomial.
  ///
  /// @todo: if monomials ever become variable-size, then this might
  /// allocate far more than double the memory of the previous
  /// vector. The problem is that they might actually take up, say, 128 bytes
  /// but the worst case is 1024k. Then if 1000 monomials fit in the previous
  /// vector, the next vector will allocate 2 * 1000 * 1024k bytes, which
  /// is far more than double what the previous vector did. Make it actually
  /// double the bytes.
  void ensureSpace() {
    MATHICGB_ASSERT(debugAssertValid());

    if (!backVector().atCapacity())
      return;

    const auto oldSize = backVector().size();
    if (oldSize > std::numeric_limits<decltype(oldSize)>::max() / 2)
      throw std::bad_alloc();
    const auto newSize = 2 * oldSize;

    mVectors.emplace_back(monoid());
    backVector().reserve(newSize);

    MATHICGB_ASSERT(debugAssertValid());
  }

  MonoVector& backVector() {
    MATHICGB_ASSERT(!mVectors.empty());
    return mVectors.back();
  }

  const MonoVector& backVector() const {
    MATHICGB_ASSERT(!mVectors.empty());
    return mVectors.back();
  }

  std::vector<MonoVector> mVectors;
};

/// Returns true if a and b contain the same monomials in the same order.
template<class Monoid>
bool operator==(const MonoArena<Monoid>& a, const MonoArena<Monoid>& b) {
  MATHICGB_ASSERT(a.monoid() == b.monoid());
  MATHICGB_ASSERT(a.debugAssertValid());
  MATHICGB_ASSERT(b.debugAssertValid());
  typedef typename Monoid::ConstMonoRef ConstMonoRef;

  const auto& monoid = a.monoid();
  auto cmpEqual = [&monoid](ConstMonoRef monoA, ConstMonoRef monoB) {
    return monoid.equal(monoA, monoB);
  };
  return std::equal(std::begin(a), std::end(a), std::begin(b), cmpEqual);
}

/// As !(*this == v).
template<class Monoid>
bool operator!=(const MonoArena<Monoid>& a, const MonoArena<Monoid>& b) {
  MATHICGB_ASSERT(a.monoid() == b.monoid());
  return !(a == b);
}

MATHICGB_NAMESPACE_END
#endif
