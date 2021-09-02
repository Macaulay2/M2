// MathicGB copyright 2013 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_RANGE_GUARD
#define MATHICGB_RANGE_GUARD

#include <limits>
#include <vector>
#include <utility>
#include <type_traits>
#include <iterator>

MATHICGB_NAMESPACE_BEGIN

// Read on, this file contains several free-standing functions that make use
// of the range concept and Range class.

/// An object that combines two iterators into a range suitable for use with
/// C++11's range for. It is most conveniently used with the function range.
/// For example:
///
///  std::vector<int> v;
///  for (int x : range(v.begin(), v.end()) {}
///
/// std::vector already has begin and end members, so range() is not necessary
/// here. range() is useful when a class does not have begin or end members, or
/// if it offers several different ranges that can be iterated through -
/// then the default range can only offer one of those ranges.
template<class Iter>
class Range {
public:
  typedef Iter Iterator;

  Range(Iterator begin, Iterator end): mBegin(begin), mEnd(end) {}
  Range(std::pair<Iterator, Iterator> pair):
    mBegin(pair.first), mEnd(pair.second)
  {}

  Iterator begin() const {return mBegin;}
  Iterator end() const {return mEnd;}

private:
  Iterator mBegin;
  Iterator mEnd;
};

/// Convenience function for constructing a Range object. This function
/// removes the need to explicitly specify the types involved.
template<class Iterator>
Range<Iterator> range(Iterator begin, Iterator end) {
  return Range<Iterator>(begin, end);
}

/// As range(std::begin(r), std::end(r)).
template<class RangeParam>
auto range(RangeParam&& r) -> decltype(range(std::begin(r), std::end(r))) {
  return range(std::begin(r), std::end(r));
}


// *** Zipping ranges together

/// Zip ties two iterators together into a single iterator. The point
/// is to enable the zip() function defined further down in this header.
///
/// Note that equality is defined ONLY by equality of the first iterator!
template<class Iterator1, class Iterator2>
class Zip {
public:
  typedef decltype(*std::declval<Iterator1>()) ValueType1;
  typedef decltype(*std::declval<Iterator2>()) ValueType2;

  /// It would be possible to do something fancy to infer the strongest
  /// iterator category that both sub-iterators can support. Since this
  /// is intended mainly to be used for things like range-for, I did not
  /// implement that since I do not need it.
  typedef std::forward_iterator_tag iterator_category;
  typedef std::pair<ValueType1, ValueType2> value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  Zip() {}
  Zip(Iterator1 it1, Iterator2 it2): mIt1(it1), mIt2(it2) {}

  Zip& operator++() {
    ++mIt1;
    ++mIt2;
    return *this;
  }

  value_type operator*() const {
    // We cannot use std::make_pair here. If .first or .second is a
    // reference, then std::make_pair will remove the reference and then the
    // conversion to value_type will add the reference back in - but now the
    // reference is not what was originally referenced, instead it is a
    // reference to the temporary object returned by std::make_pair, which
    // then promptly goes out of scope, leading to undefined behavior.
    return value_type(*mIt1, *mIt2);
  }

  /// Compares only the first iterator!
  bool operator!=(const Zip& it) const {return mIt1 != it.mIt1;}

  /// Compares only the first iterator!
  bool operator==(const Zip& it) const {return mIt1 == it.mIt1;}

private:
  Iterator1 mIt1;
  Iterator2 mIt2;
};

/// Creates a Zip iterator out of it1 and it2. This is a convenience function
/// that removes the need to specify the iterator types explicitly.
///
/// There would rarely (never?) be a need to call this function directly.
/// Prefer to call zip() directly.
template<class Iterator1, class Iterator2>
auto makeZip(
  const Iterator1& it1,
  const Iterator2& it2
) -> Zip<Iterator1, Iterator2> {
  return Zip<Iterator1, Iterator2>(it1, it2);
}

/// As zip(range(begin1, end1), range(begin2, end2)).
template<class Iterator1, class Iterator2>
auto zip(
  const Iterator1& begin1,
  const Iterator1& end1,
  const Iterator2& begin2,
  const Iterator2& end2
) -> decltype(range(makeZip(begin1, begin2), makeZip(end1, end2))) {
  return range(makeZip(begin1, begin2), makeZip(end1, end2));
}

/// Zips two ranges into a single range of pairs.
///
/// Example:
///   std::vector<std::string> a = {"hello", "world"};
///   std::vector<int> b = {4, 2, 1, 0};
///   for (const auto& p : zip(a, b))
///     std::cout << p.first << p.second << ' ';
///
/// The output will be "hello4 world2 ". If the ranges have different lengths
/// then the length of the range will be the length of the first range. If the
/// first range is longer than the second one, this means you will likely
/// run into undefined behavior. We can't even do an assert for that because
/// it's perfectly reasonable to use giant implicitly represented ranges for
/// the second parameter and if that's a forward iterator, doing std::distance
/// on it might never terminate or take a very long time.
template<class Range1, class Range2>
auto zip(Range1&& range1, Range2&& range2) ->
  decltype(
    zip(
      std::begin(range1),
      std::end(range1),
      std::begin(range2),
      std::end(range2)
    )
  )
{
  return zip(
    std::begin(range1),
    std::end(range1),
    std::begin(range2),
    std::end(range2)
  );
}


// *** Implicitly represented ranges of integers

/// An Iterator that wraps an integer.
template<class Integer>
class IntIterator {
public:
  /// It could perfectly well be a random access iterator, I just do not
  /// need that functionality right now, so I'm not implementing it.
  typedef std::forward_iterator_tag iterator_category;
  typedef Integer value_type;
  typedef ptrdiff_t difference_type;
  typedef value_type* pointer;
  typedef value_type& reference;

  IntIterator(Integer integer): mInteger(integer) {}

  value_type operator*() const {return mInteger;}

  IntIterator& operator++() {
    ++mInteger;
    return *this;
  }

  bool operator==(const IntIterator& it) const {return **this == *it;}
  bool operator!=(const IntIterator& it) const {return **this != *it;}

private:
  Integer mInteger;
};

/// Constructs a half-open interval of integers [begin, end) that is
/// compatible with range-for.
///
/// Example:
///   for (auto x : intRange(5, 10)
///     std::cout << ' ' << x;
/// The output will be " 5 6 7 8 9". See indexRange() for further examples
/// of how this sort of thing can be useful.
template<class Integer>
Range<IntIterator<Integer>> intRange(Integer begin, Integer end) {
  return Range<IntIterator<Integer>>(begin, end);
}

/// As intRange(Integer(0), end).
template<class Integer>
auto intRange(Integer end) -> decltype(intRange<Integer>(0, 0)) {
  return intRange(Integer(0), end);
}

/// As intRange<size_t>(0, max) where max is the maximum representable
/// integer of type Integer as indicated by std::numeric_limits.
inline auto intRange() -> decltype(intRange<size_t>(0, 0)) {
  return intRange<size_t>(0, std::numeric_limits<size_t>::max());
}

/// Adds indices to the elements of the range, specifying the offset of each
/// element in the range. Equivalent to zip(range(begin, end), intRange()).
///
/// Example:
///   const char*[] strs = {"hello", "world", "!"};
///   for (const auto& p : indexRange(strs))
///     std::cout << ' ' << p.first << p.second;
///
/// The output will be " hello0 world1 !2".
template<class Iterator>
auto indexRange(
  const Iterator& begin,
  const Iterator& end
) -> decltype(zip(range(begin, end), intRange())) {
  return zip(range(begin, end), intRange());
}

/// As indexRange(std::begin(r), std::end(r)).
template<class Range>
auto indexRange(
  Range&& r
) -> decltype(indexRange(std::begin(r), std::end(r))) {
  return indexRange(std::begin(r), std::end(r));
}


// *** Conversion from range to container

/// As rangeToVector(range(begin, end)).
template<class Iterator>
auto rangeToVector(
  Iterator begin,
  Iterator end
) -> decltype(
  std::vector<typename std::decay<decltype(*begin)>::type>(begin, end)
) {
  return std::vector<typename std::decay<decltype(*begin)>::type>(begin, end);
}

/// Converts a range into a vector. This is a convenience function.
/// When using this function, it is not necessary to name the type of the
/// vector. It is also not necessary to give a name to the range object.
///
/// Example:
///   const char*[] strs = {"hello", "world", "!"};
///   auto v = rangeToVector(indexRange(strs));
///
/// Without rangeToVector, the last line would have to be replaced by
///
///   auto r = indexRange(strs);
///   std::vector<std::pair<const char*, size_t>> v(r.begin(), r.end());
///
/// The return type is std::vector<T> where T is whatever type is contained
/// in the range, after stripping references and other qualifiers. So if
/// the iterators in the range return const T&, the return type will still
/// be std::vector<T>. More precisely, if the returned type is S, the returned
/// type will be std::vector<std::decay<S>::type>.
template<class RangeParam>
auto rangeToVector(
  RangeParam&& range
) -> decltype(rangeToVector(std::begin(range), std::end(range))) {
  return rangeToVector(std::begin(range), std::end(range));
}


// *** Range of adjacent pairs

/// Wraps an iterator of pairs to be an iterator of opposite pairs, so that
/// if *it == (a, b) then *OppositePairIterator(it) == (b, a).
template<class Iterator>
class OppositePairIterator {
public:
  typedef decltype((*std::declval<Iterator>()).first) InnerValueType1;
  typedef decltype((*std::declval<Iterator>()).second) InnerValueType2;

  typedef typename Iterator::iterator_category iterator_category;
  typedef std::pair<InnerValueType2, InnerValueType1> value_type;
  typedef typename Iterator::difference_type difference_type;
  typedef typename Iterator::pointer pointer;
  typedef typename Iterator::reference reference;

  OppositePairIterator() {}
  OppositePairIterator(const Iterator& it): mIt(it) {}

  OppositePairIterator& operator++() {
    ++mIt;
    return *this;
  }

  value_type operator*() const {
    return value_type((*mIt).second, (*mIt).first);
  }

  bool operator!=(const OppositePairIterator& it) const {return mIt != it.mIt;}
  bool operator==(const OppositePairIterator& it) const {return mIt == it.mIt;}

private:
  Iterator mIt;
};

/// As oppositePairRange(range(begin, end)).
template<class Iterator>
Range<OppositePairIterator<Iterator>> oppositePairRange(
  const Iterator& begin,
  const Iterator& end
) {
  return range<OppositePairIterator<Iterator>>(begin, end);
}

/// Swaps the elements of the pairs in a range of pairs.
///
/// Example:
///   auto print = [](std::pair<int,  int> p) {
///     std::cout << '(' << p.first << ',' << p.second << ')';
///   auto r = zip(intRange(1, 3), intRange(101, 103));
///   for (auto p : r)
///     print(p);
///   std::cout << '\n';
///   for (auto p : oppositePairRange(r))
///     print(p);
///
/// The output will be
///
///   (1,101)(2,102)(3,103)
///   (101,1)(102,2)(103,3)
template<class RangeParam>
auto oppositePairRange(
  RangeParam&& r
) -> decltype(oppositePairRange(std::begin(r), std::end(r))) {
  return oppositePairRange(std::begin(r), std::end(r));
}


/// As adjPairRange(range(begin, end)).
template<class Iterator>
auto adjPairRange(
  const Iterator& begin,
  const Iterator& end
) -> decltype(oppositePairRange(zip(end, end, end, end))) {
  if (begin == end)
    return oppositePairRange(zip(end, end, end, end));
  // This is a bit tricky. What we really want is
  //   zip(begin, end - 1, begin + 1, end)
  // The -1 is bad because we want this to work for forward iterators
  // that do not have operator--(). We cannot just leave out the -1 because
  // zip defines the length of the range according to the first component
  // range. However, if we swap the ranges around to
  //   zip(begin + 1, end, begin, end - 1)
  // then we can replace this by
  //   zip(begin + 1, end, begin, end)
  // because zip does not actually care about the length of the second range.
  // The problem now is that the pairs of elements will be the wrong way
  // around, but oppositePairRange solves that nicely.
  auto pastBegin = begin;
  ++pastBegin;
  return oppositePairRange(zip(pastBegin, end, begin, end));
}

template<class RangeParam>
auto adjPairRange(
  RangeParam&& r
) -> decltype(adjPairRange(std::begin(r), std::end(r))) {
  return adjPairRange(std::begin(r), std::end(r));
}

// *** Flatten range of ranges into a single range

namespace FlattenNamespace {
  template<class OuterIterator>
  struct InnerIteratorType {
    typedef typename std::decay<decltype(std::begin(*OuterIterator()))>::type type;
  };
}

/// Flatten is an iterator that iterates through each range in a range
/// of ranges. The point is to enable the flatten() function defined
/// further down in this header. This iterator is invalidated whenever
/// the outer range or any of the inner ranges invalidate any iterator
/// or change their number of elements.
template<class OuterIterator>
class Flatten {
public:
  // Yes, it would inded make more sense to inline what InnerIteratorType<>
  // does here. That worked fine on gcc 4.7.3. It did not compile on
  // MSVC 2012. I spent a lot of time trying to track down the problem
  // and as far as I can tell, it is a compiler bug. The work-around
  // with InnerIteratorType works and I'm going to leave it at that.
  typedef typename FlattenNamespace::InnerIteratorType<OuterIterator>::type
    InnerIterator;
  typedef typename std::iterator_traits<InnerIterator>::difference_type
    difference_type;
  typedef typename std::iterator_traits<InnerIterator>::value_type value_type;
  typedef typename std::iterator_traits<InnerIterator>::pointer pointer;
  typedef typename std::iterator_traits<InnerIterator>::reference reference;

  // It would be possible to do something fancy here to identify the exact
  // most general iterator category. I have not done that since I have not
  // needed it so far.
  typedef std::forward_iterator_tag iterator_category;

  Flatten() {}
  Flatten(const OuterIterator outerBegin, const OuterIterator outerEnd):
    mOuter(outerBegin),
    mOuterEnd(outerEnd)
  {
    adjustOuter();
    MATHICGB_ASSERT(debugAssertValid());
  }

  Flatten& operator++() {
    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(!atEnd());
    MATHICGB_ASSERT(mInner != std::end(*mOuter));

    ++mInner;
    if (mInner == std::end(*mOuter)) {
      ++mOuter;
      adjustOuter();
    }

    MATHICGB_ASSERT(debugAssertValid());
    return *this;
  }

  decltype(*std::declval<InnerIterator>()) operator*() const {
    MATHICGB_ASSERT(!atEnd());
    return *mInner;
  }

  bool equal(const Flatten& f) const {
    MATHICGB_ASSERT(debugAssertValid());
    MATHICGB_ASSERT(f.debugAssertValid());

    if (atEnd())
      return f.atEnd();
    else if (f.atEnd())
      return false;
    else
      return mInner == f.mInner;
  }

  bool debugAssertValid() const {
    MATHICGB_ASSERT(atEnd() || mInner != std::end(*mOuter));
    return true;
  }

private:
  bool atEnd() const {return mOuter == mOuterEnd;}

  void adjustOuter() {
    for (; !atEnd(); ++mOuter) {
      if (std::begin(*mOuter) != std::end(*mOuter)) {
        mInner = std::begin(*mOuter);
        return;
      }
    }
  }

  InnerIterator mInner;
  OuterIterator mOuter;
  OuterIterator mOuterEnd;
};

template<class OuterIterator>
bool operator==(
  const Flatten<OuterIterator>& a,
  const Flatten<OuterIterator>& b
) {
  return a.equal(b);
}

template<class OuterIterator>
bool operator!=(
  const Flatten<OuterIterator>& a,
  const Flatten<OuterIterator>& b
) {
  return !(a == b);
}

template<class OuterIterator>
Flatten<OuterIterator> makeFlatten(
  OuterIterator outerIterator, 
  OuterIterator outerEnd
) {
  return Flatten<OuterIterator>(outerIterator, outerEnd);
}

/// As flatten(range(begin, end)).
template<class OuterIterator>
Range<Flatten<OuterIterator>> flatten(
  OuterIterator outerBegin,
  OuterIterator outerEnd
) {
  return range(
    makeFlatten(outerBegin, outerEnd),
    makeFlatten(outerEnd, outerEnd)
  );
}

/// Constructs a range of the union of all (inner) ranges in outerRange.
///
/// Example:
///   std::vector<std::list<int>> v(3);
///   v[0].push_back(1);
///   v[2].push_back(2);
///   v[2].push_back(3);
///   for (const auto& i : flatten(v))
///     std::cout << i << ' ';
///
/// The output is "1 2 3 ";
template<class OuterRange>
auto flatten(
  OuterRange&& outerRange
) -> decltype(flatten(std::begin(outerRange), std::end(outerRange))) {
  return flatten(std::begin(outerRange), std::end(outerRange));
}

MATHICGB_NAMESPACE_END
#endif
