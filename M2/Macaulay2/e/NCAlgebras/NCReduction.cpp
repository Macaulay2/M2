#include <memory>
#include <iostream>
#include "stdinc-m2.hpp"
#include "NCGroebner.hpp"

#include <mathic/TourTree.h>
#include <mathic/Geobucket.h>
#include <mathic/Heap.h>

class OurQueueConfiguration
{
public:
  using Entry = int; // think of each Entry as a monomial.
  enum class CompareResult {LT, EQ, GT};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    if (a < b) return CompareResult::LT;
    if (a > b) return CompareResult::GT;
    return CompareResult::EQ;
  }
  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  static const bool supportDeduplication = false;
  bool cmpEqual(CompareResult a) const; // no implementation needed
  Entry deduplicate(Entry a, Entry b) const; // no implementation needed

  static const bool minBucketBinarySearch = false;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
};

class OurQueueConfiguration1
{
public:
  using Entry = int; // think of each Entry as a monomial.
  enum class CompareResult {LT, EQ, GT};
  CompareResult compare(const Entry& a, const Entry& b) const
  {
    if (a < b) return CompareResult::LT;
    if (a > b) return CompareResult::GT;
    return CompareResult::EQ;
  }
  bool cmpLessThan(CompareResult a) const { return a == CompareResult::LT; }

  // Specific for Geobucket
  const size_t minBucketSize = 2;
  const size_t geoBase = 4;
  static const size_t insertFactor = 4;

  static const bool supportDeduplication = true;
  bool cmpEqual(CompareResult a) const { return a == CompareResult::EQ; }
  Entry deduplicate(Entry a, Entry b) const { return a + b + 1000; }

  static const bool minBucketBinarySearch = false;
  static const bool trackFront = true;
  static const bool premerge = false;
  static const bool collectMax = true;

  static const mathic::GeobucketBucketStorage bucketStorage = mathic::GeoStoreSameSizeBuffer;
};

std::unique_ptr<mathic::Geobucket<OurQueueConfiguration>> makeQueue()
{
  OurQueueConfiguration C;
  
  return make_unique<mathic::Geobucket<OurQueueConfiguration>>(C);
}

void tryOutMathicCode()
{
  return;
  std::cout << "trying out mathic code!" << std::endl;

  auto Q = makeQueue();
  Q->push(3);
  Q->push(7);
  Q->push(2);
  Q->push(42);
  Q->push(7);
  while (not Q->empty())
    {
      int a = Q->pop();
      std::cout << "popped: " << a << std::endl;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
