// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "PolyHashTable.hpp"

#include "mathic/mathic.h"
#include <iostream>
#include <cmath>

MATHICGB_NAMESPACE_BEGIN

PolyHashTable::PolyHashTable(const PolyRing& ring):
  mHashToIndexMask(computeHashMask(1000)),
  mBuckets
    (make_unique_array<Node*>(hashMaskToBucketCount(mHashToIndexMask))),
  mRing(ring),
  mNodes(sizeofNode(ring)
  ),
  mSize()
{
  mMaxSize = static_cast<size_t>(bucketCount() * maxLoadFactor());
  std::fill_n(mBuckets.get(), bucketCount(), nullptr);
}

void PolyHashTable::rehash(const size_t requestedBucketCount) {
  const auto newHashToIndexMask = computeHashMask(requestedBucketCount);
  const auto newBucketCount = hashMaskToBucketCount(newHashToIndexMask);
  auto newBuckets = make_unique_array<Node*>(newBucketCount);
  std::fill_n(newBuckets.get(), newBucketCount, nullptr);

  const auto bucketsEnd = mBuckets.get() + bucketCount();
  for (auto bucket = mBuckets.get(); bucket != bucketsEnd; ++bucket) {
    for (auto node = *bucket; node != 0;) {
      const auto hash = monoid().hash(node->mono());
      const auto newIndex = hashToIndex(hash, newHashToIndexMask);
      const auto next = node->next();
      node->next() = newBuckets[newIndex];
      newBuckets[newIndex] = node;
      node = next;
    }
  }

  mHashToIndexMask = newHashToIndexMask;
  mBuckets = std::move(newBuckets);
  mMaxSize = static_cast<size_t>(bucketCount() * maxLoadFactor());
}

HashValue PolyHashTable::computeHashMask(const size_t requestedBucketCount) {
  // round request up to nearest power of 2.
  size_t pow2 = 1;
  while (pow2 < requestedBucketCount && 2 * pow2 != 0)
    pow2 *= 2;
  MATHICGB_ASSERT(pow2 > 0 && (pow2 & (pow2 - 1)) == 0); // power of two

  // If casting to a hash value overflows, then we get the maximum
  // possible number of buckets based on the range of the hash
  // value type. Only unsigned overflow is defined, so we need
  // to assert that the hash type is unsigned.
  static_assert(!std::numeric_limits<HashValue>::is_signed, "");
  const auto hashToIndexMask = static_cast<HashValue>(pow2 - 1);
  MATHICGB_ASSERT(pow2 == hashMaskToBucketCount(hashToIndexMask));
  return hashToIndexMask;
}

MATHICGB_NAMESPACE_END
