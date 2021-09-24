// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_POLY_HASH_TABLE_GUARD
#define MATHICGB_POLY_HASH_TABLE_GUARD

#include "PolyRing.hpp"
#include "Poly.hpp"
#include <utility>
#include "memtailor/memtailor.h"
#include <vector>

MATHICGB_NAMESPACE_BEGIN

class PolyHashTable {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoRef MonoRef;
  typedef coefficient Value;

  class Node {
  public:
    ConstMonoRef mono() const {return *Monoid::toMonoPtr(mMono);}
    MonoRef mono() {return *Monoid::toMonoPtr(mMono);}

    Value& value() {return mValue;}
    const Value& value() const {return mValue;}

  private:
    friend class PolyHashTable;

    Node*& next() {return mNext;}
    Node* next() const {return mNext;}

    Node* mNext;
    Value mValue;
    exponent mMono[1];
  };

  // Construct a hash table with at least requestedBucketCount buckets. There
  // may be more buckets. Currently the number is rounded up to the next power
  // of two.
  PolyHashTable(const PolyRing& ring);

  const PolyRing::Monoid& monoid() const {return mRing.monoid();}

  /// Return how many buckets the hash table has.
  size_t bucketCount() const {
    return hashMaskToBucketCount(mHashToIndexMask);
  }

  /// Return the number of elements (not the number of buckets).
  size_t size() const {return mSize;}

  MATHICGB_INLINE
  std::pair<Node*, bool> insertProduct(ConstMonoRef a, ConstMonoRef b) {
    auto newNode = new (mNodes.alloc()) Node();
    monoid().multiply(a, b, newNode->mono());
    const auto abHash = monoid().hash(newNode->mono());
    auto& bucket = mBuckets[hashToIndex(abHash)];

    for (auto node = bucket; node != nullptr; node = node->next()) {
      if (abHash != monoid().hash(node->mono()))
        continue;
      if (monoid().equal(newNode->mono(), node->mono())) {
        mNodes.free(newNode);
        return std::make_pair(node, false); // found a*b.
      }
    }

    mRing.coefficientSet(newNode->value(), 0);
    newNode->next() = bucket;
    bucket = newNode;
    ++mSize;
    if (mSize >= mMaxSize)
      rehash(bucketCount() * 2);
    return std::make_pair(newNode, true); // inserted mono
  }

  MATHICGB_INLINE
  std::pair<Node*, bool> insertProduct
    (ConstMonoRef a, ConstMonoRef b, Value add)
  {
    auto p = insertProduct(a, b);
    mRing.coefficientAddTo(p.first->value(), add);
    return p;
  }

  MATHICGB_INLINE
  std::pair<Node*, bool> insertProduct(NewConstTerm a, NewConstTerm b)
  {
    Value prod;
    mRing.coefficientMult(a.coef, b.coef, prod);
    return insertProduct(*a.mono, *b.mono, prod);
  }

  MATHICGB_INLINE
  void remove(Node* nodeToRemove) {
    MATHICGB_ASSERT(nodeToRemove != 0);
    MATHICGB_ASSERT(mNodes.fromPool(nodeToRemove));
    const auto index = hashToIndex(monoid().hash(nodeToRemove->mono()));
    auto nodePtr = &mBuckets[index];
    while (*nodePtr != nodeToRemove) {
      MATHICGB_ASSERT(*nodePtr != nullptr);
      nodePtr = &(*nodePtr)->next();
    }
    *nodePtr = nodeToRemove->next();
    mNodes.free(nodeToRemove);
    --mSize;
  }

  /// Removes all elements and optimizes internal resources. This is
  /// fast if there are no elements, so if you know that there are no
  /// elements and that many operations have happened since the last clear,
  /// then call clear for better cache performance. If there is even one
  /// element, then this takes linear time in the number of buckets.
  void clear() {
    if (!empty()) {
      std::fill_n(mBuckets.get(), bucketCount(), nullptr);
      mSize = 0;
    }
    mNodes.freeAllBuffers();
  }

  bool empty() const {return mSize == 0;}

  size_t getMemoryUse() const {
    return bucketCount() * sizeof(mBuckets[0]) + mNodes.getMemoryUse();
  }

private:
  /// Change the number of buckets and put all the nodes into their new
  /// places. The bucket array gets put into new memory, but all the nodes
  /// stay where they are.
  void rehash(const size_t requestedBucketCount);

  static HashValue computeHashMask(const size_t requestedBucketCount);

  static size_t hashMaskToBucketCount(const HashValue mask) {
    const auto count = static_cast<size_t>(mask) + 1u; // should be power of 2
    MATHICGB_ASSERT(count > 0 && (count & (count - 1)) == 0); 
    return count;
  }

  static size_t sizeofNode(const PolyRing& ring) {
    return
      sizeof(Node) +
      sizeof(Value) -
      sizeof(exponent) +
      ring.maxMonomialByteSize();
  }

  /// The maximum allowed value of size() / bucketCount() before a rehash
  /// is done.
  static double maxLoadFactor() {return 0.10;}

  size_t hashToIndex(const HashValue hash) const {
    const auto index = hashToIndex(hash, mHashToIndexMask);
    MATHICGB_ASSERT(index == hash % bucketCount());
    return index;
  }

  static size_t hashToIndex(const HashValue hash, const HashValue mask) {
    return hash & mask;
  }

  HashValue mHashToIndexMask;
  std::unique_ptr<Node*[]> mBuckets;
  const PolyRing& mRing;
  memt::BufferPool mNodes;
  size_t mSize;
  size_t mMaxSize;
};

MATHICGB_NAMESPACE_END
#endif
