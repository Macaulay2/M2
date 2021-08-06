// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_SIG_S_PAIR_QUEUE_GUARD
#define MATHICGB_SIG_S_PAIR_QUEUE_GUARD

#include "PolyRing.hpp"
#include <string>
#include <vector>

MATHICGB_NAMESPACE_BEGIN

typedef unsigned short SmallIndex;
typedef unsigned int BigIndex;

struct PreSPair {
  BigIndex i;
  PolyRing::Monoid::MonoPtr signature;
};

class SigPolyBasis;

// A priority queue on S-pairs where the priority is based on a
// signature as in signature Grobner basis algorithms. The class is
// not responsible for eliminating S-pairs or doing anything beyond
// order the S-pairs.
class SigSPairQueue {
public:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  virtual ~SigSPairQueue();

  typedef std::pair<size_t, size_t> Pair;
  typedef std::vector<Pair> Pairs;
  //typedef std::pair<size_t, monomial> IndexSig;
  typedef PreSPair IndexSig;
  typedef std::vector<IndexSig> IndexSigs;

  // Takes the minimal signature in the queue and adds all S-pairs of
  // that signature to pairs. Clears pairs first. Returns null and
  // leaves pairs empty if the queue is empty.
  //
  // This class does not have an empty() method on purpose - you are
  // supposed to call this method until it returns null.
  virtual Mono popSignature(Pairs& pairs) = 0;

  // If (x, sig) is an element of pairsConsumed then (pairWith, x) is
  // added to the queue. sig must be the signature of the S-pair
  // (pairWith, x).
  //
  // ATTENTION: the calls to pushPairs must have pairWith in the
  // sequence 0, 1, 2, 3 and so on. It follows from this that the
  // queue can figure out what pairWith is without being told. Thus
  // the purpose of pairWith is to make it possible to make an
  // assertion saying that the caller and the queue agree on what
  // pairWith is.
  // 
  // ATTENTION: pairsConsumed will be cleared and the signatures in it
  // will be freed on the ring. This is because the queue will need to
  // alter pairsConsumed in various ways and clearing it after that is
  // cleaner than exposing what's being done to
  // pairsConsumed. Especially since after you did pushPairs there
  // would not be a reason to care about what its content was.
  virtual void pushPairs(size_t pairWith, IndexSigs& pairsConsumed) = 0;

  // Returns a string that describes the queue.
  virtual std::string name() const = 0;

  // Returns the number of pairs currently in the queue.
  virtual size_t pairCount() const  = 0;

  // Returns number of bytes of memory used.
  virtual size_t memoryUse() const = 0;

  static std::unique_ptr<SigSPairQueue> create(SigPolyBasis const& basis);
};

MATHICGB_NAMESPACE_END
#endif
