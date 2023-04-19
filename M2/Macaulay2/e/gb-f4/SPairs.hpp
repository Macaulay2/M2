#pragma once

// Each spair's monomials will be included in a special "hash table".
// What about "pre-spairs"?  Are they in their own, or not yet inserted?
// Maybe try both?

// Inserting new spairs.
//   Making pre-spairs
//   Sort them
//   Find minimal elements (try via masks, via MonomialIdeal type structure, and maybe some mathic structures?)
//   insert each spair directly.
//   
// Grabbing a set of spairs to do (but if interrupted, don't lose the pairs!)
//   sort them? at leat by degree...
//   how to remove a pair that isn't required? (the more complicated spair approach).

// Maybe what I should do before meeting Frank:
//  Translate Matrix to Basis.
//  Translate Basis to Matrix.
// Keep free module, ring around.

// Then: Frank and I can do:
//  1A. macaulay matrix build (maybe mostly use NCGB code)
//  1B. linear algebra part (use NCGB code)
//  2. spair part (this part can be based on f4 code)
//  3. tracing/QQ part (this part is new)

// Want: allow exterior algebra, maybe Weyl algebra.
//       allow quotient rings
//       of course allow modules

using Index = int32_t;

enum class SPairType
  {
    Ring,
    Skew,
    SPair,
    Gen
  };

class SPair
{
private:
  SPairType mType;
  Index mFirst; //later in gb indexing.
  Index mLast;
  MonomialIndex mLCM;
  MonomialInt mDegree; // degree of the LCM
  MonomialIndex mQuotient; // we might not need
};

class SPairSet
{
  /// What needs to be input here?
  SPairSet();
  
  /// updatePairs: takes an element of the GB
  /// and computes all spairs needed with previous elements
  /// in the basis (and ring elements, and skew pairs too),
  /// and adds them to 'this'.
  void updatePairs(const Basis& B, index which);

  // If we keep them sorted in increasing degree, we can
  // grab the next set pretty easily.  How to remove them?
  auto getNextDegree() -> std::pair<Index, std::vector<SPair>>;
  // or maybe have an iterator for it?

  // sort in increasing degree, SPairType::Ring and SPairType::Skew
  // before SPairType::SPair before SPairType::Gen and in each degree by LCM?
  
private:
  std::vector<SPair> mSPairs;
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
