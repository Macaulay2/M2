#ifndef _word_hpp_
#define _word_hpp_

/**
 * @file NCAlgebras/Word.hpp
 * @brief `Word` and `WordWithData` --- non-owning views over the flat-int encoding of a non-commutative word.
 *
 * Declares the NC-side lightweight view that every word-table,
 * suffix-tree, overlap, and reduction routine passes around.
 * `Word` carries `mBegin` / `mEnd` pointers plus a cached size
 * into a raw `int[]` of variable indices `[v_1, ..., v_k]` ---
 * with no length or weight prefix, since `FreeMonoid` already
 * stores those separately. The class never allocates; the
 * underlying buffer normally lives in a `MemoryBlock` or in a
 * `std::vector<int>` that outlives the `Word`. `init` lets one
 * `Word` instance be rebound across many buffers in a tight
 * loop without reconstruction; the `std::vector<int>`
 * constructor is `explicit` and exists for unit tests where the
 * lifetime is obvious.
 *
 * `WordWithData` decorates a `Word` with `mEcartDegree` and
 * `mHeftDegree` so the word-table sort and divisibility checks
 * over a homogenised system can compare by ecart degree before
 * walking the variable indices. Equality on `WordWithData`
 * deliberately ignores heft degree but respects ecart degree,
 * matching the divisibility convention used by `NCGroebner`'s
 * reducer-selection step. Commutative counterpart is
 * `gb-f4/MonomialView`.
 *
 * @see FreeMonoid.hpp
 * @see WordTable.hpp
 * @see SuffixTree.hpp
 * @see OverlapTable.hpp
 * @see gb-f4/MonomialView.hpp
 */

#include <iosfwd>    // for ostream
#include <vector>    // for vector, vector<>::value_type

/**
 * @brief Non-owning view of a non-commutative word: `[begin, end)` of `int`
 * variable indices.
 *
 * @details Stores three pointers / lengths (`mBegin`, `mEnd`, `mSize`) into
 * an externally owned buffer (typically a `MemoryBlock` arena in
 * `NCF4`). The view is cheap to copy and pass around but the
 * caller must keep the backing buffer alive for the `Word`'s
 * lifetime. Equality is elementwise; iteration goes through
 * `begin()` / `end()` / `operator[]`.
 */
class Word
{
public:
  // warning: the pointers begin, end, should not go out of scope while this Word is in use.
  Word() : mBegin(nullptr), mEnd(nullptr), mSize(0) {}

  Word(const int* begin, const int* end) : mBegin(begin), mEnd(end), mSize(end-begin) {}

  // keyword 'explicit' to prevent calling this constructor implicitly.  It
  // causes some strange behavior in debugging.
  // to be honest, we should only be using this in the unit-tests file ONLY
  explicit Word(const std::vector<int>& val) : mBegin(val.data()), mEnd(val.data() + val.size()), mSize(val.size()) {}

  void init(const int* begin, const int* end) { mBegin = begin; mEnd = end; mSize = end-begin; }
  // this constructor is a bit dangerous since we have several std::vector<int> types running around
  //void init(const std::vector<int>& val) { mBegin = val.data(); mEnd = val.data() + val.size(); }
                                 
  const int* begin() const { return mBegin; }
  const int* end() const { return mEnd; }
  int size() const { return mSize; }

  bool operator==(Word rhs) const
  {
    if (mSize != rhs.mSize) return false;
    for (auto i=0; i<mSize; ++i)
      if (mBegin[i] != rhs.mBegin[i])
        return false;
    return true;
  }

  int operator[](int i) const { return *(mBegin + i); }

private:
  // Caution. We DO NOT own these pointers.
  // It is the responsibility of the calling code to not let these pointers expire.
  const int* mBegin;
  const int* mEnd;
  int mSize;
};

std::ostream& operator<<(std::ostream& o, const Word& w);

/**
 * @brief `Word` plus its ecart degree and heft degree --- the value type
 * `WordWithDataTable` stores.
 *
 * @details `mEcartDegree` records the power of an invisible homogenising
 * variable so divisibility checks in `WordWithDataTable::subword`
 * can refuse matches whose ecart is wrong; `mHeftDegree` is the
 * original heft degree the word entered with, kept for stable
 * sorting. The underlying word is held by composition (`mWord`)
 * rather than inheritance.
 */
// this class is intended for use in the word table, taking ecart degree (i.e.
// the power of an 'invisible' homogenizing variable) into consideration when
// checking divisibility.
// Other data included is the heft degree of the monomial as entered, so that
// a word table may sort.
class WordWithData
{
public:
  // warning: the pointers begin, end, should not go out of scope while this Word is in use.
  WordWithData() : mWord(), mEcartDegree(0), mHeftDegree(0) {}

  WordWithData(const int* begin, const int* end, int ecartDegree, int heftDegree) :
      mEcartDegree(ecartDegree),
      mHeftDegree(heftDegree)
  {
     mWord.init(begin,end);
  }

  // keyword 'explicit' to prevent calling this constructor implicitly.  It
  // causes some strange behavior in debugging.
  // to be honest, we should only be using this in the unit-tests file ONLY
  //explicit WordWithData(const std::vector<int>& val) : mBegin(val.data()), mEnd(val.data() + val.size()) {}

  void init(const int* begin, const int* end, int ecartDegree, int heftDegree)
  {
    mWord.init(begin,end);
    mEcartDegree = ecartDegree;
    mHeftDegree = heftDegree;
  }

  const int* begin() const { return word().begin(); }
  const int* end() const { return word().end(); }

  Word word() const { return mWord; }

  int ecartDegree() const { return mEcartDegree; }
  int heftDegree() const { return mHeftDegree; }

  size_t size() const { return word().size(); }

  bool operator==(WordWithData rhs)
  {
    // Warning: == ignores heft degree, but not ecart degree
    if (ecartDegree() != rhs.ecartDegree()) return false;
    return word() == rhs.word();
  }

private:
  // Caution. We DO NOT own the pointers that are in mWord
  // It is the responsibility of the calling code to not let these pointers expire.
  Word mWord;

  int mEcartDegree;
  int mHeftDegree;
};

std::ostream& operator<<(std::ostream& o, const WordWithData& w);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
