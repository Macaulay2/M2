#ifndef _word_hpp_
#define _word_hpp_

#include <iosfwd>    // for ostream
#include <vector>    // for vector, vector<>::value_type

class Word
{
public:
  // warning: the pointers begin, end, should not go out of scope while this Word is in use.
  Word() : mBegin(nullptr), mEnd(nullptr) {}

  Word(const int* begin, const int* end) : mBegin(begin), mEnd(end) {}

  // keyword 'explicit' to prevent calling this constructor implicitly.  It
  // causes some strange behavior in debugging.
  // to be honest, we should only be using this in the unit-tests file ONLY
  explicit Word(const std::vector<int>& val) : mBegin(val.data()), mEnd(val.data() + val.size()) {}

  void init(const int* begin, const int* end) { mBegin = begin; mEnd = end; }
  // this constructor is a bit dangerous since we have several std::vector<int> types running around
  //void init(const std::vector<int>& val) { mBegin = val.data(); mEnd = val.data() + val.size(); }
                                 
  const int* begin() const { return mBegin; }
  const int* end() const { return mEnd; }

  size_t size() const { return mEnd - mBegin; }

  bool operator==(Word rhs) const
  {
    if (size() != rhs.size()) return false;
    for (auto i=0; i<size(); ++i)
      if (mBegin[i] != rhs.mBegin[i])
        return false;
    return true;
  }

private:
  // Caution. We DO NOT own these pointers.
  // It is the responsibility of the calling code to not let these pointers expire.
  const int* mBegin;
  const int* mEnd;
};

std::ostream& operator<<(std::ostream& o, const Word& w);

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
