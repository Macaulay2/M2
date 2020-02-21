#ifndef _word_hpp_
#define _word_hpp_

#include <vector>
#include <iosfwd>

class Word
{
public:
  friend std::ostream& operator<<(std::ostream& o, const Word& w);

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

  bool operator==(Word rhs)
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

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
