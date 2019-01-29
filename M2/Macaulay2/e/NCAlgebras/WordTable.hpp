#ifndef _word_table_hpp_
#define _word_table_hpp_

#include <vector>

class ConstMonomial
{
public:
  // warning: the pointers begin, end, should not go out of scope while this ConstMonomial is in use.
  ConstMonomial(const int* begin, const int* end) : mBegin(begin), mEnd(end) {}

  ConstMonomial(const std::vector<int>& val) : mBegin(val.data()), mEnd(val.data() + val.size()) {}

  const int* begin() const { return mBegin; }
  const int* end() const { return mEnd; }

  size_t size() const { return mEnd - mBegin; }
private:
  const int* mBegin;
  const int* mEnd;
};

class WordTable
{
  // abstract table class.
public:
  WordTable() {}

  ~WordTable() {}

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(ConstMonomial w);

  // lookup routines

  // which monomial, index into word, length of monomial.
  // subwords returns a vector of all subword matches:
  //   each entry is a triple:
  //     a. index of the monomial in the table
  //     b. index into that monomial
  size_t subwords(ConstMonomial word,
                  std::vector<std::pair<int,int>>& output);

private:
  std::vector<ConstMonomial> mMonomials;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

