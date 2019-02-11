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

  bool operator==(ConstMonomial rhs)
  {
    if (size() != rhs.size()) return false;
    for (auto i=0; i<size(); ++i)
      if (mBegin[i] != rhs.mBegin[i])
        return false;
    return true;
  }
private:
  const int* mBegin;
  const int* mEnd;
};

using Triple = std::tuple<int,int,int>;

class WordTable
{
  // abstract table class.
public:
  WordTable() {}

  ~WordTable() {}

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(ConstMonomial w);

  size_t insert(ConstMonomial w, std::vector<Triple>& newRightOverlaps);

  // lookup routines

  // which monomial, index into word, length of monomial.
  // subwords returns a vector of all subword matches:
  //   each entry is a pair:
  //     a. index of the monomial in the table
  //     b. index into that monomial
  void subwords(ConstMonomial word,
                std::vector<std::pair<int,int>>& output);

  void superwords(ConstMonomial word,
                  std::vector<std::pair<int,int>>& output);
  

  // given 'word', find all left over laps with elements of the table.
  // A left overlap of 'alpha' and 'beta' is:
  //  a prefix of alpha is a suffix of beta.
  // i.e. alpha = a.b
  //      beta  = c.a (a,b,c are words)
  // returned Triple for this overlap:
  void leftOverlaps(std::vector<Triple>& newLeftOverlaps);

  // find (right) overlaps with most recent added word 'w'.
  void rightOverlaps(std::vector<Triple>& newRightOverlaps); 

private:
  static void subwordPositions(ConstMonomial word1,
                               ConstMonomial word2,
                               std::vector<int>& result_start_indices);

  // overlaps here: suffix of word1 == prefix of word2.
  // overlap value is the start of prefix of word2 in word1.
  static void overlaps(ConstMonomial word1,
                       ConstMonomial word2,
                       std::vector<int>& result_overlaps);

private:
  std::vector<ConstMonomial> mMonomials;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

