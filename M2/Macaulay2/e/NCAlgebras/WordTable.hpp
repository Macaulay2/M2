#ifndef _word_table_hpp_
#define _word_table_hpp_

#include <vector>

class ConstMonomial
{
public:
  // warning: the pointers begin, end, should not go out of scope while this ConstMonomial is in use.
  ConstMonomial(const int* begin, const int* end) : mBegin(begin), mEnd(end) {}

  ConstMonomial(const std::vector<int>& val) : mBegin(val.data()), mEnd(val.data() + val.size()) {}

  //  ConstMonomial(const Monom& m) : mBegin(m.begin()+2), mEnd(m.end()) {}
                                  
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

  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  void subwords(ConstMonomial word,
                std::vector<std::pair<int,int>>& output);

  // sets 'output' to the first pair (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  // if such a match is found, output is set, and true is returned.
  // if not, false is returned.
  bool subword(ConstMonomial word,
                std::pair<int,int>& output);
  
  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in w
  //   such that word appears in w starting at position j.
  void superwords(ConstMonomial word,
                  std::vector<std::pair<int,int>>& output);
  
  //TODO: write superword.  i.e. only return 1, if any.
  
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

  static bool subwordPosition(ConstMonomial word1,
                               ConstMonomial word2,
                               int& result_start_index);
  
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

