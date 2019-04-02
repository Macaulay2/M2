#ifndef _word_table_hpp_
#define _word_table_hpp_

#include <vector>
#include <ostream>
#include "../Polynomial.hpp"

class Word
{
public:
  friend std::ostream& operator<<(std::ostream& o, const Word& w);

  // warning: the pointers begin, end, should not go out of scope while this Word is in use.
  Word() : mBegin(nullptr), mEnd(nullptr) {}

  Word(const Monom& m) : mBegin(m.begin()+2), mEnd(m.end()) {}
  Word(const int* begin, const int* end) : mBegin(begin), mEnd(end) {}

  Word(const std::vector<int>& val) : mBegin(val.data()), mEnd(val.data() + val.size()) {}

  void init(const int* begin, const int* end) { mBegin = begin; mEnd = end; }
  void init(const std::vector<int>& val) { mBegin = val.data(); mEnd = val.data() + val.size(); }
                                 
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

  static void toAllocatedMonom(std::vector<int>& result, const Word& source)
  {
    result.push_back(source.end() - source.begin() + 2);
    result.push_back(source.end() - source.begin());
    result.insert(result.end(),source.begin(), source.end());
  }
  
private:
  const int* mBegin;
  const int* mEnd;
};

using Overlap = std::tuple<int,int,int>;

class WordTable
{
  // abstract table class.
public:
  friend std::ostream& operator<<(std::ostream& o, const WordTable& wordTable);

  WordTable() {}

  ~WordTable() {}

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(Word w);

  size_t insert(Word w, std::vector<Overlap>& newRightOverlaps);

  // access routine
  const Word& operator[](int index) const { return mMonomials[index]; }

  // lookup routines

  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  void subwords(Word word,
                std::vector<std::pair<int,int>>& output) const;

  // sets 'output' to the first pair (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  // if such a match is found, output is set, and true is returned.
  // if not, false is returned.
  bool subword(Word word,
                std::pair<int,int>& output) const;

  auto isNontrivialSuperword(Word word, int index1, int index2) const -> bool;
  
  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in w
  //   such that word appears in w starting at position j.
  void superwords(Word word,
                  std::vector<std::pair<int,int>>& output) const;
  
  // given 'word', find all left over laps with elements of the table.
  // A left overlap of 'alpha' and 'beta' is:
  //  a prefix of alpha is a suffix of beta.
  // i.e. alpha = a.b
  //      beta  = c.a (a,b,c are words)
  // returned Overlap for this overlap:
  void leftOverlaps(std::vector<Overlap>& newLeftOverlaps) const;

  // find (right) overlaps with most recent added word 'w'.
  void rightOverlaps(std::vector<Overlap>& newRightOverlaps) const; 

private:
  static void subwordPositions(Word word1,
                               Word word2,
                               std::vector<int>& result_start_indices);

  static bool subwordPosition(Word word1,
                               Word word2,
                               int& result_start_index);
  
  // overlaps here: suffix of word1 == prefix of word2.
  // overlap value is the start of prefix of word2 in word1.
  static void overlaps(Word word1,
                       Word word2,
                       std::vector<int>& result_overlaps);

private:
  std::vector<Word> mMonomials;
};

std::ostream& operator<<(std::ostream& o, const Word& w);
std::ostream& operator<<(std::ostream& o, const WordTable& wordTable);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

