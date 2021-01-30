#include "NCAlgebras/WordTable.hpp"
#include "NCAlgebras/Word.hpp"  // for Word, WordWithData, operator<<

std::ostream& operator<<(std::ostream& o, const WordTable& wordTable)
{
  if (wordTable.mMonomials.size() == 0) return o;

  int j = 0;
  for (; j < wordTable.mMonomials.size() - 1; ++j)
    {
      o << wordTable.mMonomials[j] << ",";
    }
  o << wordTable.mMonomials[j];
  return o;
}

size_t WordTable::insert(Word w)
{
  // we are making a copy of the word, since the pointers
  // to the GB elements may change during autoreduction. 
  auto newW = mMonomialSpace.allocateArray<int>(w.size());
  std::copy(w.begin(),w.end(),newW.first);
  mMonomials.push_back(Word(newW.first,newW.second));
  return mMonomials.size();
}

size_t WordTable::insert(Word w, std::vector<Overlap>& newRightOverlaps)
{
  // we are making a copy of the word, since the pointers
  // to the GB elements may change during autoreduction. 
  auto newW = mMonomialSpace.allocateArray<int>(w.size());
  std::copy(w.begin(),w.end(),newW.first);
  mMonomials.push_back(Word(newW.first,newW.second));
  rightOverlaps(newRightOverlaps); // find (right) overlaps with most recent added word 'w'.
  return mMonomials.size();
}

void WordTable::subwordPositions(Word word1,
                                 Word word2,
                                 std::vector<int>& result_start_indices)
// if there exists monomials p, q, such that p*word1*q == word2, then
// the position of word1 in word2 is added (there may be several
// matches)
{
  if (word2.size() < word1.size()) return;
  for (auto j = 0; j <= word2.size() - word1.size(); ++j)
    {
      bool match = true;
      for (auto k = 0; k < word1.size(); ++k)
        if (word1.begin()[k] != word2.begin()[j+k])
          {
            match = false;
            break;
          }
      if (match) result_start_indices.push_back(j);
    }
}

bool WordTable::subwordPosition(Word word1,
                                 Word word2,
                                 int& result_start_index)
// if there exists monomials p, q, such that p*word1*q == word2, then
// the first position of word1 in word2 is returned in result_start_index
// and true is returned.  If no match, then false is returned,
// and result_start_index is not touched.
{
  if (word2.size() < word1.size()) return false;
  for (auto j = 0; j <= word2.size() - word1.size(); ++j)
    {
      bool match = true;
      for (auto k = 0; k < word1.size(); ++k)
        if (word1.begin()[k] != word2.begin()[j+k])
          {
            match = false;
            break;
          }
      if (match)
        {
          result_start_index = j;
          return true;
        }
    }
  return false;
}

bool WordTable::isPrefixOf(Word word1, Word word2)
// true is returned if there exists monomial q, such that word1*q == word2
{
  if (word2.size() < word1.size()) return false;
  bool match = true;
  for (auto k = 0; k < word1.size(); ++k)
    if (word1.begin()[k] != word2.begin()[k])
      {
        match = false;
        break;
      }
  return match;
}

bool WordTable::isSuffixOf(Word word1, Word word2)
// true is returned if there exists monomial p, such that p*word1 == word2
{
  auto word1size = word1.size();
  auto word2size = word2.size();
  if (word2size < word1size) return false;
  // at this point if word2.size() == 0, then word2 (and hence word1) are both empty
  if (word2size == 0) return false;
  bool match = true;
  for (auto k = 0; k < word1size; ++k)
    if (word1.begin()[word1size-1-k] != word2.begin()[word2size-1-k])
      {
        match = false;
        break;
      }
  return match;
}

void WordTable::subwords(Word word,
                           std::vector<std::pair<int,int>>& output) const
{
  // this command returns a pair (i,j) where word i in the table appears
  // in position j of word.
  std::vector<int> start_indices;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      start_indices.clear();
      subwordPositions(mMonomials[i], word, start_indices);
      for (auto j : start_indices)
        output.push_back(std::make_pair(i,j));
    }
}

bool WordTable::isPrefix(Word word,
                         int& output) const
{
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (isPrefixOf(mMonomials[i], word))
        {
          output = i;
          return true;
        }
    }
  return false;
}

bool WordTable::isSuffix(Word word,
                         int& output) const
{
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (isSuffixOf(mMonomials[i], word))
        {
          output = i;
          return true;
        }
    }
  return false;
}

bool WordTable::subword(Word word,
                        std::pair<int,int>& output) const
{
  int start_index = -1;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (subwordPosition(mMonomials[i], word, start_index))
        {
          output = std::make_pair(i, start_index);
          return true;
        }
    }
  return false;
}

void WordTable::superwords(Word word,
                           std::vector<std::pair<int,int>>& output) const
{
  // this command returns a list of pairs (i,j) where word appears 
  // appears in monomial i in position j
  std::vector<int> start_indices;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      start_indices.clear();
      subwordPositions(word, mMonomials[i], start_indices);
      for (auto j : start_indices)
        output.push_back(std::make_pair(i,j));
    }
}

auto WordTable::isNontrivialSuperword(Word word, int index1, int index2) const -> bool
{
  // this command is only called on an overlap between the words mMonomials[index1]
  // and mMonomials[index2].
  std::vector<int> start_indices;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      start_indices.clear();
      subwordPositions(mMonomials[i], word, start_indices);
      for (auto j : start_indices)
        {
          if (i != index1 && i != index2) return true;
          // these commands handle when the overlap is *trivially* a multiple of one of
          // the monomials in index1 or index2
          if (j == 0 && i != index1) return true;
          if (j == word.size() - mMonomials[index2].size() && i != index2) return true;
        }
    }
  return false;
}

// This function finds overlap where suffix of word at lindex == prefix of word at rindex
void WordTable::overlaps(Word word1,
                         Word word2,
                         std::vector<int>& result_overlaps)
{
  if (word1.size() <= 1) return;
  for (int i = word1.size() - 1; i > 0 and i + word2.size() > word1.size(); --i)
    {
      Word suffix(word1.begin() + i, word1.end());
      Word prefix(word2.begin(), word2.begin() + word1.size() - i); // indices should be in range.
      if (suffix == prefix)
        result_overlaps.push_back(i);
    }
}

void WordTable::leftOverlaps(std::vector<Overlap>& newLeftOverlaps) const
{
  // word here is the last word in the dictionary.
  // For left overlap: dictword is a word in the dictionary NOT word.
  // For right overlap: dictword is any word in the dictionary.
  // A left overlap: suffix(dictword) == prefix(word)
  // A right overlap: prefix(dictword) == suffix(word).
  //   if dictword == word, a match will be a right overlap, NOT a left overlap.
  // A returned triple:
  //   (left overlap):

  int word_index = mMonomials.size()-1;
  std::vector<int> overlap_indices;
  for (int i=0; i<word_index; ++i)
    {
      overlap_indices.clear();
      overlaps(mMonomials[i], mMonomials[word_index], overlap_indices);
      for (auto j : overlap_indices)
        newLeftOverlaps.push_back(std::make_tuple(i, j, word_index,true));
    }

  // triples will be <dict word index, index into dict word where suffix starts, word_index>.
}

void WordTable::rightOverlaps(std::vector<Overlap>& newRightOverlaps) const
// find (right) overlaps with most recent added word 'w'.
{
  int word_index = mMonomials.size()-1;
  std::vector<int> overlap_indices;
  for (int i=0; i<=word_index; ++i)
    {
      overlap_indices.clear();
      overlaps(mMonomials[word_index], mMonomials[i], overlap_indices);
      for (auto j : overlap_indices)
        newRightOverlaps.push_back(std::make_tuple(word_index, j, i,true));
    }
}

std::ostream& operator<<(std::ostream& o, const WordWithDataTable& wordTable)
{
  if (wordTable.mMonomials.size() == 0) return o;

  int j = 0;
  for (; j < wordTable.mMonomials.size() - 1; ++j)
    {
      o << wordTable.mMonomials[j] << ",";
    }
  o << wordTable.mMonomials[j];
  return o;
}

size_t WordWithDataTable::insert(WordWithData w)
{
  size_t retval = mMonomials.size();
  mMonomials.push_back(w);
  mIndices.push_back(retval);
  return retval;
}

size_t WordWithDataTable::insert(WordWithData w, std::vector<Overlap>& newRightOverlaps)
{
  size_t retval = mMonomials.size();
  mMonomials.push_back(w);
  mIndices.push_back(retval);
  rightOverlaps(newRightOverlaps); // find (right) overlaps with most recent added word 'w'.
  return retval;
}

void WordWithDataTable::subwordPositions(WordWithData word1,
                                         WordWithData word2,
                                         std::vector<int>& result_start_indices)
// if there exists monomials p, q, such that p*word1*q == word2, then
// the position of word1 in word2 is added (there may be several
// matches)
{
  if (word2.size() < word1.size()) return;
  if (word2.ecartDegree() < word1.ecartDegree()) return;
  for (auto j = 0; j <= word2.size() - word1.size(); ++j)
    {
      bool match = true;
      for (auto k = 0; k < word1.size(); ++k)
        if (word1.begin()[k] != word2.begin()[j+k])
          {
            match = false;
            break;
          }
      if (match) result_start_indices.push_back(j);
    }
}

bool WordWithDataTable::subwordPosition(WordWithData word1,
                                        WordWithData word2,
                                        int& result_start_index)
// if there exists monomials p, q, such that p*word1*q == word2, then
// the first position of word1 in word2 is returned in result_start_index
// and true is returned.  If no match, then false is returned,
// and result_start_index is not touched.
{
  if (word2.size() < word1.size()) return false;
  if (word2.ecartDegree() < word1.ecartDegree()) return false;
  for (auto j = 0; j <= word2.size() - word1.size(); ++j)
    {
      bool match = true;
      for (auto k = 0; k < word1.size(); ++k)
        if (word1.begin()[k] != word2.begin()[j+k])
          {
            match = false;
            break;
          }
      if (match)
        {
          result_start_index = j;
          return true;
        }
    }
  return false;
}

bool WordWithDataTable::isPrefixOf(WordWithData word1, WordWithData word2)
// true is returned if there exists monomial q, such that word1*q == word2
{
  if (word2.size() < word1.size()) return false;
  bool match = true;
  for (auto k = 0; k < word1.size(); ++k)
    if (word1.begin()[k] != word2.begin()[k])
      {
        match = false;
        break;
      }
  return match;
}

bool WordWithDataTable::isSuffixOf(WordWithData word1, WordWithData word2)
// true is returned if there exists monomial p, such that p*word1 == word2
{
  auto word1size = word1.size();
  auto word2size = word2.size();
  if (word2size < word1size) return false;
  // at this point if word2.size() == 0, then word2 (and hence word1) are both empty
  if (word2size == 0) return false;
  bool match = true;
  for (auto k = 0; k < word1size; ++k)
    if (word1.begin()[word1size-1-k] != word2.begin()[word2size-1-k])
      {
        match = false;
        break;
      }
  return match;
}

void WordWithDataTable::subwords(WordWithData word,
                                 std::vector<std::pair<int,int>>& output) const
{
  // this command returns a pair (i,j) where word i in the table appears
  // in position j of word.
  std::vector<int> start_indices;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (mIndices[i] == -1) continue;
      start_indices.clear();
      subwordPositions(mMonomials[i], word, start_indices);
      for (auto j : start_indices)
        output.push_back(std::make_pair(i,j));
    }
}

bool WordWithDataTable::isPrefix(WordWithData word,
                                 int& output) const
{
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (mIndices[i] == -1) continue;
      if (isPrefixOf(mMonomials[i], word))
        {
          output = i;
          return true;
        }
    }
  return false;
}

bool WordWithDataTable::isSuffix(WordWithData word,
                                 int& output) const
{
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (mIndices[i] == -1) continue;
      if (isSuffixOf(mMonomials[i], word))
        {
          output = i;
          return true;
        }
    }
  return false;
}

bool WordWithDataTable::subword(WordWithData word,
                                std::pair<int,int>& output) const
{
  int start_index = -1;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (mIndices[i] == -1) continue;
      if (subwordPosition(mMonomials[i], word, start_index))
        {
          output = std::make_pair(i, start_index);
          return true;
        }
    }
  return false;
}

void WordWithDataTable::superwords(WordWithData word,
                                   std::vector<std::pair<int,int>>& output) const
{
  // this command returns a list of pairs (i,j) where word appears 
  // appears in monomial i in position j
  std::vector<int> start_indices;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (mIndices[i] == -1) continue;
      start_indices.clear();
      subwordPositions(word, mMonomials[i], start_indices);
      for (auto j : start_indices)
        output.push_back(std::make_pair(i,j));
    }
}

auto WordWithDataTable::isNontrivialSuperword(WordWithData word, int index1, int index2) const -> bool
{
  // this command is only called on an overlap between the words mMonomials[index1]
  // and mMonomials[index2].
  std::vector<int> start_indices;
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      if (mIndices[i] == -1) continue;
      start_indices.clear();
      subwordPositions(mMonomials[i], word, start_indices);
      for (auto j : start_indices)
        {
          if (i != index1 && i != index2) return true;
          // these commands handle when the overlap is trivially a multiple of one of
          // the monomials in index1 or index2
          if (j == 0 && i != index1) return true;
          if (j == word.size() - mMonomials[index2].size() && i != index2) return true;
        }
    }
  return false;
}

// This function finds overlap where suffix of word at lindex == prefix of word at rindex
void WordWithDataTable::overlaps(WordWithData word1,
                                 WordWithData word2,
                                 std::vector<int>& result_overlaps)
{
  if (word1.size() <= 1) return;
  for (int i = word1.size() - 1; i > 0 and i + word2.size() > word1.size(); --i)
    {
      // below, we are disregarding ecart degree for overlap detection/generation
      Word suffix(word1.begin() + i, word1.end());
      Word prefix(word2.begin(), word2.begin() + word1.size() - i); // indices should be in range.
      if (suffix == prefix)
        result_overlaps.push_back(i);
    }
}

void WordWithDataTable::leftOverlaps(std::vector<Overlap>& newLeftOverlaps) const
{
  // word here is the last word in the dictionary.
  // For left overlap: dictword is a word in the dictionary NOT word.
  // For right overlap: dictword is any word in the dictionary.
  // A left overlap: suffix(dictword) == prefix(word)
  // A right overlap: prefix(dictword) == suffix(word).
  //   if dictword == word, a match will be a right overlap, NOT a left overlap.
  // A returned triple:
  //   (left overlap):

  int word_index = mMonomials.size()-1;
  std::vector<int> overlap_indices;
  for (int i=0; i<word_index; ++i)
    {
      if (mIndices[i] == -1) continue;
      overlap_indices.clear();
      overlaps(mMonomials[i], mMonomials[word_index], overlap_indices);
      for (auto j : overlap_indices)
        newLeftOverlaps.push_back(std::make_tuple(i, j, word_index,true));
    }
  // triples will be <dict word index, index into dict word where suffix starts, word_index>.
}

void WordWithDataTable::rightOverlaps(std::vector<Overlap>& newRightOverlaps) const
// find (right) overlaps with most recent added word 'w'.
{
  int word_index = mMonomials.size()-1;
  std::vector<int> overlap_indices;
  for (int i=0; i<=word_index; ++i)
    {
      if (mIndices[i] == -1) continue;
      overlap_indices.clear();
      overlaps(mMonomials[word_index], mMonomials[i], overlap_indices);
      for (auto j : overlap_indices)
        newRightOverlaps.push_back(std::make_tuple(word_index, j, i,true));
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
