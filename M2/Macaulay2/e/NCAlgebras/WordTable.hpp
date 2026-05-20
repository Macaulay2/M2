#ifndef _word_table_hpp_
#define _word_table_hpp_

/**
 * @file NCAlgebras/WordTable.hpp
 * @brief `WordTable` / `WordWithDataTable` --- leading-word indices for non-commutative Gröbner basis lookup.
 *
 * Declares the two structures `NCGroebner` and `NCF4` consult
 * to answer "does some basis leading word occur as a contiguous
 * subword of this target?" --- the non-commutative analogue of
 * monomial divisibility. Storage is a parallel pair:
 * `std::vector<Word> mMonomials` for the words themselves and
 * `std::vector<int> mIndices` with `-1` marking retired
 * entries; the actual word bytes live in
 * `MemoryBlock mMonomialSpace`. The soft-delete mechanism is
 * fully wired in the companion `WordWithDataTable` (whose
 * `clear()` resets both vectors) but only partially wired in
 * `WordTable` itself --- the in-file TODO at the top tracks
 * the missing `retire(index)` / search-skipping work. Public
 * surface: `insert(w)` / `insert(w, newRightOverlaps)` extends
 * the table while harvesting the right-overlaps the new entry
 * generates with every existing word; `subword(target, out)`
 * reports the first match used to drive reduction;
 * `subwords(target, out)` collects all of them; `superwords`
 * does the reverse direction; `isPrefix`/`isSuffix`/
 * `isNontrivialSuperword` check the boundary cases;
 * `leftOverlaps` / `rightOverlaps` enumerate overlap pairs.
 *
 * `WordWithDataTable` is the ecart-degree-aware variant ---
 * subword matches additionally require the matched word's
 * ecart to be at most the target's, so inhomogeneous GB
 * computations can pick the right reducer. The string-matching
 * subword search is the hot path of the entire NC GB engine;
 * the naive walk is `O(|target| * |table|)` per query and is
 * the production choice today. `SuffixTree` is the
 * experimental constant-`O(|target|)` alternative the engine
 * is staged to swap in (commented-out alternative in
 * `NCGroebner` / `NCF4`).
 *
 * @see Word.hpp
 * @see SuffixTree.hpp
 * @see NCGroebner.hpp
 * @see NCF4.hpp
 * @see MemoryBlock.hpp
 */

#include <cstddef>
#include <ostream>
#include <tuple>
#include <utility>
#include <vector>

#include "MemoryBlock.hpp" // for MemoryBlock

class Word;
class WordWithData;

// TODO
// have a vector std::vector<int> of indices of each word.
//   an index of -1 means that this element has been removed.
// changes to code:
//   insert: push_back of the index, return that index.
//   routines that search over words in the table:
//     each should continue from any element with -1, ignoring it.
//   retire(index): set index to -1, set word to null's.
// move Word code to Word.hpp DONE

using Overlap = std::tuple<int,int,int,bool>;

/**
 * @brief Index of `Word`s (non-commutative monomials) with subword,
 * prefix/suffix, and overlap lookup used by the NC Groebner code.
 *
 * @details Stores monomials in `mMonomials` and a parallel `mIndices` slot
 * per entry (-1 marks a retired word the search routines should
 * skip). The query API exposes the operations the NC GB inner loop
 * needs --- `subwords` / `subword` (where does each table entry
 * occur inside `word`?), `isPrefix` / `isSuffix` (single-end
 * containment), `superwords` (the reverse direction), and
 * `leftOverlaps` / `rightOverlaps` (suffix-of-X equals prefix-of-Y
 * pairs that generate new S-pairs). Word storage lives in a
 * `MemoryBlock` arena so insertions are bump-pointer cheap.
 */
class WordTable
{
  // abstract table class for Word's
public:
  friend std::ostream& operator<<(std::ostream& o, const WordTable& wordTable);

  WordTable() {}

  ~WordTable() {}

  void clear() { mMonomials.clear(); }

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

  // returns true if some element in the table is a prefix/suffix of 'word'.
  // In this case, 'output' is set to the index of the corresponding element.
  bool isPrefix(Word word, int& output) const;
  bool isSuffix(Word word, int& output) const;
  
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

  // moved to public for use in retiring unnecessary overlaps
  static void subwordPositions(Word word1,
                               Word word2,
                               std::vector<int>& result_start_indices);

private:
  // returns true if word1 is a prefix/suffix of word2
  static bool isPrefixOf(Word word1, Word word2);
  static bool isSuffixOf(Word word1, Word word2);
  
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
  std::vector<int> mIndices; // -1 means word was retired, cannot be used in this class any longer.
  
  MemoryBlock mMonomialSpace;
};

/**
 * @brief Variant of `WordTable` where each stored monomial carries an
 * additional ecart-degree datum that gates subword matches.
 *
 * @details Same skeleton as `WordTable` (parallel `mMonomials` / `mIndices`
 * with -1 marking retired entries), but `subword` queries take an
 * ecart degree into account, skipping entries whose ecart is larger
 * than the query's. Used by the GB code paths that need
 * ecart-aware reduction (typically inhomogeneous problems with
 * sugar / ecart strategies).
 */
class WordWithDataTable
{
  // abstract table class for WordWithData's

  // all searching functions will ignore a WordWithData whose corresponding entry in mIndices is
  // set to -1.

  // Any subword lookups are affected by ecart degree (see subword description below)
public:
  friend std::ostream& operator<<(std::ostream& o, const WordWithDataTable& wordWithDataTable);

  WordWithDataTable() {}

  ~WordWithDataTable() {}

  void clear() { mMonomials.clear(); mIndices.clear(); }

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(WordWithData w);

  size_t insert(WordWithData w, std::vector<Overlap>& newRightOverlaps);

  // access routine
  const WordWithData& operator[](int index) const { return mMonomials[index]; }

  // lookup routines
  
  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  // ** ecart degree affects matches as well.  see description
  // ** of subword below.
  void subwords(WordWithData word,
                std::vector<std::pair<int,int>>& output) const;

  // sets 'output' to the first pair (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  // ** also, ecart degree is checked as well to see if 
  // ** ecart degree of w is less than or equal to that of 'word'
  // if such a match is found, output is set, and true is returned.
  // if not, false is returned.
  bool subword(WordWithData word,
                std::pair<int,int>& output) const;

  // returns true if some element in the table is a prefix/suffix of 'word'.
  // In this case, 'output' is set to the index of the corresponding element.
  bool isPrefix(WordWithData word, int& output) const;
  bool isSuffix(WordWithData word, int& output) const;
  
  auto isNontrivialSuperword(WordWithData word, int index1, int index2) const -> bool;
  
  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in w
  //   such that word appears in w starting at position j.
  void superwords(WordWithData word,
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

  // retire the ith word in the table
  void retire(int retiree) { mIndices[retiree] = -1; }

private:
  // returns true if word1 is a prefix/suffix of word2
  static bool isPrefixOf(WordWithData word1, WordWithData word2);
  static bool isSuffixOf(WordWithData word1, WordWithData word2);
  
  static void subwordPositions(WordWithData word1,
                               WordWithData word2,
                               std::vector<int>& result_start_indices);

  static bool subwordPosition(WordWithData word1,
                               WordWithData word2,
                               int& result_start_index);
  
  // overlaps here: suffix of word1 == prefix of word2.
  // overlap value is the start of prefix of word2 in word1.
  static void overlaps(WordWithData word1,
                       WordWithData word2,
                       std::vector<int>& result_overlaps);

private:
  // WARNING TODO: If this is to be used, one must make copies of the word part
  // in order to work with our approach to autoreduction.
  std::vector<WordWithData> mMonomials;
  std::vector<int> mIndices; // -1 means word was retired, cannot be used in this class any longer.
};


std::ostream& operator<<(std::ostream& o, const WordTable& wordTable);
std::ostream& operator<<(std::ostream& o, const WordWithDataTable& wordWithDataTable);


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

