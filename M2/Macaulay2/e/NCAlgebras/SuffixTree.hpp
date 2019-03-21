#ifndef _suffix_tree_hpp_
#define _suffix_tree_hpp_

#include <vector>
#include <unordered_set>
#include <tuple>

#include "WordTable.hpp"

// used in return value for WordTable as well
using Triple = std::tuple<int,int,int>;

// data type of an arc/vertex label
using Label = std::vector<int>;

// return value types for internal functions
using ContractedLocusType = std::tuple<SuffixTreeNode*,
				       SuffixTreeNode*,
				       Label*>;

using ExtendedLocusType = std::tuple<SuffixTreeNode*,
				     Label*>;

using InsertWorkerType = std::tuple<SuffixTreeNode*,
				    SuffixTreeNode*,
				    SuffixTreeNode*>;

using LeavesType = std::tuple<SuffixTreeNode*,
			      int>;

using SubwordsType = std::tuple<Label*,
				int,
				Label*>;

// this class is basically a wrapper on std::unordered_set, but with a few
// additional things added for dropping prefixes and adding them to
// the pool, etc.
class LabelPool
{
public:
  // take the prefix/suffix of f and find/insert it in the pool
  auto prefix(Label* f, int n) -> Label*;
  auto suffix(Label* f, int n) -> Label*;

  // this is a wrapper for std::unordered_set::insert, but all
  // return values are discarded.
  void insert(Label* f);

private:
  std::unordered_set<Label*> mLabelPool;
}

class SuffixTreeNode
{
public:
  
private:
  // should we use pointers, or indices into some list?

  // parent of this node
  SuffixTreeNode* mParent;
  // children of this node
  std::vector<SuffixTreeNode*> mChildren;
  // suffix link pointer
  SuffixTreeNode* mSuffixLink;

  // does this node correspond to a full pattern (i.e. not a suffix of one)?
  bool mIsFullPattern;
  // counts the number of pattern leaves that are descendents of this node
  int mPatternLeafCount;

  // a label of a node can be any suffix of the word w . n where n is the
  // (negative of) the index of the word w.  We use negatives here so they don't
  // collide with the nonnegative integers, which represent variables.
  Label* mArcLabel;
  // this could be inductively recomputed each time, but I think it is better just
  // to store a copy of it in the data type
  Label* mLabel;
}

class SuffixTree
{
public:

  // these functions are also in WordTable; we would like to keep the
  // basic interface of both classes the same

  // need to create the root
  SuffixTree();

  // the SuffixTree owns all Labels and SuffixTreeNodes created within,
  // as well as the monomials inserted (stored as labels).
  ~SuffixTree();

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(Word w);

  size_t insert(Word w, std::vector<Triple>& newRightOverlaps);

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
  
  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in w
  //   such that word appears in w starting at position j.
  void superwords(Word word,
                  std::vector<std::pair<int,int>>& output) const;
  
  // TODO: write superword.  i.e. only return 1, if any.
  
  // given 'word', find all left over laps with elements of the table.
  // A left overlap of 'alpha' and 'beta' is:
  //  a prefix of alpha is a suffix of beta.
  // i.e. alpha = a.b
  //      beta  = c.a (a,b,c are words)
  // returned Triple for this overlap:
  void leftOverlaps(std::vector<Triple>& newLeftOverlaps) const;

  // find (right) overlaps with most recent added word 'w'.
  void rightOverlaps(std::vector<Triple>& newRightOverlaps) const; 

private:
  // the following are internal functions needed for the SuffixTree data structure
  // to work

  // This function splits the arc from f to its parent by inserting a new internal
  // node with arc label prefix, where prefix is a prefix of f->arcLabel().
  // A pointer to the
  auto splitArc(SuffixTreeNode* f,
		Word prefix) -> SuffixTreeNode*;

  // s is a suffix not yet in the table.  This function finds 
  // the locus of the longest prefix of s whose locus exists.
  // The search starts at y, and moves down the tree according to the
  // string s.  The return value is the contracted locus x
  // and a node f which is either a child of y sharing a prefix pre with s - y.label,
  // or f is nullTreeNode if no such child exists.
  auto contractedLocus(SuffixTreeNode* y,
		       Label* s,
		       bool incrementLeafCount) -> ContractedLocusType;

  // For this function to work, there must be a path starting from x with
  // beta as a prefix (See e.g. Lemma 1 in Amir, et.al.)
  // This function finds the locus of the shortest word that has beta as a prefix.
  // it returns this locus, together with the prefix that needs to be split (if necessary)
  // if beta is empty, then simply return (x,beta) since x is the extended locus
  auto extendedLocus(SuffixTreeNode* x,
		     Label* beta) -> ExtendedLocusType;

  // Finds an arc from y to a child whose label shares a prefix with s
  // return a pair of nullptrs if no match is found, i.e. the empty prefix is the only
  // shared prefix with any child of y
  auto findMatch(SuffixTreeNode* y,
		 Label* s) -> ExtendedLocusType;

  // Return a pointer to the longest shared prefix of s and t
  auto sharePrefix(Label* s, Label* t) -> Label*;

  // Return all pattern leaves below v
  auto patternLeaves(SuffixTreeNode* v) -> std::vector<LeavesType>;
  auto patternLeavesWorker(SuffixTreeNode* v) -> std::vector<SuffixTreeNode*>;

  // return all leaves below v
  auto allLeaves(SuffixTreeNode* v) -> std::vector<LeavesType>;
  auto allLeavesWorker(SuffixTreeNode* v) -> std::vector<SuffixTreeNode*>;

  // functions for insert algorithm
  auto insertWorker(SuffixTreeNode* v,
		    Label* s,
		    bool isFullPattern) -> InsertWorkerType;
  auto insertStepC(SuffixTreeNode* v,
		   SuffixTreeNode* x,
		   Label* beta,
		   Label* s,
		   bool isFullPattern) -> InsertWorkerType;
  auto insertStepD(SuffixTreeNode* y,
		   Label* s,
		   bool isFullPattern) -> InsertWorkerType;

  // functions for subwords algorithm
  auto subwordsWorker(SuffixTreeNode* cLocus,
		      Label* beta,
		      Label* s) -> std::vector<SubwordsType>;
  auto subwordsStepC(SuffixTreeNode* x,
		     Label* beta,
		     Label* s) -> SubwordsType;
  auto subwordsStepD(SuffixTreeNode* y,
		     Label* s) -> SubwordsType;
  
  
private:
  // root node of the tree
  SuffixTreeNode* mRoot;

  // we will copy the monomials into the data structure.  Even though
  // this is of type Label, these monomials will not have the word number
  // appended as a suffix when stored in this container.
  std::vector<Label> mMonomials;

  // this is where all the labels for the data structure will be housed
  // The suffix tree owns all the labels, so all pointers in the label
  // pool must be freed upon destruction
  LabelPool mLabelPool;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
