#ifndef _suffix_tree_hpp_
#define _suffix_tree_hpp_

#include <iostream>
#include <vector>
#include <unordered_set>
#include <tuple>
#include <map>
#include <gtest/gtest.h>

#include "WordTable.hpp"

// used in return value for WordTable as well
using Triple = std::tuple<int,int,int>;

// data type of an arc/vertex label
using Label = std::vector<int>;

// this class is basically a wrapper on std::unordered_set, but with a few
// additional things added for dropping prefixes and adding them to
// the pool, etc.

#if 0
// I am not sure we need this - the labels won't take up too much additional
// room, and the copys required to do the lookup may be too much effort.
class LabelPool
{
  
public:
  // take the prefix/suffix of f and find/insert it in the pool
  // return a pointer to the label in the pool
  auto prefix(const Label& f, int lengthOfPrefix) -> Label*;
  auto suffix(const Label& f, int indexOfSuffix) -> Label*;

  // this is a wrapper for std::unordered_set::insert, but all
  // return values are discarded 
  void insert(const Label& f);

private:
  std::unordered_set<Label> mLabelPool;
}
#endif
  
class SuffixTreeNode
{
public:
  friend std::ostream& operator<<(std::ostream& o, SuffixTreeNode& suffixTreeNode);
  std::ostream& dump(std::ostream&, int depth);

  // the default constructor makes a root node
  SuffixTreeNode() :
    mParent(nullptr),
    mChildren(std::map<Label,SuffixTreeNode*> {}),
    mSuffixLink(nullptr),
    mIsFullPattern(false),
    mPatternLeafCount(0),
    mArcLabel(Label {}),
    mLabel(Label {})
  {};
  
  // this one makes nodes in the process of building the tree
  SuffixTreeNode(SuffixTreeNode* parent,
                 Label arcLabel,
                 bool isFullPattern) :
    mParent(parent),
    mChildren(std::map<Label,SuffixTreeNode*> {}),
    mSuffixLink(nullptr),
    mIsFullPattern(isFullPattern),
    mPatternLeafCount(0),
    mArcLabel(arcLabel)
  {
    // build the label of the node from the label of the parent and the arc label
    mLabel.insert(mLabel.begin(),parent->label().begin(),parent->label().end());
    mLabel.insert(mLabel.end(),arcLabel.begin(),arcLabel.end());
    parent->addChild(this);
  }

  static SuffixTreeNode* buildRoot();

  SuffixTreeNode* parent() { return mParent; }
  SuffixTreeNode* suffixLink() { return mSuffixLink; }
  Label& arcLabel() { return mArcLabel; }
  Label& label() { return mLabel; }
  int patternLeafCount() { return mPatternLeafCount; }
  bool isFullPattern() { return mIsFullPattern; }
  
  std::map<Label,SuffixTreeNode*>::iterator childrenBegin() { return mChildren.begin(); }
  std::map<Label,SuffixTreeNode*>::iterator childrenEnd() { return mChildren.end(); }
  size_t numChildren() { return mChildren.size(); }
  
  void setParent(SuffixTreeNode* newParent) { mParent = newParent; }
  void setSuffixLink(SuffixTreeNode* newSuffixLink) { mSuffixLink = newSuffixLink; }
  
  void removeChild(const Label& child) { mChildren.erase(child); }
  void addChild(SuffixTreeNode* child) { mChildren.insert(std::make_pair(child->arcLabel(),child)); }

  void dropFromArcLabel(int toDrop) { mArcLabel.erase(mArcLabel.begin(), mArcLabel.begin()+toDrop); }

  void addToPatternLeafCount(bool doIncrement) { if (doIncrement) mPatternLeafCount++; }
  void setPatternLeafCount(int newPatternLeafCount) { mPatternLeafCount = newPatternLeafCount; }

  auto getChild(Label& s) -> SuffixTreeNode*
  {
    auto search = mChildren.find(s);
    if (search != mChildren.end())
      return nullptr;
    else
      return search->second;
  }
  
private:
  // parent of this node
  SuffixTreeNode* mParent;
  // children of this node.  keys are labels, values are nodes
  std::map<Label,SuffixTreeNode*> mChildren;
  // suffix link pointer
  SuffixTreeNode* mSuffixLink;

  // does this node correspond to a full pattern (i.e. not a suffix of one)?
  bool mIsFullPattern;
  // counts the number of pattern leaves that are descendents of this node
  int mPatternLeafCount;

  // a label of a node can be any suffix of the word w . n where n is the
  // (negative of) the index of the word w.  We use negatives here so they don't
  // collide with the nonnegative integers, which represent variables.
  // warning: the negative index is 1-based, not 0-based.
  Label mArcLabel;
  // this could be inductively recomputed each time, but I think it is
  // better just to store a copy of it in the data type
  Label mLabel;
};

// return value types for internal functions for SuffixTree
using ContractedLocusType = std::tuple<SuffixTreeNode*,
				       SuffixTreeNode*,
				       Label>;

using ExtendedLocusType = std::tuple<SuffixTreeNode*,
				     Label>;

using InsertWorkerType = std::tuple<SuffixTreeNode*,
				    SuffixTreeNode*,
				    SuffixTreeNode*>;

using LeavesType = std::tuple<SuffixTreeNode*,
			      int>;

using SubwordsType = std::tuple<Label,
				int,
				Label>;

class SuffixTree
{
private:
  FRIEND_TEST(WordTable, suffixtree1);
public:
  friend std::ostream& operator<<(std::ostream& o, SuffixTree& suffixTree);
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

  const Word operator[](int index) const;

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
  

  // this command returns true if word = alpha . v . beta for some v in the
  // word table, where if v = wordTable[index1], then alpha is not empty
  // and if v = wordTable[index2] then beta is not empty.  Otherwise, it returns false.
  auto isNontrivialSuperword(Word word, int index1, int index2) const -> bool;
  
  // given 'word', find all left overlaps with elements of the table.
  // A left overlap of 'alpha' and 'beta' is:
  //  a prefix of alpha is a suffix of beta.
  // i.e. alpha = a.b
  //      beta  = c.a (a,b,c are words)
  // returned Triple for this overlap:
  void leftOverlaps(std::vector<Triple>& newLeftOverlaps) const;

  // find (right) overlaps with most recent added word 'w'.
  // Note: Not sure this is possible in this implementation
  void rightOverlaps(std::vector<Triple>& newRightOverlaps) const; 

  // other public functions:
  int numPatterns() const { return mMonomials.size(); }

  // FM: the following are really just here so the tests will run, I would prefer
  // it if they were private.  Is there a way to make this work?
  
private:
  // the following are internal functions needed for the SuffixTree data
  // structure to work

  // This function splits the arc from f to its parent by inserting a
  // new internal node with arc label prefix, where prefix is a prefix
  // of f->arcLabel().  A pointer to the new node is returned.
  auto splitArc(SuffixTreeNode* f,
		const Label& prefix) -> SuffixTreeNode*;

  // s is a suffix not yet in the table.  This function finds the
  // locus of the longest prefix of s whose locus exists.  The search
  // starts at y, and moves down the tree according to the string s.
  // The return value is the contracted locus x and a node f which is
  // either a child of y sharing a prefix pre with s - y.label, or f
  // is nullTreeNode if no such child exists.
  auto contractedLocus(SuffixTreeNode* y,
		       const Label& s,
		       bool incrementLeafCount) -> ContractedLocusType;

  // For this function to work, there must be a path starting from x
  // with beta as a prefix (See e.g. Lemma 1 in Amir, et.al.)  This
  // function finds the locus of the shortest word that has beta as a
  // prefix.  It returns this locus, together with the prefix that
  // needs to be split (if necessary) if beta is empty, then simply
  // return (x,beta) since x is the extended locus
  auto extendedLocus(SuffixTreeNode* x,
		     const Label& beta) -> ExtendedLocusType;

  // Finds an arc from y to a child whose label shares a prefix with s
  // return a std::pair of nullptrs if no match is found, i.e. the empty
  // prefix is the only shared prefix with any child of y
  auto findMatch(SuffixTreeNode* y,
		 const Label& s) -> ExtendedLocusType;

  // Return the longest shared prefix of s and t as a copy
  auto sharedPrefix(const Label& s, const Label& t) -> Label;

  // Return all pattern leaves below v
  auto patternLeaves(SuffixTreeNode* v) -> std::vector<LeavesType>;
  auto patternLeavesWorker(SuffixTreeNode* v) -> std::vector<SuffixTreeNode*>;

  // return all leaves below v
  auto allLeaves(SuffixTreeNode* v) -> std::vector<LeavesType>;
  auto allLeavesWorker(SuffixTreeNode* v) -> std::vector<SuffixTreeNode*>;

  // functions for insert algorithm
  auto insert(const Label& s, std::vector<Triple>& rightOverlaps) -> size_t;
  auto insert(std::vector<Label>& ss, std::vector<Triple>& rightOverlaps) -> size_t;
  auto insertWorker(SuffixTreeNode* v,
		    const Label& s,
		    bool isFullPattern) -> InsertWorkerType;
  auto insertStepC(SuffixTreeNode* v,
		   SuffixTreeNode* x,
		   const Label& beta,
		   const Label& s,
		   bool isFullPattern) -> InsertWorkerType;
  auto insertStepD(SuffixTreeNode* y,
		   const Label& s,
		   bool isFullPattern) -> InsertWorkerType;

  // functions for subwords algorithm
  auto subwordsWorker(SuffixTreeNode* cLocus,
		      Label beta,
		      Label s) -> std::vector<SubwordsType>;
  auto subwordsStepC(SuffixTreeNode* x,
		     Label beta,
		     Label s) -> SubwordsType;
  auto subwordsStepD(SuffixTreeNode* y,
		     Label s) -> SubwordsType;
  
private:
  // root node of the tree
  SuffixTreeNode* mRoot;

  // we will copy the monomials into the data structure.  Even though
  // this is of type Label, these monomials will not have the word number
  // appended as a suffix when stored in this container.
  std::vector<Label> mMonomials;

  // this is where all the labels for the data structure will be housed
  // The suffix tree owns all the labels, so all words in the label
  // pool must be freed upon destruction.
  // May not need.
  // LabelPool mLabelPool;
};

std::ostream& operator<<(std::ostream& o, SuffixTree& suffixTree);
std::ostream& operator<<(std::ostream& o, SuffixTreeNode& suffixTreeNode);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
