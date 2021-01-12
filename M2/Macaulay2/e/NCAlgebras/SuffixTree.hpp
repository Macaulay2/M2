#ifndef _suffix_tree_hpp_
#define _suffix_tree_hpp_

#include "NCAlgebras/Word.hpp"  // for Word

#include <iostream>  // for ostream
#include <map>       // for map, operator!=, __map_iterator, map<>::iterator
#include <tuple>     // for tuple
#include <utility>   // for pair, make_pair
#include <vector>    // for vector, vector<>::iterator, operator<

// used in return value for WordTable as well
using Overlap = std::tuple<int,int,int,bool>;

// data type of an arc/vertex label
using Label = std::vector<int>;

class SuffixTreeNode
{
public:
  friend std::ostream& operator<<(std::ostream& o, const SuffixTreeNode& suffixTreeNode);
  std::ostream& dump(std::ostream&, int depth, bool dumpChildren = true) const;

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
                 Word arcLabel,
                 bool isFullPattern) :
    mParent(parent),
    mChildren(std::map<Label,SuffixTreeNode*> {}),
    mSuffixLink(nullptr),
    mIsFullPattern(isFullPattern),
    mPatternLeafCount(0)
  {
    // copy the data in arcLabel to a label owned by the node
    for (auto i = 0; i < arcLabel.size(); ++i)
      {
        mArcLabel.push_back(arcLabel.begin()[i]);
      }

    // build the label of the node from the label of the parent and the arc label
    mLabel.insert(mLabel.begin(),parent->label().begin(),parent->label().end());
    mLabel.insert(mLabel.end(),mArcLabel.begin(),mArcLabel.end());
    parent->addChild(this);
  }

  static SuffixTreeNode* buildRoot();

  SuffixTreeNode* parent() { return mParent; }
  SuffixTreeNode* suffixLink() { return mSuffixLink; }
  Label& arcLabel() { return mArcLabel; }
  Label& label() { return mLabel; }
  int patternLeafCount() const { return mPatternLeafCount; }
  bool isFullPattern() const { return mIsFullPattern; }
  bool isLeaf() const { return (*(mLabel.end()-1) < 0); }
  
  std::map<Label,SuffixTreeNode*>::iterator childrenBegin() { return mChildren.begin(); }
  std::map<Label,SuffixTreeNode*>::iterator childrenEnd() { return mChildren.end(); }
  size_t numChildren() const { return mChildren.size(); }
  
  void setParent(SuffixTreeNode* newParent) { mParent = newParent; }
  void setSuffixLink(SuffixTreeNode* newSuffixLink) { mSuffixLink = newSuffixLink; }
  
  void removeChild(const Label& child) { mChildren.erase(child); }
  void addChild(SuffixTreeNode* child) { mChildren.insert(std::make_pair(child->arcLabel(),child)); }

  void dropFromArcLabel(int toDrop) { mArcLabel.erase(mArcLabel.begin(), mArcLabel.begin()+toDrop); }

  void addToPatternLeafCount(bool doIncrement) { if (doIncrement) mPatternLeafCount++; }
  void setPatternLeafCount(int newPatternLeafCount) { mPatternLeafCount = newPatternLeafCount; }

  int getPatternNumber() const
  {
    // this function should *only* be called on leaves
    // since internal nodes may correspond to multiple patterns
    // returns the 0 based index of the pattern corresponding to this leaf
    return -*(mLabel.end()-1)-1;
  }
  
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
				       Word>;

using ExtendedLocusType = std::tuple<SuffixTreeNode*,
				     Word>;

using InsertType = std::tuple<SuffixTreeNode*,
                              SuffixTreeNode*,
                              SuffixTreeNode*>;

using SubwordsType = std::tuple<SuffixTreeNode*,
                                Word,
                                SuffixTreeNode*,
                                bool>;

// other than the monomial list, all functions should work on Word(s) not Label(s)
// to avoid unnecessary copies

class SuffixTree
{
private:
  //FRIEND_TEST(SuffixTree, suffixtree1);
public:
  friend std::ostream& operator<<(std::ostream& o, const SuffixTree& suffixTree);
  friend void outputPatterns(std::ostream& o, const SuffixTree& suffixTree);
  // these functions are also in WordTable; we would like to keep the
  // basic interface of both classes the same

  // need to create the root
  SuffixTree();

  // the SuffixTree owns all Labels and SuffixTreeNodes created within,
  // as well as the monomials inserted (stored as labels).
  ~SuffixTree();

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(const Word& w);

  auto insert(const Word& s, std::vector<Overlap>& newRightOverlaps) -> size_t;
  //size_t insert(Word w, std::vector<Overlap>& newRightOverlaps);

  const Word operator[](int index) const;

  void clear() {
    destroyChildren(mRoot);  //also deletes mRoot
    SuffixTreeNode* root = new SuffixTreeNode();
    mRoot = root;
    mMonomials.clear();
  }


  // lookup routines

  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  bool subwords(const Word& word,
                std::vector<std::pair<int,int>>& output) const;

  // sets 'output' to the first pair (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in word
  //   such that w appears in word starting at position j.
  // if such a match is found, output is set, and true is returned.
  // if not, false is returned.
  bool subword(const Word& word,
                std::pair<int,int>& output) const;
  
  // return all pairs (i,j), where
  //   the i-th word in the table is w (say)
  //   j is a position in w
  //   such that word appears in w starting at position j.
  bool superwords(const Word& word,
                  std::vector<std::pair<int,int>>& output) const;
  

  // this command returns true if word = alpha . v . beta for some v in the
  // word table, where if v = wordTable[index1], then alpha is not empty
  // and if v = wordTable[index2] then beta is not empty.  Otherwise, it returns false.
  auto isNontrivialSuperword(const Word& word, int index1, int index2) const -> bool;
  
  // given 'word', find all left overlaps with elements of the table.
  // A left overlap of 'alpha' and 'beta' is:
  //  a prefix of alpha is a suffix of beta.
  // i.e. alpha = a.b
  //      beta  = c.a (a,b,c are words)
  // returned Triple for this overlap:
  void leftOverlaps(std::vector<Overlap>& output) const;

  // find (right) overlaps with most recent added word 'w'.
  // Note: Not sure this is possible in this implementation
  // void rightOverlaps(std::vector<Overlap>& newRightOverlaps) const; 

  // other public functions:
  int numPatterns() const { return mMonomials.size(); }

  // FM: the following are really just here so the tests will run, I would prefer
  // it if they were private.  Is there a way to make this work?
  
private:
public: // TODO: fix so we can test these private functions (right now, we just make them public)
  // the following are internal functions needed for the SuffixTree data
  // structure to work

  // This function splits the arc from f to its parent by inserting a
  // new internal node with arc label prefix, where prefix is a prefix
  // of f->arcLabel().  A pointer to the new node is returned.
  auto splitArc(SuffixTreeNode* f,
		const Word& prefix) -> SuffixTreeNode*;

  // s is a suffix not yet in the table.  This function finds the
  // locus of the longest prefix of s whose locus exists.  The search
  // starts at y, and moves down the tree according to the string s.
  // The return value is the contracted locus x and a node f which is
  // either a child of y sharing a prefix pre with s - y.label, or f
  // is nullTreeNode if no such child exists.
  auto contractedLocus(SuffixTreeNode* y,
		       const Word& s,
		       bool incrementLeafCount = false) const -> ContractedLocusType;

  // For this function to work, there must be a path starting from x
  // with beta as a prefix (See e.g. Lemma 1 in Amir, et.al.)  This
  // function finds the locus of the shortest word that has beta as a
  // prefix.  It returns this locus, together with the prefix that
  // needs to be split (if necessary) if beta is empty, then simply
  // return (x,beta) since x is the extended locus
  auto extendedLocus(SuffixTreeNode* x,
		     const Word& beta) const -> ExtendedLocusType;

  // Finds an arc from y to a child whose label shares a prefix with s
  // return a std::pair of nullptrs if no match is found, i.e. the empty
  // prefix is the only shared prefix with any child of y
  auto findMatch(SuffixTreeNode* y,
		 const Word& s) const -> ExtendedLocusType;

  // Return the longest shared prefix of s and t as a copy
  // this doesn't really belong here...
  auto sharedPrefix(const Word s, const Word t) const -> Word;

  // Return all pattern leaves below v
  auto patternLeaves(SuffixTreeNode* v, std::vector<int>& output) const -> void;
  auto patternLeavesWorker(SuffixTreeNode* v) const -> std::vector<SuffixTreeNode*>;

  // return all leaves below v
  auto allLeaves(SuffixTreeNode* v, std::vector<SuffixTreeNode*>& output) const -> void;

  // functions for insert algorithm

  // these functions are for debugging purposes.
  auto insert(Label& lab, std::vector<Overlap>& rightOverlaps) -> size_t;
  auto insert(std::vector<Label>& labs, std::vector<Overlap>& rightOverlaps) -> size_t;
  
  auto insert(std::vector<Word>& ss, std::vector<Overlap>& rightOverlaps) -> size_t;
  auto insertWorker(SuffixTreeNode* v,
		    const Word& s,
		    bool isFullPattern) -> InsertType;
  auto insertStepC(SuffixTreeNode* v,
		   SuffixTreeNode* x,
		   const Word& beta,
		   const Word& s,
		   bool isFullPattern) -> InsertType;
  auto insertStepD(SuffixTreeNode* y,
		   const Word& s,
		   bool isFullPattern) -> InsertType;

  // functions for subwords algorithm
  // the output is a list of pairs (i,j) such that monomial i appears in position j
  // of the word w.
  //auto subword(const Word& w, std::pair<int,int>& output) const -> bool;
  //auto subwords(const Word& w, std::vector<std::pair<int,int>>& output) const -> bool;
  auto subwords(const Word& w, std::vector<std::pair<int,int>>& output,bool onlyFirst) const -> bool;
  auto subwordsStepC(SuffixTreeNode* x,
		     const Word& beta,
		     const Word& s) const -> SubwordsType;
  auto subwordsStepD(SuffixTreeNode* y,
		     const Word& s) const -> SubwordsType;

  // the output is a list of pairs (i,j) such that the word w appears in monomial i in position j
  auto superword(const Word& w, std::pair<int,int>& output) const -> bool;
  //auto superwords(const Word& w, std::vector<std::pair<int,int>>& output) const -> bool;
  auto superwords(const Word& w, std::vector<std::pair<int,int>>& output,bool onlyFirst) const -> bool;

  // the output is a list of (i,j) giving the monomial and position of the overlap
  // in the second argument
  auto leftOverlaps(const Word& w,
                    std::vector<std::pair<int,int>>& output,
                    bool avoidLast = false) const -> void;
  
private:
public: // TODO: remove this public.  These are private...  Here for testing
  // root node of the tree
  SuffixTreeNode* mRoot;

  // we will copy the monomials into the data structure.  Even though
  // this is of type Label, these monomials will not have the word number
  // appended as a suffix when stored in this container.
  std::vector<Label> mMonomials;

  void destroyChildren(SuffixTreeNode* node) const;

};

// some utility/debugging functions
void outputPatterns(std::ostream& o, const SuffixTree& suffixTree);
std::ostream& operator<<(std::ostream& o, const SuffixTree& suffixTree);
std::ostream& operator<<(std::ostream& o, const SuffixTreeNode& suffixTreeNode);
void outputLabel(std::ostream& o, const Label& vec);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
