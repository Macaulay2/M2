#include "NCAlgebras/SuffixTree.hpp"
#include <algorithm>            // for copy
#include <iterator>             // for back_insert_iterator, back_inserter
#include "NCAlgebras/Word.hpp"  // for Word, operator<<

void outputLabel(std::ostream& o, const Label& vec)
{
  o << "[";
  for (auto i = vec.begin(); i != vec.end(); ++i)
    {
      if (i != vec.begin())
	o << ",";
      o << *i;
    }
  o << "]";
}

std::ostream& operator<<(std::ostream& o, const SuffixTreeNode& suffixTreeNode)
{
  suffixTreeNode.dump(o,0);
  return o;
}

std::ostream& SuffixTreeNode::dump(std::ostream& o, int depth, bool dumpChildren) const
{
  if (mParent == nullptr)
    {
      // the parent is nullptr if and only if it is the root.
      std::cout << "Root" << std::endl;
    }
  else
    {
      for (auto j = 0; j < depth; ++j) o << "  ";
      o << "Arclabel : ";
      outputLabel(o,mArcLabel);
      o << std::endl;
      
      for (auto j = 0; j < depth; ++j) o << "  ";
      o << "Label : ";
      outputLabel(o,mLabel);
      o << std::endl;
      
      for (auto j = 0; j < depth; ++j) o << "  ";
      o << "SuffixTreeLabel : ";
      if (mSuffixLink == nullptr)
        o << "nullptr";
      else
        outputLabel(o,mSuffixLink->mLabel);
      o << std::endl;
    }
  if (dumpChildren)
    {
      for (auto pair : mChildren)
        {
          pair.second->dump(o,depth+1);
        }
    }
  return o;
}

void outputPatterns(std::ostream& o, const SuffixTree& suffixTree)
{
  o << "Patterns in table: {";
  for (auto i = suffixTree.mMonomials.begin(); i != suffixTree.mMonomials.end(); ++i)
    {
      if (i != suffixTree.mMonomials.begin())
        o << ",";
      o << Word(*i);
    }
  o << "}" << std::endl;  
}

std::ostream& operator<<(std::ostream& o, const SuffixTree& suffixTree)
{
  outputPatterns(o,suffixTree);
  suffixTree.mRoot->dump(o,0);
  return o;
}

Word prefix(const Word vec, int lengthOfPrefix)
{
  // this command sets the pointers to the initial segment of vec
  return Word(vec.begin(),vec.begin()+lengthOfPrefix);
}

Word suffix(const Word vec, int indexOfSuffix)
{
  // this command sets the pointers to the terminal segment of vec
  return Word(vec.begin()+indexOfSuffix,vec.end());
}

bool concatenatesTo(const Word a, const Word b, const Word c)
{
  if (a.size() + b.size() != c.size()) return false;
  Word tmpA(c.begin(),c.begin()+a.size());
  Word tmpB(c.begin()+a.size(),c.end());
  return (tmpA == a) && (tmpB == b);
}

bool isPatternPrefix(const Word a, const Word b, int startIndex)
{
  // here, a is a pattern label so we only compare the first a.size()-1 many entries
  // sometimes we know that the first startIndex many symbols will match, so we start
  // at the first possible non-match.
  std::cout << a << " " << b << " " << startIndex << std::endl;
  if (b.size() + 1 < a.size()) return false;
  Word tmpA = Word(a.begin()+startIndex,a.end()-1);
  Word tmpB = Word(b.begin()+startIndex, b.begin()+a.size()-1);
  return (tmpA == tmpB);
}

bool isPatternPrefix(const Word a, const Word b)
{
  return isPatternPrefix(a,b,0);
}

SuffixTree::SuffixTree()
{
  SuffixTreeNode* root = new SuffixTreeNode();
  mRoot = root;
}

void SuffixTree::destroyChildren(SuffixTreeNode* p) const
{
  for (auto i = p->childrenBegin(); i != p->childrenEnd(); ++i)
      destroyChildren(i->second);
  delete p;
}

SuffixTree::~SuffixTree()
{
  // must destroy the SuffixTreeNodes owned by this object
  destroyChildren(mRoot);
}

auto SuffixTree::insert(const Word& w) -> size_t
{
  std::vector<Overlap> tmpOverlaps;
  return insert(w,tmpOverlaps);
}

auto SuffixTree::insert(const Word& w, std::vector<Overlap>& rightOverlaps) -> size_t
{
  // warning: this function appends the results to the end of right overlaps.
  int wordNum = mMonomials.size();
  auto plList = std::vector<int> {};
  Label tempS(w.begin(),w.end());
  mMonomials.push_back(tempS);
  tempS.push_back(-(wordNum + 1));
  Word s(tempS);
  auto v = mRoot;
  bool isFullPattern = true;
  while (s.size() != 0) 
    {
      InsertType insertType;
      if (v == mRoot) insertType = insertStepD(v,s,isFullPattern);
      else if (v->parent() == mRoot) insertType = insertStepC(v,mRoot,suffix(Word(v->arcLabel()),1),s,isFullPattern);
      else insertType = insertStepC(v,v->parent()->suffixLink(),Word(v->arcLabel()),s,isFullPattern);
      auto newv = std::get<0>(insertType);
      auto roRoot = std::get<1>(insertType);
      auto newLocus = std::get<2>(insertType);
      v = newv;
      if (roRoot != nullptr)
        {
          plList.clear();
          patternLeaves(roRoot,plList);
          for(auto pl : plList)
            {
              auto ro0 = newLocus->getPatternNumber();
              auto ro1 = mMonomials[ro0].size() - roRoot->label().size();
              auto ro2 = pl;
              rightOverlaps.push_back(std::make_tuple(ro0,ro1,ro2,true));
            }
        }
      if (v != mRoot && s.size() == 2)
        {
          v->setSuffixLink(mRoot);
        }
      s = suffix(s,1);
      isFullPattern = false;
    }
  return mMonomials.size();
}

auto SuffixTree::insert(std::vector<Word>& ss, std::vector<Overlap>& rightOverlaps) -> size_t
{
  // note that I got a crash if I used for (auto s : ss) and then inserted s.
  // Here, s is a copy of the element in ss, not s itself, apparently?
  // in order for it to do the right thing, I think it must be auto& s : ss
  for (auto i = ss.begin(); i != ss.end(); ++i)
    {
      insert(*i,rightOverlaps);
    }
  return mMonomials.size();
}

auto SuffixTree::insert(Label& lab, std::vector<Overlap>& rightOverlaps) -> size_t
{
  Word tmpWord(lab);
  return insert(tmpWord, rightOverlaps);
}

auto SuffixTree::insert(std::vector<Label>& labs, std::vector<Overlap>& rightOverlaps) -> size_t
{
  std::vector<Word> tmpWords;
  for (auto i = labs.begin(); i != labs.end(); ++i)
    {
      tmpWords.push_back(Word(*i));
    }
  return insert(tmpWords, rightOverlaps);
}


// auto SuffixTree::insertWorker(SuffixTreeNode* v,
// 			      const Word& s,
// 			      bool isFullPattern) -> InsertType
// {
//   if (v == mRoot) return insertStepD(v,s,isFullPattern);
//   if (v->parent() == mRoot) return insertStepC(v,mRoot,suffix(v->arcLabel(),1),s,isFullPattern);
//   return insertStepC(v,v->parent()->suffixLink(),v->arcLabel(),s,isFullPattern);
// }

auto SuffixTree::insertStepC(SuffixTreeNode* v,
			     SuffixTreeNode* x,
			     const Word& beta,
			     const Word& s,
			     bool isFullPattern) -> InsertType
{
  // Carries out step C in the algorithm.  This amounts to computing (and building,
  // if necessary) the suffix link of v.  This function can call the Step D code
  // as well (see the algorithm in the paper by Amir, et.al.)
  auto iwType = extendedLocus(x, beta);
  auto f = std::get<0>(iwType);
  auto betaHat = std::get<1>(iwType);
  if (f->arcLabel().size() == betaHat.size())
    {
      // in this case, f is in fact the locus of beta
      v->setSuffixLink(f);
      return insertStepD(f,suffix(s,f->label().size()),isFullPattern);
    }
  // in this case, f is the extended locus of beta.  We need to split
  // the arc from f to its parent and insert a node d and a child
  // w which will be the locus of beta
  auto d = splitArc(f,betaHat);
  // update pattern leaf count of d
  d->setPatternLeafCount(f->patternLeafCount());
  d->addToPatternLeafCount(isFullPattern);
  d->addToPatternLeafCount(f->isFullPattern());
  // add in the locus of s
  auto w = new SuffixTreeNode(d,suffix(s,d->label().size()),isFullPattern);
  // set the suffix link of v to d
  v->setSuffixLink(d);
  if (w->label().size() == 1)
    return std::make_tuple(d,f,w);
  else
    return std::make_tuple(d,nullptr,w);
}

auto SuffixTree::insertStepD(SuffixTreeNode* y,
			     const Word& s,
			     bool isFullPattern) -> InsertType
{
  // Carries out step D in the algorithm.  This amounts to constructing
  // the locus of the head of s (which is not yet known before calling this function).
  // find the contracted locus of s, starting from the node y
  auto clType = contractedLocus(y,s,isFullPattern);
  auto newy = std::get<0>(clType);
  auto f = std::get<1>(clType);
  auto pre = std::get<2>(clType);
  auto tmpNode = y;
  auto tmpLabel = s;

  // drop the letters from s along the path traversed from y to std::get<0>(clType);
  tmpLabel = suffix(tmpLabel, newy->label().size() - tmpNode->label().size());
  tmpNode = newy;
  if (f == nullptr)
    {
      // in this case, there is no common prefix of a label of any child of y and s
      // so just create a leaf immediately.
      auto v = new SuffixTreeNode(tmpNode,tmpLabel,isFullPattern);
      if (tmpNode->label().size() != 0 && v->arcLabel().size() == 1)
	return std::make_tuple(tmpNode,tmpNode,v);
      else
	return std::make_tuple(tmpNode,nullptr,v);
    }
  // in this case, f is the extended locus of s.  We need to split the arc from y to f
  auto p = splitArc(f,pre);
  // update the patternLeafCount of p if necessary
  p->setPatternLeafCount(f->patternLeafCount());
  p->addToPatternLeafCount(isFullPattern);
  p->addToPatternLeafCount(f->isFullPattern());
  // drop common prefix
  tmpLabel = suffix(tmpLabel,pre.size());
  // create new leaf under p for s
  auto w = new SuffixTreeNode (p,tmpLabel,isFullPattern);
  // return overlap and head information
  if (tmpLabel.size() == 1)
    return std::make_tuple(p,p,w);
  else
    return std::make_tuple(p,nullptr,w);
}

// This function splits the arc from f to its parent by inserting a
// new internal node with arc label prefix, where prefix is a prefix
// of f->arcLabel().  A pointer to the new node is returned.
auto SuffixTree::splitArc(SuffixTreeNode* f,
			  const Word& prefix) -> SuffixTreeNode*
{
  auto p = f->parent();
  auto d = new SuffixTreeNode(p,prefix,false);
  p->removeChild(f->arcLabel());
  f->dropFromArcLabel(prefix.size());
  d->addChild(f);
  f->setParent(d);
  return d;
}

// s is a suffix not yet in the table.  This function finds the
// locus of the longest prefix of s whose locus exists.  The search
// starts at y, and moves down the tree according to the string s.
// The return value is the contracted locus x and a node f which is
// either a child of y sharing a prefix pre with s - y.label, or f
// is nullTreeNode if no such child exists.
auto SuffixTree::contractedLocus(SuffixTreeNode* y,
                                 const Word& s,
                                 bool incrementLeafCount) const -> ContractedLocusType
{
  // warning: This function is not *really* const, since adding to the pattern
  // leaf count (if incrementalLeafCount = true) could change the behavior of
  // some functions.
  y->addToPatternLeafCount(incrementLeafCount);
  auto tmpNode = y;
  auto tmpLabel = s;
  auto match = findMatch(tmpNode,tmpLabel);
  while (std::get<0>(match) != nullptr && std::get<1>(match) == Word(std::get<0>(match)->arcLabel()))
    {
      tmpNode = std::get<0>(match);
      tmpLabel = suffix(tmpLabel,std::get<1>(match).size());
      tmpNode->addToPatternLeafCount(incrementLeafCount);
      match = findMatch(tmpNode,tmpLabel);
    }
  return std::make_tuple(tmpNode,std::get<0>(match),std::get<1>(match));
}

// For this function to work, there must be a path starting from x
// with beta as a prefix (See e.g. Lemma 1 in Amir, et.al.)  This
// function finds the locus of the shortest word that has beta as a
// prefix.  It returns this locus, together with the prefix that
// needs to be split (if necessary) if beta is empty, then simply
// return (x,beta) since x is the extended locus
auto SuffixTree::extendedLocus(SuffixTreeNode* x,
			       const Word& beta) const -> ExtendedLocusType
{
  if (beta.size() == 0) return std::make_tuple(x,beta);
  auto tmpNode = x;
  auto betaHat = beta;
  auto f = std::get<0>(findMatch(tmpNode,betaHat));
  if (f == nullptr)
    {
      std::cout << "Oooops" << std::endl;
      std::cout << *tmpNode << std::endl;
      std::cout << betaHat << std::endl;
    }
  while (f->arcLabel().size() < betaHat.size())
    {
      tmpNode = f;
      betaHat = suffix(betaHat,f->arcLabel().size());
      f = std::get<0>(findMatch(tmpNode,betaHat));
    }
  return std::make_tuple(f, betaHat);
}

// Finds an arc from y to a child whose label shares a prefix with s
// return a std::pair of nullptrs if no match is found, i.e. the empty
// prefix is the only shared prefix with any child of y
auto SuffixTree::findMatch(SuffixTreeNode* y,
			   const Word& s) const -> ExtendedLocusType
{
  SuffixTreeNode* f = nullptr;
  auto pre = Word();
  for (auto i = y->childrenBegin(); i != y->childrenEnd(); ++i)
    {
      auto kv = *i;
      pre = sharedPrefix(Word(kv.first),s);
      if (pre.size() != 0)
	{
	  f = kv.second;
	  break;
	}
    }
  return std::make_pair(f,pre);
}

// Return all pattern leaves below v
// warning: output is placed in the second argument to the function.  The vector is *not*
// cleared beforehand.
auto SuffixTree::patternLeaves(SuffixTreeNode* v, std::vector<int>& output) const -> void
{
  if (v->numChildren() == 0) return;
  for(auto x : patternLeavesWorker(v))
    {
      output.push_back(x->getPatternNumber());
    }
  return;
}

auto SuffixTree::patternLeavesWorker(SuffixTreeNode* v) const -> std::vector<SuffixTreeNode*>
{
  // TODO: the workers are still making a copy and returning it
  // make change to allow for reference to be passed along.
  // The workers also return the pointer to the node rather than the
  // pattern number, as they probably should.
  auto retval = std::vector<SuffixTreeNode*> {};
  if (v->numChildren() == 0 && v->isFullPattern())
    {
      retval.push_back(v);
      return retval;
    }
  if ((v->numChildren() == 0 && !v->isFullPattern()) || v->patternLeafCount() == 0)
    {
      return retval;
    }
  for (auto i = v->childrenBegin(); i != v->childrenEnd(); ++i)
    {
      auto tmp = patternLeavesWorker(i->second);
      std::copy(tmp.begin(),tmp.end(),std::back_inserter(retval));
    }
  return retval;
}

// Return the longest shared prefix of s and t.
auto SuffixTree::sharedPrefix(const Word s, const Word t) const -> Word
{
  int i = 0;
  while (i < s.size() && i < t.size() && s.begin()[i] == t.begin()[i]) i++;
  return prefix(s,i);
}

// return all leaves below v, as pointers to their nodes in the second argument
// warning: the output vector is *not* cleared beforehand
auto SuffixTree::allLeaves(SuffixTreeNode* v, std::vector<SuffixTreeNode*>& output) const -> void
{
  if (v->numChildren() == 0)
    {
      output.push_back(v);
      return;
    }
  for (auto x = v->childrenBegin(); x != v->childrenEnd(); ++x)
    {
      allLeaves(x->second,output);
    }
  return;
}

// It is very important that the subword algorithm not perform any copies
// of a label during the search.

// functions for subwords algorithm
auto SuffixTree::subword(const Word& w, std::pair<int,int>& output) const -> bool
{
  auto tmp = std::vector<std::pair<int,int>> {};
  subwords(w,tmp,true);
  if (tmp.size() == 0) return false;
  else
    {
      std::cout << (*tmp.begin()).first << " " << (*tmp.begin()).second << std::endl;
      output = *(tmp.begin());
      return true;
    }
}

auto SuffixTree::subwords(const Word& w, std::vector<std::pair<int,int>>& output) const -> bool
{
  return subwords(w,output,false);
}
  
auto SuffixTree::subwords(const Word& w,
                          std::vector<std::pair<int,int>>& output,
                          bool onlyFirst) const -> bool
{
  // this command returns a pair (i,j) where word i in the table appears
  // in position j of word.
  auto cLocus = mRoot;
  auto beta = Word();
  Word tmpLabel = w;
  int pos = 0;
  bool retval = false;
  std::cout << *(this->mRoot) << std::endl;
  std::cout << w << std::endl;
  while (tmpLabel.size() != 0)
    {
      SubwordsType swType;
      if (cLocus == mRoot)
        swType = subwordsStepD(mRoot,tmpLabel);
      else
        swType = subwordsStepC(cLocus->suffixLink(),beta,tmpLabel);
      auto newcLocus = std::get<0>(swType);
      auto newbeta = std::get<1>(swType);
      auto leaf = std::get<2>(swType);
      auto wasPattern = std::get<3>(swType);
      //if (wasPattern && isPatternPrefix(leaf->label(),tmpLabel,newcLocus->label().size()+newbeta.size()))
      if (wasPattern && isPatternPrefix(Word(leaf->label()),tmpLabel))
        {
          std::cout << Word(leaf->label()) << " " << tmpLabel << std::endl << std::flush;
          auto tmp = std::make_pair(leaf->getPatternNumber(),pos);
          output.push_back(tmp);
          retval = true;
          if (onlyFirst) return true;
        }
      pos++;
      cLocus = newcLocus;
      beta = newbeta;
      tmpLabel = suffix(tmpLabel,1);
      std::cout << *cLocus << " " << tmpLabel << std::endl;
    }
  return retval;
}
  
auto SuffixTree::subwordsStepC(SuffixTreeNode* x,
                               const Word& beta,
                               const Word& s) const -> SubwordsType
{
  if (beta.size() == 0)
    return subwordsStepD(x,suffix(s,x->label().size()));
  auto elType = extendedLocus(x,beta);
  auto f = std::get<0>(elType);
  auto betaHat = std::get<1>(elType);
  if (f->arcLabel().size() == betaHat.size())
    return subwordsStepD(f,suffix(s,f->label().size()));
  else
    return std::make_tuple(f->parent(),betaHat,nullptr,false);
}

auto SuffixTree::subwordsStepD(SuffixTreeNode* y,
                               const Word& s) const -> SubwordsType
{
  auto clType = contractedLocus(y,s);
  auto newy = std::get<0>(clType);
  auto f = std::get<1>(clType);
  auto pre = std::get<2>(clType);
  if (f == nullptr)
    return std::make_tuple(newy,pre,nullptr,false);
  else
    return std::make_tuple(newy,pre,f,f->isFullPattern());
}

// the output is a list of pairs (i,j) such that the word w appears in monomial i in position j
auto SuffixTree::superword(const Word& w, std::pair<int,int>& output) const -> bool
{
  auto tmp = std::vector<std::pair<int,int>> {};
  superwords(w,tmp,true);
  if (tmp.size() == 0) return false;
  else
    {
      output = *(tmp.begin());
      return true;
    }
}

// the output is a list of pairs (i,j) such that the word w appears in monomial i in position j
auto SuffixTree::superwords(const Word& w, std::vector<std::pair<int,int>>& output) const -> bool
{
  return superwords(w,output,false);
}

// the output is a list of pairs (i,j) such that the word w appears in monomial i in position j
auto SuffixTree::superwords(const Word& w, std::vector<std::pair<int,int>>& output,bool onlyFirst) const -> bool
{
  auto clType = contractedLocus(mRoot,w);
  auto y = std::get<0>(clType);
  auto f = std::get<1>(clType);
  auto pre = std::get<2>(clType);

  if (!concatenatesTo(Word(y->label()),pre,w)) return false;
  else if (f == nullptr)
    {
      auto leaves = std::vector<SuffixTreeNode*> {};
      allLeaves(y,leaves);
      for (auto g : leaves)
        {
          output.push_back(std::make_pair(g->getPatternNumber(),mMonomials[g->getPatternNumber()].size() - g->label().size() + 1));
          if (onlyFirst) return true;
        }
    }
  else if (f->isLeaf() && f->label().size() == w.size() + 1)
    {
      output.push_back(std::make_pair(f->getPatternNumber(),mMonomials[f->getPatternNumber()].size() - f->label().size() + 1));
      return true;
    }
  return false;
}

// the output is a list of pairs (i,j) where monomial i overlaps properly with w in position j
// in other words, the proper prefixes of w that are also suffixes in the tree
auto SuffixTree::leftOverlaps(const Word& w,
                              std::vector<std::pair<int,int>>& output,
                              bool avoidLast) const -> void
{
  auto tmpNode = mRoot;
  auto tmpLabel = w;
  auto match = findMatch(tmpNode,tmpLabel);
  auto f = std::get<0>(match);
  auto pre = std::get<1>(match);
  int patternNum = -1;
  while (f != nullptr && pre == Word(f->arcLabel()))
    {
      // in this case, the prefix found matches the arc label, so we move down
      // the tree
      tmpNode = f;
      tmpLabel = suffix(tmpLabel,pre.size());
      // after we move down add all children of tmpNode that are suffix leaves to leftOverlaps
      // as long as tmpNode is not the root
      for (auto i = tmpNode->childrenBegin(); i != tmpNode->childrenEnd(); ++i)
        {
          if (i->second->isLeaf() && !i->second->isFullPattern())
            {
              patternNum = i->second->getPatternNumber();
              if (!(avoidLast && patternNum == mMonomials.size()-1))
                output.push_back(std::make_pair(patternNum,
                                                 mMonomials[patternNum].size()-tmpNode->label().size()));
            }
        }
      match = findMatch(tmpNode,tmpLabel);
      f = std::get<0>(match);
      pre = std::get<1>(match);
     }
  // At this point, if pre != {} then there is a common prefix to a child, but
  // the label is not a full match to s.  In this case, we add
  // the unique suffix leaf that shares a prefix to our list,
  // as long as it is not a full pattern
  if (pre.size() != 0 && !f->isFullPattern())
    {
      patternNum = f->getPatternNumber();
      if (!(avoidLast && patternNum == mMonomials.size()-1))
        output.push_back(std::make_pair(patternNum,mMonomials[patternNum].size()-pre.size()));
    }
  return;
}

/// Here begin the functions to make the new class swappable with the old WordTable

// size_t SuffixTree::insert(Word w)
// {
//   Label tmp(w.begin(),w.end());
//   auto tmp2 = std::vector<Overlap> {};
//   return insert(tmp,tmp2);
// }

const Word SuffixTree::operator[](int index) const
{
  Word tmp(&*mMonomials[index].begin(),&*mMonomials[index].end());
  return tmp;
}

// bool SuffixTree::subwords(const Word& word,
//                           std::vector<std::pair<int,int>>& output) const
// {
//   Label tmp(word.begin(),word.end());
//   return subwords(tmp,output);
// }

// bool SuffixTree::subword(const Word& word,
//                 std::pair<int,int>& output) const
// {
//   Label tmp(word.begin(),word.end());
//   return subword(tmp, output);
// }
  
// bool SuffixTree::superwords(const Word& word,
//                             std::vector<std::pair<int,int>>& output) const
// {
//   Label tmp(word.begin(),word.end());
//   return superwords(tmp,output);
// }

// this command returns true if word = alpha . v . beta for some v in the
// word table, where if v = wordTable[index1], then alpha is not empty
// and if v = wordTable[index2] then beta is not empty.  Otherwise, it returns false.
auto SuffixTree::isNontrivialSuperword(const Word& word, int index1, int index2) const -> bool
{
  //Label tmp(word.begin(),word.end());
  std::vector<std::pair<int,int>> subs {};
  subwords(word, subs);
  for (auto sw : subs)
    {
      auto index = std::get<0>(sw);
      auto pos = std::get<1>(sw);
      if (index != index1 && index != index2) return true;
      if (index != index1 && pos == 0) return true;
      if (index != index2 && pos == word.size() - mMonomials[index2].size()) return true;
    }
  return false;
}

// this function computes the left overlaps of the most recently inserted monomial
// with the rest of the tree, but not matching itself (by convention, that is considered a right overlap)
auto SuffixTree::leftOverlaps(std::vector<Overlap>& output) const -> void
{
  if (mMonomials.size() == 0) return;
  auto tmpLabel = *(mMonomials.end()-1);
  auto pairs = std::vector<std::pair<int,int>> {};
  leftOverlaps(Word(tmpLabel),pairs,true);
  for (auto p : pairs)
    {
      output.push_back(std::make_tuple(p.first, p.second, mMonomials.size()-1,true));
    }
  return;
}  

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
