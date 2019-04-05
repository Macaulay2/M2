#include "SuffixTree.hpp"

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

std::ostream& operator<<(std::ostream& o, SuffixTreeNode& suffixTreeNode)
{
  suffixTreeNode.dump(o,0);
  return o;
}

std::ostream& SuffixTreeNode::dump(std::ostream& o, int depth)
{
  if (mParent == nullptr)
    {
      // the parent is nullptr if and only if it is the root.
      std::cout << "Root" << std::endl;
    }
  else
    {
      for (auto j = 0; j < depth; ++j) o << "  ";
      o << "Label : ";
      outputLabel(o,mLabel);
      o << std::endl;
      
      for (auto j = 0; j < depth; ++j) o << "  ";
      o << "Arclabel : ";
      outputLabel(o,mArcLabel);
      o << std::endl;
      
      for (auto j = 0; j < depth; ++j) o << "  ";
      o << "SuffixTreeLabel : ";
      if (mSuffixLink == nullptr)
        o << "nullptr";
      else
        outputLabel(o,mSuffixLink->mLabel);
      o << std::endl;
    }
  
  for (auto pair : mChildren)
    {
      pair.second->dump(o,depth+1);
    }
  return o;
}

std::ostream& operator<<(std::ostream& o, SuffixTree& suffixTree)
{
  o << "Patterns in table: {";
  for (auto i = suffixTree.mMonomials.begin(); i != suffixTree.mMonomials.end(); ++i)
    {
      if (i != suffixTree.mMonomials.begin())
        o << ",";
      o << *i;
    }
  o << "}" << std::endl;
  suffixTree.mRoot->dump(o,0);
  return o;
}

Label prefix(const Label& vec, int lengthOfPrefix)
{
  // this command makes a copy of the prefix of f and returns it
  Label temp;
  std::copy(vec.begin(), vec.begin()+lengthOfPrefix, std::back_inserter(temp));
  return temp;
}

Label suffix(const Label &vec, int indexOfSuffix)
{
  // this command makes a copy of the suffix of f and returns it
  Label temp;
  std::copy(vec.begin()+indexOfSuffix, vec.end(), std::back_inserter(temp));
  return temp;
}

SuffixTree::SuffixTree()
{
  SuffixTreeNode* root = new SuffixTreeNode();
  mRoot = root;
}

auto SuffixTree::insert(const Label& w, std::vector<Triple>& rightOverlaps) -> size_t
{
  // warning: this function appends the results to the end of right overlaps.
  int wordNum = mMonomials.size();
  auto plList = std::vector<int> {};
  mMonomials.push_back(w);
  Label s = w;
  s.push_back(-(wordNum + 1));
  auto v = mRoot;
  bool isFullPattern = true;
  while (s.size() != 0) 
    {
      auto iwType = insertWorker(v,s,isFullPattern);
      auto newv = std::get<0>(iwType);
      auto roRoot = std::get<1>(iwType);
      auto newLocus = std::get<2>(iwType);
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
              rightOverlaps.push_back(std::make_tuple(ro0,ro1,ro2));
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

auto SuffixTree::insert(std::vector<Label>& ss, std::vector<Triple>& rightOverlaps) -> size_t
{
  for (auto s : ss)
    {
      insert(s,rightOverlaps);
    }
  return mMonomials.size();
}

auto SuffixTree::insertWorker(SuffixTreeNode* v,
			      const Label& s,
			      bool isFullPattern) -> InsertWorkerType
{
  if (v == mRoot) return insertStepD(v,s,isFullPattern);
  if (v->parent() == mRoot) return insertStepC(v,mRoot,suffix(v->arcLabel(),1),s,isFullPattern);
  return insertStepC(v,v->parent()->suffixLink(),v->arcLabel(),s,isFullPattern);
}
auto SuffixTree::insertStepC(SuffixTreeNode* v,
			     SuffixTreeNode* x,
			     const Label& beta,
			     const Label& s,
			     bool isFullPattern) -> InsertWorkerType
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
			     const Label& s,
			     bool isFullPattern) -> InsertWorkerType
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
			  const Label& prefix) -> SuffixTreeNode*
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
				 const Label& s,
				 bool incrementLeafCount) -> ContractedLocusType
{
  y->addToPatternLeafCount(incrementLeafCount);
  auto tmpNode = y;
  auto tmpLabel = s;
  auto match = findMatch(tmpNode,tmpLabel);
  while (std::get<0>(match) != nullptr && std::get<1>(match) == std::get<0>(match)->arcLabel())
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
			       const Label& beta) -> ExtendedLocusType
{
  if (beta.size() == 0) return std::make_tuple(x,beta);
  auto tmpNode = x;
  auto betaHat = beta;
  auto match = findMatch(tmpNode,betaHat);
  while (std::get<0>(match)->arcLabel().size() < betaHat.size())
    {
      tmpNode = std::get<0>(match);
      betaHat = suffix(betaHat,std::get<0>(match)->arcLabel().size());
      match = findMatch(tmpNode,betaHat);
    }
  return std::make_tuple(std::get<0>(match), betaHat);
}

// Finds an arc from y to a child whose label shares a prefix with s
// return a std::pair of nullptrs if no match is found, i.e. the empty
// prefix is the only shared prefix with any child of y
auto SuffixTree::findMatch(SuffixTreeNode* y,
			   const Label& s) -> ExtendedLocusType
{
  SuffixTreeNode* f = nullptr;
  auto pre = Label {};
  for (auto i = y->childrenBegin(); i != y->childrenEnd(); ++i)
    {
      auto kv = *i;
      pre = sharedPrefix(kv.first,s);
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
auto SuffixTree::patternLeaves(SuffixTreeNode* v, std::vector<int>& output) -> void
{
  if (v->numChildren() == 0) return;
  for(auto x : patternLeavesWorker(v))
    {
      output.push_back(x->getPatternNumber());
    }
  return;
}

auto SuffixTree::patternLeavesWorker(SuffixTreeNode* v) -> std::vector<SuffixTreeNode*>
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

// Return the longest shared prefix of s and t.  A copy is made
auto SuffixTree::sharedPrefix(const Label& s, const Label& t) -> Label
{
  int i = 0;
  while (i < s.size() && i < t.size() && s[i] == t[i]) i++;
  return prefix(s,i);
}

// return all leaves below v
// warning: output is placed in the second argument to the function.  The vector is *not*
// cleared beforehand.
auto SuffixTree::allLeaves(SuffixTreeNode* v, std::vector<int>& output) -> void
{
  if (v->numChildren() == 0)
    {
      output.push_back(v->getPatternNumber());
      return;
    }
  for (auto x : allLeavesWorker(v))
    {
      output.push_back(x->getPatternNumber());
    }
  return;
}

auto SuffixTree::allLeavesWorker(SuffixTreeNode* v) -> std::vector<SuffixTreeNode*>
{
  // TODO: the workers are still making a copy and returning it
  // make change to allow for reference to be passed along.
  auto retval = std::vector<SuffixTreeNode*> {};
  if (v->numChildren() == 0)
    {
      retval.push_back(v);
      return retval;
    }
  for (auto x = v->childrenBegin(); x != v->childrenEnd(); ++x)
    {
      auto tmp = allLeavesWorker(x->second);
      std::copy(tmp.begin(),tmp.end(),std::back_inserter(retval));
    }
  return retval;
}

// functions for subwords algorithm
auto SuffixTree::subword(const Label& w, std::pair<int,int>& output) const -> bool
{
  auto tmp = std::vector<std::pair<int,int>> {};
  subwords(w,tmp,true);
  if (tmp.size() == 0) return false;
  else
    {
      output = *(tmp.begin());
      return true;
    }
}

auto SuffixTree::subwords(const Label& w, std::vector<std::pair<int,int>>& output) const -> void
{
  subwords(w,output,false);
}
  
auto SuffixTree::subwords(const Label& w, std::vector<std::pair<int,int>>& output, bool onlyFirst) const -> void
{
  auto cLocus = mRoot;
  auto beta = Label {};
  auto tmpLabel = w;
  int pos = 0;
  // TODO: Still working here.
  while (tmpLabel.size() != 0)
    {
      auto swType = subwordsWorker(cLocus,beta,tmpLabel);
      auto newcLocus = std::get<0>(swType);
      auto newbeta = std::get<1>(swType);
      auto wasPattern = std::get<2>(swType);
      if (wasPattern)
        {
          if (onlyFirst)
            {
              Label tmp = newcLocus->label();
            }
        }
    }
}
  

auto SuffixTree::subwordsWorker(SuffixTreeNode* cLocus,
                                const Label& beta,
                                const Label& s) const -> SubwordsWorkerType
{
}

auto SuffixTree::subwordsStepC(SuffixTreeNode* x,
                               const Label& beta,
                               const Label& s) const -> SubwordsWorkerType
{
}

auto SuffixTree::subwordsStepD(SuffixTreeNode* y,
                               const Label& s) const -> SubwordsWorkerType
{
}

#if 0
auto LabelPool::prefix(Label &f, int lengthOfPrefix) -> Label*
{
  // this command makes a copy of the prefix of f, and inserts
  // it into the pool if it is not there already.
  // indexOfSuffix should be >0, otherwise insert should be used.
  Label temp;
  std::copy(f.begin(), f.begin()+lengthOfPrefix, temp.begin());
  return insert(temp);  
}

auto LabelPool::suffix(Label &f, int indexOfSuffix) -> Label*
{
  // this command makes a copy of the suffix of f, and inserts
  // it into the pool if it is not there already.
  // indexOfSuffix should be >0, otherwise insert should be used.
  Label temp;
  std::copy(f.begin()+indexOfSuffix, f.end(), temp.begin());
  return insert(temp);
}

auto LabelPool::insert(Label &f) -> Label*
{
  // returns a pointer to the item inserted/found.
  // we don't really care about insertRetval.second, which indicates
  // whether the insertion actually took place or not.
  auto insertRetval = mLabelPool.insert(f);
  return &*(insertRetval.first);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
