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

  for (auto pair : mChildren)
    {
      pair.second->dump(o,depth+1);
    }
  return o;
}

std::ostream& operator<<(std::ostream& o, SuffixTree& suffixTree)
{
  o << "Patterns in table: {";
  for (auto m : suffixTree.mMonomials)
    {
      o << m << ",";
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

auto SuffixTree::insertWorker(SuffixTreeNode* v,
			      Label s,
			      bool isFullPattern) -> InsertWorkerType
{
  if (v == mRoot) return insertStepD(v,s,isFullPattern);
  if (v->parent() == mRoot) return insertStepC(v,mRoot,prefix(v->arcLabel(),1),s,isFullPattern);
  return insertStepC(v,v->parent()->suffixLink(),v->arcLabel(),s,isFullPattern);
}
auto SuffixTree::insertStepC(SuffixTreeNode* v,
			     SuffixTreeNode* x,
			     Label beta,
			     Label s,
			     bool isFullPattern) -> InsertWorkerType
{
  SuffixTreeNode* ret1 = nullptr;
  SuffixTreeNode* ret2 = nullptr;
  SuffixTreeNode* ret3 = nullptr;
  return std::make_tuple(ret1,ret2,ret3);
}
auto SuffixTree::insertStepD(SuffixTreeNode* y,
			     Label s,
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
  auto tmpLabel = Label {};

  // drop the letters from s along the path traversed from y to std::get<0>(clType);
  tmpLabel = suffix(s, std::get<0>(clType)->label().size() - y->label().size());
  tmpNode = newy;
  if (f == nullptr)
    {
      // in this case, there is no common prefix of a label of any child of y and s
      // so just create a leaf immediately.
      auto v = new SuffixTreeNode(y,s,isFullPattern);
      if (tmpNode->label().size() != 0 && v->arcLabel().size() == 1)
	return std::make_tuple(y,y,v);
      else
	return std::make_tuple(y,nullptr,v);
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
    return std::make_tuple(p,nullptr,w);
  else
    return std::make_tuple(p,nullptr,w);
}

// This function splits the arc from f to its parent by inserting a
// new internal node with arc label prefix, where prefix is a prefix
// of f->arcLabel().  A pointer to the new node is returned.
auto SuffixTree::splitArc(SuffixTreeNode* f,
			  Label prefix) -> SuffixTreeNode*
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
				 Label s,
				 bool incrementLeafCount) -> ContractedLocusType
{
  y->addToPatternLeafCount(incrementLeafCount);
  auto match = findMatch(y,s);
  auto tmpNode = y;
  auto tmpLabel = Label {};
  while (std::get<0>(match) != nullptr && std::get<1>(match) == std::get<0>(match)->arcLabel())
    {
      tmpNode = std::get<0>(match);
      tmpLabel = suffix(s,std::get<1>(match).size());
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
			       Label beta) -> ExtendedLocusType
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
			   Label s) -> ExtendedLocusType
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

// Return the longest shared prefix of s and t.  A copy is made
auto SuffixTree::sharedPrefix(Label s, Label t) -> Label
{
  int i = 0;
  while (i < s.size() && i < t.size() && s[i] == t[i]) i++;
  return prefix(s,i);
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
