#include "res-dep-graph.hpp"
#include "res-schreyer-frame.hpp"
#include "res-f4.hpp"

int DependencyGraph::addVertex(int level, int slantedDegree)
{
   auto fillAndReduceNode = createFillAndReduceNode(level,slantedDegree);
   auto rankNode          = createRankNode(level,slantedDegree);
   auto minimalBettiNode  = createMinimalBettiNode(level,slantedDegree);
   mVertices.emplace_back(Node {level, 
                                slantedDegree, 
				std::vector<int> {},
				fillAndReduceNode,
				rankNode,
				minimalBettiNode});

   // add flows from the fill matrix stage to the reduction stage, and from reduction to rank
   // FM: do we have to add other edges for the reduction and rank computations, or will this suffice?
   tbb::flow::make_edge(* fillAndReduceNode,* rankNode);

   return mVertices.size()-1;
}

void DependencyGraph::addFillMatrixEdge(int source, int target)
{
   mVertices[source].mEdges.push_back(target);
   tbb::flow::make_edge(* mVertices[source].mFillAndReduceNode, * mVertices[target].mFillAndReduceNode);
}

void DependencyGraph::addMinimalBettiEdge(int level, int slantedDegree, int nLevels, int nSlantedDegrees)
{
   int target = getIndex(level,slantedDegree,nLevels,nSlantedDegrees);
   tbb::flow::make_edge(* mVertices[target].mRankNode, * mVertices[target].mMinimalBettiNode);
   
   int source = getIndex(level+1,slantedDegree-1,nLevels,nSlantedDegrees);
   if ((level + 1 < nLevels) && (slantedDegree > 0))
     tbb::flow::make_edge(* mVertices[source].mRankNode, * mVertices[target].mMinimalBettiNode);
}

TBBNodePtr DependencyGraph::createFillAndReduceNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg, this](const tbb::flow::continue_msg &msg)
                                {
				  std::lock_guard<std::mutex> guard(mMutex);
				  int& status = mFrame->mComputationStatus.entry(sldeg,lev);
				  if (status != 1) return msg;

				  mFrame->mComputer->construct(lev,sldeg + lev);

                                  std::cout << "fill matrix node    lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg << std::endl;
				  
				  status = 2;
                                  return msg;
                                });
}

TBBNodePtr DependencyGraph::createRankNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg, this](const tbb::flow::continue_msg &msg)
                                {
				  std::lock_guard<std::mutex> guard(mMutex);
				  //int& status = mFrame->mComputationStatus.entry(sldeg,lev);
				  //if (status != 2) return msg;

                                  std::cout << "rank node           lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg << std::endl;

                                  //int rk = mFrame->rank(sldeg,lev);
                                  //if (rk > 0)
                                  //{
                                  //   mFrame->mBettiMinimal.entry(sldeg, lev) -= rk;
                                  //   if (sldeg <= mFrame->mHiSlantedDegree and lev > 0)
                                  //     mFrame->mBettiMinimal.entry(sldeg + 1, lev - 1) -= rk;
                                  //}
                                  //status = 3;
                                  return msg;
                                });
}

TBBNodePtr DependencyGraph::createMinimalBettiNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg, this](const tbb::flow::continue_msg &msg)
                                {
				  std::lock_guard<std::mutex> guard(mMutex);
                                  std::cout << "minimal betti node  lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg << std::endl;
                                  return msg;
                                });
}

std::stack<int> DependencyGraph::topologicalSort() const
{
   std::stack<int> result;

   std::vector<bool> visited(mVertices.size(),false);
   
   for (int i = 0; i < mVertices.size(); ++i)
      if (!visited[i])
	 topologicalSortWorker(i,visited,result);

   return result;
}

void DependencyGraph::topologicalSortWorker(int curVertex, std::vector<bool> &visited, std::stack<int> &result) const
{
   visited[curVertex] = true;

   for (auto edgeTo = mVertices[curVertex].mEdges.cbegin(); edgeTo != mVertices[curVertex].mEdges.cend(); ++edgeTo)
      if (!visited[*edgeTo])
         topologicalSortWorker(*edgeTo, visited, result);

   result.push(curVertex);
}

void DependencyGraph::print() const
{
  for (int i = 0; i < mVertices.size(); ++i)
  {
    std::cout << "Vertex : " << i << " (" << mVertices[i].mLevel << "," << mVertices[i].mSlantedDegree << ") edges=";
    for (auto edgeTo = mVertices[i].mEdges.cbegin(); edgeTo != mVertices[i].mEdges.cend(); ++edgeTo)
       std::cout << *edgeTo << " ";
    std::cout << std::endl;
  }
}

void makeDependencyGraph(DependencyGraph &G, int nlevels, int nslanted_degrees, bool doMinimalBetti)
{
  // Create the nodes
  for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
    for (int lev=0; lev<nlevels; ++lev)
    {
      int newIndex = G.addVertex(lev,sldeg);
      if (newIndex != getIndex(lev,sldeg,nlevels,nslanted_degrees))
        std::cout << "ERROR: Index of created vertex does not match expected index." << std::endl;
      if (G.getVertex(newIndex).mLevel != lev or G.getVertex(newIndex).mSlantedDegree != sldeg)
        std::cout << "ERROR: Level or Slanted Degree of Node not the expected value." << std::endl;
    }
      
  // Add the fill matrix edges
  for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
    for (int lev=0; lev<nlevels; ++lev)
      {
        if (lev > 0)
	  G.addFillMatrixEdge(getIndex(lev-1,sldeg,nlevels,nslanted_degrees),
                              getIndex(lev,sldeg,nlevels,nslanted_degrees));
        if (sldeg > 0)
          G.addFillMatrixEdge(getIndex(lev,sldeg-1,nlevels,nslanted_degrees),
                              getIndex(lev,sldeg,nlevels,nslanted_degrees));
	if ((lev < nlevels-1) && doMinimalBetti)
	  G.addMinimalBettiEdge(lev,sldeg,nlevels,nslanted_degrees);
      }
}
