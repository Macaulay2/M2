// run this with
// clang++ -I`brew --prefix tbb@2021`/include -L`brew --prefix tbb@2021`/lib --std=c++17 -ltbb res-tasking-example.cpp -g -o example

#include <tbb/tbb.h>
#include <iostream>
#include <vector>
#include <memory>
#include <unistd.h>
#include <mutex>
#include <thread>
#include <chrono>
#include <random>
#include <stack>

using TBBNode = tbb::flow::continue_node<tbb::flow::continue_msg>;
using TBBNodePtr = std::shared_ptr<TBBNode>;

// just for syncing the output in the nodes
std::mutex myMutex;

// randomizer
std::random_device rd;
std::mt19937 rng(rd());
std::uniform_int_distribution<int> uni(250,1000);

int getIndex(int lev, int sldeg, int nlevels, int nslanted_degrees)
{
   return lev + (sldeg * nlevels);
}

// return value is (lev, sldeg)
std::pair<int,int> getPair(int index, int nlevels, int nslanted_degrees)
{
   return std::make_pair(index % nlevels, index / nlevels);
}

struct Node {
    int mLevel;
    int mSlantedDegree;
    std::vector<int> mEdges;
    TBBNodePtr mFillMatrixNode;
    TBBNodePtr mReductionNode;
    TBBNodePtr mRankNode;
    TBBNodePtr mMinimalBettiNode;
};

// perhaps better name: MinimalBettiGraph
class DependencyGraph
{
private:
   tbb::flow::graph mTBBGraph;
   std::vector<Node> mVertices;

   void topologicalSortWorker(int curVertex, std::vector<bool> &visited, std::stack<int> &result) const;

   TBBNodePtr createFillMatrixNode(int level, int slantedDegree);
   TBBNodePtr createReductionNode(int level, int slantedDegree);
   TBBNodePtr createRankNode(int level, int slantedDegree);
   TBBNodePtr createMinimalBettiNode(int level, int slantedDegree);

public:

   DependencyGraph() {};

   // return value is the index of the added vertex
   int addVertex(int level, int slantedDegree);

   const Node& getVertex(int index) const { return mVertices[index]; }

   int numVertices() const { return mVertices.size(); }

   void addFillMatrixEdge(int source, int target);
   void addMinimalBettiEdge(int level, int slantedDegree, int nLevels, int nSlantedDegrees);
   
   std::stack<int> topologicalSort() const;

   void print() const;

   // need to ensure that mVertices[0] will always be the 'top' of the dependency tree
   void startComputation() { mVertices[0].mFillMatrixNode->try_put(tbb::flow::continue_msg()); }

   void waitForCompletion() { mTBBGraph.wait_for_all(); }
};

int DependencyGraph::addVertex(int level, int slantedDegree)
{
   auto fillMatrixNode   = createFillMatrixNode(level,slantedDegree);
   auto reductionNode    = createReductionNode(level,slantedDegree);
   auto rankNode         = createRankNode(level,slantedDegree);
   auto minimalBettiNode = createMinimalBettiNode(level,slantedDegree);
   mVertices.emplace_back(Node {level, 
                                slantedDegree, 
				std::vector<int> {},
				fillMatrixNode,
				reductionNode,
				rankNode,
				minimalBettiNode});

   // add flows from the fill matrix stage to the reduction stage, and from reduction to rank
   // FM: do we have to add other edges for the reduction and rank computations, or will this suffice?
   tbb::flow::make_edge(* fillMatrixNode,* reductionNode);
   tbb::flow::make_edge(* reductionNode,* rankNode);

   return mVertices.size()-1;
}

void DependencyGraph::addFillMatrixEdge(int source, int target)
{
   mVertices[source].mEdges.push_back(target);
   tbb::flow::make_edge(* mVertices[source].mReductionNode, * mVertices[target].mFillMatrixNode);
}

void DependencyGraph::addMinimalBettiEdge(int level, int slantedDegree, int nLevels, int nSlantedDegrees)
{
   int target = getIndex(level,slantedDegree,nLevels,nSlantedDegrees);
   tbb::flow::make_edge(* mVertices[target].mRankNode, * mVertices[target].mMinimalBettiNode);
   
   int source = getIndex(level+1,slantedDegree-1,nLevels,nSlantedDegrees);
   if ((level + 1 < nLevels) && (slantedDegree > 0))
     tbb::flow::make_edge(* mVertices[source].mRankNode, * mVertices[target].mMinimalBettiNode);
}

TBBNodePtr DependencyGraph::createFillMatrixNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
				  int sleepTime = uni(rng);
                                  std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
				  std::lock_guard<std::mutex> guard(myMutex);
                                  std::cout << "fill matrix node    lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg
					    << " sleep time=" << sleepTime << std::endl;
                                  return msg;
                                });
}

TBBNodePtr DependencyGraph::createReductionNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
				  int sleepTime = uni(rng);
                                  std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
				  std::lock_guard<std::mutex> guard(myMutex);
                                  std::cout << "reduction node      lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg
					    << " sleep time=" << sleepTime << std::endl;
                                  return msg;
                                });
}

TBBNodePtr DependencyGraph::createRankNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
				  int sleepTime = uni(rng);
                                  std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
				  std::lock_guard<std::mutex> guard(myMutex);
                                  std::cout << "rank node           lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg
					    << " sleep time=" << sleepTime << std::endl;
                                  return msg;
                                });
}

TBBNodePtr DependencyGraph::createMinimalBettiNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
				  int sleepTime = uni(rng);
                                  std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
				  std::lock_guard<std::mutex> guard(myMutex);
                                  std::cout << "minimal betti node  lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg
					    << " sleep time=" << sleepTime << std::endl;
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

void makeDependencyGraph(DependencyGraph &G, int nlevels, int nslanted_degrees)
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
	if (lev < nlevels-1)
	  G.addMinimalBettiEdge(lev,sldeg,nlevels,nslanted_degrees);
      }
}

int main()
{
  DependencyGraph newG;

  std::cout << "Hi there, we have " << tbb::info::default_concurrency()
            <<" thread(s) available." << std::endl;
  const int nlevels = 4;
  const int nslanted_degrees = 8;
  makeDependencyGraph(newG,nlevels,nslanted_degrees);

  // ensure graph created correctly
  newG.print();

  // topo sort test
  std::stack<int> topoSort = newG.topologicalSort();
  while (!topoSort.empty())
  {
     auto topOfStack = topoSort.top();
     std::cout << "Computing node: lev=" << newG.getVertex(topOfStack).mLevel << " sldeg="
               << newG.getVertex(topOfStack).mSlantedDegree << " sum="
               << newG.getVertex(topOfStack).mLevel+newG.getVertex(topOfStack).mSlantedDegree << std::endl;
     topoSort.pop();
  }

  // Dependency Graph with more data
  std::cout << "New TBB Graph check:" << std::endl;
  newG.startComputation();
  newG.waitForCompletion();
  
  return 0;
}

// old tbb flow graph example code

// tbb::flow::graph G;
// std::vector<std::vector<TBBNodePtr>> nodes; // nodes[lev][sldeg] is that particular node.

// TBBNodePtr createNode(tbb::flow::graph& G, int lev, int sldeg)
// {
//   return std::make_shared<TBBNode>(G,
//                                 [lev, sldeg](const tbb::flow::continue_msg &msg)
//                                 {
// 				  int sleepTime = uni(rng);
//                                   std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
// 				  std::lock_guard<std::mutex> guard(myMutex);
//                                   std::cout << "computed lev=" << lev << " sldeg="
//                                             << sldeg << " sum=" << lev + sldeg
// 					    << " sleep time=" << sleepTime << std::endl;
//                                   return msg;
//                                 });
// }

// void makeDependencyGraph(int nlevels, int nslanted_degrees)
// {
//   for (int lev = 0; lev < nlevels; ++lev) nodes.emplace_back(std::vector<TBBNodePtr>(nslanted_degrees));

//   // Create the nodes
//   for (int lev=0; lev<nlevels; ++lev)
//     for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
//       nodes[lev][sldeg] = createNode(G, lev, sldeg);

//   // Add the edges
//   for (int lev=0; lev<nlevels; ++lev)
//     for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
//       {
//         if (lev > 0)
// 	  tbb::flow::make_edge(* nodes[lev-1][sldeg], * nodes[lev][sldeg]);
//         if (sldeg > 0)
//           tbb::flow::make_edge(* nodes[lev][sldeg-1], * nodes[lev][sldeg]);
//       }
// }

// makeDependencyGraph(nlevels,nslanted_degrees);

// Just TBB dependency graph
// std::cout << "Old TBB Graph check:" << std::endl;
// nodes[0][0]->try_put(tbb::flow::continue_msg());
// G.wait_for_all();

