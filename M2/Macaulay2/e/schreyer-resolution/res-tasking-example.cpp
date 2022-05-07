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

tbb::flow::graph G;
std::vector<std::vector<TBBNodePtr>> nodes; // nodes[lev][sldeg] is that particular node.

struct Node {
    int mLevel;
    int mSlantedDegree;
    //int firstDependency;
    //int lastDependency;
    // int computationStatus;  // make an atomic variable
    std::vector<int> mEdges;
    TBBNodePtr mFillMatrixNode;
    TBBNodePtr mReductionNode;
    TBBNodePtr mRankNode;
};

//std::vector<Node> ourNodes;
//std::vector<int> ourDependencies;

// perhaps better name: MinimalBettiGraph
class DependencyGraph
{
private:
   tbb::flow::graph mTBBGraph;
   std::vector<Node> mVertices;

   void topologicalSortWorker(int curVertex, std::vector<bool> &visited, std::stack<int> &result);

   TBBNodePtr createFillMatrixNode(int level, int slantedDegree);
   TBBNodePtr createReductionNode(int level, int slantedDegree);
   TBBNodePtr createRankNode(int level, int slantedDegree);

public:

   DependencyGraph() {};

   void addVertex(int level, int slantedDegree);

   Node& getVertex(int index) { return mVertices[index]; }

   int numVertices() { return mVertices.size(); }

   void addEdge(int source, int target);
   
   void topologicalSort(std::stack<int> &result);

   void print();

   void startComputation() { mVertices[0].mFillMatrixNode->try_put(tbb::flow::continue_msg()); }

   void waitForCompletion() { mTBBGraph.wait_for_all(); }
};

void DependencyGraph::addVertex(int level, int slantedDegree)
{
   auto fillMatrixNode = createFillMatrixNode(level,slantedDegree);
   auto reductionNode  = createReductionNode(level,slantedDegree);
   auto rankNode       = createRankNode(level,slantedDegree);
   mVertices.emplace_back(Node {level, slantedDegree, std::vector<int> {}, fillMatrixNode, reductionNode, rankNode});

   // add flows from the fill matrix stage to the reduction stage, and from reduction to rank
   // FM: do we have to add other edges for the reduction and rank computations, or will this suffice?
   tbb::flow::make_edge(* fillMatrixNode,* reductionNode);
   tbb::flow::make_edge(* reductionNode,* rankNode);
}

void DependencyGraph::addEdge(int source, int target)
{
   mVertices[source].mEdges.push_back(target);
   tbb::flow::make_edge(* mVertices[source].mFillMatrixNode, * mVertices[target].mFillMatrixNode);
}

TBBNodePtr DependencyGraph::createFillMatrixNode(int lev, int sldeg)
{
  return std::make_shared<TBBNode>(mTBBGraph,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
				  int sleepTime = uni(rng);
                                  std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
				  std::lock_guard<std::mutex> guard(myMutex);
                                  std::cout << "fill matrix node lev=" << lev << " sldeg="
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
                                  std::cout << "reduction node   lev=" << lev << " sldeg="
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
                                  std::cout << "rank node        lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg
					    << " sleep time=" << sleepTime << std::endl;
                                  return msg;
                                });
}

void DependencyGraph::topologicalSort(std::stack<int> &result)
{
   std::vector<bool> visited(mVertices.size(),false);
   
   for (int i = 0; i < mVertices.size(); ++i)
      if (!visited[i])
	 topologicalSortWorker(i,visited,result);
}

void DependencyGraph::topologicalSortWorker(int curVertex, std::vector<bool> &visited, std::stack<int> &result)
{
   visited[curVertex] = true;

   for (auto edgeTo = mVertices[curVertex].mEdges.cbegin(); edgeTo != mVertices[curVertex].mEdges.cend(); ++edgeTo)
      if (!visited[*edgeTo])
         topologicalSortWorker(*edgeTo, visited, result);

   result.push(curVertex);
}

void DependencyGraph::print()
{
  for (int i = 0; i < mVertices.size(); ++i)
  {
    std::cout << "Vertex : " << i << " (" << mVertices[i].mLevel << "," << mVertices[i].mSlantedDegree << ") edges=";
    for (auto edgeTo = mVertices[i].mEdges.cbegin(); edgeTo != mVertices[i].mEdges.cend(); ++edgeTo)
       std::cout << *edgeTo << " ";
    std::cout << std::endl;
  }
}

TBBNodePtr createNode(tbb::flow::graph& G, int lev, int sldeg)
{
  return std::make_shared<TBBNode>(G,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
				  int sleepTime = uni(rng);
                                  std::this_thread::sleep_for(std::chrono::milliseconds(sleepTime));;
				  std::lock_guard<std::mutex> guard(myMutex);
                                  std::cout << "computed lev=" << lev << " sldeg="
                                            << sldeg << " sum=" << lev + sldeg
					    << " sleep time=" << sleepTime << std::endl;
                                  return msg;
                                });
}

int getIndex(int lev, int sldeg, int nlevels, int nslanted_degrees)
{
   return lev + (sldeg * nlevels);
}

// return value is (lev, sldeg)
std::pair<int,int> getPair(int index, int nlevels, int nslanted_degrees)
{
   return std::make_pair(index % nlevels, index / nlevels);
}

void makeDependencyGraph(int nlevels, int nslanted_degrees)
{
  for (int lev = 0; lev < nlevels; ++lev) nodes.emplace_back(std::vector<TBBNodePtr>(nslanted_degrees));

  // Create the nodes
  for (int lev=0; lev<nlevels; ++lev)
    for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
      nodes[lev][sldeg] = createNode(G, lev, sldeg);

  // Add the edges
  for (int lev=0; lev<nlevels; ++lev)
    for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
      {
        if (lev > 0)
	  tbb::flow::make_edge(* nodes[lev-1][sldeg], * nodes[lev][sldeg]);
        if (sldeg > 0)
          tbb::flow::make_edge(* nodes[lev][sldeg-1], * nodes[lev][sldeg]);
      }
}

void makeDependencyGraph2(DependencyGraph &G, int nlevels, int nslanted_degrees)
{
  // Create the nodes
  for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
    for (int lev=0; lev<nlevels; ++lev)
      G.addVertex(lev,sldeg);

  // Add the edges
  for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
    for (int lev=0; lev<nlevels; ++lev)
      {
        if (lev > 0)
	  G.addEdge(getIndex(lev-1,sldeg,nlevels,nslanted_degrees),
                    getIndex(lev,sldeg,nlevels,nslanted_degrees));
        if (sldeg > 0)
          G.addEdge(getIndex(lev,sldeg-1,nlevels,nslanted_degrees),
                    getIndex(lev,sldeg,nlevels,nslanted_degrees));
      }
}

int main()
{
  DependencyGraph newG;

  std::cout << "Hi there, we have " << tbb::info::default_concurrency()
            <<" thread(s) available." << std::endl;
  const int nlevels = 4;
  const int nslanted_degrees = 8;
  makeDependencyGraph(nlevels,nslanted_degrees);
  makeDependencyGraph2(newG,nlevels,nslanted_degrees);

  // ensure graph created correctly
  newG.print();

  // topo sort test
  std::stack<int> topoSort;
  newG.topologicalSort(topoSort);
  while (!topoSort.empty())
  {
     auto topOfStack = topoSort.top();
     std::cout << "Computing node: lev=" << newG.getVertex(topOfStack).mLevel << " sldeg="
               << newG.getVertex(topOfStack).mSlantedDegree << " sum="
               << newG.getVertex(topOfStack).mLevel+newG.getVertex(topOfStack).mSlantedDegree << std::endl;
     topoSort.pop();
  }

  // Just TBB dependency graph
  std::cout << "Old TBB Graph check:" << std::endl;
  nodes[0][0]->try_put(tbb::flow::continue_msg());

  G.wait_for_all();

  // Dependency Graph with more data
  std::cout << "New TBB Graph check:" << std::endl;
  newG.startComputation();
  newG.waitForCompletion();
  
  return 0;
}
