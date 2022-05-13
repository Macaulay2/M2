#ifndef _res_dep_graph_hpp_
#define _res_dep_graph_hpp_

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

class SchreyerFrame;

inline int getIndex(int lev, int sldeg, int nlevels, int nslanted_degrees)
{
   return lev + (sldeg * nlevels);
}

// return value is (lev, sldeg)
inline std::pair<int,int> getPair(int index, int nlevels, int nslanted_degrees)
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
   std::mutex mMutex;

   SchreyerFrame* mFrame;   

   void topologicalSortWorker(int curVertex, std::vector<bool> &visited, std::stack<int> &result) const;

   TBBNodePtr createFillMatrixNode(int level, int slantedDegree);
   TBBNodePtr createReductionNode(int level, int slantedDegree);
   TBBNodePtr createRankNode(int level, int slantedDegree);
   TBBNodePtr createMinimalBettiNode(int level, int slantedDegree);

public:

   DependencyGraph() : mFrame(nullptr) {};
   DependencyGraph(SchreyerFrame *framePtr) : mFrame(framePtr) {};

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

void makeDependencyGraph(DependencyGraph &G, int nlevels, int nslanted_degrees);

#endif
