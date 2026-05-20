#ifndef _res_dep_graph_hpp_
#define _res_dep_graph_hpp_

/**
 * @file schreyer-resolution/res-dep-graph.hpp
 * @brief `DependencyGraph` --- TBB flow-graph over (level, slanted-degree) cells of a Schreyer-frame resolution.
 *
 * Declares the parallel scheduler the F4 resolution driver uses
 * to run multiple homological degrees concurrently while
 * respecting the fill-matrix dependency that cell `(level,
 * slanted_degree)` requires both `(level - 1, slanted_degree)`
 * and `(level, slanted_degree - 1)` to have completed first; the
 * minimal-Betti pass adds an extra edge from `(level + 1,
 * slanted_degree - 1)`'s rank node into the current cell's
 * Betti node. Each cell is a `Node` carrying a
 * `mFillAndReduceNode`, `mRankNode`, and optional
 * `mMinimalBettiNode`, all `tbb::flow::continue_node<continue_msg>`
 * so a node fires once all its predecessors have. `addVertex`
 * / `addFillMatrixEdge` / `addMinimalBettiEdge` wire the
 * graph, `startComputation` triggers the root (`mVertices[0]`'s
 * fill-and-reduce node, which is assumed to be the top of the
 * dependency tree), and `waitForCompletion` blocks on the whole
 * frontier. The free function
 * `makeDependencyGraph(G, nlevels, nslanted_degrees,
 * doMinimalBetti)` constructs the standard layout.
 *
 * The 2-D grid is linearised via `getIndex(lev, sldeg, nlevels,
 * nslanted_degrees) = lev + sldeg * nlevels`, with the matching
 * inverse `getPair`. The whole header is `#ifdef WITH_TBB`-
 * gated; on builds without TBB the parallel layer compiles to
 * nothing and `SchreyerFrame::minimalBettiNumbers` falls back to
 * its serial `computeRanks` path. The `parallelizeByDegree`
 * flag plumbed through `F4ResComputation` is a single on/off
 * switch --- when true, `SchreyerFrame::minimalBettiNumbers`
 * builds and triggers this graph; when false, it skips the dep
 * graph entirely and runs the serial path. The graph also
 * carries a `topologicalSort` walker (used for debug printing
 * via `print()`) and a `std::mutex mMutex` for the
 * single-vertex edits done off the TBB thread pool.
 *
 * @see res-f4-computation.hpp
 * @see res-schreyer-frame.hpp
 * @see m2tbb.hpp
 */

#include "m2tbb.hpp"

#ifdef WITH_TBB

#include <iostream>
#include <vector>
#include <memory>
#include <mutex>
#include <stack>

using TBBNode = tbb::flow::continue_node<tbb::flow::continue_msg>;
using TBBNodePtr = std::shared_ptr<TBBNode>;

class SchreyerFrame;
class F4Res;

inline int getIndex(int lev, int sldeg, int nlevels, int nslanted_degrees)
{
  (void) nslanted_degrees;
  return lev + (sldeg * nlevels);
}

// return value is (lev, sldeg)
inline std::pair<int,int> getPair(int index, int nlevels, int nslanted_degrees)
{
  (void) nslanted_degrees;
  return std::make_pair(index % nlevels, index / nlevels);
}

struct Node {
    int mLevel;
    int mSlantedDegree;
    std::vector<int> mEdges;
    TBBNodePtr mFillAndReduceNode;
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

   TBBNodePtr createFillAndReduceNode(int level, int slantedDegree);
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
   void startComputation() { mVertices[0].mFillAndReduceNode->try_put(tbb::flow::continue_msg()); }

   void waitForCompletion() { mTBBGraph.wait_for_all(); }
};

void makeDependencyGraph(DependencyGraph &G, int nlevels, int nslanted_degrees, bool doMinimalBetti);

#endif // WITH_TBB

#endif // file guard
