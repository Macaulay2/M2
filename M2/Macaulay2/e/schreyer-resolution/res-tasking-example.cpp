// run this with
// clang++ -I`brew --prefix tbb@2021`/include -L`brew --prefix tbb@2021`/lib --std=c++17 -ltbb res-tasking-example.cpp -g -o example

/**
 * @file schreyer-resolution/res-tasking-example.cpp
 * @brief Standalone TBB `flow::graph` sandbox simulating the F4 resolution's `(level, degree)` task DAG.
 *
 * A self-contained test program (not part of the engine build)
 * that exercises Intel TBB's `flow::continue_node` primitives
 * in exactly the dependency pattern the production resolution
 * uses: `nodes[lev][sldeg]` is a per-cell task with predecessors
 * `nodes[lev-1][sldeg]` and `nodes[lev][sldeg-1]`, and each task
 * simulates work by sleeping a uniform random duration before
 * signalling its dependents. The leading comment carries the
 * literal Homebrew-pinned compile recipe (`brew --prefix
 * tbb@2021`) so a developer can rebuild and iterate the
 * threading harness without touching CMake.
 *
 * Lives in-tree as documentation of the intended task-graph
 * shape and as a regression-isolation harness: if the real
 * `res-dep-graph.hpp` scheduler misbehaves, this sandbox
 * verifies "is TBB itself doing the right thing?" without
 * pulling in any engine state. New contributors can use it as
 * a minimal reference for the parallel resolution design.
 *
 * @see res-dep-graph.hpp
 * @see res-schreyer-frame.hpp
 * @see m2tbb.hpp
 */

#include <tbb/tbb.h>
#include <iostream>
#include <vector>
#include <memory>
#include <unistd.h>
#include <mutex>
#include <thread>
#include <chrono>
#include <random>

using Node = tbb::flow::continue_node<tbb::flow::continue_msg>;
using NodePtr = std::shared_ptr<Node>;

tbb::flow::graph G;
std::vector<std::vector<NodePtr>> nodes; // nodes[lev][sldeg] is that particular node.

/**
 * @brief Scratch task descriptor used by the standalone TBB
 * dependency-graph example.
 *
 * @details Pairs a `(level, slanted_degree)` cell with the half-open range
 * of cells it depends on (`firstDependency` .. `lastDependency`)
 * plus a `computationStatus` slot the example sets when the cell
 * fires. The file is illustrative scaffolding for the parallel
 * Schreyer-resolution scheduler, not part of the live engine
 * code path.
 */
struct OurNode {
   int lev;
   int sldeg;
   int firstDependency;
   int lastDependency;
   int computationStatus;
};

//std::vector<Node> ourNodes;
//std::vector<int> ourDependencies;

// just for syncing the output in the nodes
std::mutex myMutex;

// randomizer
std::random_device rd;
std::mt19937 rng(rd());
std::uniform_int_distribution<int> uni(250,1000);

NodePtr createNode(tbb::flow::graph& G, int lev, int sldeg)
{
  return std::make_shared<Node>(G,
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

void makeDependencyGraph(int nlevels, int nslanted_degrees)
{
  for (int lev = 0; lev < nlevels; ++lev) nodes.emplace_back(std::vector<NodePtr>(nslanted_degrees));

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

int main()
{
  std::cout << "Hi there, we have " << tbb::info::default_concurrency()
            <<" thread(s) available." << std::endl;
  const int nlevels = 5;
  const int nslanted_degrees = 10;
  makeDependencyGraph(nlevels,nslanted_degrees);

  nodes[0][0]->try_put(tbb::flow::continue_msg());

  G.wait_for_all();
  
  for (int lev=0; lev<nlevels; ++lev)
    for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
      std::cout << "nodes[" << lev << "," << sldeg << "] = " << nodes[lev][sldeg] << std::endl;

  return 0;
}
