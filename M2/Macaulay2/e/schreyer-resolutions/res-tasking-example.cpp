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

using Node = tbb::flow::continue_node<tbb::flow::continue_msg>;
using NodePtr = std::shared_ptr<Node>;

tbb::flow::graph G;
std::vector<std::vector<NodePtr>> nodes; // nodes[lev][sldeg] is that particular node.

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
