// run this with
// clang++ -I`brew --prefix tbb@2021`/include -L`brew --prefix tbb@2021`/lib --std=c++17 -ltbb res-tasking-example.cpp -g -o example

#include <tbb/tbb.h>
#include <iostream>
#include <vector>
#include <memory>
#include <unistd.h>

using Node = tbb::flow::continue_node<tbb::flow::continue_msg>;
using NodePtr = std::shared_ptr<Node>;

tbb::flow::graph G;
std::vector<std::vector<NodePtr>> nodes; // nodes[lev][sldeg] is that particular node.

NodePtr createNode(tbb::flow::graph& G, int lev, int sldeg)
{
  return std::make_shared<Node>(G,
                                [lev, sldeg](const tbb::flow::continue_msg &msg)
                                {
                                  sleep(2);
                                  std::cout << "computed lev=" << lev << " sldeg=" << sldeg << std::endl;
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

  for (int lev=1; lev<nlevels; ++lev)
    for (int sldeg=0; sldeg < nslanted_degrees; ++sldeg)
      {
        tbb::flow::make_edge(* nodes[lev][sldeg], * nodes[lev-1][sldeg]);
        if (sldeg > 0)
          tbb::flow::make_edge(* nodes[lev][sldeg], * nodes[lev][sldeg-1]);
      }
}

int main()
{
  std::cout << "hi there, we have " << tbb::info::default_concurrency()  << " thread(s) available" << std::endl;
  makeDependencyGraph(5, 3);

  nodes[4][2]->try_put(tbb::flow::continue_msg());
  //  nodes[2][2]->try_put(tbb::flow::continue_msg());

  G.wait_for_all();
  
  for (int lev=0; lev<5; ++lev)
    for (int sldeg=0; sldeg < 3; ++sldeg)
      std::cout << "nodes[" << lev << "," << sldeg << "] = " << nodes[lev][sldeg] << std::endl;

  return 0;
}
