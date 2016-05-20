// TODO
//   gtest
//   link in memtailor and mathic
//   build in separate directory?  cmake?
//   way to read in char, and variables, and monordering, ...
//   way to read in GB
//   top level logic to run the free res
//   rank
//   ** usage of CoefficientRingZZp is not complete: in particular, we need to create the tables...
//   link in ffpack, maybe flint?

#include "stdinc.hpp"
#include "betti.hpp"
#include "memblock.hpp"
#include "moninfo.hpp"
#include "monhashtable.hpp"
#include "f4-monlookup.hpp"
#include "res-f4-mem.hpp"
#include "aring-zzp.hpp"
#include "res-gausser.hpp"
#include "res-poly-ring.hpp"
#include "res-schreyer-frame.hpp"

#include "memtailor.h"
#include "mathic.h"

#include <iostream>
#include <iomanip>
#include <fstream>
memt::BufferPool testBuffer(16);

int M2_gbTrace = 0;

ResPolyRing* readRing(std::istream& infile)
{
  // Hack job.
  // Input format:
  //   <characteristic>
  //   <n:#variables>
  //   <weight_0> <weight_1> ... <weight_(n-1)>
  //   grevlex | lex | ...
  int charac;
  int nvars;
  infile >> charac;
  infile >> nvars;
  std::vector<int> wts(nvars);
  for (int i=0; i<nvars; i++)
    {
      int a;
      infile >> a;
      wts.push_back(a);
    }
  MonomialOrdering mo(nvars, wts);
  MonomialInfo M(mo);
  M2::ARingZZp kk(101);
  ResGausser G(kk);

  ResPolyRing* R = new ResPolyRing(G,M);
  return R;
}

void loadSchreyerFrame(std::istream& infile, SchreyerFrame& S)
{
  // reads polynomials in and sets up levels 0, 1 of the frame.
  // Assumption: i contains data in the following format
  //   (NOTE: this will be changed to be more efficient, if needed!)
}

// Format for reading.
int main(int argc, char** argv)
{
  std::ifstream infile(argv[1]);
  ResPolyRing* R = readRing(infile);
  SchreyerFrame S(R);
  loadSchreyerFrame(infile, S);
  infile.close();
#if 0  


  int v;
  for (int i=0; i<50; i++)
    for (int j=0; j<50; j++)
      kk.add(v,i,j);




  ResPolyRing R(G, M);
  // TODO: need degrees of variables...
  // Possibly need different term orders
  
  // read Groebner basis
  //   
  FreeModule F(R, ncomps, degrees);
  std::vector<poly> GB;
  // read in GB somehow, or should the SchreyerResolution provide
  // a set of routines to read in the GB?

  C.startGB(nelems);
  C.startGenerator(nterms);
  C.term(intcoeff, nparts);
  C.exponent(var,exp);
  C.endGB();
  
  SchreyerResolution C(R, GB);
  // set Schreyer frame #0
  // set Schreyer frame #1
  // compute the rest of the frame

  // compute the maps in the res, possibly parallel.
  
  // compute the ranks of the matrices, obtaining minimal betti numbers.
  // WARNING: need to worry about entire degree 0 complex?
#endif
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
