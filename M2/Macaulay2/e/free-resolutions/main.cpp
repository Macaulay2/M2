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
memt::BufferPool testBuffer(16);

int M2_gbTrace = 0;

// Format for reading.
int main(int argc, char** argv)
{
  // read ring info: #vars, degrees of vars, characteristic
  int nvars = 5;
  MonomialOrdering mo(nvars);
  MonomialInfo M(mo);
  M2::ARingZZp kk(101);
  ResGausser G(kk);
  ResPolyRing R(G, M);  

  SchreyerFrame S(R, nvars);

  int v;
  for (int i=0; i<50; i++)
    for (int j=0; j<50; j++)
      kk.add(v,i,j);
#if 0



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
