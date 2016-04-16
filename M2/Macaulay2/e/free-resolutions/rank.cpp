#include "stdinc.hpp"
#include "res-schreyer-frame.hpp"

#include <iostream>
int SchreyerFrame::rank(int slanted_degree, int lev)
{
  // As above, get the size of the matrix, and 'newcols'
  // Now we loop through the elements of degree 'slanted_degree + lev' at level 'lev'
  if (not (lev > 0 and lev <= maxLevel())
      and not (slanted_degree >= mLoSlantedDegree and slanted_degree < mHiSlantedDegree))
    {
      std::cerr << "ERROR: called rank(" << slanted_degree << "," << lev << ")" << std::endl;
      return 0;
    }
  M2_ASSERT(lev > 0 and lev <= maxLevel());
  M2_ASSERT(slanted_degree >= mLoSlantedDegree and slanted_degree < mHiSlantedDegree);
  int degree = slanted_degree + lev;
  auto& thislevel = level(lev);
  int ncols = 0;
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree == degree) ncols++;
    }

  auto& prevlevel = level(lev-1);
  int* newcomps = new int[prevlevel.size()];
  int nrows = 0;
  for (int i=0; i<prevlevel.size(); i++)
    if (prevlevel[i].mDegree == degree)
      newcomps[i] = nrows++;
    else
      newcomps[i] = -1;

  // Create the ARing
  // Create a DMat
  //  M2::ARingZZpFlint R(gausser().get_ring()->characteristic());
  //  DMat<M2::ARingZZpFlint> M(R, nrows, ncols);

#if 0
  // TODO: Change this
  M2::ARingZZpFFPACK R(gausser().get_ring()->characteristic());
  DMat<M2::ARingZZpFFPACK> M(R, nrows, ncols);
#endif
  
  // Fill in DMat
  // loop through the elements at thislevel,
  // and for each, loop through the terms of mSyzygy.
  // if the component x satisfies newcomps[x] >= 0, then place
  // this coeff into the mutable matrix.
  int col = 0;
  long nnonzeros = 0;
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree != degree) continue;
      auto& f = p->mSyzygy;
      auto end = poly_iter(ring(), f, 1);
      auto i = poly_iter(ring(), f);
      for ( ; i != end; ++i)
        {
          long comp = monoid().get_component(i.monomial());
          if (newcomps[comp] >= 0)
            {
              #if 0
              // TODO: change this line:
              M.entry(newcomps[comp], col) = gausser().coeff_to_int(i.coefficient());
              #endif
              nnonzeros++;
            }
        }
      ++col;
    }
  double frac_nonzero = (nrows*ncols);
  frac_nonzero = nnonzeros / frac_nonzero;

  //  buffer o;
  //  displayMat(o, M);
  //  std::cout << o.str() << std::endl;
  
  // call rank
  //  auto a = DMatLinAlg<M2::ARingZZpFlint>(M);
#if 0
  // TODO: change this line
  auto a = DMatLinAlg<M2::ARingZZpFFPACK>(M);
  long numrows = M.numRows();
  long numcols = M.numColumns();
#endif
  long numrows = 0;
  long numcols = 0;
  clock_t begin_time0 = clock();
  int rk = 0; // TODO: static_cast<int>(a.rank());
  clock_t end_time0 = clock();
  double nsecs0 = (double)(end_time0 - begin_time0)/CLOCKS_PER_SEC;
  if (M2_gbTrace >= 2)
    {
      std::cout << "rank (" << slanted_degree << "," << lev << ") = " << rk
                << " time " << nsecs0 << " size= " << numrows
                << " x " << numcols << " nonzero " << nnonzeros << std::endl;
    }
  
  return rk;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
