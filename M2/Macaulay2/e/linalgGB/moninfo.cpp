// Copyright 2005  Michael E. Stillman

#include "moninfo.hpp"
#include <cstdio>
#include <cstdlib>

MonomialInfo::MonomialInfo(int nvars0)
{
  nvars = nvars0;
  nslots = 2 + nvars;
  hashfcn = newarray(long,nvars);
  for (int i=0; i<nvars; i++)
    hashfcn[i] = rand();
  mask = 0x10000000;
}

MonomialInfo::~MonomialInfo()
{
  deletearray(hashfcn);
}

void MonomialInfo::show() const
{
  fprintf(stderr, "monomial info\n");
  fprintf(stderr, "  nvars  = %d",nvars);
  fprintf(stderr, "  nslots = %d",nslots);
  fprintf(stderr, "  mask   = %ld",mask);
  fprintf(stderr, "  hash values for each variable\n");
  for (int i=0; i<nvars; i++)
    fprintf(stderr, "    %ld\n",hashfcn[i]);
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
