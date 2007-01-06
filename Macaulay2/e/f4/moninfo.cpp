// Copyright 2005  Michael E. Stillman

#include "../newdelete.hpp"
#include "moninfo.hpp"
#include <cstdio>
#include <cstdlib>

MonomialInfo::MonomialInfo(int nvars0)
{
  nvars = nvars0;
  nslots = 2 + nvars;
  hashfcn = newarray_atomic(monomial_word,nvars);
  for (int i=0; i<nvars; i++)
    hashfcn[i] = rand();
  mask = 0x10000000;
}

MonomialInfo::~MonomialInfo()
{
  deletearray(hashfcn);
}

monomial_word MonomialInfo::monomial_weight(const_packed_monomial m, const M2_arrayint wts) const
{
    const_packed_monomial m1 = m+2;
    int top = wts->len;
    int *n = wts->array;
    monomial_word sum = 0;
    for (int j=top; j>0; --j) sum += *m1++ * *n++;
    return sum;
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

void MonomialInfo::show(const_packed_monomial m) const
{
  fprintf(stderr, "[");
  for (int v=1; v<monomial_size(m); v++)
    {
      if (v > 1) fprintf(stderr, " ");
      fprintf(stderr, "%ld", m[v]);
    }
  fprintf(stderr, "]");
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
