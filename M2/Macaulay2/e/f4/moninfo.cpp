// Copyright 2005-2021  Michael E. Stillman

#include "f4/moninfo.hpp"
#include "monordering.hpp"                // for monomialOrderingToMatrix
#include "newdelete.hpp"                  // for freemem, newarray_atomic

#include <cstdio>                         // for fprintf, stderr, stdout
#include <cstdlib>                        // for rand
#include <iostream>

MonomialInfo::MonomialInfo(int nvars0,
                           const MonomialOrdering *mo,
                           const std::vector<int>& heftDegrees,
                           const std::vector<int>& moduleHeftDegrees)
  : mHeftDegrees(heftDegrees),
    mModuleHeftDegrees(moduleHeftDegrees)
{
  nvars = nvars0;
  hashfcn = new monomial_word[nvars];
  for (int i = 0; i < nvars; i++) hashfcn[i] = rand();
  mask = 0x10000000;

  ncalls_compare = 0;
  ncalls_mult = 0;
  ncalls_get_component = 0;
  ncalls_from_expvector = 0;
  ncalls_to_expvector = 0;
  ncalls_to_varpower = 0;
  ncalls_from_varpower = 0;
  ncalls_is_equal = 0;
  ncalls_is_equal_true = 0;
  ncalls_divide = 0;
  ncalls_weight = 0;
  ncalls_unneccesary = 0;
  ncalls_quotient_as_vp = 0;

  monomialOrderingToMatrix(*mo,
                           mWeightVectors, // flattened
                           mTieBreakerIsRevLex, // true means RevLex, false means Lex
                           mPositionUp, // +1 means Up, -1 means Down
                           mComponentLoc); // 0 means check first comp, -1 means check at end, otherwise check before this location

  mNumWeights = (nvars == 0 ? 0 : mWeightVectors.size() / nvars);

  // if (moIsLex(mo))
  //   {
  //     compare = &MonomialInfo::compare_lex;

  //     if (M2_gbTrace >= 1) fprintf(stderr, "lex order\n");
  //   }
  // else if (moIsGRevLex(mo))
  //   {
  //     compare = &MonomialInfo::compare_grevlex;

  //     if (M2_gbTrace >= 1) fprintf(stderr, "grevlex order\n");
  //   }
  // else
  //   {
  //     weight_vectors = moGetWeightValues(mo);
  //     nweights = weight_vectors->len / nvars;
  //     compare = &MonomialInfo::compare_weightvector;

  //     if (M2_gbTrace >= 1) fprintf(stderr, "weight order\n");
  //   }

  nslots = 2 + nvars + mNumWeights;
  firstvar = 2 + mNumWeights;
}

MonomialInfo::~MonomialInfo()
{ 
  delete [] hashfcn; 
}

monomial_word MonomialInfo::monomial_heft(const_packed_monomial m) const
{
  ncalls_weight++;
  const_packed_monomial m1 = m + firstvar;
  monomial_word sum = mModuleHeftDegrees[get_component(m)];
  for (int j = 0; j < mHeftDegrees.size(); ++j)
    {
      sum += mHeftDegrees[j] * m1[j];
    }
  return sum;
}

void MonomialInfo::show() const
{
  fprintf(stderr, "monomial info\n");
  fprintf(stderr, "  nvars  = %d", nvars);
  fprintf(stderr, "  nslots = %d", nslots);
  fprintf(stderr, "  firstvar = %d", firstvar);
  fprintf(stderr, "  nweights = %d", mNumWeights);
  fprintf(stderr, "  tiebreaker = %d", mTieBreakerIsRevLex);
  fprintf(stderr, "  mask   = %ld", mask);
  fprintf(stderr, "  hash values for each variable\n");
  for (int i = 0; i < nvars; i++) fprintf(stderr, "    %ld\n", hashfcn[i]);
  fprintf(stderr, "  #calls compare = %lu\n", ncalls_compare);
  fprintf(stderr, "  #calls mult    = %lu\n", ncalls_mult);
  fprintf(stderr, "  #calls get comp= %lu\n", ncalls_get_component);
  fprintf(stderr, "  #calls fromexp = %lu\n", ncalls_from_expvector);
  fprintf(stderr, "  #calls toexp   = %lu\n", ncalls_to_expvector);
  fprintf(stderr, "  #calls fromvp  = %lu\n", ncalls_from_varpower);
  fprintf(stderr, "  #calls tovp    = %lu\n", ncalls_to_varpower);
  fprintf(stderr, "  #calls is equal= %lu\n", ncalls_is_equal);
  fprintf(stderr, "  #calls eq true = %lu\n", ncalls_is_equal_true);
  fprintf(stderr, "  #calls divide  = %lu\n", ncalls_divide);
  fprintf(stderr, "  #calls weight  = %lu\n", ncalls_weight);
  fprintf(stderr, "  #calls unneeded= %lu\n", ncalls_unneccesary);
  fprintf(stderr, "  #calls vp quot = %lu\n", ncalls_quotient_as_vp);
}

void MonomialInfo::show(const_packed_monomial m) const
{
  fprintf(stderr, "[");
  for (int v = 1; v < monomial_size(m); v++)
    {
      if (v > 1) fprintf(stderr, " ");
      fprintf(stderr, "%ld", m[v]);
    }
  fprintf(stderr, "]");
}

void MonomialInfo::showAlpha(const_packed_monomial m) const
{
  long comp = get_component(m);

  m += 2 + mNumWeights;  // get by: hashcode, component, weightvals
  for (int i = 0; i < nvars; i++)
    {
      long e = *m++;
      if (e == 0) continue;
      fprintf(stdout, "%c", 'a' + i);
      if (e > 1) fprintf(stdout, "%ld", e);
    }
  fprintf(stdout, "<%ld>", comp);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
