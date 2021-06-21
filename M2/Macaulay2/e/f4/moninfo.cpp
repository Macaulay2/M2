// Copyright 2005-2021  Michael E. Stillman

#include "f4/moninfo.hpp"
#include "interface/monomial-ordering.h"  // for moGetWeightValues, moIsGRevLex
#include "newdelete.hpp"                  // for deletearray, newarray_atomic

#include <cstdio>                         // for fprintf, stderr, stdout
#include <cstdlib>                        // for rand

MonomialInfo::MonomialInfo(int nvars0, const MonomialOrdering *mo)
{
  nvars = nvars0;
  hashfcn = newarray_atomic(monomial_word, nvars);
  for (int i = 0; i < nvars; i++) hashfcn[i] = rand();
  mask = 0x10000000;

  ncalls_compare = 0;
  ncalls_mult = 0;
  ncalls_get_component = 0;
  ncalls_from_exponent_vector = 0;
  ncalls_to_exponent_vector = 0;
  ncalls_to_varpower = 0;
  ncalls_from_varpower = 0;
  ncalls_is_equal = 0;
  ncalls_is_equal_true = 0;
  ncalls_divide = 0;
  ncalls_weight = 0;
  ncalls_unneccesary = 0;
  ncalls_quotient_as_vp = 0;

  nweights = 0;
  weight_vectors = 0;
  if (moIsLex(mo))
    {
      compare = &MonomialInfo::compare_lex;

      if (M2_gbTrace >= 1) fprintf(stderr, "lex order\n");
    }
  else if (moIsGRevLex(mo))
    {
      compare = &MonomialInfo::compare_grevlex;

      if (M2_gbTrace >= 1) fprintf(stderr, "grevlex order\n");
    }
  else
    {
      weight_vectors = moGetWeightValues(mo);
      nweights = weight_vectors->len / nvars;
      compare = &MonomialInfo::compare_weightvector;

      if (M2_gbTrace >= 1) fprintf(stderr, "weight order\n");
    }

  nslots = 2 + nvars + nweights;
  firstvar = 2 + nweights;
}

MonomialInfo::~MonomialInfo() { deletearray(hashfcn); }
monomial_word MonomialInfo::monomial_weight(const_packed_monomial m,
                                            const M2_arrayint wts) const
{
  ncalls_weight++;
  const_packed_monomial m1 = m + 2;
  int top = wts->len;
  int *n = wts->array;
  monomial_word sum = 0;
  for (int j = top; j > 0; --j) sum += *m1++ * *n++;
  return sum;
}

void MonomialInfo::show() const
{
  fprintf(stderr, "monomial info\n");
  fprintf(stderr, "  nvars  = %d", nvars);
  fprintf(stderr, "  nslots = %d", nslots);
  fprintf(stderr, "  mask   = %ld", mask);
  fprintf(stderr, "  hash values for each variable\n");
  for (int i = 0; i < nvars; i++) fprintf(stderr, "    %ld\n", hashfcn[i]);
  fprintf(stderr, "  #calls compare = %lu\n", ncalls_compare);
  fprintf(stderr, "  #calls mult    = %lu\n", ncalls_mult);
  fprintf(stderr, "  #calls get comp= %lu\n", ncalls_get_component);
  fprintf(stderr, "  #calls fromexp = %lu\n", ncalls_from_exponent_vector);
  fprintf(stderr, "  #calls toexp   = %lu\n", ncalls_to_exponent_vector);
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

  m += 2 + nweights;  // get by: hashcode, component, weightvals
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
