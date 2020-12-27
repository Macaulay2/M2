// Copyright 2016  Michael E. Stillman

#include "schreyer-resolution/res-moninfo-dense.hpp"
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_monomial_word

#include <cstdio>                                      // for fprintf, stderr
#include <cstdlib>                                     // for rand

ResMonoidDense::ResMonoidDense(int nvars0,
                               const std::vector<int>& var_degrees,
                               const std::vector<int>& weightvecs,
                               const MonomialOrderingType moType)
{
  nvars = nvars0;
  hashfcn = std::unique_ptr<res_monomial_word[]>(new res_monomial_word[nvars]);
  for (int i = 0; i < nvars; i++) hashfcn[i] = rand();
  mask = 0x10000000;
  mVarDegrees = var_degrees;

  ncalls_hash_value = 0;
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
#if 0  
  if (moType == MonomialOrderingType::Lex) // moIsLex(mo)
    {
      compare = &ResMonoidDense::compare_lex;

      if (M2_gbTrace >= 1)
        fprintf(stderr, "lex order\n");
    }
  else if (moType == MonomialOrderingType::GRevLex) // moIsGRevLex(mo)
    {
      compare = &ResMonoidDense::compare_grevlex;

      if (M2_gbTrace >= 1)
        fprintf(stderr, "grevlex order\n");
    }
  else
    {
      weight_vectors = weightvecs;
      nweights = static_cast<int>(weight_vectors.size()) / nvars;
      compare = &ResMonoidDense::compare_weightvector;

      if (M2_gbTrace >= 1)
        fprintf(stderr, "weight order\n");
    }
#endif

  nslots = 2 + nvars + nweights;
  firstvar = 2 + nweights;
}

ResMonoidDense::~ResMonoidDense()
{
  //  delete [] hashfcn;
}

void ResMonoidDense::show() const
{
  fprintf(stderr, "monomial info\n");
  fprintf(stderr, "  nvars  = %d", nvars);
  fprintf(stderr, "  nslots = %d", nslots);
  fprintf(stderr, "  mask   = %d", mask);
  fprintf(stderr, "  hash values for each variable\n");
  for (int i = 0; i < nvars; i++) fprintf(stderr, "    %d\n", hashfcn[i]);
  fprintf(stderr, "  #calls hashval = %lu\n", ncalls_hash_value);
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

void ResMonoidDense::show(res_const_packed_monomial m) const
{
  fprintf(stderr, "[");
  for (int v = 1; v < monomial_size(m); v++)
    {
      if (v > 1) fprintf(stderr, " ");
      fprintf(stderr, "%d", m[v]);
    }
  fprintf(stderr, "]");
}

void ResMonoidDense::showAlpha(res_const_packed_monomial m) const
{
  component_index comp = get_component(m);

  m += 2 + nweights;  // get by: hashcode, component, weightvals
  for (int i = 0; i < nvars; i++)
    {
      long e = *m++;
      if (e == 0) continue;
      fprintf(stdout, "%c", 'a' + i);
      if (e > 1) fprintf(stdout, "%ld", e);
    }
  fprintf(stdout, "<%d>", comp);
}

void ResMonoidDense::dump(std::ostream& o, res_const_packed_monomial mon)
{
  o << "[";
  for (int ell = 0; ell < monomial_size(mon); ell++)
    {
      if (ell != 0) o << " ";
      o << mon[ell];
    }
  o << "]";
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
