#include "../linalgGB/monhashtable.hpp"
#include <cstdlib>
#include <ctime>

long *readMatrix(char *fil, int &nvars, long &nlines)
{
  FILE *f = fopen(fil, "r");
  fscanf(f, "%d %ld", &nvars, &nlines);
  long *result = newarray(long, nvars * nlines);
  long *e = result;
  for (long j=0; j<nlines; j++, e+=nvars)
    {
      for (int i=0; i<nvars; i++)
	fscanf(f, "%ld", e+i);
    }
  fclose(f);
  return result;
}

MonomialHashTable *readMonomials(char *fil)
{
  FILE *f = fopen(fil, "r");
  int nv;
  long nlines;
  long *result;
  fscanf(f, "%d %ld", &nv, &nlines);
  MonomialInfo *M = new MonomialInfo(nv);
  M->show();
  MonomialHashTable *H = new MonomialHashTable(M,16);
  long *e = newarray(long,nv);
  for (long j=0; j<nlines; j++)
    {
      long *m = newarray(long,M->max_monomial_size());
      for (int i=0; i<nv; i++)
	fscanf(f, "%ld", e+i);
      M->from_exponent_vector(e, 0, m);
      H->find_or_insert(m,result);
    }
  return H;
}

MonomialHashTable *toHash(int nvars, long nlines, long *exps)
{
  MonomialInfo *M = new MonomialInfo(nvars);
  M->show();
  MonomialHashTable *H = new MonomialHashTable(M,17);
  long *e = exps;
  long *f;
  for (long j=0; j<nlines; j++, e+=nvars)
    {
      long *m = newarray(long,M->max_monomial_size());
      M->from_exponent_vector(e, 0, m);
      H->find_or_insert(m,f);
    }
  return H;
}

int main(int argc, char **argv)
{
  clock_t s1,s2,s3;
  int nvars;
  long nlines;
  if (argc != 2)
    {
      printf("usage: %s <filename>\n", argv[0]);
      exit(1);
    }
  srand(time(0));
  s1 = clock();
  long *exps = readMatrix(argv[1],nvars,nlines);
  s2 = clock();
  MonomialHashTable *H = toHash(nvars,nlines,exps);
  s3 = clock();
  H->dump();
  //H->show();
  printf("readtime %f makehash %f (seconds)\n", ((double)(s2-s1))/CLOCKS_PER_SEC,
	 ((double)(s3-s2))/CLOCKS_PER_SEC);
  return 0;
}
/*
  nvars  = 6  nslots = 2  mask   = 268435456  hash values for each variable
    865829753
    651466599
    1327496987
    1034251826
    937800764
    1234955215
*/
/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/tests test_monhash && $M2BUILDDIR/Macaulay2/e/tests/test_monhash"
// End:
*/

