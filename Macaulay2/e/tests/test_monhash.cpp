#include "../linalgGB/monhashtable.hpp"
#include <cstdlib>
#include <ctime>

typedef MonomialHashTable<MonomialInfo> MonomialHash;
//typedef MonomialHashTable MonomialHash;

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

long *makeProducts(int nvars, long nlines, long *exps, long &nproducts)
{
  nproducts = nlines * (nlines-1) / 2;
  fprintf(stderr, "product array size = %ld\n", nproducts*nvars);
  long *g = newarray(long,nproducts*nvars);
  long *g0 = g;
  long *e = exps;
  for (long i=0; i<nlines; i++, e +=nvars)
    {
      long *f = e + nvars;
      for (long j = i+1; j<nlines; j++, f += nvars)
	{
	  for (int p=0; p<nvars; p++)
	    g0[p] = e[p] + f[p];
	  g0 += nvars;
	}
    }
  return g;
}

MonomialHash *toHash(int nvars, long nlines, long *exps)
{
  MonomialInfo *M = new MonomialInfo(nvars);
  M->show();
  MonomialHash *H = new MonomialHash(M,18);
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
  clock_t s1,s2,s3,s4;
  int nvars;
  long nlines, nproducts;
  if (argc != 2)
    {
      printf("usage: %s <filename>\n", argv[0]);
      exit(1);
    }
  for (int i=0; i<473; i++)
    rand();
  //srand(time(0));
  s1 = clock();
  long *exps = readMatrix(argv[1],nvars,nlines);
  s2 = clock();
  long *exps2 = makeProducts(nvars,nlines,exps,nproducts);
  s3 = clock();
  MonomialHash *H = toHash(nvars,nproducts,exps2);
  s4 = clock();
  H->dump();
  H->show();
  printf("readtime %f products %f makehash %f (seconds)\n", ((double)(s2-s1))/CLOCKS_PER_SEC,
	 ((double)(s3-s2))/CLOCKS_PER_SEC, ((double)(s4-s3))/CLOCKS_PER_SEC);
  return 0;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/tests test_monhash && $M2BUILDDIR/Macaulay2/e/tests/test_monhash"
// End:
*/

