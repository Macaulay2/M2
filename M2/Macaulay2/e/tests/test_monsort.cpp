#include "../linalgGB/monsort.hpp"
#include <cstdlib>
#include <ctime>
typedef MonomialInfo::monomial monomial;
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
MonomialInfo::monomial *toMonomialArray(MonomialInfo *M, int nvars, long nlines, long *exps)
{
  long *e = exps;
  long *result0 = newarray(long,M->max_monomial_size() * nlines);
  monomial *result = newarray(long *,nlines);
  monomial m = result0;
  for (long j=0; j<nlines; j++, e+=nvars, m += M->max_monomial_size())
    {
      M->from_exponent_vector(e, 0, m);
      result[j] = m;
    }
  return result;
}
void displayMonomialArray(MonomialInfo *M, monomial *monoms, long *indices, long nlines)
{
  for (long j=0; j<nlines; j++)
    {
      M->show(monoms[indices[j]]);
      fprintf(stderr,"\n");
    }
}
int main(int argc, char **argv)
{
  clock_t s1,s2,s3,s4;
  int nvars;
  long nlines;
  if (argc != 2)
    {
      printf("usage: %s <filename>\n", argv[0]);
      exit(1);
    }
  for (int i=0; i<473; i++) rand();
  s1 = clock();
  long *exps = readMatrix(argv[1],nvars,nlines);
  MonomialInfo *M = new MonomialInfo(nvars);
  M->show();
  s2 = clock();
  monomial *monoms = toMonomialArray(M,nvars,nlines,exps);
  long *indices = newarray(long,nlines);
  for (long i=0; i<nlines; i++) indices[i] = i;
  //displayMonomialArray(M,monoms,indices,nlines);
  s3 = clock();
  MSorter::sort(M,monoms,indices,nlines);
  s4 = clock();
  //fprintf(stderr, "sorted monomials\n");
  //displayMonomialArray(M,monoms,indices,nlines);
  printf("readtime %f make %f sort %f (seconds)\n", ((double)(s2-s1))/CLOCKS_PER_SEC,
	 ((double)(s3-s2))/CLOCKS_PER_SEC, ((double)(s4-s3))/CLOCKS_PER_SEC);
  return 0;
}
/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/tests test_monsort && $M2BUILDDIR/Macaulay2/e/tests/test_monsort"
// End:
*/
