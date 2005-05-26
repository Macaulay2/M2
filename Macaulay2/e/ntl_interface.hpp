#ifndef _ntl_interface_hpp_
#define _ntl_interface_hpp_

#include <gmp.h>
class ZZ;
class mat_ZZ;
class MutableMatrix;
extern mat_ZZ *makeNTLMatrixZZ(int nrows, int ncols);
void mat_ZZ_set_entry(mat_ZZ *A, long i, long j, mpz_t a);
void mat_ZZ_get_entry(mat_ZZ *A, long i, long j, mpz_t result);

void dntl_matZZ(const mat_ZZ *A);
void dntl_ZZ(const ZZ *f);
mat_ZZ *mutableMatrix_to_NTL_mat_ZZ(const MutableMatrix *M);
bool ntl_LLL(const MutableMatrix *M, long a, long b, int strategy);
  // the threshold is a/b.  strategy is explained for rawLLL in engine.h
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:





