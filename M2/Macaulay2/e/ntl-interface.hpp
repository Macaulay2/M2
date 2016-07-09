#ifndef _ntl_interface_hpp_
#define _ntl_interface_hpp_

#include <stddef.h>
#include <gmp.h>
#include <mpfr.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <NTL/ZZ.h>
#include <NTL/mat_ZZ.h>
#include <NTL/LLL.h>
#pragma GCC diagnostic pop

//NTL_USE_NNS

class MutableMatrix;
extern NTL::mat_ZZ *makeNTLMatrixZZ(int nrows, int ncols);
void mat_ZZ_set_entry(NTL::mat_ZZ *A, long i, long j, mpz_t a);
void mat_ZZ_get_entry(const NTL::mat_ZZ *A, long i, long j, mpz_t result);

extern void ntl_ZZ_to_mpz(mpz_t result, const NTL::ZZ &a);
extern NTL::ZZ ntl_ZZ_from_mpz(mpz_t a);

void dntl_matZZ(const NTL::mat_ZZ *A);
void dntl_ZZ(const NTL::ZZ *f);
NTL::mat_ZZ *mutableMatrix_to_NTL_mat_ZZ(const MutableMatrix *M);
bool ntl_LLL(MutableMatrix *M, MutableMatrix *U, long a, long b, int strategy);
  // U can be NULL.  If it is not, it will be set to the transform matrix
  // the threshold is a/b.  strategy is explained for rawLLL in engine.h
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
