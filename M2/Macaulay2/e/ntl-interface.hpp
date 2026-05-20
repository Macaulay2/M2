#ifndef _ntl_interface_hpp_
#define _ntl_interface_hpp_

/**
 * @file ntl-interface.hpp
 * @brief Engine bridge into NTL --- type conversions and the NTL-backed LLL entry point.
 *
 * Pulls in `<NTL/ZZ.h>`, `<NTL/mat_ZZ.h>`, and `<NTL/LLL.h>`
 * (with diagnostic pragmas around the include to silence NTL's
 * internal conversion warnings) and declares the engine-side
 * conversion helpers: `ntl_ZZ_to_mpz` / `ntl_ZZ_from_mpz`
 * marshal between NTL's `ZZ` and GMP `mpz_t`, while
 * `makeNTLMatrixZZ` / `mat_ZZ_set_entry` / `mat_ZZ_get_entry`
 * and `mutableMatrix_to_NTL_mat_ZZ` provide the entry-by-entry
 * plumbing to copy an engine `MutableMatrix` (transposed; NTL
 * stores rows-first) to and from an NTL `mat_ZZ`. The `dntl_*`
 * debugger printers from `ntl-debugio.cpp` are forward-declared
 * here.
 *
 * `ntl_LLL(M, U, a, b, strategy)` is the NTL-backed LLL entry
 * point: threshold `delta = a/b`, optional unimodular transform
 * captured in `U`, and `strategy` is a bit-packed selector that
 * picks the NTL precision variant --- `LLL_FP`, `LLL_QP`,
 * `LLL_XD`, or `LLL_RR` --- as documented for `rawLLL` in
 * `engine.h`. The companion `ntl-internal.cpp` isolates the
 * NTL-typed routines into their own translation unit (per the
 * top-of-file comment, "routines which often conflict with our
 * names") so the rest of the engine never has to drop NTL
 * symbols into its namespace.
 *
 * @see LLL.hpp
 * @see fplll-interface.hpp
 * @see ntl-debugio.cpp
 */

#include <stddef.h>
#include <M2/math-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <NTL/ZZ.h>
#include <NTL/mat_ZZ.h>
#include <NTL/LLL.h>
#pragma GCC diagnostic pop

// NTL_USE_NNS

class MutableMatrix;
extern NTL::mat_ZZ *makeNTLMatrixZZ(int nrows, int ncols);
void mat_ZZ_set_entry(NTL::mat_ZZ *A, long i, long j, mpz_srcptr a);
void mat_ZZ_get_entry(const NTL::mat_ZZ *A, long i, long j, mpz_t result);

extern void ntl_ZZ_to_mpz(mpz_t result, const NTL::ZZ &a);
extern NTL::ZZ ntl_ZZ_from_mpz(mpz_srcptr a);

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
