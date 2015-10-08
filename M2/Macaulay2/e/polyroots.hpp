
#ifndef _polyroots_hpp_
#define _polyroots_hpp_

#include <pari/pari.h>

#include "relem.hpp"
#include "polyring.hpp"
#include "aring-CC.hpp"
#include "aring-CCC.hpp"

typedef M2::ARingCC::complex complex;

typedef M2::ARingCCC::mpfc_struct mpc_t[1];
typedef M2::ARingCCC::mpfc_ptr mpc_ptr;

extern "C" GEN mpz_get_GEN (const mpz_ptr f);
extern "C" GEN mpq_get_GEN (const mpq_ptr c);
extern "C" GEN mpfr_get_GEN (const mpfr_ptr f);
extern "C" GEN mpc_get_GEN (const mpc_ptr c);
extern "C" void pari_mpfr_init_set_GEN(mpfr_ptr f, GEN x,
                                       mpfr_prec_t default_prec);
extern "C" void pari_mpc_init_set_GEN(mpc_ptr c, GEN x,
                                      mpfr_prec_t default_prec);

engine_RawRingElementArrayOrNull rawRoots(const RingElement *p, long prec,
                                          int unique);

#endif
