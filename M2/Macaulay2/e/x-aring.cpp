#include "engine.h"
#include "exceptions.hpp"

#include "aring-glue.hpp"
#include "aring-zzp.hpp"
#include "aring-gf.hpp"


const Ring /* or null */ *rawARingZZp(int p)
  /* p must be a prime number <= 32767 */
{
  if (p <= 1 || p >= 32750)
    {
      ERROR("ZZP: expected a prime number p in range 2 <= p <= 32749");
      return 0;
    }
  M2::ARingZZp *A = new M2::ARingZZp(p);
  return M2::RingWrap<M2::ARingZZp>::create(A);
}

const Ring /* or null */ *rawARingGaloisField(int p, int n)
{
  // Check that the ring R of f is a polynomial ring in one var over a ZZ/p
  // Check that f has degree >= 2
  // Check that f is monic
  // If any of these fail, then return 0.
     try {
#if defined(HAVE_FFLAS_FFPACK) && defined(HAVE_GIVARO)
       M2::ARingGF *A = new M2::ARingGF(p,n);
       return M2::RingWrap<M2::ARingGF>::create(A);
#else
       ERROR("add --enable-fflas-ffpack --enable-givaro when building M2");
       return 0;
#endif
     }
     catch (exc::engine_error e) {
          ERROR(e.what());
          return NULL;
     }
}
