#include <gmp.h>
#include <gc/gc.h>
#include "../d/M2mem.h"

inline void mpz_reallocate_limbs (mpz_ptr _z)
{ 
  int _s = _z->_mp_size;
  int _as = (_s>0)?_s:-_s;
  mp_limb_t *_p = GC_MALLOC(_as*sizeof(mp_limb_t));
  memcpy(_p,_z->_mp_d,_as*sizeof(mp_limb_t));
  mpz_clear(_z);
  _z->_mp_d = _p;
  _z->_mp_size = _s;
  _z->_mp_alloc = _as;
}

inline gmp_ZZ moveToZZ (mpz_ptr _z)
{ 
  int _s = _z->_mp_size;
  int _as = (_s>0)?_s:-_s;
  mp_limb_t *_p = GC_MALLOC(_as*sizeof(mp_limb_t));
  memcpy(_p,_z->_mp_d,_as*sizeof(mp_limb_t));
  mpz_clear(_z);
  _z->_mp_d = _p;
  _z->_mp_size = _s;
  _z->_mp_alloc = _as;
  return _z;
}

inline gmp_ZZ mpzToZZ(mpz_srcptr z)
{
  // create a gmp_ZZ on the GC heap, copy limb space of z to gmp_ZZ in GC heap
  mpz_ptr result = getmemstructtype(mpz_ptr);
  int s = z->_mp_size;
  int as = (s>0) ? s : -s;
  mp_limb_t *p = GC_MALLOC(as*sizeof(mp_limb_t));
  memcpy(p, z->_mp_d, as*sizeof(mp_limb_t));
  result->_mp_d = p;
  result->_mp_size = s;
  result->_mp_alloc = as;
  return result;
}


