#include <gmp.h>
#include <gc/gc.h>
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

