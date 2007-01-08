#ifndef _overflow_h_
#define _overflow_h_

// methods for detecting arithmetic overflows

#include "exceptions.hpp"

#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

namespace safe {
     static inline int32_t over_1(int32_t x) { return x < 0 ; }
     static inline int32_t over_2(int32_t x) { return (0x80008000 & x) != 0 ; }
     static inline int32_t over_4(int32_t x) { return (0x80808080 & x) != 0 ; }

     static inline int32_t add(int32_t x, int32_t y, const char *msg) {
	  int32_t z = x+y;
	  if ((x < 0) != (z < 0) && (x < 0) == (y < 0)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t add(int32_t x, int32_t y) { 
	  return add(x,y,"overflow: int32_t + int32_t"); 
     }

     static inline int32_t sub(int32_t x, int32_t y, const char *msg) {
	  int32_t z = x-y;
	  if ((x < 0) != (z < 0) && (x < 0) != (y < 0)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t sub(int32_t x, int32_t y) { 
	  return sub(x,y,"overflow: int32_t - int32_t"); 
     }

     static inline int32_t sub_pos(int32_t x, int32_t y, const char *msg) {
	  if (x <= y) return 0;
	  int32_t z = x-y;
	  if (z < 0) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t sub_pos(int32_t x, int32_t y) { 
	  return sub_pos(x,y,"overflow: int32_t - int32_t [pos]"); 
     }

     static inline int32_t minus(int32_t x, const char *msg) {
	  int32_t z = -x;
	  if (z == x && x != 0) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t minus(int32_t x) { 
	  return minus(x,"overflow: - int32_t"); 
     }

     static inline int32_t pos_add(int32_t x, int32_t y, const char *msg) {
	  assert(! over_1(x) && ! over_1(y));
	  int32_t z = x+y;
	  if (over_1(z)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t pos_add(int32_t x, int32_t y) { 
	  return pos_add(x,y,"overflow: pos int32_t + pos int32_t");
     }
     static inline int32_t pos_add_2(int32_t x, int32_t y, const char *msg) {
	  assert(! over_2(x) && ! over_2(y));
	  int32_t z = x+y;
	  if (over_2(z)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t pos_add_2(int32_t x, int32_t y) { 
	  return pos_add_2(x,y,"overflow: pos int32_t + pos int32_t, packed 2");
     }
     static inline int32_t pos_add_4(int32_t x, int32_t y, const char *msg) {
	  assert(! over_4(x) && ! over_4(y));
	  int32_t z = x+y;
	  if (over_4(z)) throw(exc::overflow_error(msg));
	  return z;
     }
     static inline int32_t pos_add_4(int32_t x, int32_t y) { 
	  return pos_add_4(x,y,"overflow: pos int32_t + pos int32_t, packed 4");
     }

     static inline int32_t mult(int32_t x, int32_t y, const char *msg) {
	  int64_t z = x*(int64_t)y;
	  int32_t w = z;
	  if (z != (int64_t)w) throw(exc::overflow_error(msg));
	  return w;
     }
     static inline int32_t mult(int32_t x, int32_t y) { 
	  return mult(x,y,"overflow: int32_t + int32_t"); 
     }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
