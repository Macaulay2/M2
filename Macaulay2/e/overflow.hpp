#ifndef _overflow_h_
#define _overflow_h_

// #define TRYLONGLONG

// methods for detecting arithmetic overflows

#include "exceptions.hpp"
#include <assert.h>
#include "config.h"

#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

namespace safe {

     void ov(const char *msg);

     static inline int32_t fits_7(int32_t x,const char *msg) {
	  if ((x & ~0x7f) != 0) ov(msg);
	  return x;
     }
     static inline int32_t fits_15(int32_t x,const char *msg) {
	  if ((x & ~0x7fff) != 0) ov(msg);
	  return x;
     }
     static inline int32_t fits_31(int32_t x,const char *msg) {
	  if (x < 0) ov(msg);
	  return x;
     }

     static inline int32_t over_1(int32_t x) { return x < 0 ; }
     static inline int32_t over_2(int32_t x) { return (0x80008000 & x) != 0 ; }
     static inline int32_t over_4(int32_t x) { return (0x80808080 & x) != 0 ; }

#ifdef TRYLONGLONG
     static inline int32_t add(int32_t x, int32_t y, const char *msg) {
	  int32_t z = x+y;
	  int64_t w = (int64_t)x+y;
	  if (w != (int64_t)z) ov(msg);
	  return z;
     }
#else
     static inline int32_t add(int32_t x, int32_t y, const char *msg) {
	  int32_t z = x+y;
	  if ((x < 0) != (z < 0) && (x < 0) == (y < 0)) ov(msg);
	  return z;
     }
#endif

     static inline int32_t add(int32_t x, int32_t y) { 
	  return add(x,y,"overflow: int32_t + int32_t"); 
     }

#ifdef TRYLONGLONG
     static inline int32_t add_to(int32_t &x, int32_t y, const char *msg) {
	  int32_t z = x+y;
	  int64_t w = (int64_t)x+y;
	  if (w != (int64_t)z) ov(msg);
	  return x=z;
     }
#else
     static inline int32_t add_to(int32_t &x, int32_t y, const char *msg) {
	  int32_t z = x+y;
	  if ((x < 0) != (z < 0) && (x < 0) == (y < 0)) ov(msg);
	  return x=z;
     }
#endif

     static inline int32_t add_to(int32_t &x, int32_t y) { 
	  return add_to(x,y,"overflow: int32_t + int32_t"); 
     }

#ifdef TRYLONGLONG
     static inline int32_t sub(int32_t x, int32_t y, const char *msg) {
	  int32_t z = x-y;
	  int64_t w = (int64_t)x-y;
	  if (w != (int64_t)z) ov(msg);
	  return z;
     }
#else
     static inline int32_t sub(int32_t x, int32_t y, const char *msg) {
	  int32_t z = x-y;
	  if ((x < 0) != (z < 0) && (x < 0) != (y < 0)) ov(msg);
	  return z;
     }
#endif

     static inline int32_t sub(int32_t x, int32_t y) { 
	  return sub(x,y,"overflow: int32_t - int32_t"); 
     }

#ifdef TRYLONGLONG
     static inline int32_t sub_from(int32_t &x, int32_t y, const char *msg) {
	  int64_t z = (int64_t)x-y;
	  int32_t w = x-y;
	  if (z != (int64_t)w) ov(msg);
	  return x=w;
     }
#else
     static inline int32_t sub_from(int32_t &x, int32_t y, const char *msg) {
	  int32_t z = x-y;
	  if ((x < 0) != (z < 0) && (x < 0) != (y < 0)) ov(msg);
	  return x=z;
     }
#endif

     static inline int32_t sub_from(int32_t &x, int32_t y) { 
	  return sub_from(x,y,"overflow: int32_t - int32_t"); 
     }

     static inline int32_t sub_pos(int32_t x, int32_t y, const char *msg) {
	  if (x <= y) return 0;
	  int32_t z = x-y;
	  if (z < 0) ov(msg);
	  return z;
     }
     static inline int32_t sub_pos(int32_t x, int32_t y) { 
	  return sub_pos(x,y,"overflow: int32_t - int32_t [pos]"); 
     }

     static inline int32_t minus(int32_t x, const char *msg) {
	  int32_t z = -x;
	  if (z == x && x != 0) ov(msg);
	  return z;
     }
     static inline int32_t minus(int32_t x) { 
	  return minus(x,"overflow: - int32_t"); 
     }

     static inline int32_t pos_add(int32_t x, int32_t y, const char *msg) {
	  assert(! over_1(x) && ! over_1(y));
	  int32_t z = x+y;
	  if (over_1(z)) ov(msg);
	  return z;
     }
     static inline int32_t pos_add(int32_t x, int32_t y) { 
	  return pos_add(x,y,"overflow: pos int32_t + pos int32_t");
     }
     static inline int32_t pos_add_2(int32_t x, int32_t y, const char *msg) {
	  assert(! over_2(x) && ! over_2(y));
	  int32_t z = x+y;
	  if (over_2(z)) ov(msg);
	  return z;
     }
     static inline int32_t pos_add_2(int32_t x, int32_t y) { 
	  return pos_add_2(x,y,"overflow: pos int32_t + pos int32_t, packed 2");
     }
     static inline int32_t pos_add_4(int32_t x, int32_t y, const char *msg) {
	  assert(! over_4(x) && ! over_4(y));
	  int32_t z = x+y;
	  if (over_4(z)) ov(msg);
	  return z;
     }
     static inline int32_t pos_add_4(int32_t x, int32_t y) { 
	  return pos_add_4(x,y,"overflow: pos int32_t + pos int32_t, packed 4");
     }

     static inline int32_t mult(int32_t x, int32_t y, const char *msg) {
	  int64_t z = (int64_t)x * y;
	  int32_t w = z;
	  if (z != (int64_t)w) ov(msg);
	  return w;
     }
     static inline int32_t mult(int32_t x, int32_t y) { 
	  return mult(x,y,"overflow: int32_t * int32_t"); 
     }

     static inline int32_t mult_by(int32_t &x, int32_t y, const char *msg) {
	  int64_t z = (int64_t)x * y;
	  int32_t w = z;
	  if (z != (int64_t)w) ov(msg);
	  return x=w;
     }
     static inline int32_t mult_by(int32_t &x, int32_t y) { 
	  return mult_by(x,y,"overflow: int32_t * int32_t"); 
     }

     static inline int32_t div(int32_t x, int32_t y, const char *msg) {
	  if (x == -x && x < 0 && y == -1) ov(msg);
	  return x / y;
     }
     static inline int32_t div(int32_t x, int32_t y) {
	  return div(x,y,"overflow: int32 / int32");
     }

     static inline int32_t div_by(int32_t &x, int32_t y, const char *msg) {
	  if (x == -x && x < 0 && y == -1) ov(msg);
	  return x /= y;
     }
     static inline int32_t div_by(int32_t &x, int32_t y) {
	  return div_by(x,y,"overflow: int32 / int32");
     }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
