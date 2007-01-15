/* pi.cc -- routines for packed integers and monomials */
// pi = packed integer
// piv = packed integers
// bits = number of bits per subfield
// T = type of int to pack them into
#include "overflow.hpp"
#ifdef __GNUC__
#define expect_false(x) (__builtin_expect(x,0))
#define expect_true(x)  (__builtin_expect(x,1))
#else
#define expect_false(x) (x)
#define expect_true(x)  (x)
#endif
#include "pi-masks.h"

template <typename T> struct masks {
     static inline T lomask(int bits);
     static inline T himask(int bits);
};

template <> inline int32_t masks<int32_t>::lomask(int bits) { return lomask32[bits]; }
template <> inline int32_t masks<int32_t>::himask(int bits) { return himask32[bits]; }
template <> inline int64_t masks<int64_t>::lomask(int bits) { return lomask64[bits]; }
template <> inline int64_t masks<int64_t>::himask(int bits) { return himask64[bits]; }

template <typename T> int f();
template <> int f<int>() { return 1; }

template <typename T, int bits> class pi { 
     T i;
public:
     pi(T i0) : i(i0) {}
     operator T() { return i; }
     pi operator+(pi &y) {
	  T s = i + y.i;
	  if expect_false (s & (masks<T>::himask(bits))) safe::ov("overflow: pi + pi");
	  return s; }
     pi operator+=(pi &y) {
	  T s = i += y.i;
	  if expect_false (s & (masks<T>::himask(bits))) safe::ov("overflow: pi += pi");
	  return s; }
};
template <typename T,int bits,int len> class piv { 
     T i[len];
public:
     piv &operator += (piv &y) {
	  T o=0;
	  for (int j=0; j<len; j++) o |= i[j] += y.i[j];
	  if expect_false(o & (masks<T>::himask(bits))) safe::ov("overflow: piv += piv");
	  return *this; }
};
