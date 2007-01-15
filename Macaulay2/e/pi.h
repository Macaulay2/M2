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
#define numbits(T) (8 * sizeof(T))
#define divides(a,b) ((b)%(a)==0)
template <typename T> struct masks {
     static inline T lomask(int bits);
     static inline T himask(int bits);
};
template <> inline int32_t masks<int32_t>::lomask(int bits) { return lomask32[bits]; }
template <> inline int32_t masks<int32_t>::himask(int bits) { return himask32[bits]; }
template <> inline int64_t masks<int64_t>::lomask(int bits) { return lomask64[bits]; }
template <> inline int64_t masks<int64_t>::himask(int bits) { return himask64[bits]; }
template <typename T, int bits> class pi { 
     static inline T lomask() { return masks<T>::lomask(bits); }
     static inline T himask() { return masks<T>::himask(bits); }
     static T inline add(T x, T y, T &carries) {
	  T sum = x + y;
	  T diffs = x ^ y;
	  carries |= diffs ^ sum;
	  if expect_false (divides(bits,numbits(T)) && diffs > 0 && (sum^x)<0) safe::ov("overflow: pi + pi");
	  return sum;
     }
     static void inline add_cleanup(T &carries) {
	  if expect_false (0 != (carries & lomask())) safe::ov("overflow: pi + pi");
     }
public:
     static T inline add(T x, T y) {
	  T carries = 0;
	  T res = add(x,y,carries);
	  add_cleanup(carries);
	  return res;
     }
     static void inline add(int len, T res[], T x[], T y[]) {
	  T carries = 0;
	  for (int j=0; j<len; j++) res[j] = add(x[j], y[j], carries);
	  add_cleanup(carries);
     }
};
template <typename T, int bits, int len> class piv {
     static void inline add(T res[], T x[], T y[]) {
	  pi<T,bits>::add(len,res,x,y);
     }
};     


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s"
// End:
