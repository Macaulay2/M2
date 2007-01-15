/* pi.cc -- routines for packed integers and monomials */
// pi = packed integer
// piv = packed integer vector
// pui = packed unsigned integer
// puiv = packed unsigned integer vector
// pgi = packed integer, with one extra guard bit per field
// pgiv = packed integer vector, with one extra guard bit per field
// pgui = packed unsigned integer, with one extra guard bit per field
// pguiv = packed unsigned integer vector, with one extra guard bit per field
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
template <typename T, int bits> class pui {
     static T bit_add(T x, T y) { return x^y; }
     static T bit_sub(T x, T y) { return x^y; }
     static T bit_eq(T x, T y) { return ~(x^y); }
     static T bit_neq(T x, T y) { return x^y; }
     static T bit_imp(T x, T y) { return (~x) | y; }
     static T bit_nimp(T x, T y) { return x & ~y; }
     static int numbits() { return 8 * sizeof(T); }
     static int numfields() { return numbits() / bits; }
     static int extrabits() { return numbits() - bits * numfields(); }
     static T lomask() { return masks<T>::lomask(bits); }
     static T himask() { return masks<T>::himask(bits); }
     static T add(T x, T y, T &oflows) {
	  T sum = x + y;
	  oflows |= bit_nimp(sum, bit_neq(x,y)); /* fix... */
	  return sum;
     }
     static void add_final(T &oflows) {
	  if expect_false (0 != (oflows & himask())) safe::ov("overflow: pui - pui");
     }
     static T sub(T x, T y, T &oflows) {
	  T dif = x - y;
	  oflows |= bit_neq(bit_sub(x,y),dif);
	  if expect_false (extrabits() == 0 && x<y) safe::ov("overflow: pui - pui");
	  return dif;
     }
     static void sub_final(T &oflows) {
	  if expect_false (0 != (oflows & lomask())) safe::ov("overflow: pui - pui");
     }
public:
     static T sub(T x, T y) {
	  T oflows = 0;
	  T res = sub(x,y,oflows);
	  sub_final(oflows);
	  return res;
     }
     static void sub(int len, T res[], T x[], T y[]) {
	  T oflows = 0;
	  for (int j=0; j<len; j++) res[j] = sub(x[j], y[j], oflows);
	  sub_final(oflows);
     }
};
template <typename T, int bits, int len> class puiv {
     static void sub(T res[], T x[], T y[]) {
	  pui<T,bits>::sub(len,res,x,y);
     }
};     


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pui-demangled.s"
// End:
