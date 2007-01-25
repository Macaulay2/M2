/* pi.cc -- routines for packed integers and monomials -*- c++ -*- */
// pi = packed integer
// nbits = number of bits per subfield
// "fields" of type U are packed into "bins" of type T, either int32_t or int64_t
#include "overflow.hpp"
#ifdef __GNUC__
#define expect_false(x) (__builtin_expect(x,0))
#define expect_true(x)  (__builtin_expect(x,1))
#else
#define expect_false(x) (x)
#define expect_true(x)  (x)
#endif
#include "pi-masks.h"
template <typename T> struct masks { static inline T ovmask(int nbits), himask(int nbits); };
template <> inline int32_t masks<int32_t>::ovmask(int nbits) { return ovmask32[nbits]; }
template <> inline int32_t masks<int32_t>::himask(int nbits) { return himask32[nbits]; }
template <> inline int64_t masks<int64_t>::ovmask(int nbits) { return ovmask64[nbits]; }
template <> inline int64_t masks<int64_t>::himask(int nbits) { return himask64[nbits]; }
template <typename T> static T bool_add(T x, T y) { return x^y; }
template <typename T> static T bool_sub(T x, T y) { return x^y; }
template <typename T> static T bool_eq(T x, T y) { return ~(x^y); }
template <typename T> static T bool_neq(T x, T y) { return x^y; }
template <typename T> static T bool_imp(T x, T y) { return (~x) | y; }
template <typename T> static T bool_nimp(T x, T y) { return x & ~y; }
template <typename T, typename U, int nbits> class pui { // U is an unsigned integer type holding at least nbits; we pack them into the integer type T
     static int bits_per_bin() { return 8 * sizeof(T); }
     static int extrabits() { return bits_per_bin() - nbits * flds_per_bin(); }	// to determine whether the overflow mask includes a bit for the highest field
     static T ovmask() { return masks<T>::ovmask(nbits); }
     static T himask() { return masks<T>::himask(nbits); }
     static U field_mask() { return ((U)1 << nbits) - 1; }
     static U field       (T t, int i) { return (U)(t >> (nbits * (flds_per_bin() - 1 - i))) & field_mask(); }
     static U field_at_bit(T t, int i) { return (U)(t >> i                                 ) & field_mask(); }
     static U checkfit(U u) { if (0 != (u & field_mask())) safe::ov("overflow: pui"); return u ; }
     static T add(T x, T y, T &carries) {
	  T sum = x + y;
	  carries |= bool_neq(bool_add(x,y),sum);
	  if expect_false (extrabits() == 0 && sum<x) safe::ov("overflow: pui + pui");
	  return sum; }
     static void add_final(T &carries) {
	  T oflows = carries & ovmask();
	  if expect_false (0 != oflows) safe::ov("overflow: pui + pui"); }
     static T sub(T x, T y, T &borrows) {
	  T dif = x - y;
	  borrows |= bool_neq(bool_sub(x,y),dif);
	  if expect_false (extrabits() == 0 && x<y) safe::ov("overflow: pui - pui");
	  return dif; }
     static void sub_final(T &borrows) {
	  T oflows = borrows & ovmask();
	  if expect_false (0 != oflows) safe::ov("overflow: pui - pui"); }
public:
     static int flds_per_bin() { return bits_per_bin() / nbits; }
     static void pack(T *dest, U *src, int numfields) {
	  if (numfields == 0) return;
	  while (1) {
	       T t = 0;
	       for (int j = (flds_per_bin()-1)*nbits; j >= 0; j -= nbits) {
		    t |= checkfit(*src++) << j;
		    if expect_false (--numfields == 0) {
			 *dest++ = t;
			 return;
		    }
	       }
	       *dest++ = t;
	  }
     }
     static void unpack(U *dest, T *src, int numfields) {
	  if (numfields == 0) return;
	  while (1) {
	       T t = *src++;
	       for (int bit = (flds_per_bin()-1)*nbits; bit >= 0; bit -= nbits) {
		    *dest++ = field_at_bit(t,bit);
		    if expect_false (--numfields == 0) return;
	       }
	  }
     }
     static int scan(T *x, T *y, int numfields, int (*f)(U,U)) {
	  if (numfields == 0) return 0;
	  while (1) {
	       T a = *x++;
	       T b = *y++;
	       for (int j = (flds_per_bin()-1)*nbits; j >= 0; j -= nbits) {
		    int r = f(field_at_bit(a,j), field_at_bit(b,j));
		    if expect_false (r != 0) return r;
		    if expect_false (--numfields == 0) return 0;
	       }
	  }
     }
     static T add(T x, T y) {
	  T carries = 0;
	  T res = add(x,y,carries);
	  add_final(carries);
	  return res; }
     static void add(T res[], T x[], T y[], int numbins) {
	  T carries = 0;
	  for (int j=0; j<numbins; j++) res[j] = add(x[j], y[j], carries);
	  add_final(carries); }
     static T cmp_lex(T x, T y) { return x > y ? 1 : x < y ? -1 : 0 ; }
     static T cmp_lex(T *x, T *y, int numbins) { 
	  for (int j = 0; j < numbins; j++) {
	       if expect_false (x[j] > y[j]) return  1; else
	       if expect_false (x[j] < y[j]) return -1;
	  }
	  return 0;
     }
     static T geq_each(T x, T y) {
	  if expect_true (extrabits() == 0 && x<y) return false;
	  T dif = x - y;
	  T borrows = bool_neq(bool_sub(x,y),dif);
	  T oflows = borrows & ovmask();
	  if expect_true (0 != oflows) return false;
	  return true;
     }
     static T sub(T x, T y) {
	  T borrows = 0;
	  T res = sub(x,y,borrows);
	  sub_final(borrows);
	  return res; }
     static void sub(T res[], T x[], T y[], int numbins) {
	  T borrows = 0;
	  for (int j=0; j<numbins; j++) res[j] = sub(x[j], y[j], borrows);
	  sub_final(borrows); } 
};

template <typename T, typename U, int nbits, int len> class puiv {
     static void sub(T res[], T x[], T y[]) {
	  pui<T,U,nbits>::sub(res,x,y,len); }
};     


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s"
// End:
