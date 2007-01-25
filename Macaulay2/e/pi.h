/* pi.cc -- routines for packed integers and monomials -*- c++ -*- */
// pi = packed integer
// bits_per_fld = number of bits per subfield
// "fields" of type U are packed into "bins" of unsigned type T, either uint32_t or uint64_t
// the type U is either signed or unsigned
// encoding from type U to an unsigned field is done in such a way that the ordering is either preserved or reversed

// example:
//   n=3 bits per field
//   unsigned           :  0  1  2  3  4  5  6  7    ui      x -> 0+x    0
//   signed             : -4 -3 -2 -1  0  1  2  3    i       x -> 4+x    4 == 1 << (n-1)
//   unsigned, reversed :  7  6  5  4  3  2  1  0    ru      x -> 7-x    7 == (1 << n) - 1
//   signed, reversed   :  3  2  1  0 -1 -2 -3 -4    rui     x -> 3-x    3 == (1 << (n-1)) - 1

// an "area" will consist of a sequence of bins, each with the same number of bits per field; some fields in the last bin may be unused
// routines operating on an area will update references to pointers, so the next area can continue processing; thus area routines need not know their offset, just their length
// an "encoded monomial" will consist of a sequence of areas
// comparison of encoded monomials is always unsigned and lexicographic
// routines for monomial arithmetic have to take the type of conversion into account
#include "overflow.hpp"
#ifdef __GNUC__
#define expect_false(x) (__builtin_expect(x,0))
#define expect_true(x)  (__builtin_expect(x,1))
#else
#define expect_false(x) (x)
#define expect_true(x)  (x)
#endif
#include "pi-masks.h"
static const int bits_per_byte = 8;
template <typename T> struct masks { static inline T himask(int bits_per_fld); };
template <> inline uint32_t masks<uint32_t>::himask(int bits_per_fld) { return himask32[bits_per_fld]; }
template <> inline uint64_t masks<uint64_t>::himask(int bits_per_fld) { return himask64[bits_per_fld]; }
template <typename T> static T bool_add(T x, T y) { return x^y; }
template <typename T> static T bool_sub(T x, T y) { return x^y; }
template <typename T> static T bool_eq(T x, T y) { return ~(x^y); }
template <typename T> static T bool_neq(T x, T y) { return x^y; }
template <typename T> static T bool_imp(T x, T y) { return (~x) | y; }
template <typename T> static T bool_nimp(T x, T y) { return x & ~y; }
typedef enum { SIGNED, UNSIGNED, SIGNED_REVERSED, UNSIGNED_REVERSED } field_type;
template <typename T, typename U, int bits_per_fld, field_type type> class pui {
     static bool reversed() {
	  switch(type) {
	  case SIGNED: case UNSIGNED: return false;
	  case SIGNED_REVERSED: case UNSIGNED_REVERSED: return true; } }
     static int bits_per_bin() { return bits_per_byte * sizeof(T); }
     static int bits_per_U() { return bits_per_byte*sizeof(U); }
     static int fldbits_per_bin() { return bits_per_fld * flds_per_bin(); }
     static int extrabits_per_bin() { return bits_per_bin() - fldbits_per_bin() ; }
     static U mask_one_field() { return (~(U)0) >> (bits_per_U() - bits_per_fld); }
     static T mask_all_fields() { return (~(T)0) >> extrabits_per_bin(); }
     static T himask() { return masks<T>::himask(bits_per_fld); }
     static T ovmask() { return himask() << 1; }
     static U encoded_zero() {
	  switch(type) {
	  case SIGNED: return 1 << (bits_per_fld-1);
	  case UNSIGNED: return 0;
	  case SIGNED_REVERSED: return (1 << (bits_per_fld-1)) - 1;
	  case UNSIGNED_REVERSED: return mask_one_field(); }}
     static T encoded_zeroes() {
	  switch(type) {
	  case UNSIGNED: return 0;
	  case UNSIGNED_REVERSED: return mask_all_fields();
	  case SIGNED: return himask();
	  case SIGNED_REVERSED: return mask_all_fields() ^ himask();
	  }}
     static U field(T t, int i) { 
	  if (flds_per_bin() == 1) return t;
	  return (U)(t >> (bits_per_fld * (flds_per_bin() - 1 - i))) & mask_one_field();}
     static U field_at_bit(T t, int i) { 
	  if (flds_per_bin() == 1) return t;
	  U u = t >> i;
	  u &= mask_one_field();
	  u = u - encoded_zero();
	  if (reversed()) u = -u;
	  return u; }
     static U checkfit(U u) { 
	  if expect_false (0 != (u & mask_one_field())) safe::ov("overflow: pui"); 
	  return u ; }
     static T add(T x, T y, T &carries) {
	  T sum = x + y - encoded_zeroes();
	  carries |= bool_neq(bool_add(x,y),sum); // ???
	  if expect_false (extrabits_per_bin() == 0 && sum<x) safe::ov("overflow: pui + pui");
	  return sum; }
     static void add_final(T &carries) {
	  T oflows = carries & ovmask();
	  if expect_false (0 != oflows) safe::ov("overflow: pui + pui"); }
     static T sub(T x, T y, T &borrows) {
	  T dif = x - y + encoded_zeroes();
	  borrows |= bool_neq(bool_sub(x,y),dif); // ???
	  if expect_false (extrabits_per_bin() == 0 && x<y) safe::ov("overflow: pui - pui");
	  return dif; }
     static void sub_final(T &borrows) {
	  T oflows = borrows & ovmask();
	  if expect_false (0 != oflows) safe::ov("overflow: pui - pui"); }
public:
     static int flds_per_bin() { return bits_per_bin() / bits_per_fld; }
     static void pack(T *dest, U *src, int numfields) {
	  if (numfields == 0) return;
	  while (1) {
	       T t = 0;
	       for (int j = (flds_per_bin()-1)*bits_per_fld; j >= 0; j -= bits_per_fld) {
		    U u = *src++;
		    if (reversed()) u = -u;
		    u = u + encoded_zero();
		    checkfit(u);
		    t |= u << j;
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
	       for (int bit = (flds_per_bin()-1)*bits_per_fld; bit >= 0; bit -= bits_per_fld) {
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
	       for (int j = (flds_per_bin()-1)*bits_per_fld; j >= 0; j -= bits_per_fld) {
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
     static int cmp_lex(T x, T y) { return x > y ? 1 : x < y ? -1 : 0 ; }
     static int cmp_lex(T *x, T *y, int numbins) { 
	  for (int j = 0; j < numbins; j++) {
	       if expect_false (x[j] > y[j]) return  1; else
	       if expect_false (x[j] < y[j]) return -1;
	  }
	  return 0;
     }
     static T geq_each(T x, T y) {
	  if expect_true (extrabits_per_bin() == 0 && x<y) return false;
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

template <typename T, typename U, int bits_per_fld, int len, field_type type> class puiv {
     static void sub(T res[], T x[], T y[]) {
	  pui<T,U,bits_per_fld,type>::sub(res,x,y,len); }
};     


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s"
// End:
