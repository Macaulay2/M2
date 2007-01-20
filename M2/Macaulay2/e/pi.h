/* pi.cc -- routines for packed integers and monomials -*- c++ -*- */
// pi = packed integer
// piv = packed integer vector
// pui = packed unsigned integer
// puiv = packed unsigned integer vector
// pgi = packed integer, with one extra guard bit per field
// pgiv = packed integer vector, with one extra guard bit per field
// pgui = packed unsigned integer, with one extra guard bit per field
// pguiv = packed unsigned integer vector, with one extra guard bit per field
// nbits = number of bits per subfield
// T = type of int to pack them into, either int32_t or int64_t
#include "overflow.hpp"
#ifdef __GNUC__
#define expect_false(x) (__builtin_expect(x,0))
#define expect_true(x)  (__builtin_expect(x,1))
#else
#define expect_false(x) (x)
#define expect_true(x)  (x)
#endif
#include "pi-masks.h"
template <typename T> struct masks { static inline T lomask(int nbits), himask(int nbits); };
template <> inline int32_t masks<int32_t>::lomask(int nbits) { return lomask32[nbits]; }
template <> inline int32_t masks<int32_t>::himask(int nbits) { return himask32[nbits]; }
template <> inline int64_t masks<int64_t>::lomask(int nbits) { return lomask64[nbits]; }
template <> inline int64_t masks<int64_t>::himask(int nbits) { return himask64[nbits]; }
template <typename T> static T bool_add(T x, T y) { return x^y; }
template <typename T> static T bool_sub(T x, T y) { return x^y; }
template <typename T> static T bool_eq(T x, T y) { return ~(x^y); }
template <typename T> static T bool_neq(T x, T y) { return x^y; }
template <typename T> static T bool_imp(T x, T y) { return (~x) | y; }
template <typename T> static T bool_nimp(T x, T y) { return x & ~y; }
template <typename T, int nbits> class pui {
     static int numbits() { return 8 * sizeof(T); }
     static int numfields() { return numbits() / nbits; }
     static int extrabits() { return numbits() - nbits * numfields(); }
     static T lomask() { return masks<T>::lomask(nbits); }
     static T himask() { return masks<T>::himask(nbits); }
     static T add(T x, T y, T &carries) {
	  T sum = x + y;
	  carries |= bool_neq(bool_add(x,y),sum);
	  if expect_false (extrabits() == 0 && sum<x) safe::ov("overflow: pui + pui");
	  return sum; }
     static void add_final(T &carries) {
	  T oflows = carries & lomask();
	  if expect_false (0 != oflows) safe::ov("overflow: pui + pui"); }
     static T sub(T x, T y, T &borrows) {
	  T dif = x - y;
	  borrows |= bool_neq(bool_sub(x,y),dif);
	  if expect_false (extrabits() == 0 && x<y) safe::ov("overflow: pui - pui");
	  return dif; }
     static void sub_final(T &borrows) {
	  T oflows = borrows & lomask();
	  if expect_false (0 != oflows) safe::ov("overflow: pui - pui"); }
public:
     static T add(T x, T y) {
	  T carries = 0;
	  T res = add(x,y,carries);
	  add_final(carries);
	  return res; }
     static void add(int len, T res[], T x[], T y[]) {
	  T carries = 0;
	  for (int j=0; j<len; j++) res[j] = add(x[j], y[j], carries);
	  add_final(carries); } 
     static T geq_each(T x, T y) {
	  if expect_true (extrabits() == 0 && x<y) return false;
	  T dif = x - y;
	  T borrows = bool_neq(bool_sub(x,y),dif);
	  T oflows = borrows & lomask();
	  if expect_true (0 != oflows) return false;
	  return true;
     }
     static T sub(T x, T y) {
	  T borrows = 0;
	  T res = sub(x,y,borrows);
	  sub_final(borrows);
	  return res; }
     static void sub(int len, T res[], T x[], T y[]) {
	  T borrows = 0;
	  for (int j=0; j<len; j++) res[j] = sub(x[j], y[j], borrows);
	  sub_final(borrows); } 
};
template <typename T, int nbits, int len> class puiv {
     static void sub(T res[], T x[], T y[]) {
	  pui<T,nbits>::sub(len,res,x,y); }
};     


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e pi-demangled.s"
// End:
