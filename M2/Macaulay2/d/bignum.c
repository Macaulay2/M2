/* Copyright 1995 by Daniel R. Grayson */

#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>
#include <string.h>

#include "bignum.h"
#undef malloc
#define malloc GC_malloc
#define newinteger(n) 
#define makeinteger(_negative,_len,_body) ({ \
	  int _size = _len*sizeof(digit); \
  	  integer _z=(integer)GC_malloc(sizeof(struct INTEGER)+_size); \
	  _z->negative = _negative; \
	  _z->len = _len; \
	  memcpy(_z->body,_body,_size); \
	  _z; \
	  })
#define true 1
#define false 0

enum COMPARE {LESS=-1, EQUAL=0, GREATER=1};

#define divroundup(i,j) (((i) + (j) - 1)/ (j))

__inline__ int integer_sizeof(integer x) {
     return sizeof(struct INTEGER) + x->len * sizeof(digit);
     }

__inline__ int integer_space(int len) {
     return sizeof(struct INTEGER) + len * sizeof(digit);
     }

static enum COMPARE compare_body(integer x, integer y) {
     int xlen = x->len;
     int ylen = y->len;
     digit *xbody = x->body;
     digit *ybody = y->body;
     int i;
     if (xlen > ylen) return GREATER;
     if (xlen < ylen) return LESS;
     for (i=xlen-1; i>=0; i--) {
	  if (xbody[i] > ybody[i]) return GREATER;
	  if (xbody[i] < ybody[i]) return LESS;
	  }
     return EQUAL;
     }

static int compare(integer x, integer y) {
     if (x->negative) {
     	  if (y->negative) return compare_body(y,x);
	  else return LESS;
	  }
     else {
	  if (y->negative) return GREATER;
	  else return compare_body(x,y);
	  }
     }

static void add(integer x, integer y, integer z) {
     /* x + y stored in z, which must have space for as many digits as are in
        x or in y, together with a possible carry. */
     int i;
     doubledigit s=0;
     if (y->len < x->len) {
	  for (i=0; i<y->len; i++) {
	       s += x->body[i]; 
	       s += y->body[i]; 
	       z->body[i] = s; 
	       s >>= BITS(digit);
	       }
	  for (i=y->len; i<x->len; i++) {
	       s += x->body[i]; 
	       z->body[i] = s; 
	       s >>= BITS(digit);
	       }
	  }
     else {
	  for (i=0; i<x->len; i++) {
	       s += x->body[i]; 
	       s += y->body[i]; 
	       z->body[i] = s; 
	       s >>= BITS(digit);
	       }
	  for (i=x->len; i<y->len; i++) {
	       s += y->body[i]; 
	       z->body[i] = s; 
	       s >>= BITS(digit);
	       }
	  }
     if (s != 0) z->body[i++] = s;
     z->len = i;
     }

static void sub(integer x, integer y, integer z) {
     /* x - y stored in z, which must have space for as many digits as are in x */
     /* it is assumed that x >= y */
     int i;
     signeddoubledigit s=0;
     for (i=0; i<y->len; i++) {
	  s += x->body[i]; 
	  s -= y->body[i]; 
	  z->body[i] = s; 
	  s >>= BITS(digit);
	  }
     for (i=y->len; i<x->len; i++) {
	  s += x->body[i]; 
	  z->body[i] = s; 
	  s >>= BITS(digit);
	  }
     for (; i>0 && z->body[i-1]==0; i--) {}
     z->len = i;
     }

#define max(i,j) ({ int _i = i, _j = j; _i < _j ? _j : _i; })

#define NUMSMALL 300
integer __smallints[2*NUMSMALL+1];
integer *smallints = __smallints + NUMSMALL;

static void initialize() {
     int i;
     {
	  integer x = (integer) malloc(sizeof(struct INTEGER));
     	  x->len = 0;
	  x->negative = false;
	  smallints[0] = x;
	  }
     for (i=1; i<=NUMSMALL; i++) {
	  integer x = (integer) malloc(sizeof(struct INTEGER) + sizeof(digit));
     	  x->len = 1;
	  x->negative = false;
	  x->body[0] = i;
	  smallints[i] = x;
	  }
     for (i=1; i<=NUMSMALL; i++) {
     	  integer y = (integer) malloc(sizeof(struct INTEGER) + sizeof(digit));
     	  y->len = 1;
	  y->negative = true;
	  y->body[0] = i;
	  smallints[-i] = y;
	  }
     }

integer toInteger(int i) {
     integer x;
     if (i < 0) {
	  if (-i <= NUMSMALL) return smallints[i];
	  x = (integer) malloc(sizeof(struct INTEGER) + sizeof(digit));
	  x->len = 1;
     	  x->negative = true;
	  x->body[0] = -i;
     	  return x;
	  }
     else if (i == 0) {
	  return smallints[0];
	  }
     else {
	  if (i <= NUMSMALL) return smallints[i];
	  x = (integer) malloc(sizeof(struct INTEGER) + sizeof(digit));
	  x->len = 1;
     	  x->negative = false;
	  x->body[0] = i;
     	  return x;
	  }
     }

__inline__ int toInteger_space(int i) {
     return integer_space(divroundup(sizeof(int),sizeof(digit)));
     }

void toInteger_d(int i, integer x) {
     if (i < 0) {
	  x->negative = true;
	  if (sizeof(int) <= sizeof(digit) || (unsigned)(-i) <= MAXDIGIT) {
	       x->len = 1;
	       x->body[0] = -i;
	       }
	  else {
	       x->len = 2;
	       i = -i;
	       x->body[0] = i;
	       x->body[1] = i >> BITS(digit);
	       }
	  }
     else if (i == 0) {
	  x->negative = false;
	  x->len = 0;
	  }
     else {
     	  x->negative = false;
	  if (sizeof(int) <= sizeof(digit) || (unsigned)(i) <= MAX(digit)) {
	       x->len = 1;
	       x->body[0] = i;
	       }
	  else {
	       x->len = 2;
	       x->body[0] = i;
	       x->body[1] = i >> BITS(digit);
	       }
	  }
     }

static __inline__ integer raw_copy(integer x) {
     int n = integer_sizeof(x);
     integer z = (integer) malloc(n);
     memcpy(z,x,n);
     }

integer copy(integer w) {
     if (w->len == 0) return smallints[0];
     else if (w->len == 1 && w->body[0] <= NUMSMALL) {
	  if (w->negative) return smallints[-w->body[0]];
	  else return smallints[w->body[0]];
	  }
     else return raw_copy(w);
     }

__inline__ int integer_plus_space(integer x,integer y) {
     return sizeof(struct INTEGER) + (max(x->len, y->len) + 1) * sizeof(digit);
     }

__inline__ int integer_diff_space(integer x,integer y) {
     return sizeof(struct INTEGER) + (max(x->len, y->len) + 1) * sizeof(digit);
     }

void integer_plus_d(integer x,integer y,integer z) {
     if (x->negative) {
	  if (y->negative) { z->negative = true; add(x,y,z); }
	  else {
	       switch (compare_body(x,y)) {
		    case GREATER: { z->negative = true; sub(x,y,z); break; }
		    case EQUAL: { z->negative = false; z->len = 0; break; }
		    default:
		    case LESS: { z->negative = false; sub(y,x,z); break; }
		    }
	       }
	  }
     else {
	  if (y->negative) {
	       switch (compare_body(y,x)) {
		    case GREATER: { z->negative = true; sub(y,x,z); break; }
		    case EQUAL: { z->negative = false; z->len = 0; break; }
		    default:
		    case LESS: { z->negative = false; sub(x,y,z); break; }
		    }
	       }
	  else { z->negative = false; add(x,y,z); }
	  }
     }

void integer_diff_d(integer x,integer y,integer z) {
     if (x->negative) {
	  if (! y->negative) { z->negative = true; add(x,y,z); }
	  else {
	       switch (compare_body(x,y)) {
		    case GREATER: { z->negative = true; sub(x,y,z); break; }
		    case EQUAL: { z->negative = false; z->len = 0; break; }
		    default:
		    case LESS: { z->negative = false; sub(y,x,z); break; }
		    }
	       }
	  }
     else {
	  if (! y->negative) {
	       switch (compare_body(y,x)) {
		    case GREATER: { sub(y,x,z); z->negative = true; break; }
		    case EQUAL: { z->len = 0; z->negative = false; break; }
		    default: 
		    case LESS: { sub(x,y,z); z->negative = false; break; }
		    }
	       }
	  else { z->negative = false; add(x,y,z); }
	  }
     }

integer integer_plus(integer x, integer y) {
     integer z = (integer) alloca(integer_plus_space(x,y));
     integer_plus_d(x,y,z);
     return copy(z);
     }

integer integer_plus_integer_int(integer x, int i) {
     integer y = (integer)alloca(toInteger_space(i));
     toInteger_d(i,y);
     return integer_plus(x,y);
     }

integer integer_diff(integer x, integer y) {
     integer z = (integer) alloca(integer_diff_space(x,y));
     integer_diff_d(x,y,z);
     return copy(z);
     }

__inline__ int integer_minus_space(integer x) {
     return integer_sizeof(x);
     }

__inline__ void integer_minus_d(integer x, integer z) {
     memcpy(z,x,integer_sizeof(x));
     z->negative = ! x->negative;
     }

integer integer_minus(integer x) {
     if (x->len == 0) return x;
     else if (x->len == 1 && x->body[0] <= NUMSMALL) {
	  if (x->negative) return smallints[ x->body[0]];
	  else return smallints[-x->body[0]];
	  }
     else {
	  integer z = raw_copy(x);
     	  z->negative = ! x->negative;
	  return z;
	  }
     }

__inline__ int integer_times_space(integer x, integer y) {
     return integer_space(x->len + y->len + 1);
     }

#define LIM 4

static void integer_times_dd(int xlen, digit *x,int ylen, digit *y, digit *z) {
     /* z has enough space and is cleared to zero */
     if (xlen > ylen) {
	  digit *p;
	  int len = xlen;
	  xlen = ylen;
	  ylen = len;
	  p = x;
	  x = y;
	  y = p;
	  }
     if (xlen < LIM || ylen < LIM || true) {
	  for (; xlen > 0; xlen--, x++, z++) {
	       digit xdigit = *x;
	       register int j;
	       register doubledigit s = 0;
	       for (j=0; j<ylen; j++) {
		    z[j] = s += z[j] + (doubledigit)xdigit * y[j];
		    s >>= BITS(digit);
		    }
	       z[j] += s;
	       }
	  }
     }

void integer_times_d(integer x, integer y, integer z) {
     int len;
     if (x->len == 0 || y->len == 0) {
	  z->negative = false;
	  z->len = 0;
	  return;
	  }
     len = x->len + y->len;
     memset(z,0,integer_space(len));
     integer_times_dd(x->len,x->body,y->len,y->body,z->body);
     z -> negative = x -> negative ^ y -> negative;
     if (z->body[len-1] == 0) len--;
     z->len = len;
     }

integer integer_times(integer x, integer y) {
     integer z = (integer) alloca(integer_times_space(x,y));
     integer_times_d(x,y,z);
     return copy(z);
     }

/* testing routines below this point */
     
struct {bool negative; short len; digit body[3];} 
a = { false, 3, {0x85858585U, 0x85858585U, 0x85858585U} },
b = { true,  2, {0x51515151, 0x51515151} };

void print(char *s, integer x) {
     int i;
     printf("%-20s = ",s);
     if (x->negative) putchar('-'); else putchar(' ');
     for (i=x->len-1; i>=0; i--) {
	  printf("%08lx",x->body[i]);
	  }
     putchar('\n');
     }

static integer x1 = (integer)&a;
static integer x2 = (integer)&b;

int main(){
     integer z,w,t;
     initialize();
     print("x1",x1);
     print("x2",x2);
     z = integer_plus(x1,x2);
     print("x1 + x2",z);
     w = integer_diff(z,x2);
     print("x1 + x2 - x2",w);
     t = integer_plus(x1,x1);
     print("x1 + x1",t);
     print("- t", integer_minus(t));
     print("0x123",toInteger(0x123));
     print("-0x123",toInteger(-0x123));
     print("0x41-0x123",integer_diff(toInteger(0x41),toInteger(0x123)));
     print("-0x41-0x123",integer_diff(toInteger(-0x41),toInteger(0x123)));
     print("-0x41+0x123",integer_plus(toInteger(-0x41),toInteger(0x123)));
     print("-0x41+-0x123",integer_plus(toInteger(-0x41),toInteger(-0x123)));
     print("0x41+-0x123",integer_plus(toInteger(0x41),toInteger(-0x123)));
     print("0x41+0x123",integer_plus(toInteger(0x41),toInteger(0x123)));
     printf("x1 ? x2 = %d\n",compare(x1,x2));
     print("x1 x2", integer_times(x1,x2));
     print("(x1 x2) w", integer_times(integer_times(x1,x2),w));
     print("x1 (x2 w)", integer_times(x1,integer_times(x2,w)));
     printf("(x1 - x2)(x1 + x2) ? x1^2 - x2^2 == %d\n",
	  compare(
	       integer_times(
		    integer_diff(x1,x2),
		    integer_plus(x1,x2)),
	       integer_diff(
		    integer_times(x1,x1),
		    integer_times(x2,x2))));
     exit(0);
     }
