// (c) 1994 Michael E. Stillman
#ifndef _style_hh_
#define _style_hh_

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>

#include <gmp.h>

extern char newline[];

#if defined(__MWERKS__)
#define Random RandomFoo
#endif

#include "classes.hpp"
#include "error.hpp"
#include "buffer.hpp"
#include "mem.hpp"

typedef unsigned int unsigned_int;
typedef int *ptr_to_int;
typedef unsigned int *ptr_to_unsigned_int;

template <class T>
inline const T &min(const T &a, const T &b)
{ return (a<b ? a : b); }

template <class T>
inline const T &max(const T &a, const T &b)
{ return (a>b ? a : b); }

template <class T> 
inline void swap(T &t1, T &t2)
{
  T tmp = t1;
  t1 = t2;
  t2 = tmp;
}

const int LT = -1;
const int EQ = 0;
const int GT = 1;
const int INCOMPARABLE = 2;

typedef enum {FIRST, LAST} direction;

#endif
