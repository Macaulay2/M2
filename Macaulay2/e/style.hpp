// (c) 1994 Michael E. Stillman
#ifndef _style_hh_
#define _style_hh_


#include <iostream.h>
#include <iomanip.h>
#include <strstream.h>

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>

#include <gmp.h>

#include "classes.hpp"
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

typedef unsigned long int ulong;

inline ulong range_safe_add(ulong a, ulong b)
{
  ulong c = a + b;
  if (c < a)
    {
      cerr << "ulong integer addition overflow" << endl;
      exit(-1);
    }
  return c;
}

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
