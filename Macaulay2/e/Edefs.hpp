// Copyright 1998 by Michael Stillman
#ifndef __defs_hpp_
#define __defs_hpp_

#include <iostream.h>
#include "object.hpp"
//class ostream;
//class istream;

#if !defined(NULL)
#define NULL ((void *)0)
#endif

#if 0
const int EQ = 0;
const int LT = -1;
const int GT = 1;

class object
{
  friend void bump_up(const object *p);
  friend void bump_down(const object *p);

  int refcount;

public:
  object() : refcount(0) {}
  virtual ~object() {}
};

inline void bump_up(const object *p)
{
  object *q = (object *) p;
  q->refcount++;
}
inline void bump_down(const object *p)
{
  object *q = (object *) p;
  if (--q->refcount == 0) delete q;
}
#endif
#endif
