// Copyright 1995  Michael E. Stillman

#ifndef _ringelem_hh_
#define _ringelem_hh_

#include <gmp.h>

struct Nterm;

typedef Nterm *tpoly;

union ring_elem
{
  int    int_val;
  Nterm * poly_val;

  ring_elem() : poly_val(0) {}
  ring_elem(int a) : int_val(a) {}
  ring_elem(Nterm *a) : poly_val(a) {}

  operator int() const { return int_val; }
  operator tpoly() const { return poly_val; }

  ring_elem &operator=(int a) { int_val = a; return *this; }
  ring_elem &operator=(Nterm *a) { poly_val = a; return *this; }
};

struct Nterm
{
  Nterm *    next;
  ring_elem coeff;
  int       monom[1];
};

typedef struct vecterm *vec;
struct vecterm
{
  vec       next;
  int       comp;
  ring_elem coeff;
};

#endif
