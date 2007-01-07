// Copyright 1995  Michael E. Stillman

#ifndef _ringelem_hh_
#define _ringelem_hh_

#include <gmp.h>
#include "newdelete.hpp"

struct Nterm;

typedef Nterm *tpoly;

union ring_elem
{

  int    int_val;
  Nterm * poly_val;
 private:			// move this line up to the top eventually
  mpz_ptr  mpz_val;

 public:
  ring_elem() : poly_val(0) {}
  //explicit ring_elem(int a) : int_val(a) {} // really want this version...
  ring_elem(int a) : int_val(a) {}
  ring_elem(Nterm *a) : poly_val(a) {}
  ring_elem(mpz_ptr a) : mpz_val(a) {}

  operator int() const { return int_val; }
  operator tpoly() const { return poly_val; }

  int     get_int() const { return int_val; }
  Nterm * get_poly() const { return poly_val; }
  mpz_ptr get_mpz() const { return mpz_val; }

  ring_elem &operator=(int a) { int_val = a; return *this; }
  ring_elem &operator=(Nterm *a) { poly_val = a; return *this; }
  // ring_elem &operator=(mpz_t a) { mpz_val = a; return *this; }

};

struct Nterm
{
  Nterm *    next;
  ring_elem coeff;
  int       monom[1];
};

typedef struct vecterm *vec;
struct vecterm : public our_new_delete
{
  vec       next;
  int       comp;
  ring_elem coeff;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
