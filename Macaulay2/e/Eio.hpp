// Copyright 1998 by Michael E. Stillman

#ifndef _io_hpp_
#define _io_hpp_

const int MAGIC_ZZ = 98;
const int MAGIC_ZZP = 99;	// Format: Magic_ZZP <int:P>
const int MAGIC_MONORDER = 100;
const int MAGIC_MONOID = 101;
const int MAGIC_POLYRING = 102;
const int MAGIC_FREEMODULE = 103;
const int MAGIC_LIST_VECTORS = 201;
const int MAGIC_MATRIX = 202;

const int MAGIC_VECTOR = 200;

const int MAGIC_MONORDER_END = 500;
const int MAGIC_END = 501;	// End of a block.  Should this be used?
#endif

#if 0
// First: define a ring
POLYRING
  ZZP 5
  COMMUTATIVE_MONOID
    MONOMIAL_ORDER
      ...
    WEIGHTS 1 2 3 4  (# = nvars)
    PRINT_ORDER 0 1 2 3

FREEMODULE
  (give component loc, Schreyer order info if any, and degrees of each element)

VECTORS <#vectors>
  VECTOR <#terms>
    coeff1 monomial1 coeff2 monomial2 ...

///////////////////////
// Here, a coeff is an <int>.  A monomial is an exponent vector, component pair.
// So for a polynomial ring, a term has length 1 + 1 + nvars.
// Non-commutative monomials are stored as [<len> e1 e2 e3 ... en component].
#endif
