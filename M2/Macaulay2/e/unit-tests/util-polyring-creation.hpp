#ifndef M2_UNIT_TESTS_UTIL_POLYRING_CREATION_HPP
#define M2_UNIT_TESTS_UTIL_POLYRING_CREATION_HPP

#include <iostream>
#include <memory>
#include <gtest/gtest.h>
#include <utility>
#include <vector>

#include "interface/monomial-ordering.h"
#include "monomials/monordering.hpp"
#include "interface/ring.h"
#include "interface/aring.h"
#include "monoid.hpp"
#include "rings/poly.hpp"
#include "rings/polyring.hpp"

const Monoid* degreeMonoid(const std::vector<std::string>& names);

const PolynomialRing* degreeRing(const std::vector<std::string>& names);

const PolynomialRing* degreeRing(int ndegrees);// TODO: currently requires ndegrees is 1!

// This create a polynomial ring with all degrees 1.
const PolynomialRing* simplePolynomialRing(const Ring* kk,
                                           const std::vector<std::string>& names,
                                           MonomialOrdering* monorder);

// This create a polynomial ring with all degrees 1, and with GRevLex order
const PolynomialRing* simplePolynomialRing(int p, const std::vector<std::string>& names);

// Creates a skew commutative ring, with degree rank one, GRevLex monomial
// order.  skewvars lists the indices of the anti-commuting variables.
const PolynomialRing* simpleSkewPolynomialRing(int p,
                                               const std::vector<std::string>& names,
                                               const std::vector<int>& skewvars);

// Creates a Weyl algebra, with degree rank one, GRevLex monomial order.
const WeylAlgebra* simpleWeylAlgebra(long p,
                                     const std::vector<std::string> varnames,
                                     const std::vector<int> comms,
                                     const std::vector<int> derivs);

class Matrix;

// Helpers for testing Ring::makeTerm.

// Build a varpower [2n+1, v1, e1, ..., vn, en] from (variable, exponent)
// pairs.  The pairs must be in decreasing order of variable.
std::vector<int> varpowerOf(const std::vector<std::pair<int, int>>& pairs);

// The same monomial, built with the ring's own arithmetic, so that the result
// of makeTerm can be compared against something built a different way.
ring_elem monomialOf(const Ring* R, const std::vector<std::pair<int, int>>& pairs);

// Create a 1-row matrix (ideal generators) from polynomial strings.
// Each string is a polynomial like "x^2+3*x*y-1".
const Matrix* idealFromStrings(const PolynomialRing* R,
                               const std::vector<std::string>& polys);

// Compute a Groebner basis of the ideal given by the 1-row matrix M.
// Returns a 1-row matrix whose columns are the GB elements.
const Matrix* computeGB(const Matrix* M);

// Create quotient ring R / (generators).
// Computes the GB of the generators, then forms the quotient.
const Ring* simpleQuotientRing(const PolynomialRing* R,
                               const std::vector<std::string>& generators);

#endif
// Local Variables:
// indent-tabs-mode: nil
// End:
