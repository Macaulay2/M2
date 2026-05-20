#pragma once

/**
 * @file unit-tests/util-polyring-creation.hpp
 * @brief One-line helpers for building degree monoids and polynomial rings inside gtest cases.
 *
 * Declares `degreeMonoid(names)` and the two overloads of
 * `degreeRing(...)` (variable-name list or fixed `ndegrees`)
 * that wrap the engine-side boilerplate of constructing a
 * degree monoid; plus `simplePolynomialRing(kk, names,
 * monorder)` and its variant `simplePolynomialRing(p, names)`
 * which builds a `Z/p` GRevLex polynomial ring with all
 * degree-1 variables. Tests pull these in to replace ~10 lines
 * of monomial-order / monoid / ring plumbing per fixture.
 *
 * Heavily used by `NewF4Test.cpp`, `NCGroebnerTest.cpp`,
 * `MatrixIOTest.cpp`, and `PolyRingTest.cpp`. Companion to the
 * `.cpp` implementation file under the same `file-test-harness`
 * markdown family.
 *
 * @see polyring.hpp
 * @see monoid.hpp
 * @see interface/monomial-ordering.h
 * @see NewF4Test.cpp
 */

#include <iostream>
#include <memory>
#include <gtest/gtest.h>
#include <vector>

#include "interface/monomial-ordering.h"
#include "monordering.hpp"
#include "interface/ring.h"
#include "interface/aring.h"
#include "monoid.hpp"
#include "poly.hpp"
#include "polyring.hpp"

const Monoid* degreeMonoid(const std::vector<std::string>& names);

const PolynomialRing* degreeRing(const std::vector<std::string>& names);

const PolynomialRing* degreeRing(int ndegrees);// TODO: currently requires ndegrees is 1!

// This create a polynomial ring with all degrees 1.
const PolynomialRing* simplePolynomialRing(const Ring* kk,
                                           const std::vector<std::string>& names,
                                           MonomialOrdering* monorder);

// This create a polynomial ring with all degrees 1, and with GRevLex order
const PolynomialRing* simplePolynomialRing(int p, const std::vector<std::string>& names);

// Local Variables:
// indent-tabs-mode: nil
// End:
