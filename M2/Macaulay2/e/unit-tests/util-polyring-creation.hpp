#pragma once

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
