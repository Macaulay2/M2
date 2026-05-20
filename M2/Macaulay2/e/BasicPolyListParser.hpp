/**
 * @file BasicPolyListParser.hpp
 * @brief Parsers from text (string or file) into a `BasicPolyList`, including the Msolve input format.
 *
 * Three free functions read polynomial systems from text and
 * return a portable `BasicPolyList`. `parseMsolveFromString` /
 * `parseMsolveFile` consume the input format of the external
 * Msolve solver: the first non-comment line lists the variables
 * (and the Msolve-style characteristic / `]` lines are skipped),
 * after which every remaining non-comment line is parsed as one
 * polynomial. `parseBasicPolyListFromString(contents, varnames)`
 * consumes the engine's simpler "one polynomial per line, `#`
 * starts a comment" text format, building the
 * variable-name-to-index table from the caller-supplied
 * `varnames`. The free-function shape reflects that parsers
 * carry no state across calls --- text in, polynomials out.
 *
 * Because `BasicPolyList` is ring-agnostic, these parsers run
 * before any `PolynomialRing` is constructed, which makes them
 * suitable for early pipeline stages: test-input wiring, the
 * `gb-f4` streaming `PolynomialList` consumer, and the round-trip
 * half of Msolve dispatch.
 *
 * @see BasicPoly.hpp
 * @see BasicPolyList.hpp
 */

// This class implements parsing of polynomials from a string or file
// as well as Msolve format.
#pragma once


#include <string>
#include <vector>

#include "BasicPolyList.hpp"

BasicPolyList parseMsolveFromString(std::string contents); // requires Msolve header to determine variables, etc.
BasicPolyList parseMsolveFile(std::string filename);

BasicPolyList parseBasicPolyListFromString(std::string contents, std::vector<std::string> varnames);

// Local Variables:
// indent-tabs-mode: nil
// End:
