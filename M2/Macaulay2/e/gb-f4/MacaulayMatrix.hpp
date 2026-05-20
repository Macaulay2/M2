#pragma once

/**
 * @file gb-f4/MacaulayMatrix.hpp
 * @brief `newf4::MacaulayMatrix` --- batched matrix of S-polynomials and tail-reducers (refactor stub).
 *
 * Declares the F4-side Macaulay matrix the new GB engine
 * row-reduces once per degree: rows are S-polynomials together
 * with selected basis tail-reducers, columns are the monomials
 * appearing in any row sorted by the monomial order, and entries
 * are sparse coefficients drawn from the `VectorArithmetic`
 * backend. Each echelon row whose leading column is new is
 * promoted to a basis element; the matrix is then discarded and
 * the driver advances to the next degree. This is the batching
 * that makes F4 fast --- instead of reducing one S-polynomial at
 * a time, every pair of the current degree shares its tail-
 * reduction work through the matrix.
 *
 * Header is intentionally minimal at this point in the refactor
 * (the `Row` / `Column` structs are placeholders, and only the
 * `mMonomials` column-index table is populated); see the
 * `TODO-refactor-f4` note in this subdir for the planned API.
 * The mature counterpart in `f4/` still lives next door.
 *
 * @see MonomialHashTable.hpp
 * @see GBF4Computation.hpp
 * @see SPairs.hpp
 * @see Basis.hpp
 */

#include "MonomialHashTable.hpp"

namespace newf4 {

class MacaulayMatrix
{
 private:
  MonomialHashTable mMonomials;
};

struct Column
{
};

struct Row
{
};

} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End: