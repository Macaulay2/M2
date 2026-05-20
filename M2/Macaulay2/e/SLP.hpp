// Copyright 2015 Anton Leykin and Mike Stillman

// Anton Leykin's code in this file is in the public domain.

#ifndef _slp_hpp_
#define _slp_hpp_

/**
 * @file SLP.hpp
 * @brief Public umbrella header for the templated straight-line-program evaluator (`SLProgram` / `SLEvaluatorConcrete`).
 *
 * A straight-line program is a DAG whose nodes are inputs,
 * constants, and gate operations: the `SLProgram::GATE_TYPE`
 * enum in `SLP-defs.hpp` lists exactly `Copy`, `MCopy`, `Sum`,
 * `Product`, `MSum`, `MProduct`, `Det`, and `Divide` (the `M`
 * prefix marks the matrix-valued variant). Callers compile a
 * polynomial system or a Jacobian into one of these once and
 * then evaluate it many times --- a single forward sweep that
 * shares common subexpressions across outputs and stays cache
 * friendly relative to walking a polynomial AST. Derivatives
 * for Newton-step paths are kept as separately-compiled SLPs
 * for the relevant Jacobian (see `PathTracker`'s `slpH` /
 * `slpHxt` / `slpHxtH` / `slpHxH` fields in `NAG.hpp` --- the
 * trailing `|H` variants pair the derivative with the homotopy
 * for in-step substitution), not produced by reverse-mode
 * autodiff inside the SLP.
 *
 * The implementation is split across three files so the same
 * code can be instantiated over the engine's numerical rings
 * `M2::ARingRR`, `M2::ARingCC`, `M2::ARingRRR`, `M2::ARingCCC`:
 * `SLP-defs.hpp` declares `SLProgram`, `HomotopyAlgorithm`, and
 * the value-type templates, `SLP-imp.hpp` provides
 * `SLEvaluatorConcrete<RT>` and the homotopy implementations,
 * and `SLP.cpp` carries the non-template glue. A separate older
 * `StraightLineProgram` / `SLP<Field>` design in `NAG.hpp` is
 * what `NAG.cpp`'s `PathTracker` actually uses.
 *
 * @see NAG.hpp
 * @see SLP-defs.hpp
 * @see SLP-imp.hpp
 */

#include "mutablemat-defs.hpp"
#include "SLP-defs.hpp"
#include "SLP-imp.hpp"
#include "mutablemat-imp.hpp"

#include "buffer.hpp"
#include "matrix.hpp"
#include "aring-glue.hpp"

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
