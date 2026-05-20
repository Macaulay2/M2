/*****************************************************************************
 *   Copyright (C) 2011 by Mikhail V. Zinin                                  *
 *   mzinin@gmail.com                                                        *
 *                                                                           *
 *   You may redistribute this file under the terms of the GNU General       *
 *   Public License as published by the Free Software Foundation, either     *
 *   version 2 of the License, or any later version.                         *
 *****************************************************************************/

/**
 * @file bibasis/bibasis.cpp
 * @brief `rawBIBasis` --- C entry point bridging M2 to BIBasis's involutive-basis engine.
 *
 * Defines the single `extern "C"` symbol `rawBIBasis(Matrix*, int)`
 * that the M2 interpreter (via the `BIBasis` package) calls to
 * compute a Boolean involutive basis. The function constructs a
 * `BIBasis::Launcher`, forwards the input matrix and the
 * `toGroebner` flag, and returns the resulting matrix --- so this
 * file is the thinnest possible glue layer between the engine's
 * `Matrix` API and the BIBasis subsystem.
 *
 * The actual algorithm --- reducing inputs mod `x_i^2 - x_i`,
 * inserting into a `JanetTree`, walking prolongations until no
 * new polynomials appear --- lives behind `Launcher` and the
 * `Polynom`/`Monom` types. The `toGroebner` flag controls whether
 * the involutive output is reduced down to a Gröbner basis before
 * being returned.
 *
 * @see launcher.hpp
 * @see janettree.hpp
 * @see polynom.hpp
 */

#include "launcher.hpp"
#include "matrix.hpp"

extern "C" const Matrix* rawBIBasis(const Matrix* matrix, int toGroebner)
{
    BIBasis::Launcher launcher;
    
    return launcher.GetBIBasisMatrix(matrix, toGroebner);
}
