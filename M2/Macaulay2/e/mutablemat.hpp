// Copyright 2005-2012  Michael E. Stillman

#ifndef _mutable_mat_hpp_
#define _mutable_mat_hpp_

/**
 * @file mutablemat.hpp
 * @brief Umbrella header that ties together `MutableMat` declarations, implementations, and the SLP variant.
 *
 * Pulls in `mutablemat-defs.hpp` (the templated `MutableMat<MatT>`
 * class declarations and the abstract `MutableMatrix` virtuals it
 * implements), `mutablemat-imp.hpp` (the inline template
 * implementations that forward `n_rows`, `n_columns`, `set_entry`,
 * the row / column ops, and the heavy linear-algebra routines into
 * the wrapped `DMat<R>` / `SMat<R>` storage), and the matching
 * `SLP-defs.hpp` / `SLP-imp.hpp` bodies so straight-line-program
 * code that consumes mutable matrices resolves in one include.
 * Including this header is what gives a translation unit the full
 * `MutableMatrix` virtual surface plus its specialised templated
 * back end at the appropriate compile-time instantiation.
 *
 * `MutableMatrix` itself (the abstract base) lives in `mat.hpp`;
 * this header is for the wrapper that bridges to the per-ring
 * `DMat<R>` / `SMat<R>` types declared in `dmat.hpp` and
 * `smat.hpp`.
 *
 * @see mat.hpp
 * @see mutablemat-defs.hpp
 * @see mutablemat-imp.hpp
 * @see dmat.hpp
 * @see smat.hpp
 */

#include "mutablemat-defs.hpp"
#include "SLP-defs.hpp"
#include "mutablemat-imp.hpp"
#include "SLP-imp.hpp"
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
