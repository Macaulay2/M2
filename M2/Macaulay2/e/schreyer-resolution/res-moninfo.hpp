// Copyright 2016  Michael E. Stillman

#ifndef _res_moninfo_hpp_
#define _res_moninfo_hpp_

/**
 * @file schreyer-resolution/res-moninfo.hpp
 * @brief `ResMonoid` dispatcher --- single typedef switch between `ResMonoidDense` and `ResMonoidSparse`.
 *
 * Pulls in both monomial-layout implementations the F4
 * resolution uses and aliases `ResMonoid` to the active one ---
 * `ResMonoidDense` is the live `using` in production, with a
 * commented-out `using ResMonoid = ResMonoidSparse;` line kept
 * directly beneath it as the swap-in toggle. The dense layout
 * stores each monomial in a fixed `nslots`-word array
 * `[hash, component, weights..., exponents_0..exponents_{nvars-1}]`
 * and wins for dense exponents or small variable counts; the
 * sparse layout uses a length-prefixed `(variable, exponent)`
 * list (its `monomial_size(m)` returns `*m`) and wins for
 * low-support monomials in many-variable rings.
 *
 * The two implementations share most of their public
 * surface --- `mult` / `divide` / `monomial_size` /
 * `to_expvector` / `from_expvector` / `from_varpower_monomial`
 * / `compare_schreyer` are live in both. `compare_grevlex` is
 * live only in `ResMonoidSparse`; the dense version has it
 * `#if 0`-d out, so switching the typedef silently changes
 * which grevlex comparator the rest of the resolution sees.
 * The rest of the schreyer-resolution code references
 * `ResMonoid` through this typedef, which keeps the
 * implementation choice pinned to a single file and lets
 * benchmark-driven swaps stay zero-touch at call sites; future
 * builds may promote the choice to configure-time, but for now
 * the source-comment toggle is the only mechanism.
 *
 * @see res-moninfo-dense.hpp
 * @see res-moninfo-sparse.hpp
 * @see res-poly-ring.hpp
 * @see res-schreyer-frame.hpp
 */

#include "schreyer-resolution/res-moninfo-dense.hpp"
#include "schreyer-resolution/res-moninfo-sparse.hpp"

using ResMonoid = ResMonoidDense;
// using ResMonoid = ResMonoidSparse;

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
