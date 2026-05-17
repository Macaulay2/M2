# `dmat-ffpack.cpp` — historical FFLAS-FFPACK dispatch (no longer in use)

`dmat-ffpack.cpp` is a **legacy / historical file** that was once the
FFLAS-FFPACK-routed implementation of `DMat<R>` linear-algebra
operations. The file body is gated under `#if 0`; the functionality
has moved to `dmat.cpp` and the templated `mat-linalg.hpp` machinery.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Header preamble

```cpp
#if 0
// This file is not in use.  These functions are now in dmat.cpp.
#if 0
    template <typename CoeffRing>
    template <class RingType>
    size_t DMat<CoeffRing>::rank(
        typename enable_if<is_givaro_or_ffpack<RingType>::value>::type *dummy) const {
        // assert not necessary because the test is already done by  "enable_if<…>"
        std::cout << "Calling rankGF_or_FFPACK" << std::endl;
        ElementType *N = newarray(ElementType, n_rows() * n_cols());
        /// @jakob replace with memcopy or something fast.
        /// @jakob potention problem: (n_rows() * n_cols()) - overflow for big matrices
```

The double `#if 0` is the giveaway: the whole file is dead code, kept
around for reference. The "@jakob" inline comments are notes to the
file's previous owner about subsequent improvements that were folded
into the live code path.

## What used to live here

Before the engine standardised on
[`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) plus templated
linear algebra in [`file-mat-linalg.md`](file-mat-linalg.md), each
combination of `(coefficient ring, operation)` had its own
hand-written dispatcher. `dmat-ffpack.cpp` was the FFLAS-FFPACK
dispatcher for `rank`, LU, and friends — using `enable_if` to
constrain template instantiations to FFLAS-compatible rings (Z/p
finite fields via FFLAS, Givaro-backed GF).

The replacement pattern (in `dmat.cpp` and templated headers) is
cleaner: each ring's specialised `DMat<R>` carries the relevant
back-end inline; no separate dispatcher file is needed.

## Status

The file is kept for **historical reference** and so that grep
searches for "FFPACK" turn up this code path's origin. It is not
compiled into the engine; CMake / autotools build rules exclude it.

A future cleanup pass will likely delete the file entirely.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-aring-zzp-ffpack.md`](file-aring-zzp-ffpack.md) — current
  FFLAS-FFPACK-backed aring.
- [`file-dmat.md`](file-dmat.md), [`file-mat-linalg.md`](file-mat-linalg.md)
  — successor implementations.
- fflas-ffpack & givaro under
  [`../../submodules/README.md`](../../submodules/README.md).
