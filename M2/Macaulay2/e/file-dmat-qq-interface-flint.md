# `dmat-qq-interface-flint.hpp` — FLINT-mat translation for legacy GMP-based `DMat<ARingQQ>`

`dmat-qq-interface-flint.hpp` is a **bridge layer** that lets the
engine perform fast FLINT-matrix arithmetic on a
[`DMat<M2::ARingQQ>`](file-dmat.md) (which stores elements as GMP
`mpq_t` values, not FLINT `fmpq_t`). It translates between the two
representations on the way into and out of FLINT calls.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Why this layer exists

The default `ARingQQ` typedef in
[`file-aring-qq.md`](file-aring-qq.md) points at `ARingQQGMP`, so
`DMat<ARingQQ>` stores entries as GMP `mpq_t`s. But FLINT's matrix
routines (`fmpq_mat_*`) are much faster — and they require FLINT
storage (`fmpq_t`).

This file provides:

- A `FlintZZMat` helper class that wraps a FLINT integer matrix
  alongside the GMP-typed `DMat<ARingQQ>`.
- Conversion routines that copy entries `mpq_t → fmpq_t` on the way
  in and back the other way on the way out.

The header comment is explicit about the temporary nature of this
arrangement:

```cpp
// This class is designed to use DMat<M2::ARingQQ>, which stores elements as gmp ints
// This sets up flint fmpq_mat matrices, and provides translation.  This is
// significantly faster than doing the operations in a naive manner.
// This will become un-needed once DMat<ARingQQ> starts using flint integers/rationals.
```

When the default `ARingQQ` switches to `ARingQQFlint`
([`file-aring-qq-flint.md`](file-aring-qq-flint.md)), the engine
will use [`file-dmat-qq-flint.md`](file-dmat-qq-flint.md) directly
and this translation file can be removed.

## `FlintZZMat`

A small RAII wrapper:

```cpp
class FlintZZMat {
public:
    // ... fmpq_mat_t mMat plus init/clear/translation methods ...
};
```

The class allocates a FLINT matrix, copies an `mpq_t`-based
`DMat<ARingQQ>` into it on construction, and copies the result back
on demand. Destructors clear the FLINT storage.

## When used

Whenever code wants FLINT-fast QQ matrix arithmetic but the
underlying `DMat<ARingQQ>` uses GMP storage. Today this applies to
several call paths in [`file-mat-linalg.md`](file-mat-linalg.md).

After the default switches to `ARingQQFlint`, those call paths can
go through [`file-dmat-qq-flint.md`](file-dmat-qq-flint.md) directly
and this layer becomes unnecessary.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-aring-qq.md`](file-aring-qq.md) — typedef chooser.
- [`file-aring-qq-flint.md`](file-aring-qq-flint.md), [`file-aring-qq-gmp.md`](file-aring-qq-gmp.md)
  — backend element types.
- [`file-dmat-qq-flint.md`](file-dmat-qq-flint.md) — successor (used
  when `ARingQQ = ARingQQFlint`).
- FLINT submodule under [`../../submodules/README.md`](../../submodules/README.md).
