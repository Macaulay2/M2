# `M2/include/M2/` — public M2 C/C++ headers

Headers installed alongside the M2 binary that downstream consumers (the
interpreter, the engine, occasionally external code) include. Several of
these are generated from `.in` / `.cmake` templates at configure time.

| File | Role | Deep dive |
|---|---|---|
| `atomic-field.h` | Atomic-field helper macros used by the interpreter's atomic types (`d/atomic.d`) | [`file-atomic-field-h.md`](file-atomic-field-h.md) |
| `gc-include.h` | A single header that brings in the right bdwgc headers for the current build flavour | [`file-gc-include-h.md`](file-gc-include-h.md) |
| `math-include.h` | Aggregated math headers (GMP / MPFR / MPFI) — guarded against double-inclusion across translation units | [`file-math-include-h.md`](file-math-include-h.md) |
| `synchronization.h.in` | Template for the threading/synchronisation primitives used by the supervisor and interpreter. Substituted at configure time | — |
| `config.h.cmake` | CMake-side template for `config.h` | — |

**Coverage:** every non-templated `.h` file in this directory has a dedicated deep-dive doc.

## Why are these here, not in `Macaulay2/`?

Because some of them are **generated** and some are **bundled** for external
consumers. They live one level above the source dirs so the build tree's
include path is `-I .../M2/include` rather than reaching inside `Macaulay2/`.

## Related

- [`../README.md`](../README.md) — includes overview.
- [`../../Macaulay2/d/`](../../Macaulay2/d/README.md) — interpreter, primary
  consumer of `atomic-field.h` and `synchronization.h`.
- [`../../Macaulay2/system/`](../../Macaulay2/system/README.md) — supervisor,
  also uses the synchronisation primitives.
