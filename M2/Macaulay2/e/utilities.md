# Engine utilities

Cross-cutting helpers used throughout the engine: I/O buffers, error
reporting, debug printing, overflow-checked arithmetic, and a bump allocator.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## I/O

| File pair | Purpose |
|---|---|
| `buffer.{cpp,hpp}` | Append-only byte buffer — used for serialisation, pretty-printing, error messages |
| `text-io.{cpp,hpp}` | Text I/O helpers layered on `buffer` |

The `buffer` is intentionally simpler than `std::ostringstream` — it is
GC-aware (uses `our_new_delete`) and avoids C++ stream-state overhead in hot
loops.

## Error / debug

| File pair | Purpose |
|---|---|
| `error.{cpp,hpp}` | Engine-side error reporting (sets a thread-local error message the interpreter then surfaces) |
| `debug.{cpp,hpp}` | Debug-time printing of engine values |

## Overflow-checked arithmetic

| File pair | Purpose |
|---|---|
| `overflow.{cpp,hpp}` | Overflow-checked add/multiply for the small integer types used in monomial exponents and degrees. **Deep dive:** [`file-overflow.md`](file-overflow.md) |

The engine uses `overflow.hpp` extensively. A silent monomial-exponent
overflow can corrupt a Gröbner basis without any visible error; centralised
checked-arithmetic helpers prevent that. See `README.md` (Historical notes
section) for the list of files that depend on `overflow.hpp`.

## Memory

| File pair / header | Purpose |
|---|---|
| `MemoryBlock.hpp` | Bump-pointer allocator used by F4 inner loops and other tight code paths |
| `newdelete.hpp` (in subdirectories) | GC-friendly `operator new` / `delete` overloads (`our_new_delete`, `our_new_gc`) |

For a deeper discussion of memory and GC see
[`README.md`](README.md#historical-notes) (Historical notes).

## Random numbers

| File pair | Purpose |
|---|---|
| `random.{cpp,hpp}` (lives in [`interface/`](interface/README.md)) | Engine-side RNG state, seeded from the interpreter |

## Files (in [`interface/`](interface/README.md)) that look like utilities

| File pair | Purpose |
|---|---|
| `m2-mem.{h,cpp}` | Allocation hooks exposed back to the interpreter |
| `m2-types.{h,cpp}` | Opaque type tags the interpreter passes to the engine |
| `gmp-util.h` | GMP helpers |

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — biggest user of
  `overflow`.
- [`groebner-bases.md`](groebner-bases.md) — biggest user of `MemoryBlock`.
- [`README.md`](README.md) — historical notes include the full list of files
  depending on `overflow.hpp`.
