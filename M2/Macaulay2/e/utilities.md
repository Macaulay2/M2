# Engine utilities

Cross-cutting helpers used throughout the engine: I/O buffers, error
reporting, debug printing, overflow-checked arithmetic, and a bump allocator.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## I/O

| File pair | Purpose |
|---|---|
| `buffer.{cpp,hpp}` | Append-only byte buffer — used for serialisation, pretty-printing, error messages. **Deep dive:** [`file-buffer.md`](file-buffer.md) |
| `text-io.{cpp,hpp}` | Text I/O helpers layered on `buffer`. **Deep dive:** [`file-text-io.md`](file-text-io.md) |

The `buffer` is intentionally simpler than `std::ostringstream` — it is
GC-aware (uses `our_new_delete`) and avoids C++ stream-state overhead in hot
loops.

## Error / debug

| File pair | Purpose |
|---|---|
| `error.{c,h}` | Engine-side error reporting (sets a thread-local error message the interpreter then surfaces). **Deep dive:** [`file-error.md`](file-error.md) |
| `debug.{cpp,hpp}` | Debug-time printing of engine values. **Deep dive:** [`file-debug.md`](file-debug.md) |

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
| `MemoryBlock.hpp` | Bump-pointer allocator used by F4 inner loops and other tight code paths. **Deep dive:** [`file-MemoryBlock.md`](file-MemoryBlock.md) |
| `newdelete.hpp` (in subdirectories) | GC-friendly `operator new` / `delete` overloads (`our_new_delete`, `our_new_gc`) |
| `hash.hpp` | `EngineObject` / `MutableEngineObject` (GC bases all engine objects inherit). **Deep dive:** [`file-hash.md`](file-hash.md) |
| `mem.hpp` | `stash` — size-class slab allocator. **Deep dive:** [`file-mem.md`](file-mem.md) |
| `myalloc.hpp` | `StatsAllocator` — debug allocator. **Deep dive:** [`file-myalloc.md`](file-myalloc.md) |
| `finalize.hpp` | Engine-object finalisation hooks (releases C-managed external library state). **Deep dive:** [`file-finalize.md`](file-finalize.md) |

### Three memory lifetimes

Every engine object follows one of three lifetimes:

1. **GC-managed (Boehm GC)** — the default. Inherit from `our_new_delete`
   (or `MutableEngineObject` for mutable + GC-tracked):

   ```cpp
   class Foo : public our_new_delete { ... };
   Foo *f = new Foo;       // GC-tracked
   ```

   No `delete` needed — the GC reclaims unreachable objects.

2. **Pool-allocated** — for hot inner loops with predictable allocation
   patterns:

   ```cpp
   MemoryBlock<MyNode> pool;
   MyNode *node = pool.allocate();
   // pool releases all at end; no individual frees
   ```

   Used in F4 GB ([`f4/file-memblock.md`](f4/file-memblock.md)) and
   Schreyer-resolution
   ([`schreyer-resolution/file-res-memblock.md`](schreyer-resolution/file-res-memblock.md))
   where billions of small allocations would otherwise overwhelm GC.

3. **External-library-managed** — GMP `mpz_t`, MPFR `mpfr_t`, FLINT
   `fmpz_t`, NTL `ZZ`. The engine wraps these in GC-managed wrappers and
   uses **finalisers** ([`file-finalize.md`](file-finalize.md)) to call the
   library's free function when the wrapper is collected.

## Random numbers

| File pair | Purpose |
|---|---|
| `random.{cpp,hpp}` (lives in [`interface/`](interface/README.md)) | Engine-side RNG state, seeded from the interpreter |

## Comparison codes

[`file-style.md`](file-style.md) defines the engine's universal
comparison-result convention:

```
GT     =  1     // a > b
EQ     =  0     // a == b
LT     = -1     // a < b
INCOMP =  2     // incomparable (NaN, vector ordering, ...)
```

Every `compare()` method returns one of these. Centralising in one place
ensures consistency.

## Exception handling

Two complementary mechanisms:

- **Flag-based** ([`file-error.md`](file-error.md)) — a thread-local
  "error pending" flag. Engine functions set it on failure; callers check
  after each call.
- **C++ exceptions** ([`file-exceptions.md`](file-exceptions.md)) —
  typed exception hierarchy for cleaner error handling within C++ code.

The boundary (C ABI through `engine.h`) uses flags exclusively. Internal
C++ code can use either, with the boundary catching exceptions and
converting to flags.

## Threading and TBB

[`file-m2tbb.md`](file-m2tbb.md) — abstracts Intel TBB's `parallel_for` /
`parallel_reduce` so the engine can opt in/out of threading at compile
time. When TBB isn't available, the wrapper becomes sequential.

Used by the Schreyer-resolution engine
([`schreyer-resolution/file-res-dep-graph.md`](schreyer-resolution/file-res-dep-graph.md))
to parallelise across (level, degree) cells.

## Timing

[`file-timing.md`](file-timing.md) — `timing.hpp` provides engine-side
timestamps. Used by benchmarking code, the M2 `time` operator, and
performance tracking.

## CRT and reconstruction

[`file-cra.md`](file-cra.md) — `ChineseRemainder` (CRT plus rational
reconstruction). Used by modular-arithmetic paths that compute over
finite fields and lift back to ZZ / QQ.

## Stream reading

[`file-reader.md`](file-reader.md) — `Reader<RingType>` template for
parsing ring elements from a stream. Used by matrix-IO paths and test
fixtures.

## Files (in [`interface/`](interface/README.md)) that look like utilities

| File pair | Purpose |
|---|---|
| `m2-mem.{h,cpp}` | Allocation hooks exposed back to the interpreter |
| `m2-types.{h,cpp}` | Opaque type tags the interpreter passes to the engine |
| `gmp-util.h` | GMP helpers |

## Why "utilities" instead of folding into other areas

The utility layer is **horizontal** — it has clients in every other area.
Folding `our_new_delete` into "rings" would be wrong because every matrix,
every monoid, every computation also uses it. The utility area is the
**shared substrate**.

## How to use this area

When working on the engine:

- **Adding a new class** — inherit from `our_new_delete` (or
  `MutableEngineObject` for mutable + GC-tracked) per
  [`file-hash.md`](file-hash.md).
- **Doing inner-loop arithmetic on degrees / exponents** — use `safe::*`
  from [`file-overflow.md`](file-overflow.md).
- **Reporting an error** — pick flag or exception per
  [`file-error.md`](file-error.md) / [`file-exceptions.md`](file-exceptions.md).
- **Streaming text output** — write through a `buffer` from
  [`file-buffer.md`](file-buffer.md).
- **Comparing two objects** — return a code from
  [`file-style.md`](file-style.md).
- **Hot loop allocating millions of small things** — use
  [`file-MemoryBlock.md`](file-MemoryBlock.md).
- **Wrapping an external-library type** — use a finaliser from
  [`file-finalize.md`](file-finalize.md).

## Related

- [`monoids-and-monomials.md`](monoids-and-monomials.md) — biggest user of
  `overflow`.
- [`groebner-bases.md`](groebner-bases.md) — biggest user of `MemoryBlock`.
- [`computations.md`](computations.md) — the Computation framework relies
  on the utility layer.
- [`coefficient-rings.md`](coefficient-rings.md) — heavy user (every ring
  class).
- [`README.md`](README.md) — historical notes include the full list of files
  depending on `overflow.hpp`.
