# Engine architecture

This document is the **architectural reference** for the
`M2/Macaulay2/e/` engine — the C++ mathematical kernel of
Macaulay2. It pulls together the high-level story that the 10
per-area pages and ~300 per-file deep dives each cover one piece of.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e)

## The four-layer architecture

The engine is best thought of as **four concentric layers**:

```
                ┌──────────────────────────────────────────────┐
                │   interface/      public C entry points       │ ← d/engine.dd calls in here
                ├──────────────────────────────────────────────┤
                │   Computation framework                       │
                │   (comp-gb, comp-res, hilb, LLL, NAG, …)      │
                ├──────────────────────────────────────────────┤
                │   Mathematical objects                        │
                │   (rings, monoids, matrices, free modules)    │
                ├──────────────────────────────────────────────┤
                │   Primitives                                  │
                │   (allocators, monomial encodings, GMP/FLINT) │
                └──────────────────────────────────────────────┘
```

### Layer 1: Public C interface — [`interface/`](interface/README.md)

The **only surface the interpreter sees**. Every entry point is
a plain C function taking opaque pointers and primitive types,
so it can be called from `.d`/`.dd` code after translation by
`scc1`. No file in `interface/` is allowed to `#include "engine.h"`.

Examples:

- `IM2_Matrix_multiply(A, B)` — multiply two matrices.
- `IM2_RingElement_add(a, b)` — add two ring elements.
- `IM2_GB_make(M, ...)` — start a Gröbner basis computation.

Each of these dispatches into the appropriate internal class.
See [`interface/`](interface/README.md) for the full list.

### Layer 2: Computation framework — [`computations.md`](computations.md)

Long-running algorithms (Gröbner bases, resolutions, Hilbert
series, NAG, ...) are not blocking function calls. They are
**resumable objects** the interpreter creates and steps:

```cpp
Computation* c = new GBComputation(matrix, ...);
c->set_stop_conditions(degree_limit, ...);
c->start_computation();    // runs until stop condition or completion
result = c->get_partial_result();
```

The interpreter holds the `Computation*`, lets the user inspect
partial results, and can resume by changing the stop conditions.

See `comp.{cpp,hpp}` ([`file-comp.md`](file-comp.md)) for the
abstract base and `comp-gb.cpp` / `comp-res.cpp` for the GB and
resolution specialisations.

### Layer 3: Mathematical objects

The bulk of the engine. Rings, monoids, matrices, modules, and
the operations on them. Organised into 8 per-area docs:

- [`coefficient-rings.md`](coefficient-rings.md) — `aring-*`, `ZZ`, `ZZp`, `GF`, `RR`, `CC`, …
- [`polynomial-rings.md`](polynomial-rings.md) — `polyring`, `qring`, `frac`, `weylalg`, `skewpoly`, …
- [`monoids-and-monomials.md`](monoids-and-monomials.md) — `monoid`, `monorder`, `imonorder`, monomial tables
- [`matrices.md`](matrices.md) — `Matrix`, `MutableMatrix`, `DMat<R>`, `SMat<R>`
- [`free-modules.md`](free-modules.md) — `FreeModule`, Schreyer orders
- [`groebner-bases.md`](groebner-bases.md) — `comp-gb*`, `gb-*`, `reducedgb-*`, `gbring`
- [`resolutions.md`](resolutions.md) — `comp-res`, `res-a0/a1/a2`, `Eschreyer`, `betti`
- [`ring-elements-and-maps.md`](ring-elements-and-maps.md) — `relem`, `ringmap`, `RingElement`

### Layer 4: Primitives — [`utilities.md`](utilities.md)

The boundary against external libraries (FLINT, GMP, MPFR,
bdwgc) and the place where memory allocation, monomial encoding,
and overflow-checked arithmetic live. Horizontal across all
other layers.

## The dual-representation story

The engine has **two parallel ring abstractions** that coexist for
historical reasons:

```
Legacy:    Ring (abstract base)  →  ring_elem (tagged-pointer values)
              ↑                          ↑
              │ virtual dispatch         │ used at the C ABI boundary
              │                          │
Modern:    ARing<...> (templated)  →  ElementType (typed values)
              ↑                          ↑
              │ compile-time dispatch    │ used in templated inner loops
              │                          │
Bridge:    ConcreteRing<ARingType> ────  aring-glue.hpp
              ↑
              │ wraps an ARing as a legacy Ring
```

- **Legacy `Ring`** is what the C ABI exposes (because virtual
  dispatch on `Ring*` is fine for the boundary).
- **Modern `aring`** is what templated inner loops use (because
  compile-time dispatch is free).
- **The bridge** — `ConcreteRing<ARingType>` — wraps an `aring`
  ring to *also* look like a legacy `Ring`.

This means new code can be written either way. New rings target
`aring`; legacy paths stay on `Ring`. See
[`file-aring-glue.md`](file-aring-glue.md) for the bridge and
[`ring-elements-and-maps.md`](ring-elements-and-maps.md) for the
full discussion.

## Memory model

Every engine object follows one of three lifetimes:

### 1. GC-managed (Boehm GC) — the default

```cpp
class Foo : public our_new_delete { ... };
Foo *f = new Foo;       // GC-tracked, no delete needed
```

`our_new_delete` (in [`file-newdelete.md`](file-newdelete.md))
routes `operator new` through Boehm GC. Most engine objects
inherit from it (or from `MutableEngineObject` in
[`file-hash.md`](file-hash.md) for mutable + GC-tracked).

### 2. Pool-allocated — for hot inner loops

```cpp
MemoryBlock<Node> pool;
Node *n = pool.allocate();
// pool releases all at end; no individual frees
```

Used where billions of small allocations would overwhelm GC.
See [`file-MemoryBlock.md`](file-MemoryBlock.md) for the
top-level type; [`f4/file-memblock.md`](f4/file-memblock.md) and
[`schreyer-resolution/file-res-memblock.md`](schreyer-resolution/file-res-memblock.md)
for specialised variants used by the F4 and Schreyer-resolution
engines respectively.

The BIBasis engine has its own slab allocator
([`bibasis/file-allocator.md`](bibasis/file-allocator.md))
specialised for its tiny `Triple` / `Polynom` / `Monom` objects.

### 3. External-library-managed

`mpz_t`, `mpfr_t`, `fmpz_t`, NTL `ZZ` — wrapped in GC-managed
handles with **finalisers** from
[`file-finalize.md`](file-finalize.md) that call the library's
free function when the GC reclaims the wrapper.

## The Computation framework

```
            Computation                            (abstract base)
                │
   ┌────────────┼─────────────┬──────────────┬──────────────┐
   ▼            ▼             ▼              ▼              ▼
GBComputation  ResolutionComp.  HilbertComp.   LLLComp.    NAGComp.
   │            │             │              │              │
   │ subclasses each per algorithm strategy   │              │
   ▼            ▼             ▼              ▼              ▼
gb-default    res-a0/a1/a2  hilb           LLL            NAG
gb-toric      schreyer-res.
gb-walk
gb-f4
...
```

Every subclass:

- Stores its **state** (current partial result).
- Provides **`start_computation`** (runs until done or stops).
- Provides **`get_result`** (return the partial state).
- Periodically polls **stop conditions** (degree, time, basis-size).

See [`file-comp.md`](file-comp.md) for the base class.

## Threading

Two threading frameworks coexist:

- **Supervisor + pthreads** ([`../system/file-supervisor.md`](../system/file-supervisor.md))
  — at the M2-language level via `Task` / `schedule`.
- **Intel TBB** ([`file-m2tbb.md`](file-m2tbb.md)) — within the
  engine's templated inner loops, primarily in the
  Schreyer-resolution dependency-graph traversal
  ([`schreyer-resolution/file-res-dep-graph.md`](schreyer-resolution/file-res-dep-graph.md)).

The two layers don't communicate directly. M2 tasks run separate
engine workloads in parallel; TBB parallelises *within* one engine
workload.

## The engine boundary

```
M2 user code (m2/*.m2)
   │
   ▼ M2-level dispatch
d/<area>.dd (interpreter binding)
   │
   ▼ Ccode(...) escape
d/engine.dd (interpreter ↔ engine bridge)
   │
   ▼ C function call
e/interface/<area>.{h,cpp} (public C API)
   │
   ▼ converts opaque pointer
e/<class>.{cpp,hpp} (internal C++ implementation)
```

Three rules govern the boundary:

1. **`engine.h` is the legacy aggregating header.** New entry
   points go in narrower `interface/<area>.h` files.
2. **`interface/` uses only plain C types.** No `<engine.h>`
   includes; no `class` exposure across the boundary.
3. **Values cross as `ring_elem`, `M2_arrayint`, opaque
   pointers.** The interpreter only knows shapes; the engine knows
   contents.

See [`interface/README.md`](interface/README.md) and
[`file-engine-h.md`](file-engine-h.md) for the boundary in detail.

## Specialised engines as subdirectories

When a sub-system grew too large to live inline, it became a
subdirectory:

| Subdir | What it is | Decoupled from `e/` because |
|---|---|---|
| [`interface/`](interface/README.md) | Public C API | Every other engine file is forbidden from including `interface/*.h` (avoids include-cycle hell) |
| [`f4/`](f4/README.md) | Original F4 GB engine | Templated heavily on monomial type |
| [`gb-f4/`](gb-f4/README.md) | Refactored F4 (newer) | Newer cleaner separation of concerns |
| [`schreyer-resolution/`](schreyer-resolution/README.md) | F4-style free resolution | Parallel TBB graph layout |
| [`NCAlgebras/`](NCAlgebras/README.md) | Non-commutative free algebras | Whole different algebraic structure (no monoid) |
| [`NCResolutions/`](NCResolutions/README.md) | NC free resolutions | Uses NCAlgebras |
| [`bibasis/`](bibasis/README.md) | Involutive (Janet) bases for Boolean rings | Specialised algorithm for `F_2[x_i]/(x_i^2-x_i)` |
| [`unit-tests/`](unit-tests/README.md) | C++ gtest suite | Pure test code |
| [`doxygen-settings/`](doxygen-settings/README.md) | Doxygen configuration | Pure documentation |

Each subdir has its own README plus per-file deep dives.

## Cross-cutting concerns

### Overflow safety

Polynomial degree calculations multiply exponents and sum them.
Silent overflow can corrupt a Gröbner basis without raising an
error. The engine uses `safe::add` / `safe::mul` / `safe::pow`
from [`file-overflow.md`](file-overflow.md) pervasively in
monomial / degree code.

### Comparison codes

Every `compare()` method returns one of:

```
GT = 1,  EQ = 0,  LT = -1,  INCOMP = 2
```

defined in [`file-style.md`](file-style.md). Centralised so all
code agrees on the convention.

### Error reporting

Two channels:

- Flag-based (`error.h`, see [`file-error.md`](file-error.md))
  for the C ABI boundary.
- C++ exceptions
  ([`file-exceptions.md`](file-exceptions.md)) within engine code.

The boundary always catches and converts to flags so the
interpreter never sees a C++ exception.

### GC integration

Every engine class inherits from `our_new_delete` (or
`MutableEngineObject`). Boehm GC scans the resulting objects'
fields automatically. The hard cases:

- External library types — wrapped with finalisers.
- STL containers — use `gc_allocator` (see `gc_std.hpp` in
  [`../system/file-mutex.md`](../system/file-mutex.md)).
- Pool-allocated objects — outside the GC's tracking entirely;
  the pool's lifetime bounds the objects' lifetimes.

## "Calling into the engine" reference flow

```
M2 user:        gb I
   ↓
Core m2:        m2/gb.m2                  builds a request
   ↓
Interpreter:    d/interface.dd            extracts engine pointers
   ↓
Bridge:         d/engine.dd               wraps Ccode(...) call
   ↓
C API:          e/interface/groebner.cpp  IM2_GB_make(...)
   ↓
Internal:       e/comp-gb.{cpp,hpp}       new GBComputation(...)
   ↓
Algorithm:      e/gb-default.cpp          OR e/gb-f4/GBF4Computation.cpp
   ↓
Inner loops:    e/f4/f4.cpp               OR e/gb-f4/MacaulayMatrix.cpp
   ↓
Primitives:     e/utilities.md            allocators, overflow checks
```

## Adding new things — patterns

See the **Cross-cutting flows** section of the
[top-level README](../../../README.md#cross-cutting-flows) for the
canonical recipes:

- Adding a new coefficient ring (modelled on
  [`file-aring-zz-flint.md`](file-aring-zz-flint.md)).
- Adding a new computation (subclass of
  [`Computation`](file-comp.md)).
- Adding a new built-in operator.
- Adding a new user-distributed package.
- Adding a new external library dependency.

## Related

- [`README.md`](README.md) — engine navigation hub.
- [`../../../README.md#engine-deep-dive-m2macaulay2e`](../../../README.md#engine-deep-dive-m2macaulay2e)
  — the same architecture from the top-level perspective.
- Per-area docs (8 files, listed in [Layer 3](#layer-3-mathematical-objects)
  above plus [Layer 4](#layer-4-primitives--utilitiesmd)).
- Per-file deep dives — ~300 `file-*.md` files alongside each
  source file.
