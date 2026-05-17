# `engine.dd` — the interpreter → engine bridge

`engine.dd` is the **interpreter → engine bridge** — the `.dd` file
that imports the engine's public C interface
([`../e/file-engine-h.md`](../e/file-engine-h.md)) and exposes every
`raw*` function to the M2 layer.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What it does

`engine.dd` declares wrappers for every engine entry point:

```d
import rawARingZZp(p:ulong):RawRingOrNull;
import rawMatrix(...):RawMatrixOrNull;
import rawGB(...):RawComputationOrNull;
-- hundreds more ...
```

Each `import` brings a C-side function (declared in `interface/*.h`
in [`../e/`](../e/README.md)) into the `.d` namespace as something
the M2 layer can call. The bulk of the file is just imports.

## How M2 calls into the engine

The chain:

1. M2 code: `gb I`.
2. Method dispatch finds the `gb Ideal` body.
3. The body calls `rawGB(...)` — an imported function.
4. `engine.dd`'s import resolves to the corresponding
   [`interface/file-groebner-interface.md`](../e/interface/file-groebner-interface.md)
   C function.
5. The engine performs the computation, returns an opaque
   `RawComputation*` pointer.
6. M2 wraps the pointer in a `Computation` HashTable.

`engine.dd` is the boundary where the chain crosses from
`.dd`-translated C++ to the engine's hand-written C++.

## Why a `.dd`, not `.d`

The engine boundary involves C++-only types (templated `aring`
classes, STL containers). `scc1`'s `.dd` form (which compiles to
C++) handles these correctly; the `.d` form (compiling to C) would
break.

## Result-type aliases

Many imports are typed `RawXxxOrNull` — a sum type that's either
the success result or `null`. The pattern lets the engine signal
errors without exceptions across the boundary:

```d
import rawARingZZp(p:ulong):RawRingOrNull;

when rawARingZZp(p)
is r:RawRing do ...
is null do error("invalid prime")
```

Used pervasively throughout the M2 layer.

## Used by

- [`file-evaluate.md`](file-evaluate.md), `actors*.d` — call engine
  functions for arithmetic on engine-backed values.
- [`../m2/file-engine.md`](../m2/file-engine.md) — M2-side imports
  of the same surface.
- Every M2-language operation that touches the engine.

## Related

- [`README.md`](README.md) — d/ overview.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) — engine's
  aggregating header.
- [`../e/interface/README.md`](../e/interface/README.md) — narrower
  per-area headers.
- [`../m2/file-engine.md`](../m2/file-engine.md) — M2-side
  `Raw*` type declarations.
