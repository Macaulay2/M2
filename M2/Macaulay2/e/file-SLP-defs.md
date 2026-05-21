# `SLP-defs.hpp` — `SLProgram` / `M2SLProgram` type declarations

`SLP-defs.hpp` declares the **straight-line program** type
hierarchy used by Numerical Algebraic Geometry: `SLProgram` (the
underlying program), `M2SLProgram` (the M2-facing wrapper), and the
opaque type tags they expose. The implementations live in
[`file-SLP.md`](file-SLP.md) and `SLP-imp.hpp`
([`file-SLP-imp.md`](file-SLP-imp.md)).

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Declarations

```cpp
// SLP
class SLProgram;

class M2SLProgram : public MutableEngineObject {
    std::unique_ptr<SLProgram> mSLProgram;
public:
    M2SLProgram(SLProgram *pa) : mSLProgram(pa) {}
    SLProgram &value() { return *mSLProgram; }
};

class SLProgram {
    // ... constants, gates, inputs, outputs ...
};
```

Two types:

- **`SLProgram`** — the actual program. A DAG of arithmetic gates
  (`+`, `*`, `−`, `^`, `−1`) plus designated **input** and **output**
  positions. Templates over the coefficient ring.
- **`M2SLProgram`** — the M2-facing wrapper. Inherits from
  [`MutableEngineObject`](file-hash.md) for GC integration plus
  mutable-hash discipline. Owns the `SLProgram` via
  `std::unique_ptr`.

The `value()` accessor on `M2SLProgram` returns a reference to the
wrapped `SLProgram` so engine code can manipulate the underlying
program directly when needed.

## Why two classes

The split matches the engine's general "owned-pointer + abstract
wrapper" pattern, but here it serves a specific use case: NAG passes
`M2SLProgram*` across the engine ↔ M2 boundary, while the
templated `SLProgram` does the actual numerical work via
`SLEvaluatorConcrete<RT>` (in [`file-SLP-imp.md`](file-SLP-imp.md)).

The wrapper keeps the inner template invisible to the interpreter;
templated specialisations don't leak through M2-visible names.

## Used by

- [`file-NAG.md`](file-NAG.md) — primary user.
- [`file-SLP.md`](file-SLP.md), `file-SLP-imp.md` — implementations.
- `interface/NAG.h`
  ([`interface/file-NAG-interface.md`](interface/file-NAG-interface.md))
  — exports `M2SLProgram*` as an opaque pointer.

## Author note

Anton Leykin's code in this file is in the public domain (see the
SPDX-style copyright comment at the top).

## Related

- [`computations.md`](computations.md) — area overview.
- [`file-SLP.md`](file-SLP.md) — umbrella header.
- [`file-NAG.md`](file-NAG.md) — primary consumer.
- [`file-hash.md`](file-hash.md) — `MutableEngineObject` base.
