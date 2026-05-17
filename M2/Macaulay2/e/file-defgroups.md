# `defgroups.h` — Doxygen group definitions

`defgroups.h` is **pure Doxygen documentation** — it contains no
code. It defines the `@ingroup` categories that other engine
headers use to organise the generated C++ API docs.

Part of the [engine](README.md) — documentation.

[← engine overview](README.md)

## Header

```c
/**
    \mainpage Macaulay2 C++ engine documentation

    The Macaulay2 engine implements rings, elements in these rings,
    matrices over these rings, mutable matrices, ring maps, and
    computations including

    - Groebner bases
    - Hilbert functions
    - free resolutions

    The engine is written in C++ over a long period of time, some of it
    before the c++ standard library was available or robust.  The engine
    is being updated to c++17, and eventually to c++20.
*/
```

The `\mainpage` directive sets the top-level title of the
Doxygen-generated HTML in
[`../docs/`](../../docs/README.md). The doxygen run pulls
`defgroups.h` first to establish:

- The mainpage description shown above.
- All `@defgroup` declarations (rings, gb, computations, matrices,
  resolutions, ...).
- Group ordering and relationships.

## What groups are defined

Typical Doxygen groups:

| Group | Used by classes in |
|---|---|
| `@defgroup rings` | `ring.hpp`, all `aring-*.hpp` |
| `@defgroup gb` | `comp-gb*.hpp`, `gb-*.hpp` |
| `@defgroup reducedgb` | `reducedgb*.hpp` |
| `@defgroup computations` | `comp.hpp` |
| `@defgroup matrices` | `mat.hpp`, `dmat.hpp`, `smat.hpp` |
| `@defgroup resolutions` | `comp-res.hpp`, `res-a*.hpp` |

Each class header uses `\ingroup <group>` to land in the right
section.

## Why a dedicated header

Doxygen processes files in a deterministic order. By putting all
group definitions in one file (`defgroups.h`), the developer
guarantees groups exist before any class tries to join them.
Without this, group ordering could shift depending on which class
file Doxygen happened to read first.

## Used by

- The Doxygen build in [`../../docs/`](../../docs/README.md).
- Every class header that uses `\ingroup`.

## Related

- [`README.md`](README.md) — engine overview.
- [`../../docs/README.md`](../../docs/README.md) — Sphinx + Doxygen
  C++ API docs.
- [`doxygen-settings/README.md`](doxygen-settings/README.md) —
  Doxygen styling.
