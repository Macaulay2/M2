# `M2/Macaulay2/e/bibasis/` — involutive (Janet) bases for Boolean rings

**See [`architecture.md`](architecture.md)** for the standalone architectural reference (Boolean-ring specialisation, 64-bit packed monomial trick, involutive-vs-Gröbner distinction, Janet tree data structure, templated dispatch on monomial order, `FastAllocator` slab strategy).

A specialised Gröbner-basis engine that computes **involutive Janet bases**
over the Boolean polynomial ring `F_2[x_1,…,x_n]/(x_i^2 - x_i)`. Used by the
[`BIBasis`](../../packages/BIBasis.m2) package.

Janet bases are a richer alternative to Gröbner bases that come with a unique
involutive division structure, making certain calculations (e.g. reductions in
specific cones) faster than with a generic GB.

The original short notes are in plain text in [`README`](README).

## Per-file deep dives

| File doc | Class / topic |
|---|---|
| [`file-bibasis.md`](file-bibasis.md) | `BIBasis` engine-facing driver |
| [`file-launcher.md`](file-launcher.md) | `launcher.{cpp,hpp}` — order dispatch |
| [`file-involutive.md`](file-involutive.md) | `Involutive<MonomType>` templated algorithm |
| [`file-monom.md`](file-monom.md) | `Monom` base class |
| [`file-monom-orders.md`](file-monom-orders.md) | `MonomLex`, `MonomDL`, `MonomDRL` order specialisations |
| [`file-polynom.md`](file-polynom.md) | `Polynom<MonomType>` polynomial value type |
| [`file-janettree.md`](file-janettree.md) | `JanetTree<MonomType>` involutive-division structure |
| [`file-allocator.md`](file-allocator.md) | `FastAllocator` slab pool |
| [`file-bibasis-internals.md`](file-bibasis-internals.md) | `Triple`, `TSet`, `QSet`, `PComparator`, `SettingsManager` |

**Coverage:** every source file in this directory has a dedicated deep-dive doc (some grouped per cohesive topic).

## Files

### Driver

| File | Role |
|---|---|
| `bibasis.{cpp,hpp}` | Main algorithm entry point |
| `launcher.{cpp,hpp}` | Wrapper that exposes the algorithm to the engine |
| `settings-manager.{cpp,hpp}` | Algorithm configuration |
| `allocator.{cpp,hpp}` | Memory pool used by the inner loop |

### Monomial representations (three alternative orderings)

| File | Role |
|---|---|
| `monom.{cpp,hpp}` | Generic monomial interface |
| `monomLex.{cpp,hpp}` | Lex-ordered monomial |
| `monomDL.{cpp,hpp}` | Degree-lex monomial |
| `monomDRL.{cpp,hpp}` | Degree-reverse-lex monomial |

### Data structures

| File | Role |
|---|---|
| `polynom.hpp` | Polynomial type |
| `janettree.hpp`, `tset.hpp`, `qset.hpp` | Janet tree and supporting sets |
| `triple.hpp` | Triple `(α, m, p)` representing an involutive division step |
| `pcomparator.hpp` | Polynomial comparator |
| `involutive.hpp` | Involutive-basis algorithm bits factored out for reuse |

## What triggers this engine

This involutive-basis (Janet basis) engine is selected when:

| M2 user code | What happens | Why this engine |
|---|---|---|
| User loads [`BIBasis`](../../packages/BIBasis.m2), then `rawBIBasis(I, …)` | Direct engine entry through `launcher` (see [`file-launcher.md`](file-launcher.md)) | The only entry path — there is no generic `gb` auto-routing to this engine |
| User calls `involutiveBasis I` from the `BIBasis` package | M2-level wrapper → `rawBIBasis` → `launcher` | The package's main exported function |
| The ring is `F_2[x_1, …, x_n] / (x_i^2 - x_i)` and the user wants an involutive basis | Strategy selected via `BIBasis` package options | Specialised for Boolean rings only |

Unlike `f4/` and `gb-f4/`, this engine is **opt-in** via the `BIBasis` package — `gb I` over a Boolean ring uses a general-purpose engine unless the user explicitly asks for involutive bases.

## Where in the engine pipeline this fits

```
M2:  needsPackage "BIBasis"; involutiveBasis I
   ↓
m2 (BIBasis pkg)  →  rawBIBasis(...)
   ↓ d/interface2.dd  →  Ccode(...)
   ↓
e/bibasis/launcher.cpp (file-launcher.md)
   ↓ picks a monomial-order specialisation:
Involutive<MonomLex>      (file-involutive.md, file-monom-orders.md)
Involutive<MonomDL>
Involutive<MonomDRL>
   ↓ each uses
Monom<MonomType>          (file-monom.md)              — 64-bit packed monomial
Polynom<MonomType>        (file-polynom.md)            — polynomial value type
JanetTree<MonomType>      (file-janettree.md)          — involutive-division tree
TSet, QSet, Triple, PComparator
                          (file-bibasis-internals.md)  — per-step bookkeeping
SettingsManager           (file-bibasis-internals.md)  — algorithm configuration
FastAllocator             (file-allocator.md)          — slab pool for tiny objects
   ↓ produces
A set of polynomials forming an involutive basis, returned to the package
   for further M2-level analysis
```

The **64-bit bitmask monomial trick** is the heart of the engine's speed for Boolean rings: every variable is a bit, multiplication is bitwise OR, division is bitwise AND check. This lets the involutive division steps run in 1-2 CPU cycles each.

## Why specialise vs use the generic GB

Boolean polynomial rings satisfy three properties that make general-purpose GB engines slow:
- Every variable squares to itself: `x_i^2 = x_i`. The `f4/` and `gb-f4/` engines waste work reducing these implicit relations.
- All coefficients are in `F_2`. The general path's coefficient arithmetic via `ARingZZpFlint(2)` has overhead the bitmask path doesn't.
- Involutive bases give the **unique normal form** under the involutive division relation — useful for specific applications where Gröbner normality is insufficient.

For these reasons, the `BIBasis` package is the standard tool for Boolean ring analysis in M2, and this engine is its only backend.

## Related

- [`BIBasis`](../../packages/BIBasis.m2) — the user-facing package.
- General-purpose GB engines: [`../f4/`](../f4/README.md),
  [`../gb-f4/`](../gb-f4/README.md), and mathicgb (submodule).
- [`../groebner-bases.md`](../groebner-bases.md) — the parent area doc; `BIBasis` is listed in the "M2 strategy → engine algorithm" table.

[← back to engine overview](../README.md)
