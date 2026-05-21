# `mathicgb-interface.{cpp,hpp}` — bridge to the mathicgb library

This file is the **adapter** between the engine's `GBComputation` interface and
the [mathicgb](https://github.com/Macaulay2/mathicgb) library (vendored as a
[submodule](../../submodules/README.md)). mathicgb is a separate, highly
tuned, signature-based Gröbner basis engine; this file lets M2 dispatch to it
when the user requests `Strategy => MathicGB`.

Part of the [Gröbner bases](groebner-bases.md) area.

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## What mathicgb is

mathicgb implements **signature-based Gröbner basis algorithms** (the
F5/SBA family) tuned for monomial-heavy commutative cases. It uses
*mathic*'s data structures — KD-trees, divisor lookups, Janet/Pommaret-style
tables — for fast leading-monomial work. The library has its own internal
`PolyRing` / `Polynomial` types.

## What the interface does

The interface has two jobs:

1. **Translate** an engine `PolynomialRing` plus input matrix into the form
   mathicgb consumes:
   - monoid → `mathicgb::PolyRing::Field`-style descriptor,
   - input columns → `mathicgb::Polynomial` instances.
2. **Translate back** the resulting basis as engine `gbvector`s.

Because the two libraries use different monomial encodings, value types,
and degree conventions, translation is non-trivial. The header defines
helper templates that walk the engine's `gbvector` and call into mathicgb's
`PolyAdder` / `PolyReader`.

## Why a separate engine

- mathicgb is **signature-based**, which can outperform Buchberger for
  certain large monomial-heavy inputs.
- It supports better caching and threading patterns than the engine's
  legacy GB code, exposed via `mathicgb`'s public C++ API.
- Keeping it in a sibling repository means improvements there benefit M2
  without M2-side changes.

## When the interface is used

The dispatcher `GBComputation::choose_gb(...)` (see [`file-comp-gb.md`](file-comp-gb.md))
selects `MathicGBInterface` when:

- The ring is commutative and has a "compatible" monomial order (graded
  reverse lex over a field).
- The user passed `Strategy => MathicGB` (or the autodetection in
  [`m2/gb.m2`](../m2/README.md) preferred it).

Otherwise the engine falls back to [`gbA`](file-gb-default.md) or another
strategy.

## Submodule pointer

The header pulls includes from `mathicgb/` and `mathic/` paths that are
symlinks into [`submodules/mathicgb/`](../../submodules/README.md) and
[`submodules/mathic/`](../../submodules/README.md). The CMake build wires
this up via [`FindMathicgb.cmake`](../../cmake/README.md); the autotools
build does it via `libraries/mathicgb`.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`file-comp-gb.md`](file-comp-gb.md) — dispatcher that selects the strategy.
- mathicgb submodule under [`submodules/`](../../submodules/README.md).
- mathic submodule (data structures used by mathicgb).
