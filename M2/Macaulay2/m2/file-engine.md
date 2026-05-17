# `engine.m2` — M2-side engine bridge

`engine.m2` declares the **engine bridge types** the rest of the M2
Core uses to interact with the engine: `RawObject`, `RawMonomial`,
`RawMatrix`, `RawRing`, etc. These are the opaque-pointer wrappers
the engine returns; M2 code typically holds them via higher-level
types and only manipulates the `Raw*` directly when crossing the
boundary.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "expressions.m2"
needs "integers.m2"

spliceInside = x -> new class x from deepSplice toSequence x

-- basic type

setAttribute(RawObject, ReverseDictionary, symbol RawObject)
```

The `setAttribute(...)` line is the canonical M2 way to bind a type
to its public symbol. Once set, `RawObject` can be printed and
queried by name.

## `Raw*` type hierarchy

`engine.m2` declares the wrapper types for engine-side values:

- **`RawObject`** — the abstract base.
- **`RawRing`** — opaque `Ring*` from the engine.
- **`RawRingElement`** — opaque `RingElement*`.
- **`RawMonomial`** — opaque `EngineMonomial*`.
- **`RawMonomialIdeal`** — opaque `MonomialIdeal*`.
- **`RawMatrix`** — opaque `Matrix*`.
- **`RawMutableMatrix`** — opaque `MutableMatrix*`.
- **`RawFreeModule`** — opaque `FreeModule*`.
- **`RawRingMap`** — opaque `RingMap*`.
- **`RawComputation`** — opaque `Computation*`.
- ... and others.

Each type is essentially "an opaque pointer the engine returned;
don't poke inside."

## How M2 uses Raw* values

When the M2 layer wraps an engine value, it stores the `Raw*` inside
a `HashTable`-backed M2 type:

```m2
Matrix = new Type of HashTable
-- field f.RawMatrix holds a RawMatrix value
```

The user never sees the `Raw*` directly — they get the M2 wrapper.
Inside M2 code, `raw m = m.RawMatrix` extracts the engine pointer
when needed to call an engine function.

## `spliceInside`

```m2
spliceInside = x -> new class x from deepSplice toSequence x
```

A small helper that takes a structured value `x`, converts it to a
sequence, deep-splices (flattens nested sequences), and reconstructs.
Used by various engine-call helpers to massage their input shapes.

## Used by

- Every M2 file that touches engine values.
- [`file-enginering.md`](file-enginering.md), [`file-matrix.md`](file-matrix.md),
  [`file-modules.md`](file-modules.md), etc. — primary consumers.
- M2's `raw f` extractor — uses these type declarations.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-enginering.md`](file-enginering.md) — `EngineRing` consumer.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) — engine's
  aggregating header.
- [`../d/engine.dd`](../d/README.md) — interpreter-side bridge.
