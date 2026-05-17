# `last.m2` — final load-sequence file

`last.m2` is — true to its name — the **last** `.m2` file loaded
in the Core load sequence. It performs the final wiring that
requires everything else to already exist: finalising package
registrations, attaching `AfterPrint` methods that depend on the
full type hierarchy, computing derived data caches.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
--		Copyright 1993-2001 by Daniel R. Grayson

-- this file should be mentioned *last* in dumpseq

needs "engine.m2"
needs "methods.m2"
needs "nets.m2"
needs "monoids.m2"
needs "packages.m2"
needs "robust.m2"
```

The comment is unambiguous: **mentioned last in dumpseq**
(`dumpseq` is an older name for `loadsequence`). Inserting a file
after `last.m2` would break things.

## What it does

The file's responsibilities:

- **Final method installations** — methods that need every type to
  be declared before they can register (since their dispatch tables
  depend on the complete hierarchy).
- **Cache initialisation** — derived caches like the
  `synonym` plural-form table, the `methods` look-up index,
  precedence tables.
- **`AfterPrint` hooks** — most types' "what to show after the
  value prints" hooks are wired here, since they need access to the
  whole type system.
- **`Core` package finalisation** — `endPackage Core` is called
  here, sealing Core's public symbol set.
- **Welcome banner / startup messages** — emitted in interactive
  mode.

## Why the order matters

Some methods can only be installed after everything they need to
look up has been declared. For example, an `AfterPrint Module`
method that walks the module's free-resolution display needs
`Complexes` / `ChainComplex` to exist. Putting those methods in
`last.m2` is the engine's way of saying "wait until everything's
ready."

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-loadsequence.md`](file-loadsequence.md) — `last.m2` is
  literally the last entry.
- [`file-Core.md`](file-Core.md) — the package being finalised.
- [`file-packages.md`](file-packages.md) — `endPackage` definition.
