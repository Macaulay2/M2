# `Core.m2` — the Core package definition

`Core.m2` is the **first** Macaulay2 source file loaded at startup
and the file that creates the `Core` package — the set of symbols
every M2 user gets without an explicit `needsPackage` call.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What it contains

```m2
--		Copyright 1993-2003 by Daniel R. Grayson
-- The source code of Macaulay2 is contained in multiple files,
-- all contained in the subdirectory "Macaulay2/".

-- newPackage is called in packages.m2!
CorePackage = (
    "Core",
    Date     => version#"compile time",
    Version  => version#"VERSION",
    Headline => "a computer algebra system designed to support algebraic geometry",
    HomePage => "https://Macaulay2.com/",
    Authors => {
        {Name => "Daniel R. Grayson", ...},
        {Name => "Michael E. Stillman", ...},
    },
    DebuggingMode => debuggingMode,
    Reload => true,
)
```

Two things to note:

1. **`CorePackage` is a tuple**, not a package object. The actual
   `newPackage(...)` call happens later, in `packages.m2`, using
   this tuple as input. Splitting "describe the package" from
   "instantiate the package" lets `packages.m2` itself be loaded
   first.
2. **`Reload => true`** — the Core package supports being reloaded
   while M2 is running. This is used during development.

## Why Core is special

Most M2 packages live as standalone files in
[`Macaulay2/packages/`](../packages/README.md). The Core package is
different:

- Its sources are split across ~100 `.m2` files in
  [`m2/`](README.md).
- The load order is governed by [`loadsequence`](file-loadsequence.md).
- It bundles into the compiled `M2-binary` (rather than being
  loaded on demand).
- It defines everything that's "in scope" at the M2 prompt.

`Core.m2` is the file that orchestrates this: when M2 starts, the
interpreter loads `Core.m2` first, which arranges for the rest of the
`.m2` files to follow in the right order.

## The package object

After all of Core's `.m2` files have loaded, the package is
materialised by `packages.m2`. From that point on, `Core` is a
package object accessible at the M2 prompt:

```m2
Core
-- Core
class Core
-- Package
options Core
-- {Authors => {{"Daniel R. Grayson", ...}, ...}, Date => "...", ...}
```

Every symbol declared anywhere in `m2/` becomes a member of `Core`.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-loadsequence.md`](file-loadsequence.md) — load order.
- [`file-exports.md`](file-exports.md) — public-symbol declarations.
- [`packages.m2`](README.md) — registers Core as a package.
- [`../packages/README.md`](../packages/README.md) — sibling
  user-installable packages.
