# `packages.m2` — package machinery

`packages.m2` implements **`newPackage`**, **`needsPackage`**,
**`loadPackage`**, and the underlying machinery for tracking loaded
packages, their public symbols, dependencies, and lifecycle.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
--		Copyright 1993-2003 by Daniel R. Grayson
-- TODO: eventually we won't be able to keep all packages open, anyway, since 256 can be our limit on open file descriptors

needs "code.m2"
needs "files.m2"
needs "fold.m2"
needs "lists.m2"
needs "methods.m2"
needs "regex.m2"
needs "system.m2"
needs "hypertext.m2"

loadedPackages = {}

rawKey   = "raw documentation"
```

The TODO captures a long-running design concern: M2 keeps every
loaded package permanently in memory. With 400+ packages in the
distribution, opening them all at once would exhaust file
descriptors and memory. In practice users load on demand; the
concern is real but not yet binding.

`loadedPackages` is the **flat list** of currently loaded packages.

`rawKey` is the magic key under which documentation is stashed in
each package's hash table — used by
[`file-installPackage.md`](file-installPackage.md) and
[`file-help.md`](file-help.md).

## User-facing API

- **`newPackage(name, ...)`** — declare a new package. Called at the
  top of every `.m2` package file. Sets up the package's symbol
  table, options, and documentation slot.
- **`needsPackage name`** — load a package if not already loaded.
  Idempotent: subsequent calls are no-ops.
- **`loadPackage(name, Reload => true)`** — force-reload a package.
  Used during package development.
- **`installPackage name`** — render the package's documentation
  ([`file-installPackage.md`](file-installPackage.md)).
- **`endPackage name`** — close a package, sealing its public
  symbol set.

## How the Core package is created

The Core package is special — it's not loaded via `needsPackage`
because Core is what `needsPackage` lives in. Instead,
[`file-Core.md`](file-Core.md) constructs the package tuple, and
`packages.m2` is the file that actually calls `newPackage(CorePackage)`
during startup to materialise it.

## Per-package state

Each loaded package is a hash table with:

- `Options` — the options the user passed to `newPackage`.
- `Title`, `Headline`, `Authors`, ... — metadata.
- `Dictionary` — the symbol table.
- `raw documentation` — the doc-node database (under
  [`file-document.md`](file-document.md)).
- `exampleHashes` — cached example outputs for
  [`file-installPackage.md`](file-installPackage.md).

## Used by

- Every M2 package — `newPackage(...)` is the first line of every
  package file.
- Every M2 user — `needsPackage "X"` is the standard way to load X.
- The build system — `check-Foo` and `install-Foo` targets call
  through here.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-Core.md`](file-Core.md), [`file-loadsequence.md`](file-loadsequence.md)
  — Core bootstrap.
- [`file-document.md`](file-document.md) — docs system.
- [`file-installPackage.md`](file-installPackage.md) — package rendering.
- [`../packages/README.md`](../packages/README.md) — distributed
  packages.
