# `shared.m2` — method stubs shared across Core and packages

`shared.m2` is a **stub-definitions file**. It declares method names
(but no implementations) so multiple Core files and user packages can
register implementations against the same method without race
conditions.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Full content (excerpted)

```m2
-- in this file we write stub definitions (no content) for use by packages,
-- so multiple packages can use the same thing

needs "methods.m2"

-- methods

chi      = method()
euler    = method()
eulers   = method()
genera   = method()
-- ...
union    = method()
tensor   = method()
adjoint  = method()
-- ...
```

The file is intentionally minimal — just `name = method()` lines.
No implementations. Concrete behaviour gets added in other Core
files or in user packages.

## Why a separate stub file

Without `shared.m2`, two situations cause problems:

1. **Two Core files** both want to add methods to `chi`. Whichever
   loads second would discover `chi` already exists (a soft error in
   M2) — unless `chi` was pre-declared somewhere.
2. **A user package** wants to add `chi(MySheaf)` to the same `chi`
   used by Core. Without the stub, the package would either need to
   load Core's relevant file (creating a dependency) or risk creating
   a private `chi` that doesn't dispatch correctly.

`shared.m2` solves both: it declares `chi` once, very early in the
load sequence, with no implementations. Everyone else just registers
their own.

## What gets declared

Standard math vocabulary: `chi`, `euler`, `eulers`, `genera`,
`union`, `tensor`, `adjoint`, etc. Plus a handful of less-obvious
helpers that come up in multiple Core files.

The list grows over time. Adding an entry here has no cost — it just
reserves the name globally.

## Used by

- Every Core file that registers a method on a name declared here.
- Every user package that extends a Core-shared method.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-methods.md`](file-methods.md) — `method` declarations.
- [`file-loadsequence.md`](file-loadsequence.md) — `shared.m2` loads
  early.
- [`file-Hom.md`](file-Hom.md), [`file-multilin.m2`](file-multilin.md)
  — register methods on names from here.
