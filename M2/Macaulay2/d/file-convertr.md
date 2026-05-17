# `convertr.d` — late-bound function-pointer registry

`convertr.d` is the **"forward-reference holder"** — it declares
function variables (`AssignElemFun`, `NewFun`, `InstallMethodFun`,
…) initially set to dummy implementations, with the real ones
patched in from later files.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
--		Copyright 1994 by Daniel R. Grayson
use binding;
use common;
use util;

export AssignElemFun := dummyTernaryFun;	-- filled
export AssignQuotedElemFun := dummyTernaryFun;	-- filled
export NewFun := dummyUnaryFun;	  -- filled in later
export NewFromFun := dummyBinaryFun;	  -- filled in later
export NewOfFun := dummyBinaryFun;	  -- filled in later
export NewOfFromFun := dummyTernaryFun;	  -- filled in later

export AssignNewFun := dummyBinaryFun;
export AssignNewOfFun := dummyTernaryFun;
...
```

Long sequence of function-pointer declarations. Every one has a
"-- filled in later" comment.

## Why this pattern exists

Consider `installMethod`. Its definition needs `actors4.d` (lots
of high-level support), but other earlier files want to *call*
`installMethod`. Without `convertr.d`, you'd have a circular
dependency: actors4 imports lowlevel; lowlevel needs actors4 to
call install.

The convention here:

1. `convertr.d` declares `InstallMethodFun := dummyMultaryFun`.
2. Earlier files call `InstallMethodFun(...)` (dispatching through
   the variable).
3. Later, `actors4.d` writes the *real* `installMethod` and assigns
   `InstallMethodFun = installMethod;`.

After bootstrap, the variable holds the real function. Before
that, calls hit the dummy and error out — but bootstrap never
calls them, so it's fine.

## The functions managed here

The major ones:

- **`NewFun`, `NewFromFun`, ...** — `new X` constructors.
- **`AssignElemFun`** — `a.field = value`.
- **`InstallMethodFun`** — method installation.
- **`UnaryInstallMethodFun`** — install unary operators.

## When to add to `convertr.d`

Whenever you want to call a function from an early file that has
to be defined in a late file. The pattern is mature; adding new
entries is rare and usually means a refactor would be cleaner.

## Used by

- [`file-binding.md`](file-binding.md) — uses these dummies in
  semantic-analysis code paths.
- [`file-evaluate.md`](file-evaluate.md) — calls
  `InstallMethodFun`, `NewFun` etc. polymorphically.
- `actors*.d` files — install the real implementations.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-binding.md`](file-binding.md), [`file-actors.md`](file-actors.md)
  — fill-in sites for the dummies.
- [`file-evaluate.md`](file-evaluate.md) — primary caller.
