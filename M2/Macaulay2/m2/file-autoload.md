# `autoload.m2` — `autoload` (lazy-load on first reference)

`autoload.m2` implements the **`autoload`** mechanism — a way to
declare that a symbol's value lives in a separate file and should
be loaded the first time the symbol is dereferenced (rather than at
M2 startup).

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"

autoload = method()
autoload(Symbol, String) := (sym, filename) -> (
    if value sym =!= sym then error ("symbol ", toString sym, " already has a value");
    sym <- f := x -> (
        load filename;
        if f === value sym
        ...
    )
)
```

The pattern:

1. Check that the symbol isn't already defined.
2. Set the symbol's value to a **trampoline closure** `f`.
3. The trampoline, when called, `load`s the file (which is expected
   to redefine the symbol with its real implementation).
4. If the load succeeded and the symbol was redefined, the
   trampoline retires.

`autoload(symbol foo, "foo-impl.m2")` ⇒ first call to `foo`
triggers `load "foo-impl.m2"`.

## Why this matters

M2 has hundreds of `.m2` files in Core. If every one had to fully
load at startup, M2 wouldn't be usable interactively. `autoload`
lets the bootstrap path stay tight: only essential symbols are
populated immediately; the rest are autoload trampolines.

When the user actually uses a feature, the relevant file loads on
demand.

## Comparison with `needsPackage` vs `loadPackage`

- **`needsPackage`** — loads a package eagerly. The user typed
  it explicitly.
- **`loadPackage`** — same as `needsPackage` but always reloads.
- **`autoload`** — the *symbol* triggers the load. The user might
  not even know they're calling something deferred.

Different mechanisms for different scopes.

## Used by

- Core's startup path — many less-essential Core symbols are
  autoloaded.
- Packages that want to split their own implementations across
  multiple files for the same lazy-load reason.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-packages.md`](file-packages.md) — heavyweight load
  mechanism.
- [`file-loadsequence.md`](file-loadsequence.md) — `loadsequence`
  determines what's loaded eagerly.
- [`file-methods.md`](file-methods.md) — `method()` declarations.
