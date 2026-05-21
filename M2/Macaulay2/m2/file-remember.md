# `remember.m2` — `memoize` (function-result caching)

`remember.m2` provides **`memoize`** — the standard Macaulay2
function-result cache. Wrapping a function with `memoize` turns it
into one that caches `(args, result)` pairs and returns cached
results for repeated calls.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"

memoize = method()

memoize(Function, List) := (f, initialValues) -> (
    values := new MutableHashTable from initialValues;
    x -> (
        -- This code is common to any function that has been memoized with initial values.
        ...
    )
)
```

`memoize` returns a wrapper function whose closure holds:

- A `MutableHashTable` mapping argument tuples to cached results.
- Optionally a list of pre-populated `initialValues` (so e.g.
  `factorial 0 == 1` is in the cache before the first call).

The wrapper:

1. Checks the cache for the argument.
2. If present, return the cached value.
3. Otherwise call `f`, cache the result, return it.

## Why caches matter

Recursive M2 functions like `binomial`, `partitions`, `subsets`
without memoisation are exponential. With `memoize`, they become
linear in the number of distinct argument tuples. The technique is
worth ~10-100× speed-up on typical combinatorial computations.

## Variants

The file also defines:

- **`memoize(Function)`** — same but with empty initial cache.
- **`memoize(Function, List)`** — with initial entries.

## Caveats

`memoize` caches forever — there's no eviction. For pure functions
this is fine; for functions that aren't pure (depend on global
state), caching is wrong. The user must ensure purity before
wrapping.

The cache lives in the function's closure, so it persists for the
function's lifetime. If you want session-scoped caching, that's
just how `memoize` works. If you want per-call caching, use
`cacheValue` instead (defined in `enginering.m2` or nearby).

## Used by

- [`file-combinatorics.md`](file-combinatorics.md) — recursive
  functions all wrapped in `memoize`.
- Many M2 packages — `gcd` cache, polynomial-factorisation cache,
  ...
- Documentation paths in
  [`file-installPackage.md`](file-installPackage.md) — caches example
  outputs.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-combinatorics.md`](file-combinatorics.md) — primary user.
- `cacheValue` (defined elsewhere) — per-object caching alternative.
- [`file-methods.md`](file-methods.md) — methods themselves cache
  dispatch decisions.
