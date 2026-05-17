# `exports.m2` — Core package symbol exports

`exports.m2` is the **public-symbol manifest** for the Core package.
It is a long `export {...}` block listing every symbol Core exposes
to user code. The list runs to thousands of entries and is one of
M2's biggest files.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
needs "packages.m2"

export {
    "!",
    "!=",
    "#",
    "#?",
    "%",
    ...thousands more entries...
}
```

The list is organised loosely by section but isn't strictly sorted.
Symbols of all kinds are exported:

- **Operators** — `"!"`, `"!="`, `"#"`, `"#?"`, `"%"`, `"&"`, ...
- **Functions** — `"rank"`, `"degree"`, `"dim"`, ...
- **Type names** — `"Ring"`, `"Matrix"`, `"Module"`, ...
- **Option keys** — `"Strategy"`, `"DegreeLimit"`, ...
- **Constants** — `"infinity"`, `"null"`, ...

## Why a single file

Two reasons:

1. **Discoverability** — having every public symbol in one place
   makes it easy to grep for "is X exported?" without walking the
   loadsequence.
2. **Order independence** — the file is loaded near the end
   ([`file-last.md`](file-last.md)-ish), after every symbol has
   been declared, so the `export` is a no-shenanigans operation.

The downside is that adding a new public symbol requires editing
this file, and merge conflicts here are common.

## What `export` does

`export {sym1, sym2, ...}` marks each symbol as **visible** to user
code. Without it, symbols defined in Core are private to Core's own
namespace — user code couldn't reach them.

The full mechanism is in [`file-packages.md`](file-packages.md):
`export` registers the symbol in the package's public dictionary.

## Re-exports and intentional omissions

Some symbols are intentionally **not** exported:

- Helper functions that have generic names (`merge`, `combine`,
  `apply'`).
- Symbols meant for engine-side use only (`raw`, `rawXxxx`).
- Half-deprecated functions kept around but discouraged.

The `protect` mechanism (used pervasively elsewhere) is
complementary — `protect symbol X` prevents `X` from being shadowed
but doesn't change its visibility.

## Used by

- Every M2 user implicitly — the `export` list is what makes the M2
  prompt usable.
- Editor tools that need to know what symbols M2 provides for syntax
  highlighting / autocomplete.
- [`../editors/make-M2-symbols.m2`](../editors/README.md) — extracts
  this list to generate editor grammar files.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-packages.md`](file-packages.md) — `export` machinery.
- [`file-loadsequence.md`](file-loadsequence.md) — `exports.m2`
  comes near the end of the sequence.
- [`../editors/README.md`](../editors/README.md) — consumes this
  list to make grammar files.
