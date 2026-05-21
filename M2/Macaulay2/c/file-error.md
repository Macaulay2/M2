# `error.c`, `error.h` — translator error reporting

`error.c` provides the **error / fatal / assert reporting** that
`scc1` uses when source code is ill-typed, ambiguous, or otherwise
problematic.

Part of the [`c/` scc1 translator](README.md).

[← back to c/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Entry points

```c
void fatal(const char *s,...)
{
     va_list ap;
     va_start(ap,s);
     ...
}
```

The main API:

- **`fatal(fmt, ...)`** — printf-style error and exit. Used when
  something is so wrong recovery is impossible.
- **`fatalpos(node, fmt, ...)`** — same with a source position.
- **`error(node, fmt, ...)`** — error at a position; record it
  but allow analysis to continue.
- **`errorcount`** — global counter.

Plus a family of `assert*` macros:

```c
#define assertpos(e,f)     ((e) ? 0 : failpos(__FILE__, __LINE__,f))
#define assert(e)          ((e) ? 0 : fail(__FILE__, __LINE__))
```

These fire on translator-internal bugs (invariant violations).
`assertpos` includes a `.d` source-position hint; `assert` is for
"we already messed up before knowing the position."

## `#if 1` toggle

```c
#if 1
#define assertpos(e,f)     ((e) ? 0 : failpos(__FILE__, __LINE__,f))
...
#else
#define assertpos(e,f)     0
#define assert(e)          0
```

The `#if 1` block enables the assertions; switching to `#else`
makes them no-ops for "release" builds. In practice the asserts
are always on — `scc1` is used during development of M2, so
catching translator bugs early matters more than performance.

## Position formatting

Errors print position like:

```
file.d:42:7:(...): missing 'when' clause
```

The format comes from [`compat.c`](file-compat.md)'s `posfmt`
string — different on classic Mac (MPWC) than on Unix.

## `errorcount`

After processing, `scc1`'s `main()` checks `errorcount > 0` and
exits non-zero if so. The build system uses this exit code to
fail the build cleanly.

## Used by

- Every other `.c` file in `c/` reports errors via this module.
- [`file-chk.md`](file-chk.md) is the heaviest user — most errors
  are type-check errors.
- [`file-scc1.md`](file-scc1.md) — checks `errorcount` at exit.

## Related

- [`README.md`](README.md) — c/ overview.
- [`file-compat.md`](file-compat.md) — position format strings.
- [`file-readfile.md`](file-readfile.md) — source positions
  come from here.
