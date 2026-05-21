# `files.m2` — file and directory operations

`files.m2` provides M2's **file system operations** — reading,
writing, creating directories, scanning. It wraps the
engine/runtime calls and adds M2-side conveniences.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"

printpass := ID -> x -> (stderr << ID << ": " << x << endl; x)
fold3 := (f, x, v) -> (scan(v, y -> x = f(x, y)); x)
fold2 := (f, v) -> fold3(f, v#0, drop(v, 1))
mergeopts := x -> fold2((a, b) -> merge(a, b, last), x)
makeDir := name -> if name != "" and (not fileExists name or not isDirectory (name | "/.")) then mkdir name
```

The file is a mix of low-level helpers and user-facing functions.
Key user-facing entries (mostly defined below the snippet):

- **`get filename`** — read a file's full contents as a string.
- **`<<`** at the M2 prompt — write to a file (`s << "foo"` writes
  `"foo"` to file `s`).
- **`fileExists`, `isDirectory`, `readDirectory`** — probes.
- **`mkdir`, `mkpath`** — directory creation.
- **`copyFile`, `moveFile`, `removeFile`, `removeDirectory`** —
  modification.
- **`scanLines f path`** — process a file line-by-line.
- **`temporaryFileName()`** — generate a guaranteed-unique temp
  path.

## `makeDir`

```m2
makeDir := name -> if name != "" and (not fileExists name or not isDirectory (name | "/.")) then mkdir name
```

The `(name | "/.")` trick is M2's way of saying "treat as a
directory" — appending `/.` ensures the check sees the directory
itself, not a symlink that happens to share the name. Idiomatic M2
for "ensure this directory exists."

## Why a thin layer

The engine and runtime (via `d/files.d`) already provide raw file
I/O. `files.m2` adds:

- M2-friendly defaults (string returns, error checks).
- The `<<` operator overloads for streaming output.
- Helpers like `temporaryFileName` and `makeDir` that aggregate
  multiple low-level calls.

## Used by

- Every M2 user reading or writing files.
- [`file-installPackage.md`](file-installPackage.md) — produces a
  whole tree of output files.
- [`file-examples.md`](file-examples.md) — writes example
  intermediates.
- Every package that loads supplementary data.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`../d/files.d`](../d/README.md) — engine-side file I/O.
- `system.m2` ([`file-system.md`](file-system.md)) — running external
  commands and pipelines.
- `regex.m2` — file-content pattern matching.
