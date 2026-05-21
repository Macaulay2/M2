# `programs.m2` — `Program` and `ProgramRun` (external program detection)

`programs.m2` defines the **`Program`** and **`ProgramRun`** types —
M2's abstraction over external programs (`4ti2`, `bertini`, `gfan`,
`normaliz`, etc.) that packages dispatch to. It manages program
detection, path resolution, version checks, and invocation.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "methods.m2"

Program     = new Type of HashTable
ProgramRun  = new Type of HashTable
programPaths = new MutableHashTable

fixPath = programPath -> (
    -- escape any unescaped spaces or parentheses
    programPath = replace(///(?<!\\)([ ()])///, ///\\\1///, programPath);
    -- we expect a trailing slash in the path, but the paths given in the
    ...
)
```

Two types:

- **`Program`** — represents an external program. Carries name,
  executable path, version, capabilities.
- **`ProgramRun`** — represents a single invocation. Captures stdin,
  stdout, stderr, exit code, timing.

Plus a mutable `programPaths` table that maps program names to
detected paths.

## User-facing API

- **`findProgram(name, ...)`** — locate an external program.
  Searches `PATH`, common install locations, and platform-specific
  directories.
- **`runProgram(prog, args, ...)`** — invoke the program. Captures
  output, applies timeouts, returns a `ProgramRun`.
- **`addStartFunction f`** — register a function to run at M2
  startup (used by packages to set up their programs).

## `fixPath`

The local `fixPath` helper:

- Escapes spaces and parentheses in paths so they survive shell
  invocation.
- Normalises trailing slashes.

These are the kinds of cross-platform path bugs that bite packages
when users install programs in `C:\Program Files\…` or
`/Applications/MyTool.app/…`.

## Used by

- Every package that dispatches to an external program:
  `FourTiTwo`, `Bertini`, `Normaliz`, `Polyhedra`, `gfanInterface`,
  etc.
- The build / install workflow when verifying program availability.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-system.md`](file-system.md) — `run` / `chkrun` primitives.
- [`file-files.md`](file-files.md) — filesystem operations.
- `../../libraries/` — distribution-side wrappers for the programs
  this file detects.
