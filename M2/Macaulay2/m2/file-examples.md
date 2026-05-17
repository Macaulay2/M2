# `examples.m2` — example execution + capture machinery

`examples.m2` defines **`EXAMPLE`**, the **example-execution runner**
used by package documentation, and the `capture` function that
records example outputs for inclusion in HTML / info docs.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-----------------------------------------------------------------------------
-- Methods for processing and accessing examples from the documentation
-----------------------------------------------------------------------------
-* Exported:
 * EXAMPLE
 * capture
 * examples
 *-

processExamplesStrict = true

needs "hypertext.m2"
needs "run.m2"
needs "document.m2"  -- for DocumentTag
```

Three exports:

- **`EXAMPLE`** — used inside `document{...}` blocks to mark a piece
  of code to be executed and rendered with its output.
- **`capture`** — programmatically capture the output of running an
  M2 string.
- **`examples`** — retrieve the list of `EXAMPLE` blocks attached to
  a doc node.

`processExamplesStrict = true` controls whether unexpected output in
example execution is a hard error or a soft warning. Strict mode is
the default; turning it off is a development convenience.

## How example execution works

When [`file-installPackage.md`](file-installPackage.md) processes a
documentation node containing an `EXAMPLE`:

1. The example's M2 source is written to a temporary file.
2. A **fresh M2 subprocess** is spawned (no state leak from the parent).
3. The subprocess runs the example, capturing stdout and stderr.
4. The output is appended to the example for HTML / info rendering.
5. If the subprocess exits abnormally and `processExamplesStrict` is
   true, the package install fails.

The fresh-subprocess discipline ensures examples are reproducible
and don't depend on hidden state in the parent M2 session.

## `EXAMPLE` block conventions

```m2
EXAMPLE lines ///
    R = QQ[x, y]
    I = ideal(x^2 - y)
    gb I
///
```

`lines ///...///` parses the indented multi-line string into a
sequence of lines that `EXAMPLE` then executes.

## Caching

To avoid re-running every example on every `installPackage`, M2
caches example outputs in a `.example-outputs` database keyed by the
package version + source hash. Cached results are reused when the
example source hasn't changed.

## Used by

- Every M2 package that documents examples.
- [`file-installPackage.md`](file-installPackage.md) — primary
  caller.
- The test infrastructure ([`../tests/`](../tests/README.md)) —
  reuses some of the same capture machinery.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-installPackage.md`](file-installPackage.md) — primary
  caller.
- [`file-document.md`](file-document.md) — DSL embedding `EXAMPLE`.
- [`file-packages.md`](file-packages.md) — package machinery.
- `run.m2` — subprocess management.
