# `testing.m2` — `TestInput` and `check` machinery

`testing.m2` provides the **M2-side testing framework** —
`check Package`, `TestInput` declarations inside packages, and the
test-runner that captures expected output and verifies against actual
output.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
needs "packages.m2"
needs "code.m2"
needs "run.m2"

-----------------------------------------------------------------------------
-- TestInput
-----------------------------------------------------------------------------
TestInput = new SelfInitializingType of HashTable
TestInput.synonym = "test input"
```

The `TestInput` type wraps a single test:

- Source M2 code.
- File position (so failing tests give meaningful locations).
- Expected outcome.

## User-facing API

- **`check Package`** — run all tests in a package; report failures.
- **`TEST ///...///`** — declare a test inside a package source file.
  Each `TEST` block becomes a `TestInput` registered with the package.

Inside a `TEST` block, the user writes assertions:

```m2
TEST ///
    R = QQ[x, y]
    I = ideal(x^2)
    assert(numgens I == 1)
    assert(I_0 == x^2)
///
```

`assert` raises an error on failure; the test runner catches it and
records the failure.

## How tests run

`check Package`:

1. Iterates over the package's registered `TestInput`s.
2. For each, spawns a **fresh M2 subprocess** running just that
   test.
3. Captures the exit code and any stderr.
4. Reports successes / failures / errors.

The fresh-subprocess discipline mirrors
[`file-examples.md`](file-examples.md) — tests can't influence each
other.

## Tests vs. examples

`TEST` vs. `EXAMPLE` are similar but serve different purposes:

| | `TEST` | `EXAMPLE` |
|---|---|---|
| Purpose | Verify behaviour | Demonstrate usage |
| Output captured | No | Yes |
| Appears in docs | No | Yes |
| Failure | Error | Warning |

A package author typically writes both: examples that teach, tests
that verify.

## Used by

- Every M2 package — most have a `TEST` block per major feature.
- `check Package` invoked by users and by CI.
- The [`tests/`](../tests/README.md) infrastructure for top-level
  M2-script regression tests.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-examples.md`](file-examples.md) — sister capture
  mechanism.
- [`file-packages.md`](file-packages.md) — package-level
  registration.
- [`file-installPackage.md`](file-installPackage.md) — also runs
  `TEST` blocks during install.
