# `ComputationsBook/` — book-examples test catalogue

The 12 chapter subdirectories in `ComputationsBook/` replay the
M2 examples from the book *Computations in Algebraic Geometry
with Macaulay 2* (Eisenbud, Grayson, Stillman, Sturmfels, eds.,
Springer 2002). Each chapter is a **regression test**: a baseline
expected output is captured once, future M2 runs must match.

Part of [`ComputationsBook/`](README.md).

[← ComputationsBook overview](README.md) · [← tests overview](../README.md)

## The 12 chapter subdirectories

| Subdir | Chapter topic | Files |
|---|---|---|
| `preface/` | Book preface examples | 2 |
| `programming/` | M2 programming basics | 2 |
| `constructions/` | Standard ring / module constructions | 2 |
| `monomialIdeals/` | Monomial ideal computations | 2 |
| `completeIntersections/` | Complete-intersection examples (+ extra `simplify.m2`) | 3 |
| `exterior-algebra/` | Exterior algebra | 2 |
| `d-modules/` | D-modules | 2 |
| `schemes/` | Affine and projective schemes | 2 |
| `varieties/` | Varieties as schemes | 2 |
| `geometry/` | Algebraic geometry helpers (+ extra file) | 3 |
| `solving/` | Solving polynomial systems (+ `realroots.m2`) | 3 |
| `toricHilbertScheme/` | Toric Hilbert schemes (+ `minPres.m2`, `polarCone.m2`, `test-one-that-changes.m2`) | 5 |

## Files per chapter (canonical pattern)

Every chapter subdir contains the same five files:

```
chapter.m2                  ← the book's chapter code, replayable
chapter.out.expected        ← expected stdout from running chapter.m2
test.m2                     ← test harness invoking chapter.m2
test.out.expected           ← expected harness output
test.oldvalues              ← captured baseline values for assertions
patterns                    ← regex substitutions for output diffing
Makefile.in                 ← per-chapter build glue
README                      ← chapter-specific notes
```

Some chapters add **extra `.m2` files** for examples that the book
references but didn't fit into `chapter.m2` (e.g.
`completeIntersections/simplify.m2`,
`solving/realroots.m2`).

## How the regression test works

1. **Run** `chapter.m2` in a fresh M2 — capture stdout.
2. **Apply** `patterns` regex substitutions to normalise output
   (timestamps, memory addresses, etc.).
3. **Diff** against `chapter.out.expected`.
4. **Pass** if no diff; **fail** if differences appear.

The `patterns` file is crucial: M2's output legitimately changes
between versions (a different number of GC pauses, a different
random seed, etc.). The regex substitutions filter out non-mathematical
variation while preserving the mathematical content.

## `test.m2` and `test.oldvalues`

A second test mechanism for **value-level regressions**:

- `chapter.m2` runs the book examples.
- `test.m2` then asks for specific *values* (e.g., the degree of a
  specific module, the rank of a matrix).
- These values are recorded in `test.oldvalues`.
- The harness compares this run's values to `test.oldvalues`.

This catches subtle regressions where the output text formatting
hasn't changed but the underlying mathematical result has — a
nightmare scenario without explicit value checks.

## `capture.m2` — value-capture machinery

```m2
saveClearAll := clearAll
erase symbol clearAll
stash := new MutableHashTable
unstash := R -> if stash#?R then stash#R else R
F := openOut "test.oldvalues"
F << "-- version " << version#"VERSION" << endl;
F << "{" << endl
g := (a,b) -> F << "  " <<  a << " => " << b << "," << endl
f := (e,x) -> ...
```

The shared `capture.m2` script (one level up from the chapter
subdirs) is the **harness logic**. It:

1. Walks every defined symbol after `chapter.m2` runs.
2. For each, records the type, name, and value to
   `test.oldvalues`.
3. Writes a version header so future diffs know what version
   produced the baseline.

When `chapter.m2` is updated, regenerate `test.oldvalues` by
running with `capture.m2` in update mode.

## Why this matters

The book is M2's **canonical reference text**. If the engine
silently produces different answers than the book documents:

- Users following the book get confused.
- Reviewers can't reproduce the book's claims.
- The book itself becomes effectively unreliable.

This test suite is the **safety net** ensuring M2's outputs stay
aligned with the book.

## Running

```sh
cd M2/BUILD/build
ctest -R "ComputationsBook"           # all chapters
ctest -R "ComputationsBook/varieties" # one chapter
```

Each chapter is its own CTest test.

## Used by

- CI on every PR.
- Anyone touching code that might affect book-example output.

## Related

- [`README.md`](README.md) — ComputationsBook overview.
- [`../README.md`](../README.md) — tests overview.
- [`../normal/file-normal-tests-catalogue.md`](../normal/file-normal-tests-catalogue.md)
  — sister default-tier suite.
- *Computations in Algebraic Geometry with Macaulay 2* (Springer
  2002) — the source book.
