# Small test suites — `goals/`, `gigantic/`, `threads/`, `quarantine/`, `rationality/`

The four-to-nine-file test suites are catalogued here together
for proximity. Each has its own README; this doc gives a single
consolidated view.

Part of [`tests/`](README.md).

[← tests overview](README.md)

## `goals/` — worked exercises

3 `.m2` files plus build glue. Each is a **target computation** —
a "can M2 do this?" benchmark posed at some point in the
project's history.

| File | Origin |
|---|---|
| `ISSAC-97.m2` | A problem from the ISSAC 1997 conference |
| `gbZZ.m2` | A specific GB-over-`ZZ` challenge |
| `joswig.m2` | A Joswig-tropical-geometry problem |

These are kept around to:

- Verify M2 can still do them after refactors.
- Inform future feature work.
- Demonstrate computational reach to new users.

## `gigantic/` — extreme-size tests

1 `.m2` file, but a brutal one. `gbmatrix.m2` builds an extremely
large matrix and computes its GB. Used to:

- Stress-test memory management on big inputs.
- Catch integer overflow in monomial encodings.
- Verify the engine doesn't crash at scale.

Almost never run. When run, can take hours on beefy machines.

## `threads/` — concurrency tests

1 `.m2` file. `schur-2.m2` exercises M2's `Task` /
`schedule` machinery (see
[`../d/file-threads.md`](../d/file-threads.md) and
[`../system/file-supervisor.md`](../system/file-supervisor.md)) by
running a Schur-polynomial computation across multiple worker
threads.

Why only one test? Thread-correctness is hard to test
programmatically — a missing race condition won't fail the test
until it does, intermittently. The current single test is a
smoke check that the threading machinery works at all.

## `quarantine/` — temporarily-disabled tests

9 `.m2` files **awaiting fixes**. The README's metaphor: they're
"in quarantine" because they once worked but currently fail, and
their fixes haven't been prioritised.

| File | Why quarantined |
|---|---|
| `2-homog-bug.m2` | Homogeneous-degree edge case |
| `C05.m2` | Specific historical bug |
| `HH.m2` | Hom-functor edge case |
| `issac-97.m2` | (vs. `goals/ISSAC-97.m2`) — likely a different variant |
| `ker8.m2` | Kernel-computation specific case |
| `lapack.m2` | LAPACK boundary issue |
| `newlines.m2` | Output formatting |
| `res9.m2` | Resolution-engine specific case |

Quarantine is a **valuable engineering signal**: someone wrote
this test, it broke, the breakage is acknowledged, but the fix
is non-trivial. Moving here keeps CI green while preserving the
known-failing case.

## `rationality/` — rationality-question tests

9 `.m2` files focused on **rationality questions in algebraic
geometry**:

| File | Topic |
|---|---|
| `ScorzaOcta.m2`, `ScorzaOcta-fixed.m2` | Scorza's classification of cubic surfaces |
| `plausibility.m2`, `plausibility1.m2`, `plausibility2.m2`, `plausibility3.m2` | "Plausibility" tests for rationality assertions |

These are research-driven tests — added when someone was working
on rationality computations and wanted regression coverage. The
multiple `plausibility*.m2` files are likely test data split for
modularity.

## `ComputationsBook/` — examples from the M2 book

The largest of the small suites (1+ `.m2` files plus several
subdirs of chapter examples). Contains examples from:

> *Computations in Algebraic Geometry with Macaulay 2*
> (Eisenbud, Grayson, Stillman, Sturmfels, eds.)

Run via `ctest -R ComputationsBook`. The subdirs (`constructions/`,
`d-modules/`, `exterior-algebra/`, `geometry/`, etc.) mirror the
book's chapter structure.

## Used by

- CTest (most run in default tier; some not).
- Maintainers periodically.
- Anyone curious about M2's historical computational reach.

## Related

- [`README.md`](README.md) — tests overview.
- [`normal/file-normal-tests-catalogue.md`](normal/file-normal-tests-catalogue.md)
  — the default tier.
- [`slow/file-slow-tests-catalogue.md`](slow/file-slow-tests-catalogue.md)
  — slower tests.
- [`engine/file-engine-tests-catalogue.md`](engine/file-engine-tests-catalogue.md)
  — engine integration.
