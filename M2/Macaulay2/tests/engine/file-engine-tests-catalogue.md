# `tests/engine/` — engine-integration tests catalogue

The 35 `.m2` files in this directory exercise the engine through
M2-level scripts, with **finer-grained coverage** than the default
`normal/` suite. Currently CI-skipped (per issue #1213); useful
for manual verification.

Part of [`tests/engine/`](README.md).

[← engine tests overview](README.md) · [← tests overview](../README.md)

## The `raw-*` family

The bulk of files exercise the engine's **`raw…()`-prefixed C
entry points** — the boundary functions in
[`../../e/interface/`](../../e/interface/README.md). They poke at
specific engine functions directly, where `normal/` tests go via
the higher-level M2 API.

| File | Tests |
|---|---|
| `raw-decompose.m2` | `rawIdealPrimaryDecomposition` |
| `raw-freemodule.m2` | `rawFreeModule`, `rawSubmodule`, etc. |
| `raw-gb.m2` | `rawGB`, `rawGBSet…`, `rawStartComputation` |
| `raw-hilb.m2` | `rawHilbertSeries`, `rawHilbertFunction` |
| `raw-hilbert-basis.m2` | `rawHilbertBasis` (Normaliz integration) |
| `raw-localgb.m2` | Local-order GB engines |
| `raw-mat-bug1.m2` | Specific historical matrix bug |
| `raw-matrix.m2` | Core matrix operations |
| `raw-monideal.m2` | `MonomialIdeal` primitives |
| `raw-monoid.m2` | `rawMonoid`, monomial ordering construction |
| `raw-monomial.m2` | Monomial arithmetic |
| `raw-mutable.m2` | `MutableMatrix` lifecycle |
| `raw-numerics.m2` | `rawSetSeed`, numerical entry points |
| `raw-res.m2` | `rawResolution`, `rawBettiTallies` |

Each test typically:

1. Constructs a small example via `raw…` calls.
2. Inspects intermediate state.
3. Asserts specific invariants.

## Higher-level engine tests

| File | Topic |
|---|---|
| `LU.m2` | LU decomposition correctness across rings |
| `gb-2.m2`, `gb-bayes.m2`, `gb-quotients.m2` | GB edge cases (Bayes-net ideals, quotients) |
| `gb-bench.m2` | Benchmark workloads (timing, not correctness) |
| `normal.m2` | General-purpose engine sanity tests |

## Why CI-skipped

The README at the top of [`engine/`](README.md) explains: many
tests were written against engine behaviour that has since drifted
(M2 has run for 30 years; engines evolve). Bringing them all green
requires either rewriting tests or adding compatibility shims.
Neither has been prioritised because:

- The [`normal/`](../normal/) suite catches regressions at the
  user-API level.
- The [C++ gtest](../../e/unit-tests/README.md) suite catches them
  at the engine internals level.
- The `EngineTests` package
  ([`../../packages/file-EngineTests.md`](../../packages/file-EngineTests.md))
  is the modern home for engine-integration tests.

So `engine/` here remains as a **historical reference** — useful for
debugging specific old issues, not as a routine CI gate.

## Running anyway

To run a single test outside CI:

```sh
cd M2/BUILD/build
./M2 --script ../../../Macaulay2/tests/engine/raw-gb.m2
```

A `pass` exit code means the test still works.

## Related

- [`README.md`](README.md) — engine tests overview.
- [`../normal/file-normal-tests-catalogue.md`](../normal/file-normal-tests-catalogue.md)
  — sister default-tier catalogue.
- [`../../e/unit-tests/README.md`](../../e/unit-tests/README.md) —
  C++ gtest suite (replaces some `engine/` coverage).
- [`../../packages/file-EngineTests.md`](../../packages/file-EngineTests.md)
  — modern M2-level engine test package.
- [`../../e/interface/README.md`](../../e/interface/README.md) —
  the `raw…` entry points these tests exercise.
