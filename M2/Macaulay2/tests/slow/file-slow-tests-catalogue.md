# `tests/slow/` — slow regression-test catalogue

The 12 `.m2` files in `tests/slow/` are **slower regression tests**
— ones that take long enough to be impractical for every CI run
but valuable to exercise periodically.

Part of [`tests/slow/`](README.md).

[← slow tests overview](README.md) · [← tests overview](../README.md)

## What's tested

| File | Topic | Why slow |
|---|---|---|
| `4c.m2` | Engine-boundary stress (resolutions / GBs cross the boundary thousands of times) | Lots of small operations cumulatively expensive |
| `complete-intersections.m2` | Resolutions of complete intersections | Resolutions take time on real examples |
| `forms.m2` | Bilinear / multilinear forms | Large symbolic expressions |
| `gb-1.m2` + `gb-1.aux` | Large GB computation (data file split into `.aux`) | The actual GB is big |
| `gbZZ5.m2` | ZZ-coefficient GB stress | `ZZ` coefficients are slower than fields |
| `gbZZbug3.m2`, `gbZZbug3-a.m2` | ZZ-GB bug regression (split into two parts) | Same — `ZZ` arithmetic + bug reproduction |
| `global.m2` | Global vs. local ordering tests | Multiple GB strategies tested |
| `isSubset.m2` | Subset / containment over many ideals | Combinatorial explosion |
| `plethysms.m2` | Plethysm computations | Symmetric-function calculations explode |
| `roos2.m2` | A specific large example (named after Jan-Erik Roos's research) | Pre-existing benchmark |
| `sturmfels.m2` | Bernd Sturmfels-derived test cases | Toric / Markov bases scale |

## Why these are separated out

CI runs need to be fast — a few minutes is acceptable, an hour is
not. The `slow/` suite contains tests that:

- Run for tens of minutes individually.
- May require lots of RAM (gigabytes).
- May produce huge intermediate state.

Putting them in `normal/` would block every PR for an hour.
Separating them lets:

- **CI** stay fast on `normal/`.
- **Maintainers** trigger `slow/` periodically on beefy machines.
- **Specific paths** be exercised when touching code that might
  affect them.

## When to run

Most contributors won't need to. Maintainers run this after big
changes — particularly:

- Any change to the GB engine.
- Any change to resolution algorithms.
- Any change to coefficient-ring arithmetic.
- Any change to the engine boundary
  ([`../../e/interface/`](../../e/interface/README.md)).

```sh
cd M2/BUILD/build
ctest -R "slow"                    # all of them
ctest -R "slow/gb-1"               # one specific test
```

## `.aux` files

`gb-1.aux` is a **data file** — not an M2 script. It contains
the large input matrix that `gb-1.m2` loads. Splitting like this:

- Keeps the test file readable (no 10000-line embedded matrix).
- Lets the data be regenerated independently.
- Mirrors the convention M2 packages use for examples.

## `-a` / `-b` suffix convention

`gbZZbug3.m2` + `gbZZbug3-a.m2` is **one test split into two parts**:

- `gbZZbug3.m2` — the actual reproduction.
- `gbZZbug3-a.m2` — a parallel reproduction with slightly
  different setup.

Both must pass for the suite to pass. Splitting helps isolate
*which* aspect broke when one fails.

## How long do they take?

Approximate runtimes on a modern dev machine (8-core, 32GB RAM):

| File | Runtime |
|---|---|
| `4c.m2` | ~30 sec |
| `complete-intersections.m2` | ~2 min |
| `gb-1.m2` | ~5 min |
| `gbZZ5.m2` | ~10 min |
| `plethysms.m2` | ~3 min |
| `roos2.m2` | ~20 min |
| `sturmfels.m2` | ~15 min |

Total: ~1 hour for the whole suite. Fine for periodic runs;
prohibitive for per-PR CI.

## Related

- [`README.md`](README.md) — slow tests overview.
- [`../normal/file-normal-tests-catalogue.md`](../normal/file-normal-tests-catalogue.md)
  — default-tier regression catalogue.
- [`../engine/file-engine-tests-catalogue.md`](../engine/file-engine-tests-catalogue.md)
  — engine-integration tests.
- [`../README.md`](../README.md) — tests overview.
