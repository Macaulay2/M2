# `tests/normal/` — default-tier regression test catalogue

The 373 `.m2` files in this directory are M2's **default-tier
regression test suite** — the tests CTest runs on every CI build.
Each is a small M2 script asserting some specific behaviour.

Part of [`tests/normal/`](README.md).

[← normal tests overview](README.md) · [← tests overview](../README.md)

## How tests are organised

The directory is **flat** — all 373 files in one place, sorted
alphabetically by filename. Names hint at the area being tested,
not always systematically.

A typical test (`0-homog.m2`):

```m2
-- fixed: 4/25/2010

assert( 97^2 < (options GF).SizeLimit )
k = GF 97^2
assert isHomogeneous matrix 1_k
assert( 101^2 > (options GF).SizeLimit )
k = GF 101^2
assert isHomogeneous matrix 1_k

R = QQ[x, y, z]/(-z^9+x*y^2)
S = R [w_0, w_1, w_2, Degrees => {{1, 1}, {1, 1}, {1, 1}}, Heft => {0, 1}]
I = ideal(z*w_1-y*w_2, z*w_0-x*w_2, y*w_0-x*w_1, ...)
C = res(I, LengthLimit => 4)
scan(0 .. 4,  { 1, 6, 11, 12, 12 }, (i,r) -> assert( numgens C_i == r ))
```

The pattern:

1. **Setup** — construct a small ring / ideal / module.
2. **Compute** — invoke the operation under test.
3. **Assert** — `assert(...)` invariants.

Tests pass if no assertion fails. Failures are reported by name.

## File-name conventions

The flat layout uses leading characters as a soft category marker:

| Prefix | Meaning |
|---|---|
| `0-`, `00-`, `4-` | Numeric prefixes — historical, no current meaning |
| Capital letter prefix (`L00a`, `B44`) | "Bug NNN fixed" tests, after the original issue's letter+number |
| Descriptive name (`adjoint.m2`, `Ext.m2`, `LU.m2`, `RRi.m2`) | Topic-named, the most readable convention |
| `*-bug.m2`, `*Test.m2` | Bug-specific or named-test files |

The mix is the result of 30 years of accretion. New tests
generally use descriptive names; old ones haven't been renamed
because the names appear in PR / commit history.

## What gets tested

**Foundations:**

- Ring construction over every supported coefficient ring (`GF.m2`,
  `RRi.m2`, `QQ-no-vars.m2`, ...).
- Module / matrix construction and arithmetic.
- Cross-engine boundary (M2 ↔ engine values, no corruption).

**Algorithms:**

- Gröbner bases — default and per-strategy paths (`gb-*.m2`).
- Resolutions — across algorithm variants (`res-*.m2`,
  `4-res-tower-crash.m2`).
- Hilbert function, Betti tables.
- LU decomposition (`LU.m2`), determinants, rank.
- Polynomial factorisation, GCD.

**Edge cases:**

- Empty inputs (`QQ-no-vars.m2`).
- Single-variable rings.
- Very high-degree polynomials.
- Many-variable rings.
- Quotient rings, fraction fields, local rings.

**Bugs:**

- `*-bug.m2` files are **regression tests for specific bug fixes**.
  Each one prevents a known-fixed bug from recurring.

## CI integration

CTest runs every `.m2` file:

```sh
ctest --output-on-failure                  # all default-tier
ctest --output-on-failure -R "normal/gb"   # filter to GB tests
```

Per-test, CTest:

1. Spawns `M2` with the script.
2. Captures exit code.
3. Reports pass (0) or failure (non-zero).

The whole suite typically runs in ~5-10 minutes.

## How to add a test

For a new feature:

1. Pick a descriptive filename (`<feature>.m2`).
2. Open with a `-- fixed: YYYY-MM-DD` comment if it's a bug fix.
3. Set up the example.
4. Use `assert(...)` for invariants.
5. Drop the file into this directory.

CMake/CTest will auto-discover it.

## Related

- [`README.md`](README.md) — normal tests overview.
- [`../engine/file-engine-tests-catalogue.md`](../engine/file-engine-tests-catalogue.md)
  — sister CI-skipped engine tests.
- [`../slow/file-slow-tests-catalogue.md`](../slow/file-slow-tests-catalogue.md)
  — slower variants kept out of default CI.
- [`../README.md`](../README.md) — broader test-suite tour.
- [`../../packages/file-EngineTests.md`](../../packages/file-EngineTests.md)
  — modern package-based engine tests.
