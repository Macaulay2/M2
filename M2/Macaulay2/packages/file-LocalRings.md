# `LocalRings.m2` — operations over localised rings `R_P`

The `LocalRings` package implements **commutative algebra over local
rings** — the localisation of a polynomial ring at a maximal or prime
ideal `P`. Provides `localRing(R, P)`, `hilbertSamuelFunction`,
`liftUp` (the global → local descent map), and the full battery of
localised counterparts to standard module operations (`localsyz`,
`localMingens`, `localModulo`, `localPrune`, `localResolution`).

The classical use case: study a singularity by localising the
ambient ring at the singular point and computing the standard
homological invariants there.

**Not auto-loaded**, but core enough that several auto-loaded
packages (e.g. [`Saturation`](file-Saturation.md), which this package
re-exports) integrate with local-ring inputs.

- Main file: `LocalRings.m2` (586 lines)
- Auxiliary directory: `LocalRings/` (8 files, **2 836 lines**)
- Authors: Mahrud Sayrafi, Mike Stillman, David Eisenbud
- Version: 2.1 (May 2021)
- Re-exports: [`Saturation`](file-Saturation.md), [`Complexes`](file-Complexes.md)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Modern interface (post-2017 rewrite)

```m2
R = localRing(S, P)        -- the localisation S_P (S a polynomial ring, P a prime ideal)
liftUp(M)                  -- lift a localised matrix back to the polynomial cover
liftUp(I)                  -- ideal version
hilbertSamuelFunction(R, M, n)
                            -- the Hilbert-Samuel function of M w.r.t. the maximal ideal
presentationComplex(M)     -- canonical presentation as a complex
```

### Legacy interface (pre-2017, kept for back-compat)

```m2
setMaxIdeal P              -- declare a "current" max ideal (mutates global state)
localComplement, localsyz, localMingens, localModulo, localPrune,
localResolution            -- local counterparts that all consult the
                            -- max ideal set by setMaxIdeal
```

Plus internal symbols `residueMap`, `maxIdeal` (undocumented; used by
the legacy path).

### `LocalRing` type

The package **`exportFrom_Core { "LocalRing" }`** — the type itself
lives in Core (see [`m2/file-localring.md`](../m2/file-localring.md))
because the engine has a corresponding [`LocalRing` C++ class](../e/file-localring.md). This package provides the M2-level
operations on it.

## The lift-and-descend pattern

The package's central technique:

```
Object over R_P  →  Lift to R  →  Operate in R  →  Localise back  →  Prune
```

The header comment captures it precisely:

> "The main lemma involves lifting the objects from R_P back to R,
>  performing the operation, then localizing by tensoring with R_P
>  and pruning the resolution."

Concretely, to compute `localsyz M` (syzygies over `R_P`):

1. **`liftUp M`** — turn the matrix `M` over `R_P` into a matrix
   over `R` (the polynomial cover). Internally calls
   `rawLiftLocalMatrix` (engine entry).
2. **`syz`** of the lifted matrix in `R` — uses the standard GB
   machinery from [`groebner-bases.md`](../e/groebner-bases.md).
3. **Localise back** — tensor the result with `R_P`. Now the syzygies
   are over `R_P`.
4. **`localPrune`** — remove redundant generators by exploiting the
   fact that elements outside `P` are units in `R_P`.

Step 4 is the localisation-specific cost; the rest reuses the
polynomial-ring machinery.

## Architecture

```
LocalRings.m2 (586 lines)             — entry orchestration + exports
   │
   ├─→ LocalRings/localring.m2 (104)  — modern type / constructor wrappers
   ├─→ LocalRings/LU.m2 (117)          — LU decomposition over local rings
   ├─→ LocalRings/homotopy.m2 (159)    — homotopy / chain-complex operations
   ├─→ LocalRings/legacy.m2 (509)      — pre-2017 setMaxIdeal-style API
   ├─→ LocalRings/mike-linkage.m2 (296) — additional linkage / Cohen-Macaulay tooling
   │
   ├─→ LocalRings/doc.m2 (391)         — M2-level documentation
   ├─→ LocalRings/examples.m2 (610)    — worked examples
   └─→ LocalRings/tests.m2 (650)       — test suite
```

The split between `localring.m2` (modern, ~100 lines) and `legacy.m2`
(pre-2017, ~500 lines) reflects the 2017 rewrite — the new code
treats local rings as first-class types with `localRing(S, P)`
construction; the old code worked through a mutable global "current
max ideal" set by `setMaxIdeal`. Both still load; new code should use
the modern interface.

## The `setMaxIdeal` legacy global

The legacy pattern:

```m2
R = QQ[x, y, z]
setMaxIdeal ideal(x, y, z)
-- now localsyz, localMingens, etc. operate at this max ideal
localResolution(matrix{{x, y, z}})
```

The implicit "current max ideal" is convenient for repeated
operations but creates **non-local reasoning** — function calls
depend on hidden state. Modern code uses `R = localRing(S, P)`
explicitly:

```m2
S = QQ[x, y, z]
R = localRing(S, ideal(x, y, z))
res(matrix{{x_R, y_R, z_R}})    -- uses the local ring
```

This way the local ring is a normal M2 object (an instance of `LocalRing`); no global state.

## How local rings interact with the auto-loaded packages

| Operation in auto-loaded package | Behaviour over `R_P` |
|---|---|
| `gb I` for `I ⊂ R_P` | Engine routes through the legacy lift-syz-localize pattern; tracked at the [`LocalRing` engine class](../e/file-localring.md) |
| `saturate(I, J)` | Available via the re-exported [`Saturation`](file-Saturation.md); the localisation-aware path handles it correctly |
| `freeResolution M` | Available via the re-exported [`Complexes`](file-Complexes.md); routes through `liftUp` + global resolution + tensor-back + prune |
| `minimalPrimes I` | The auto-loaded path uses generic strategies; `LocalRings` doesn't override (yet — see TODO #5 in the header) |
| `radical I` | Same as above; uses the generic [`MinimalPrimes`](file-MinimalPrimes.md) path |
| `dim R_P` | Standard `dim` works over `LocalRing` and returns the Krull dimension at `P` |

The header's TODO #5 explicitly notes that `MinimalPrimes` / `AssociatedPrimes` should get a local-ring hook — until then, those operations use the generic path which may not localise optimally.

## When this is slow

| Symptom | Try |
|---|---|
| `localResolution M` very slow | The bottleneck is the lifted-ring resolution; pass `LengthLimit` to bound it |
| `liftUp` produces huge denominators | The lift's denominator polynomial doesn't depend on `M`; pre-compute and cache it for repeated `liftUp` calls on the same ring |
| `hilbertSamuelFunction` doesn't converge | Bound the iteration via the optional length argument; values are stable past `dim R_P + 1` |
| Legacy `setMaxIdeal` + `localsyz` slower than expected | Switch to modern `localRing(S, P)` — the legacy path has more overhead per call |

## See also

- [`file-Saturation.md`](file-Saturation.md) — re-exported by this package; local-ring inputs work with saturation
- [`file-Complexes.md`](file-Complexes.md) — re-exported; resolutions over local rings
- Engine `LocalRing` class: [`e/file-localring.md`](../e/file-localring.md)
- Core M2 `localring.m2`: [`m2/file-localring.md`](../m2/file-localring.md)
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Repo `RING-ZOO.md`](../../../RING-ZOO.md) — local rings entry in the catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
