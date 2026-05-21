# `Normaliz.m2` — interface to the Normaliz toric / affine-monoid library

The `Normaliz` package is M2's **interface to the external Normaliz
library** (Bruns-Söger, etc.) — a high-performance implementation of
**affine-monoid algorithms**: integral closures, Hilbert bases of
cones, Ehrhart polynomials, integral closures of monomial ideals,
class groups of monoid rings, and various invariants from
combinatorial commutative algebra and lattice geometry.

Sibling to [`FourTiTwo`](file-FourTiTwo.md) (both target overlapping
toric/lattice problems with different algorithmic strengths) and to
[`Bertini`](file-Bertini.md) / [`PHCpack`](file-PHCpack.md) (the
numerical-solver wrappers — same architectural pattern, different
mathematical domain).

JSAG-certified (vol. 2, 2010).

- File: `Normaliz.m2` (2 042 lines — single file)
- Authors: Gesa Kaempf, Christof Soeger (and original Bruns-Söger work)
- Version: 2.6 (February 2023)
- **Requires Normaliz installed externally** (binary on `PATH`)

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Headline operations

```m2
normalToricRing M               -- the normalisation of the toric ring k[t^m_i for m_i a column of M]
intclToricRing M                -- the integral closure
intclMonIdeal I                 -- the integral closure of a monomial ideal
ehrhartRing M                   -- the Ehrhart ring (lattice points in dilations of the polytope)
torusInvariants R               -- ring of torus invariants
finiteDiagInvariants(R, …)      -- finite diagonal invariants
diagInvariants(R, …)            -- general diagonal invariants
```

### Lower-level access

```m2
normaliz inputs                 -- run Normaliz directly with a specified input
setNmzOption("opt", true)       -- set a Normaliz option flag (computation mode, …)
showNmzOptions                  -- list the current options
writeNmzData(filename, M)       -- write data to a Normaliz input file
readNmzData filename            -- read data from a Normaliz output file
readMultipleNmzData             -- read all output files in one call
rmNmzFiles                      -- clean up temp files
allComputations                 -- list of all computation modes
grading                         -- access / set the grading data
```

### Specialised types

```m2
MonomialSubalgebra              -- the type
createMonomialSubalgebra(R, gens)
RationalCone                    -- the rational-cone type
```

### Auxiliary

```m2
intersectionValRings R, intersectionValRingIdeals
                                -- valuation-ring intersections
getNumInvs C                    -- access numerical invariants of a computation
nmzVersion, nmzDataPath,
nmzFilename, nmzNumberThreads   -- exported mutable variables for configuration
```

## What Normaliz does

Normaliz is a **specialised tool for affine monoids and rational cones** — the natural objects in combinatorial commutative algebra and lattice geometry. Given a cone `C ⊂ ℝ^n` or an affine monoid `M ⊂ ℤ^n`, Normaliz computes:

| Object | Method (M2-side) | Computation |
|---|---|---|
| Hilbert basis of `C ∩ ℤ^n` | (via `Polyhedra` → `Strategy => "Normaliz"`) | The unique minimal generating set of the integer points in the cone |
| Integral closure of `M` (≡ Hilbert basis of the cone) | `normalToricRing M`, `intclToricRing M` | `normalToricRing` returns the closure; `intclToricRing` returns it as an `Ideal` |
| Integral closure of a monomial ideal | `intclMonIdeal I` | The radical-like operation specialised to monomial inputs |
| Ehrhart polynomial / Ehrhart ring | `ehrhartRing M` | Counts lattice points in `kP` for each integer `k ≥ 0` |
| Class group of a normal toric ring | (via `getNumInvs`) | Number-theoretic invariant from divisor theory |
| Torus / diagonal invariants | `torusInvariants`, `finiteDiagInvariants`, `diagInvariants` | Rings of invariants under a torus or finite-diagonal-group action |
| Volumes of polytopes | (via `getNumInvs`) | Normalised volume |

These are **harder than they look**: Hilbert-basis computation, for instance, is the main bottleneck of many polyhedral algorithms. Normaliz uses sophisticated cone-decomposition + lattice-traversal techniques to be among the fastest tools available.

## How it differs from `FourTiTwo`

Both packages target similar problems (toric ideals, Hilbert bases, monoid algorithms) with different algorithmic specialties:

| Operation | `FourTiTwo` strength | `Normaliz` strength |
|---|---|---|
| Markov / Graver / circuits of toric ideals | Headline | Not exposed |
| Toric Gröbner basis | Yes | Not directly |
| Hilbert basis of a cone | OK; depends on cone shape | **Fastest** for most shapes — Normaliz's headline operation |
| Integral closure of a toric ring | Indirect | **Direct API** (`normalToricRing`, `intclToricRing`) |
| Ehrhart polynomial / volume | No | **Direct API** (`ehrhartRing`) |
| Class group | No | **Direct via** `getNumInvs` |
| Torus / diagonal invariants | No | **Direct API** |

When in doubt, **try Normaliz first** for cone-and-monoid problems; try **FourTiTwo first** for toric-ideal generating-set problems.

## Computation modes — the `setNmzOption` mechanism

Normaliz internally supports many computation modes (Hilbert basis only, integral closure only, Ehrhart only, etc.) — each cheaper than the full general algorithm. The M2 side exposes this through:

```m2
setNmzOption("computationName", true)
showNmzOptions                  -- inspect current settings
```

Common modes:

| Option flag | Effect |
|---|---|
| `"normal"` | Compute the full normalisation (default) |
| `"hilbertBasis"` | Only the Hilbert basis |
| `"ehrhart"` | Only the Ehrhart polynomial / ring |
| `"supp"` | Support hyperplanes only |
| `"triang"` | Triangulation data |
| `"classGroup"` | The class group |
| `"isIntegrallyClosed"` | A yes/no test |

For complex inputs, selecting a narrower mode can dramatically speed up the run.

## When the interface runs

```
M2:  normalToricRing M
   ↓
write M to a temp .in file in Normaliz format
   ↓
shell out to `normaliz` binary
   ↓ Normaliz writes .out, .gen, .typ, .inv files
parse those files (via readNmzData / readMultipleNmzData)
   ↓ return as M2 Matrix, Ideal, or specialised type
```

The `nmzDataPath` exported mutable variable controls where temp files go. `rmNmzFiles` cleans them up after a run.

## Multi-threading

Normaliz supports OpenMP-based multi-threading. The package exposes this via:

```m2
nmzNumberThreads = 8     -- use 8 threads
```

For large cones, this can give near-linear speedup. The Normaliz binary must be compiled with OpenMP support (most distribution-provided binaries are).

## Heavy downstream consumers

| Package | What it uses from Normaliz |
|---|---|
| [`Polyhedra`](file-Polyhedra.md) | `hilbertBasis` via `Strategy => "Normaliz"` — the fastest backend for that operation |
| [`NormalToricVarieties`](file-NormalToricVarieties.md) | Imported (auto-loaded) for normalisation / Hilbert-basis computations |
| `MonomialAlgebras` | Integral closure of monomial ideals |
| Various combinatorial-commutative-algebra add-ons | Class group, Ehrhart polynomial |

## When this is slow

| Symptom | Try |
|---|---|
| `normalToricRing M` slow for high-dim cones | Set `nmzNumberThreads = N` for OpenMP parallelisation |
| `ehrhartRing M` slow on a high-dim polytope | Use `setNmzOption("ehrhart", true)` to enable Ehrhart-specific code paths |
| `intclMonIdeal I` slow | The monomial-ideal integral closure problem is hard; consider whether a partial answer suffices |
| Normaliz not found at load | Set `nmzDataPath` and ensure `normaliz` is on `PATH` |
| Output files left behind | `rmNmzFiles` deletes them; or set `nmzDataPath` to a temp dir that's auto-cleaned |

## See also

- [`file-FourTiTwo.md`](file-FourTiTwo.md) — sibling toric/lattice external-library wrapper
- [`file-Bertini.md`](file-Bertini.md), [`file-PHCpack.md`](file-PHCpack.md) — sibling external-library wrappers (numerical AG)
- [`file-Polyhedra.md`](file-Polyhedra.md) — uses Normaliz as a backend for `hilbertBasis`
- [`file-NormalToricVarieties.md`](file-NormalToricVarieties.md) — auto-loaded import; uses Normaliz internally
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- [Normaliz](https://www.normaliz.uni-osnabrueck.de/) — the upstream library
- [JSAG 2010 article](https://msp.org/jsag/2010/2-1/p04.xhtml) — Kaempf-Soeger: *A Macaulay2 interface for Normaliz*
- [Repo `DEPENDENCIES.md`](../../../DEPENDENCIES.md) — external-library catalogue
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — affine-monoid / polyhedral catalogue
