# `Tropical.m2` — tropical geometry in M2

The `Tropical` package implements **tropical geometry** as a
research-level toolkit: tropical varieties of ideals, tropical
prevarieties (intersections of tropical hypersurfaces), tropical
cycles (balanced rational polyhedral complexes with multiplicities),
Bergman fans of linear / matroid varieties, **stable intersection**
in the tropical projective torus, and `isBalanced` / `isTropicalBasis`
predicates.

The package is the **M2-level umbrella** for tropical geometry:
combinatorial and algorithmic operations are delegated to
[`gfanInterface`](file-gfanInterface.md) (for tropical-variety
computations) and [`Polyhedra`](file-Polyhedra.md) (for the
underlying polyhedral complexes), while this package provides the
**`TropicalCycle` type**, the tropical-prevariety machinery, and
the higher-level mathematical operations (balancing checks,
multiplicities, stable intersection).

- File: `Tropical.m2` (2 012 lines — single file)
- Auxiliary directory: `Tropical/` (auxiliary support, cached example outputs)
- Authors: Carlos Amendola, Kathlen Kohn, Sara Lamboglia, Diane Maclagan (maintainer), Ben Smith, Jeff Sommars, Paolo Tripoli, Magdalena Zajaczkowska
- Version: 1.0 (July 2019)
- Re-exports: [`gfanInterface`](file-gfanInterface.md), `EliminationMatrices`, `Matroids`, [`Polyhedra`](file-Polyhedra.md)
- Configuration: `tropicalMax => false` (min vs max convention), `polymakeCommand`, `path`, `fig2devpath`, `keepfiles`

[← back to packages overview](README.md) ·
[← top-level TOC](../../../README.md#packages)

## Exported API

### Types

```
TropicalCycle                       -- a balanced rational polyhedral complex with multiplicities
tropicalCycle(F, mults)             -- constructor from a Fan + multiplicity list
```

### Headline operations

```m2
tropicalVariety I                   -- the tropical variety of ideal I
tropicalPrevariety L                -- the intersection of tropical hypersurfaces of polynomials in L
isTropicalBasis(L)                  -- is L a tropical basis (i.e. does its prevariety coincide with the variety)?
stableIntersection(C, D)            -- stable intersection of two TropicalCycles
multiplicities C                    -- multiplicity at each maximal cone
isBalanced C                        -- balancing-condition check
```

### Specialised constructors

```m2
BergmanFan(M)                       -- the Bergman fan of a matroid (or linear ideal)
visualizeHypersurface f             -- render a tropical hypersurface
```

### Options

```m2
ComputeMultiplicities => true|false  -- compute multiplicities along the way?
Prime => true|false                   -- treat input as a prime ideal
Symmetry => …                         -- pass a known symmetry group to Gfan
Valuation => …                        -- the valuation of the base field (default: trivial)
IsHomogeneous => true|false           -- input known homogeneous?
tropicalMax => true|false             -- max vs min convention (default min)
```

## What tropical geometry is

The **tropical variety** of a polynomial ideal `I ⊂ k[x_1, …, x_n]`
(with `k` a valued field) is the closure in `ℝ^n` of:

```
trop(I) = { w ∈ ℝ^n : in_w(I) contains no monomial }
```

It's a finite rational polyhedral fan of pure dimension equal to
`dim V(I)` (for irreducible `I`). Tropical varieties are the
combinatorial shadows of algebraic varieties and encode combinatorial
information (e.g. Newton polytopes, lattice-point counts, generic
geometric properties of the variety).

`tropicalVariety I` returns this as a `TropicalCycle`:
- The underlying fan (in `Polyhedra.Fan` format).
- A list of multiplicities (one per maximal cone), encoding
  scheme-theoretic information.

## Min vs max convention

Tropical geometry has two conventions:

| Convention | Operations | M2 setting |
|---|---|---|
| **Min-convention** (the default) | `a ⊕ b = min(a, b)`, `a ⊗ b = a + b` | `Configuration => { "tropicalMax" => false }` |
| **Max-convention** | `a ⊕ b = max(a, b)`, `a ⊗ b = a + b` | `Configuration => { "tropicalMax" => true }` |

The two are related by negation but **subtle sign flips** appear
in tropical balancing conditions and stable-intersection formulas.
Set the configuration once at package load; mixing conventions
within a session is not supported.

## The tropical-prevariety vs tropical-variety distinction

A **tropical prevariety** is `trop(f_1) ∩ trop(f_2) ∩ … ∩ trop(f_k)`
— the intersection of the tropical hypersurfaces of a generating
set. The **tropical variety** is the smaller object cut out by
`trop(I)` for `I = (f_1, …, f_k)`.

A generating set `L = {f_1, …, f_k}` is a **tropical basis** iff
`trop(L) = trop(I)`. Tropical bases are hard to find — `isTropicalBasis L`
checks the equality but doesn't compute one when L isn't.

Computationally:
- `tropicalPrevariety L` is **fast** (uses Gfan's hypersurface intersection).
- `tropicalVariety I` is **slow** (requires the full Gfan tropical-variety algorithm).

Use `tropicalPrevariety` when you have a known tropical basis and want speed; use `tropicalVariety` when you need the correct answer.

## Stable intersection

The **stable intersection** of two tropical cycles `C` and `D`:

```m2
stableIntersection(C, D)
```

computes the cycle whose support is `|C| ∩ |D|` with multiplicities determined by the **Hadamard formula**:

```
(C ∩ D)_σ = Σ_{τ, ρ : τ ∩ ρ = σ}  m_C(τ) · m_D(ρ) · [Λ_τ + Λ_ρ : Λ_σ]
```

(where `Λ_τ` is the lattice spanned by `τ`'s direction). This is the **tropical analogue** of the intersection product of algebraic cycles. The implementation traverses the cones of `C` and `D` and computes the lattice indices.

## Bergman fans of matroids

For a matroid `M` (from the imported `Matroids` package), `BergmanFan M` returns its **Bergman fan** — a polyhedral fan that:

- Recovers the tropicalisation of any **linear ideal** when `M` is the matroid of the column-set of a generic linear form.
- For non-realisable matroids, gives a tropical object with no algebraic counterpart.

Bergman fans are central in **matroid theory + tropical geometry intersections**, including Adiprasito-Huh-Katz's resolution of the Heron-Rota-Welsh conjecture.

## Architecture

```
Tropical.m2 (2 012 lines)                     ← single-file package
   ↓ delegates heavy lifting to
gfanInterface (re-exported)                    ← tropical-variety, tropical-basis, tropical-traverse
Polyhedra (re-exported)                        ← polyhedral-complex types
EliminationMatrices (re-exported)              ← elimination-style operations
Matroids (re-exported)                         ← matroid type for BergmanFan
   ↓ produces
TropicalCycle objects (this package's own type)
```

This is the **M2-side coordinator**: it doesn't reimplement Gfan's algorithms, but it provides:
1. A unified type (`TropicalCycle`) over Gfan's output + Polyhedra's polyhedral complexes.
2. Higher-level operations (`stableIntersection`, `isBalanced`, `BergmanFan`).
3. Mathematical correctness checks (`isTropicalBasis`).
4. The `tropicalMax` convention switch.

## When this is slow

| Symptom | Try |
|---|---|
| `tropicalVariety I` slow | Gfan's tropical-variety algorithm is fundamentally expensive; first compute a tropical basis if known, then `tropicalPrevariety L` instead |
| `isTropicalBasis L` slow | Internally computes both `tropicalPrevariety L` and `tropicalVariety I`; if you already have one, pass it via options |
| `stableIntersection(C, D)` slow on high-dim cycles | The lattice-index computations grow; consider sampling specific cones if you don't need the full intersection |
| `BergmanFan M` slow on a large matroid | The Bergman fan grows with the matroid's flats; check `M.rank` first as a sanity check |

## See also

- [`file-gfanInterface.md`](file-gfanInterface.md) — re-exported; provides the underlying tropical algorithms
- [`file-Polyhedra.md`](file-Polyhedra.md) — re-exported; provides `Fan` / `Cone` / `PolyhedralComplex`
- [`file-package-conventions.md`](file-package-conventions.md) — package conventions
- `EliminationMatrices` (re-exported) — elimination-style operations
- `Matroids` (re-exported) — provides the matroid type for `BergmanFan`
- [`file-FourTiTwo.md`](file-FourTiTwo.md), [`file-Normaliz.md`](file-Normaliz.md) — sibling polyhedral packages
- [Repo `PACKAGES.md`](../../../PACKAGES.md) — package ecosystem reference
- [Repo `COMPUTATIONS.md`](../../../COMPUTATIONS.md) — tropical / polyhedral catalogue
- Maclagan-Sturmfels, *Introduction to Tropical Geometry*, AMS GSM 161 — the canonical reference
