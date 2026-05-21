# Refactored F4 GB engine architecture

This document is the **architectural reference** for the
refactored F4 Gröbner-basis engine in `M2/Macaulay2/e/gb-f4/`.
Newer than the [`f4/`](../f4/architecture.md) variant; cleaner
type separation; uses the `newf4::` namespace internally.

[← gb-f4/ overview](README.md) · [← engine architecture](../architecture.md)

## What "refactored" means

The original [`f4/`](../f4/architecture.md) was written ~2012
when most of M2's engine was still adopting C++11. The newer
`gb-f4/` (started ~2020) reflects a decade of design lessons:

| Original `f4/` | Refactored `gb-f4/` |
|---|---|
| Heavy template specialisation on monomial type | Typed-integer indices (`MonomialIndex`, `HashInt`, `Index`) |
| Polynomial = linked list of terms | Polynomial = struct-of-arrays with explicit views |
| Monomial encoding mixed into algorithm | Clean `MonomialView` abstraction |
| Hash table tightly coupled to F4 step | Reusable `MonomialHashTable` + `MonomialLookupTable` |
| Algorithm split across many files | Algorithm consolidated in `GBF4Computation` |
| GC-managed throughout | More aggressive pool allocation |

Both engines coexist. `f4/` is the current default for `Z/p` GB
because it's been hardened over many years. `gb-f4/` is opt-in
via `Strategy =>` and is gradually picking up workloads as it
matures.

## The newf4 namespace

```cpp
namespace newf4 {
  class GBF4Computation;
  class Basis;
  class SPairs;
  class MacaulayMatrix;
  class MonomialHashTable;
  ...
}
```

All internal types live under `newf4::` to avoid collision with the
legacy `f4/` engine. The boundary class `GBF4Interface` (see
[`file-GBF4Interface.md`](file-GBF4Interface.md)) crosses the
namespace boundary to expose `GBComputation*` to the engine.

## Typed-integer family

```cpp
using Index         = uint32_t;   // index into Basis / SPairs
using MonomialIndex = uint32_t;   // index into MonomialHashTable
using HashInt       = uint64_t;   // hash of a monomial
```

(See [`file-MonomialTypes.md`](file-MonomialTypes.md).)

The original `f4/` used raw `int` everywhere, which led to
**confusion bugs**: "is this row-index or column-index? oh, that
parameter was the monomial id." The typed-integer family makes
those bugs into compile errors.

## Three-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   Engine boundary                                     │
│   GBF4Interface (wraps as GBComputation*)             │
├──────────────────────────────────────────────────────┤
│   Algorithm orchestration                             │
│   GBF4Computation (main loop)                         │
│   Basis (evolving GB)                                 │
│   SPairs (S-pair queue + selection)                   │
│   MacaulayMatrix (per-step matrix)                    │
├──────────────────────────────────────────────────────┤
│   Polynomial / monomial primitives                    │
│   PolynomialList (struct-of-arrays polynomials)       │
│   MonomialHashTable (intern monomials)                │
│   MonomialLookupTable (divisibility index)            │
│   MonomialView (non-owning monomial reference)        │
│   MonomialTypes (typed-integer aliases)               │
└──────────────────────────────────────────────────────┘
```

## The pipeline

```
input matrix
   │
   ▼ GBF4Interface
GBF4Computation built; matrix converted to PolynomialList
   │
   ▼ start_computation()
main loop in GBF4Computation:
   ┌──────────────────────────────────────────────┐
   │ while spairs not empty:                       │
   │   select_minimal_degree_spairs()              │
   │     ↓  SPairs                                 │
   │   build_macaulay_matrix_at_degree(d)          │
   │     ↓  MacaulayMatrix, MonomialHashTable      │
   │   row_reduce()                                │
   │     ↓  linear algebra over Z/p                │
   │   extract_new_basis_elements()                │
   │     ↓                                         │
   │   update_basis_and_spairs()                   │
   │     ↓  Basis, SPairs                          │
   └──────────────────────────────────────────────┘
   │
   ▼ result extracted via GBF4Interface
```

Same structural shape as [`f4/architecture.md`](../f4/architecture.md);
the difference is the *clean type separation* between the phases.

## Polynomial representation

Original `f4/` polynomials were linked lists:

```
P = term₁ → term₂ → term₃ → ...
```

Refactored `gb-f4/` uses **struct-of-arrays**:

```cpp
struct PolynomialList {
  std::vector<MonomialIndex> monomials;   // all monomials in all polynomials
  std::vector<Coefficient>   coefficients; // matching coefficients
  std::vector<Index>         poly_offsets; // where each polynomial starts
};
```

Why: cache-friendly. The reduction loop touches monomial indices
sequentially; struct-of-arrays keeps them packed.
[`file-PolynomialList.md`](file-PolynomialList.md).

## Monomial hashing

Every monomial that appears anywhere in the computation gets a
unique **`MonomialIndex`** via [`file-MonomialHashTable.md`](file-MonomialHashTable.md).
Two benefits:

- **Comparisons are integer comparisons** — no monomial-shape
  arithmetic in the inner loop.
- **Memory** — each monomial stored once, even when it appears in
  many polynomials.

The hash function is in [`file-MonomialHashTable.md`](file-MonomialHashTable.md);
divisibility lookup is in [`file-MonomialLookupTable.md`](file-MonomialLookupTable.md).

## The Macaulay matrix

[`file-MacaulayMatrix.md`](file-MacaulayMatrix.md) builds the
matrix sweep:

- Rows: S-polynomials at the current degree, plus reducers.
- Columns: every monomial that appears in any row.
- Cells: coefficients (mostly zero).

Stored sparsely (just nonzero `(row, col, coeff)` triples).
Row-reduction uses FFPACK / FLINT for finite-field arithmetic.

## Comparison with `f4/`

| Aspect | `f4/` | `gb-f4/` |
|---|---|---|
| First written | ~2012 | ~2020 |
| Default for Z/p gb | yes (currently) | no (opt-in) |
| Polynomial repr | linked lists | struct-of-arrays |
| Monomial repr | template-specialised | hash-table-interned |
| Typed integers | raw int | `Index` / `MonomialIndex` / `HashInt` |
| Production-tested | yes | maturing |

Both will be maintained for the foreseeable future. The plan is
for `gb-f4/` to eventually become the default.

## Used by

- M2's `gb` when strategy `NewF4` is selected.
- Some benchmarks comparing against external GB engines (msolve,
  OSCAR).

## Related

- [`README.md`](README.md) — gb-f4/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine architecture.
- [`../f4/architecture.md`](../f4/architecture.md) — sister
  original-F4 engine.
- [`../groebner-bases.md`](../groebner-bases.md) — top-level GB
  area.
- [`file-GBF4Interface.md`](file-GBF4Interface.md) — engine
  boundary entry.
