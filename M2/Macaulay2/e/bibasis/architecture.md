# Boolean involutive-basis (BIBasis) architecture

This document is the **architectural reference** for
`M2/Macaulay2/e/bibasis/` — a specialised Gröbner-basis engine
for the **Boolean polynomial ring** `F_2[x_1, …, x_n] / (x_i^2 -
x_i)`. Authored by Mikhail V. Zinin (2006-2011), used by M2's
`BIBasis` user package.

[← bibasis/ overview](README.md) · [← engine architecture](../architecture.md)

## What's special about Boolean polynomial rings

In `F_2[x_1, …, x_n] / (x_i^2 - x_i)`:

- **Coefficients are 0 or 1** — addition is XOR, multiplication
  is AND.
- **`x_i^2 = x_i`** — exponents are 0 or 1; monomials are subsets
  of variables.
- **A monomial in 64 variables packs into a single `unsigned
  long`** — one bit per variable.

This collapses an entire layer of generic GB infrastructure. A
specialised engine can outperform generic F4 by orders of
magnitude on Boolean inputs.

## What "involutive basis" means

A **Janet involutive basis** is a stronger structure than a
Gröbner basis. Every Gröbner basis is an involutive basis but
not vice versa.

The trick: instead of "leading monomial divides," involutive
division asks "leading monomial divides *while respecting the
multiplicative variables*." Each polynomial in the basis carries
a **partition of variables** into *multiplicative* (allowed to
multiply by) and *non-multiplicative* (forbidden).

The benefit: **a unique reduction at every step** — no S-pair
processing needed. The basis grows by:

1. Pick the first non-reduced triple from the Q-set.
2. Reduce it modulo the T-set (current basis).
3. If non-zero, insert into T-set and add prolongations to Q-set.

Repeat until Q is empty.

## Three-layer architecture

```
┌──────────────────────────────────────────────────────┐
│   Driver / engine boundary                            │
│   BIBasis (engine-facing class)                       │
│   launcher.cpp (order dispatch)                       │
├──────────────────────────────────────────────────────┤
│   Algorithm                                           │
│   Involutive<MonomType> (templated algorithm)         │
│   TSet (terminal set — current basis)                 │
│   QSet (queue set — pending triples)                  │
│   JanetTree (involutive-division index)               │
│   PComparator (polynomial comparator)                 │
├──────────────────────────────────────────────────────┤
│   Primitives                                          │
│   Monom base + MonomLex/MonomDL/MonomDRL              │
│   Polynom<MonomType>                                  │
│   Triple (polynomial + var partition)                 │
│   FastAllocator (slab pool)                           │
│   SettingsManager (parameters)                        │
└──────────────────────────────────────────────────────┘
```

## Templated dispatch on monomial order

The algorithm is **templated on the monomial type**. Three
concrete instantiations:

```
Involutive<MonomLex>
Involutive<MonomDL>     // Degree Lex
Involutive<MonomDRL>    // Degree Reverse Lex
```

[`launcher.cpp`](file-launcher.md) dispatches to the right one
based on user choice. Templated dispatch (vs virtual) means the
comparison operator inlines into the reduction loop — critical
for performance.

## The 64-bit packed monomial

```cpp
typedef unsigned long brMonomial;
```

The genius simplification: a Boolean monomial in ≤64 variables is
just a **64-bit bitmask**. Operations:

- Multiplication = bitwise OR.
- Divisibility (`a | b`) = `(a & b) == a`.
- GCD = bitwise AND.
- LCM = bitwise OR.
- Degree = popcount.

All `O(1)`. The base `Monom` class abstracts over this so
algorithms can work with > 64 variables by using arrays of
`brMonomial`s.

## The Janet tree

[`file-janettree.md`](file-janettree.md) implements the **central
data structure** of involutive bases: a tree that indexes the
T-set by leading monomial in a way that makes involutive-division
lookups `O(log n)` per query.

Each node represents a monomial; children correspond to
*multiplications by individual variables*. Looking up "does any
basis element involutively divide this monomial?" becomes
tree traversal.

## Memory model

BIBasis stresses allocation hard — billions of small objects.
Three approaches:

1. **`FastAllocator`** ([`file-allocator.md`](file-allocator.md))
   — custom slab allocator. Every `Triple`, `Polynom`, monomial
   node overloads `operator new` / `operator delete` to route
   through it. Slabs survive the whole computation; objects are
   never individually freed.
2. **Boehm GC** — for results crossing the engine boundary.
3. **Stack allocation** — for transient locals.

The trade-off is "no `free`" (slabs grow monotonically) — fine
because BIBasis runs are finite and the OS reclaims at process
exit.

## When BIBasis wins

For inputs that are:

- **Boolean polynomial systems** (e.g., from cryptanalysis,
  SAT-style problems, biological-network modelling).
- **High variable count** (the bit-packing wins big).
- **Many input generators with shared support** (the
  involutive-division uniqueness pays off).

For inputs that are non-Boolean or have a small number of
variables, generic F4 is usually faster.

## Specialisation rationale

Why a dedicated subdir instead of folding into `f4/`?

1. **The math is different.** Involutive bases vs Gröbner bases —
   different algorithm, not just a special case.
2. **The data structures are different.** Janet tree, T-set / Q-set
   don't appear in F4.
3. **Coefficient is fixed.** F4 is templated on coefficient; this
   is fixed to `F_2`.

## Used by

- The `BIBasis` user package.
- Cryptanalysis research workflows.
- Algebraic Boolean SAT solvers.

## File-by-file

| Component | File doc |
|---|---|
| Engine-facing driver | [`file-bibasis.md`](file-bibasis.md) |
| Order dispatch | [`file-launcher.md`](file-launcher.md) |
| Templated algorithm | [`file-involutive.md`](file-involutive.md) |
| `Monom` base class | [`file-monom.md`](file-monom.md) |
| Order specialisations | [`file-monom-orders.md`](file-monom-orders.md) |
| Polynomial type | [`file-polynom.md`](file-polynom.md) |
| Janet tree | [`file-janettree.md`](file-janettree.md) |
| Slab allocator | [`file-allocator.md`](file-allocator.md) |
| Internal structures | [`file-bibasis-internals.md`](file-bibasis-internals.md) |

## Related

- [`README.md`](README.md) — bibasis/ navigation hub.
- [`../architecture.md`](../architecture.md) — engine architecture.
- [`../f4/architecture.md`](../f4/architecture.md) — generic F4
  GB engine.
- [`../groebner-bases.md`](../groebner-bases.md) — top-level GB
  area.
- [`../file-franzi-brp.md`](../file-franzi-brp.md) — sister
  Boolean-ring GB engine by Franziska Hinkelmann (different
  approach).
