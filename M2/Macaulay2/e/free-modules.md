# Free modules

A **free module** `R^n` over a ring `R` is the simplest non-trivial module.
Most of the engine's machinery — matrices, Gröbner bases, resolutions —
ultimately works on free modules: a matrix is a homomorphism `F → G` between
two free modules, a GB lives in a free module, a resolution is a complex of
free modules.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Files

| File pair | Purpose |
|---|---|
| `freemod.{cpp,hpp}` | The `FreeModule` class — base ring, rank, degree vector per generator. **Deep dive:** [`file-freemod.md`](file-freemod.md) |
| `schorder.{cpp,hpp}` | Schreyer orderings on free modules — used to make leading-term computations local to a homological degree in resolutions. **Deep dive:** [`file-schorder.md`](file-schorder.md) |

## Anatomy of a `FreeModule`

A `FreeModule` carries:

- a pointer to its **base ring**
- the **rank** (number of generators)
- a **degree vector** for each generator (in the ring's degree monoid)
- optionally a **Schreyer order** (when used as the target of a syzygy module
  in a resolution)

Matrices ([`matrices.md`](matrices.md)) carry pointers to source and target
free modules; their entries respect the degrees.

## Schreyer orders

A Schreyer order on `F = R^r` is induced from a list of *leading monomials*
`(m_1, …, m_r)` (one per generator) plus the ring's ambient monomial order.
The order on `F` is:

```
e_i m  <  e_j m'   iff   m_i m  <  m_j m'   (ambient order)
                          or equal-and-then i > j (tiebreak)
```

This makes the leading term of a polynomial in `F` factor cleanly across
homological degrees, which is the trick that makes Schreyer-style resolutions
efficient. See [`schreyer-resolution/`](schreyer-resolution/README.md) for the
F4-style implementation.

## M2 operation → engine entry

Common operations on `FreeModule` and how they bottom out in the engine:

| M2 operation | Engine entry | Source file | Notes |
|---|---|---|---|
| `R^n` (rank-`n` free module) | `Ring::makeFreeModule(n)` → `new FreeModule(R, n, …)` | `freemod.{cpp,hpp}` | The simplest constructor; generators all in degree 0 |
| `R^{-d_1, -d_2, …}` (twisted free module) | `new FreeModule` with `degree_monomial[]` populated from negated degrees | `freemod.{cpp,hpp}` | Negation because M2 convention is `R^{-d}` ↔ generator in degree `d` |
| `F ++ G` (direct sum) | `FreeModule::direct_sum(F, G)` | `freemod.{cpp,hpp}` | Concatenates degree lists |
| `F ** G` (tensor product) | `FreeModule::tensor(F, G)` | `freemod.{cpp,hpp}` | Cartesian product of degree lists (sums them) |
| `dual F` (dual module) | `FreeModule::dual(F)` | `freemod.{cpp,hpp}` | Negates every degree |
| `degrees F` | `FreeModule::get_degrees()` | `freemod.{cpp,hpp}` | Returns the list of generator degrees |
| `rank F` | `FreeModule::rank()` | `freemod.{cpp,hpp}` | Just the count |
| Schreyer-ordered free module (in a resolution) | `FreeModule::set_schreyer_order(...)` | `schorder.{cpp,hpp}` | Used during `freeResolution` construction; not directly exposed to users |
| Matrix construction `matrix {{a,b},{c,d}}` | Auto-builds source and target `FreeModule`s | `matrix-con.{cpp,hpp}` | Inferred degrees and ranks from the entries |

Construction routes through:

```
M2: F = R^{-1, -2}
   ↓
m2/modules.m2  →  rawFreeModule(R, {-1, -2})
   ↓
d/interface.dd  →  Ccode(RawFreeModule, "IM2_FreeModule_make(R, ...)")
   ↓
e/interface/freemodule.h  →  IM2_FreeModule_make(R, degrees)
   ↓
e/freemod.cpp  →  new FreeModule(R, degrees)
   ↓
returned as FreeModule* to interpreter (held as `RawFreeModuleCell` in d/)
```

## When to use what

| Want | Pick |
|---|---|
| Plain rank-n free module | `R^n` — uses degree-0 generators |
| Free module with non-zero generator degrees | `R^{-d_1, …}` — note the minus convention |
| Source / target of a matrix | Constructed implicitly by `matrix {{…}}`; query via `source f` / `target f` |
| Schreyer-ordered free module (advanced) | Not constructed directly; let `freeResolution` produce it; query via `F.cache.?schreyer` |
| Direct sum / tensor / dual | Use `++`, `**`, `dual` at the M2 level — never construct by hand at the engine level |
| Quotient of a free module | `R^n / I` — produces a `Module` (subquotient), not a `FreeModule` |

A `FreeModule` is **lighter** than a general `Module`: it stores just `(ring, rank, generator-degrees, optional Schreyer order)`. A `Module` adds presentation data (generators + relations). When in doubt:

| Question | Answer |
|---|---|
| Does the object only need rank + degrees? | `FreeModule` (cheaper, in this file) |
| Does the object need generators and relations? | `Module` — see [`matrices.md`](matrices.md) and the `subquotient` constructor |

## Related

- [`matrices.md`](matrices.md) — matrices between free modules.
- [`schreyer-resolution/`](schreyer-resolution/README.md) — primary consumer
  of Schreyer orders.
- [`interface/freemodule.{h,cpp}`](interface/README.md) — public API.
