# `M2/Macaulay2/e/bibasis/` — involutive (Janet) bases for Boolean rings

A specialised Gröbner-basis engine that computes **involutive Janet bases**
over the Boolean polynomial ring `F_2[x_1,…,x_n]/(x_i^2 - x_i)`. Used by the
[`BIBasis`](../../packages/BIBasis.m2) package.

Janet bases are a richer alternative to Gröbner bases that come with a unique
involutive division structure, making certain calculations (e.g. reductions in
specific cones) faster than with a generic GB.

The original short notes are in plain text in [`README`](README).

## Files

### Driver

| File | Role |
|---|---|
| `bibasis.{cpp,hpp}` | Main algorithm entry point |
| `launcher.{cpp,hpp}` | Wrapper that exposes the algorithm to the engine |
| `settings-manager.{cpp,hpp}` | Algorithm configuration |
| `allocator.{cpp,hpp}` | Memory pool used by the inner loop |

### Monomial representations (three alternative orderings)

| File | Role |
|---|---|
| `monom.{cpp,hpp}` | Generic monomial interface |
| `monomLex.{cpp,hpp}` | Lex-ordered monomial |
| `monomDL.{cpp,hpp}` | Degree-lex monomial |
| `monomDRL.{cpp,hpp}` | Degree-reverse-lex monomial |

### Data structures

| File | Role |
|---|---|
| `polynom.hpp` | Polynomial type |
| `janettree.hpp`, `tset.hpp`, `qset.hpp` | Janet tree and supporting sets |
| `triple.hpp` | Triple `(α, m, p)` representing an involutive division step |
| `pcomparator.hpp` | Polynomial comparator |
| `involutive.hpp` | Involutive-basis algorithm bits factored out for reuse |

## Related

- [`BIBasis`](../../packages/BIBasis.m2) — the user-facing package.
- General-purpose GB engines: [`../f4/`](../f4/README.md),
  [`../gb-f4/`](../gb-f4/README.md), and mathicgb (submodule).

[← back to engine overview](../README.md)
