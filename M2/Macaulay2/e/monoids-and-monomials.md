# Monoids and monomials

A **monoid** in the engine is the multiplicative structure of monomials in a
polynomial ring — variables, a monomial ordering, and optional group/grading
data. Choice of monomial representation and ordering is the single biggest
performance lever in Gröbner basis computation, so this area has multiple
encodings.

[← engine overview](README.md) · [← top-level TOC](../../../README.md#engine-deep-dive-m2macaulay2e) · [per-area docs](README.md#top-level-files-per-area-docs)

## Monoids

| File pair | Purpose |
|---|---|
| `monoid.{cpp,hpp}` | The `Monoid` class — variables, ordering, degree map, weight vectors. **Deep dive:** [`file-monoid.md`](file-monoid.md) |
| `monorder.{cpp,hpp}` | The user-facing description of monomial orderings (lex, GRevLex, weight blocks, …) |
| `imonorder.{cpp,hpp}` | Internal-monomial-order — the optimised, encoded form of a `monorder` used in hot loops. **Deep dive:** [`file-imonorder.md`](file-imonorder.md) |

The dual representation (`monorder` ↔ `imonorder`) is deliberate: the user
specifies high-level orderings, the inner loop runs on the encoded form.

## Monomial encodings

The engine uses several monomial encodings, each tuned for a different access
pattern:

| File pair / header | Encoding |
|---|---|
| `ExponentList.{cpp,hpp}` | Variable-length sparse exponent list (newer code, top-level). **Deep dive:** [`file-ExponentList.md`](file-ExponentList.md) |
| `ExponentVector.hpp` | Fixed-length dense exponent vector view (top-level) |
| [`f4/varpower-monomial.hpp`](f4/file-varpower-monomial.md) | Sparse `(variable, exponent)` pair encoding — used inside F4 |
| [`f4/ntuple-monomial.hpp`](f4/file-ntuple-monomial.md) | Dense exponent vector encoding — used inside F4 |

Newer GB engines layer additional encodings on top of these — see
[`f4/`](f4/README.md), [`gb-f4/`](gb-f4/README.md), and
[`schreyer-resolution/`](schreyer-resolution/README.md) for their
local representations.

## Monomial tables and lookups

| File pair | Purpose |
|---|---|
| `montable.{cpp,hpp}` | Generic monomial table — map monomial → polynomial / row index. **Deep dive:** [`file-montable.md`](file-montable.md) |
| `montableZZ.{cpp,hpp}` | Specialisation for the ZZ-coefficient case (where leading-coefficient signs matter for GB) |

## M2 `MonomialOrder` ↔ engine ordering

The orderings M2 users see when constructing a ring (`R = QQ[x,y,z, MonomialOrder => …]`) map to engine `MonomialOrdering` and `imonorder` representations:

| M2 spec | Engine ordering | Defined in | Notes |
|---|---|---|---|
| `Lex` | `MO_LEX` | `monordering.{c,h}` | Lexicographic; expensive but small encoding |
| `GLex` | `MO_GREVLEX` ⊕ block | composed | Graded lex via degree-then-lex block |
| `GRevLex` (default) | `MO_GREVLEX` | `monordering.{c,h}` | Graded reverse lex; the cheapest ordering and the GB default |
| `RevLex` | `MO_REVLEX` | `monordering.{c,h}` | Reverse lex; usually wrapped in a graded block |
| `Weights => {w_1, …}` | `MO_WEIGHTS` block | `monordering.{c,h}` | A leading weight block ahead of the main order |
| `Eliminate(k)` | `MO_GREVLEX` over first k + `MO_GREVLEX` over rest | composed | Elimination order used by [`Elimination` package](../packages/file-Elimination.md) |
| `GroupLex(n)` / `GroupRevLex(n)` | `MO_GROUP_LEX` / `MO_GROUP_REVLEX` | `monordering.{c,h}` | Allow negative exponents (Laurent polynomials) |
| `Position => Up` / `Down` | `MO_POSITION_UP` / `MO_POSITION_DOWN` | `monordering.{c,h}` | Module-component tiebreaker |
| Block syntax `{Lex => 2, GRevLex => 3}` | sequence of `imonorder` blocks | `imonorder.{cpp,hpp}` | Composite orderings stack their blocks |

Construction routes through [`d/monomial_ordering.dd`](../d/file-engine-interfaces.md) (interpreter binding) → [`interface/monomial-ordering.h`](interface/file-monomial-ordering-interface.md) (engine C boundary) → `monordering.c` / `imonorder.cpp` (internal).

## Choosing an encoding — quick reference

When implementing a new GB-style algorithm, the encoding choice dominates performance. Rough rules:

| Need | Pick |
|---|---|
| Few generators, low degree, dense exponents | `ExponentVector` (fixed-length array) |
| Many generators, mostly-zero exponents | `ExponentList` or `varpower-monomial` (sparse) |
| Templated over both — write code that compiles to either | `f4/MonomialView` and the templated `f4` infrastructure ([`f4/file-monhashtable.md`](f4/file-monhashtable.md), [`f4/file-moninfo.md`](f4/file-moninfo.md)) |
| Frequent comparison, infrequent arithmetic | `f4/ntuple-monomial` (dense, comparison via byte memcmp at the cost of larger storage) |
| Boolean polynomial ring `F_2[x]/(x_i^2-x_i)` | `bibasis/Monom` 64-bit packed bitmask ([`bibasis/file-monom.md`](bibasis/file-monom.md)) |
| Need a custom packed form | Subclass `monomial` and follow the `imonorder` interface |

## Overflow safety

Monomial arithmetic is overflow-prone: multiplying two monomials adds their
exponent vectors, and a single overflow can corrupt a GB silently. The engine
uses overflow-checked integer math throughout the monomial layer — see
`overflow.{cpp,hpp}` in [`utilities.md`](utilities.md). Files that need it are
listed in `e/README.md`.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — uses monoids as the monomial
  half of a polynomial ring.
- [`groebner-bases.md`](groebner-bases.md) — biggest consumer of monomial
  orderings and tables.
- [`utilities.md`](utilities.md) — `overflow.{cpp,hpp}`.
- [`interface/monoid.{h,cpp}`](interface/README.md) and
  [`interface/monomial-ordering.{h,cpp}`](interface/README.md) — public API.
