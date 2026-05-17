# `Polynomial.{cpp,hpp}` — `Monom` / `Poly` (shared NC + new GB)

`Polynomial.hpp` declares **`Monom`** and **`Poly`** — a modern,
lightweight polynomial value type shared between the
[non-commutative algebra](NCAlgebras/README.md) code and the newer
commutative GB code in [`gb-f4/`](gb-f4/README.md). It coexists with the
legacy `gbvector` used by the older GB engines
([`file-gbring.md`](file-gbring.md)).

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## `Monom` layout

The header's comment gives the encoding:

```text
A monomial is an array of ints, the first of which is the array's length
(including the length field).

Each monomial has the form:
    <length: n+2> <degree: (currently) n> <var1> <var2> ... <varn>

Example: xyxy  →  6 4 0 1 0 1
                  ^ ^ ^ ^ ^ ^
                  | | | | | last var
                  | | | | "y"
                  | | | "x"
                  | | first var "x"
                  | total degree = 4
                  total length = 6
```

So a `Monom` is a **non-commutative word with a degree prefix**: the
first int is the array length, the second is the total degree, and the
rest are the variable indices in order.

The comment qualifies "(currently) n" — the degree happens to equal the
length of the word in the most-used cases, but the format reserves the
slot for non-trivial degree functions.

## `Poly`

A `Poly` is a parallel pair of vectors:

- A vector of **coefficients** (`ring_elem`).
- A vector of **monomials** (offsets into a shared int pool).

This is **column-store style** — easy to scan all coefficients in one
loop or all monomials in another, cache-friendly for the matrix
operations F4 needs.

## Use sites

- [`NCAlgebras/file-FreeAlgebra.md`](NCAlgebras/file-FreeAlgebra.md) and
  friends — the NC algebra layer's primary polynomial type.
- [`gb-f4/file-Basis.md`](gb-f4/file-Basis.md) and
  [`gb-f4/file-MacaulayMatrix.md`](gb-f4/file-MacaulayMatrix.md) — the
  refactored F4 stores polynomials this way.
- [`schreyer-resolution/file-res-poly-ring.md`](schreyer-resolution/file-res-poly-ring.md)
  — `ResPolynomial` is a related, resolution-specialised variant.

## Why two polynomial representations

The engine has two coexisting polynomial value types:

| Type | File | Use |
|---|---|---|
| `gbvector` | [`file-gbring.md`](file-gbring.md) | Older GB / resolution code paths |
| `Poly` | this file | NC algebra + newer F4 / Schreyer resolution |

`gbvector` is an intrusive linked list — fast for term-by-term sorted
merging in the Buchberger inner loop. `Poly` is column-store — faster for
matrix-style batch operations in F4. Each shines in its own context;
no plan to fully merge them.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-gbring.md`](file-gbring.md) — older `gbvector` representation.
- [`NCAlgebras/README.md`](NCAlgebras/README.md) — primary consumer.
- [`gb-f4/README.md`](gb-f4/README.md) — refactored F4 consumer.
- [`schreyer-resolution/file-res-poly-ring.md`](schreyer-resolution/file-res-poly-ring.md)
  — resolution-specialised variant.
