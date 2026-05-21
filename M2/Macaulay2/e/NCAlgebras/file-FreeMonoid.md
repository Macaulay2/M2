# `FreeMonoid.{cpp,hpp}` — `FreeMonoid`

`FreeMonoid` is the **monoid of words** in a finite alphabet — the
multiplicative side of a free (non-commutative) algebra. Unlike the
commutative engine's `Monoid` ([`../file-monoid.md`](../file-monoid.md)),
the order in which generators appear matters here.

Part of the [`NCAlgebras/`](README.md) subdirectory.

[← NCAlgebras overview](README.md) · [← engine overview](../README.md)

## Encoding

A non-commutative monomial is a sequence of variable indices `[v_1, v_2, …,
v_k]` — i.e. a **word**. The encoding stores:

- Total length (number of variables in the word).
- Optional **weight** prefix (one int per weight function).
- The word itself, variable indices in order.

The header comment lays out the format:

```
[total length] wt0 wt1 ... w(tr-1) w0 w1 ... ws
```

with `r` weights followed by `s+1` variable indices. The leading length lets
the engine `memcpy` a word in one shot without walking it.

## Multiplication

Multiplication of two words is **concatenation**:

```
[a, b, c] * [d, e]  =  [a, b, c, d, e]
```

No re-sorting, no normalisation — the word is what it is. The non-commutative
GB algorithms in [`NCGroebner`](file-NCGroebner.md) /
[`NCF4`](file-NCF4.md) handle non-uniqueness by ordering on words.

## Word ordering

The header lists TODOs about weight handling:

- input correctness from front end,
- ensuring monomials get weight-function values stored,
- ensuring compare uses that info.

The intended ordering is **weight-first then lex**: compare total weight,
then per-weight components, then break ties lexicographically. This matches
how non-commutative GB algorithms in the literature define "leading word".

## `FreeMonoidLogger`

A debug-time helper used to trace word-arithmetic operations during the
development of the new NC algorithms. Normally compiled out.

## Related

- [`README.md`](README.md) — NCAlgebras overview.
- [`file-FreeAlgebra.md`](file-FreeAlgebra.md) — the ring built on this monoid.
- [`Word.cpp`](README.md), [`WordTable.cpp`](README.md) — adjacent helpers
  for word storage and search.
- [`../file-monoid.md`](../file-monoid.md) — commutative analogue.
