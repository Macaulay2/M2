# `monideal-minprimes.{cpp,hpp}` — `MinimalPrimes` (monomial-ideal minimal primes)

`monideal-minprimes.cpp` implements **`MinimalPrimes`** — the
state machine that computes the **minimal primes** of a
[`MonomialIdeal`](file-monideal.md). It is a specialised cousin of
[`AssociatedPrimes`](file-assprime.md), focused only on the **minimal**
primes (smallest codimension); `AssociatedPrimes` does the full set.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## Class shape

```cpp
class MinimalPrimes {
public:
    MinimalPrimes(const MonomialIdeal *const &I);

    // state machine: do_codim → do_primes
    enum { do_codim, do_primes } state;

    int            min_codim;
    int            nvars;
    MonomialIdeal *mi;       // radical of input
    MonomialIdeal *primes;   // accumulator
    int          **exps;     // scratch for exponent vectors
    // ...
};
```

The constructor in `monideal-minprimes.cpp` shows the setup:

```cpp
MinimalPrimes::MinimalPrimes(const MonomialIdeal *const &I)
    : state(do_codim),
      min_codim(I->get_ring()->n_vars() + 1),
      nvars(I->get_ring()->n_vars()),
      mi(I->radical())          // operate on the radical
{
    exps = newarray(int *, nvars + 2);
    for (int i = 0; i <= nvars + 1; i++)
        exps[i] = nullptr;
    primes = new MonomialIdeal(I->get_ring());
}
```

Key invariants:

- `mi` is set to the **radical** of the input ideal. Minimal primes
  of `I` are the same as minimal primes of `rad(I)`; working with the
  radical eliminates unnecessary higher-power monomials early.
- `min_codim` starts at `nvars + 1` (impossibly high) and is reduced
  as the algorithm finds primes.
- `state` is a two-stage flag: first compute the codimension cheaply,
  then enumerate the minimal-codimension primes.

## Why two stages

Often the user only wants the **codimension** (the minimum codim
across all minimal primes), not the primes themselves. The `do_codim`
phase computes that quickly with no allocation per prime found. If
the user then asks for the primes, the `do_primes` phase enumerates
them.

This staging matches how M2's `codim` and `minimalPrimes` functions
are typically invoked: the first call is cheap, the second only as
needed.

## Algorithm

Both stages use the standard **monomial primary decomposition** recursion:

```text
ass_min(I) = ass_min(I, x_i) ∪ { (x_i) + ass_min(I : x_i^∞) }
```

Each branch represents "is `x_i` in the prime, or not?" The recursion
bottoms out when `I` becomes the zero ideal (the prime is the chosen
support so far) or when the current prime would exceed `min_codim`
(prune; cannot be minimal).

The `exps` array is the scratch space for tracking which variables
are currently in the candidate prime.

## Used by

- M2-level `codim(I)`, `dim(R/I)` — call `do_codim`.
- M2-level `minimalPrimes(I)`, `decompose(I)` — call both stages.
- [`file-assprime.md`](file-assprime.md) — handles the general
  associated-primes case (broader output).

## Related

- [`computations.md`](computations.md) — area overview.
- [`file-monideal.md`](file-monideal.md) — `MonomialIdeal` (the input).
- [`file-assprime.md`](file-assprime.md) — `AssociatedPrimes` (the
  full version).
- [`interface/file-monomial-ideal-interface.md`](interface/file-monomial-ideal-interface.md)
  — public C wiring.
