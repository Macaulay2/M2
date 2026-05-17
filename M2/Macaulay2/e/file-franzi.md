# `franzi-*.{cpp,hpp}` — Hinkelmann's Boolean ring GB (public-domain)

The `franzi-*` family of files implements **Gröbner bases over Boolean
rings** — `F_2[x_1, …, x_n] / (x_i^2 - x_i)` — contributed by Franziska
Hinkelmann. The code is explicitly in the **public domain** (see the
header copyright line).

Part of the [Gröbner bases](groebner-bases.md) area. Companion path to
[`bibasis/`](bibasis/README.md) (also a Boolean-ring GB engine).

[← per-area: groebner-bases](groebner-bases.md) · [← engine overview](README.md)

## Files

| File | Role |
|---|---|
| `franzi-brp.{cpp,hpp}` | Boolean-ring polynomial type (`brMonomial`) and its multi-set / lex storage |
| `franzi-brp-test.cpp` | Standalone test driver — exercises `brMonomial` operations directly |
| `franzi-gb.{cpp,hpp}` | The Buchberger-style GB algorithm over Boolean rings |
| `franzi-interface.{cpp,hpp}` | M2-side adapter so the algorithm is reachable via the standard GB dispatcher |

## Header glimpse

```cpp
/* This code written by Franziska Hinkelmann is in the public domain */

#include <set>
#include <vector>
#include <iostream>
#include <list>
#include <map>
#include <string>

//////// CAREFUL ////////
// addition is not const! It changes this
//////// CAREFUL ////////

typedef unsigned long brMonomial;

struct lex {
    bool operator()(const brMonomial &lhs, const brMonomial &rhs) const {
        return lhs > rhs;
    }
};
```

Two design choices to note:

1. **`brMonomial` = `unsigned long`** — each monomial is encoded as a
   single 64-bit unsigned int. Bit `i` of the value is set iff variable
   `x_i` appears in the monomial. This limits the engine to **64
   variables maximum** — fine for many applications (cryptographic
   Boolean functions, polynomial systems from coding theory).
2. **"addition is not const"** — the header warns that adding two
   `brMonomial`s mutates the left operand. This is a deliberate
   performance choice but a pitfall for engine-style `const`-careful
   code.

The `lex` functor reverses the natural unsigned `<` so that highest-bit
monomials come first under lex order.

## Algorithm sketch

`franzi-gb.cpp` implements Buchberger's algorithm specialised to the
Boolean case:

- Multiplication of Boolean monomials = bitwise OR (with idempotent
  exponents).
- Two monomials are coprime iff their bitwise AND is zero.
- S-polynomials and reductions all work via XOR of supporting bit sets.

The implementation is dramatically faster than the general GB
machinery on Boolean inputs because every monomial operation reduces
to one or two machine instructions.

## Status vs. `bibasis/`

The engine has two independent Boolean-ring GB engines:

| Engine | Algorithm | Variables limit |
|---|---|---|
| `franzi-*` (this family) | Buchberger | 64 (one `unsigned long`) |
| [`bibasis/`](bibasis/README.md) | Janet / involutive bases | no fixed limit |

Each shines on different inputs. The dispatcher selects per user
strategy option.

## Related

- [`groebner-bases.md`](groebner-bases.md) — area overview.
- [`bibasis/README.md`](bibasis/README.md) — alternative Boolean-ring
  engine.
- [`file-comp-gb.md`](file-comp-gb.md) — `GBComputation` base wrapping
  this.
- [`BooleanGB`](../packages/BooleanGB.m2) — the M2-side wrapper package.
