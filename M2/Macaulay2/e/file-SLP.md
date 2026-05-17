# `SLP.{cpp,hpp}`, `SLP-defs.hpp`, `SLP-imp.hpp` — straight-line programs

A **straight-line program (SLP)** is a directed acyclic graph whose nodes are
arithmetic operations on a fixed set of inputs and constants. SLPs are the
evaluation model used by [Numerical Algebraic Geometry](file-NAG.md): given a
polynomial system once, compile it to an SLP, then evaluate the SLP at every
continuation step.

Part of the [Other computations](computations.md) area.

[← per-area: computations](computations.md) · [← engine overview](README.md)

## File split

| File | Role |
|---|---|
| `SLP.hpp` | Public header — pulls in `SLP-defs.hpp` and `SLP-imp.hpp` |
| `SLP-defs.hpp` | Node-type definitions and value-type templates |
| `SLP-imp.hpp` | Template implementations of evaluation, partial differentiation, etc. |
| `SLP.cpp` | Non-template glue and registry |

The header/template split exists so the same SLP code can be instantiated
over multiple coefficient types — `double`, `mpfr_t` (via `RRR`), `acb_t`
(via `CCC` for complex Arb), and so on. See
[`coefficient-rings.md`](coefficient-rings.md) for the numerical types.

## Node types

Conceptually each SLP node is one of:

- **Input**: parameter `x_i`.
- **Constant**: a numerical literal.
- **Binary op**: `+`, `*`, occasionally `−`, `/`, `^`.
- **Unary op**: scalar negation, square root for Newton steps.
- **Output**: tagged "this is the value to return at index *j*".

Evaluation is a single forward pass — extremely cache-friendly compared to
re-parsing a polynomial AST.

## Why SLPs (vs. dense or sparse polynomial evaluation)

For homotopy continuation, the same system is evaluated at thousands of
points along a path. An SLP:

- shares subexpressions exactly once across all outputs,
- caches partial derivatives by reusing the forward graph with a backward
  pass,
- compiles to tight inner loops because the operations and their order are
  known at SLP construction time.

`SLP-imp.hpp` includes a partial-derivative pass needed by Newton's method
inside the continuation step.

## Use sites

- [`NAG.{cpp,hpp}`](file-NAG.md) — primary user.
- [`unit-tests/PointArray.cpp`](unit-tests/README.md) — exercises SLP
  evaluation.

## TODO

`TODO-SLPs` in this directory tracks outstanding work — better just-in-time
compilation of SLPs, vectorised evaluation, support for additional numerical
back ends.

## Related

- [`file-NAG.md`](file-NAG.md) — the principal consumer.
- [`computations.md`](computations.md) — area overview.
- [`coefficient-rings.md`](coefficient-rings.md) — numerical types SLPs
  evaluate over.
