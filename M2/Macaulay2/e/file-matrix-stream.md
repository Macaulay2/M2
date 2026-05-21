# `matrix-stream.{cpp,hpp}` — `MatrixStream` (stream-based matrix construction)

`matrix-stream.cpp` implements the engine's **streaming matrix
construction** API — a builder pattern that accepts polynomials term by
term and produces a [`Matrix`](file-matrix.md) at the end. It is the
preferred path for reading matrices from text or binary input formats
because it never holds the full input in memory.

Part of the [Matrices](matrices.md) area.

[← per-area: matrices](matrices.md) · [← engine overview](README.md)

## Streaming API (paraphrased)

```cpp
#include "poly.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"

// Example of building an ideal (1-row matrix) over Z/p[x, y]:
s.idealBegin(2);
  s.appendPolynomialBegin(2);                  // x^2 − y
    s.appendTermBegin(0);
      s.appendExponent(0, 2);                  // x^2
    s.appendTermDone(1);                       // coefficient 1
    s.appendTermBegin(0);
      s.appendExponent(1, 1);                  // y
    s.appendTermDone(/* -1 */);                // coefficient -1
  s.appendPolynomialDone();
s.idealDone();

Matrix *m = s.value();
```

The flow is a hand-rolled state machine:

1. **`idealBegin(nGenerators)`** — start a single-row matrix.
2. **`matrixBegin(nrows, ncols)`** — alternative entry for a general
   matrix.
3. **`appendPolynomialBegin(nTerms)`** — start the next polynomial in
   the matrix.
4. **`appendTermBegin(component)`** — start the next term within the
   polynomial. The `component` is the row index in the target free
   module.
5. **`appendExponent(var, exp)`** — add `var^exp` to the current term's
   monomial.
6. **`appendTermDone(coefficient)`** — close the term with the given
   coefficient.
7. **`appendPolynomialDone()`** — close the polynomial.
8. **`idealDone()` / `matrixDone()`** — close the matrix.
9. **`value()`** — retrieve the constructed `Matrix*`.

Strict ordering: each `Begin` must have a matching `Done` at the same
nesting depth.

## Why a streaming API

A `MatrixConstructor` ([`file-matrix-con.md`](file-matrix-con.md)) needs
the whole polynomial constructed before it can `set_column(...)`.
`MatrixStream` lets a parser emit one int at a time, with no peeking
ahead. This:

- **Saves memory** — no intermediate `mpz_t` arrays or polynomial
  AST.
- **Supports large inputs** — file-backed matrix import doesn't load
  the file into memory.
- **Works with any tokenizer** — text formats, binary formats, gRPC
  streams, etc. all feed the same API.

## Underlying construction

Inside `MatrixStream`, each completed polynomial is handed to a wrapped
`MatrixConstructor`. `appendTermDone(coeff)` constructs a single `vec`
entry for the current `(coeff, monomial)` pair and accumulates it.
`appendPolynomialDone()` finalises the column and hands it to the
`MatrixConstructor`. `value()` calls `MatrixConstructor::to_matrix()`
and returns its result.

## Used by

- [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md) —
  streams polynomial input.
- [`file-BasicPoly.md`](file-BasicPoly.md) and its parser.
- Test inputs in [`unit-tests/`](unit-tests/README.md).
- M2-side `installPackage` when round-tripping example outputs.

## Related

- [`matrices.md`](matrices.md) — area overview.
- [`file-matrix-con.md`](file-matrix-con.md) — non-streaming builder.
- [`file-matrix.md`](file-matrix.md) — the output type.
- [`file-BasicPoly.md`](file-BasicPoly.md) — portable polynomial value
  type used by parsers feeding this API.
