# `BasicPolyListParser.{cpp,hpp}` — parser for `BasicPolyList` text formats

`BasicPolyListParser.hpp` declares parsers that read polynomial data
from text (file or string) into the engine's portable
[`BasicPolyList`](file-BasicPoly.md) type. Two formats are supported:
the engine's own simple text format and **Msolve's** input format.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Entry points

```cpp
#pragma once

#include <string>
#include <vector>

#include "BasicPolyList.hpp"

// requires Msolve header to determine variables, etc.
BasicPolyList parseMsolveFromString(std::string contents);
BasicPolyList parseMsolveFile(std::string filename);

BasicPolyList parseBasicPolyListFromString(std::string contents,
                                            std::vector<std::string> varnames);
```

Three free functions:

- **`parseMsolveFromString`** / **`parseMsolveFile`** — parse a
  polynomial system in [Msolve](https://msolve.lip6.fr/) input
  format. Msolve is an external solver M2 can dispatch to; round-
  tripping inputs through it requires reading and writing its format.
- **`parseBasicPolyListFromString`** — parse the engine's own
  human-readable polynomial list format, given an explicit list of
  variable names.

## Why a free-function API

Parsers don't carry state across calls — they take text in, return
`BasicPolyList` out. The free-function shape is the most direct
expression of that. The output `BasicPolyList`
([`file-BasicPoly.md`](file-BasicPoly.md)) is a portable polynomial
container that doesn't require a fully constructed `PolynomialRing`,
so parsers can run before any engine ring is set up.

## Engine text format

The format `parseBasicPolyListFromString` accepts:

```text
ideal:
  poly_1
  poly_2
  ...
```

Each `poly_i` is a sum-of-terms with explicit `*` for multiplication,
`^` for exponents. Variable names come from the second argument so
the parser can map text to indices.

## Msolve format

Msolve's input is similar but more constrained — it expects the
variable list and characteristic on dedicated header lines. The
parser handles both the header parsing and the polynomial-body
parsing.

## Used by

- [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md) —
  uses streaming construction; this parser feeds it.
- M2-side `BasicPolyList` test inputs.
- Msolve dispatch in the
  [`Msolve`](../packages) package wrappers.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-BasicPoly.md`](file-BasicPoly.md) — output type.
- [`file-matrix-stream.md`](file-matrix-stream.md) — streaming
  matrix builder.
- [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md) —
  primary consumer.
- Msolve — external solver.
