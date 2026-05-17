# `aring-translate.hpp` — cross-ring coercion templates

`aring-translate.hpp` is the **cross-ring coercion** layer of the
`aring` framework. It declares conversion functions between any two
[`aring`-based](file-aring.md) rings — moving values from `Z` to `Q`,
`Q` to `RR`, `Z/p` to `Z`, and so on — at compile-time-resolved speed.

Part of the [Coefficient rings](coefficient-rings.md) area.

[← per-area: coefficient-rings](coefficient-rings.md) · [← engine overview](README.md)

## Header preamble

```cpp
///////////////////////////////////////////////////////
// Contains functions which are "ring translational" //
///////////////////////////////////////////////////////

#include "aring-RR.hpp"
#include "aring-CC.hpp"
#include "aring-RRR.hpp"
#include "aring-CCC.hpp"
#include "aring-RRi.hpp"
#include "aring-CCi.hpp"
// ... pulls in every aring ...
```

The header pulls in **every** `aring-*.hpp` so it can declare
specialised conversion routines between any two combinations.

## What it provides

Templated functions of the form:

```cpp
template <typename SourceRing, typename TargetRing>
void translate(typename TargetRing::elem &result,
               const SourceRing      &S,
               const TargetRing      &T,
               const typename SourceRing::elem &value);
```

For every supported `(SourceRing, TargetRing)` pair, a specialisation
performs the conversion. Examples:

- `Z → Q`: numerator gets the integer, denominator gets 1.
- `Q → RR`: divide as MPFR float.
- `Z/p → Z`: lift to the canonical representative in `[0, p)`.
- `Z → Z/p`: reduce modulo `p`.
- `Z/p → Z/q` (different primes): forbidden unless `q | p` (and even
  then it's lossy).

Unsupported pairs (like `RR → Z`, which has no canonical answer)
produce a compile-time error rather than a silent default.

## Why a separate file

Each aring file knows about itself but not about every other aring.
Cross-ring conversions naturally live in a separate header that knows
about **all** of them. This way:

- Adding a new aring only requires touching its own file plus
  registering with this one.
- Compile-time resolution of `translate<Source, Target>(...)` lets
  the compiler inline the right code at every call site.

## Companion: `aring-glue.hpp`

[`file-aring-glue.md`](file-aring-glue.md) is the **vertical** bridge —
from `aring` to the legacy `Ring` API. `aring-translate.hpp` is the
**horizontal** bridge — from one aring to another. Together they
form the full aring-integration story:

- `aring-glue.hpp` makes an aring look like a legacy `Ring`.
- `aring-translate.hpp` lets two arings exchange values.

## Used by

- [`file-aring.md`](file-aring.md) — dispatcher uses these in
  `promote` / `lift` paths.
- [`file-relem.md`](file-relem.md)'s cross-ring operations.
- [`file-ringmap.md`](file-ringmap.md) — applying a `RingMap` is
  cross-ring conversion plus polynomial substitution.

## Related

- [`coefficient-rings.md`](coefficient-rings.md) — area overview.
- [`file-aring.md`](file-aring.md), [`file-aring-glue.md`](file-aring-glue.md)
  — sibling pieces of the aring framework.
- Each individual `aring-*.{cpp,hpp}` — sources / targets of
  translation.
