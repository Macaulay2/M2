# `f4-types.hpp` — F4 type vocabulary

`f4-types.hpp` declares the **type vocabulary** the F4 GB engine uses:
the `spair` struct, the `gbelem_type` enum, the basis-array type, and a
size-of-spair macro. It is to F4 what
[`gb-f4/file-MonomialTypes.md`](../gb-f4/file-MonomialTypes.md) is to
the new F4.

Part of the [`f4/`](README.md) subdirectory.

[← f4 overview](README.md) · [← engine overview](../README.md)

## Header includes

```cpp
#include <climits>                   // for INT_MIN
#include "VectorArithmetic.hpp"      // for ElementArray
#include "f4/f4-monlookup.hpp"       // for F4MonomialLookupTableT
#include "f4/moninfo.hpp"            // for MonomialInfo, monomial_word, pac...
#include "f4/varpower-monomial.hpp"  // for varpower_monomials, varpower_mon...
#include "newdelete.hpp"             // for our_new_delete, VECTOR
#include "style.hpp"                 // for LT
```

`f4-types.hpp` pulls together the encoded monomial layer, the
templated arithmetic ([`../file-VectorArithmetic.md`](../file-VectorArithmetic.md)),
and the lookup table. Everything F4 needs to declare a basis element
or an S-pair lives downstream of this header.

## `sizeofspair` macro

```cpp
#define sizeofspair(s, len) \
    (sizeof(*s) - sizeof(s->lcm) + (len) * sizeof(s->lcm[0]))
```

`spair`'s `lcm` field is a **flexible array** at the end of the struct
(the C trick for variable-length storage). `sizeofspair(s, len)`
computes the actual byte size needed to hold an `spair` with a given
lcm-monomial length, by subtracting the fixed-size flexible-array
declaration from `sizeof(*s)` and adding the right number of bytes.

This pattern matches the engine's preferred approach for inline
variable-length data — same idea as `gbvector`'s trailing `monom[1]`
([`../file-gbring.md`](../file-gbring.md)).

## `gbelem_type` enum

```cpp
enum gbelem_type {
    ELEM_IN_RING,         // ring elements (e.g. in the defining ideal of a quotient)
    ELEM_POSSIBLE_MINGEN, // candidate minimal generators (graded case: actually minimal)
    ELEM_MIN_GB,          // minimal GB elements
    ELEM_NON_MIN_GB       // GB elements which are not minimal
};
```

Every basis element carries one of these tags so the algorithm can:

- Tell the user which generators were minimal (`ELEM_POSSIBLE_MINGEN`).
- Skip ring elements when generating S-pairs.
- Distinguish "minimal GB" from "GB but redundant" for output filtering.

## `spair` struct (declared here, used by `F4SPairSet`)

The struct itself is declared in this header (rather than its consumer
[`file-f4-spairs.md`](file-f4-spairs.md)) because multiple parts of
F4 — basis update, lookup, sorting — need its fields directly. Keeping
it in `f4-types.hpp` centralises the layout.

## Used by

Essentially every other file in [`f4/`](README.md). Modifying this
header forces an F4-wide rebuild.

## Related

- [`README.md`](README.md) — f4 overview.
- [`file-f4-spairs.md`](file-f4-spairs.md), [`file-f4.md`](file-f4.md),
  [`file-f4-computation.md`](file-f4-computation.md) — primary
  consumers.
- [`../file-VectorArithmetic.md`](../file-VectorArithmetic.md) —
  arithmetic dispatcher referenced via `ElementArray`.
- [`file-moninfo.md`](file-moninfo.md), [`file-varpower-monomial.md`](file-varpower-monomial.md)
  — supplies the monomial typedefs.
