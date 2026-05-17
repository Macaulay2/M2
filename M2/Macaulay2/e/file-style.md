# `style.hpp` — engine-wide stylistic constants and macros

`style.hpp` is a tiny **engine-wide stylistic header**. It defines a
handful of constants and macros — comparison-result codes, the
`INTSIZE(x)` cast, the `GEOHEAP_SIZE` constant — that essentially
every engine file uses. Including it is the engine's analogue of
"and one more useful thing" for boilerplate.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Constants

```cpp
#include "newdelete.hpp"
#include "engine-includes.hpp"

#define INTSIZE(a) static_cast<int>((a).size())

const int LT           = -1;
const int EQ           =  0;
const int GT           =  1;
const int INCOMPARABLE =  2;
const int EXCEPTION    = -2;

// Used for all of the heap types: polynomial, vector, resolution vectors.
#define GEOHEAP_SIZE 15

extern const int heap_size[GEOHEAP_SIZE];
```

### Comparison-result codes

`LT`, `EQ`, `GT`, `INCOMPARABLE`, `EXCEPTION` are the **integer return
codes** for every engine comparator. The convention:

- `LT = -1`, `EQ = 0`, `GT = +1` — the standard three-way result.
- `INCOMPARABLE = 2` — used by partial orders where two elements have
  no order relation (e.g. monomials in a free module with a "position
  up" component).
- `EXCEPTION = -2` — the comparator raised an exception; the caller
  should propagate or recover.

Every `compare` method on a monomial, monoid, ring element, etc.
returns one of these.

### `INTSIZE(a)` macro

```cpp
#define INTSIZE(a) static_cast<int>((a).size())
```

The engine often deals with sizes returned by `std::vector::size()`,
which is `size_t` (unsigned). When mixed with engine `int`-typed
indices, this can produce compiler warnings about signed/unsigned
conversion. `INTSIZE` casts to `int` in one place. The pattern dates to
before the engine standardised on C++17.

### `GEOHEAP_SIZE` and `heap_size`

The constant `GEOHEAP_SIZE = 15` is the **size of the "geometric heap"**
data structure used in polynomial / vector accumulation. Multiple
engine paths use a heap whose levels grow geometrically; this fixes the
number of levels.

`heap_size[i]` (declared `extern`, defined elsewhere) gives the size of
the `i`-th heap level — typically a sequence like `4, 16, 64, 256, …`.

## Why a header for so little

The engine pre-dates the C++ `enum class`; `LT` / `EQ` / `GT` are kept
as named `int` constants so existing code keeps working. They could be
modernised to an `enum class` but that would be a wide-touch refactor.

## Used by

Essentially every `.cpp` and `.hpp` in `e/`. Including this header
costs almost nothing; the file is intentionally tiny.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-overflow.md`](file-overflow.md) — pairs with `style.hpp` in
  most monomial-arithmetic code.
- `engine-includes.hpp` (umbrella header).
- `newdelete.hpp` (in subdirectories) — sibling tiny header.
