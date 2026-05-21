# `util.hpp` — small string / array conversion helpers

`util.hpp` is a tiny **utility header** with string and array
conversion helpers between the engine's M2 types (`M2_string`,
`M2_arrayint`) and the standard library's `std::string`,
`std::vector<int>`. Including it costs almost nothing; the few inline
functions it declares are usable everywhere.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Content

```cpp
#include <string>
#include <vector>
#include <iostream>

#include "interface/m2-mem.h"     // for getmemarraytype
#include "interface/m2-types.h"   // for M2_* types

/**
 * Utilities for converting between M2 types and standard C++ types
 */

inline M2_string string_std_to_M2(const std::string &s) {
    // ... allocate an M2_string and copy ...
}

// inverse:
inline std::string string_M2_to_std(M2_string s) {
    // ... return std::string(s->array, s->len) ...
}

// std::vector<int>     ↔ M2_arrayint
std::vector<int>   M2_arrayint_to_stdvector(M2_arrayint a);
M2_arrayint        stdvector_to_M2_arrayint(const std::vector<int> &v);

// (and similar for vector<long> / M2_arrayint, etc.)
```

The bidirectional conversions handle:

- **Strings** — `std::string ↔ M2_string`. Engine-side text usually
  starts as `std::string` (cleaner C++); M2 wants `M2_string` at the
  interpreter boundary.
- **Integer arrays** — `std::vector<int> ↔ M2_arrayint`. Same story:
  the engine prefers `std::vector` internally but exposes
  `M2_arrayint` at the boundary.

`M2_string` and `M2_arrayint` are length-prefixed, GC-managed types
from [`interface/file-m2-types-interface.md`](interface/file-m2-types-interface.md).
The conversions allocate fresh M2-side memory through the hooks in
[`interface/file-m2-mem-interface.md`](interface/file-m2-mem-interface.md).

## Why this file exists

Without `util.hpp`, every translation unit would re-implement the
string / array conversions ad-hoc — error-prone and duplicative.
Centralising them here:

- Reduces boilerplate per call site.
- Ensures one place to fix bugs (GC integration, length-prefix
  handling).
- Lets the conversions be `inline`d cheaply when the optimiser sees
  them.

## Used by

Essentially every engine `.cpp` that produces an M2-visible result or
consumes one — too many to enumerate.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`interface/file-m2-types-interface.md`](interface/file-m2-types-interface.md)
  — `M2_string`, `M2_arrayint` definitions.
- [`interface/file-m2-mem-interface.md`](interface/file-m2-mem-interface.md)
  — memory hooks the conversions go through.
