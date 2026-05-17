# `m2util.hpp` — supervisor-side M2 type helpers

`m2util.hpp` is a **tiny helper header** providing utility
functions / inline allocations that the supervisor needs to
construct M2-shaped values for callbacks into the interpreter.

Part of the [`system/` directory](README.md).

[← system/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The whole file

```cpp
#ifndef _system_m2util_h_
#define _system_m2util_h_

#include "mutexclass.hpp"

#include <M2/gc-include.h>

extern M2_string M2_tostring_3(const char *);
struct parse_Sequence_struct {unsigned short type_;int len;parse_Expr array[1];};

typedef struct parse_Sequence_struct * parse_Sequence;

extern parse_Sequence expr_emptySequence;

parse_Sequence allocSequence(int len)
{
  return (parse_Sequence)GC_MALLOC(sizeof(parse_Sequence_struct)+sizeof(parse_Expr)*(len-1));
}

#endif
```

What's here:

- **`M2_tostring_3`** — convert a C string to an `M2_string`
  (declared, implemented in interpreter).
- **`parse_Sequence_struct`** — the wire format of an M2
  `Sequence` (mirror of what `scc1` would generate from
  `tokens.d`).
- **`expr_emptySequence`** — sentinel for the empty sequence.
- **`allocSequence(len)`** — GC-allocates a `parse_Sequence` with
  `len` slots.

## Why this header exists

The supervisor sometimes needs to call into the interpreter with
M2-shaped arguments. For example, when a task completes,
result-passing must produce a `Sequence` containing the result.
The supervisor can't (and shouldn't) call into the interpreter's
own helper functions for this — the interpreter is in a different
linkage unit.

`m2util.hpp` provides just enough wire-format knowledge to
construct these values from C++ code.

## The `array[1]` trick

```cpp
struct parse_Sequence_struct {unsigned short type_;int len;parse_Expr array[1];};
```

The classic **flexible array member** trick (pre-C99). Declares
`array[1]` but `allocSequence` allocates `sizeof(struct) +
(len-1)*sizeof(parse_Expr)` so the array can actually hold `len`
elements. Modern code would use `array[]` (C99 flexible array),
but the `[1]` form predates that.

## Why a `.hpp` with `.cpp`-shaped code?

`allocSequence` is `inline`d in the header (no separate `.cpp`)
because:

- It's tiny.
- It's only used in a few places.
- Avoiding a separate translation unit cuts build time.

The `inline` is implicit because the function is fully defined in
a header — multiple translation units that include the header
each get their own copy, and the linker (typically) keeps only
one.

## Used by

- [`file-supervisor.md`](file-supervisor.md) — supervisor uses
  these to construct task-completion notifications.
- Anywhere the system layer needs to talk to M2 values directly.

## Related

- [`README.md`](README.md) — system/ overview.
- [`../d/file-tokens.md`](../d/file-tokens.md) — defines
  `parse_Sequence` on the interpreter side.
- [`file-supervisor.md`](file-supervisor.md) — primary consumer.
