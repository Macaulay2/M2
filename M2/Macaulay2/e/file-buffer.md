# `buffer.{cpp,hpp}` — the engine's append-only byte buffer

`buffer` is the engine's lightweight **append-only byte buffer**. It is
used everywhere the engine produces text — pretty-printing, error
messages, serialised output, debug traces — as a GC-friendly, leak-proof
alternative to `std::ostringstream`.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## State

```cpp
class buffer : public our_new_delete {
    int   _size;
    int   _capacity;
    char *_buf;
    void  expand(int newcap);
};
```

A `buffer` holds:

- `_buf` — a `char*` allocated with `our_new_delete` (Boehm GC).
- `_size` — current logical length.
- `_capacity` — physical allocation length.

When `_size` would exceed `_capacity`, `expand(newcap)` reallocates with
`our_new_delete` again. The old buffer becomes unreferenced and is
collected by the GC; no manual freeing.

`BUFFER_INITIAL_CAPACITY = 100` is the starting size.

## Why not `std::ostringstream`

- **GC integration** — the buffer's storage is part of the GC heap; the
  STL's `string` is on the system heap and would create a leak boundary.
- **Performance** — no synchronisation locking, no `streambuf` indirection,
  no formatting traits. The buffer is a sequential `memcpy` and a length
  bump.
- **Determinism** — no locale-dependent number formatting; the engine
  prints numbers via its own `out << RingElement` infrastructure.

## API

The header overloads `operator<<` for all the engine's primitive types
plus a handful of helpers:

- `buffer & operator<<(char c)`, `operator<<(const char *s)`,
  `operator<<(int)`, etc.
- `operator<<(indent)` — emit `n` space characters (the `indent` struct in
  the header is a marker type that triggers this overload).
- `operator<<(cc_struct *)`, `operator<<(cc_doubles_struct *)` etc. —
  complex-number formatters tied to the engine's numeric types.

To consume the contents you typically extract `c_str()` (returns the
internal buffer with a nul terminator) or `to_string()` (a copy).

## Cross-engine reuse

Nearly every engine class exposes a `text_out(buffer &)` method that
prints itself into a buffer. The top-level error machinery
([`utilities.md`](utilities.md)) builds errors as `buffer` instances and
hands them to the interpreter.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`text-io.{cpp,hpp}`](utilities.md) — text-formatting helpers layered on
  top of `buffer`.
- `error.{cpp,hpp}` — uses `buffer` to build error messages.
- `newdelete.hpp` (per subdir) — `our_new_delete` backing storage.
