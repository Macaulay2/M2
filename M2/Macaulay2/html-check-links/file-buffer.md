# `buffer.h` — generic resizable-array macro

`buffer.h` provides a **macro-generated resizable-array template**
in pure C — the equivalent of a generic `std::vector` in a
language without templates.

Part of [`html-check-links/`](README.md).

[← html-check-links/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## The macro

```c
#define BUFFER(type) \
	typedef struct { int size, used; type *array; } type##buf; \
	static inline void rm##type(type##buf *x) { \
		if (x->used > 0) x->used--; \
		} \
	static inline void empty##type(type##buf *x) { \
		x->used = 0; \
		} \
	static inline void add##type(type##buf *x,type i) { \
		if (x->used == x->size) { \
			unsigned newsize = 2 * x->size + 1; \
			type *y = (type *) getmem(newsize * sizeof(type)); \
			if (x->size > 0) memcpy(y, x->array, x->size * sizeof(type)); \
			x->array = y; \
			x->size = newsize; \
```

`BUFFER(T)` generates:

- A struct type `Tbuf` with `size`, `used`, `array`.
- `rmT(buf)` — pop one element.
- `emptyT(buf)` — clear.
- `addT(buf, x)` — push (with capacity doubling).

Usage:

```c
BUFFER(int)              // generates intbuf, addint, rmint, emptyint
intbuf myints;
addint(&myints, 42);
addint(&myints, 99);
```

## Why a macro and not a real generic

In 1998-era C, this was the **only** way to get type-safe
generic containers:

- `void *` containers lose type safety.
- Macros that expand into typed code give you back type-checking.
- Real generics arrived in C++ but the rest of the tool is C.

Modern C still doesn't have templates; `_Generic` (C11) is too
limited for this. So the macro pattern remains canonical.

## Capacity doubling

```c
if (x->used == x->size) {
    unsigned newsize = 2 * x->size + 1;
    ...
}
```

Classic amortised-O(1) `push`: when full, double the capacity.
The `+1` handles the initial allocation (when `size == 0`,
doubling stays 0).

## Used by

- [`file-grammar.md`](file-grammar.md) — `BUFFER(location)` for
  tracking open tags.
- Anywhere else in the tool needing a dynamic array.

## Related

- [`README.md`](README.md) — html-check-links overview.
- [`file-getmem.md`](file-getmem.md) — allocator used by the
  macro.
