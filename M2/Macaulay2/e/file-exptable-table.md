# `exptable.h`, `table.h` — Hanson-style generic hash table

`table.h` is a **generic C hash table** (David R. Hanson's
implementation from *C Interfaces and Implementations*).
`exptable.h` is M2's **specialised wrapper** for the case key =
exponent vector, value = `unsigned long`.

Part of the [engine](README.md) — utilities.

[← engine overview](README.md) · [utilities](utilities.md)

## `table.h` — Hanson's generic table

```c
/* This is from Hanson's code, from his book: C interfaces and implementations.
   I have added c++ ifdef's to be able to use it from C++, MES, July 2002 */

#define T Table_T
struct T;
typedef struct T T;

#if defined(__cplusplus)
extern "C" {
#endif
extern T *   Table_new (int hint, ...);
```

Classic Hanson opaque-type style: `T` is `typedef`'d at the top
and used everywhere. M2 adopted this **circa 2002** when it
needed a fast generic hash table.

Operations:

- **`Table_new(hint)`** — allocate with hint about expected size.
- **`Table_get(T, key)`** — lookup; returns value or `NULL`.
- **`Table_put(T, key, value)`** — insert or update.
- **`Table_remove(T, key)`** — delete.
- **`Table_map(T, f)`** — iterate.

The implementation uses chained hashing with `hash` and `cmp`
function pointers stored in the table struct.

## `exptable.h` — exponent → ulong specialisation

```c
/* Implementation of a hashtable [exponent vectors, of fixed length] --> unsigned long int. */
/* The implementation uses table.{h,c}, which was written by David R. Hanson */

typedef int * exponent;
typedef struct exponent_table exponent_table;

extern exponent_table * exponent_table_new(int hint, int nvars);
```

Wraps `Table` for the exponent-vector use case:

- Key is `int *` (exponent vector of fixed length `nvars`).
- Value is `unsigned long` (typically a sequential ID).
- Hash function tuned for exponent vectors.
- Comparison is component-wise.

Used by old GB engines to assign unique IDs to monomials — the
F4 engine in [`gb-f4/`](gb-f4/README.md) has its own
`MonomialHashTable` which is faster but lives behind the same
conceptual abstraction.

## Why a C hash table

When this code was written, the C++ standard library wasn't
universally available (and `unordered_map` came in C++11, much
later). Hanson's table is:

- **Portable** — pure C, works anywhere.
- **Battle-tested** — appears in many projects.
- **Simple** — small enough to read top-to-bottom.

Modern code would use `std::unordered_map`, but ripping out
`Table` would be a heavy refactor with little gain in this
specific module.

## Used by

- Older GB / resolution engines accumulating monomial IDs.
- A few utility paths in the engine.
- *Not* used by the modern F4 engine
  ([`gb-f4/`](gb-f4/README.md)) which has its own table.

## Related

- [`README.md`](README.md) — engine overview.
- [`utilities.md`](utilities.md) — area.
- [`gb-f4/file-MonomialHashTable.md`](gb-f4/file-MonomialHashTable.md)
  — modern alternative.
- David R. Hanson, *C Interfaces and Implementations* — source of
  the design.
