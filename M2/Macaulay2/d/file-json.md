# `json.d` — JSON parser (Jansson)

`json.d` provides M2's **JSON parser** — bindings to the
[Jansson](https://digip.org/jansson/) library for reading and
writing JSON.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
use common;
use util;
use hashtables;

header "#include <jansson.h>";

json_tstar  := Pointer "struct json_t *";
json_error_t := Type "struct json_error_t";
```

The `Pointer "struct json_t *"` and `Type "struct json_error_t"`
syntax is `.d`'s mechanism for declaring foreign-pointer types
that map to C structs.

## What's exposed

- **`parseJSON(str)`** — parse a string into M2 native types
  (`HashTable`, `Sequence`, `Number`, `String`, `Boolean`, `Nothing`).
- **`encodeJSON(value)`** — serialise an M2 value to a JSON string.

The conversion mapping:

| JSON | M2 |
|---|---|
| `null` | `null` |
| `true` / `false` | `true` / `false` |
| Number | `ZZ` / `QQ` / `RR` as appropriate |
| String | `String` |
| Array | `Sequence` |
| Object | `HashTable` |

## Why Jansson

Jansson is:

- Small (~5K LOC).
- C99, no STL dependency.
- Permissively licensed (MIT).
- Used by other projects M2 already depends on indirectly.

The alternative (parsing JSON by hand in `.d`) would be tedious;
linking Jansson is cheap.

## When JSON is used

- Importing configuration files.
- Exchanging data with web services.
- Some packages that talk to external databases / APIs.
- M2-Macaulay2Web protocol uses JSON-like messages.

## Used by

- M2 users calling `parseJSON` / `encodeJSON`.
- `webapp.m2` ([`../m2/file-webapp.md`](../m2/file-webapp.md))
  indirectly.

## Related

- [`README.md`](README.md) — d/ overview.
- Sister FFI files: [`file-python.md`](file-python.md),
  [`file-ffi.md`](file-ffi.md), [`file-xml.md`](file-xml.md),
  [`file-mysql.md`](file-mysql.md).
- Jansson — external linked library.
