# `buckets.dd` — `Dictionary` bucket iteration

`buckets.dd` exposes the **internal bucket iteration** of a
`Dictionary` to user code — primarily for introspection and the
package-loading machinery that needs to enumerate symbols.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 1994,2010 by Daniel R. Grayson
use common;
bucketsfun(e:Expr):Expr := (
     when e
     is dc:DictionaryClosure do (
	  d := dc.dictionary;
	  lockRead(d.symboltable.mutex);
	  res := Expr(...)));
```

The single function `bucketsfun` takes a `DictionaryClosure` and
returns its internal symbol table — locked for reading first, then
walked.

## Why a dedicated file

Bucket iteration touches the **internal symbol-table mutex**.
Exposing this in `common.d` or `hashtables.dd` would mean every
file that uses those imports also pulls in the locking primitives.
Isolating bucket-iteration in its own tiny file keeps the
dependency graph clean.

## What "buckets" means

A `Dictionary` is a hash table of `Symbol` entries. The hash table
has a bucket array — `bucketsfun` exposes that array for:

- The package loader (introspecting a dictionary's contents).
- The `globalDictionaries` listing.
- `mutable HashTable` -> `HashTable` snapshot conversions.
- Test code asserting symbol-table invariants.

## Used by

- The `setup` code that walks dictionaries during package loading
  ([`../m2/file-packages.md`](../m2/file-packages.md)).
- M2-level introspection of dictionaries.
- The interpreter's symbol-resolution path in
  [`file-binding.md`](file-binding.md).

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-hashtables.md`](file-hashtables.md) — `HashTable` types.
- [`file-tokens.md`](file-tokens.md) — `Dictionary` and `Symbol`
  type definitions.
- [`file-binding.md`](file-binding.md) — primary consumer.
