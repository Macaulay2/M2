# `classes.dd` — M2 root type registry

`classes.dd` registers the **root types of the M2 type system** —
`Thing`, `Type`, `HashTable`, `Dictionary`, etc. — as M2-level
constants the rest of the interpreter and the Core layer can refer
to symbolically.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
--		Copyright 1994,2010 by Daniel R. Grayson
use common;

setupconst("Type",Expr(typeClass));
setupconst("Thing",Expr(thingClass));
setupconst("HashTable",Expr(hashTableClass));
setupconst("Dictionary",Expr(dictionaryClass));
setupconst("LocalDictionary",Expr(localDictionaryClass));
```

Long sequences of `setupconst` calls. `setupconst("Foo", Expr(...))`
binds a new global symbol `Foo` whose value is the given internal
HashTable (the *class* object for that type). After this file
runs, you can write `Type` or `HashTable` in M2 code and get the
class.

## Why this is a `.dd` not `.d`

C++ is required because:

- Some root types touch the engine via static initialisers.
- The class-objects themselves are constructed via C++
  factory functions in [`file-hashtables.md`](file-hashtables.md).

## The class hierarchy

The constants registered here form M2's **root of the type tree**:

```
Thing
├── Nothing
├── Boolean
├── Number
│   ├── ZZ, QQ, RR, CC, ...
├── String / Net / ...
├── HashTable
│   ├── Type
│   │   ├── HashTable        (yes — self-referential)
│   │   ├── ImmutableType
│   │   ├── Module, Matrix, Ring, ...
│   ├── BasicList
│   │   ├── List, Sequence, Array, ...
│   ├── ...
├── BasicList
├── ...
```

The Core M2 file [`../m2/file-classes.md`](../m2/file-classes.md)
extends this with the user-visible class hierarchy (Module, Matrix,
Ring, etc.).

## Load order

`classes.dd` runs **before** any M2 user code — it's part of the
interpreter bootstrap. The Core M2 [`classes.m2`](../m2/file-classes.md)
runs afterward to wire up user types.

## Used by

- The Core layer's [`classes.m2`](../m2/file-classes.md) extends
  from this base.
- Type checks throughout `actors*.d`.
- The documentation system, which walks the class tree.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-hashtables.md`](file-hashtables.md) — `HashTable` runtime
  primitive that root types are built on.
- [`file-tokens.md`](file-tokens.md) — `Symbol` and `Word` types
  these classes also use.
- [`../m2/file-classes.md`](../m2/file-classes.md) — Core M2
  extension of this hierarchy.
