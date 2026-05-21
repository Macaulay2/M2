# `methods.m2` — method dispatch and `MethodFunction` machinery

`methods.m2` implements the **method-dispatch system** at the heart
of the M2 language. Methods are M2's primary form of dynamic
dispatch (the other being Hooks); this file is where they live.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What "method" means in M2

```m2
-*
  Methods are the most common type of dynamic dispatch, along with Hooks.
  The method function is stored under the youngest mutable hash table in the
  method key sequence.
*-
```

A **method** in M2 is a function whose implementation is chosen at
call time based on the types of its arguments. The user defines a
method via `method(...)` and registers an implementation:

```m2
f = method()
f ZZ := x -> "got an integer: " | toString x
f String := s -> "got a string: " | s
```

At the call site `f(5)`, M2 looks up the implementation registered
for `(f, ZZ)` and invokes it. This is the workhorse of M2 polymorphism.

## Storage convention

The header comment explains the lookup convention:

> The method function is stored under the youngest mutable hash table
> in the method key sequence.

Method implementations are stored as key-value pairs in the
classes' own hash tables — e.g., `f ZZ := body` stores `body` under
the key `f` in `ZZ`'s hash table. The "youngest mutable" rule
ensures the lookup is consistent across class hierarchies and
package overrides.

## `all'` helper

```m2
all' := (L, p) -> not any(L, x -> not p x)
```

A local utility: "all of L satisfy p". Equivalent to `all` (which is
declared in `lists.m2` per the comment), but local here to avoid the
load-order dependency. The pattern of providing a local
implementation of something defined elsewhere is common in early-
loaded Core files.

## How dispatch happens

For a method call `f(x, y, z)`:

1. Compute the **method key** = `(f, class x, class y, class z)`.
2. Walk the ancestors of each `class` argument
   ([`file-classes.md`](file-classes.md)'s `ancestors`).
3. Find the most specific registered handler that matches the
   ancestor combination.
4. Invoke that handler.

The walk is potentially expensive but cached aggressively. Most
method calls resolve in O(1) after the first invocation.

## Used by

Every M2 polymorphic operation goes through method dispatch:

- Arithmetic operators (`+`, `*`, etc.) are methods.
- Built-in functions (`rank`, `dim`, `degree`, …) are methods.
- User code rarely calls `method()` directly but uses it transitively.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-classes.md`](file-classes.md) — type hierarchy that
  dispatch walks.
- `file-option.md` — option-handling, the other half of method calls.
- M2-side `methods(f)` and `lookup(f, args)` built-ins surface this
  machinery.
