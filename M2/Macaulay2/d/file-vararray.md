# `vararray.d` — variable-length `int` arrays

`vararray.d` provides **`vararrayint`** — a dynamically-growing
array of `int`. It's the `int`-shaped sibling of
[`varstring`](file-strings.md), used wherever code accumulates an
unknown-length integer sequence.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's in the file

```d
--		Copyright 1994 by Daniel R. Grayson

use arithmetic;

export vararrayint := {
     ints:array(int),
     size:int
     };
export newvararrayint(i:int):vararrayint := vararrayint(
     new array(int) len i do provide 0, 
     0);
needatleast(i:int,v:vararrayint):void := (
     if length(v.ints) < i then (
     	  v.ints = new array(int) len 2*i do (
	       foreach c in v.ints do provide c;
	       while true do provide 0
	       );
     	  );
     );
export (v:vararrayint) << (c:int) : vararrayint := ...;
```

The pattern:

- **Capacity-doubling** growth (the `2*i` in `needatleast`).
- **`<<` operator overload** for appending — matches the
  `varstring` API style.
- **No shrink** — the assumption is that `vararrayint` is
  short-lived; clean it up when done.

## What it's used for

`vararrayint` shows up wherever the interpreter accumulates an
unknown-length list of integers:

- **The parser** accumulates token positions / IDs.
- **The lexer** accumulates code points.
- **`actors*.d`** collects indices for array-shape operations.

## Why a custom type instead of `array(int)`

`array(int)` is fixed-length in `.d`. Growing it requires
allocating a new one and copying — exactly what `vararrayint`
hides behind a clean API.

## Used by

- The lexer / parser — accumulating IDs and positions.
- [`file-nets.md`](file-nets.md) — accumulating column widths.
- [`file-evaluate.md`](file-evaluate.md) — sequence-building paths.

## Related

- [`README.md`](README.md) — d/ overview.
- [`file-strings.md`](file-strings.md) — `varstring` sibling.
- [`file-arithmetic.md`](file-arithmetic.md) — `int` type.
