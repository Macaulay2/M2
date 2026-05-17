# `text-io.{cpp,hpp}` — text-formatting helpers over `buffer`

`text-io.cpp` is a thin layer of text-formatting helpers built on top of
the engine's [`buffer`](file-buffer.md) class: number formatting, wrapped
line emission, indentation-aware writing. The goal is a uniform
appearance across all engine-produced text without each caller
re-implementing wrap logic.

Part of the [Utilities](utilities.md) area.

[← per-area: utilities](utilities.md) · [← engine overview](README.md)

## Key entry points

```cpp
extern int i_text_io();          // initialise

#define wrapping_prefix "   -- "

void bignum_text_out(buffer &o, mpz_srcptr a);

void clear_emit_size();
void emit_wrapped(const char *s);
inline void emit_wrapped(int prlevel, const char *s) {
    if (M2_gbTrace >= prlevel) emit_wrapped(s);
}
```

### `bignum_text_out`

Prints an `mpz_t` integer to a `buffer`. Used everywhere the engine needs
to render arbitrary-precision integers — coefficients of polynomials,
Hilbert-series numerators, Betti table entries, etc.

The implementation respects the engine's display conventions (no thousands
separator, leading `-` for negatives, no leading `+`).

### `emit_wrapped` and the wrapping prefix

`emit_wrapped(s)` prints `s` to stdout, wrapping long lines at a fixed
column and prefixing continuation lines with `wrapping_prefix` (`"   -- "`)
so the wrapping is visually unambiguous in M2's terminal-style output:

```
this is a very long line that exceeds the wrap column and so it must
   -- continue on the next line with the wrapping prefix
```

The overloaded form checks `M2_gbTrace >= prlevel` before emitting —
i.e. tracing output is opt-in via the `gbTrace` global.

### `clear_emit_size`

Resets the running line-length counter that drives wrapping. Called
between unrelated tracing blocks so a fresh block starts on a clean
column.

## Relation to `buffer`

[`buffer`](file-buffer.md) is the in-memory accumulator (no I/O).
`text-io` is the layer that bridges `buffer` to actual stdout, plus the
formatting helpers (`bignum_text_out`) that *fill* a `buffer` with
specific numeric representations.

## Related

- [`utilities.md`](utilities.md) — area overview.
- [`file-buffer.md`](file-buffer.md) — underlying buffer.
- `error.{cpp,hpp}` — uses `text-io` to format error messages.
- `debug.{cpp,hpp}` — uses `text-io` for debug traces.
- `M2_gbTrace` (defined in `d/`) — the global verbosity level.
