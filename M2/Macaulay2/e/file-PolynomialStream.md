# `PolynomialStream.hpp` — streaming polynomial-input concept

`PolynomialStream.hpp` defines a **C++ concept (informally)** that
streaming polynomial-input consumers in the refactored F4 path
implement. It declares the integer type aliases shared with
[`gb-f4/file-MonomialTypes.md`](gb-f4/file-MonomialTypes.md) and
sketches the protocol an SLP-style streaming producer should follow.

Part of the [Polynomial rings](polynomial-rings.md) area.

[← per-area: polynomial-rings](polynomial-rings.md) · [← engine overview](README.md)

## Type aliases

```cpp
namespace newf4 {

// TODO: these must match BasicPoly.
using Coefficient = int32_t;
using VarIndex    = int32_t;
using Exponent    = int32_t;
using Component   = int32_t;

}
```

Four typed aliases naming the streamed components:

| Alias | Meaning |
|---|---|
| `Coefficient` | A term's coefficient (or its placeholder, since the parser may produce values before the ring is in scope) |
| `VarIndex` | The variable index in a term |
| `Exponent` | The exponent of a variable in a term |
| `Component` | The free-module component the term belongs to |

The TODO note "must match BasicPoly" flags that these typedefs
should track [`file-BasicPoly.md`](file-BasicPoly.md)'s in-progress
coefficient migration (from `int32_t` to a more general type).

## The streaming concept

The rest of the header sketches what a `PolynomialStream` should
provide:

```cpp
// TODO: once we go to c++20, enable the concept PolynomialStream
#if 0
template <typename T>
concept PolynomialStream = requires (T str,
                                     size_t count,
                                     Component com,
                                     VarIndex index,
                                     Exponent exponent,
                                     /* ... */) {
    str.idealBegin(count);
    str.appendPolynomialBegin(count);
    str.appendTermBegin(com);
    str.appendExponent(index, exponent);
    str.appendTermDone(/* coefficient */);
    str.appendPolynomialDone();
    str.idealDone();
};
#endif
```

The C++20 `concept` is currently in `#if 0` because the engine still
targets C++17. The shape is what concrete streaming consumers like
[`file-matrix-stream.md`](file-matrix-stream.md) and
[`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md)
already implement.

When the engine moves to C++20, the concept can be enabled and
templated APIs that accept any polynomial-streaming consumer can be
written with proper constraints.

## Used by

- [`gb-f4/file-PolynomialList.md`](gb-f4/file-PolynomialList.md) —
  consumer.
- [`file-matrix-stream.md`](file-matrix-stream.md) — adjacent
  consumer.
- [`file-BasicPolyListParser.md`](file-BasicPolyListParser.md) —
  producer.

## Related

- [`polynomial-rings.md`](polynomial-rings.md) — area overview.
- [`file-BasicPoly.md`](file-BasicPoly.md) — concrete value type.
- [`gb-f4/file-MonomialTypes.md`](gb-f4/file-MonomialTypes.md) —
  parallel newf4-side typedefs.
