# `reader.hpp`, `reader.cpp` — generic stream → ring-element reader

`reader.hpp` provides a **templated `Reader<RingType>`** class —
reads ring elements from a `std::istream`. Used for matrix /
polynomial I/O, test input, and any path that needs to parse
serialised ring elements.

Part of the [engine](README.md) — utilities.

[← engine overview](README.md) · [utilities](utilities.md)

## What's declared

```cpp
namespace M2 {

template <typename RingType>
class Reader
{
 public:
  typedef typename RingType::ElementType ElementType;

  Reader(const RingType& ring) : mRing(ring) {}
  void read(std::istream& i, ElementType& result);

 private:
  const RingType& mRing;
};

template <>
void Reader<ARingZZp>::read(std::istream& i, ElementType& result);
}
```

A small, focused class:

- Construct with a ring reference.
- Call `read(istream, result)` to parse one element.

The **specialisations** live in `reader.cpp`:

```cpp
template <>
void Reader<ARingZZp>::read(std::istream& i, ElementType& result)
{
  mpz_t a;
  mpz_init(a);
  i >> a;
  mRing.set_from_mpz(result, a);
  mpz_clear(a);
}
```

The `i >> a` uses GMP's stream-reading operator (from `gmpxx.h`).
The pattern: parse to a temporary `mpz_t`, then convert to the
ring's `ElementType`.

## Why templated

Different rings need different parse routines:

- **`ARingZZp`** — parse as `mpz_t`, reduce mod p.
- **`ARingRR`** — parse as `double` (no temporary needed).
- **`ARingRRR`** — parse via MPFR.
- **`ARingZZ`** — parse directly as `mpz_t`.

Each gets its own specialisation. The base template declares
intent but doesn't compile if instantiated for an unsupported
ring — a static check that the developer added the specialisation.

## What gets read

The format is **simple decimal-style**:

```
3
-7
12345/678
3.14
```

with optional minus sign and (for rationals) `/`. Not exotic
formats like scientific notation or hex — those would be added as
needed.

## Used by

- Test code: matrix I/O tests
  ([`unit-tests/file-dmat-matrix-tests.md`](unit-tests/file-dmat-matrix-tests.md)).
- Some matrix-stream paths
  ([`file-matrix-stream.md`](file-matrix-stream.md) if added).
- Developers serialising / deserialising ring elements for
  benchmarks.

## Related

- [`README.md`](README.md) — engine overview.
- [`unit-tests/file-test-harness.md`](unit-tests/file-test-harness.md)
  — `fromStream` is the higher-level form.
- [`utilities.md`](utilities.md) — area.
