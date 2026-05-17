# `ARingZZTest.cpp`, `ARingZZpTest.cpp`, `ARingQQGmpTest.cpp`, `ARingQQFlintTest.cpp` — integer / modular / rational `aring` tests

These four files together test the **integer-shaped templated
ring backends**: `ARingZZ` (FLINT-backed integers), `ARingZZp`
(modular arithmetic), `ARingQQGMP` (GMP rationals), and
`ARingQQFlint` (FLINT rationals).

Part of the [engine unit-tests suite](README.md).

[← back to unit-tests overview](README.md) · [← engine overview](../README.md)

## Common pattern

Each file follows the same skeleton:

```cpp
template <>
void getElement<M2::ARingXXX>(const M2::ARingXXX& R,
                              int index,
                              M2::ARingXXX::ElementType& result)
{
  if (index < 50)
    R.set_from_long(result, index - 25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      // ... set result from a ...
    }
}
```

The specialisation of `getElement` (declared in
[`ARingTest.hpp`](file-ARingTest-hpp.md)) drives the trials.
Then gtest macros (`TEST(...)`, `EXPECT_TRUE(...)`) exercise:

- **Construction** — `init`, `clear`, `set_zero`, `set_from_long`.
- **Arithmetic** — `add`, `subtract`, `mult`, `negate`, `invert`.
- **Identities** — `a + (-a) = 0`, `a * 1 = a`, `(a+b)+c = a+(b+c)`.
- **Comparison** — `is_zero`, `is_equal`, `is_unit`.

## `ARingZZTest.cpp`

Tests `M2::ARingZZ`, the FLINT-backed integers (file
[`../file-aring-zz-flint.md`](../file-aring-zz-flint.md)).

Notable: `getRandomInteger()` is declared `extern` here and
defined in `ARingZZpTest.cpp`. Sharing one random-integer source
keeps the tests deterministic across files when seeded the same.

## `ARingZZpTest.cpp`

Tests `M2::ARingZZp` (file
[`../file-aring-zzp.md`](../file-aring-zzp.md)) and indirectly
the FFPACK-backed variant (`aring-zzp-ffpack.hpp`).

This file is where `getRandomInteger()` is *defined*:

```cpp
gmp_ZZ getRandomInteger()
{
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      ...
    }
  ...
}
```

The random integers are bounded by `maxH = 10^11` — large enough
to exercise interesting paths, small enough to keep numbers
human-readable in failure messages.

## `ARingQQGmpTest.cpp` / `ARingQQFlintTest.cpp`

Two rational implementations:

| File | Backend | Header |
|---|---|---|
| `ARingQQGmpTest.cpp` | GMP `mpq_t` | `aring-qq-gmp.hpp` |
| `ARingQQFlintTest.cpp` | FLINT `fmpq_t` | `aring-qq-flint.hpp` |

Both should give identical answers — that's effectively what the
test suite verifies. Differences point to:

- A FLINT regression.
- A GMP edge case (e.g., `0/-1` normalisation).
- A wrapper bug.

Running both suites is the engine's cross-check that the two
backends agree.

## What these tests catch

Real bugs caught by this family historically:

- A FLINT version where `fmpq_canonicalise` of `0/-1` returned a
  non-canonical sign.
- A GMP rounding subtlety in `ARingQQGMP::invert(0)`.
- An off-by-one in `set_from_long` for `INT_MIN`.

## Related

- [`README.md`](README.md) — unit-tests overview.
- [`file-ARingTest-hpp.md`](file-ARingTest-hpp.md) — common fixture.
- [`../file-aring-zz-flint.md`](../file-aring-zz-flint.md),
  [`../file-aring-zzp.md`](../file-aring-zzp.md),
  [`../file-aring-qq-gmp.md`](../file-aring-qq-gmp.md) (if added),
  [`../file-aring-qq-flint.md`](../file-aring-qq-flint.md) (if
  added) — the rings under test.
- [`../coefficient-rings.md`](../coefficient-rings.md) — overall
  area.
