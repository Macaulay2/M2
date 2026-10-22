// Read one integer, leaving the suffix unread and the output unchanged on
// failure. ARing outputs must already be initialized by the caller.

#include "unit-tests/RingTest.hpp"

#include <gmpxx.h>

#include <istream>

#include "basic-rings/aring-ZZp.hpp"
#include "rings/ZZp.hpp"

template <>
std::istream& fromStream<M2::ARingZZp>(std::istream& input,
                                       const M2::ARingZZp& ring,
                                       M2::ARingZZp::ElementType& result)
{
  int value;
  if (input >> value) ring.set(result, value);
  return input;
}

template <>
bool fromStream<Z_mod>(std::istream& input,
                       const Z_mod& ring,
                       ring_elem& result)
{
  int value;
  if (!(input >> value)) return false;
  result = ring.from_long(value);
  return true;
}

template <>
bool fromStream<RingZZ>(std::istream& input,
                        const RingZZ& ring,
                        ring_elem& result)
{
  mpz_class value;
  if (!(input >> value)) return false;
  result = ring.from_int(value.get_mpz_t());
  return true;
}
