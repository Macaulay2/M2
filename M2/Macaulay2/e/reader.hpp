#ifndef _reader_hpp_
#define _reader_hpp_

/**
 * @file reader.hpp
 * @brief `M2::Reader<RingType>` --- parse a single ring element from a `std::istream`.
 *
 * Declares the per-ring stream reader: construct with a
 * `RingType&` and call `read(istream, result)` to consume one
 * element's textual form and land its value in
 * `RingType::ElementType`. The header forward-declares the
 * primary template and provides the explicit specialisation for
 * `ARingZZp`; per-ring `.cpp` specialisations parse to a
 * temporary (`mpz_t`, `mpfr_t`, ...) using the underlying
 * library's stream operator and then call the ring's
 * `set_from_*` to land the value in `ElementType`.
 *
 * The pattern keeps parse logic close to each ring's native
 * value type instead of going through a single `Ring::from_text`
 * virtual. Used by matrix / polynomial I/O, the streaming
 * polynomial parsers, and test harnesses that read serialised
 * inputs.
 *
 * @see aring-zzp.hpp
 */

#include "aring-zzp.hpp"
#include <cstddef>

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
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
