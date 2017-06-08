#ifndef _reader_hpp_
#define _reader_hpp_

#include "aring-zzp.hpp"
#include <cstddef>
#include <gmp.h>

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
