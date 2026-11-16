#ifndef M2_BASIC_RINGS_READER_HPP_
#define M2_BASIC_RINGS_READER_HPP_

#include "basic-rings/aring-ZZp.hpp"
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
