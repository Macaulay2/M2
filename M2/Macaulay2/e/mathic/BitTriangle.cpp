#include "stdinc.h"
#include "BitTriangle.h"

namespace mathic {
  size_t BitTriangle::getMemoryUse() const
  {
	size_t sum = mColumns.capacity() * sizeof(mColumns.front());
	const size_t stop = mColumns.size();
	for (size_t i = 0; i != stop; ++i) {
	  size_t const capacity = mColumns[i].capacity();
	  sum += (capacity + 7) / 8; // 8 bits per byte rounded up
	}
	return sum;
  }
}
