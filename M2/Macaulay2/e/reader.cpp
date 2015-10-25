#include "reader.hpp"

// The following includes a definition for: i >> mpz_t
#include "gmpxx.h"

namespace M2 {

  template<> void Reader<ARingZZp>::read(std::istream& i, ElementType& result)
  {
    mpz_t a;
    mpz_init(a);
    i >> a;
    mRing.set_from_mpz(result, a);
    mpz_clear(a);
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
