#include <stdexcept>

using namespace std;

namespace exc {
     struct overflow : public overflow_error {
	  explicit overflow(const string &msg) : overflow_error(msg) {}
     };
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
