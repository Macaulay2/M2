#include <stdexcept>

using namespace std;

namespace exc {
     struct engine_error : public runtime_error {
	  explicit engine_error(const string &msg) : runtime_error(msg) {}
     };
     struct overflow_error : public engine_error {
	  explicit overflow_error(const string &msg) : engine_error(msg) {}
     };
     struct internal_error : public engine_error {
	  explicit internal_error(const string &msg) : engine_error(msg) {}
     };

}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
