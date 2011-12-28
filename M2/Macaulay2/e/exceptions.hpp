#ifndef _exceptions_h_
#define _exceptions_h_

#include <stdexcept>
#include "newdelete.hpp"

using namespace std;

namespace exc {
     struct engine_error : public runtime_error, public our_new_delete {
          explicit engine_error(const string &msg) : runtime_error(msg) {}
     };
     struct overflow_error : public engine_error {
          explicit overflow_error(const string &msg) : engine_error(msg) {}
     };
     struct division_by_zero_error : public engine_error {
          explicit division_by_zero_error(const string &msg) : engine_error(msg) {}
     };
     struct internal_error : public engine_error {
          explicit internal_error(const string &msg) : engine_error(msg) {}
     };

}

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
