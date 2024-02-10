#ifndef _exceptions_h_
#define _exceptions_h_

#include "newdelete.hpp"
#include <stdexcept>
#include <string>

namespace exc {
struct engine_error : public std::runtime_error, public our_new_delete
{
  explicit engine_error(const std::string &msg) : std::runtime_error(msg) {}
};
struct overflow_exception : public engine_error
{
  explicit overflow_exception(const std::string &msg) : engine_error(msg) {}
};
struct division_by_zero_error : public engine_error
{
  explicit division_by_zero_error(const std::string &msg) : engine_error(msg) {}
  explicit division_by_zero_error() : engine_error(std::string{"atttempt to divide by zero"}) {}
};
struct internal_error : public engine_error
{
  explicit internal_error(const std::string &msg) : engine_error(msg) {}
};
}

#define TRY \
  try       \
    {
#define CATCH                         \
  }                                   \
  catch (const exc::engine_error& __x424621) \
  {                                   \
    ERROR(__x424621.what());          \
    return NULL;                      \
  }

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
