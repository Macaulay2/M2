#include "error.h"
#include <sstream>

namespace mathic {
  void reportError(const std::string& errorMsg) {
    throw MathicException("ERROR: " + errorMsg);
  }

  void reportInternalError(const std::string& errorMsg) {
    throw InternalMathicException("INTERNAL ERROR: " + errorMsg);
  }

  void reportInternalError
  (const std::string& errorMsg, const char* file, unsigned int lineNumber) {

    std::ostringstream err;
    err << errorMsg << '\n'
        << "The internal error occurred in file " << file
        << " on line " << lineNumber << '.';
    reportInternalError(err.str());
  }
}
