#include "IntegerParameter.h"

#include "error.h"
#include <sstream>
#include <cctype>

namespace mathic {
  IntegerParameter::IntegerParameter(const std::string& name,
                                     const std::string& description,
                                     unsigned int defaultValue):
    CliParameter(name, description),
    _value(defaultValue) {
  }

  std::string IntegerParameter::argumentType() const {
    return "INTEGER";
  }

  std::string IntegerParameter::valueAsString() const {
    std::ostringstream out;
    out << value();
    return out.str();
  }

  void IntegerParameter::processArgument(const std::string& argument) {
    std::istringstream in(argument);
    in >> _value;
    std::ostringstream out;
    out << _value;
    if (out.str() != argument) {
      reportError("The value given to option -" + name() + " was not "
        "as expected. It must be a non-negative integer in the range "
        "[0, 2^31-1].");
    }
  }
}
