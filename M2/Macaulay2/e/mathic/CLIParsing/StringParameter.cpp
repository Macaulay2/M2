#include "StringParameter.h"

namespace mathic {
  StringParameter::StringParameter(const std::string& name,
                                   const std::string& description,
                                   const std::string& defaultValue):
    CliParameter(name, description),
    _value(defaultValue) {
  }

  std::string StringParameter::argumentType() const {
    return "STRING";
  }

  std::string StringParameter::valueAsString() const {
    return _value;
  }

  void StringParameter::processArgument(const std::string& argument) {
    _value = argument;
  }
}
