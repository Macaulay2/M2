#include "BoolParameter.h"

#include "error.h"

namespace mathic {
  BoolParameter::BoolParameter(const std::string& name,
                               const std::string& description,
                               bool defaultValue):
    CliParameter(name, description),
    _value(defaultValue) {
  }

  std::string BoolParameter::argumentType() const {
    return "[BOOL]";
  }

  std::string BoolParameter::valueAsString() const {
    return _value ? "on" : "off";
  }

  void BoolParameter::processArgument(const std::string& argument) {
    if (argument.empty() || argument == "on")
      _value = true;
    else if (argument == "off")
      _value = false;
    else {
      reportError("Option -" + name() + " was given the argument \"" +
        argument + "\". The only valid arguments are \"on\" and \"off\".");
    }
  }
}
