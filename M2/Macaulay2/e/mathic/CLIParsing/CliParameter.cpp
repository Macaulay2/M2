#include "CliParameter.h"
#include "error.h"

namespace mathic {
  CliParameter::CliParameter(
    const std::string& name,
    const std::string& description
  ):
    _name(name),
    _description(description) {
  }

  CliParameter::~CliParameter() {
  }

  void CliParameter::appendToDescription(const std::string& str) {
    _description += str;
  }
}
