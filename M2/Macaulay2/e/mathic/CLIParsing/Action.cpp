#include "Action.h"
#include "error.h"

namespace mathic {
  Action::~Action() {
  }

  void Action::directOptions(
    std::vector<std::string> tokens,
    CliParser& parser
  ) {
    if (!tokens.empty()) {
      reportError("Expected a dash (-) to indicate an option when reading \"" +
        tokens.front() + "\".");
    }
  }
}
