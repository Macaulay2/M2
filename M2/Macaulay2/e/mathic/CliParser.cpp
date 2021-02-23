#include "CliParser.h"

#include "error.h"
#include "CliParameter.h"

namespace mathic {
  namespace {
    // We are using a NameFactory just for its prefix-finding functionality, so
    // we set the thing being created to be just void pointers that are null.
    typedef void* Dummy;
    typedef NameFactory<Dummy> ParamNames;
    ParamNames makeParamNames(std::vector<CliParameter*> params) {
      ParamNames names("option");
      for (size_t i = 0; i < params.size(); ++i) {
        const auto makeNull = [](){return std::unique_ptr<Dummy>();};
        names.registerProduct(params[i]->name(), makeNull);
      }
      return names;
    }
  }

  void CliParser::pushBackRegisteredActionNames(
    std::vector<std::string>& names
  ) const {
    _actions.namesWithPrefix("", names);
  }


  CliParser::CliParser(): _actions("action") {}

  std::unique_ptr<Action> CliParser::createActionWithPrefix(
    const std::string& prefix
  ) {
    return createWithPrefix(_actions, prefix);
  }

  std::unique_ptr<Action> CliParser::parse(int argc, char** argv) {
    std::vector<std::string> commandLine(argv, argv + argc);
    return parse(commandLine);
  }

  std::unique_ptr<Action> CliParser::parse
    (const std::vector<std::string>& commandLine) {
    if (commandLine.empty())
      throwError<UnknownNameException>("No action specified.");
    std::unique_ptr<Action> action = createActionWithPrefix(commandLine[0]);

    std::vector<CliParameter*> params;
    action->pushBackParameters(params);
    ParamNames paramNames = makeParamNames(params);

    std::vector<std::string> options;
    std::vector<std::string> directOptions;
    bool sawDash = false;
    for (size_t i = 1; i < commandLine.size(); ++i) {
      if (commandLine[i].empty())
        continue;
      if (commandLine[i][0] == '-')
        sawDash = true;
      if (sawDash)
        options.push_back(commandLine[i]);
      else
        directOptions.push_back(commandLine[i]);
    }

    action->directOptions(directOptions, *this);

    size_t i = 1;
    for (size_t i = 0; i < options.size(); ++i) {
      std::string const& token = options[i];
      if (token[0] != '-')
        reportError("Expected an option when reading \"" +
                    token + "\", but options start with a dash (-).\n");
      std::string noDash(token.begin() + 1, token.end());
      std::string name = uniqueNameWithPrefix(paramNames, noDash);

      std::string optionArgument;
      if (i + 1 < options.size() && options[i + 1][0] != '-') {
        optionArgument = options[i + 1];
        ++i;
      }

      for (std::vector<CliParameter*>::iterator it = params.begin();; ++it) {
        if (it == params.end()) {
          // shouldn't get here as name was recognized above
          reportInternalError("Processing non-existent option \""
            + name + "\".");
        }
        if ((*it)->name() == name) {
          (*it)->processArgument(optionArgument);
          break;
        }
      }
    }
    return action;
  }
}
