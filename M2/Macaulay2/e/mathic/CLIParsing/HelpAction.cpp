#include "HelpAction.h"

#include "CliParameter.h"
#include "error.h"
#include "display.h"
#include "ColumnPrinter.h"
#include "CliParser.h"

#include <iostream>
#include <algorithm>

namespace mathic {
  HelpAction::HelpAction() {}

  const char* HelpAction::name() const {
    return staticName();
  }

  const char* HelpAction::description() const {
    return 
      "Giving the parameter 'help' displays a list of the available actions.\n"
      "Giving the parameters 'help action' displays a detailed description of "
      "that action.\n";
  }

  const char* HelpAction::shortDescription() const {
    return "Return information about the command line interface.";
  }

  void HelpAction::directOptions(
    std::vector<std::string> tokens,
    CliParser& parser
  ) {
    _parser = &parser;
    if (tokens.size() == 1)
      _topic = tokens.front();
    else if (tokens.size() > 1)
      reportError("Did not expect second option \"" + tokens[1] + "\".");
  }

  void HelpAction::pushBackParameters(std::vector<CliParameter*>& parameters) {
  }

  namespace {
    // Helper function for displayActionHelp().
    bool paramCmp(CliParameter* a, CliParameter* b) {
      MATHIC_ASSERT(a != 0);
      MATHIC_ASSERT(b != 0);
      return std::string(a->name()) < b->name();
    }
  }

  void HelpAction::displayActionHelp(Action& action) {
    std::ostringstream out;
    out << "Displaying information on action: " << action.name() << "\n\n";
    out << action.description() << "\n";

    std::vector<CliParameter*> parameters;
    action.pushBackParameters(parameters);
    sort(parameters.begin(), parameters.end(), paramCmp);

    display(out.str());

    if (!parameters.empty()) {
      fprintf(stderr, "\nThe parameters accepted by %s are as follows.\n",
              action.name());

      typedef std::vector<CliParameter*>::const_iterator cit;
      for (cit it = parameters.begin(); it != parameters.end(); ++it) {
        std::string defaultValue = (*it)->valueAsString();
        fprintf(stderr, "\n -%s %s   (default is %s)\n",
                (*it)->name().c_str(),
                (*it)->argumentType().c_str(),
                (*it)->valueAsString().c_str());
        display((*it)->description(), "   ");
      }
    }
  }

  void HelpAction::performAction() {
    if (_topic != "") {
      displayActionHelp(*_parser->createActionWithPrefix(_topic));
      return;
    }

    std::ostringstream out;

    out << _parser->helpPreMessage() << '\n';

    ColumnPrinter printer;
    printer.addColumn(false, " ");
    printer.addColumn(true, " - ");

    std::vector<std::string> names;
    _parser->pushBackRegisteredActionNames(names);
    for (std::vector<std::string>::const_iterator it = names.begin();
         it != names.end(); ++it) {
      std::unique_ptr<Action> action(_parser->createActionWithPrefix(*it));
      printer[0] << action->name() << '\n';
	  printer[1] << action->shortDescription() << '\n';
    }
    printer.print(out);
    out << '\n' << _parser->helpPostMessage() << '\n';
    display(out.str());
  }

  const char* HelpAction::staticName() {
    return "help";
  }

  const std::string& HelpAction::topic() const {
    return _topic;
  }
}
