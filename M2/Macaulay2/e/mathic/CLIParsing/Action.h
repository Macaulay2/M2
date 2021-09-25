#ifndef MATHIC_ACTION_GUARD
#define MATHIC_ACTION_GUARD

#include "stdinc.h"
#include <string>
#include <vector>

namespace mathic {
  class CliParameter;
  class CliParser;

  class Action {
  public:
    virtual ~Action();

    // Called with tokens that precede any option of the
    // form -option. The default is to give an error saying
    // that a dash was expected if tokens is not empty.
    virtual void directOptions
      (std::vector<std::string> tokens, CliParser& parser);

    // Do what it is this action does.
    virtual void performAction() = 0;

    // ***************************************
    // **** Information provided by each Action

    // The name of the action.
    virtual const char* name() const = 0;

    // More detailed explanation of what the action does.
    virtual const char* description() const = 0;

    // One-line summary of the description.
    virtual const char* shortDescription() const = 0;

    // Append the parameters for this action to the passed-in container.
    // Do not clear the passed-in container.
    virtual void pushBackParameters(std::vector<CliParameter*>& parameters) = 0;

    // Return true if this class is HelpAction.
    virtual bool isHelpAction() const {return false;}
  };
}

#endif
