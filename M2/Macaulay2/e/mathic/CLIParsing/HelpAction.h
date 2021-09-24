#ifndef MATHIC_HELP_ACTION_GUARD
#define MATHIC_HELP_ACTION_GUARD

#include "stdinc.h"
#include "Action.h"
#include <string>

namespace mathic {
  class HelpAction : public Action {
   public:
    HelpAction();

    virtual void directOptions
      (std::vector<std::string> tokens, CliParser& parser);
    virtual void performAction();

    virtual const char* name() const;
    virtual const char* description() const;
    virtual const char* shortDescription() const;
    virtual void pushBackParameters(std::vector<CliParameter*>& parameters);

    virtual bool isHelpAction() const {return true;}

    static const char* staticName();

  protected:
    const std::string& topic() const;

  private:
    void displayActionHelp(Action& action);

    CliParser* _parser;
    std::string _topic;
  };
}

#endif
