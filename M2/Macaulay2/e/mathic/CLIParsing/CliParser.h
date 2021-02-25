#ifndef MATHIC_CLI_PARSER_GUARD
#define MATHIC_CLI_PARSER_GUARD

#include "NameFactory.h"
#include "Action.h"

namespace mathic {
  class CliParser {
  public:
    CliParser();

    template<class ConcreteAction>
    void registerAction(const std::string& name);

    // picks the name up from ConcreteAction::staticName().
    template<class ConcreteAction>
    void registerAction();

    void registerHelpAction
      (const std::string& preMessage, const std::string& postMessage);

    void pushBackRegisteredActionNames(
      std::vector<std::string>& names
    ) const;

    const std::string& helpPreMessage() const {return _helpPreMessage;}
    const std::string& helpPostMessage() const {return _helpPostMessage;}

    std::unique_ptr<Action> parse(int argc, char** argv);
    std::unique_ptr<Action> parse(const std::vector<std::string>& commandLine);
    std::unique_ptr<Action> createActionWithPrefix(const std::string& prefix);

  private:
    NameFactory<Action> _actions;
    std::string _helpPreMessage;
    std::string _helpPostMessage;
  };

  template<class ConcreteAction>
  void CliParser::registerAction(const std::string& name) {
    nameFactoryRegister<ConcreteAction>(_actions, name);
  };

  template<class ConcreteAction>
  void CliParser::registerAction() {
    nameFactoryRegister<ConcreteAction>(_actions);
  };
}

#endif
