#ifndef MATHIC_PARAMETER_GUARD
#define MATHIC_PARAMETER_GUARD

#include "stdinc.h"
#include <string>

namespace mathic {
  class CliParameter {
  public:
    virtual ~CliParameter();

    const std::string& name() const {return _name;}
    const std::string& description() const {return _description;}
    void appendToDescription(const std::string& str);

    virtual std::string argumentType() const = 0;
    virtual std::string valueAsString() const = 0;
    virtual void processArgument(const std::string& argument) = 0;

  protected:
    CliParameter(const std::string& name, const std::string& description);

  private:
    std::string _name;
    std::string _description;
  };
}

#endif
