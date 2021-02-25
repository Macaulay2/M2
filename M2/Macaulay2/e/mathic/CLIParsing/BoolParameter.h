#ifndef MATHIC_BOOL_PARAMETER_GUARD
#define MATHIC_BOOL_PARAMETER_GUARD

#include "stdinc.h"
#include "CliParameter.h"
#include <utility>
#include <string>

namespace mathic {
  class BoolParameter : public CliParameter {
  public:
    BoolParameter(const std::string& name,
                  const std::string& description,
                  bool defaultValue);

    bool value() const {return _value;}
    void setValue(bool value) {_value = value;}

    operator bool() const {return value();}
    void operator=(bool value) {setValue(value);}

    virtual std::string argumentType() const;
    virtual std::string valueAsString() const;
    virtual void processArgument(const std::string& argument);

  private:
    bool _value;
  };
}

#endif
