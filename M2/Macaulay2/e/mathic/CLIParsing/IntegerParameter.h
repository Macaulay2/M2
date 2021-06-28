#ifndef MATHIC_INTEGER_PARAMETER_GUARD
#define MATHIC_INTEGER_PARAMETER_GUARD

#include "stdinc.h"
#include "CliParameter.h"
#include <utility>
#include <string>

namespace mathic {
  class IntegerParameter : public CliParameter {
  public:
    IntegerParameter(const std::string& name,
                     const std::string& description,
                     unsigned int defaultValue);

    unsigned int value() const {return _value;}
    void setValue(unsigned int value) {_value = value;}

    operator unsigned int() const {return value();}
    void operator=(unsigned int value) {setValue(value);}

    virtual std::string argumentType() const;
    virtual std::string valueAsString() const;
    virtual void processArgument(const std::string& argument);

  private:
    unsigned int _value;
  };
}

#endif
