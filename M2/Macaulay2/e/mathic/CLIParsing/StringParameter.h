#ifndef MATHIC_STRING_PARAMETER_GUARD
#define MATHIC_STRING_PARAMETER_GUARD

#include "stdinc.h"
#include "CliParameter.h"
#include <utility>
#include <string>

namespace mathic {
  class StringParameter : public CliParameter {
  public:
    StringParameter(const std::string& name,
                    const std::string& description,
                    const std::string& defaultValue);

    const std::string& value() const {return _value;}
    void setValue(const std::string& value) {_value = value;}

    operator const std::string&() const {return value();}
    void operator=(const std::string& value) {setValue(value);}
    bool operator==(const std::string& str) const {return value() == str;}

    virtual std::string argumentType() const;
    virtual std::string valueAsString() const;
    virtual void processArgument(const std::string& argument);

  private:
    std::string _value;
  };
}

#endif
