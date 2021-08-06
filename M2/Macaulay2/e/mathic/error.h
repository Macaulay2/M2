#ifndef MATHIC_ERROR_GUARD
#define MATHIC_ERROR_GUARD

#include <stdexcept>
#include <string>

namespace mathic {
  /** This is the base of the Mathic exception hierarchy for exceptions
   that can occur due to expected error conditions. */
  class MathicException : public std::runtime_error {
  public:
    MathicException(const std::string& str): runtime_error(str) {}
  };

  /** This exception signals that a bug in Mathic has been detected. */
  class InternalMathicException : public std::logic_error {
   public:
    InternalMathicException(const std::string& str): logic_error(str) {}
  };

  // The do {...} while (0) is to collect everything into a single
  // statement that still requires a semicolon after it. The throw is to
  // prevent spurious compiler warnings about a missing return
  // statement.
  #define MATHIC_INTERNAL_ERROR(msg) \
    do { \
      reportInternalError(msg, __FILE__, __LINE__); \
      throw; \
    } while (false)
  #define INTERNAL_ERROR_UNIMPLEMENTED() \
    INTERNAL_ERROR("Called function that has not been implemented.")

  // These methods throw exceptions.
  void reportError(const std::string& errorMsg);
  void reportInternalError(const std::string& errorMsg);
  void reportInternalError
  (const std::string& errorMsg, const char* file, unsigned int lineNumber);

  template<class Exception>
  void throwError(const std::string& errorMsg) {
    throw Exception("ERROR: " + errorMsg + '\n');
  }


  #define MATHIC_DEFINE_EXCEPTION(NAME) \
    class NAME##Exception : public MathicException { \
    public: \
      NAME##Exception(const std::string& str): MathicException(str) {} \
    }

  MATHIC_DEFINE_EXCEPTION(UnknownName);
  MATHIC_DEFINE_EXCEPTION(AmbiguousName);
  MATHIC_DEFINE_EXCEPTION(Unsupported);
}

#endif
