// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_LOG_DOMAIN_GUARD
#define MATHICGB_LOG_DOMAIN_GUARD

#include "mtbb.hpp"
#include <ostream>
#include <ctime>
#include <sstream>

MATHICGB_NAMESPACE_BEGIN

/// A named area of logging that can be turned on or off at runtime and at
/// compile time.
///
/// A logger that is turned off at compile time emits no code
/// into the executable and all the code that writes to that logger is also
/// removed by the optimizer if it is written in the correct way. Use the
/// logging macroes to ensure proper use so that compile-time disabled
/// LogDomains properly have zero overhead. LogDomains can be turned on
/// and off at compile time and at runtime individually.
///
/// Compile-time enabled loggers automatically register themselves at start-up
/// with LogDomainSet::singleton().
///
/// @todo: support turning all loggers off globally with a macro, regardless
/// of their individual compile-time on/off setting.

template<bool CompileTimeEnabled>
class LogDomain {};

template<>
class LogDomain<true> {
public:
  static const bool compileTimeEnabled = true;

  LogDomain(
    const char* const name,
    const char* const description,
    const bool enabled,
    const bool streamEnabled
  );

  const char* name() const {return mName;}
  const char* description() const {return mDescription;}
  bool enabled() const {return mEnabled;}
  bool streamEnabledPure() const {return mStreamEnabled;}
  bool streamEnabled() const {return enabled() && streamEnabledPure();}

  void setEnabled(const bool enabled) {mEnabled = enabled;}
  void setStreamEnabled(const bool enabled) {mStreamEnabled = enabled;}

  std::ostream& stream();

  /// Class for recording time that is logged. Movable.
  class Timer;

  /// Returns a started timer.
  Timer timer();

  /// Returns true if any time has been logged on this logger, even if the
  /// duration of that time was zero (that is., less than the resolution
  /// of the timer).
  bool hasTime() const {return mHasTime;}

  double loggedSecondsReal() const;


  typedef unsigned long long Counter;

  Counter count() const {return mCount;}

  void setCount(const Counter counter) {
    if (enabled()) {
      mCount = counter;
      mHasCount = true;
    }
  }

  /// Returns true if setCount has been called.
  bool hasCount() const {return mHasCount;}

  /// Resets this object to the state it had when it was
  /// constructed.
  void reset();

private:
  struct TimeInterval {
    // todo: support user time too. clock() doesn't seem to sum the time
    // for all threads, so that didn't work.
    double realSeconds;

    void print(std::ostream& out) const;
  };
  void recordTime(TimeInterval interval);

  bool mEnabled;
  const bool mOriginallyEnabled;
  bool mStreamEnabled;
  const bool mOriginallyStreamEnabled;
  const char* mName;
  const char* mDescription;

  TimeInterval mInterval; /// Total amount of time recorded on this log.
  bool mHasTime; /// Whether any time has been registered (even if 0s).

  Counter mCount;
  bool mHasCount; /// Whether the count has been set (even if set to zero)
};

class LogDomain<true>::Timer {
public:
  /// Start the timer running. The elapsed time will be logged to the logger
  /// once the timer is stopped or destructed.
  Timer(LogDomain<true>& logger);

  /// Stops the timer.
  ~Timer();

  /// Returns true if the timer is currently recording time.
  bool running() const {return mTimerRunning;}

  /// Stops recording time and logs the elapsed time to the logger.
  ///
  /// This is a no-op if the timer is not running. If the logger
  /// is disabled then no time is logged.
  void stop();

  /// Start recording time on a stopped timer.
  ///
  /// This is a no-op if the timer is already running or if the logger is
  /// disabled.
  void start();

private:
  LogDomain<true>& mLogger;
  bool mTimerRunning;
  mtbb::tick_count mRealTicks; // high precision
};

/// This is a compile-time disabled logger. You are not supposed to dynamically
/// call any non-const methods on it other than the constructor. Code that
/// calls other code will compile but it is an error if any of those
/// methods get called at runtime.
template<>
class LogDomain<false> {
public:
  static const bool compileTimeEnabled = false;

  LogDomain(const char* const, const char* const, const bool) {}

  bool enabled() const {return false;}
  bool streamEnabled() const {return false;}

  class Timer {
  public:
    Timer(LogDomain<false>&) {}
    bool running() const {return false;}
    void stop() {MATHICGB_ASSERT(false);}
    void start() {MATHICGB_ASSERT(false);}
  };
  Timer timer() {
    MATHICGB_ASSERT(false);
    return Timer(*this);
  }

  std::ostream& stream() {
    MATHICGB_ASSERT(false);
    abort();
    //return *static_cast< std::ostream*>(0);
  }

  typedef unsigned long long Counter;
  Counter count() const {return 0;}
  void setCount(const Counter counter) {MATHICGB_ASSERT(false);}
  bool hasCount() const {return false;}
  void reset() {}
};

namespace LogDomainInternal {
  // Helpers for the logging macroes

  template<class Tag, bool Default>
  struct SelectValue {static const bool value = Default;};

  template<class> struct Tag_ {};
  template<class> struct Tag_0 {};
  template<class> struct Tag_1 {};

  template<bool Default>
  struct SelectValue<Tag_0<int>, Default> {static const bool value = false;};

  template<bool Default>
  struct SelectValue<Tag_1<int>, Default> {static const bool value = true;};

  template<class L>
  struct LambdaRunner {L& log;};
  template<class L>
  LambdaRunner<L> lambdaRunner(L& l) {
    LambdaRunner<L> r = {l};
    return r;
  }
  template<class L, class T>
  void operator+(LambdaRunner<L> runner, T&& lambda) {
    lambda(runner.log, runner.log.stream());
  }

  struct LogAliasRegisterer {
    LogAliasRegisterer(const char* alias, const char* of);
  };
}

MATHICGB_NAMESPACE_END

/// Defines LogDomainInternal::value_##NAME to be equal to the value of
/// the macro MATHICGB_LOG_##NAME if that macro expands to 0 or 1. Otherwise
/// the macro MATHICGB_LOG_##NAME is ignored and instead DEFAULT_VALUE is used.
#define MATHICGB_CAPTURE_LOG_ENABLED(NAME, DEFAULT_VALUE) \
  namespace mgb{namespace LogDomainInternal {             \
    template<class> struct Tag_MATHICGB_LOG_##NAME {}; \
    typedef MATHICGB_CONCATENATE_AFTER_EXPANSION(Tag_, MATHICGB_LOG_##NAME)<int> \
      SelectedTag_##NAME; \
    static const bool value_##NAME = \
      SelectValue<SelectedTag_##NAME, DEFAULT_VALUE>::value; \
  }}

/// Defines a LogDomain with the given name and description.
///
/// The logger is default compile-time enabled depending on MATHICGB_LOG_##NAME
/// (see MATHICGB_CAPTURE_LOG_ENABLED) and it is initially runtime
/// enabled depending on the value of DEFAULT_RUNTIME_ENABLED. It is default
/// runtime enabled for streaming (when also enabled in general) depending on
/// DEFAULT_RUNTIME_STREAM_ENABLED.
#define MATHICGB_DEFINE_LOG_DOMAIN_WITH_DEFAULTS( \
  NAME, DESCRIPTION, \
  DEFAULT_RUNTIME_ENABLED, \
  DEFAULT_RUNTIME_STREAM_ENABLED, \
  DEFAULT_COMPILE_TIME_ENABLED \
) \
  MATHICGB_CAPTURE_LOG_ENABLED(NAME, DEFAULT_COMPILE_TIME_ENABLED); \
  namespace mgb{namespace logs {                                      \
    typedef LogDomain< ::mgb::LogDomainInternal::value_##NAME> Type##NAME; \
    Type##NAME NAME( \
      #NAME, \
      DESCRIPTION, \
      DEFAULT_RUNTIME_ENABLED, \
      DEFAULT_RUNTIME_STREAM_ENABLED \
    ); \
  }}

/// Defines a LogDomain with the given name and description.
///
/// The defaults for the logger are as follows.
///       compile-time: enabled,
///            runtime: disabled,
///  runtime streaming: enabled (only takes effect if also enabled)
#define MATHICGB_DEFINE_LOG_DOMAIN(NAME, DESCRIPTION) \
  MATHICGB_DEFINE_LOG_DOMAIN_WITH_DEFAULTS(NAME, DESCRIPTION, 0, 1, 1);

#define MATHICGB_DEFINE_LOG_ALIAS(ALIAS, OF) \
  namespace mgb{namespace LogDomainInternal {                           \
    LogAliasRegisterer MATHICGB_CONCATENATE_AFTER_EXPANSION(reg_, __LINE__) \
      (ALIAS, OF); \
  }}

/// This expression yields an l-value reference to the indicated logger.
///
/// Example:
///   auto timer = MATHICGB_LOGGER(MyDomain).timer();
#define MATHICGB_LOGGER(DOMAIN) ::mgb::logs::DOMAIN

/// This expression yields the type of the indicated logger.
///
/// Example:
///   if (MATHICGB_LOGGER_TYPE(MyDomain)::compileTimeEnabled)
///     std::ostream << "MyDomain is compiled time enabled";
#define MATHICGB_LOGGER_TYPE(DOMAIN) ::mgb::logs::Type##DOMAIN

/// Runs the code in the following scope delimited by braces {} if the
/// indicated logger is enabled for streaming - otherwise does
/// nothing.  Within the following scope there is a local reference
/// variable log that refers to the indicated logger and a local
/// reference variable stream that refers to log.stream().
///
/// Example:
///   MATHICGB_IF_STREAM_LOG(MyDomain) {
///     std::string msg;
///     expensiveFunction(msg);
///     stream << msg; // or log.stream() << msg;
///   }
#define MATHICGB_IF_STREAM_LOG(DOMAIN) \
  if (MATHICGB_LOGGER(DOMAIN).streamEnabled()) \
    LogDomainInternal::lambdaRunner(MATHICGB_LOGGER(DOMAIN)) + \
      [&](MATHICGB_LOGGER_TYPE(DOMAIN)& log, std::ostream& stream)

/// Display information to the log using <<.
/// If domain is not enabled and stream enabled then the log message is not
/// displayed and the code after << is not executed.
///
/// Example: (f() only called if logger is enabled)
///   MATHICGB_LOG(domain) << "f() = " << f();
#define MATHICGB_LOG(DOMAIN) \
  if (MATHICGB_LOGGER(DOMAIN).streamEnabled()) MATHICGB_LOGGER(DOMAIN).stream()

/// Will log the time to execute the remaining code in the current scope
/// to the indicated domain. Also supports printing a message using <<.
/// The message is printed right away while the time is printed when
/// the scope ends.
///
/// Example:
///   MATHICGB_LOG_SCOPE_TIME(MyDomain) << "Starting timed task";
#define MATHICGB_LOG_TIME(DOMAIN) \
  auto MATHICGB_CONCATENATE_AFTER_EXPANSION( \
    DOMAIN, MATHICGB_CONCATENATE_AFTER_EXPANSION(_timer_, __LINE__)) \
  (MATHICGB_LOGGER(DOMAIN).timer()); \
  MATHICGB_LOG(DOMAIN)

/// Increments the count of DOMAIN by the value of the expression BY. The
/// expression BY is evaluated at most once and it is not evaluated if
/// DOMAIN is disabled.
///
/// Example:
///   MATHICGB_LOG_INCREMENT_BY(MyDomain, 3);
#define MATHICGB_LOG_INCREMENT_BY(DOMAIN, BY) \
  do { \
    auto& MGBLOG_log = MATHICGB_LOGGER(DOMAIN); \
    if (MGBLOG_log.enabled()) { \
      MGBLOG_log.setCount(MGBLOG_log.count() + BY); \
    } \
  } while (false)

/// Increments the count of DOMAIN by 1.
#define MATHICGB_LOG_INCREMENT(DOMAIN) \
  MATHICGB_LOG_INCREMENT_BY(DOMAIN, 1)

#endif
