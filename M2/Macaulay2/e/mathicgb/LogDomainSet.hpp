// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_LOG_DOMAIN_SET_GUARD
#define MATHICGB_LOG_DOMAIN_SET_GUARD

#include "LogDomain.hpp"
#include "mtbb.hpp"
#include <string>
#include <vector>
#include <algorithm>
#include <cstring>
#include <ostream>

MATHICGB_NAMESPACE_BEGIN

class LogDomainSet {
public:
  void registerLogDomain(LogDomain<true>& domain);
  void registerLogDomain(const LogDomain<false>& domain) {}

  void registerLogAlias(const char* alias, const char* of);

  /// A log command has the format AXB, where
  ///   X       the name of a compile-time enabled log domain
  ///   A       a prefix
  ///   B       a suffix
  /// The possible values of A are
  ///           enable X (this is the empty string)
  ///   +       enable X
  ///   -       disable X
  ///   0       do nothing
  /// The possible values of B are
  ///           do nothing (this is the empty string)
  ///   +       stream-enabled X
  ///   -       stream-disable X
  ///   0       do nothing
  ///
  /// No white-space is allowed.
  /// If the command cannot be parsed then you will get an exception.
  ///
  /// *** Example ***
  /// Consider this sequence of commands:
  ///   "+MyLog-" will enabled MyLog, but silence any streaming from it.
  ///   "+MyLog" will make no difference, as MyLog is already enabled.
  ///   "-MyLog+" will disable MyLog, but set the streaming state to enabled.
  ///     As MyLog is disabled there will still be no streaming output.
  ///   "+MyLog" will enabled MyLog. Since the streaming state was enabled
  ///     before, we now get streaming.
  ///
  void performLogCommand(std::string cmd)
    {performLogCommandInternal(' ', std::move(cmd), ' ');}

  /// Performs a comma-seperated list of commands. No white-space is allowed.
  void performLogCommands(const std::string& cmds)
    {performLogCommandsInternal(' ', cmds, ' ');}

  LogDomain<true>* logDomain(const char* const name);

  const char* alias(const char* name);

  const std::vector<LogDomain<true>*>& logDomains() const {return mLogDomains;}
  const std::vector<std::pair<const char*, const char*>>& aliases() const
    {return mAliases;}

  void printReport(std::ostream& out) const;
  void printTimeReport(std::ostream& out) const;
  void printCountReport(std::ostream& out) const;

  /// Resets the logging system as though the program had just started up.
  /// This resets all counts, all recorded time and the enabledness of all logs.
  /// You should not have a timer running for a log when you call this method.
  void reset();

  static LogDomainSet& singleton();

private:
  void performLogCommandInternal(
    char prefix,
    std::string name,
    char suffix
  );
  void performLogCommandsInternal(
    const char prefix,
    const std::string& cmds,
    const char suffix
  );
  LogDomainSet(); // private for singleton

  std::vector<LogDomain<true>*> mLogDomains;
  std::vector<std::pair<const char*, const char*>> mAliases;
  mgb::mtbb::tick_count mStartTime;
};

MATHICGB_NAMESPACE_END
#endif
