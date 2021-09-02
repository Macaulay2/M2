// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "LogDomainSet.hpp"

#include "mathic/mathic.h"

MATHICGB_NAMESPACE_BEGIN

LogDomainSet::LogDomainSet():
  mStartTime(mgb::mtbb::tick_count::now()) {
}

void LogDomainSet::registerLogDomain(LogDomain<true>& domain) {
  MATHICGB_ASSERT(std::strcmp(domain.name(), "none") != 0);
  MATHICGB_ASSERT(std::strcmp(domain.name(), "all") != 0);
  mLogDomains.push_back(&domain);
}

LogDomain<true>* LogDomainSet::logDomain(const char* const name) {
  const auto func = [&](const LogDomain<true>* const ld){
    return std::strcmp(ld->name(), name) == 0;
  };
  const auto it = std::find_if(mLogDomains.begin(), mLogDomains.end(), func);
  return it == mLogDomains.end() ? static_cast<LogDomain<true>*>(0) : *it;
}

const char* LogDomainSet::alias(const char* name) {
  const auto func = [&](const std::pair<const char*, const char*> p){
    return std::strcmp(p.first, name) == 0;
  };
  const auto it = std::find_if(mAliases.begin(), mAliases.end(), func);
  return it == mAliases.end() ? static_cast<const char*>(0) : it->second;
}

void LogDomainSet::registerLogAlias(const char* alias, const char* of) {
  MATHICGB_ASSERT(this->alias(alias) == 0);
  mAliases.push_back(std::make_pair(alias, of));
}

void LogDomainSet::performLogCommandsInternal(
  const char prefix,
  const std::string& cmds,
  const char suffix
) {
  size_t offset = 0;
  while (offset < cmds.size()) {
    const size_t next = cmds.find(',', offset);
    performLogCommandInternal
      (prefix, cmds.substr(offset, next - offset), suffix);
    offset = next;
    if (offset < cmds.size()) {
      MATHICGB_ASSERT(cmds[offset] == ',');
      ++offset;
    }
  }
}

void LogDomainSet::performLogCommandInternal(
  char prefix,
  std::string cmd,
  char suffix
) {
  const auto isSign =
    [](const char c) {return c == '+' || c == '-' || c == '0';};
  MATHICGB_ASSERT(prefix == ' ' || isSign(prefix));
  MATHICGB_ASSERT(suffix == ' ' || isSign(suffix));

  if (cmd.empty())
    return;

  // This could be more efficient, but this is not supposed to be a
  // method that is called very often.

  if (isSign(cmd[0])) {
    if (prefix == ' ')
      prefix = cmd[0];
    cmd.erase(cmd.begin());
  }

  if (!cmd.empty() && isSign(*cmd.rbegin())) {
    if (suffix == ' ')
      suffix = *cmd.rbegin();
    cmd.erase(cmd.end() - 1);
  }

  if (cmd == "none")
    return;

  if (cmd == "all") {
    for (auto it = mLogDomains.begin(); it != mLogDomains.end(); ++it)
      performLogCommandInternal(prefix, (*it)->name(), suffix);
    return;
  }

  auto aliasOf = alias(cmd.c_str());
  if (aliasOf != 0) {
    performLogCommandsInternal(prefix, aliasOf, suffix);
    return;
  }

  // The default modifiers are +X0.
  if (prefix == ' ')
    prefix = '+';
  if (suffix == ' ')
    suffix = '0';
  auto log = logDomain(cmd.c_str());
  if (log != 0) {
    if (prefix != '0')
      log->setEnabled(prefix != '-');
    if (suffix != '0')
      log->setStreamEnabled(suffix == '+');
    return;
  }

  mathic::reportError("Unknown log \"" + cmd + "\".\n");
}

void LogDomainSet::printReport(std::ostream& out) const {
  printCountReport(out);
  printTimeReport(out);
}

void LogDomainSet::printCountReport(std::ostream& out) const {
  mathic::ColumnPrinter pr;
  auto& names = pr.addColumn(true);
  auto& counts = pr.addColumn(false);

  names << "Log name  \n";
  counts << "  Count\n";
  pr.repeatToEndOfLine('-');

  bool somethingToReport = false;
  const auto end = logDomains().cend();
  for (auto it = logDomains().cbegin(); it != end; ++it) {
    const auto& log = **it;
    if (!log.enabled() || !log.hasCount())
      continue;
    somethingToReport = true;

    names << log.name() << "  \n";
    counts << "  " << mathic::ColumnPrinter::commafy(log.count()) << '\n';
  }
  if (!somethingToReport)
    return;

  out << "***** Logging count report *****\n\n" << pr << '\n';
}

void LogDomainSet::printTimeReport(std::ostream& out) const {
  const auto allTime = (mgb::mtbb::tick_count::now() - mStartTime).seconds();

  mathic::ColumnPrinter pr;
  auto& names = pr.addColumn(true);
  auto& times = pr.addColumn(false);
  auto& ratios = pr.addColumn(false);
  times.precision(3);
  times << std::fixed;
  ratios.precision(3);
  ratios << std::fixed;

  names << "Log name  \n";
  times << "  Time/s (real)\n";
  ratios << "  Ratio\n";
  pr.repeatToEndOfLine('-');

  double timeSum = 0;
  bool somethingToReport = false;
  const auto end = logDomains().cend();
  for (auto it = logDomains().cbegin(); it != end; ++it) {
    const auto& log = **it;
    if (!log.enabled() || !log.hasTime())
      continue;
    somethingToReport = true;

    const auto logTime = log.loggedSecondsReal();
    timeSum += logTime;
    names << log.name() << "  \n";
    times << logTime << '\n';
    ratios << mathic::ColumnPrinter::percentDouble(logTime, allTime) << '\n';
  }
  if (!somethingToReport)
    return;
  pr.repeatToEndOfLine('-');
  names << "sum\n";
  times << timeSum;
  ratios << mathic::ColumnPrinter::percentDouble(timeSum, allTime) << '\n';

  const auto oldFlags = out.flags();
  const auto oldPrecision = out.precision();
  out << std::fixed;
  out.precision(3);
  out << "***** Logging time report *****\nTime elapsed: "
    << allTime << "s\n\n" << pr << '\n';

  // todo: restore the stream state using RAII, since the above code might
  // throw an exception.
  out.precision(oldPrecision);
  out.flags(oldFlags);
}

void LogDomainSet::reset() {
  mStartTime = mgb::mtbb::tick_count::now();
  const auto end = logDomains().cend();
  for (auto it = logDomains().cbegin(); it != end; ++it) {
    MATHICGB_ASSERT(*it != 0);
    (*it)->reset();
  }
}

LogDomainSet& LogDomainSet::singleton() {
  static LogDomainSet set;
  return set;
}

MATHICGB_NAMESPACE_END
