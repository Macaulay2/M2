// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "LogDomain.hpp"

#include "LogDomainSet.hpp"
#include "mathic/mathic.h"
#include <iostream>

MATHICGB_NAMESPACE_BEGIN

static const auto logDomainGlobalStartTime = mgb::mtbb::tick_count::now();

LogDomain<true>::LogDomain(
  const char* const name,
  const char* const description,
  const bool enabled,
  const bool streamEnabled
):
  mEnabled(enabled),
  mOriginallyEnabled(enabled),
  mStreamEnabled(streamEnabled),
  mOriginallyStreamEnabled(streamEnabled),
  mName(name),
  mDescription(description),
  mInterval(),
  mHasTime(false),
  mCount(0),
  mHasCount(false)
{
  LogDomainSet::singleton().registerLogDomain(*this);
}

void LogDomain<true>::reset() {
  mEnabled = mOriginallyEnabled;
  mStreamEnabled = mOriginallyStreamEnabled;
  mInterval = TimeInterval();
  mHasTime = false;
  mCount = 0;
  mHasCount = false;
}

std::ostream& LogDomain<true>::stream() {
  return std::cerr;
}

LogDomain<true>::Timer LogDomain<true>::timer() {
  return Timer(*this);
}

double LogDomain<true>::loggedSecondsReal() const {
  return mInterval.realSeconds;
}

void LogDomain<true>::TimeInterval::print(std::ostream& out) const {
  const auto oldFlags = out.flags();
  const auto oldPrecision = out.precision();
  out.precision(3);
  out << std::fixed << realSeconds << "s (real)";
  // todo: restore the stream state using RAII, since the above code might
  // throw an exception.
  out.precision(oldPrecision);
  out.flags(oldFlags);
}

void LogDomain<true>::recordTime(TimeInterval interval) {
  if (!enabled())
    return;
  mInterval.realSeconds += interval.realSeconds;
  mHasTime = true;

  if (streamEnabled()) {
    MATHICGB_ASSERT(mName != 0);
    stream() << mName << " time recorded:        ";
    interval.print(stream());
    stream() << std::endl;
  }
}

LogDomain<true>::Timer::Timer(LogDomain<true>& logger):
  mLogger(logger),
  mTimerRunning(false),
  mRealTicks()
{
  start();
}

LogDomain<true>::Timer::~Timer() {
  stop();
}

void LogDomain<true>::Timer::stop() {
  if (!running())
    return;
  mTimerRunning = false;
  if (!mLogger.enabled())
    return;
  TimeInterval interval;
  interval.realSeconds = (mgb::mtbb::tick_count::now() - mRealTicks).seconds();
  mLogger.recordTime(interval);
  return;
}

void LogDomain<true>::Timer::start() {
  if (!mLogger.enabled() || mTimerRunning)
    return;
  mTimerRunning = true;
  mRealTicks = mgb::mtbb::tick_count::now();
}

LogDomainInternal::LogAliasRegisterer::LogAliasRegisterer(const char* alias, const char* of) {
  LogDomainSet::singleton().registerLogAlias(alias, of);
}

MATHICGB_NAMESPACE_END
