#include "Timer.h"

namespace mathic {
  unsigned long Timer::getMilliseconds() const {
    const double floatSpan = clock() - _clocksAtReset;
    const double floatMilliseconds = 1000 * (floatSpan / CLOCKS_PER_SEC);
    unsigned long milliseconds = static_cast<unsigned long>(floatMilliseconds);
    if (floatMilliseconds - milliseconds >= 0.5)
      ++milliseconds;
    return milliseconds;
  }

  void Timer::print(FILE* out) const {
    unsigned long milliseconds = getMilliseconds();
    unsigned long seconds = milliseconds / 1000;
    unsigned long minutes = seconds / 60;
    unsigned long hours = minutes / 60;

    milliseconds %= 1000;
    seconds %= 60;
    minutes %= 60;

    fputc('(', out);
    if (hours != 0)
      fprintf(out, "%luh", hours);
    if (minutes != 0 || hours != 0)
      fprintf(out, "%lum", minutes);
    fprintf(out, "%lu.%03lus)", seconds, milliseconds);
  }

  void Timer::print(std::ostream& out) const {
    unsigned long milliseconds = getMilliseconds();
    unsigned long seconds = milliseconds / 1000;
    unsigned long minutes = seconds / 60;
    unsigned long hours = minutes / 60;

    milliseconds %= 1000;
    seconds %= 60;
    minutes %= 60;

    if (hours != 0)
      out << hours << 'h';
    if (minutes != 0 || hours != 0)
      out << minutes << 'm';
    out << seconds << '.';
    out << (milliseconds / 100);
    out << ((milliseconds / 10) % 10);
    out << (milliseconds % 10);
    out << "s";
  }
}
