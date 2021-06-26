#ifndef MATHIC_COLUMN_PRINTER_GUARD
#define MATHIC_COLUMN_PRINTER_GUARD

#include "stdinc.h"
#include <sstream>
#include <vector>
#include <cstdio>
#include <memory>
#include <string>

namespace mathic {
  class ColumnPrinter {
  public:
	ColumnPrinter(size_t columnCount = 0);

	void setPrefix(const std::string& prefix);
	std::ostream& addColumn(
      bool flushLeft = true,
	  const std::string& prefix = "  ",
	  const std::string& suffix = ""
    );
	size_t getColumnCount() const;

	std::ostream& operator[](size_t col);

    /// Inserts a newline after padding the current line with repeatThis 
    /// chars until it fills the column width.
    void repeatToEndOfLine(const char repeatThis, size_t col);

    /// Works on all columns.
    void repeatToEndOfLine(const char repeatThis);

	void print(std::ostream& out) const;

    /// Returns "123,456,789" for parameter value 123456789.
    static std::string commafy(unsigned long long l);

    /// Returns "123.4G" for parameter value 123456789. So the SI prefix is a
    /// suffix of the returned string, yet it is still called an SI *prefix*
    /// because these are usually used as prefixes to units such as in Km.
    static std::string withSIPrefix(unsigned long long l);

    /** returns (3,100) as "3.0%". */
    static std::string percentInteger(
      unsigned long long numerator,
      unsigned long long denominator
    );
    static std::string percentDouble(double numerator, double denominator);

    /** returns (3,100) as "  3.0%". The string always has the same length for
      ratios equal to or less than 999.9%. */
    static std::string percentIntegerFixed(
      unsigned long long numerator,
      unsigned long long denominator);

    /** returns 0.7565 as "75.7%". */
    static std::string percentDouble(double ratio);

    /** Returns (7,4) as "1.8" */
    static std::string ratioInteger(
      unsigned long long numerator,
      unsigned long long denominator
    );
    static std::string ratioDouble(double numerator, double denominator);

	/** Returns d as a string printed to 1 decimal place, rounding up at 0.5 */
    static std::string oneDecimal(double d);

	/** Prints as X bytes, X kilobytes, X megabytes etc. */
    static std::string bytesInUnit(unsigned long long bytes);

  private:
	struct Col {
	  std::string prefix;
	  std::stringstream text;
	  std::string suffix;
	  bool flushLeft;
      std::vector<std::pair<size_t, char> > repeatToEndOfLine;
	};
	std::vector<std::unique_ptr<Col>> _cols;
	std::string _prefix;
  };

  std::ostream& operator<<(std::ostream& out, const ColumnPrinter& printer);
  void print(FILE* out, const ColumnPrinter& pr);
}

#endif
