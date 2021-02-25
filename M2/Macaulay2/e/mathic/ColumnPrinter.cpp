#include "ColumnPrinter.h"

namespace mathic {
  namespace {
	size_t getLineWidth(const std::string& str, size_t pos) {
	  size_t startPos = pos;
	  while (pos < str.size() && str[pos] != '\n' && str[pos] != '\0')
		++pos;
	  return pos - startPos;
	}

	void printChars(std::ostream& out, size_t howMany, const char c) {
	  while (howMany > 0) {
		out << c;
		--howMany;
	  }
	}
  }

  ColumnPrinter::ColumnPrinter(size_t columnCount):
	_cols() {
	while (columnCount > 0) {
	  addColumn();
	  --columnCount;
	}
  }

  void ColumnPrinter::setPrefix(const std::string& prefix) {
	_prefix = prefix;
  }

  std::ostream& ColumnPrinter::addColumn(bool flushLeft,
								const std::string& prefix,
								const std::string& suffix) {
	std::unique_ptr<Col> col(new Col());
	col->prefix = prefix;
	col->suffix = suffix;
	col->flushLeft = flushLeft;

	_cols.emplace_back(std::move(col));

    return _cols.back()->text;
  }

  size_t ColumnPrinter::getColumnCount() const {
	return _cols.size();
  }

  std::ostream& ColumnPrinter::operator[](size_t col) {
	MATHIC_ASSERT(col < getColumnCount());
	return _cols[col]->text;
  }

  void ColumnPrinter::repeatToEndOfLine(const char repeatThis, size_t col) {
    MATHIC_ASSERT(col < getColumnCount());
    (*this)[col] << '\0' << repeatThis;
  }

  void ColumnPrinter::repeatToEndOfLine(const char repeatThis) {
    MATHIC_ASSERT(repeatThis != '\n');
    for (size_t col = 0; col < getColumnCount(); ++col)
      repeatToEndOfLine(repeatThis, col);
  }

  void ColumnPrinter::print(std::ostream& out) const {
    // stringstream::str() copies the string, so we need
    // to extract all the strings and store them to avoid copying
    // at every access.
    std::vector<std::string> texts(getColumnCount());
	for (size_t col = 0; col < getColumnCount(); ++col)
      texts[col] = _cols[col]->text.str();
    
    // Calculate the width of each column.
	std::vector<size_t> widths(getColumnCount());
	for (size_t col = 0; col < getColumnCount(); ++col) {
      const auto& text = texts[col];
	  size_t maxWidth = 0;

	  size_t pos = 0;
	  while (pos < text.size()) {
		size_t width = getLineWidth(text, pos);
		if (width > maxWidth)
		  maxWidth = width;

		// We can't just increment pos unconditionally by width + 1, as
		// that could result in an overflow.
		pos += width;
        if (pos == text.size())
          break;
        if (text[pos] == '\0') {
          ++pos;
          if (pos == text.size())
            break;
          ++pos;
        } else if (text[pos] == '\n')
		  ++pos;
	  }
	  widths[col] = maxWidth;
	}

	// Print each row
	std::vector<size_t> poses(getColumnCount());
	while (true) {
	  bool done = true;
	  for (size_t col = 0; col < getColumnCount(); ++col) {
		if (poses[col] < texts[col].size()) {
		  done = false;
		  break;
		}
	  }
	  if (done)
		break;

	  out << _prefix;
	  for (size_t col = 0; col < getColumnCount(); ++col) {
		out << _cols[col]->prefix;

		const std::string& text = texts[col];
		size_t& pos = poses[col];
		size_t width = getLineWidth(text, pos);

        char padChar = ' ';
        if (
          pos + width < text.size() &&
          text[pos + width] == '\0' &&
          pos + width + 1 < text.size()
        ) {
          padChar = text[pos + width + 1];
        }

		if (!_cols[col]->flushLeft)
		  printChars(out, widths[col] - width, padChar);

		while (pos < text.size()) {
		  if (text[pos] == '\n') {
			++pos;
			break;
		  }
          if (text[pos] == '\0') {
            ++pos;
            if (pos < text.size()) {
              MATHIC_ASSERT(text[pos] == padChar);
              ++pos;
            }
            break;
          }
		  out << text[pos];
		  ++pos;
		}

		if (_cols[col]->flushLeft)
		  printChars(out, widths[col] - width, padChar);

		out << _cols[col]->suffix;
	  }
	  out << '\n';
	}
  }

  std::string ColumnPrinter::commafy(const unsigned long long l) {
    std::stringstream out;
    out << l;
    const auto uncomma = out.str();
    std::string str;
    for (size_t i = 0; i < uncomma.size(); ++i) {
      str += uncomma[i];
      if (i != uncomma.size() - 1 && ((uncomma.size() - i) % 3) == 1)
        str += ',';
    }
    return str;
  }

  std::string ColumnPrinter::percentInteger(
    const unsigned long long numerator,
    const unsigned long long denominator
  ) {
    return ratioInteger(numerator * 100, denominator) + '%';
  }

  std::string ColumnPrinter::percentDouble(
    const double numerator,
    const double denominator
  ) {
    return ratioDouble(numerator * 100, denominator) + '%';
  }

  std::string ColumnPrinter::percentIntegerFixed(
    const unsigned long long numerator,
    const unsigned long long denominator
  ) {
    auto str = percentInteger(numerator, denominator);
    const size_t maxSize = 6;
    MATHIC_ASSERT(maxSize == std::string("100.0%").size());
    const auto size = str.size();
    if (size > maxSize)
      return str;
    return std::string(maxSize - str.size(), ' ') + std::move(str);
  }

  std::string ColumnPrinter::percentDouble(const double ratio) {
    return oneDecimal(ratio * 100) + '%';
  }

  std::string ColumnPrinter::ratioInteger(
    const unsigned long long numerator,
    const unsigned long long denominator
  ) {
    if (denominator == 0)
      return std::string(numerator == 0 ? "0/0" : "?/0");
    return oneDecimal(static_cast<double>(numerator) / denominator);
  }

  std::string ColumnPrinter::ratioDouble(
    const double numerator,
    const double denominator
  ) {
    const auto epsilon = 0.000000001;
    if (-epsilon < denominator && denominator < epsilon) {
      if (-epsilon < numerator && numerator < epsilon)
        return "0/0";
      else
        return "?/0";
    }
    return oneDecimal(static_cast<double>(numerator) / denominator);
  }

  std::string ColumnPrinter::oneDecimal(const double d) {
    std::ostringstream out;
    unsigned long long l = static_cast<unsigned long long>(d * 10 + 0.5);
    out << l / 10 << '.' << l % 10;
	return out.str();
  }

  std::string ColumnPrinter::withSIPrefix(unsigned long long l) {
	std::ostringstream out;
	if (l < 1000)
      out << l;
    else {
	  const char* const units[] = {"k", "M", "G", "T"};
	  const size_t unitCount = sizeof(units) / sizeof(*units);
	  double amount = static_cast<double>(l) / 1000.0;
	  size_t i = 0;
      // the stop condition is at 999.5 because that value will get
      // rounded to 1000.0.
	  for (i = 0; i < unitCount && amount >= 999.95; ++i)
        amount /= 1000.0;
	  out << oneDecimal(amount) << units[i];
    }
    return out.str();
  }

  std::string ColumnPrinter::bytesInUnit(const unsigned long long bytes) {
	std::ostringstream out;
	if (bytes < 1024)
	  out << bytes << 'B';
    else {
	  const char* const units[] = {"kB", "MB", "GB", "TB"};
	  const size_t unitCount = sizeof(units) / sizeof(*units);
	  double amount = static_cast<double>(bytes) / 1024.0;
	  size_t i = 0;
      // the stop condition is 1023.95 because that value will get
      // rounded to 1024.0.
	  for (i = 0; i < unitCount && amount >= 1023.95; ++i)
        amount /= 1024.0;
	  out << oneDecimal(amount) << units[i];
    }
    return out.str();
  }


  std::ostream& operator<<(
    std::ostream& out,
    const ColumnPrinter& printer
  ) {
	printer.print(out);
	return out;
  }

  void print(FILE* out, const ColumnPrinter& pr) {
	std::ostringstream str;
	str << pr;
	fputs(str.str().c_str(), out);
  }
}
