#include "display.h"

#include <cctype>
#include <cstdio>

namespace mathic {
  namespace {
    /** Automatically break lines at this width. */
    static const size_t ConsoleWidth = 79;

    /** Helper class for display(). */
    class Printer {
    public:
      Printer(const std::string& msg, const std::string& prepend):
        _pos(0), _lineSize(0), _msg(msg), _prefix(prepend) {

	    std::string wordSpacePrefix;

        while (_pos < _msg.size()) {
          // We are always at the start of a line at this point.
		  MATHIC_ASSERT(_lineSize == 0);
          readIndentation();
		  printRaw(_prefix);
		  printRaw(_indentation);

          if (_pos == _msg.size())
            break;
          if (_msg[_pos] == '\n') {
            newLine();
            ++_pos;
            continue;
          }

		  wordSpacePrefix.clear();
          while (_pos < _msg.size()) {
            if (_msg[_pos] == '\n') {
              ++_pos;
              break;
            }
            if (isspace(_msg[_pos])) {
			  wordSpacePrefix += _msg[_pos];
			  ++_pos;
              continue;
            }
            MATHIC_ASSERT(!isspace(_msg[_pos]));
            MATHIC_ASSERT(_msg[_pos] != '\n');
            MATHIC_ASSERT(_pos < _msg.size());

            std::string word;
            while (_pos < _msg.size() &&
				   _msg[_pos] != '\n' &&
				   !isspace(_msg[_pos])) {
              word += _msg[_pos];
              ++_pos;
            }
            MATHIC_ASSERT(!word.empty());
            printWord(wordSpacePrefix, word);
		    wordSpacePrefix.clear();
          }

		  newLine();
        }
      }

    private:
      void newLine() {
        printRaw('\n');
        _lineSize = 0;
      }

      void readIndentation() {
        // Read whitespace at beginning of line.
        _indentation.clear();
        while (_pos < _msg.size() && _msg[_pos] != '\n' && isspace(_msg[_pos])) {
          _indentation += _msg[_pos];
          ++_pos;
        }
      }

      void printWord(const std::string& wordSpacePrefix, const std::string& word) {
        MATHIC_ASSERT(!word.empty());

        // Note that this will print beyond the console width if word is
        // the first thing we are printing on this line. That is because
        // there then is no way to fit the word on one line.
	    size_t wordAndPrefixSize = word.size() + wordSpacePrefix.size();
	    if (_lineSize != 0 && _lineSize + wordAndPrefixSize  > ConsoleWidth) {
		  // we skip space before word if inserting newline
		  newLine();
		  printRaw(_prefix);
		  printRaw(_indentation);
	    } else
		  printRaw(wordSpacePrefix);
        printRaw(word);
      }

      void printRaw(const std::string& word) {
        fputs(word.c_str(), stderr);
        _lineSize += word.size();
      }

      void printRaw(char c) {
        fputc(c, stderr);
        ++_lineSize;
      }

      size_t _pos;
      size_t _lineSize;
      const std::string& _msg;
      const std::string& _prefix;
      std::string _indentation;
    };
  }

  void display(const std::string& msg, const std::string& prepend) {
    Printer(msg, prepend);
  }

  void displayNote(const std::string& msg) {
    display("NOTE: " + msg + "\n");
  }

  void displayError(const std::string& msg) {
    display("ERROR: " + msg + "\n");
  }

  void displayInternalError(const std::string& msg) {
    display("INTERNAL ERROR: " + msg + "\n");
  }

  void displayException(const std::exception& exception) {
    try {
      display(exception.what());
    } catch (...) {
      fputs("\n\n*** Error while printing error! ***\n", stderr);
      fflush(stderr);
      fputs("*** Retrying display of error using simpler display method. ***\n",
            stderr);
      fflush(stderr);
      fputs(exception.what(), stderr);
      fflush(stderr);
      throw;
    }
  }
}
