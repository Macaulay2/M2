#ifndef MATHIC_DISPLAY_GUARD
#define MATHIC_DISPLAY_GUARD

#include <string>
#include "stdinc.h"

/** @file display.h

 This file contains functions for printing strings to standard
 error. They all perform automatic line breaking suitable for printing
 to a console.
*/

namespace mathic {
  /** Display msg to standard error with automatic line breaking. If a
   automatically broken line begins with whitespace, that whitespace is
   repeated in front of every line that is generated from breaking
   it.

   @param prepend Print this in front of every line that is printed.
  */
  void display(const std::string& msg, const std::string& prepend = "");

  /** Display msg to standard error in a way that indicates that this is
   something that the user should take note of but that is not an
   error. */
  void displayNote(const std::string& msg);

  /** Display msg to standard error in a way that indicates that it is
   an error. */
  void displayError(const std::string& msg);

  /** Display msg to standard in a way that indicates that it is an
   internal error. */
  void displayInternalError(const std::string& errorMsg);

  /** Display the message of exception. */
  void displayException(const std::exception& exception);
}

#endif
