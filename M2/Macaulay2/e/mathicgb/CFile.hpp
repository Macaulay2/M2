// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_C_FILE_GUARD
#define MATHICGB_C_FILE_GUARD

#include <string>
#include <cstdio>

MATHICGB_NAMESPACE_BEGIN

/// RAII handle for a C FILE*.
///
/// The purpose of using the C IO interface instead of iostreams is that the
/// former is faster to a ridiculous degree. This class wraps the C IO
/// interface to be more useful in a C++ context. For example the file is
/// automatically closed in the destructor and if the file cannot be opened
/// then an exception is thrown instead of returning a null pointer.
class CFile {
public:
  struct NoThrowTag {};

  /// Sets the handle to null if the file cannot be opened - does not
  /// throw an exception. The purpose of the NoTrowTag parameter is only
  /// to indicate that no exception should be thrown on error.
  CFile(const std::string& fileName, const char* mode, NoThrowTag);

  /// Opens the file and throws an exception if the file cannot be opened.
  CFile(const std::string& fileName, const char* mode);

  ~CFile();

  bool hasFile() const {return mFile != 0;}

  FILE* handle() {return mFile;}
  void close();

private:
  FILE* mFile;
};

MATHICGB_NAMESPACE_END
#endif
