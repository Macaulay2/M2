// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "CFile.hpp"

#include "mathic/mathic.h"
#include <sstream>

MATHICGB_NAMESPACE_BEGIN

CFile::CFile(const std::string& fileName, const char* mode, NoThrowTag):
  mFile(fopen(fileName.c_str(), mode)
) {}

CFile::CFile(const std::string& fileName, const char* mode):
  mFile(fopen(fileName.c_str(), mode)
) {
  if (mFile == 0) {
    std::ostringstream error;
    error << "Could not open file " << fileName << " in mode " << mode << '.';
    mathic::reportError(error.str());
  }
}

CFile::~CFile() {
  close();
}

void CFile::close() {
  if (mFile != 0)
    fclose(mFile);
}

MATHICGB_NAMESPACE_END
