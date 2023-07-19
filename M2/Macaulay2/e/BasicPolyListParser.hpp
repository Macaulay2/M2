// This class implements parsing of polynomials from a string or file
// as well as Msolve format.
#pragma once

#include <iostream>
#include <string>
#include <sstream>
#include <fstream>
#include <vector>

#include "BasicPolyList.hpp"

BasicPolyList parseMsolveFromString(std::string contents); // requires Msolve header to determine variables, etc.
BasicPolyList parseMsolveFile(std::string filename);

BasicPolyList parseBasicPolyListFromString(std::string contents, std::vector<std::string> varnames);

// Local Variables:
// indent-tabs-mode: nil
// End:
