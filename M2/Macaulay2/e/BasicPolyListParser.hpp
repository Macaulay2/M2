/** 
* This class implements parsing of polynomials from a string or file
* as well as Msolve format.
*/
#ifndef M2_BASICPOLYLISTPARSER_HPP
#define M2_BASICPOLYLISTPARSER_HPP

#include <string>
#include <vector>

#include "BasicPolyList.hpp"

/**
 * Parses polynomials from string in the Msolve format. Msolve's format
 * includes headers for variables, a characteristic, and a list of polynomials.
 * See the 
 * <a href="https://msolve.lip6.fr/downloads/msolve-tutorial.pdf">msolve docs</a>
 * for more information information.
 *
 * \throws parsing_error
 */
BasicPolyList parseMsolveFromString(std::string contents); 
/**
* Reads the contents of the file at `filename` to a string and then calls 
* parseMsolveFromString.
 * \throws parsing_error
*/
BasicPolyList parseMsolveFile(std::string filename);

/**
 * \throws parsing_error
*/
BasicPolyList parseBasicPolyListFromString(std::string contents, std::vector<std::string> varnames);

#endif
// Local Variables:
// indent-tabs-mode: nil
// End:
