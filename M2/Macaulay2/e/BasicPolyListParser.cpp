#include "BasicPolyListParser.hpp"

#include <iostream>
#include <fstream>

std::string readEntireFile(const std::string &fileName)
{
  std::ifstream ifs(fileName.c_str(), std::ios::in | std::ios::binary | std::ios::ate);

  std::ifstream::pos_type fileSize = ifs.tellg();
  ifs.seekg(0, std::ios::beg);

  std::vector<char> bytes(fileSize);
  ifs.read(bytes.data(), fileSize);

  return std::string(bytes.data(), fileSize);
}

// TODO: also remove initial white space on the line.
std::string_view next_line(std::string_view& str)
// modifies str (removes first line and \n\r chars), and returns first line as a string
{
  long last = -1;
  long i = 0;
  for (; i < str.size(); ++i)
    {
      char c = str[i];
      if (c == '\r' or c == '\n')
        {
          last = i;
          break;
        }
    }
  std::string_view result = str.substr(0, last);
  str = str.substr(last+1);
  return result;
}


// TODO: check for overflow
// TODO: make a readCoefficient function
//  it should be able to read arbitrary precision ints too...
//  also maybe a set of variables for the coefficient ring
//  and allow e.g.: (3*a+2)*x^2*y^3
long readInteger(const std::string_view& str, size_t& begin_loc, size_t end_loc)
{
  // if str[0] is a digit, find the value, and increment str past the number.
  // if it is not: return 1, leave str unchanged.
  if (not isdigit(str[begin_loc])) return 1;
  long result = 0;
  size_t loc = begin_loc;
  while (loc < end_loc and isdigit(str[loc]))
    {
      result = 10 * result + (str[loc] - '0');
      loc++;
    }

  begin_loc = loc;
  return result;
}

int readIdentifier(const std::string_view& str, const IdentifierHash& map, size_t& begin_loc, size_t end_loc)
{
  // if str[0] is a character, find the identifier, and increment str past that,
  if (begin_loc >= end_loc or not isalpha(str[begin_loc])) return -1; // TODO: throw an error here?
  size_t loc = begin_loc;
  while (loc < end_loc and (isdigit(str[loc]) or isalpha(str[loc]) or str[loc] == '_'))
    {
      loc++;
    }
  std::string_view iden = str.substr(begin_loc, loc-begin_loc);
  begin_loc = loc;
  return map.find(iden);
}

std::vector<std::string> readIdentifierList(const std::string_view line)
{
  std::vector<std::string> result;
  for (auto i = 0; i < line.size(); ++i)
    {
      char c = line[i];
      if (not isalpha(c)) continue; // possibly should give an error if we see a number? or non-identifieer start char?
      auto loc = i+1;
      while (loc < line.size() and (isalpha(line[loc]) or isdigit(line[loc]) or line[loc] == '_'))
        {
          loc++;
        }
      std::string iden {line.substr(i, loc-i)};
      result.push_back(iden);
    }
  return result;
}

// This function reads in a polynomial with (signed?) integer coefficients (but limited to size 2^31-1)
// A parse error results in a thrown error with an indication of the error (and which line, character in the string).
// TODO: throwing the error: not done
// TODO: keep track of line, positioninline.
// TODO: check for errors!
// TODO: ignore white space
void parseBasicPoly(const std::string_view& str, const IdentifierHash& idenHash, BasicPoly& result)
{
  size_t begin_loc = 0;
  size_t end_loc = str.size();

  result.mCoefficients.clear();
  result.mMonomials.clear();

  if (end_loc > begin_loc and str[begin_loc] == '[')
    {
      ++begin_loc;
    }
  if (end_loc > begin_loc and str[end_loc-1] == ',') --end_loc;
  if (end_loc > begin_loc and str[end_loc-1] == ':') --end_loc;
  if (end_loc > begin_loc and str[end_loc-1] == ']') --end_loc;

  if (begin_loc == end_loc) return; // result is already set to 0.

  while (end_loc > begin_loc)
    {
      int sign = 1;

      // Read the next term into `result`.
      if (str[begin_loc] == '+')
        {
          ++begin_loc;
        }
      // TODO: do not want +- ...
      if (str[begin_loc] == '-')
        {
          ++begin_loc;
          sign = -1;
        }
      long coeff = readInteger(str, begin_loc, end_loc); // defaults to 1 if no integer present.

      if (sign == -1) coeff = -coeff;
      result.mCoefficients.push_back(coeff);

      // Now we read the monomial part.
      long loc = result.mMonomials.size(); // this is where the length field will go.
      result.mMonomials.push_back(1); // 1 means that the monomial is `1`.

      // We expect the first character to be an identifier char.
      // then right after that a `*` or `^'.
      // If we get to "+", or "-" or end of string: we set result.mMonomials[loc] to the correct size.
      while (end_loc > begin_loc)
        {
          char c = str[begin_loc];
          if (c == '-' or c == '+')
            break; // on to the next term
          if (c == '*')
            {
              ++begin_loc;
              if (begin_loc == end_loc)
                {
                  throw parsing_error("line ends after a *");
                  // throw an error
                }
              c = str[begin_loc];
            }

          if (not isalpha(c))
            // not well forrmed, I think.
            {
              throw parsing_error("expected an identifier at position " + std::to_string(begin_loc));
            }
          // TODO: in fact, throw an error here
          auto prev_loc = begin_loc;
          int v = readIdentifier(str, idenHash, begin_loc, end_loc);
          if (v == -1)
            {
              throw parsing_error("expected a variable name at position " + std::to_string(prev_loc));
            }
          // TODO: if the identifier is not found, throw an error.
          int e = 1;
          if (end_loc > begin_loc and str[begin_loc] == '^')
            {
              ++begin_loc;
              // if not a digit, throw an error.  Note: here we are currently assuming positive exponents.
              if (begin_loc >= end_loc or not std::isdigit(str[begin_loc]))
                {
                  throw parsing_error("expected a digit at position " + std::to_string(begin_loc));
                }
              e = readInteger(str, begin_loc, end_loc);
            }
          // if exponent is zero, don't add anything to monomial.
          if (e != 0)
            {
              result.mMonomials.push_back(v);
              result.mMonomials.push_back(e);
              result.mMonomials[loc] += 2;
            }
        }
    }
}

BasicPoly parseBasicPoly(std::string poly, std::vector<std::string> varnames)
{
  std::string_view str { poly};
  IdentifierHash idenMap {varnames};
  BasicPoly result;
  parseBasicPoly(str, idenMap, result);
  return result;
}

BasicPolyList parseBasicPolyListFromString(std::string contents, const IdentifierHash& idenMap)
// The string should contain a polynomial per line, although lines starting with # are ignored.
{
  std::string_view fileView { contents };
  BasicPolyList Fs;
  while (fileView.size() > 0)
    {
      std::string_view thisline = next_line(fileView);

      if (thisline.size() == 0 or thisline[0] == '#')
        {
          continue;
        }

      BasicPoly F;
      parseBasicPoly(thisline, idenMap, F);
      Fs.push_back(F);
    }
  return Fs;
}

BasicPolyList parseBasicPolyListFromString(std::string contents, std::vector<std::string> varnames)
// The string should contain a polynomial per line, although lines starting with # are ignored.
{
  IdentifierHash idenMap {varnames};
  return parseBasicPolyListFromString(contents, idenMap);
}

///////////////////////////////
// MSolve specific functions //
///////////////////////////////
bool lineContainsVars(std::string_view& line) // if returns true, line now contains the part of the line with the variable names.
{
  std::string varHeader {"#for variable order "};
  if (line.compare(0, varHeader.size(), varHeader) != 0)
    return false;
  line.remove_prefix(varHeader.size());
  return true;
}

BasicPolyList parseMsolveFromString(std::string contents)
{
  // Read in file
  // Read in enough of the header to get identifiers

  IdentifierHash idenMap;
  std::string_view fileView { contents };

  BasicPolyList Fs;
  while (fileView.size() > 0)
    {
      std::string_view thisline = next_line(fileView);

      if (lineContainsVars(thisline))
        {
          std::vector<std::string> idenList = readIdentifierList(thisline);
          idenMap = { idenList };
          continue;
        }

      if (thisline.size() == 0)
        std::cout << "oops, this isn't good?" << std::endl;
      if (thisline.size() == 0 or thisline[0] == '#')
        {
          continue;
        }

      BasicPoly F;
      parseBasicPoly(thisline, idenMap, F);
      Fs.push_back(F);
    }
  return Fs;
}

BasicPolyList parseMsolveFile(std::string filename)
{
  std::string fileContents { readEntireFile(filename) };
  return parseMsolveFromString(fileContents);
}

// TODO:
//   readMSolveHeader: returns vector of strings, characteristic, and monomial order, and number of polynomials.
//     input format: first line is list of variables
//                   second line is characteristic
//                   what about monomial order? (is that done in file or on command line?)
//   parsing: should give errors, not infinite loops!

// BasicPolyList: should have a memoryUsed function.
// Monoid: should return std::vector<std::string> of variable names.
//

// TODO: read sparse matrix, first line is `#rows #cols`, each line is of the form e.g. `0 5 2*x^2*y^2-3*x*y`
//  how to end it?
// TODO: read dense matrix, first line is `#rows #cols`, each line is of the form e.g. `2*x^2*y^2-3*x*y`

// Local Variables:
// indent-tabs-mode: nil
// End:
