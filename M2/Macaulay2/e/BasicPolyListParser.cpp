#include "BasicPolyListParser.hpp"

#include <unordered_map>

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

class IdentifierHash
{
private:
  std::vector<std::string> mAllocatedStrings;
  std::unordered_map<std::string_view, int> mMap;
public:
  IdentifierHash() = default;
  IdentifierHash(std::vector<std::string>& idens)
    : mAllocatedStrings(idens),
      mMap()
  {
    for (int i=0; i<mAllocatedStrings.size(); ++i)
      {
        mMap[std::string_view(mAllocatedStrings[i])] = i;
      }
    std::cout << "#variables is " << mMap.size() << std::endl;    
  }

  auto find(std::string_view s) const -> int
  {
    auto foundloc = mMap.find(s);
    return (foundloc != mMap.end() ? foundloc->second : -1); // TODO: throw an error if not found?
  }
};

// TODO: check for overflow
long readInteger(std::string_view& str)
{
  // if str[0] is a digit, find the value, and increment str past the number.
  // if it is not: return 1, leave str unchanged.
  if (not isdigit(str[0])) return 1;
  long result = 0;
  size_t loc = 0;
  while (loc < str.size() and isdigit(str[loc]))
    {
      result = 10 * result + (str[loc] - '0');
      loc++;
    }
  str = str.substr(loc, str.size()-loc);
  return result;
}

int readIdentifier(std::string_view& str, const IdentifierHash& map)
{
  // if str[0] is a character, find the identifier, and increment str past that,
  if (not isalpha(str[0])) return -1;
  size_t loc = 0;
  while (loc < str.size() and (isdigit(str[loc]) or isalpha(str[loc]) or str[loc] == '_'))
    {
      loc++;
    }
  std::string_view iden = str.substr(0, loc);
  str = str.substr(loc, str.size() - loc);
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
      while ((isalpha(line[loc]) or isdigit(line[loc]) or line[loc] == '_') and (loc < line.size()))
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
void parseBasicPoly(std::string_view& str, const IdentifierHash& idenHash, BasicPoly& result)
{
  result.mCoefficients.clear();
  result.mMonomials.clear();
  
  if (str.size() > 0 and str[0] == '[') str.remove_prefix(1);
  if (str.size() > 0 and str[str.size()-1] == ',') str.remove_suffix(1);
  if (str.size() > 0 and str[str.size()-1] == ':') str.remove_suffix(1);
  if (str.size() > 0 and str[str.size()-1] == ']') str.remove_suffix(1);

  if (str.size() == 0) return; // result is already set to 0.

  while (str.size() > 0)
    {
      int sign = 1;
      
      // Read the next term into `result`.
      if (str[0] == '+') str.remove_prefix(1);
      if (str[0] == '-')
        {
          str.remove_prefix(1);
          sign = -1;
        }
      long coeff = readInteger(str); // defaults to 1 if no integer present.
        
      if (sign == -1) coeff = -coeff;
      result.mCoefficients.push_back(coeff);

      // Now we read the monomial part.
      long loc = result.mMonomials.size(); // this is where the length field will go.
      result.mMonomials.push_back(1); // 1 means that the monomial is `1`.

      // We expect the first character to be an identifier char.
      // then right after that a `*` or `^'.
      // If we get to "+", or "-" or enf of string: we set result.mMonomials[loc] to the correct size.
      while (str.size() > 0)
        {
          char c = str[0];
          if (c == '-' or c == '+')
            break; // on to the next term
          if (c == '*')
            {
              str.remove_prefix(1);
              if (str.size() == 0)
                {
                  // throw an error
                }
              c = str[0];
            }
          
          if (not isalpha(c)) {} // not well forrmed, I think.
          int v = readIdentifier(str, idenHash);
          int e = 1;
          if (str.size() > 0 and str[0] == '^')
            {
              str.remove_prefix(1);
              e = readInteger(str);
            }
          result.mMonomials.push_back(v);
          result.mMonomials.push_back(e);
          result.mMonomials[loc] += 2;
        }
    }
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

// Local Variables:
// indent-tabs-mode: nil
// End:
