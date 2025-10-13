// Current restriction: the coefficients must be an integral type.
//   TODO: allow infinite precision integers too.
//   TODO: how should we handle coefficients which are: GF(p^n), QQ, fraction fields? or even polynomials?
#pragma once

#include "exceptions.hpp"
#include <vector>
#include <iosfwd>
#include <string>
#include <unordered_map>
#include <gmpxx.h>

class BasicPoly
{
public:
  std::vector<mpz_class> mCoefficients;
  std::vector<int> mComponents; // if zero length: all components are 0.
  std::vector<int> mMonomials; // a concatenated list of varpower monomials.  Each first entry is its length.

  void clear(); // resets all data to represent the zero polynomial
  ~BasicPoly() { clear(); }
  
  size_t termCount() const { return mCoefficients.size(); } 
  void debug_display(std::ostream& o) const;

  template<typename T>
  static void displayCoefficient(std::ostream& o, T val, bool print_plus, bool print_one);

  void display(std::ostream& o,
                     const std::vector<std::string> & varnames,
                     bool print_one,
                     bool print_plus,
                     bool print_parens) const;
  void display(std::ostream& o, const std::vector<std::string> & varnames) const
  {
    display(o, varnames, true, false, false);
  }
  std::string toString(const std::vector<std::string> & varnames,
                       bool print_one,
                       bool print_plus,
                       bool print_parens) const;
  std::string toString(const std::vector<std::string> & varnames) const
  {
    return toString(varnames, true, false, false);
  }

  long bytesUsed() const;
};

///////////////////////////////////
// Simple parsing of polynomials //
///////////////////////////////////

struct parsing_error : public exc::engine_error
{
  explicit parsing_error(const std::string &msg) : exc::engine_error(msg) {}
};

/// IdentifierHash: used to facilitate parsing of polynomials from strings and files
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
  }

  auto find(std::string_view s) const -> int
  {
    auto foundloc = mMap.find(s);
    return (foundloc != mMap.end() ? foundloc->second : -1); // TODO: throw an error if not found?
  }
};

// These will throw a parsing_error if there is a parsing error.  The
// plan is that that will include the location in the string of the
// error.

BasicPoly parseBasicPoly(std::string poly, std::vector<std::string> varnames);

/// This version is a potentially faster alternative when reading many polynomials
void parseBasicPoly(const std::string_view& str, const IdentifierHash& idenHash, BasicPoly& result);


// TODO: we want an iterator type here.
// TODO: 
// 

// Local Variables:
// indent-tabs-mode: nil
// End:
