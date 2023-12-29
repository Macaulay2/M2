// Current restriction: the coefficients must be an integral type.
//   TODO: allow infinite precision integers too.
//   TODO: how should we handle coefficients which are: GF(p^n), QQ, fraction fields? or even polynomials?
#pragma once

#include "exceptions.hpp"
#include <string>
#include <vector>
#include <iosfwd>
  
class BasicPoly
{
public:
  std::vector<int> mCoefficients;
  std::vector<int> mComponents; // if zero length: all components are 0.
  std::vector<int> mMonomials; // a concatenated list of varpower monomials.  Each first entry is its length.
  
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

// These will throw a parsing_error if there is a parsing error.  The
// plan is that that will include the location in the string of the
// error.

BasicPoly parseBasicPoly(std::string poly, std::vector<std::string> varnames);

struct parsing_error : public exc::engine_error
{
  explicit parsing_error(const std::string &msg) : exc::engine_error(msg) {}
};

// TODO: we want an iterator type here.
// TODO: 
// 

// Local Variables:
// indent-tabs-mode: nil
// End:
