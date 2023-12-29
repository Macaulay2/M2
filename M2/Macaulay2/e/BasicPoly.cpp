#include "BasicPoly.hpp"
#include <sstream>

std::string BasicPoly::toString(const std::vector<std::string> & varnames,
                                bool print_one,
                                bool print_plus,
                                bool print_parens) const
{
  std::ostringstream o;
  display(o, varnames, print_one, print_plus, print_parens);
  return o.str();
}

long BasicPoly::bytesUsed() const
{
  return 3 * sizeof(std::vector<int>)
    + sizeof(int) * mCoefficients.size()
    + sizeof(int) * mComponents.size()
    + sizeof(int) * mMonomials.size();
}

// Local Variables:
// indent-tabs-mode: nil
// End:
