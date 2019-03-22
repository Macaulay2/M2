#include "Polynomial.hpp"

std::ostream& operator<<(std::ostream& o, const Monom& m)
{
  o << "[";
  for (int i=0; i<m[0]; ++i)
    o << m[i] << " ";
  o << "]";
  return o;
}

std::ostream& operator<<(std::ostream& o, const ModuleMonom& m)
{
  o << "[";
  for (int i=0; i<m[0]-1; ++i)
    o << m[i] << " ";
  o << m[m[0]-1] << "]";
  return o;
  o << "val=" << m[1] << " comp= " << m[3] << " [";
  for (int i=3; i<m.size(); ++i)
      o << m[i] << " ";
  o << "]" << std::endl;
  return o;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
