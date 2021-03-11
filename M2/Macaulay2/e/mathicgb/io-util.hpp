// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_IO_UTIL_GUARD
#define MATHICGB_IO_UTIL_GUARD

#include "PolyRing.hpp"

MATHICGB_NAMESPACE_BEGIN

class Poly;
class SigPolyBasis;
class ModuleMonoSet;
class PolyBasis;
class Basis;

std::unique_ptr<PolyRing> ringFromString(std::string ringinfo);
monomial monomialFromString(const PolyRing *R, std::string mon);
monomial monomialParseFromString(const PolyRing *R, std::string mon);
std::string monomialToString(const PolyRing *R, const_monomial mon);
std::string monomialDisplay(const PolyRing *R, const_monomial mon);

Monomial stringToMonomial(const PolyRing *R, std::string mon);
std::string monomialToString(const PolyRing *R, const Monomial& mon);

std::string toString(SigPolyBasis *);
std::string toString(ModuleMonoSet *);
std::string toString(SigPolyBasis *, int unused); // also displays signature
std::string toString(Basis *);
std::string toString(const Poly *);

std::unique_ptr<Basis> basisParseFromString(std::string str);
std::unique_ptr<Poly> polyParseFromString
  (const PolyRing *R, const std::string &s);

void output(std::ostream &o, const PolyBasis &I);

MATHICGB_NAMESPACE_END
#endif
