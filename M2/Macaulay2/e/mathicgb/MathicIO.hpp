// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MATHIC_IO_GUARD
#define MATHICGB_MATHIC_IO_GUARD

#include "Basis.hpp"
#include "Poly.hpp"
#include "Scanner.hpp"
#include "PolyRing.hpp"
#include "MonoProcessor.hpp"
#include <ostream>
#include <string>

MATHICGB_NAMESPACE_BEGIN

/// Class for input and output in Mathic's format.
template<
  class Monoid = PolyRing::Monoid,
  class BaseField = PolyRing::Field
>
class MathicIO;

template<class M, class BF>
class MathicIO {
public:
  typedef BF BaseField;
  typedef typename BaseField::Element Coefficient;
  typedef typename BaseField::RawElement RawCoefficient;

  typedef M Monoid;
  typedef typename Monoid::MonoRef MonoRef;
  typedef typename Monoid::Exponent Exponent;
  typedef typename Monoid::VarIndex VarIndex;
  typedef typename Monoid::ConstMonoRef ConstMonoRef;
  typedef typename Monoid::Order Order;
  typedef typename Monoid::MonoVector MonoVector;
  typedef MonoProcessor<Monoid> Processor;

  typedef typename  Order::Gradings Gradings;

  BaseField readBaseField(Scanner& in);
  void writeBaseField(const BaseField& field, std::ostream& out);

  std::pair<std::unique_ptr<PolyRing>, Processor> readRing(
    const bool withComponent,
    Scanner& in
  );
  void writeRing(
    const PolyRing& ring,
    const Processor& processor,
    const bool withComponent,
    std::ostream& out
  );

  Order readOrderWithVarCount(bool withComponent, Scanner& in);
  void writeOrderWithVarCount
    (const Order& monoid, const bool withComponent, std::ostream& out);

  Order readOrder(
    const VarIndex varCount,
    const bool withComponent,
    Scanner& in
  );

  void writeOrder(
    const Order& order,
    const bool withComponent,
    std::ostream& out
  );

  Basis readBasis(
    const PolyRing& ring,
    const bool readComponent,
    Scanner& in
  );

  void writeBasis(
    const Basis& basis,
    const bool writeComponent,
    std::ostream& out
  );

  /// Reads a polynomial and does not reorder the terms to be in descending
  /// order.
  Poly readPolyDoNotOrder(
    const PolyRing& ring,
    const bool readComponent,
    Scanner& in
  );

  /// Reads a polynomial and orders the terms in descending order.
  Poly readPoly(const PolyRing& ring, const bool readComponent, Scanner& in);

  void writePoly(
    const Poly& poly,
    const bool writeComponent,
    std::ostream& out
  );

  void readTerm(
    const PolyRing& ring,
    const bool readComponent,
    Coefficient& coef,
    MonoRef mono,
    Scanner& in
  );

  void writeTerm(
    const PolyRing& ring,
    const bool writeComponent,
    const Coefficient coef,
    ConstMonoRef mono,
    bool writeSignEvenIfPositive,
    std::ostream& out
  );

  /// Read a monomial with no coefficient. A 1 on its own is not
  /// considered a coefficient here - it is the monomial with all
  /// exponents zero and no coefficient.
  ///
  /// @todo: Eventually, pick up readComponent from Monoid::HasComponent.
  void readMonomial(
    const Monoid& monoid,
    const bool readComponent,
    MonoRef mono,
    Scanner& in
  );

  /// Print a monomial with no coefficient.
  void writeMonomial(
    const Monoid& monoid,
    const bool writeComponent,
    ConstMonoRef mono,
    std::ostream& out
  );

  /// Reads a non-empty space-separated list of monomials. The monomials
  /// are appended to the end of the vector.
  void readMonomialVector(
    const bool readComponents,
    Scanner& in,
    MonoVector& v
  ) {
    MATHICGB_ASSERT(Monoid::HasComponent || !readComponents);
    while (true) {
      v.push_back();
      readMonomial(v.monoid(), readComponents, v.back(), in);
      if (in.peek() != ' ')
        break;
      in.get();
    }
  }

  void writeMonomialVector(
    const MonoVector& v,
    const bool writeComponent,
    std::ostream& out
  ) {
    MATHICGB_ASSERT(Monoid::HasComponent || !writeComponent);
    for (auto it = v.begin(); it != v.end(); ++it) {
      if (it != v.begin())
        out << ' ';
      writeMonomial(v.monoid(), writeComponent, *it, out);
    }
    out << '\n';
  }

  /// Read the trailing indicator of the component of a module monomial.
  void readComponent(
    const Monoid& monoid,
    MonoRef mono,
    Scanner& in
  );

  void writeComponent(
    const Monoid& monoid,
    ConstMonoRef mono,
    std::ostream& out
  );
};

template<class M, class BF>
auto MathicIO<M, BF>::readBaseField(Scanner& in) -> BaseField {
  return BaseField(in.readInteger<RawCoefficient>());
}

template<class M, class BF>
void MathicIO<M, BF>::writeBaseField(
  const BaseField& field,
  std::ostream& out
) {
  out << field.charac();
}

template<class M, class BF>
auto MathicIO<M, BF>::readRing(
  const bool withComponent,
  Scanner& in
) -> std::pair<std::unique_ptr<PolyRing>, Processor> {
  auto baseField = readBaseField(in);
  const auto varCount = in.readInteger<VarIndex>();
  auto order = readOrder(varCount, withComponent, in);
  const bool componentsAscendingDesired = order.componentsAscendingDesired();
  const bool schreyering = order.schreyering();
  auto ring = make_unique<PolyRing>
    (std::move(baseField), Monoid(std::move(order)));

  Processor processor(
    ring->monoid(),
    componentsAscendingDesired,
    schreyering
  );

  return std::make_pair(std::move(ring), std::move(processor));
}

template<class M, class BF>
void MathicIO<M, BF>::writeRing(
  const PolyRing& ring,
  const Processor& processor,
  const bool withComponent,
  std::ostream& out
){
  writeBaseField(ring.field(), out);
  out << ' ' << ring.varCount() << '\n';

  auto&& order = ring.monoid().makeOrder(
    processor.componentsAscendingDesired(),
    processor.schreyering()
  );
  writeOrder(order, withComponent, out);
}

template<class M, class BF>
auto MathicIO<M, BF>::readOrderWithVarCount(bool withComponent, Scanner& in)
  -> Order
{
  const auto varCount = in.readInteger<VarIndex>();
  return readOrder(varCount, withComponent, in);
}

template<class M, class BF>
void MathicIO<M, BF>::writeOrderWithVarCount(
  const Order& order,
  const bool withComponent,
  std::ostream& out
) {
  out << order.varCount() << '\n';
  writeOrder(order, withComponent, out);
}

template<class M, class BF>
auto MathicIO<M, BF>::readOrder(
  const VarIndex varCount,
  const bool withComponent,
  Scanner& in
) -> Order {
  const bool schreyering = in.match("schreyer");
  bool lexBaseOrder = !in.match("revlex") && in.match("lex");

  auto gradingCount = in.readInteger<VarIndex>();
  bool componentsAscendingDesired = true;
  auto componentCompareIndex = Order::ComponentAfterBaseOrder;
  Gradings gradings;
  gradings.reserve(static_cast<size_t>(varCount) * gradingCount);
  for (VarIndex grading = 0; grading <  gradingCount; ++grading) {
    const bool com = in.match("component");
    if (com || in.match("revcomponent")) {
      if (!withComponent)
        in.reportError("Cannot specify component comparison for non-modules.");
      MATHICGB_ASSERT(Monoid::HasComponent);
      if (componentCompareIndex != Order::ComponentAfterBaseOrder)
        in.reportError("Component comparison must be specified at most once.");
      componentsAscendingDesired = com;
      componentCompareIndex = grading;
    } else {
      for (VarIndex i = 0; i < varCount; ++i)
        gradings.emplace_back(in.readInteger<Exponent>());
    }
  }

  const bool moreLex = in.match("_lex");
  if (moreLex || in.match("_revlex")) {
    lexBaseOrder = moreLex;
    const bool moreCom = in.match("component");
    if (moreCom || in.match("revcomponent")) {
      if (!withComponent)
        in.reportError("Cannot specify component comparison for non-modules.");
      MATHICGB_ASSERT(Monoid::HasComponent);
      componentsAscendingDesired = moreCom;
    }
  }

  Order order(
    varCount,
    std::move(gradings),
    lexBaseOrder ?
      Order::LexBaseOrderFromRight : Order::RevLexBaseOrderFromRight,
    componentCompareIndex,
    componentsAscendingDesired,
    schreyering
  );
  return std::move(order);
}

template<class M, class BF>
void MathicIO<M, BF>::writeOrder(
  const Order& order,
  const bool withComponent,
  std::ostream& out
) {
  MATHICGB_ASSERT(Monoid::HasComponent || !withComponent);

  const auto baseOrder =
    order.baseOrder() == Order::LexBaseOrderFromRight ? "lex" : "revlex";
  const auto componentOrder =
    order.componentsAscendingDesired() ? "component" : "revcomponent";

  if (order.schreyering())
    out << "schreyer ";
  const bool componentLast =
    order.componentBefore() == Order::ComponentAfterBaseOrder;
  out << baseOrder << ' ' << order.gradingCount() + !componentLast << '\n';
  for (VarIndex grading = 0; grading < order.gradingCount(); ++grading) {
    if (withComponent && grading == order.componentBefore())
      out << ' ' << componentOrder << '\n';
    for (VarIndex var = 0; var < order.varCount(); ++var) {
      const auto index = var + grading * order.varCount();
      out << ' ' << unchar(order.gradings()[index]);
    }
    out << '\n';
  }
  if (withComponent && order.componentBefore() == order.gradingCount())
    out << ' ' << componentOrder << '\n';

  if (
    withComponent &&
    !order.componentsAscendingDesired() &&
    order.componentBefore() == Order::ComponentAfterBaseOrder
  ) {
    out << " _" << baseOrder << "\n " << componentOrder << '\n';
  }
}

template<class M, class BF>
Basis MathicIO<M, BF>::readBasis(
  const PolyRing& ring,
  const bool readComponent,
  Scanner& in
) {
  const auto polyCount = in.readInteger<size_t>();
  Basis basis(ring);
  for (size_t i = 0; i < polyCount; ++i) {
    auto p = make_unique<Poly>(readPoly(ring, readComponent, in));
    *p = p->polyWithTermsDescending();
    basis.insert(std::move(p));
  }
  return basis;
}

template<class M, class BF>
void MathicIO<M, BF>::writeBasis(
  const Basis& basis,
  const bool writeComponent,
  std::ostream& out
) {
  out << basis.size() << '\n';
  for (size_t i = 0; i < basis.size(); ++i) {
    out << ' ';
    writePoly(*basis.getPoly(i), writeComponent, out);
    out << '\n';
  }
}

template<class M, class BF>
Poly MathicIO<M, BF>::readPoly(
  const PolyRing& ring,
  const bool readComponent,
  Scanner& in
) {
  return readPolyDoNotOrder(ring, readComponent, in).polyWithTermsDescending();
}

template<class M, class BF>
Poly MathicIO<M, BF>::readPolyDoNotOrder(
  const PolyRing& ring,
  const bool readComponent,
  Scanner& in
) {
  Poly p(ring);

  // also skips whitespace
  if (in.match('0') || in.match("+0") || in.match("-0"))
    return p;
  MATHICGB_ASSERT(!in.peekWhite());

  auto mono = ring.monoid().alloc();
  auto coef = ring.field().zero();
  do {
    if (!p.isZero() && !in.peekSign() && (!readComponent || in.peek() != '<'))
      in.expect('+', '-');
    readTerm(ring, readComponent, coef, mono, in);
    p.append(coef.value(), *mono);
  } while (!in.peekWhite() && !in.matchEOF());
  return p;
}

template<class M, class BF>
void MathicIO<M, BF>::writePoly(
  const Poly& poly,
  const bool writeComponent,
  std::ostream& out
) {
  if (poly.isZero()) {
    out << '0';
    return;
  }

  const auto end = poly.end();
  for (auto it = poly.begin(); it != end; ++it) {
    writeTerm(
      poly.ring(),
      writeComponent,
      it.coef(),
      it.mono(),
      it != poly.begin(),
      out
    );
  }
}

template<class M, class BF>
void MathicIO<M, BF>::readTerm(
  const PolyRing& ring,
  const bool readComponent,
  Coefficient& coef,
  MonoRef mono,
  Scanner& in
) {
  // ** Read coefficient, if any.
  const auto& field = ring.field();
  const auto& monoid = ring.monoid();
  const bool negate = !in.match('+') && in.match('-');
  if (in.peekDigit()) {
    coef = in.readModular(field, negate);

    if (!in.peekAlpha()) {
      // Identify a number c on its own as the monomial 1 times c.
      monoid.setIdentity(mono);
      if (readComponent)
        this->readComponent(monoid, mono, in);
      return;
    }
  } else if (negate)
    coef = field.minusOne();
  else
    coef = field.one();

  readMonomial(monoid, readComponent, mono, in);
}

template<class M, class BF>
void MathicIO<M, BF>::writeTerm(
  const PolyRing& ring,
  const bool writeComponent,
  Coefficient coef,
  ConstMonoRef mono,
  bool writeSignEvenIfPositive,
  std::ostream& out
) {
  Coefficient coef1 = coef;
  if (ring.field().isNegative(coef)) {
    out << "-";
    coef1 = ring.field().negativeNonZero(coef);
  } else if (writeSignEvenIfPositive)
    out << '+';
  if (!ring.field().isOne(coef1)) {
    out << unchar(coef1.value());
    if (ring.monoid().isIdentity(mono)) {
      if (writeComponent)
        this->writeComponent(ring.monoid(), mono, out);
      return;
    }
  } 
  writeMonomial(ring.monoid(), writeComponent, mono, out);
}

template<class M, class BF>
void MathicIO<M, BF>::readMonomial(
  const Monoid& monoid,
  const bool readComponent,
  MonoRef mono,
  Scanner& in
) {
  MATHICGB_ASSERT(!readComponent || Monoid::HasComponent);

  monoid.setIdentity(mono);
  if (in.peek() == '1') {
    const auto e = in.readInteger<Exponent>();
    if (e != 1) {
      std::ostringstream err;
      err << "Expected monomial, but got " << e << " (did you mean 1?).";
      in.reportError(err.str());
    }
  } else if (!readComponent || in.peek() != '<') {
    bool sawSome = false;
    while (true) {
      const auto letterCount = 'z' - 'a' + 1;
      const auto letter = in.peek();
      
      VarIndex var;
      if ('a' <= letter && letter <= 'z')
        var = letter - 'a';
      else if ('A' <= letter && letter <= 'Z')
        var = (letter - 'A') + letterCount;
      else if (sawSome)
        break;
      else {
        std::ostringstream err;
        err << "Expected letter while reading monomial, but got '"
          << static_cast<char>(letter) << "'.";
        in.reportError(err.str());
        return;
      }
      in.get(); // skip past letter
      
      MATHICGB_ASSERT(var < 2 * letterCount);
      if (var >= monoid.varCount()) {
        std::ostringstream err;
        err << "Saw the variable " << static_cast<char>(letter)
          << ", but the monoid only has "
          << monoid.varCount() << " variables.";
        in.reportError(err.str());
        return;
      }
      if (monoid.externalExponent(mono, var) > static_cast<Exponent>(0)) {
        std::ostringstream err;
        err << "Variable " << static_cast<char>(letter) <<
          " must not be written twice in one monomial.";
        in.reportError(err.str());
      }
      
      if (in.peekDigit())
        monoid.setExternalExponent(var, in.readInteger<Exponent>(), mono);
      else
        monoid.setExternalExponent(var, static_cast<Exponent>(1), mono);
      sawSome = true;
    }
  }

  if (readComponent)
    this->readComponent(monoid, mono, in);
}

template<class M, class BF>
void MathicIO<M, BF>::readComponent(
  const Monoid& monoid,
  MonoRef mono,
  Scanner& in
) {
  MATHICGB_ASSERT(Monoid::HasComponent);
  in.expect('<');
  monoid.setComponent(in.readInteger<Exponent>(), mono);
  in.expect('>');
}

template<class M, class BF>
void MathicIO<M, BF>::writeComponent(
  const Monoid& monoid,
  ConstMonoRef mono,
  std::ostream& out
) {
  MATHICGB_ASSERT(Monoid::HasComponent);
  out << '<' << unchar(monoid.component(mono)) << '>';
}

/// Print a monomial with no coefficient.
template<class M, class BF>
void MathicIO<M, BF>::writeMonomial(
  const Monoid& monoid,
  const bool writeComponent,
  ConstMonoRef mono,
  std::ostream& out
) {
  const auto letterCount = 'z' - 'a' + 1;

  bool printedSome = false;
  for (VarIndex var = 0; var < monoid.varCount(); ++var) {
    const auto e = monoid.exponent(mono, var);
    if (e == 0)
      continue;
    char letter;
    if (var < letterCount)
      letter = 'a' + static_cast<char>(var);
    else if (var < 2 * letterCount)
      letter = 'A' + (static_cast<char>(var) - letterCount);
    else {
      mathic::reportError("Too few letters in alphabet to print variable.");
      return;
    }
    printedSome = true;
    out << letter;
    if (e != 1)
      out << unchar(e);
  }
  if (!printedSome)
    out << '1';
  if (writeComponent)
    this->writeComponent(monoid, mono, out);
}

MATHICGB_NAMESPACE_END
#endif
