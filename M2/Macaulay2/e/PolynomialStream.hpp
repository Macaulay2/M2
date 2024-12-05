#pragma once

namespace newf4
{

// TODO: these must match BasicPoly.
using Coefficient = int32_t;
using VarIndex = int32_t; 
using Exponent = int32_t;
using Component = int32_t;

// this feels more like specifying a base class than a concept...
// TODO: once we go to c++20, enable the concept PolynomialStream
#if 0  
template <typename T>
concept PolynomialStream = requires (T str,
                                     size_t count,
                                     Component com,
                                     VarIndex index,
                                     Exponent exponent,
                                     Coefficient coefficient) {
  { str.idealBegin(count) } -> std::convertible_to<void>;
  { str.appendPolynomialBegin(count) } -> std::convertible_to<void>;
  { str.appendTermBegin(com) } -> std::convertible_to<void>;
  { str.appendExponent(index, exponent) } -> std::convertible_to<void>;
  { str.appendTermDone(coefficient) } -> std::convertible_to<void>;
  { str.appendPolynomialDone() } -> std::convertible_to<void>;
  { str.idealDone() } -> std::convertible_to<void>;
  // testing to see if concept works
  // { str.dummy() } -> std::convertible_to<void>;
};
#endif
  
} // end namespace newf4

// Local Variables:
// indent-tabs-mode: nil
// End:
