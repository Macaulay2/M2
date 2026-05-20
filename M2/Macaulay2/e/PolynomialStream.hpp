#pragma once

/**
 * @file PolynomialStream.hpp
 * @brief Type aliases and the (currently disabled) C++20 concept that streaming polynomial consumers implement.
 *
 * Declares `newf4::Coefficient`, `VarIndex`, `Exponent`, and
 * `Component` --- the `int32_t` aliases that name the four
 * pieces of every streamed term and that callers throughout the
 * refactored F4 path share with `gb-f4/MonomialTypes.hpp`. The
 * in-source TODO requires these aliases to stay in lockstep with
 * `BasicPoly`.
 *
 * The remainder of the header sketches the streaming protocol
 * any concrete consumer must support: `idealBegin` /
 * `appendPolynomialBegin` / `appendTermBegin` / `appendExponent`
 * / `appendTermDone` / `appendPolynomialDone` / `idealDone`. The
 * actual `concept PolynomialStream` is parked under `#if 0`
 * pending the engine's move to C++20; in the meantime consumers
 * such as `BasicPolyList` and the `gb-f4` `PolynomialList`
 * builder implement the shape unchecked, and `BasicPolyListParser`
 * is the natural producer end.
 *
 * @see BasicPoly.hpp
 * @see BasicPolyList.hpp
 * @see BasicPolyListParser.hpp
 */

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
