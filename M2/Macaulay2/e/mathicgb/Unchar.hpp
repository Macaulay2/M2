// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_UNCHAR_GUARD
#define MATHICGB_UNCHAR_GUARD

#include <type_traits>

MATHICGB_NAMESPACE_BEGIN

/// std::ostream and std::istream handle characters differently from
/// other integers. That is not desired when using char as an
/// integer. Use Unchar and unchar() to cast chars to a different type
/// that get handled as other integers do.
template<class T>
struct Unchar {typedef T type;};

// Strange but true: char, signed char and unsigned char are 3
// distinct types. Also, the signedness of char is unspecified. This
// is in contrast to all the other built-in types. For example, int
// and signed int are always the exact same type.

namespace UncharInternal {
  // Two cases depending on whether char is signed or not.
  template<bool Signed = std::is_signed<char>::value>
  struct ExtendedChar {typedef signed short type;};
  template<>
  struct ExtendedChar<false> {typedef unsigned short type;};    
};

template<>
struct Unchar<char> {
  typedef UncharInternal::ExtendedChar<>::type type;
};
template<>
struct Unchar<signed char> {typedef short type;};
template<>
struct Unchar<unsigned char> {typedef unsigned short type;};

template<class T>
typename Unchar<T>::type unchar(const T& t) {return t;}

MATHICGB_NAMESPACE_END
#endif
