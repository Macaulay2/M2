#ifndef MATHIC_STDINC_GUARD
#define MATHIC_STDINC_GUARD

#ifdef _MSC_VER // For Microsoft Compiler in Visual Studio C++.
// Sometimes you know that a function will be called very rarely so you want to
// tell the compiler not to inline it even if it could be inlined at only a
// modest increase in code size. That is what MATHIC_NO_INLINE does.
#define MATHIC_NO_INLINE __declspec(noinline)

// Sometimes the compiler just will not inline functions that should
// be inlined. Use sparingly --- preferably only if a profiler says
// that a tiny often-called function consumes a significant amount of time.
#define MATHIC_INLINE __forceinline

// Tells the compiler to always assume that the expression X is true.
#define MATHIC_ASSUME(X) __assume(X)

// As MATHIC_ASSUME, but might actually evaluate X at run-time if it has
// side-effects. The point is that this can be used on compilers with no other
// support for assuming things. So there is no difference on MS VC++.
#define MATHIC_ASSUME_AND_MAY_EVALUATE(X) __assume(X)

// Tells the compiler that this function returns a pointer that is not an alias
// for any other point that is currently valid in the program - like malloc.
#define MATHIC_RETURN_NO_ALIAS __declspec(restrict)

// Tells the compiler that this function will never throw an exception.
#define MATHIC_NOTHROW __declspec(nothrow)

// Tells the compiler that this function has no effects except the return value
// and the return value depends only on the arguments and first-level
// indirections of the arguments. (this is the common denominator of GCC
// and MS VC++ capabilities)
#define MATHIC_PURE __declspec(noalias)

// Tells the compiler that the return value of this function must be looked
// at by the caller. For example this is appropriate for realloc.
#define MATHIC_MUST_CHECK_RETURN_VALUE

// Tells the compiler that the current line of code cannot be reached.
#define MATHIC_UNREACHABLE __assume(false)

// Tells the compiler that a variable that is a pointer (not a reference)
// does not alias any other pointer that is used in the current scope.
#define MATHIC_RESTRICT __restrict

#elif defined (__GNUC__) // GCC compiler

#define MATHIC_NO_INLINE __attribute__((noinline))
#define MATHIC_INLINE __attribute__((always_inline)) inline
#define MATHIC_ASSUME(X)
#define MATHIC_ASSUME_AND_MAY_EVALUATE(X) do {if(!(X)){MATHIC_UNREACHABLE;}while(0)}
#define MATHIC_RETURN_NO_ALIAS __attribute__(malloc)
#define MATHIC_NOTHROW __attribute__(nothrow)
#define MATHIC_PURE __attribute__(pure)
#define MATHIC_MUST_CHECK_RETURN_VALUE __attribute__(warn_unused_result)
#define MATHIC_UNREACHABLE __builtin_unreachable()

#else

#define MATHIC_NO_INLINE
#define MATHIC_INLINE inline
#define MATHIC_ASSUME(X)
#define MATHIC_ASSUME_AND_MAY_EVALUATE(X)
#define MATHIC_RETURN_NO_ALIAS
#define MATHIC_NOTHROW
#define MATHIC_PURE
#define MATHIC_MUST_CHECK_RETURN_VALUE
#define MATHIC_UNREACHABLE

#endif


#ifdef MATHIC_SLOW_DEBUG
// for asserts that take a long time.
#define MATHIC_SLOW_ASSERT(X) MATHIC_ASSERT(X)
#ifndef MATHIC_DEBUG
#define MATHIC_DEBUG
#endif
#else
#define MATHIC_SLOW_ASSERT(X)
#endif

#ifdef MATHIC_DEBUG
#include <cassert>
#define MATHIC_ASSERT(X) do{assert(X);}while(0)
#define MATHIC_ASSERT_NO_ASSUME(X) MATHIC_ASSERT(X)
#else
#define MATHIC_ASSERT(X) MATHIC_ASSUME(X)
#define MATHIC_ASSERT_NO_ASSUME(X)
#endif

namespace mathic {
  static unsigned long const BitsPerByte = 8;
}

#ifndef MATHIC_NO_MIC_NAMESPACE
namespace mic {
  using namespace mathic;
}
#endif

#endif
