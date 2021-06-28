/* Copyright (C) 2011 Bjarke Hammersholt Roune (www.broune.com)
   MemTailor is distributed under the Modified BSD License. See license.txt. */

#ifndef MEMT_STDINC_GUARD
#define MEMT_STDINC_GUARD

#ifdef _MSC_VER // For Microsoft Compiler in Visual Studio C++.
// Sometimes you know that a function will be called very rarely so you want to
// tell the compiler not to inline it even if it could be inlined at only a
// modest increase in code size. That is what MEMT_NO_INLINE does.
#define MEMT_NO_INLINE __declspec(noinline)

// Sometimes the compiler just will not inline functions that should
// be inlined. Use sparingly --- preferably only if a profiler says
// that a tiny often-called function consumes a significant amount of time.
#define MEMT_INLINE __forceinline

// Tells the compiler to always assume that the expression X is true.
#define MEMT_ASSUME(X) __assume(X)

// As MEMT_ASSUME, but might actually evaluate X at run-time if it has
// side-effects. The point is that this can be used on compilers with no other
// support for assuming things. So there is no difference on MS VC++.
#define MEMT_ASSUME_AND_MAY_EVALUATE(X) __assume(X)

// Tells the compiler that this function returns a pointer that is not an alias
// for any other point that is currently valid in the program - like malloc.
#define MEMT_RETURN_NO_ALIAS __declspec(restrict)

// Tells the compiler that this function will never throw an exception.
#define MEMT_NOTHROW __declspec(nothrow)

// Tells the compiler that this function has no effects except the return value
// and the return value depends only on the arguments and first-level
// indirections of the arguments. (this is the common denominator of GCC
// and MS VC++ capabilities)
#define MEMT_PURE __declspec(noalias)

// Tells the compiler that the return value of this function must be looked
// at by the caller. For example this is appropriate for realloc.
#define MEMT_MUST_CHECK_RETURN_VALUE

// Tells the compiler that the current line of code cannot be reached.
#define MEMT_UNREACHABLE __assume(false)

// Tells the compiler that a variable that is a pointer (not a reference)
// does not alias any other pointer that is used in the current scope.
#define MEMT_RESTRICT __restrict

#elif defined (__GNUC__) // GCC compiler

#define MEMT_NO_INLINE __attribute__((noinline))
#define MEMT_INLINE __attribute__((always_inline)) inline
#define MEMT_ASSUME(X)
#define MEMT_ASSUME_AND_MAY_EVALUATE(X) do {if(!(X)){MEMT_UNREACHABLE;}while(0)}
#define MEMT_RETURN_NO_ALIAS __attribute__(malloc)
#define MEMT_NOTHROW __attribute__(nothrow)
#define MEMT_PURE __attribute__(pure)
#define MEMT_MUST_CHECK_RETURN_VALUE __attribute__(warn_unused_result)
#define MEMT_UNREACHABLE __builtin_unreachable()

#else

#define MEMT_NO_INLINE
#define MEMT_INLINE inline
#define MEMT_ASSUME(X)
#define MEMT_ASSUME_AND_MAY_EVALUATE(X)
#define MEMT_RETURN_NO_ALIAS
#define MEMT_NOTHROW
#define MEMT_PURE
#define MEMT_MUST_CHECK_RETURN_VALUE
#define MEMT_UNREACHABLE

#endif


#ifdef MEMT_SLOW_DEBUG
// for asserts that take a long time.
#define MEMT_SLOW_ASSERT(X) MEMT_ASSERT(X)
#ifndef MEMT_DEBUG
#define MEMT_DEBUG
#endif
#else
#define MEMT_SLOW_ASSERT(X)
#endif

#ifdef MEMT_DEBUG
#include <cassert>
#define MEMT_ASSERT(X) do{assert(X);}while(0)
#define MEMT_ASSERT_NO_ASSUME(X) MEMT_ASSERT(X)
#else
#define MEMT_ASSERT(X) MEMT_ASSUME(X)
#define MEMT_ASSERT_NO_ASSUME(X)
#endif


namespace memt {
  /// The alignment that memory allocators must ensure. In other words
  /// allocators must return pointer addresses that are divisible by
  /// MemoryAlignment. MemoryAlignment must be a power of 2.
  static const unsigned int MemoryAlignment = static_cast<unsigned int>(sizeof(void*));

  static const unsigned int BitsPerByte = 8;
}

#endif
