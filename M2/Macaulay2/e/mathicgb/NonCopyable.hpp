// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_NON_COPYABLE_GUARD
#define MATHICGB_NON_COPYABLE_GUARD

MATHICGB_NAMESPACE_BEGIN

/// Derive from this class to disable the compiler-generated copy
/// constructor and assignment. T should be the class that is deriving
/// from NonCopyable.
///
/// The purpose of the template parameter is to avoid any chance of
/// getting a diamond-graph inheritance graph. Diamond graphs can lead
/// to runtime overhead.
template<class T>
class NonCopyable {
public:
  NonCopyable() {}
  NonCopyable(NonCopyable&&) {} // still movable.

private:
  NonCopyable(const NonCopyable&); // unavailable
  void operator=(const NonCopyable&); // unavailable
};

MATHICGB_NAMESPACE_END
#endif
