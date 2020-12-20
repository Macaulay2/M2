#ifndef __range__
#define __range__

#include <utility>
#include <vector>

template<typename T>
class Range
{
private:
  T* mFirst;
  T* mLast;
public:
  Range(T* first, T* last) : mFirst(first), mLast(last) {}
  Range(std::pair<T*, T*> a) : mFirst(a.first), mLast(a.second) {}
  
  explicit Range(std::vector<T>& vec) : mFirst(vec.data()), mLast(vec.data() + vec.size()) {}
  explicit Range(VECTOR(T)& vec) : mFirst(vec.data()), mLast(vec.data() + vec.size()) {}

  int size() const { return mLast - mFirst; }
  T* begin() { return mFirst; }
  T* end() { return mLast; }

  T& operator[](int i) { return *(mFirst + i); }
  const T& operator[](int i) const { return *(mFirst + i); }
  
  const T* cbegin() const { return mFirst; }
  const T* cend() const { return mLast; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
