// Copyright 2011  Michael E. Stillman

// This is TEST code for the moment. (7 Jan 2011)

#ifndef spair_heap_hh_
#define spair_heap_hh_

// From example 13-30, C++ in a nutshell, pages 614-615

#include <cstddef>
#include <limits>
#include <iostream>
#include <queue>

template <typename T>
class myallocator {
 public:
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;
  typedef T* pointer;
  typedef const T* const_pointer;
  typedef T& reference;
  typedef const T& const_reference;
  typedef T value_type;

  template<class U> struct rebind {
    typedef myallocator<U> other;
  };

  myallocator() throw() {}
  myallocator(const myallocator &) throw() {}
  template<class U> myallocator(const myallocator<U>&) throw() {}
  ~myallocator() throw() {}

  pointer address(reference x) const { return &x; }
  const_pointer address(const_reference x) const { return &x; }

  pointer allocate(size_type n, void *hint = 0) {
    std::cout << "allocating " << n << std::endl;
    return static_cast<T*>(::operator new (n * sizeof(T)) );
  }

  void deallocate(pointer p, size_type n) {
    std::cout << "deallocating " << n << std::endl;
    ::operator delete(static_cast<void *>(p));
  }

  size_type max_size() const throw() {
    std::cout << "max_size" << std::endl;
    return std::numeric_limits<size_type>::max() / sizeof(T);
  }

  void construct(pointer p, const T& val) {
    std::cout << "construct" << std::endl;
    new(static_cast<void *>(p)) T(val);
  }

  void destroy(pointer p) {
    std::cout << "destroy" << std::endl;
    p->~T();
  }
};

template<typename T>
bool operator==(const myallocator<T>&, const myallocator<T>&)
{
  return true;
}

template<typename T>
bool operator!=(const myallocator<T>&, const myallocator<T>&)
{
  return false;
}

template<>
class myallocator<void> {
 public:
  typedef void * pointer;
  typedef const void * const_pointer;
  typedef void value_type;
  template <class U> struct rebind {
    typedef myallocator<U> other;
  };
};

#include "spair.hpp"

class SPairCompare
{
  long ncmps_;
public:
  SPairCompare() : ncmps_(0) {}
  bool operator() (const s_pair *lhs, const s_pair *rhs)
  {
    // returns true if rhs should be handled after lhs.  Otherwise returns false.
    ncmps_ ++;
    /* Compare using degree, then type, then lead term of spoly */
    int cmp = lhs->degree - rhs->degree;
    if (cmp < 0) return true;
    if (cmp > 0) return false;
    return false;
  }
};

class spair_heap {
public:
  spair_heap(const Monoid *M);
  ~spair_heap();

  bool empty();
  void push(s_pair *);
  s_pair *pop();
  s_pair *top();
  size_t size();

private:
  const Monoid *M_;
  SPairCompare spairCompareObject_;
  std::priority_queue<s_pair *, std::vector<s_pair *, myallocator<s_pair *> >, SPairCompare> S_;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
