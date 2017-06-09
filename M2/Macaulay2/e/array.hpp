// (c) 1994  Michael E. Stillman.
#ifndef _array_hh_
#define _array_hh_

#include "newdelete.hpp"
#include "mem.hpp"

template <class T>
class array;

const unsigned int init_array_size = 16;

template <class T>
class array : public our_new_delete
{
  T *entries;
  unsigned int len;
  unsigned int max;
  void expand(unsigned int newtop)
  {
    unsigned int newlen = (len > 1 ? len : 1);
    for (; newtop >= newlen; newlen *= 2)
      ;
    T *tmp = newarray(T, newlen);
    engine_alloc(newlen * sizeof(T));

    for (unsigned int j = 0; j < max; j++) tmp[j] = entries[j];
    engine_dealloc(len * sizeof(T));
    deletearray(entries);
    entries = tmp;
    len = newlen;
  }

 public:
  array(unsigned int i_size = init_array_size) : max(0)
  {
    len = (init_array_size > i_size ? init_array_size : i_size);
    entries = newarray(T, len);
    engine_alloc(len * sizeof(T));
  }
  array(const array<T> &a) : len(a.len), max(a.max)
  {
    entries = newarray(T, len);
    engine_alloc(len * sizeof(T));
    for (unsigned int i = 0; i < max; i++) entries[i] = a.entries[i];
  }

  ~array()
  {
    engine_dealloc(len * sizeof(T));
    deletearray(entries);
    entries = NULL;
  }

  unsigned int length() const { return max; }
  void shrink(unsigned int newmax)
  {
    assert(newmax >= 0);
    if (newmax < max) max = newmax;
  }

  T &operator[](unsigned int i)
  {
    if (i < max) return entries[i];
    if (i >= len) expand(i);
    max = i + 1;
    return entries[i];
  }

  const T &operator[](unsigned int i) const
  {
    assert(i < max);
    return entries[i];
  }

  T &rawelem(unsigned int i)
  {
    assert(i < max);
    return entries[i];
  }

  const T &rawelem(unsigned int i) const
  {
    assert(i < max);
    return entries[i];
  }

  void append(const T &t)
  {
    if (max == len) expand(max);
    entries[max++] = t;
  }

  array<T> &operator=(const array<T> &a)
  {
    if (&a == this) return *this;
    engine_dealloc(len * sizeof(T));
    deletearray(entries);
    unsigned int n = (init_array_size > a.max ? init_array_size : a.max);
    max = a.max;
    len = n;
    entries =
        newarray(T, len);  // the constructors for the entries are not run here
    engine_alloc(len * sizeof(T));
    for (unsigned int i = 0; i < max; i++) entries[i] = a.entries[i];
    return *this;
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
