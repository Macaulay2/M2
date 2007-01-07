// (c) 1994  Michael E. Stillman.
#ifndef _array_hh_
#define _array_hh_

#include "style.hpp"

template <class T> class array;

const int init_array_size = 16;

template<class T>
class array : public our_new_delete
{
  T    *entries;
  int  len;
  int  max;
  void expand(int newtop) {
      int newlen = (len > 1 ? len : 1);
      for (; newtop>=newlen; newlen *= 2);
      T *tmp = newarray(T ,newlen);
      engine_alloc(newlen * sizeof(T));

      for (int j = 0; j<max; j++) tmp[j] = entries[j];
      engine_dealloc(len * sizeof(T));
      deletearray(entries);
      entries = tmp;
      len = newlen;
      }
   
public:
  array(int i_size = init_array_size) 
    : max(0)
    {
      len = (init_array_size > i_size ? init_array_size : i_size);
      entries = newarray(T,len);
      engine_alloc(len * sizeof(T));
    }
  array(const array<T> &a) : len(a.len), max(a.max)
    {
      entries = newarray(T,len);
      engine_alloc(len * sizeof(T));
      for (int i=0; i<max; i++) entries[i] = a.entries[i];
    }

  ~array() { 
    engine_dealloc(len * sizeof(T)); 
    deletearray(entries);
    }

  int  length() const { return max; }

  void shrink(int newmax) { 
       assert( newmax >= 0 );
       if (newmax < max) max = newmax;
  }

  T * get_raw_array() { return entries; }

  T &operator[](int i)
    {
      assert(i >= 0);
      if (i < max) return entries[i];
      if (i >= len) expand(i);
      max = i+1;
      return entries[i];
    }

  const T &operator[](int i) const
    {
      assert(i >= 0);
      assert(i < max);
      return entries[i];
    }

  T &rawelem(int i)
    { 
	 assert(i >= 0);
	 assert(i < max);
	 return entries[i]; 
    }

  const T &rawelem(int i) const
    { 
	 assert(i >= 0);
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
      int n = (init_array_size > a.max ? init_array_size : a.max);
      max = a.max;
      len = n;
      entries = newarray(T,len);
      engine_alloc(len * sizeof(T));
      for (int i=0; i<max; i++) entries[i] = a.entries[i];
      return *this;
    }
};

template <class T>
class array_class : public our_new_delete {
  T    *entries;
  int  len;
  int  max;
  void expand(int newtop)
   {
     int newlen = (len > 1 ? len : 1);
     for (; newtop>=newlen; newlen *= 2);
     T *tmp = new T[newlen];
     engine_alloc(newlen * sizeof(T));

     for (int j = 0; j<max; j++) tmp[j] = entries[j];
     engine_dealloc(len * sizeof(T));
     delete[] entries;
     entries = tmp;
     len = newlen;
   }
   
public:

  int  length() const { return max; }

  void shrink(int newmax) { 
       assert( newmax >= 0 );
       if (newmax < max) max = newmax;
  }

  T * get_raw_array() { return entries; }

  T &operator[](int i)
    {
      assert(i >= 0);
      if (i < max) return entries[i];
      if (i >= len) expand(i);
      max = i+1;
      return entries[i];
    }

  const T &operator[](int i) const
    {
      assert(i >= 0);
      assert(i < max);
      return entries[i];
    }

  T &rawelem(int i)
    { 
	 assert(i >= 0);
	 assert(i < max); 
	 return entries[i]; 
    }

  const T &rawelem(int i) const
    { 
	 assert(i >= 0);
	 assert(i < max); 
	 return entries[i];
    }

  void append(const T &t) 
    { 
      if (max == len) expand(max); 
      entries[max++] = t; 
    }
  array_class(int i_size = init_array_size) 
    : max(0)
    {
      len = (init_array_size > i_size ? init_array_size : i_size);
      entries = new T[len];
      engine_alloc(len * sizeof(T));
    }
  array_class(const array_class<T> &a) : len(a.len), max(a.max)
    {
      entries = new T[len];
      engine_alloc(len * sizeof(T));
      for (int i=0; i<max; i++) entries[i] = a.entries[i];
    }
 ~array_class()
    { 
      engine_dealloc(len * sizeof(T)); 
      delete[] entries; 
    }
  array_class<T> &operator=(const array<T> &a)
    {
      if (&a == this) return *this;
      engine_dealloc(len * sizeof(T));
      delete[] entries;
      int n = (init_array_size > a.max ? init_array_size : a.max);
      max = a.max;
      len = n;
      entries = new T[len];
      engine_alloc(len * sizeof(T));
      for (int i=0; i<max; i++) entries[i] = a.entries[i];
      return *this;
    }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
