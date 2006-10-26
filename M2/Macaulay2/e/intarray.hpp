// (c) 1995  Michael E. Stillman
#ifndef _intarray_hh_
#define _intarray_hh_

#include "style.hpp"

const int init_intarray_size = 16;

class intarray
{
  int *entries;
  int  len;			// Allocated length
  int  max;			// Current length
public:
  intarray() : entries(NULL), len(0), max(0) {}

  intarray(int i_size) 
    : max(0)
    {
      int n = init_intarray_size;
      if (i_size > n) n = i_size;
      entries = reinterpret_cast<int *>(doubles->new_elem(sizeof(int)*n));
      len = doubles->allocated_size(entries)/sizeof(int);
    }

  intarray(const intarray &a) : max(a.max)
    {
      if (a.len == 0)
	{ entries = NULL; len = 0; }
      else
	{
	  entries = reinterpret_cast<int *>(doubles->new_elem(sizeof(int)*a.max));
	  len = doubles->allocated_size(entries)/sizeof(int);
	  for (int i=0; i<max; i++) entries[i] = a.entries[i];
	}
    }

  ~intarray() { doubles->delete_elem(entries); entries = 0; }

  void remove() { doubles->delete_elem(entries); entries = 0; }

  void expand(int newtop);

  int  length() const { return max; }

  void shrink(int newmax) { if (newmax < max) max = newmax; }

  int operator[](int i) const
    {
      assert(i < max);
      return entries[i];
    }

  int &operator[](int i)
    {
      assert(i < max);
      return entries[i];
    }

  int *raw() { assert(entries != NULL); return entries; }
  const int *raw() const { assert(entries != NULL); return entries; }

  void append(int t) 
    { 
      if (max == len) expand(max); 
      entries[max++] = t; 
    }

  int *alloc(int extra) 
    // make room for 'extra' elements, return pointer to first,
    // and set 'max' to be max+extra.
    {
      if (len <= max+extra) expand(len+extra);
      int *result = entries + max;
      max += extra;
      return result;
    }

  int start()
    {
      int csize = max;
      append(0);
      return csize;
    }

  void end(int startval)
    {
      if (startval + 1 == max)
	max--;
      else
	entries[startval] = max - startval;
    }

  int *copy(int lngth, const int *a)
    {
      int *t = alloc(lngth);
      int *result = t;
      for (int i=0; i<lngth; i++)
	*t++ = *a++;
      return result;
    }

  intarray &operator=(const intarray &a)
    {
      if (&a == this) return *this;
      doubles->delete_elem(entries);
      int n = init_intarray_size;
      if (a.max > n) n = a.max;
      max = a.max;
      entries = reinterpret_cast<int *>(doubles->new_elem(sizeof(int)*n));
      len = doubles->allocated_size(entries)/sizeof(int);
      for (int i=0; i<max; i++) entries[i] = a.entries[i];
      return *this;
    }

  int operator==(const intarray &a) const
    {
      if (max != a.max) return 0;
      for (int i=0; i<max; i++) 
	if (entries[i] != a.entries[i])
	  return 0;
      return 1;
    }

  int operator!=(const intarray &a) const { return !(operator==(a)); }

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
