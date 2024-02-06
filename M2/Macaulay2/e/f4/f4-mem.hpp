// Copyright 2007 Michael E. Stillman

#ifndef __f4mem_h_
#define __f4mem_h_

#include "newdelete.hpp"
#include "f4/moninfo.hpp"  // only for monomial_word

typedef int *f4vec;

#define DOUBLESIZE 30
extern const unsigned int doublesize[DOUBLESIZE];

#define SIZE_MASK 0x07ffffff
#define STASH_SHIFT (8 * sizeof(int) - 5)
#define STASH(a) (a >> STASH_SHIFT)

class F4Vec
{
  // Format for the items pointed to:
  //  a[-1] = (top 5 bits: which stash this belongs to)
  //           (rest of the bits: current size of the array)
  const char *name;

  int nreallocs;
  int nallocs[DOUBLESIZE];
  int highwater[DOUBLESIZE];
  int current[DOUBLESIZE];
  int ndeallocs[DOUBLESIZE];

  int find_alloc_size(int sz)
  {
    sz++;
    int s = 0;
    while (doublesize[s] < sz) s++;
    return s;
  }

 public:
  void reset()
  {
    nreallocs = 0;
    for (int i = 0; i < DOUBLESIZE; i++) nallocs[i] = 0;
    for (int i = 0; i < DOUBLESIZE; i++) highwater[i] = 0;
    for (int i = 0; i < DOUBLESIZE; i++) current[i] = 0;
    for (int i = 0; i < DOUBLESIZE; i++) ndeallocs[i] = 0;
  }

  F4Vec(const char *name0) : name(name0) { reset(); }
  ~F4Vec() { name = 0; }
  int size(f4vec a) { return (SIZE_MASK & (a[-1])); }
  int alloc_stash(f4vec a) { return (a[-1]) >> STASH_SHIFT; }
  f4vec allocate(int alloc_sz)
  {
    if (alloc_sz == 0) return 0;
    int s = find_alloc_size(alloc_sz);
    int nextlen = doublesize[s];
    nallocs[s]++;
    current[s]++;
    if (highwater[s] < current[s]) highwater[s] = current[s];
    int *result = newarray_atomic(int, nextlen);
    result[0] = (s << STASH_SHIFT) | alloc_sz;
    return (result + 1);
  }

  void deallocate(f4vec &a)
  {
    if (a == 0) return;
    int s = alloc_stash(a);
    ndeallocs[s]++;
    current[s]--;
    freemem(a - 1);
    a = 0;
  }

  void clear(f4vec &a)
  {
    // Don't change the amount allocated, but set the size back to 0.
    // (and zero out the array (or place garbage in there...)
    int sz = size(a);
    for (int i = sz; i > 0; i--) a[i] = 0x03030303;
    a[-1] -= sz;
  }

  void reallocate(f4vec &a, int newsize)
  {
    nreallocs++;
    f4vec newa = allocate(newsize);
    int oldsize = size(a);
    int *oldp = a;
    int *p = newa;
    for (int i = oldsize; i > 0; i--) *p++ = *oldp++;
    newa[-1] += (newsize - oldsize);
    deallocate(a);
    a = p;
  }

  void resize(f4vec &a, int newsize)
  {
    // grow 'a' to have size 'newsize'.
    // If the size becomes too large for a to handle
    // then reallocation is done.
    int s = alloc_stash(a);
    if (doublesize[s] < newsize)
      reallocate(a, newsize);
    else
      a[-1] += (newsize - size(a));
  }

  void grow(f4vec &a, int ntoadd) { resize(a, size(a) + ntoadd); }
  void show();
};

class F4Mem : public our_new_delete
{
  long monom_alloc;
  long monom_total;  // number of ints total asked for
  long monom_freed;  // number of ints deallocated
  long monom_dealloc;
  long monom_highwater;
  long monom_current;

 public:
  F4Vec components;
  F4Vec coefficients;

  F4Mem();

  monomial_word *allocate_monomial_array(int len)
  {
    if (len == 0) return 0;
    monom_alloc++;
    monom_total += len;
    monom_current += len;
    if (monom_highwater < monom_current) monom_highwater = monom_current;
    monomial_word *result = newarray_atomic(monomial_word, len);
    return result;
  }

  void deallocate_monomial_array(monomial_word *&a, int len)
  {
    if (len == 0) return;
    monom_dealloc++;
    monom_freed += len;
    monom_current -= len;
    freemem(a);
    a = 0;
  }

  void show();
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
