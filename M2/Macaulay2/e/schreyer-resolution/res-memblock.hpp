// Copyright 2004-2016 Michael E. Stillman
#ifndef __res_memblock_hpp_
#define __res_memblock_hpp_

template <typename T, long int NSLAB = 4092>
class ResMemoryBlock
{
  struct slab
  {
    slab *next;
    T block[NSLAB];
  };

  slab *first_slab;
  slab *current_slab;
  slab *last_slab;
  T *next_free; /* points into current_slab */

 private:
  slab *new_slab();

 public:
  ResMemoryBlock();
  ~ResMemoryBlock();

  void reset();

  T *reserve(int len);       // returns space for len T's.
  void intern(int len);      // increments
  T *allocate(int len = 1);  // reserve and intern

  int n_slabs() const;

  long memoryUsage()
      const;  // total number of bytes allocated in slabs (plus size of this)
};

////////////////////
// ResMemoryBlock //
////////////////////

template <typename T, long int NSLAB>
ResMemoryBlock<T, NSLAB>::ResMemoryBlock()
    : first_slab(nullptr), current_slab(nullptr), last_slab(nullptr), next_free(nullptr)
{
  first_slab = new_slab();
  current_slab = first_slab;
  last_slab = first_slab;
  next_free = current_slab->block;
}

template <typename T, long int NSLAB>
ResMemoryBlock<T, NSLAB>::~ResMemoryBlock()
{
  // Destroy the slabs one by one
  while (first_slab != nullptr)
    {
      slab *tmp = first_slab;
      first_slab = first_slab->next;
      delete tmp;
    }

  current_slab = nullptr;
  last_slab = nullptr;
  next_free = nullptr;
}

template <typename T, long int NSLAB>
typename ResMemoryBlock<T, NSLAB>::slab *ResMemoryBlock<T, NSLAB>::new_slab()
{
  slab *result = new slab;
  result->next = nullptr;
  return result;
}

template <typename T, long int NSLAB>
void ResMemoryBlock<T, NSLAB>::reset()
{
  current_slab = first_slab;
  next_free = current_slab->block;
}

template <typename T, long int NSLAB>
T *ResMemoryBlock<T, NSLAB>::reserve(int len)
{
  if (next_free + len > current_slab->block + NSLAB)
    {
      if (current_slab->next == nullptr)
        {
          last_slab->next = new_slab();
          last_slab = last_slab->next;
          current_slab = last_slab;
        }
      else
        {
          current_slab = current_slab->next;
        }
      next_free = current_slab->block;
    }
  return next_free;
}

template <typename T, long int NSLAB>
void ResMemoryBlock<T, NSLAB>::intern(int len)
{
  next_free += len;
}

template <typename T, long int NSLAB>
T *ResMemoryBlock<T, NSLAB>::allocate(int len)
{
  T *result = reserve(len);
  next_free += len;
  return result;
}

template <typename T, long int NSLAB>
int ResMemoryBlock<T, NSLAB>::n_slabs() const
{
  int result = 0;
  for (slab *p = first_slab; p != nullptr; p = p->next) result++;
  return result;
}

template <typename T, long int NSLAB>
long ResMemoryBlock<T, NSLAB>::memoryUsage() const
{
  long result = sizeof(*this);
  result += n_slabs() * sizeof(slab);
  return result;
}

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
