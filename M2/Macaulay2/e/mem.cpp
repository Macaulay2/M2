// (c) 1995  Michael E. Stillman

#include "mem.hpp"
#include "text-io.hpp"

static int allocated_amount = 0;
static int deleted_amount = 0;

int slab::n_slabs = 0;

// Array of stashes of 2^n powers.
// This looks thread unsafe, but is actually thread safe because it is only
// initialized once when the engine is setup.
doubling_stash *doubles = NULL;

stash::stash(const char *s, size_t len)
    : name(s),
      slabs(NULL),
      free_list(NULL),
      n_allocs(0),
      n_inuse(0),
      highwater(0),
      n_frees(0)
{
  // Make sure element_size is a multiple of the word size.
  if (len <= 0) len = word_size;
  element_size = word_size * ((len + word_size - 1) / word_size);
  // number of elements per slab is the slab size divided by the element size
  // rounded down.
  n_per_slab = static_cast<int>((slab_size - sizeof(void *)) / element_size);
  n_per_slab = 0;
  initializeSpinLock(&list_spinlock);
}

stash::~stash()
{
  acquireSpinLock(&list_spinlock);
  while (slabs != NULL)
    {
      slab *p = slabs;
      slabs = slabs->next;
      GC_FREE(p);  // this dramatically improves our memory usage
      // printf("removed %p\n", p);
    }
}

void stash::chop_slab()
{
  // grab a new slab, and chop it into element_size pieces, placing them
  // onto the free list.

  slab *new_slab = new slab;
  new_slab->next = slabs;
  slabs = new_slab;

  // Time to chop it up.

  char *prev = NULL;
  char *current = slabs->s;
  for (int i = 0; i < n_per_slab; i++)
    {
      *(reinterpret_cast<char **>(current)) = prev;
      prev = current;
      current += element_size;
    }
  free_list = prev;
}

void stash::text_out(buffer &o) const
// Display statistics about this stash.
{
  char s[200];
  sprintf(s,
          "%16s %9dk %9dk %10zd %10zu %10zu %10zu %10zu%s",
          name,
          static_cast<int>((element_size * highwater + 1023) / 1024),
          static_cast<int>((element_size * n_inuse + 1023) / 1024),
          element_size,
          n_allocs,
          n_inuse,
          highwater,
          n_frees,
          newline);
  o << s;
}
// TODO: MAKE THREADSAFE -- For statistics purposes only
size_t engine_allocated = 0;
// TODO: MAKE THREADSAFE -- For statistics purposes only
size_t engine_highwater = 0;

void stash::stats(buffer &o)
{
  //  o << "total space allocated from system = " << engine_allocated << endl;
  //  o << "number of global delete's  = " << engine_dealloc << endl;
  int n = (slab::n_slabs * slab_size) / 1024 +
          (allocated_amount - deleted_amount) / 1024;
  o << "size of each slab = " << sizeof(slab) << newline;
  o << "total engine stash space allocated = " << n << "k" << newline;

  if (n > 0)
    {
      char s[200];
      sprintf(s,
              "%16s %10s %10s %10s %10s %10s %10s %10s%s",
              "stash",
              "k total",
              "k in use",
              "size",
              "nalloc",
              "inuse",
              "highwater",
              "freed",
              newline);
      o << s;
    }
}

//--------- Doubling Stashes -----------------------------------------

doubling_stash::doubling_stash()
{
  int size = 2;
  for (int i = 0; i < NDOUBLES; i++)
    {
      size *= 2;
      doubles[i] = new stash("double", sizeof(int) * (size + 1));
      double_size[i] = sizeof(int) * size;
    }
}

doubling_stash::~doubling_stash()
{
  for (int i = 0; i < NDOUBLES; i++)
    {
      if (doubles[i] != NULL)
        emit("internal warning -- deleting a double stash");
      freemem(doubles[i]);
    }
}

void *doubling_stash::new_elem(size_t size)
// size is in chars
{
  // first find the correct stash
  int st = 0;
  while (double_size[st] < size) st++;

  int *result = reinterpret_cast<int *>(doubles[st]->new_elem());
  result[0] = st;
  result++;
  return reinterpret_cast<void *>(result);
}

void doubling_stash::delete_elem(void *p)
{
  if (p == NULL) return;
  int *q = (int *)p;
  q--;
  doubles[*q]->delete_elem(q);
}

size_t doubling_stash::allocated_size(void *p)
{
  int *q = reinterpret_cast<int *>(p);
  assert(q[-1] >= 0);
  assert(q[-1] <= NDOUBLES);
  return double_size[q[-1]];
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
