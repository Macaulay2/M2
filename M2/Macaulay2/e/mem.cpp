// (c) 1995  Michael E. Stillman

#include "style.hpp"
#include "mem.hpp"
#include "text_io.hpp"

static int allocated_amount = 0;
static int deleted_amount = 0;

const int trace_bad_deletes = 0;

int slab::n_slabs = 0;
stash *stash::stash_list = NULL;

doubling_stash *doubles = NULL;

void out_of_memory()
{
  emit_line("Engine: out of memory.  Bye bye... ");
  exit(0);
}

stash::stash(char *s, size_t len)
: next(NULL), name(s), slabs(NULL), free_list(NULL),
  n_allocs(0), n_inuse(0), highwater(0), n_frees(0)
{
  // Make sure element_size is a multiple of the word size.
  if (len <= 0) len = word_size;
  element_size = word_size * ((len + word_size - 1) / word_size);
  n_per_slab = (slab_size - sizeof(void *)) / element_size;
  this->next = stash_list;
  stash_list = this;
}

stash::~stash()
{
  while (slabs != NULL)
    {
      slab *p = slabs;
      slabs = slabs->next;
      delete p;
    }
  assert(stash_list != NULL);
  if (stash_list == this)
    stash_list = next;
  else 
    {
      for (stash *q = stash_list; q->next != NULL; q = q->next)
	if (q->next == this)
	  {
	    q->next = next;
	    next = NULL;
	    return;
	  }
      assert(0);
    }
}

void *stash::new_elem()
     // Allocate space for an object from this stash.
{
  n_allocs++;
  n_inuse++;
  if (n_inuse > highwater) highwater = n_inuse;
  if (free_list == NULL) 
    {
      if (n_per_slab == 0) 
	{
	  void *result = new char[element_size];
	  if (result == NULL) out_of_memory();
	  allocated_amount += element_size;
	  return result;
	}
      chop_slab();
    }
  assert(free_list != NULL);	// chop_slab should not let this happen.
  void *result = free_list;
  free_list = *((void **) free_list);
  return result;
}

void stash::delete_elem(void *p)
     // Delete the object 'p', placing it on the free list for this stash.
{
  if (p == NULL) return;
  if (trace_bad_deletes)
    {
      for (void *q = free_list; q != NULL; q = *((void **) q))
	if (q == p)
	  assert(0);
    }

  n_inuse--;
  n_frees++;
  if (n_per_slab == 0)
    {
      deleted_amount += element_size;
      char *q = (char *)p;
      delete [] q;
      return;
    }
  *((void **) p) = free_list;
  free_list = p;
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
  for (int i=0; i<n_per_slab; i++)
    {
      *((char **) current) = prev;
      prev = current;
      current += element_size;
    }
  free_list = prev;
}

void stash::text_out(buffer &o) const
    // Display statistics about this stash.
{
  char s[200];
  sprintf(s, "%16s %9dk %9dk %10d %10d %10d %10d %10d%s",
	  name, 
	  (int)((element_size * highwater + 1023)/1024),
	  (int)((element_size * n_inuse + 1023)/1024),
	  (int)element_size,
	  n_allocs,
	  n_inuse,
	  highwater,
	  n_frees,
	  newline);
  o << s;
}

unsigned int engine_allocated = 0;
unsigned int engine_highwater = 0;

void stash::stats(buffer &o)
{
//  o << "total space allocated from system = " << engine_allocated << endl;
//  o << "number of global delete's  = " << engine_dealloc << endl;
  int n = (slab::n_slabs*slab_size)/1024 + 
    (allocated_amount - deleted_amount)/1024;
  o << "total engine space allocated = " 
    << n << "k" << newline;

  char s[200];
  sprintf(s, "%16s %10s %10s %10s %10s %10s %10s %10s%s",
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

  for (stash *p = stash_list; p != NULL; p = p->next)
    //    if (p->n_allocs > 0)
      p->text_out(o);

}

//--------- Doubling Stashes -----------------------------------------

doubling_stash::doubling_stash()
{
  int size = 2;
  for (int i=0; i<NDOUBLES; i++)
    {
      size *= 2;
      doubles[i] = new stash("double", sizeof(int)*(size+1));
      double_size[i] = sizeof(int)*size;
    }
}

doubling_stash::~doubling_stash()
{
  for (int i=0; i<NDOUBLES; i++)
    {
      if (doubles[i] != NULL)
	emit("internal warning -- deleting a double stash");
      delete doubles[i];
    }
}

void *doubling_stash::new_elem(size_t size)
     // size is in chars
{
  // first find the correct stash
  int st = 0;
  while (double_size[st] < size) st++;

  int *result = (int *) doubles[st]->new_elem();
  result[0] = st;
  result++;
  return (void *) result;
}

void doubling_stash::delete_elem(void *p)
{
  /*
  if (p == NULL) return;
  int *q = (int *) p;
  q--;
  doubles[*q]->delete_elem(q);
  */
}

size_t doubling_stash::allocated_size(void *p)
{
  int *q = (int *) p;
  assert(q[-1] >= 0);
  assert(q[-1] <= NDOUBLES);
  return double_size[q[-1]];
}

//--------- Use GC routines ----------------------------

#include <gc.h>

extern "C" void outofmem();

// the new way:

void* operator new( size_t size ) {
  void *p = GC_MALLOC( size );
  if (p == NULL) outofmem();
  return p;
}

void operator delete( void* obj ) {
  if (obj != NULL) GC_FREE( obj );
}

void* operator new []( size_t size ) {
  void *p = GC_MALLOC( size );
  if (p == NULL) outofmem();
  return p;
}

void operator delete []( void* obj ) {
  if (obj != NULL) GC_FREE( obj );
}


// the old way:
#if 0

// g++ -g -O2 -Wall -Wshadow -Wcast-qual -Wno-parentheses -O3 -fexpensive-optimizations -Wno-shadow -Wno-cast-qual  -DDEBUG -I/capybara/include -I/capybara/encap/Macaulay2-0.9.5/include -I../../include -I../util -I.  -c -o mem.o ../../../Macaulay2/e/mem.cpp
// /tmp/ccYQ57ad.s: Assembler messages:
// /tmp/ccYQ57ad.s:2492: Error: symbol `__builtin_new' is already defined
// /tmp/ccYQ57ad.s:2543: Error: symbol `__builtin_delete' is already defined
// /tmp/ccYQ57ad.s:2581: Error: symbol `__builtin_vec_new' is already defined
// /tmp/ccYQ57ad.s:2632: Error: symbol `__builtin_vec_delete' is already defined

void* __builtin_new( unsigned int size ) {
  void *p = GC_MALLOC( size );
  if (p == NULL) outofmem();
  return p;
}

void __builtin_delete( void* obj ) {
  if (obj != NULL) GC_FREE( obj );
}

void* __builtin_vec_new( unsigned int size ) {
  void *p = GC_MALLOC( size );
  if (p == NULL) outofmem();
  return p;
}

void __builtin_vec_delete( void* obj ) {
  if (obj != NULL) GC_FREE( obj );
}

#endif
