// (c) 1994 Michael E. Stillman

#ifndef _handles_hh_
#define _handles_hh_

#include "object.hpp"
#include "hashtab.hpp"

struct handle
{
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  int h;
  object obj;
  handle() : h(0), obj() {}
  handle(int h, const object &o) 
    : h(h), obj(o) {}
};

class handles 
{
  unsigned int next_handle;
  hashtable<handle *> objs;

public:
  handles() : next_handle(0), objs(hashtable<handle *>((handle *)0)) {}
  ~handles() {}
  
  int current() { return objs.current(); }
  int highwater() { return objs.highwater(); }
  int is_valid_handle(int h);

  int deref(int h, object &result);
      // Given the handle h, set result to the corresponding object.
      // Return 1 if h corresponds to a handle, otherwise return 0.

  int enlist(const object &o);
      // Given an object, return its handle (i.e. its address).
      // If 'o' is not already in the hashtable, place it there (refount 1)
      // Otherwise increment the handles refcount.

  void forget(int h) {
       handle *elem; 
       objs.remove(elem,h);
       delete elem;
       }
      // Find the handle in the hashtable with key 'h'.  Decrement its
      // refcount, possibly deleting it if refcount drops to zero.

  void text_out(buffer &o) const;
};

extern handles gHandles;

#endif
