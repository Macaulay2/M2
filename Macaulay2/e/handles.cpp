// (c) 1994 Michael E. Stillman

#include "handles.hpp"
#include "interp.hpp"

handles gHandles;
stash *handle::mystash;

int handles::is_valid_handle(int h)
{
  handle *hand;
  return objs.search(hand,h);
}

int handles::deref(int h, object &result)
     // Given the handle h, set result to the corresponding object.
     // Return 1 if h corresponds to a handle, otherwise return 0.
{
  handle *hand;
  int found = objs.search(hand,h);
  if (found) result = hand->obj;
  return found;
}

#ifndef NDEBUG
int traphandle = -1;
#endif

extern "C" void trap(void);

int handles::enlist(const object &o)
     // Given an object, return its handle (i.e. its address).
     // If 'o' is not already in the hashtable, place it there (refount 1)
     // Otherwise increment the handles refcount.
{
  handle *hand;
  int h = next_handle++;
#ifndef NDEBUG
  if (h == traphandle) trap();
#endif
  hand = new handle(h,o);
  objs.insert(hand, h);
  return h;
}

void handles::text_out(buffer &o) const
{
  //gHandles.objs.debug_display(cout);
  o << "--- heap ---------" << newline;
  o << "handle refcount type" << newline;
  int nnulls = 0;
  int nnullobjs = 0;
  for (cursor_hashtable<handle *> i(gHandles.objs); i.valid(); ++i)
    {
      const handle *hand = i.elem();
      if (hand == NULL)
	{
	  nnulls++;
	  continue;
	}
      o << i.key() << '\t';
      if (!(hand->obj.is_null()))
	o << ' ' << hand->obj->refcount << '\t' << hand->obj->type_name();
      else nnullobjs++;
      o << newline;
    }
  o << "Number of null entries in table = " << nnulls << newline;
  o << "Number of null objects in table = " << nnullobjs << newline;
}

extern "C"
void prhandles(handles *h)
{
  buffer o;
  h->text_out(o);
  emit(o.str());
}
