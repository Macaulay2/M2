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

int handles::enlist(const object &o)
     // Given an object, return its handle (i.e. its address).
     // If 'o' is not already in the hashtable, place it there (refount 1)
     // Otherwise increment the handles refcount.
{
  handle *hand;
  int h = next_handle++;
  hand = new handle(h,o);
  objs.insert(hand, h);
  return h;
}

void handles::forget(int h)
     // Find the handle in the hashtable with key 'h'.  Decrement its
     // refcount, possibly deleting it if refcount drops to zero.
{
  handle *hand;
  if (!objs.search(hand, h))
    {
      //*gError << "attempting to forget an invalid engine handle";
      cerr << "attempting to remove " << h << endl;
    }
  else
    {
      //cout << "removing " << h << endl;
      objs.remove(h);
      delete hand;
    }
  //else
  //  cout << "decremented " << h << '(' << hand->refcount << ')' << endl;
}

void handles::text_out(ostream &o) const
{
  //gHandles.objs.debug_display(cout);
  o << "--- heap ---------" << endl;
  o << "handle refcount type" << endl;
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
      o << i.index() << '\t';
      if (!(hand->obj.is_null()))
	o << ' ' << hand->obj->refcount << '\t' << hand->obj->type_name();
      else nnullobjs++;
      o << endl;
    }
  o << "Number of null entries in table = " << nnulls << endl;
  o << "Number of null objects in table = " << nnullobjs << endl;
}
