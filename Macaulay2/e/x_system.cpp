// (c) 1994 Michael E. Stillman

#include "mem.hpp"
#include "interp.hpp"

#include "handles.hpp"

//////// System level commands ///////////////////////////////////////

void cmdQuit(void)
{
  exit(0);
}

void cmdMemStats(void)
{
  stash::stats(gOutput);
}
//////// Stack commands //////////////////////////////////////////////
void cmdDup(void)
{
  gStack.duplicate();
}
void cmdDuplicate(object &s)
{
  gStack.duplicate(s->int_of());
}
void cmdPick(object &s)
{
  gStack.pick(s->int_of());
}
void cmdPopOne()
{
  if (gStack.is_empty()) return;
  gStack.remove();
}
void cmdPoppem(object &s)
{
  gStack.poppem(s->int_of());
}
//////// Heap commands  //////////////////////////////////////////////
void cmdAddress(void)
{
  if (gStack.is_empty()) return;
  object r = gStack.remove();
  int h = gHandles.enlist(r);
  gStack.insert(make_object_int(h));
}
void cmdForget(object &r)
{
  gHandles.forget(r->int_of());
}
void cmdDeref(object &r)
{
  object s;
  if (gHandles.deref(r->int_of(), s))
    gStack.insert(s);
  else
    gError << "bad handle: " << r->int_of();
}
//////// Input of objects  ///////////////////////////////////////////
void cmdInt(void)
{
  mpz_t val;
  mpz_init(val);
  bin_mpz_in(val,gInput,gInputLen);
  gStack.insert(make_object_int(val));
  mpz_clear(val);
}
void cmdString(void)
{
  gStack.insert(new object_string(gInput,gInputLen));
}
void cmdIntarray(void)
{
  gStack.insert(new object_intarray(gInput,gInputLen));
}
void cmdIntarray1(void)
     // Create an intarray from elements on the stack.
     // On stack: a0 a1 a2 ... a(n-1) n >> intarray
{
  if (gStack.is_empty())
    {
      gError << "Intarray: unexpected empty stack";
      return;
    }
  if (gStack[0]->type_id() != TY_INT)
    {
      gError << "Intarray: expected integer count on stack";
      return;
    }
  int count = gStack.remove()->int_of();
  object_intarray *a = new object_intarray;
  for (int i=count-1; i >= 0 ; i--) 
    {
      if (!gStack.in_bounds(i) || gStack[i]->type_id() != TY_INT)
	{
	  gError << "Intarray: expected integer on the stack";
	  return;
	}
      a->val[count-1-i] = gStack[i]->int_of();
    }
  gStack.poppem(count);
  gStack.insert(a);
}

//////// Viewing information  ////////////////////////////////////////
void cmdVToNet()
{
  if (gStack.is_empty()) return;
  object r = gStack.remove();
  r.bin_out(gOutput);
}  
void cmdSee()
{
  if (gStack.is_empty()) return;
  object r = gStack.remove();
  r.text_out(gOutput);
}
void cmdShowStack()
{
  gStack.text_out(gOutput);
  gOutput << newline;
}
void cmdShowHeap(void)
{
  gHandles.text_out(gOutput);
  gOutput << newline;
}
//////// Informational  //////////////////////////////////////////////
void cmdIndex(object &r)
{
  if (gStack.is_empty()) return;
  int i = r->int_of();
  object s = gStack.remove();
  gStack.insert(s->index_of(i));
}
void cmdLength(void)
{
  if (gStack.is_empty()) return;
  object r = gStack.remove();
  gStack.insert(make_object_int(r->length_of()));
}
int i_sys_cmds(void) 
{
#include "cmdinst.hpp"

  // system commands
  install(ggquit, cmdQuit);
  install(ggmem, cmdMemStats);  

  // stack commands
  install(ggdup, cmdDup);
  install(ggduplicate, cmdDuplicate, TY_INT);
  install(ggpick, cmdPick, TY_INT);
  install(ggpop, cmdPopOne);
  install(ggpoppem, cmdPoppem, TY_INT);
  install(ggstack, cmdShowStack);
  
  // heap commands
  install(ggaddress, cmdAddress);
  install(ggforget, cmdForget, TY_INT);
  install(ggderef, cmdDeref, TY_INT);
  install(ggheap, cmdShowHeap);
  
  // bringing information to the engine
  install(ggINT, cmdInt);
  install(ggSTRING, cmdString);
  install(ggINTARRAY, cmdIntarray);
  
  // viewing information
  install(ggtonet, cmdVToNet);
  install(ggsee, cmdSee);
  
  // information about objects
  install(ggindex, cmdIndex, TY_INT);
  install(gglength, cmdLength);

  return 0;
}
