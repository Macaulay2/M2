// (c) 1994 Michael E. Stillman

#ifndef _primitive_hh_
#define _primitive_hh_

#include "object.hpp"
#include "stack.hpp"
#include "interp.hpp"

class cmd 
{
public:
  cmd *next;
  cmd(cmd *next) : next(next) {}
  virtual int check_args() = 0;  // Return 1 if the stack arguments match the
				 // function.
  virtual void execute() = 0;	 // Call the function with the stack 
				 // arguments
};

class cmd0 : public cmd 
{
  cmdfcn0 f;
public:
  cmd0(cmd *next, cmdfcn0 f) : cmd(next), f(f) {}
  int check_args() { return 1; }
  void execute() { (*f)(); }
};

class cmd1 : public cmd 
{
  cmdfcn1 f;
  int arg1;
public:
  cmd1(cmd *next, cmdfcn1 f, int arg1) : cmd(next), f(f), arg1(arg1) {}

  int check_args() 
    { 
      return (gStack.in_bounds(0) &&
	      gStack[0]->type_of() == arg1);
    }

  void execute() 
    {
      object r1 = gStack.remove();
      (*f)(r1);
    }
};

class cmd2 : public cmd 
{
  cmdfcn2 f;
  int arg1;
  int arg2;
public:
  cmd2(cmd *next, cmdfcn2 f, int arg1, int arg2) 
    : cmd(next), f(f), arg1(arg1), arg2(arg2) {}

  int check_args() 
    { 
      return (gStack.in_bounds(1) &&
	      gStack[1]->type_of() == arg1 && 
	      gStack[0]->type_of() == arg2);
    }

  void execute() 
    {
      object r2 = gStack.remove();
      object r1 = gStack.remove();
      (*f)(r1,r2);
    }
};

class cmd3 : public cmd 
{
  cmdfcn3 f;
  int arg1;
  int arg2;
  int arg3;
public:
  cmd3(cmd *next, cmdfcn3 f, int arg1, int arg2, int arg3) 
    : cmd(next), f(f), arg1(arg1), arg2(arg2), arg3(arg3) {}

  int check_args() 
    {
      return (gStack.in_bounds(2) &&
	      gStack[2]->type_of() == arg1 &&
	      gStack[1]->type_of() == arg2 &&
	      gStack[0]->type_of() == arg3);
    }

  void execute() 
    {
      object r3 = gStack.remove();
      object r2 = gStack.remove();
      object r1 = gStack.remove();
      (*f)(r1,r2,r3);
    }
};

class cmd4 : public cmd 
{
  cmdfcn4 f;
  int arg1;
  int arg2;
  int arg3;
  int arg4;
public:
  cmd4(cmd *next, cmdfcn4 f, int arg1, int arg2, int arg3, int arg4) 
    : cmd(next), f(f), arg1(arg1), arg2(arg2), arg3(arg3), arg4(arg4) {}

  int check_args() 
    {
      return (gStack.in_bounds(3) &&
	      gStack[3]->type_of() == arg1 &&
	      gStack[2]->type_of() == arg2 &&
	      gStack[1]->type_of() == arg3 &&
	      gStack[0]->type_of() == arg4);
    }

  void execute() 
    {
      object r4 = gStack.remove();
      object r3 = gStack.remove();
      object r2 = gStack.remove();
      object r1 = gStack.remove();
      (*f)(r1,r2,r3,r4);
    }
};

class cmd5 : public cmd 
{
  cmdfcn5 f;
  int arg1;
  int arg2;
  int arg3;
  int arg4;
  int arg5;
public:
  cmd5(cmd *next, cmdfcn5 f, int arg1, int arg2, int arg3, int arg4, int arg5) 
    : cmd(next), f(f), arg1(arg1), arg2(arg2), arg3(arg3), 
      arg4(arg4), arg5(arg5) {}

  int check_args() 
    {
      return (gStack.in_bounds(4) &&
	      gStack[4]->type_of() == arg1 &&
	      gStack[3]->type_of() == arg2 &&
	      gStack[2]->type_of() == arg3 &&
	      gStack[1]->type_of() == arg4 &&
	      gStack[0]->type_of() == arg5);
    }

  void execute() 
    {
      object r5 = gStack.remove();
      object r4 = gStack.remove();
      object r3 = gStack.remove();
      object r2 = gStack.remove();
      object r1 = gStack.remove();
      (*f)(r1,r2,r3,r4,r5);
    }
};

class primitive
{
  char * _name;
  cmd  * _fcns;
public:
    primitive(char *name)
      : _name(name), 
        _fcns(0) {}

  ~primitive() {}
  
  void install(cmdfcn0 cmd)
    { _fcns = new cmd0(_fcns, cmd); engine_alloc(sizeof(cmd0)); }
  
  void install(cmdfcn1 cmd, int arg1)
    { _fcns = new cmd1(_fcns, cmd, arg1); engine_alloc(sizeof(cmd1)); }
  
  void install(cmdfcn2 cmd, int arg1, int arg2)
    { _fcns = new cmd2(_fcns, cmd, arg1, arg2); engine_alloc(sizeof(cmd2)); }
  
  void install(cmdfcn3 cmd, int arg1, int arg2, int arg3)
    { _fcns = new cmd3(_fcns, cmd, arg1, arg2, arg3); 
      engine_alloc(sizeof(cmd3)); }
  
  void install(cmdfcn4 cmd, int arg1, int arg2, int arg3, int arg4)
    { _fcns = new cmd4(_fcns, cmd, arg1, arg2, arg3, arg4); 
      engine_alloc(sizeof(cmd4)); }

  void install(cmdfcn5 cmd, int arg1, int arg2, int arg3, int arg4, int arg5)
    { _fcns = new cmd5(_fcns, cmd, arg1, arg2, arg3, arg4, arg5); 
      engine_alloc(sizeof(cmd5)); }
  
  void execute() 
    {
      for (cmd *fs = _fcns; fs != NULL; fs = fs->next)
	if (fs->check_args())
	  {
	    fs->execute();
	    return;
	  }
      gError << "Incorrect stack arguments for command '" << _name << "' ";
    }
};

#endif
