// (c) 1994 Michael E. Stillman

#ifndef _interpreter_hh_
#define _interpreter_hh_

#include "stack.hpp"
#include "obj_int.hpp"
#include "obj_str.hpp"
#include "obj_iarr.hpp"

#include "object.hpp"
#include "cmdnames.hpp"

extern stack<object> gStack;

typedef void (*cmdfcn0)();
typedef void (*cmdfcn1)(object &);
typedef void (*cmdfcn2)(object &,object &);
typedef void (*cmdfcn3)(object &,object &,object &);
typedef void (*cmdfcn4)(object &,object &,object &,object &);
typedef void (*cmdfcn5)(object &,object &,object &,object &,object &);

void install_name(int h, char *name);
void install(int h, cmdfcn0 cmd);
void install(int h, cmdfcn1 cmd, int arg1);
void install(int h, cmdfcn2 cmd, int arg1, int arg2);
void install(int h, cmdfcn3 cmd, int arg1, int arg2, int arg3);
void install(int h, cmdfcn4 cmd, int arg1, int arg2, int arg3, int arg4);
void install(int h, cmdfcn5 cmd, int arg1, int arg2, int arg3, int arg4, int arg5);

int error_exists();

#endif
