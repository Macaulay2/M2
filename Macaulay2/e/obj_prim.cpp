// (c) 1994 Michael E. Stillman

#include "obj_prim.hpp"

primitive *operations[NCOMMANDS];

void install_name(int h, char *name)
{
  if (h < NCOMMANDS)
    {
      operations[h] = new primitive(name);
      engine_alloc(sizeof(primitive));
    }
}

void install(int h, cmdfcn0 cmd) 
{
  operations[h]->install(cmd);
}

void install(int h, cmdfcn1 cmd, int arg1) 
{
  operations[h]->install(cmd, arg1);
}

void install(int h, cmdfcn2 cmd, int arg1, int arg2) 
{
  operations[h]->install(cmd, arg1, arg2);
}

void install(int h, cmdfcn3 cmd, int arg1, int arg2, int arg3) 
{
  operations[h]->install(cmd, arg1, arg2, arg3);
}

void install(int h, cmdfcn4 cmd, int arg1, int arg2, int arg3, int arg4) 
{
  operations[h]->install(cmd, arg1, arg2, arg3, arg4);
}

void install(int h, cmdfcn5 cmd, int arg1, int arg2, int arg3, int arg4, int arg5) 
{
  operations[h]->install(cmd, arg1, arg2, arg3, arg4, arg5);
}

void execute(int opcode)
{
  if (opcode <= 0 || opcode >= NCOMMANDS)
    gError << "Unknown opcode: " << opcode;
  else
    operations[opcode]->execute();
}
