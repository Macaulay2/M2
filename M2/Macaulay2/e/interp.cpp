// (c) 1994 Michael E. Stillman

#include "interp.hpp"
#include "text_io.hpp"
#include "handles.hpp"
#include "error.hpp"

extern "C" void *make_string(char *,int);
extern "C" void GB_gbstart(void);
extern "C" void *GBgbprocess(char *,int);
extern "C" void GB_gbforget(int);

extern void execute(int opcode); // In primitive.cc

#include "random.hpp"
extern void i_stashes();
extern int i_text_io();
extern int i_sys_cmds();
extern void i_monoid_cmds();
extern void i_monomial_cmds();
extern void i_NGB_cmds();
extern void i_ring_elem_cmds();
extern void i_Vector_cmds();
extern void i_Matrix_cmds();
extern void i_factor_cmds();
extern int i_res_cmds();
extern int i_res2_cmds();
extern int i_gbres_cmds();

void GB_gbforget(int h)
{
  gHandles.forget(h);
}

int error_exists()
{
  return (gError->pcount() > 1);
}

void GB_gbstart(void)
     // Must be called before using the engine
{
  i_stashes();
  gOutput = new ostrstream;
  gError = new ostrstream;
  i_text_io();
  i_sys_cmds();
  i_monoid_cmds();
  i_monomial_cmds();
  i_NGB_cmds();
  i_ring_elem_cmds();
  i_Vector_cmds();
  i_Matrix_cmds();
  i_res_cmds();
  i_res2_cmds();
  i_gbres_cmds();
  i_factor_cmds();
  Random::i_random();
}

void *GBgbprocess(char *instream, int inlen)
     // The compiled-in version.
{
  void *result;

  gInput = instream;
  gInputLen = inlen;

  if (gOutput->pcount() != 1)
    {
      delete gOutput;
      gOutput = new ostrstream;
      *gOutput << '\0';
    }

  if (gError->pcount() != 1)
    {
      delete gError;
      gError = new ostrstream;
      *gError << '\1';
    }

  while (gInputLen > 0)
    {
      int opcode = bin_int_in(gInput, gInputLen);
      execute(opcode);

      if (error())
	{
	  *gError << error_message();
	  clear_error();
	}
      if (gError->pcount() > 1)
	{
	  result = make_string(gError->str(), gError->pcount());
	  gError->freeze(0);
	  return result;
	}
    }

  result = make_string(gOutput->str(), gOutput->pcount());
  gOutput->freeze(0);

  return result;
}

stack<object> gStack;
ostrstream   *gError;
ostrstream   *gOutput;
char         *gInput;
int           gInputLen;

