// (c) 1994 Michael E. Stillman

#include "interp.hpp"
#include "text_io.hpp"
#include "handles.hpp"

extern "C" {
void *make_string(char *,int);
void GB_gbstart(void);
void *GBgbprocess(char *,int);
void GB_gbforget(int);
}

extern void execute(int opcode); // In primitive.cc

#include "random.hpp"
extern void i_stashes();
extern int i_text_io();
extern int i_sys_cmds();
extern void i_Ecommands();
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
extern void i_EGB();
void GB_gbforget(int h)
{
  gHandles.forget(h);
}

int error_exists()
{
  return (gError.size() > 1) || error();
}

void GB_gbstart(void)
     // Must be called before using the engine
{
  i_stashes();
  i_text_io();
  i_sys_cmds();
  i_Ecommands();
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
#if defined(ARING)
  extern void i_ARING();
  i_ARING();
#endif
  i_EGB();
}

void *GBgbprocess(char *instream, int inlen)
     // The compiled-in version.
{
  void *result;

  gInput = instream;
  gInputLen = inlen;

  gOutput.reset();
  gOutput.put('\0');
  gError.reset();
  gError.put('\1');

  while (gInputLen > 0)
    {
      clear_emit_size();  // Only for verbose output...
      int opcode = bin_int_in(gInput, gInputLen);
      execute(opcode);

      if (error())
	{
	  gError << error_message();
	  clear_error();
	}
      if (gError.size() > 1)
	{
	  result = make_string(gError.str(), gError.size());
	  return result;
	}
    }

  result = make_string(gOutput.str(), gOutput.size());
  return result;
}

stack<object> gStack;
buffer        gError;
buffer        gOutput;
char         *gInput;
int           gInputLen;

extern "C" int M2main(int argc,char **argv);

int main(int argc,char **argv)
{
  return M2main(argc,argv);
}
