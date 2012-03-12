/*		Copyright 1994 by Daniel R. Grayson		*/
#include "interp-exports.h"
#include "../platform/platform.h"

#include "M2mem.h"
#include "M2inits.h"
#include "types.h"
#include "debug.h"

#include "../system/supervisorinterface.h"


static void ignore(int);

static void putstderr(const char *m) {
     ignore(write(STDERR,m,strlen(m)));
     ignore(write(STDERR,"\n",strlen("\n")));
     }

static void ignore(int x) { }

void WerrorS(const char *m) {
  putstderr(m);
  exit(1);
}

void WarnS(const char *m) {
  putstderr(m);
}



static struct COUNTER { 
     int *count; char *filename; int lineno; char *funname;
     struct COUNTER *next;
     } *counters = NULL;

int register_fun(int *count, char *filename, int lineno, char *funname) {
     struct COUNTER *p = (struct COUNTER *) getmem(sizeof(struct COUNTER));
     p->count = count;
     p->filename = filename;
     p->lineno = lineno;
     p->funname = funname;
     p->next = counters;
     counters = p;
     return 0;
     }

//Forward declaration of clean_up found later in file.
void clean_up();
/***
    Initialize readline variables?
**/
extern void init_readline_variables();
/// Not exactly clear what this is.
extern char *GC_stackbottom;
/// Not exactly clear what this is.
extern void arginits(int, const char **);
/// Not exactly clear what this is.
extern int gotArg(const char *arg, const char ** argv);
/***
    This structure is used to hold the state of the environment/program arguments.
    It contains everything needed to reconstruct the information passed into the program.
 ***/
struct saveargs
{
  int argc;
  char** argv;
  char** envp;
  int volatile envc;
};
/***
    File local copy of the save args struct.
    This is used to pass information between Macaulay2_Main and interpFunc.
**/
static struct saveargs* vargs;
/***
    This is used, given a list of initializations, to run the initializations in reverse order.
**/
static void reverse_run(struct FUNCTION_CELL *p) { if (p) { reverse_run(p->next); (*p->fun)(); } }
/***
    This has something to do with garbage collection.
***/
static char dummy;

/***
   Function that is called by task system to start the interperter.
   @param unused An unused parameter slot.
***/
void* interpFunc(void* unused)
{
  char** saveargv = vargs->argv;
  int argc = vargs->argc;
  int envc = vargs->envc;
  setInterpThread();
  // Since there are a number of thread local variables that are pointers, etc, they must be initialized.
  reverse_run(thread_prepare_list);
  // Initialize readline.
  init_readline_variables();
  arginits(argc,(const char **)vargs->argv);
  //Not exactly clear what this does.
  if (GC_stackbottom == NULL) GC_stackbottom = &dummy;
     
  M2_envp = M2_tostrings(envc,getEnviron());
  M2_argv = M2_tostrings(argc,getEnviron());
  M2_args = M2_tostrings(argc == 0 ? 0 : argc - 1, (char **)saveargv + 1);
  interp_setupargv();

  //This calls the Macaulay2 interpereter.
  //When this exits, the Macaulay2 process will begin to clean up for exit.
  interp_process(); 

  clean_up();
  //print GC statistics if needed.
  onExitGCStatistics();
  exit(0);
  return NULL;
}

/* these get put into startup.c by Makefile.in */
/***
    Entry point for Macaulay 2 code.
    @param argc Number of arguments.
    @param argv Array of string arguments.
***/
int Macaulay2_main(int argc,char** argv)
{

     int volatile envc = 0;
     void main_inits();

     char **x = getEnviron(); 
     while (*x) envc++, x++;

     system_cpuTime_init();
     
     //performWindowsNameMangling(argv);

#ifdef HAVE__SETMODE
     {
     extern void _setmode(int, int);
     _setmode(STDIN ,_O_BINARY);
     _setmode(STDOUT,_O_BINARY);
     _setmode(STDERR,_O_BINARY);
     }
#endif


     if (__gmp_allocate_func != (void *(*) (size_t))getmem_atomic) {
          FATAL("possible memory leak, gmp allocator not set up properly");
	  fprintf(stderr,"--internal warning: possible memory leak, gmp allocator not set up properly, resetting\n");
     }

     signal(SIGPIPE,SIG_IGN);
     system_handleInterruptsSetup(TRUE);
     
     vargs = GC_MALLOC_UNCOLLECTABLE(sizeof(struct saveargs));
     vargs->argv=argv;
     vargs->argc=argc;
     vargs->envp=getEnviron();
     vargs->envc = envc;


     initializeThreadSupervisor();
     struct ThreadTask* interpTask = createThreadTask("Interp",interpFunc,vargs,0,0,0);
     pushTask(interpTask);
     waitOnTask(interpTask);
     return 0;
     }
/***
   Not exactly all to clear what this does.
***/
void clean_up(void) {
     extern void close_all_dbms();
     close_all_dbms();
     while (pre_final_list != NULL) {
	  pre_final_list->fun();
	  pre_final_list = pre_final_list->next;
	  }
     while (final_list != NULL) {
	  final_list->fun();
	  final_list = final_list->next;
	  }
#    ifndef NDEBUG
     trap();
#    endif
     }

void scclib__prepare(void) {}

int system_isReady(int fd) {
  int ret;
  static fd_set r, w, e;
  struct timeval timeout;
  FD_SET(fd,&r);
  timerclear(&timeout);
  ret = select(fd+1,&r,&w,&e,&timeout);
  FD_CLR(fd,&r);
  return ret;
}

int system_hasException(int fd) {
  int ret;
  static fd_set r, w, e;
  struct timeval timeout;
  FD_SET(fd,&e);
  timerclear(&timeout);
  ret = select(fd+1,&r,&w,&e,&timeout);
  FD_CLR(fd,&e);
  return ret;
}

#include "../e/rand.h"

int system_randomint(void) {
#if 0
     extern long random();
     return random();
#elif 0
     extern long random00();
     return random00();
#else
     return rawRandomInt(2<<31-1);
#endif
     }

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d M2lib.o "
// tags-file-name: "TAGS"
// End:
*/
