/*
 * these two macros affect the definition of GC_INIT, but have to
 * appear before the include directives, in order to take effect
 */
#define GC_FREE_SPACE_DIVISOR 12
#define GC_INITIAL_HEAP_SIZE 70000000

#define BOOST_STACKTRACE_USE_ADDR2LINE /* show source file and line number */
// #define BOOST_STACKTRACE_USE_NOOP /* disable stacktrace */

#include <M2/gc-include.h>

#include "interp-exports.h"

#include "M2mem.h"
#include "types.h"
#include "debug.h"

#include "engine.h" /* to get IM2_initialize() : */
#include "supervisorinterface.h"

#include <gdbm.h>
#include <mpfr.h>
#include <readline/readline.h>

#include <boost/stacktrace.hpp>
#include <iostream>

/* ######################################################################### */

JumpCell abort_jmp;
JumpCell interrupt_jmp;

extern "C" void clean_up();
extern "C" void system_cpuTime_init();
extern "C" bool gotArg(const char *arg, const char * const* argv);

extern "C" void interrupts_clearInterruptFlag();
extern "C" void interrupts_clearAlarmedFlag();
extern "C" void interrupts_determineExceptionFlag();

extern int have_arg_no_int;

extern int tokens_stopIfError_id;

bool tokens_stopIfError;
bool interrupts_interruptPending;
bool interrupts_interruptShield;

/* ######################################################################### */

void* interpFunc(ArgCell* vargs);
void* testFunc(ArgCell* p);

int main(/* const */ int argc, /* const */ char *argv[], /* const */ char *env[])
{
  /* find the number of environment variables defined */
  int envc = -1;
  while (env[++envc] != NULL) { /* iterate over environ until you hit NULL */ }

  GC_INIT();
  IM2_initialize();

#ifndef NDEBUG
  trap(); /* we call trap() once so variables (such as trapset) can be set */
#endif

  system_cpuTime_init();

#ifdef WITH_PYTHON
  Py_SetProgramName(argv[0]);
  Py_Initialize();
#endif

  abort_jmp.is_set = FALSE;
  interrupt_jmp.is_set = FALSE;

  signal(SIGPIPE,SIG_IGN); /* ignore the broken pipe signal */
  rl_catch_signals = FALSE; /* tell readline not to catch signals, such as SIGINT */

  static struct ArgCell* M2_vargs;
  M2_vargs = (ArgCell*) GC_MALLOC_UNCOLLECTABLE(sizeof(struct ArgCell));
  M2_vargs->argv = argv; /* argument vector */
  M2_vargs->argc = argc; /* argument count */
  M2_vargs->envp = env; /* environment pointer */
  M2_vargs->envc = envc; /* environment count */

  if (gotArg("--no-threads", argv)) {
    // testFunc(M2_vargs);
    interpFunc(M2_vargs);
  } else {
    initializeThreadSupervisor();
    struct ThreadTask* interpTask = createThreadTask("Interp", (ThreadTaskFunctionPtr) interpFunc, M2_vargs, 0, 0, 0);
    pushTask(interpTask);
    waitOnTask(interpTask);
  }
  return 0;
}

/* ######################################################################### */

extern "C" void stack_trace() {
  std::cout << "-* stack trace, pid: " << (long) getpid() << std::endl;
  std::cout << boost::stacktrace::stacktrace();
  std::cout << "-- end stack trace *-" << std::endl;
}

void segv_handler(int sig) {
  static int level;
  fprintf(stderr, "-- SIGSEGV\n");
  level ++;
  if (level > 1) {
    fprintf(stderr,"-- SIGSEGV handler called a second time, aborting\n");
    _exit(2);
  }
  stack_trace();
  level --;
  _exit(1);
}

static void reverse_run(struct FUNCTION_CELL *p) { if (p) { reverse_run(p->next); (*p->fun)(); } }

void* testFunc(void* p)
{
  // TODO: do some math here?
  printf("testfunc %p\n",p);
  return NULL;
}

extern "C" void interp_setupargv();
extern "C" void interp_process();

void* interpFunc(ArgCell* vargs)
{
  setInterpThread();
  /* re-initialize any thread local variables */
  reverse_run(thread_prepare_list);

  /* setting variables defined in M2.dd */
  M2_envc = vargs->envc;
  M2_argc = vargs->argc;
  M2_envp = M2_tostrings(M2_envc, (char **) vargs->envp);
  M2_argv = M2_tostrings(M2_argc, (char **) vargs->argv);
  M2_args = M2_tostrings(M2_argc == 0 ? 0 : M2_argc - 1, (char **) vargs->argv + 1);
  /* setting commandLine and environment values for frontend */
  interp_setupargv();

  SETJMP(abort_jmp.addr);
  abort_jmp.is_set = TRUE;

  signal(SIGSEGV, segv_handler);

  interp_process(); /* this is where all the action happens, see interp.d, where it is called simply process */

  clean_up();

#ifdef MEMDEBUG
  fprintf(stderr, "gc: heap size = %d, free space divisor = %ld, collections = %ld\n",
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no-old_collections);
#endif

  exit(0);
  return NULL;
}

/* ######################################################################### */

extern "C" void alarm_handler(int sig), interrupt_handler(int sig);
extern "C" void oursignal(int sig, void (*handler)(int)) {
 #ifdef HAVE_SIGACTION
  struct sigaction act;
  act.sa_flags = 0;	/* no SA_RESTART */
  act.sa_handler = handler;
  sigemptyset(&act.sa_mask);
  sigfillset(&act.sa_mask);
  sigaction(sig,&act,NULL);
 #else
  signal(sig,handler);
 #endif
}

extern "C" void alarm_handler(int sig) {
  if (tryGlobalAlarm() == 0) {
    interrupts_setAlarmedFlag();
  }
  oursignal(SIGALRM,alarm_handler);
}

#undef ABORT
#define ABORT 1

extern "C" void interrupt_handler(int sig) {
  if (tryGlobalInterrupt() == 0) {
    if (test_Field(THREADLOCAL(interrupts_interruptedFlag, struct atomic_field)) ||
                   THREADLOCAL(interrupts_interruptPending, bool)) {

      if (isatty(STDIN) && isatty(STDOUT)) {

	while (TRUE) {
	  char buf[10];
#ifndef ABORT
	  printf("\nExit (y=yes/n=no/b=backtrace)? ");
#else
	  printf("\nAbort (y/n)? ");
#endif
	  fflush(stdout);
	  if (NULL == fgets(buf,sizeof(buf),stdin)) {
	    fprintf(stderr,"exiting\n");
	    exit(11);
	  }
	  if (buf[0]=='b' || buf[0]=='B') {
	    stack_trace();
	    fprintf(stderr,"exiting\n");
	    exit(12);
	  }
	  if (buf[0]=='y' || buf[0]=='Y') {
#ifndef NDEBUG
	    trap();
#endif
#ifndef ABORT
	    fprintf(stderr,"exiting\n");
	    exit(12);
#endif
	    if (!tokens_stopIfError && abort_jmp.is_set) {
	      fprintf(stderr,"returning to top level\n");
	      fflush(stderr);

	      interrupts_clearAlarmedFlag();
	      interrupts_clearInterruptFlag();

	      interrupts_interruptPending = FALSE;
	      interrupts_interruptShield = FALSE;

	      interrupts_determineExceptionFlag();
	      LONGJUMP(abort_jmp.addr);
	    }
	  } else if (buf[0]=='n' || buf[0]=='N') {
	    break;
	  }
	}

      } else {
#ifndef NDEBUG
	trap();
#endif
	exit(13);
      }

    } else {

      if (THREADLOCAL(interrupts_interruptShield, bool)) {
	THREADLOCAL(interrupts_interruptPending, bool) = TRUE;
      } else {
	if (THREADLOCAL(tokens_stopIfError, bool)) {
	  int interruptExit = 2; /* see also interp.d */
	  fprintf(stderr,"interrupted, stopping\n");
	  _Exit(interruptExit);
	}
	interrupts_setInterruptFlag();
	if (interrupt_jmp.is_set) LONGJUMP(interrupt_jmp.addr);
      }

    }
  }
  oursignal(SIGINT,interrupt_handler);
}


#if 0 /* TODO: what was this used to be for? */
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
#endif

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/bin'\" && make -C $M2BUILDDIR/Macaulay2/bin M2lib.o "
// tags-file-name: "TAGS"
// End:
*/
