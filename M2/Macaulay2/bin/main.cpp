#ifndef EXPERIMENT
extern "C" int Macaulay2_main(const int argc, const char * const* argv);

int main(int argc, char **argv)
{
  return Macaulay2_main(argc, argv);
}
#else

/*
 * these two macros affect the definition of GC_INIT, but have to
 * appear before the include directives, in order to take effect
 */
#define GC_FREE_SPACE_DIVISOR 12
#define GC_INITIAL_HEAP_SIZE 70000000


#define BOOST_STACKTRACE_USE_ADDR2LINE /* show source file and line number */
// #define BOOST_STACKTRACE_USE_NOOP /* disable stacktrace */

#include <M2/gc-include.h>

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

struct Args
{
  int argc, envc;
  const char * const* argv;
  const char * const* envp;
};

extern "C" void clean_up();
extern "C" void system_cpuTime_init();
extern "C" bool gotArg(const char *arg, const char * const* argv);

/* ######################################################################### */

void* interpFunc(Args* vargs);
void* testFunc(Args* p);

int main(const int argc, const char * * argv)
{
  /* find the number of environment variables defined */
  int envc = -1;
  while (environ[++envc] != NULL) { /* iterate over environ until you hit NULL */ }

  GC_INIT();
  IM2_initialize();

  system_cpuTime_init();

  signal(SIGPIPE,SIG_IGN); /* ignore the broken pipe signal */
  rl_catch_signals = FALSE; /* tell readline not to catch signals, such as SIGINT */

  static struct Args* M2_vargs;
  M2_vargs = (Args*) GC_MALLOC_UNCOLLECTABLE(sizeof(struct Args));
  M2_vargs->argv = argv; /* argument vector */
  M2_vargs->argc = argc; /* argument count */
  M2_vargs->envp = environ; /* environment pointer */
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

void stack_trace() {
  std::cout << "-* stack trace, pid: " << (long) getpid() << std::endl;
  std::cout << boost::stacktrace::stacktrace();
  std::cout << "-- end stack trace *-" << std::endl;
}

/* static */ void segv_handler(int sig) {
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

void* interpFunc(Args* vargs)
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

#endif /* !EXPERIMENTAL */

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d M2lib.o "
// tags-file-name: "TAGS"
// End:
*/
