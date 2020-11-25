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
#include <atomic>
#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

/* ######################################################################### */

JumpCell abort_jmp;
JumpCell interrupt_jmp;

extern "C" void clean_up();
extern "C" void system_cpuTime_init();

static bool gotArg(const char* arg, char* const * argv) {
  for (; *argv; argv++) if (0 == strcmp(arg, *argv)) return true;
  return false;
}

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
void* profFunc(ArgCell* p);
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
#if PROFILING
    std::thread profileThread(profFunc, M2_vargs);
#endif
    // testFunc(M2_vargs);
    interpFunc(M2_vargs);
  } else {
    initializeThreadSupervisor();
#if PROFILING
    struct ThreadTask* profileTask = createThreadTask("Profile", (ThreadTaskFunctionPtr)profFunc, M2_vargs, 0, 0, 0);
    pushTask(profileTask);
#endif
    struct ThreadTask* interpTask = createThreadTask("Interp", (ThreadTaskFunctionPtr)interpFunc, M2_vargs, 0, 0, 0);
    pushTask(interpTask);
    waitOnTask(interpTask);
  }
  return 0;
}

/* ######################################################################### */

std::ofstream prof_log;
thread_local std::vector<char*> M2_stack;

void stack_trace(std::ostream &stream, bool M2) {
  if(M2) {
    stream << "M2";
    for (char* M2_frame : M2_stack)
      stream << ";" << M2_frame;
    stream << std::endl;
  } else {
    stream << "-* stack trace, pid: " << (long) getpid() << std::endl;
    stream << boost::stacktrace::stacktrace();
    stream << "-- end stack trace *-" << std::endl;
  }
}

extern "C" {
  void M2_stack_trace() { stack_trace(std::cout, false); }
#if PROFILING
  void M2_stack_push(char* M2_frame) { M2_stack.emplace_back(M2_frame); }
  void M2_stack_pop() { M2_stack.pop_back(); }
#else
  void M2_stack_push(char* M2_frame) {}
  void M2_stack_pop() {}
#endif
}

void* profFunc(ArgCell* p)
{
  using namespace std::chrono_literals;
  std::string filename("profile-" + std::to_string(getpid())+ ".raw");
  // std::cerr << "Saving profile data in " << filename << std::endl;
  prof_log.open(filename, std::ios::out | std::ios::trunc );
  while(true) {
    std::this_thread::sleep_for(1000ms);
    tryGlobalTrace();
  }
  return NULL;
}

void* testFunc(ArgCell* p)
{
  // TODO: do some math here?
  printf("testfunc %p\n",p);
  return NULL;
}

extern "C" void interp_setupargv(), interp_process();
extern "C" void reverse_run(struct FUNCTION_CELL *list);
extern "C" void alarm_handler(int), interrupt_handler(int), trace_handler(int), segv_handler(int);

void* interpFunc(ArgCell* vargs)
{
  /* set this as the interpreter's thread? */
  setInterpThread();
  /* re-initialize any thread local variables */
  reverse_run(thread_prepare_list);

  /* setting variables defined in M2.dd */
  M2_envc = vargs->envc;
  M2_argc = vargs->argc;
  M2_envp = M2_tostrings(M2_envc, vargs->envp);
  M2_argv = M2_tostrings(M2_argc, vargs->argv);
  M2_args = M2_tostrings(M2_argc == 0 ? 0 : M2_argc - 1, vargs->argv + 1);
  /* setting commandLine and environment values for frontend */
  interp_setupargv();

  SETJMP(abort_jmp.addr); /* longjmp to this point when aborted */
  abort_jmp.is_set = TRUE;

  signal(SIGSEGV, segv_handler);  /* dump the stack trace and exit */
  signal(SIGUSR1, trace_handler); /* log the stack trace to file */

  /*
    process() in interp.dd is where all the action happens, however, interp__prepare()
    from interp-tmp.cc is called first. This happens even before main() because all
    "_prepare()" functions have "__attribute__ ((constructor))" in their declaration.
  */
  interp_process();

  clean_up();

#ifdef MEMDEBUG
  fprintf(stderr, "gc: heap size = %zu, free space divisor = %ld, collections = %ld\n",
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no /* -old_collections */);
#endif

  exit(0);
  return NULL;
}

/* ######################################################################### */

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

void trace_handler(int sig) {
  if (tryGlobalTrace() == 0)
    stack_trace(prof_log, true);
  oursignal(SIGUSR1,trace_handler);
}

void alarm_handler(int sig) {
  if (tryGlobalAlarm() == 0)
    interrupts_setAlarmedFlag();
  oursignal(SIGALRM,alarm_handler);
}

void segv_handler(int sig) {
  static int level;
  fprintf(stderr, "-- SIGSEGV\n");
  level ++;
  if (level > 1) {
    fprintf(stderr,"-- SIGSEGV handler called a second time, aborting\n");
    _exit(2);
  }
  stack_trace(std::cerr, false);
  level --;
  _exit(1);
}

void interrupt_handler(int sig) {
  if (tryGlobalInterrupt() == 0) {
    if (test_Field(THREADLOCAL(interrupts_interruptedFlag, struct atomic_field)) ||
                   THREADLOCAL(interrupts_interruptPending, bool)) {

      if (isatty(STDIN) && isatty(STDOUT)) {

	while (TRUE) {
	  char buf[10];
	  printf("\nExit (y=yes/n=no/a=abort/b=backtrace)? ");
	  fflush(stdout);
#ifndef NDEBUG
	  trap();
#endif
	  if (NULL == fgets(buf,sizeof(buf),stdin)) {
	    fprintf(stderr,"exiting\n");
	    exit(11);
	  }
	  if (buf[0]=='a' || buf[0]=='A') {
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
	  }
	  if (buf[0]=='b' || buf[0]=='B') {
	    stack_trace(std::cout, false);
	    fprintf(stderr,"exiting\n");
	    exit(12);
	  }
	  if (buf[0]=='y' || buf[0]=='Y') {
	    fprintf(stderr,"exiting\n");
	    exit(12);
	  }
	  if (buf[0]=='n' || buf[0]=='N') {
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

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/bin'\" && make -C $M2BUILDDIR/Macaulay2/bin M2lib.o "
// tags-file-name: "TAGS"
// End:
*/
