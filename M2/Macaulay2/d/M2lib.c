/*		Copyright 1994 by Daniel R. Grayson		*/

#include "interp-exports.h"

/* defining GDBM_STATIC makes the cygwin version work, and is irrelevant for the other versions */
#define GDBM_STATIC
#include <gdbm.h>

#include "M2mem.h"
#include "M2inits.h"
#include "../dumpdata/map.h"
#include "types.h"
#include "debug.h"

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else
#ifdef __GNUC__
#ifndef alloca
#define alloca __builtin_alloca
#endif
#endif

#endif

#if !defined(PROFILING)
#error PROFILING not defined
#endif

#ifdef HAVE_LINUX_PERSONALITY_H
#include <linux/personality.h>
#undef personality
#endif

#ifdef HAVE_DECL_ADDR_NO_RANDOMIZE
#else
#define ADDR_NO_RANDOMIZE 0x0040000
#endif

#ifdef HAVE_PERSONALITY
extern long personality(unsigned long persona);
#endif

#include "../system/supervisorinterface.h"


static void ignore(int);

static void putstderr(const char *m) {
     ignore(write(STDERR,m,strlen(m)));
     ignore(write(STDERR,NEWLINE,strlen(NEWLINE)));
     }

static void ignore(int x) { }

/*
void WerrorS(const char *m) {
  putstderr(m);
  exit(1);
}
*/

void WarnS(const char *m) {
  putstderr(m);
}

static void alarm_handler(int sig), interrupt_handler(int sig);
static void oursignal(int sig, void (*handler)(int)) {
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

static int have_arg_no_int;

void system_handleInterruptsSetup(M2_bool handleInterrupts) {
     if (!have_arg_no_int) {
	  oursignal(SIGINT,handleInterrupts ? interrupt_handler : SIG_DFL);
	  }
     oursignal(SIGALRM,handleInterrupts ? alarm_handler : SIG_DFL);
     }

static void unblock(int sig) {
 #ifdef HAVE_SIGPROCMASK
  sigset_t s;
  sigemptyset(&s);
  sigaddset(&s,sig);
  sigprocmask(SIG_UNBLOCK,&s,NULL);
 #else
  signal(sig,SIG_DFL);
 #endif
}

static void alarm_handler(int sig) {
     if (tryGlobalAlarm() == 0) {
	  interrupts_setAlarmedFlag();
	  }
     oursignal(SIGALRM,alarm_handler);
     }

#define BACKTRACE_WORKS 0	/* doesn't work under ubuntu, or works very slowly when threads are involved */

#if defined(HAVE_EXECINFO_H) && defined(HAVE_BACKTRACE)
#include <execinfo.h>
#endif

#if !BACKTRACE_WORKS
int backtrace(void **buffer, int size) {
  buffer[0] = NULL;		/* see GC_save_callers() in gc's os_dep.c, which insists on at least one */
  return 1;
}
#endif

#if defined(HAVE_BACKTRACE) && BACKTRACE_WORKS
#define STACK_SIZE 20
void stack_trace() {
  static void *buf[STACK_SIZE];
  int n = backtrace(buf,STACK_SIZE);
  backtrace_symbols_fd(buf,n,STDERR);
}

#elif __GNUC__

#ifdef HAVE_SIGLONGJMP
 static sigjmp_buf stack_trace_jump;
#else
 static jmp_buf stack_trace_jump;
#endif

void segv_handler2(int sig) {
     // fprintf(stderr,"--SIGSEGV during stack trace\n");
     #ifdef HAVE_SIGLONGJMP
      siglongjmp(stack_trace_jump,1);
     #else
      longjmp(stack_trace_jump,1);
     #endif
}

void stack_trace() {
     void (*old)(int) = signal(SIGSEGV,segv_handler2); /* in case traversing the stack below causes a segmentation fault */
     unblock(SIGSEGV);
     fprintf(stderr,"-- stack trace, pid %ld:\n", (long)getpid());
     if (0 == 
         #ifdef HAVE_SIGLONGJMP
	  sigsetjmp(stack_trace_jump,TRUE)
	 #else
	  setjmp(stack_trace_jump)
	 #endif
	 ) {
#	  define D fprintf(stderr,"level %d -- return addr: 0x%p -- frame: 0x%p\n",i,__builtin_return_address(i),__builtin_frame_address(i))
#	  define i 0
	  D;
#	  undef i
#ifdef BUILTIN_RETURN_ADDRESS_ACCEPTS_NONZERO_ARGUMENT
#	  define i 1
	  D;
#	  undef i
#	  define i 2
	  D;
#	  undef i
#	  define i 3
	  D;
#	  undef i
#	  define i 4
	  D;
#	  undef i
#	  define i 5
	  D;
#	  undef i
#	  define i 6
	  D;
#	  undef i
#	  define i 7
	  D;
#	  undef i
#	  define i 8
	  D;
#	  undef i
#	  define i 9
	  D;
#	  undef i
#	  define i 10
	  D;
#	  undef i
#	  define i 11
	  D;
#	  undef i
#	  define i 12
	  D;
#	  undef i
#	  define i 13
	  D;
#	  undef i
#	  define i 14
	  D;
#	  undef i
#	  define i 15
	  D;
#	  undef i
#	  define i 16
	  D;
#	  undef i
#	  define i 17
	  D;
#	  undef i
#	  define i 18
	  D;
#	  undef i
#	  define i 19
	  D;
#	  undef i
#	  define i 20
	  D;
#	  undef i
#	  define i 20
	  D;
#	  undef i
#	  define i 21
	  D;
#	  undef i
#	  define i 22
	  D;
#	  undef i
#	  define i 23
	  D;
#	  undef i
#	  define i 24
	  D;
#	  undef i
#	  define i 25
	  D;
#	  undef i
#	  define i 26
	  D;
#	  undef i
#	  define i 27
	  D;
#	  undef i
#	  define i 28
	  D;
#	  undef i
#	  define i 29
	  D;
#	  undef i
#	  define i 30
	  D;
#	  undef i
#	  define i 31
	  D;
#	  undef i
#	  define i 32
	  D;
#	  undef i
#	  define i 33
	  D;
#	  undef i
#	  define i 34
	  D;
#	  undef i
#	  define i 35
	  D;
#	  undef i
#	  define i 36
	  D;
#	  undef i
#	  define i 37
	  D;
#	  undef i
#	  define i 38
	  D;
#	  undef i
#	  define i 39
	  D;
#	  undef i
#endif
     }
     fprintf(stderr,"-- end stack trace\n");
     signal(SIGSEGV,old);
}
#else
void stack_trace() {
  fprintf(stderr,"-- stack trace not available\n");
}
#endif

void segv_handler(int sig) {
  static int level;
  fprintf(stderr,"-- SIGSEGV\n");
  level ++;
  if (level > 1) {
    fprintf(stderr,"-- SIGSEGV handler called a second time, aborting\n");
    _exit(2);
  }
  stack_trace();
  level --;
  _exit(1);
}

#if DUMPDATA
#ifdef HAVE_SIGLONGJMP
 static sigjmp_buf loaddata_jump;
#else
 static jmp_buf loaddata_jump;
#endif
#endif

#ifdef HAVE_SIGLONGJMP
 static sigjmp_buf abort_jump;
 sigjmp_buf interrupt_jump;
#else
 static jmp_buf abort_jump;
 jmp_buf interrupt_jump;
#endif

bool interrupt_jump_set = FALSE;

#undef ABORT

static bool abort_jump_set = FALSE;

#include <readline/readline.h>
static void interrupt_handler(int sig) {
     if (tryGlobalInterrupt() == 0) {
	  if (test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field)) || THREADLOCAL(interrupts_interruptPending,bool)) {
	       if (isatty(STDIN) && isatty(STDOUT)) {
		    while (TRUE) {
			 char buf[10];
			 #              ifdef ABORT
			 printf("\nAbort (y/n)? ");
			 #              else
			 printf("\nExit (y=yes/n=no/b=backtrace)? ");
			 #              endif
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
			      #                   ifndef NDEBUG
			      trap();
			      #                   endif
			      #                   ifdef ABORT
			      if (!tokens_stopIfError && abort_jump_set) {
				   extern void evaluate_clearInterruptFlag(), evaluate_determineExceptionFlag(), evaluate_clearAlarmedFlag();
				   fprintf(stderr,"returning to top level\n");
				   fflush(stderr);
				   interrupts_clearInterruptFlag();
				   libfac_interruptflag = FALSE;
				   interrupts_interruptPending = FALSE;
				   interrupts_interruptShield = FALSE;
				   evaluate_clearAlarmedFlag();
				   evaluate_determineExceptionFlag();
                                   #ifdef HAVE_SIGLONGJMP
				    siglongjmp(abort_jump,1); 
				   #else
                                    longjmp(abort_jump,1); 
				   #endif
				   }
			      else {
				   #                   endif
				   fprintf(stderr,"exiting\n");
				   exit(12);
				   #                   ifdef ABORT
				   }
			      #                   endif
			      }
			 else if (buf[0]=='n' || buf[0]=='N') {
			      break;
			      }
			 }
			 }
	       else {
		    #              ifndef NDEBUG
		    trap();
		    #              endif
		    exit(13);
		    }
	       }
	  else {
	    if (THREADLOCAL(interrupts_interruptShield,bool)) THREADLOCAL(interrupts_interruptPending,bool) = TRUE;
	       else {
		    if (THREADLOCAL(tokens_stopIfError,bool)) {
			 int interruptExit = 2;	/* see also interp.d */
			 fprintf(stderr,"interrupted, stopping\n");
			 _Exit(interruptExit);
			 }
		    interrupts_setInterruptFlag();
		    # if 0
		    /* readline doesn't cancel the partially typed line, for some reason, and this doesn't help: */
		    if (reading_from_readline) rl_free_line_state();
		    #endif
		    if (interrupt_jump_set) 
			 #ifdef HAVE_SIGLONGJMP
			 siglongjmp(interrupt_jump,1);
			 #else
		         longjmp(interrupt_jump,1);
			 #endif
		    }
	       }
          }
     oursignal(SIGINT,interrupt_handler);
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

#if defined(HAVE_CLOCK_GETTIME) && ( defined(CLOCK_THREAD_CPUTIME_ID) || defined(CLOCK_THREAD_CPUTIME) || defined(CLOCK_PROCESS_CPUTIME_ID) || defined(CLOCK_PROCESS_CPUTIME) )

	#ifdef HAVE_SYS_TYPES_H
	#include <sys/types.h>
	#endif
	#include <stdlib.h>
	#ifdef HAVE_SYSCALL_H
	#include <syscall.h>
	#endif
	#include <time.h>
	#include <stdio.h>
	#include <unistd.h> // for sysconf
	static __thread double startTime;
	double system_cpuTime(void) {
	     struct timespec t;
	     int err = clock_gettime(
		  #if defined(CLOCK_THREAD_CPUTIME_ID)
		  CLOCK_THREAD_CPUTIME_ID
		  #elif defined(CLOCK_THREAD_CPUTIME)
		  CLOCK_THREAD_CPUTIME
		  #elif defined(CLOCK_PROCESS_CPUTIME_ID)
		  CLOCK_PROCESS_CPUTIME_ID
		  #elif defined(CLOCK_PROCESS_CPUTIME)
		  CLOCK_PROCESS_CPUTIME
		  #else
		  #error "clock_gettime: no suitable argument for getting CPU time"
		  #endif
		  ,&t);
	     if (err) return 0;		/* silent about error */
	     double u = t.tv_sec + t.tv_nsec * 1e-9;
	     return u - startTime;
	     }
        void system_cpuTime_init(void) {
	     startTime = system_cpuTime();
	     }

#else

    #if !defined(CLOCKS_PER_SEC) || CLOCKS_PER_SEC > 10000

	
	#define INITVAL 100000000 /* very long, and then an interrupt occurs, sadly */
        void system_cpuTime_init(void) {
	     struct itimerval it;
	     int ret;
	     it.it_value.tv_sec = INITVAL;
	     it.it_value.tv_usec = 0;
	     it.it_interval.tv_sec = INITVAL;
	     it.it_interval.tv_usec = 0;
	     ret = setitimer(ITIMER_VIRTUAL,&it,(struct itimerval *)NULL);
	     if (ret) perror("setitimer() failed");
	     signal(SIGVTALRM,SIG_IGN);
	     }
	double system_cpuTime(void) {
	     struct itimerval it;
	     long sec,usec;
	     getitimer(ITIMER_VIRTUAL,&it);
	     sec = INITVAL - it.it_value.tv_sec;
	     usec =   0    - it.it_value.tv_usec;
	     return sec + usec / 1000000.;
	     }

    #else
					/* ANSI C */
	static clock_t start_time;
	void system_cpuTime_init(void) {
	     start_time = clock();
	     }
	double system_cpuTime(void) {
	     return (double)(clock()-start_time) / CLOCKS_PER_SEC;
	     }
    #endif

#endif

#if defined HAVE___ENVIRON
    #define our_environ __environ
    #if !HAVE_DECL___ENVIRON
    extern char **__environ;
    #endif
#elif defined HAVE__ENVIRON
    #define our_environ _environ
    #if !HAVE_DECL__ENVIRON
    extern char **_environ;
    #endif
#elif defined HAVE_ENVIRON
    #define our_environ environ
    #if !HAVE_DECL_ENVIRON
    extern char **environ;
    #endif
#else
    #error "no environment variable available"
#endif

extern void clean_up();
extern void init_readline_variables();
extern char *GC_stackbottom;
extern void arginits(int, const char **);
extern bool gotArg(const char *arg, const char ** argv);

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

static void call_shared_library() {
#if 0
  const char *libname = "libM2.so";
  const char *funname = "entry";
  void *handle;
  int (*g)();
  errno = 0;
  handle = dlopen(libname, RTLD_LAZY | RTLD_GLOBAL);
  if (handle == NULL) { error("can't load library %s", libname); return; }
  g = dlsym(handle, funname);
  if (g == NULL) { error("can't link function %s from sharable library %s",funname,libname); return; }
  g();
  if (0 != dlclose(handle)) { error("can't close sharable library %s",libname); return; }
#endif
}

#ifdef HAVE_PYTHON
#include <python2.5/Python.h>
#endif

void* testFunc(void* q )
{
  printf("testfunc %p\n",q);
  return NULL;
}

struct saveargs
{
  int argc;
  char** argv;
  char** envp;
  int volatile envc;
};

static void reverse_run(struct FUNCTION_CELL *p) { if (p) { reverse_run(p->next); (*p->fun)(); } }

static char dummy;
static struct saveargs* vargs;
void* interpFunc(void* vargs2)
{
  struct saveargs* args = (struct saveargs*) vargs;
  char** saveenvp = args->envp;
  char** saveargv = args->argv;
  int argc = args->argc;
  int volatile envc = args->envc;
     setInterpThread();
     reverse_run(thread_prepare_list);// -- re-initialize any thread local variables
     init_readline_variables();
     arginits(argc,(const char **)saveargv);

     //     void M2__prepare();
     ///     M2__prepare();

     M2_envp = M2_tostrings(envc,(char **)saveenvp);
     M2_argv = M2_tostrings(argc,(char **)saveargv);
     M2_args = M2_tostrings(argc == 0 ? 0 : argc - 1, (char **)saveargv + 1);
     interp_setupargv();
     #ifdef HAVE_SIGLONGJMP
      sigsetjmp(abort_jump,TRUE);
     #else
      setjmp(abort_jump);
     #endif
     abort_jump_set = TRUE;

#if __GNUC__
     signal(SIGSEGV, segv_handler);
#endif

     interp_process(); /* this is where all the action happens, see interp.d, where it is called simply process */

     clean_up();
#if 0
     fprintf(stderr,"gc: heap size = %d, free space divisor = %ld, collections = %ld\n", 
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no-old_collections);
#endif
     exit(0);
     return NULL;
}

int have_arg(char **argv, const char *arg) {
     for (;*argv;argv++) if (0 == strcmp(*argv,arg)) return TRUE;
     return FALSE;
     }

int Macaulay2_main(argc,argv)
int argc; 
char **argv;
{

     int volatile envc = 0;
#if DUMPDATA
     static int old_collections = 0;
     const char ** volatile saveenvp = NULL;
     const char ** volatile saveargv;
     int volatile savepid = 0;
#else
#define saveenvp our_environ
#define saveargv argv
#endif
     void main_inits();

     char **x = our_environ; 
     while (*x) envc++, x++;

     system_cpuTime_init();
     call_shared_library();

#ifdef HAVE_PYTHON
     Py_SetProgramName(argv[0]);
     Py_Initialize();
#endif

#if defined HAVE_PERSONALITY && !PROFILING
     if (!gotArg("--no-personality", (const char **)argv)) {
	  /* this avoids mmap() calls resulting in address randomization */
	  int oldpersonality = personality(-1);
	  if ((oldpersonality & ADDR_NO_RANDOMIZE) == 0) {
	       int newpersonality;
	       personality(oldpersonality | ADDR_NO_RANDOMIZE);
	       newpersonality = personality(-1);
	       personality(oldpersonality | ADDR_NO_RANDOMIZE);	/* just in case the previous line sets the personality to -1, which can happen */
	       if ((newpersonality & ADDR_NO_RANDOMIZE) != 0) return execvp(argv[0],argv);
	  }
	  else personality(oldpersonality);
     }
#endif

#if defined(_WIN32)
     if (argv[0][0]=='/' && argv[0][1]=='/' && argv[0][3]=='/') {
       /* we must be in Windows 95 or NT running under CYGWIN32, and
	  the path to our executable has been mangled from D:/a/b/c
	  into //D/a/b/c */
       argv[0][0] = argv[0][2];
       argv[0][1] = ':';
       strcpy(argv[0]+2,argv[0]+3);
     }
     {
       /* change all \ in path to executable to / */
       char *p;
       for (p=argv[0]; *p; p++) if (*p == '\\') *p = '/';
     }
#endif

     abort_jump_set = FALSE;

#ifdef HAVE__SETMODE
     {
	  /* extern void _setmode(int, int); */
	  /* extern int  _setmode(int, int); */
     _setmode(STDIN ,_O_BINARY);
     _setmode(STDOUT,_O_BINARY);
     _setmode(STDERR,_O_BINARY);
     }
#endif

#if DUMPDATA
     {
	  int i;

	  /* save arguments on stack in case they're on the heap */
	  saveargv = (char **)alloca((argc + 1)*sizeof(char *));
	  for (i=0; i<argc; i++) {
	       saveargv[i] = alloca(strlen(argv[i]) + 1);
	       strcpy(saveargv[i],argv[i]);
	  }
	  saveargv[i] = NULL;

	  /* save environment on stack in case it's on the heap */
	  saveenvp = (char **)alloca((envc + 1)*sizeof(char *));
	  for (i=0; i<envc; i++) {
	       saveenvp[i] = alloca(strlen(our_environ[i]) + 1);
	       strcpy(saveenvp[i],our_environ[i]);
	  }
	  saveenvp[i] = NULL;
     }
#endif

#if DUMPDATA
     if (0 != 
	 #ifdef HAVE_SIGLONGJMP
	 sigsetjmp(loaddata_jump,TRUE)
	 #else
	 setjmp(loaddata_jump)
         #endif
	  ) {
	  if (gotArg("--notify", saveargv)) putstderr("--loaded cached memory data");
	  struct GC_stack_base sb;
	  GC_get_stack_base(&sb);
	  GC_stackbottom = (char *)sb.mem_base;	/* the stack may have moved (since we may have reloaded all the static data) */
	  old_collections = GC_gc_no;
          {
	       char **environ0;
	       int i;
	       our_environ = saveenvp;	/* our_environ is a static variable that points
					to the heap and has been overwritten by
					loaddata(), thereby pointing to a previous
					incarnation of the heap. */
	       /* Make a copy of the environment on the heap for 'our_environ'. */
	       /* In some systems, putenv() calls free() on the old item,
		  so we are careful to use malloc here, and not GC_malloc. */
	       environ0 = (char **)malloc((envc + 1)*sizeof(char *));
	       /* amazing but true:
		  On linux, malloc calls getenv to get values for tunable
		  parameters, so don't trash our_environ yet.
		  */
	       if (environ0 == NULL) fatal("out of memory");
	       for (i=0; i<envc; i++) {
		    environ0[i] = malloc(strlen(saveenvp[i]) + 1);
		    if (environ0[i] == NULL) fatal("out of memory");
		    strcpy(environ0[i],saveenvp[i]);
	       }
	       environ0[i] = NULL;
	       our_environ = environ0;
               }
	  }
#endif

     if (__gmp_allocate_func != (void *(*) (size_t))getmem_atomic) {
          FATAL("possible memory leak, gmp allocator not set up properly");
	  fprintf(stderr,"--internal warning: possible memory leak, gmp allocator not set up properly, resetting\n");
     }

     signal(SIGPIPE,SIG_IGN);
     have_arg_no_int = have_arg(argv,"--int");
     if (have_arg_no_int)
	  rl_catch_signals = FALSE; /* tell readline not to catch signals, such as SIGINT */

     system_handleInterruptsSetup(TRUE);
     
     vargs = GC_MALLOC_UNCOLLECTABLE(sizeof(struct saveargs));
     vargs->argv=saveargv;
     vargs->argc=argc;
     vargs->envp=saveenvp;
     vargs->envc = envc;


     initializeThreadSupervisor();
     struct ThreadTask* interpTask = createThreadTask("Interp",interpFunc,vargs,0,0,0);
     pushTask(interpTask);
     waitOnTask(interpTask);
     return 0;
     }

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
#ifdef HAVE_PYTHON
     if (Py_IsInitialized()) Py_Finalize();
#endif
#    ifndef NDEBUG
     trap();
#    endif
     }

void scclib__prepare(void) {}

int system_dumpdata(M2_string datafilename)
{
     /* this routine should keep its data on the stack */
#if !DUMPDATA
     return ERROR;
#else
     bool haderror = FALSE;
     char *datafilename_s = tocharstar(datafilename);
     if (ERROR == dumpdata(datafilename_s)) haderror = TRUE;
     GC_FREE(datafilename_s);
     return haderror ? ERROR : OKAY;
#endif
     }

#define FENCE 0x47474747

int system_loaddata(int notify, M2_string datafilename){
#if !DUMPDATA
     return ERROR;
#else
     char *datafilename_s = tocharstar(datafilename);
     volatile int fence0 = FENCE;
     #ifdef HAVE_SIGLONGJMP
     sigjmp_buf save_loaddata_jump;
     #else
     jmp_buf save_loaddata_jump;
     #endif
     volatile int fence1 = FENCE;
     /* int loadDepth = system_loadDepth; */
     memcpy(save_loaddata_jump,loaddata_jump,sizeof(loaddata_jump));
     if (ERROR == loaddata(M2_notify,datafilename_s)) return ERROR;
     memcpy(loaddata_jump,save_loaddata_jump,sizeof(loaddata_jump));
     /* system_loadDepth = loadDepth + 1; */
     if (fence0 != FENCE || fence1 != FENCE) {
       putstderr("--internal error: fence around loaddata longjmp save area on stack destroyed, aborting");
       abort();
     }
     if (M2_notify) putstderr("--loaddata: data loaded, ready for longjmp");
     #ifdef HAVE_SIGLONGJMP
      siglongjmp(loaddata_jump,1);
     #else
      longjmp(loaddata_jump,1);
     #endif
#endif
     }

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
