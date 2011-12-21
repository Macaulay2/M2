#include <tokens-exports.h>
#include <interrupts-exports.h>
#include "platform.h"
/**
   Posix spec guarentees the existance of these headers
**/
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>		/* just for window width */
#include <termios.h>		/* just for window width */
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#include <netdb.h>     	    	/* needed for gethostbyname() */
#include <netinet/in.h>	    	/* needed for struct sockaddr_in */
#include <arpa/inet.h>	   	/* needed for inet_addr() */
#include <dirent.h>

int backtrace(void **buffer, int size);
static void stack_trace();

//Declare interrupt handler
static void interrupt_handler(int sig);
/***
   Set the signal handler for the given signal.
   In particular, we want to disable restarting the signal upon the handler being called.
   @param sig Signal to set the handler for.
   @param handler Function to call upon receiving signal.
***/
static void oursignal(int sig, void (*handler)(int)) {
  struct sigaction act;
  act.sa_flags = 0;	/* no SA_RESTART */
  act.sa_handler = handler;
  sigemptyset(&act.sa_mask);
  sigfillset(&act.sa_mask);
  sigaction(sig,&act,NULL); /* old way: signal(sig,interrupt_handler); */
}
/***
   Initialize M2 signal handling.
   @param handleInterrupts ???? 
***/
void system_handleInterruptsSetup(M2_bool handleInterrupts) {
  oursignal(SIGINT,handleInterrupts ? interrupt_handler : SIG_DFL);
}
/***
   Unblock the given signal
   @param sig Signal to unblock.
***/
static void unblock(int sig) {
  sigset_t s;
  sigemptyset(&s);
  sigaddset(&s,sig);
  sigprocmask(SIG_UNBLOCK,&s,NULL);
}

static sigjmp_buf abort_jump;

sigjmp_buf interrupt_jump;
int interrupt_jump_set = FALSE;

/***
    This variable should be set to true to disable the backtrace option for interrupts.
***/
static int use_abort = FALSE;

static int abort_jump_set = FALSE;

#include <readline/readline.h>
static void interrupt_handler(int sig) {
     if (tryGlobalInterrupt() == 0) {
	  if (test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field)) || THREADLOCAL(interrupts_interruptPending,int)) {
	       if (isatty(STDIN) && isatty(STDOUT)) {
		    while (TRUE) {
			 char buf[10];
			 if(use_abort)
			   printf("\nAbort (y/n)? ");
			 else
			   printf("\nExit (y=yes/n=no/b=backtrace)? ");
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
			      #                   ifdef ABORT
			      if (!tokens_stopIfError && abort_jump_set) {
				extern void evaluate_clearInterruptFlag(), evaluate_determineExceptionFlag();
				   fprintf(stderr,"returning to top level\n");
				   fflush(stderr);
				   interrupts_clearInterruptFlag();
				   libfac_interruptflag = FALSE;
				   interrupts_interruptPending = FALSE;
				   interrupts_interruptShield = FALSE;
				   evaluate_determineExceptionFlag();
				   siglongjmp(abort_jump,1); 
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
		    exit(13);
		    }
	       }
	  else {
	    if (THREADLOCAL(interrupts_interruptShield,int)) THREADLOCAL(interrupts_interruptPending,int) = TRUE;
	       else {
		    if (THREADLOCAL(tokens_stopIfError,int)) {
			 int interruptExit = 2;	/* see also interp.d */
			 fprintf(stderr,"interrupted, stopping\n");
			 exit(interruptExit);
			 }
		    interrupts_setInterruptFlag();
		    # if 0
		    /* readline doesn't cancel the partially typed line, for some reason, and this doesn't help: */
		    if (reading_from_readline) rl_free_line_state();
		    #endif
		    if (interrupt_jump_set) siglongjmp(interrupt_jump,1);
		    }
	       }
          }
     oursignal(SIGINT,interrupt_handler);
     }


/**
   Mysterious code for handling backtraces on seg faults
**/
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
static void stack_trace() {
  static void *buf[STACK_SIZE];
  int n = backtrace(buf,STACK_SIZE);
  backtrace_symbols_fd(buf,n,STDERR);
}

#elif __GNUC__

static sigjmp_buf stack_trace_jump;

void segv_handler2(int sig) {
     // fprintf(stderr,"--SIGSEGV during stack trace\n");
     siglongjmp(stack_trace_jump,1);
}

static void stack_trace() {
     void (*old)(int) = signal(SIGSEGV,segv_handler2); /* in case traversing the stack below causes a segmentation fault */
     unblock(SIGSEGV);
     fprintf(stderr,"-- stack trace, pid %ld:\n", (long)getpid());
     if (0 == sigsetjmp(stack_trace_jump,TRUE)) {
#	  define D fprintf(stderr,"level %d -- return addr: 0x%08lx -- frame: 0x%08lx\n",i,(long)__builtin_return_address(i),(long)__builtin_frame_address(i))
#	  define i 0
	  D;
#	  undef i
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
     }
     fprintf(stderr,"-- end stack trace\n");
     signal(SIGSEGV,old);
}
#else
static void stack_trace() {
  fprintf(stderr,"-- stack trace not available\n");
}
#endif
/***
   Seg fault handler
***/
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
