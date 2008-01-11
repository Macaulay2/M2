/*		Copyright 1994 by Daniel R. Grayson		*/

#include <factoryconf.h>
extern const char factoryVersion[]; /* extracted from factory's factory.h */
extern int libfac_interruptflag; /* extracted from libfac's factor.h */
#include <NTL/version.h>
/* defining GDBM_STATIC makes the cygwin version work, and is irrelevant for the other versions */
#define GDBM_STATIC
#include <gdbm.h>

#include <alloca.h>

#include "M2mem.h"
#include "M2mem2.h"
#include "M2inits.h"
#include "../dumpdata/map.h"
#include "types.h"
#include "debug.h"

/* char *config_args[] = { CONFIG_ARGS 0 }; */
char *config_args = CONFIG_ARGS ;

void trap(void) {}		/* we don't put it in debug.c, or it will get optimized away! */

#if !defined(PROFILING)
#error PROFILING not defined
#endif

#if HAVE_LINUX_PERSONALITY_H
#include <linux/personality.h>
#undef personality
#endif

#if HAVE_DECL_ADDR_NO_RANDOMIZE
#else
#define ADDR_NO_RANDOMIZE 0x0040000
#endif

#if HAVE_PERSONALITY
extern long personality(unsigned long persona);
#endif

const char *get_libfac_version();	/* in version.cc */
static const char *get_cc_version(void) {
  static char buf[100] = "cc (unknown)";
# ifdef __GNUC__
#  ifdef __GNUC_PATCHLEVEL__
   sprintf(buf,"gcc %d.%d.%d",__GNUC__,__GNUC_MINOR__,__GNUC_PATCHLEVEL__);
#  else
   sprintf(buf,"gcc %d.%d",__GNUC__,__GNUC_MINOR__);
#  endif
# endif  
  return buf;
}

static void putstderr(char *m) {
     write(STDERR,m,strlen(m));
     write(STDERR,NEWLINE,strlen(NEWLINE));
     }

void WerrorS(char *m) {
  putstderr(m);
  exit(1);
}

void WarnS(char *m) {
  putstderr(m);
}

#ifdef includeX11
Display *display;
Font font;
#endif

M2_bool system_exceptionFlag = FALSE;
M2_bool system_interruptedFlag = FALSE;
M2_bool system_interruptPending = FALSE;
M2_bool system_interruptShield = FALSE;
M2_bool system_alarmedFlag = FALSE;

#if __GNUC__

static sigjmp_buf stack_trace_jump;

void segv_handler2(int sig) {
     // fprintf(stderr,"--SIGSEGV during stack trace\n");
     longjmp(stack_trace_jump,1);
}

void stack_trace() {
     void (*old)(int) = signal(SIGSEGV,segv_handler2); /* in case traversing the stack below causes a segmentation fault */
     fprintf(stderr,"-- stack trace:\n");
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
     }
     fprintf(stderr,"-- end stack trace\n");
     signal(SIGSEGV,old);
}

void segv_handler(int sig) {
     fprintf(stderr,"-- SIGSEGV\n");
     stack_trace();
     abort();
}

#endif

static void alarm_handler(int sig)
{
     extern void evaluate_setAlarmedFlag();
     evaluate_setAlarmedFlag();
#ifdef SIGALRM
     signal(SIGALRM,alarm_handler);
#endif
     }

#if DUMPDATA
static sigjmp_buf loaddata_jump;
#endif

static sigjmp_buf abort_jump;
static bool abort_jump_set = FALSE;

sigjmp_buf interrupt_jump;
bool interrupt_jump_set = FALSE;

#undef ABORT

static void interrupt_handler(int sig)
{
     if (system_interruptedFlag || system_interruptPending) {
	  if (isatty(STDIN) && isatty(STDOUT)) while (TRUE) {
	       char buf[10];
#              ifdef ABORT
	       printf("\nAbort (y/n)? ");
#              else
	       printf("\nExit (y/n)? ");
#              endif
	       fflush(stdout);
	       if (NULL == fgets(buf,sizeof(buf),stdin)) {
		    fprintf(stderr,"exiting\n");
		    exit(1);
	            }
	       if (buf[0]=='y' || buf[0]=='Y') {
#                   ifdef DEBUG
     		      trap();
#                   endif
#                   ifdef ABORT
		    if (!tokens_stopIfError && abort_jump_set) {
			 extern void evaluate_clearInterruptFlag(), evaluate_determineExceptionFlag(), evaluate_clearAlarmedFlag();
     	  		 fprintf(stderr,"returning to top level\n");
     	  		 fflush(stderr);
			 evaluate_clearInterruptFlag();
			 libfac_interruptflag = FALSE;
			 system_interruptPending = FALSE;
			 system_interruptShield = FALSE;
			 evaluate_clearAlarmedFlag();
			 evaluate_determineExceptionFlag();
     	  		 siglongjmp(abort_jump,1); 
			 }
		    else {
#                   endif
			 fprintf(stderr,"exiting\n");
		    	 exit(1);
#                   ifdef ABORT
			 }
#                   endif
		    }
	       else if (buf[0]=='n' || buf[0]=='N') {
		    break;
		    }
	       }
	  else {
#              ifndef NDEBUG
     	       trap();
#              endif
	       exit(1);
	       }
	  }
     else {
	  if (system_interruptShield) system_interruptPending = TRUE;
	  else {
	       extern void evaluate_setInterruptFlag();
	       if (tokens_stopIfError) {
		    int interruptExit = 2;	/* see also interp.d */
		    fprintf(stderr,"interrupted, stopping\n");
		    exit(interruptExit);
	       }
	       evaluate_setInterruptFlag();
	       libfac_interruptflag = TRUE;
	       if (interrupt_jump_set) siglongjmp(interrupt_jump,1);
	       }
	  }
     signal(SIGINT,interrupt_handler);
     }

static struct COUNTER { 
     int *count; char *filename; int lineno; char *funname;
     struct COUNTER *next;
     } *counters = NULL;

int register_fun(count, filename, lineno, funname)
int *count;
char *filename;
int lineno;
char *funname;
{
     struct COUNTER *p = (struct COUNTER *) getmem(sizeof(struct COUNTER));
     p->count = count;
     p->filename = filename;
     p->lineno = lineno;
     p->funname = funname;
     p->next = counters;
     counters = p;
     return 0;
     }

M2_string actors5_CCVERSION;
M2_string actors5_VERSION;
M2_string actors5_OS;
M2_string actors5_ARCH;
M2_string actors5_NODENAME;
M2_string actors5_REL;
M2_string actors5_timestamp;
M2_string actors5_GCVERSION;
M2_string actors5_GMPVERSION;
M2_string actors5_startupString1;
M2_string actors5_startupString2;
M2_string actors5_NTLVERSION;
M2_string actors5_LIBFACVERSION;
M2_string actors5_FACTORYVERSION;
M2_string actors5_READLINEVERSION;
M2_string actors5_MPFRVERSION;
M2_string actors5_M2SUFFIX;
M2_string actors5_EXEEXT;
M2_string actors5_endianness;
M2_string actors5_packages;
M2_string actors5_build;
M2_string actors5_host;
int actors5_pointersize;
M2_bool actors5_DUMPDATA;
M2_bool actors5_FACTORY;
M2_bool actors5_MP;

M2_stringarray system_envp;
M2_stringarray system_argv;
M2_stringarray system_args;
M2_string actors5_configargs;
int system_loadDepth;

#if !defined(CLOCKS_PER_SEC) || CLOCKS_PER_SEC > 10000
static struct itimerval it;
#define INITVAL 1000000		/* a million seconds is very long */
void system_stime(void) {
     it.it_value.tv_sec = INITVAL;
     it.it_value.tv_usec = 0;
     (void) setitimer(ITIMER_VIRTUAL,&it,(struct itimerval *)NULL);
     }
double system_etime(void) {
     long sec,usec;
     (void) getitimer(ITIMER_VIRTUAL,&it);
     sec = INITVAL - it.it_value.tv_sec;
     usec =   0    - it.it_value.tv_usec;
     if (usec<0) usec+=1000000, sec-=1;
     return sec + usec / 1000000.;
     }
#else
				/* ANSI C */
static clock_t start_time;
void system_stime(void) {
     start_time = clock();
     }
double system_etime(void) {
     return (double)(clock()-start_time) / CLOCKS_PER_SEC;
     }
#endif

#if !HAVE___ENVIRON
#if HAVE__ENVIRON
#define __environ _environ
#elif HAVE_ENVIRON
#define __environ environ
#endif
#endif

extern char **__environ;

extern char timestamp[];
static void clean_up();

void dummy_GC_warn_proc(char *msg, GC_word arg) { }

#define stringify(x) #x

#if defined(__GNUC__)
#define stringize(a) #a
char CCVERSION[] = "gcc " stringize(__GNUC__) "." stringize(__GNUC_MINOR__) ;
#else
char CCVERSION[] = "unknown" ;
#endif

extern void init_readline_variables();
extern char *GC_stackbottom;
extern void arginits(int, char **);

extern bool gotArg(char *arg, char ** volatile argv);

int pid;			/* initialized below */
int system_getpid(void) {
     return pid;
}

#include <readline/readline.h>

static char *endianness() {
     static int x[2] = {0x61626364,0};
     return (char *)x;
}

int Macaulay2_main(argc,argv)
int argc; 
char **argv;
{
     char READLINEVERSION[8];	/* big enough for "255.255" */
     char dummy;
     int returncode = 0;
     int volatile envc = 0;
#if DUMPDATA
     static int old_collections = 0;
     char ** volatile saveenvp = NULL;
     char ** volatile saveargv;
     int volatile savepid = 0;
#else
#define saveenvp __environ
#define saveargv argv
#endif
     void main_inits();
     static void *reserve = NULL;
     extern void actors4_setupargv();
     extern void interp_process(), interp_process2();
     extern int interp_topLevel();

#if HAVE_PERSONALITY && !PROFILING
     if (!gotArg("--no-personality", argv)) {
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

     out_of_memory_jump_set = FALSE;
     abort_jump_set = FALSE;

#if HAVE__SETMODE
     {
     extern void _setmode(int, int);
     _setmode(STDIN ,_O_BINARY);
     _setmode(STDOUT,_O_BINARY);
     _setmode(STDERR,_O_BINARY);
     }
#endif

#if DUMPDATA
     {
	  int i;
	  char **x;

	  /* save arguments on stack in case they're on the heap */
	  saveargv = (char **)alloca((argc + 1)*sizeof(char *));
	  for (i=0; i<argc; i++) {
	       saveargv[i] = alloca(strlen(argv[i]) + 1);
	       strcpy(saveargv[i],argv[i]);
	  }
	  saveargv[i] = NULL;

	  /* save environment on stack in case it's on the heap */
	  for (envc=0, x=__environ; *x; x++) envc++;
	  saveenvp = (char **)alloca((envc + 1)*sizeof(char *));
	  for (i=0; i<envc; i++) {
	       saveenvp[i] = alloca(strlen(__environ[i]) + 1);
	       strcpy(saveenvp[i],__environ[i]);
	  }
	  saveenvp[i] = NULL;
     }
#endif

     pid = getpid();

#if DUMPDATA
     savepid = pid;		/* glibc getpid() caches the result in memory and performs the system call only once, so we can't use it after dumpdata */
     if (0 != sigsetjmp(loaddata_jump,TRUE)) {
	  pid = savepid;
	  if (gotArg("--notify", saveargv)) fprintf(stderr,"--loaded cached memory data\n");
	  struct GC_stack_base sb;
	  GC_get_stack_base(&sb);
	  GC_stackbottom = (char *)sb.mem_base;	/* the stack may have moved (since we may have reloaded all the static data) */
     	  GC_free_space_divisor = 4;
	  old_collections = GC_gc_no;
          {
	       char **environ0;
	       int i;
	       __environ = saveenvp;	/* __environ is a static variable that points
					to the heap and has been overwritten by
					loaddata(), thereby pointing to a previous
					incarnation of the heap. */
	       /* Make a copy of the environment on the heap for '__environ'. */
	       /* In some systems, putenv() calls free() on the old item,
		  so we are careful to use malloc here, and not GC_malloc. */
	       environ0 = (char **)malloc((envc + 1)*sizeof(char *));
	       /* amazing but true:
		  On linux, malloc calls getenv to get values for tunable
		  parameters, so don't trash __environ yet.
		  */
	       if (environ0 == NULL) fatal("out of memory");
	       for (i=0; i<envc; i++) {
		    environ0[i] = malloc(strlen(saveenvp[i]) + 1);
		    if (environ0[i] == NULL) fatal("out of memory");
		    strcpy(environ0[i],saveenvp[i]);
	       }
	       environ0[i] = NULL;
	       __environ = environ0;
               }
	  }
#endif

     system_stime();

     if (__gmp_allocate_func != (void *(*) (size_t))getmem) {
	  fprintf(stderr,"--internal warning: possible memory leak, gmp allocator not set up properly, resetting\n");
	  enterM2();
     }

     if (!gotArg("--int", saveargv))
     {
       signal(SIGINT,interrupt_handler);
#      ifdef SIGPIPE
       signal(SIGPIPE, SIG_IGN);
#      endif
     }

#      ifdef SIGALRM
       signal(SIGALRM,alarm_handler);
#      endif

     arginits(argc,saveargv);

     if (GC_stackbottom == NULL) GC_stackbottom = &dummy;
     system_newline = tostring(newline);
     actors5_CCVERSION = tostring(get_cc_version());
     actors5_VERSION = tostring(PACKAGE_VERSION);
     actors5_OS = tostring(OS);
     actors5_ARCH = tostring(ARCH);
     actors5_NODENAME = tostring(NODENAME);
     actors5_REL = tostring(REL);
     {
	  char const * p = strrchr(factoryVersion,' ');
	  p = p ? p+1 : factoryVersion;
	  actors5_FACTORYVERSION = tostring(p);
     }
     actors5_LIBFACVERSION = tostring(get_libfac_version());
     sprintf(READLINEVERSION,"%d.%d",(rl_readline_version>>8)&0xff,rl_readline_version&0xff);
     actors5_READLINEVERSION = tostring(READLINEVERSION);
     actors5_MPFRVERSION = tostring(mpfr_version);
     actors5_M2SUFFIX = tostring(M2SUFFIX);
     actors5_EXEEXT = tostring(EXEEXT);
     actors5_timestamp = tostring(timestamp);
     actors5_startupString1 = tostring(startupString1);
     actors5_startupString2 = tostring(startupString2);
     actors5_endianness = tostring(endianness());
     actors5_packages = tostring(PACKAGES);
     actors5_pointersize = sizeof(void *);
     actors5_host = tostring(hostsystemtype);
     actors5_build = tostring(buildsystemtype);
#if DUMPDATA
     actors5_DUMPDATA = TRUE;
     if (!haveDumpdata()) actors5_DUMPDATA = FALSE; /* even if dumpdata was enabled at configuration time, we may not have implemented it in the C code */
#else
     actors5_DUMPDATA = FALSE;
#endif
     {
	  char buf[100];
	  unsigned major, minor, alpha;
	  major = GC_version >> 16;
	  minor = (GC_version >> 8) & 0xff;
	  alpha = GC_version & 0xff;
	  if (alpha == 0xff) {
	       sprintf(buf,"%d.%d", major, minor);
	       }
	  else {
	       sprintf(buf,"%d.%d alpha %d", major, minor, alpha);
	       }
	  actors5_GCVERSION = tostring(buf);
	  }
     actors5_GMPVERSION = tostring(__gmp_version);
     actors5_NTLVERSION = tostring(NTL_VERSION);
     system_envp = tostrings(envc,saveenvp);
     system_argv = tostrings(argc,saveargv);
     system_args = tostrings(argc == 0 ? 0 : argc - 1, saveargv + 1);
     /*     actors5_configargs = tostrings(sizeof(config_args)/sizeof(char *) - 1, config_args); */
     actors5_configargs = tostring(config_args);

#ifdef includeX11
     display = XOpenDisplay(NULL);
     font = XLoadFont(display,"6x13");
#endif
     init_readline_variables();
     main_inits();		/* run all the startup code in the *.d files, see tmp_init.c */
     actors4_setupargv();
     if (reserve == NULL) {
	  reserve = GC_MALLOC_ATOMIC(102400);
	  }
     sigsetjmp(abort_jump,TRUE);
     abort_jump_set = TRUE;

#if __GNUC__
     signal(SIGSEGV, segv_handler);
#endif

     if (sigsetjmp(out_of_memory_jump,TRUE)) {
	  if (reserve != NULL) {
	       GC_FREE(reserve);
	       reserve = NULL;
	       }
#if 0
	  fprintf(stderr,", collecting garbage");
	  fflush(stderr);
	  GC_gcollect();
#endif
	  fprintf(stderr,"\n");
	  fflush(stderr);
          returncode = ! interp_topLevel();
	  }
     else {
          out_of_memory_jump_set = TRUE;
          interp_process();
     }
     clean_up();
#if 0
     fprintf(stderr,"gc: heap size = %d, free space divisor = %ld, collections = %ld\n", 
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no-old_collections);
#endif
     exit(returncode);
     return returncode;
     }

static void clean_up(void) {
     extern void close_all_dbms();
     close_all_dbms();
     while (pre_final_list != NULL) {
	  pre_final_list->final();
	  pre_final_list = pre_final_list->next;
	  }
     while (final_list != NULL) {
	  final_list->final();
	  final_list = final_list->next;
	  }
#    ifndef NDEBUG
     trap();
#    endif
     }

void system_exit(x)
int x;
{
     clean_up();
     exit(x);
     }

void scclib__prepare(void) {}

extern int etext, end;

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

int system_loaddata(int notify, M2_string datafilename){
#if !DUMPDATA
     return ERROR;
#else
     char *datafilename_s = tocharstar(datafilename);
     sigjmp_buf save_loaddata_jump;
     int loadDepth = system_loadDepth;
     memcpy(save_loaddata_jump,loaddata_jump,sizeof(loaddata_jump));
     if (ERROR == loaddata(notify,datafilename_s)) return ERROR;
     memcpy(loaddata_jump,save_loaddata_jump,sizeof(loaddata_jump));
     system_loadDepth = loadDepth + 1;
     siglongjmp(loaddata_jump,1);
#endif
     }

void C__prepare(void) {}

int actors4_isReady(int fd) {
#if defined(_WIN32)
     return 1;
#else
  int ret;
  static fd_set r, w, e;
  struct timeval timeout;
  FD_SET(fd,&r);
  timerclear(&timeout);
  ret = select(fd+1,&r,&w,&e,&timeout);
  FD_CLR(fd,&r);
  return ret;
#endif
}

int actors5_WindowWidth(int fd) {
#if defined(__alpha) || defined(_WIN32)
     return 0;
#else
     struct winsize x;
     ioctl(1,TIOCGWINSZ,&x);	/* see /usr/include/$SYSTEM/termios.h */
     return x.ws_col;
#endif
     }

int actors5_WindowHeight(int fd) {
#if defined(__alpha) || defined(_WIN32)
     return 0;
#else
     struct winsize x;
     ioctl(1,TIOCGWINSZ,&x);	/* see /usr/include/$SYSTEM/termios.h */
     return x.ws_row;
#endif
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
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// tags-file-name: "TAGS"
// End:
*/
