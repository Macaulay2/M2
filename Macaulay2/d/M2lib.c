/*		Copyright 1994 by Daniel R. Grayson		*/

#include <factoryconf.h>
#include <NTL/version.h>

extern int haveDumpdata();	/* in dumpdata/map.o */
#include "types.h"

/* defining GDBM_STATIC makes the cygwin version work, and is irrelevant for the other versions */
#define GDBM_STATIC
#include <gdbm.h>

const char *get_libfac_version();	/* in version.cc */
static const char *get_cc_version() {
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

char *progname;
#ifdef includeX11
Display *display;
Font font;
#endif

M2_bool system_interrupted = FALSE;
M2_bool system_interruptPending = FALSE;
M2_bool system_interruptShield = FALSE;
M2_bool system_alarmed = FALSE;

static void alarm_handler(int sig)
{
     system_alarmed = TRUE;
     if (system_interruptShield) system_interruptPending = TRUE;
     else {
	  system_interrupted = TRUE;
#     	  ifdef FACTORY
     	  libfac_interruptflag = TRUE;
#     	  endif
	  }
#ifdef SIGALRM
     signal(SIGALRM,alarm_handler);
#endif
     }

#if defined(__MWERKS__) || (defined(_WIN32) && !defined(__CYGWIN__))
#define sigjmp_buf jmp_buf
#define siglongjmp(j,c) longjmp(j,c)
#define sigsetjmp(j,m) setjmp(j)
#endif

static sigjmp_buf loaddata_jump, out_of_memory_jump, abort_jump;
static bool out_of_memory_jump_set = FALSE, abort_jump_set = FALSE;

static void interrupt_handler(int sig)
{
     if (system_interrupted || system_interruptPending) {
	  if (isatty(STDIN) && isatty(STDOUT)) while (TRUE) {
	       char buf[10];
	       printf("\nAbort (y/n)? ");
	       fflush(stdout);
	       if (NULL == fgets(buf,sizeof(buf),stdin)) {
		    fprintf(stderr,"exiting\n");
		    exit(1);
	            }
	       if (buf[0]=='y' || buf[0]=='Y') {
     		    trap();
		    if (!actors5_stopIfError && abort_jump_set) {
     	  		 fprintf(stderr,"returning to top level\n");
     	  		 fflush(stderr);
			 system_interrupted = FALSE;
#     	   	     	 ifdef FACTORY
			 libfac_interruptflag = FALSE;
#     	   	     	 endif
			 system_interruptPending = FALSE;
			 system_interruptShield = FALSE;
			 system_alarmed = FALSE;
     	  		 siglongjmp(abort_jump,1);
			 }
		    else {
			 fprintf(stderr,"exiting\n");
		    	 exit(1);
			 }
		    }
	       else if (buf[0]=='n' || buf[0]=='N') {
		    break;
		    }
	       }
	  else {
     	       trap();
	       exit(1);
	       }
	  }
     else {
	  if (system_interruptShield) system_interruptPending = TRUE;
	  else {
	       if (!isatty(STDIN)) {
		    fprintf(stderr,"interrupted%s",NEWLINE);
		    exit(1);
	       }
	       system_interrupted = TRUE;
#     	       ifdef FACTORY
	       libfac_interruptflag = TRUE;
#     	       endif
	       }
	  }
     signal(SIGINT,interrupt_handler);
     }

void outofmem(){
     static int count = 0;
     if (!actors5_stopIfError && out_of_memory_jump_set && count++ < 5) {
     	  fprintf(stderr,"out of memory, returning to top level");
     	  fflush(stderr);
     	  siglongjmp(out_of_memory_jump,1);
	  }
     else {
     	  fprintf(stderr,"out of memory, exiting\n");
	  exit(1);
	  }
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
M2_string actors5_DATE;
M2_string actors5_TIME;
M2_string actors5_GCVERSION;
M2_string actors5_GMPVERSION;
M2_string actors5_startupString1;
M2_string actors5_startupString2;
M2_string actors5_NTLVERSION;
M2_string actors5_LIBFACVERSION;
M2_string actors5_FACTORYVERSION;
M2_bool actors5_DUMPDATA;
M2_bool actors5_FACTORY;
M2_bool actors5_MP;

M2_stringarray system_envp;
M2_stringarray system_argv;
M2_stringarray system_args;
int system_loadDepth;

int system_randomint() {
#if 0
     extern long random();
     return random();
#else
     extern long random00();
     return random00();
#endif
     }

#ifdef __DJGPP__
void system_stime(){
     extern double start_timer();
     start_timer();
     }
double system_etime(){
     double return_elapsed_time(double);
     return return_elapsed_time(0.);
     }
#elif !defined(CLOCKS_PER_SEC) || CLOCKS_PER_SEC > 10000
static struct itimerval it;
#define INITVAL 1000000		/* a million seconds is very long */
void system_stime(){
     it.it_value.tv_sec = INITVAL;
     it.it_value.tv_usec = 0;
     (void) setitimer(ITIMER_VIRTUAL,&it,(struct itimerval *)NULL);
     }
double system_etime(){
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
void system_stime(){
     start_time = clock();
     }
double system_etime(){
     return (double)(clock()-start_time) / CLOCKS_PER_SEC;
     }
#endif

#if defined(DUMPDATA)
#if defined(__sun__) || defined(_WIN32) || defined(__CYGWIN__)
#define __environ _environ
#elif defined(__FreeBSD__) || defined(__MACH__) && defined(__POWERPC__)
#define __environ environ
#endif

extern char **__environ;
#endif

extern char timestamp[];
static void clean_up();

static void nop (p)		/* used below to keep variables out of registers */
void *p;
{}

#define NOTHING(p) nop((void *)p)
#define ONSTACK(p) nop((void *)&p)

#ifdef NDEBUG
void dummy_GC_warn_proc(char *msg, GC_word arg) { }
#endif

#if defined(__MWERKS__)

void SetMinimumStack(long minSize)
{
	long newApplLimit;

	if (minSize > LMGetDefltStack())
	{
		newApplLimit = (long) GetApplLimit()
				- (minSize - LMGetDefltStack());
		SetApplLimit((Ptr) newApplLimit);
		MaxApplZone();
	}
}

#define cMinStackSpace (512L * 1024L)
#endif

#define stringify(x) #x

#if defined(__GNUC__)
#define stringize(a) #a
char CCVERSION[] = "gcc " stringize(__GNUC__) "." stringize(__GNUC_MINOR__) ;
#else
char CCVERSION[] = "unknown" ;
#endif

/* we test gc to whether it properly marks pointers found in registers */
static void uniq(void *p, ...) {
  va_list a;
  void *q[100];
  int n = 0, i, j;
  q[n++] = p;
  va_start(a,p);
  for (;(q[n] = va_arg(a,void *));n++) ;
  va_end(a);
  for (i=0; i<n; i++) for (j=0; j<i; j++) if (q[i] == q[j]) {
    fprintf(stderr,
	    "%s: error: gc library doesn't find all the active pointers!\n"
	    "           Perhaps GC_push_regs was configured incorrectly.\n",
	    progname
	    );
    exit(1);
  }
}
static void testgc () {
  uniq(
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       GC_malloc(12), GC_malloc(12), GC_malloc(12), (GC_gcollect(),GC_malloc(12)),
       (void *)0);
}

extern void init_readline_variables();

int Macaulay2_main(argc,argv)
int argc; 
char **argv;
{
     char dummy;
     int returncode = 0;
     extern char *GC_stackbottom;
     char **x;
     char **saveenvp = NULL;
     int envc = 0;
     static int old_collections = 0;
     char **saveargv;
     int i;
     void main_inits();
     static void *reserve = NULL;
     extern void actors4_setupargv();
     extern void interp_process(), interp_process2();
     extern int interp_topLevel();

     //MES     GC_stackbottom = &dummy;

     ONSTACK(saveenvp);

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

#if defined(_WIN32) && !defined(__CYGWIN32__)
     _setmode(STDIN ,_O_BINARY);
     _setmode(STDOUT,_O_BINARY);
     _setmode(STDERR,_O_BINARY);
#endif

#ifdef __DJGPP__
     __file_handle_modes[STDIN ] = O_BINARY;
     __file_handle_modes[STDOUT] = O_BINARY;
     __file_handle_modes[STDERR] = O_BINARY;
#endif

#ifdef __MWERKS__
	/* Make sure we have lots and lots of stack space. 	*/
	SetMinimumStack(cMinStackSpace);
	/* Cheat and let stdio initialize toolbox for us.	*/
	/* printf("Macaulay2 for the MacOS\n"); */
     saveargv = argv;
#else
     /* save arguments on stack in case they're on the heap */
     saveargv = (char **)alloca((argc + 1)*sizeof(char *));
     for (i=0; i<argc; i++) {
	  saveargv[i] = alloca(strlen(argv[i]) + 1);
	  strcpy(saveargv[i],argv[i]);
     }
     saveargv[i] = NULL;
#endif

#if defined(DUMPDATA) && !defined(__MWERKS__) && !defined(__CYGWIN__)
     /* save environment on stack in case it's on the heap */
     for (envc=0, x=__environ; *x; x++) envc++;
     saveenvp = (char **)alloca((envc + 1)*sizeof(char *));
     for (i=0; i<envc; i++) {
	  saveenvp[i] = alloca(strlen(__environ[i]) + 1);
	  strcpy(saveenvp[i],__environ[i]);
     }
     saveenvp[i] = NULL;
#endif

#if !defined(__MWERKS__)
     ONSTACK(envc);
#endif

#ifdef MEM_DEBUG
     GC_all_interior_pointers = TRUE; /* set this before using gc routines!  (see gc.h) */
#endif
     GC_free_space_divisor = 2;	/* this was intended to be used only when we are about to dump data */

     if (0 != sigsetjmp(loaddata_jump,TRUE)) {
	  char **environ0;
     	  GC_free_space_divisor = 4;
	  if (GC_stackbottom == NULL) GC_stackbottom = &dummy;
	  old_collections = GC_gc_no;
#if defined(DUMPDATA) && !defined(__MWERKS__) && !defined(__CYGWIN__)
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
#endif
	  }

     system_stime();
     signal(SIGINT,interrupt_handler);

#ifdef SIGALRM
     signal(SIGALRM,alarm_handler);
#endif

#ifdef SIGPIPE
     signal(SIGPIPE, SIG_IGN);
#endif

     trap();
     progname = saveargv[0];
     /* for (p=progname; *p; p++) if (*p=='/') progname = p+1; */
     testgc();

     if (GC_stackbottom == NULL) GC_stackbottom = &dummy;
     M2_init_gmp();
     system_newline = tostring(newline);
     actors5_CCVERSION = tostring(get_cc_version());
     actors5_VERSION = tostring(PACKAGE_VERSION);
     actors5_OS = tostring(OS);
     actors5_ARCH = tostring(ARCH);
     actors5_NODENAME = tostring(NODENAME);
     actors5_REL = tostring(REL);
     actors5_LIBFACVERSION = tostring(get_libfac_version());
     actors5_FACTORYVERSION = tostring(FACTORYVERSION);
     actors5_DATE = tostring(current_date);
     actors5_TIME = tostring(current_time);
     actors5_startupString1 = tostring(startupString1);
     actors5_startupString2 = tostring(startupString2);
#ifdef DUMPDATA
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
     fprintf(stderr,"heap size = %d, divisor = %ld, collections = %ld\n", 
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no-old_collections);
#endif
     exit(returncode);
     return returncode;
     }

static void clean_up() {
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
     trap();
     }

void system_exit(x)
int x;
{
     clean_up();
     exit(x);
     }
     
int SPINCOUNT = 10000;
int spincount = 10000;		/* this one is decremented during loops */

void spincursor(){
     spincount = SPINCOUNT;
#ifdef __MWERKS__
     SpinCursor();
#endif
     }

int system_setspinspan(int n){
  int result = SPINCOUNT;
  SPINCOUNT = spincount = n;
  return result;
}

void system_spincursor(){
#ifdef __MWERKS__
  if (--spincount == 0) {
    SpinCursor();
    spincount = SPINCOUNT;
  }
#endif
}
void scclib__prepare(){}

extern int etext, end;

int system_dumpdata(M2_string datafilename)
{
     /* this routine should keep its data on the stack */
#ifndef DUMPDATA
     return ERROR;
#else
     bool haderror = FALSE;
     char *datafilename_s = tocharstar(datafilename);
     if (ERROR == dumpdata(datafilename_s)) haderror = TRUE;
     GC_FREE(datafilename_s);
     return haderror ? ERROR : OKAY;
#endif
     }

int system_loaddata(M2_string datafilename){
#ifndef DUMPDATA
     return ERROR;
#else
     char *datafilename_s = tocharstar(datafilename);
     sigjmp_buf save_loaddata_jump;
     int loadDepth = system_loadDepth;
     memcpy(save_loaddata_jump,loaddata_jump,sizeof(loaddata_jump));
     if (ERROR == loaddata(datafilename_s)) return ERROR;
     memcpy(loaddata_jump,save_loaddata_jump,sizeof(loaddata_jump));
     system_loadDepth = loadDepth + 1;
     siglongjmp(loaddata_jump,1);
#endif
     }

void C__prepare() {}

int actors4_isReady(int fd) {
#if defined(__MWERKS__) || defined(_WIN32)
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
#if defined(__DJGPP__) || defined(__alpha) || defined(__MWERKS__) || defined(_WIN32)
     return 0;
#else
     struct winsize x;
     ioctl(1,TIOCGWINSZ,&x);	/* see /usr/include/$SYSTEM/termios.h */
     return x.ws_col;
#endif
     }


