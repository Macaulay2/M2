/*		Copyright 1994 by Daniel R. Grayson		*/

#undef _POSIX_THREAD_SAFE_FUNCTIONS
#undef _REENTRANT

#define FACTORY 1
#define MP 1

#ifdef _WIN32
#undef FACTORY
#undef MP
#define alloca _alloca
#endif

#if defined(__DJGPP__)
#define NEWLINE "\r\n"
#elif defined(__MWERKS__)
#define NEWLINE "\r"
#else
#define NEWLINE "\n"
#endif

char newline[] = NEWLINE;

#ifdef FACTORY
extern char *libfac_version;
#endif

#include "../../Makeconf.h"	/* VERSION is defined here */

#ifdef __MWERKS__
#include ".._c_compat.h"
#else
#include "../c/compat.h"
#endif

#undef malloc
#undef free
#include <gmp.h>

#ifdef __MWERKS__
#include ".._c_compat.c"
#else
#include "../c/compat.c"
#endif

#ifdef HAS_UNISTD_H
#include <unistd.h>
#endif

#if !defined(__MWERKS__)
#include <sys/types.h>
#include <sys/stat.h>
#endif

#if !defined(__MWERKS__) && !defined(_WIN32)
#include <sys/time.h>
#include <sys/wait.h>
#endif

#include <signal.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#if !defined(_WIN32)
#include <alloca.h>
#endif

#include <setjmp.h>

#if defined(__DJGPP__) || defined(__MWERKS__) || defined(_WIN32)
#define HAVE_SOCKETS FALSE
#else
#define HAVE_SOCKETS TRUE
#include <sys/ioctl.h>		/* just for window width */
#include <termios.h>		/* just for window width */
#include <sys/mman.h>		/* needed for mmap() */
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#include <netdb.h>     	    	/* needed for gethostbyname() */
#include <netinet/in.h>	    	/* needed for struct sockaddr_in */
#include <arpa/inet.h>	   	/* needed for inet_addr() */
#endif

#ifdef __DJGPP__
#include <fcntl.h>
#include <libc/dosio.h>
#endif

#ifdef includeX11
#include <X11/Xlib.h>
#endif
#include "compat.h"

#ifndef O_BINARY
#define O_BINARY 0		/* for non msdos systems */
#endif

#define PAGESIZE getpagesize()
#define RUP(x) ((((x) + PAGESIZE - 1) / PAGESIZE) * PAGESIZE)

#if !(defined(GDBM) || defined(NDBM))
#define GDBM
#endif

#ifdef GDBM
#if 0
#include <gdbm.h>
#else
# ifdef __MWERKS__
#  include "::dbm:dbm.h"
# else
#  include "../dbm/dbm.h"
# endif
#endif
#define DBM_REPLACE GDBM_REPLACE
#define DBM_WRCREAT GDBM_WRCREAT
#define DBM_RD GDBM_READER
#define DBM_FILE GDBM_FILE
#define dbm_close gdbm_close
#define dbm_open gdbm_open
#define dbm_store gdbm_store
#define dbm_fetch gdbm_fetch
#define dbm_delete gdbm_delete
#define dbm_firstkey gdbm_firstkey
#define dbm_nextkey gdbm_nextkey
#define DBM_CREAT GDBM_CREAT
#define DBM_FAST GDBM_FAST
#endif

#ifdef NDBM
#include <ndbm.h>
#define DBM_FAST GDBM_FAST
#define DBM_CREAT O_CREAT
typedef DBM *DBM_FILE;
#define DBM_WRCREAT (O_RDWR | O_CREAT)
#define DBM_RD O_RDONLY
#endif

void *sbrk();		/* not really ansi standard, sigh */

#ifdef MP
#define link _link
#include <MP.h>
#undef link
#endif

#if defined(__NeXT__)
 /* on the NeXT Step i386 machine, brk always returns -1, and doesn't work. */
#   define brk(p) (int)sbrk(p-sbrk(0))
#else
int brk();		/* not really ansi standard, sigh */
#endif

#define STDIN 0
#define STDOUT 1
#define STDERR 2

#undef ERROR
#define ERROR (-1)      /* in Windows NT there is a file that sets ERROR to 0 */

static void putstderr(char *m) {
     write(STDERR,m,strlen(m));
     write(STDERR,NEWLINE,strlen(NEWLINE));
     }

#if defined(__NeXT__)
#   define STARTDATA (void *)0x00146000	/* provisional! */
#   define STARTGAP  (void *)0x00154000
#   define ENDGAP    (void *)0x007b6000
#   define ENDDATA   sbrk(0)
#   define HAVE_MMAP FALSE
#elif defined(__hp9000s800)
#   define STARTDATA (void *)0x40001000
#   define ENDDATA sbrk(0)
#   define HAVE_MMAP FALSE
#elif defined(__hp9000s700)
#   define STARTDATA (void *)0x40001000
#   define ENDDATA sbrk(0)
#   define HAVE_MMAP FALSE
#elif defined(__R3000) || defined(__sgi__)
#   define STARTDATA (void *)0x10000000
#   define ENDDATA sbrk(0)
#   define HAVE_MMAP FALSE
#elif defined(__DJGPP__)
    extern void *__djgpp_stack_limit;
    extern int _stklen;
#   define STARTDATA (void *)&etext
#   define STARTGAP (void *)&end
#   define ENDGAP (__djgpp_stack_limit + _stklen)
#   define ENDDATA sbrk(0)
#   define HAVE_MMAP FALSE
#elif defined(_WIN32)
#   define STARTDATA (char *)0
#   define ENDDATA (char *)0
#   define HAVE_MMAP FALSE
#else
#   define STARTDATA first_rw_page_after_etext()
#   define ENDDATA sbrk(0)
#   define HAVE_MMAP TRUE
#endif

static char *progname;
#ifdef includeX11
Display *display;
Font font;
#endif
void trap(){}

static void
#if defined(__STDC__) || defined(_WIN32)  
fatal(char *s,...)   {
     va_list ap;
#else
fatal( va_alist  ) 
va_dcl
{
     va_list ap;
     char *s;
#endif
#if defined(__STDC__) || defined(_WIN32)  
     va_start(ap,s);
#else
     va_start(ap);
     s = va_arg(ap, char *);
#endif
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
     trap();
     exit(1);
     }

bool system_interrupted = FALSE;
bool system_interruptPending = FALSE;
bool system_interruptShield = FALSE;
bool system_alarmed = FALSE;

#ifdef FACTORY
extern int libfac_interruptflag;
#endif

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

extern bool interp_StopIfError;
static jmp_buf loaddata_jump, out_of_memory_jump, abort_jump;
static bool out_of_memory_jump_set = FALSE, abort_jump_set = FALSE;

static void interrupt_handler(int sig)
{
     if (system_interrupted || system_interruptPending) {
	  if (isatty(STDIN)) while (TRUE) {
	       char buf[10];
	       printf("Abort (y/n)? ");
	       fgets(buf,sizeof(buf),stdin);
	       if (buf[0]=='y' || buf[0]=='Y') {
     		    trap();
		    if (!interp_StopIfError && abort_jump_set) {
     	  		 fprintf(stderr,"returning to top level\n");
     	  		 fflush(stderr);
			 system_interrupted = FALSE;
#     	   	     	 ifdef FACTORY
			 libfac_interruptflag = FALSE;
#     	   	     	 endif
			 system_interruptPending = FALSE;
			 system_interruptShield = FALSE;
			 system_alarmed = FALSE;
     	  		 longjmp(abort_jump,1);
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
	       system_interrupted = TRUE;
#     	       ifdef FACTORY
	       libfac_interruptflag = TRUE;
#     	       endif
	       }
	  }
     signal(SIGINT,interrupt_handler);
     }

void outofmem(){
     if (!interp_StopIfError && out_of_memory_jump_set) {
     	  fprintf(stderr,"out of memory, returning to top level");
     	  fflush(stderr);
     	  longjmp(out_of_memory_jump,1);
	  }
     else {
     	  fprintf(stderr,"out of memory, exiting\n");
	  exit(1);
	  }
     }

void fatalarrayindex(indx, len, file, line, column)
int indx;
int len;
char *file;
int line;
int column;
{
     char msg[100];
     sprintf(msg,"array index %d out of bounds 0 .. %d",indx,len-1);
     if (column == -1) {
     	  fatal(errfmtnc,file,line,msg);
	  }
     else {
     	  fatal(errfmt,file,line,column,msg);
	  }
     /* eventually when there is an interpreter we will have break loop here */
     }

void fatalarraylen(len, file, line, column)
int len;
char *file;
int line;
int column;
{
     char msg[100];
     sprintf(msg,"new array length %d less than zero",len);
     if (column == -1) {
     	  fatal(errfmtnc,file,line,msg);
	  }
     else {
     	  fatal(errfmt,file,line,column,msg);
	  }
     /* eventually when there is an interpreter we will have break loop here */
     }

void fatalrefctcheck(file,line,column)
char *file;
int line;
int column;
{
     if (column == -1) {
     	  fatal("%s:%d: item already freed (internal error)", file,line);
	  }
     else {
     	  fatal("%s:%d.%d: item already freed (internal error)",
	       file,line,column);
	  }
     }

static struct COUNTER { 
     int *count; char *filename; int lineno; char *funname;
     struct COUNTER *next;
     } *counters = NULL;

char *getmem(n)
unsigned int n;
{
     char *p;
     p = GC_MALLOC(n);
     if (p == NULL) outofmem();
     return p;
     }

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

bool system_gc = GaCo;

void system_accountfor(n)
int n;
{
     }

#ifdef DEBUG
static int trapnum=-1;
static int chunknum=0;
#endif

int system_returncode;

typedef struct {
     unsigned int len;
     char array[1];
     } *M2_string;

typedef struct {
     unsigned int len;
     int array[1];
     } *arrayint;

int system_write(int fd, M2_string buffer, int len){
     if (buffer->len < len) fatalarrayindex(len,buffer->len,__FILE__,__LINE__,-1);
     return write(fd,buffer->array,len);
     }

M2_string system_newline;

int system_read(fd,buffer,len)
int fd;
M2_string buffer;
int len;
{
     if (buffer->len < len) fatalarrayindex(len,buffer->len,__FILE__,__LINE__,-1);
     return read(fd,buffer->array,len);
     }

int system_read_1(fd,buffer,len,offset)
int fd;
M2_string buffer;
int len;
int offset;
{
     if (offset < 0) {
	  fatalarrayindex(offset,buffer->len,__FILE__,__LINE__,-1);
	  }
     if (buffer->len < len+offset) {
	  fatalarrayindex(len+offset,buffer->len,__FILE__,__LINE__,-1);
	  }
     return read(fd,buffer->array+offset,len);
     }

#define sizeofarray(s,len) (sizeof(*s) - sizeof(s->array) \
     + (len)*sizeof(s->array[0]))

typedef struct {
     unsigned int len;
     M2_string array[1];
     } *stringarray;

M2_string tostring(s)
char *s;
{
     int n = strlen(s);
     M2_string p = (M2_string)getmem(sizeofarray(p,n));
     p->len = n;
     memcpy(p->array,s,n);
     return p;
     }

M2_string tostringn(s,n)
char *s;
int n;
{
     M2_string p = (M2_string)getmem(sizeofarray(p,n));
     p->len = n;
     memcpy(p->array,s,n);
     return p;
     }

arrayint toarrayint(n,p)
int n;
int *p;
{
     arrayint z = (arrayint)getmem(sizeofarray(z,n));
     z->len = n;
     memcpy(z->array,p,n * sizeof(int));
     return z;
     }

stringarray tostrings(n,s)
int n;
char **s;
{
     int i;
     stringarray a;
     a = (stringarray) getmem (sizeofarray(a,n));
     a->len = n;
     for (i=0; i<n; i++) a->array[i] = tostring(s[i]);
     return a;
     }

stringarray system_envp;
stringarray system_argv;
stringarray system_args;
int system_reloaded;

struct FINAL {
     void (*final)();
     struct FINAL *next;
     } *final_list, *pre_final_list;

void system_atend(void (*f)()){
     struct FINAL *this_final = (struct FINAL *)getmem(sizeof(struct FINAL));
     this_final -> final = f;
     this_final -> next = pre_final_list;
     pre_final_list = this_final;
     }

void initrandom(){
#if 0
     extern char *initstate();
#endif
#if 0
     static char state[32];
     unsigned int seed = time(NULL);
     initstate(seed,(void *)state,sizeof(state));
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

extern char **environ;
extern char timestamp[];
static void clean_up();

void nop(void *x){}		/* used below to keep variables out of registers */

void *GC_malloc1 (size_t size_in_bytes) {
     void *p;
     p = GC_MALLOC_UNCOLLECTABLE(size_in_bytes);
     if (p == NULL) outofmem();
     return p;
     }

void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p = GC_REALLOC(s,new);
     if (p == NULL) outofmem();
     return p;
     }

void GC_free2 (void *s, size_t old) {
     GC_FREE(s);
     }

void main(argc,argv)
int argc; 
char **argv;
{
     char *p, **x;
     char **saveenvp = NULL;
     int envc = 0;
     char **saveargv;
     int i, n;
     void main_inits();
     static void *reserve = NULL;
     extern void actors4_setupargv();
     extern void interp_process();
     out_of_memory_jump_set = FALSE;
     abort_jump_set = FALSE;

#ifdef __DJGPP__
     __file_handle_modes[0] = O_BINARY;
     __file_handle_modes[1] = O_BINARY;
     __file_handle_modes[2] = O_BINARY;
#endif

#ifdef __MWERKS__
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

#if !defined(__MWERKS__)
     /* save environment on stack in case it's on the heap */
     for (envc=0, x=environ; *x; x++) envc++;
     saveenvp = (char **)alloca((envc + 1)*sizeof(char *));
     for (i=0; i<envc; i++) {
	  saveenvp[i] = alloca(strlen(environ[i]) + 1);
	  strcpy(saveenvp[i],environ[i]);
     }
     saveenvp[i] = NULL;
#endif

     for (n=1; ; n++) {
	  if (n >= argc) {
	       char buf[100];
	       sprintf(buf,"Macaulay 2, version %s",VERSION);
	       putstderr(buf);
	       putstderr("  Copyright 1993-1997, all rights reserved, D. R. Grayson and M. E. Stillman");
#     	       ifdef FACTORY
	       putstderr("  Factory library from Singular, copyright 1993-1997, G.-M. Greuel, R. Stobbe");
	       sprintf(buf,"  Factorization and characteristic sets %s, copyright 1996, M. Messollen",
		    libfac_version);
	       putstderr(buf);
#     	       endif
#     	       ifdef MP
	       sprintf(buf,"  MP %s, copyright 1993-1997, S. Gray, N. Kajler, P. Wang",MP_VERSION);
#     	       endif
	       putstderr("  GC, copyright 1996, Hans-J. Boehm, Alan J. Demers, Xerox, Silicon Graphics");
	       putstderr("  GNU libc and libg++, copyright 1996, Free Software Foundation");
	       putstderr("  GNU MP, copyright 1996, Free Software Foundation");
	       break;
       	       }
	  if (0 == strcmp(argv[n],"-silent")) break;
	  }
#if !defined(__MWERKS__)
     nop((void *)&envc);
#endif
     GC_free_space_divisor = 14;
     if (0 != setjmp(loaddata_jump)) {
	  char **environ0;
     	  GC_free_space_divisor = 4;
#if !defined(__MWERKS__)
     	  environ = saveenvp;	/* environ is a static variable that points
				   to the heap and has been overwritten by
				   loaddata(), thereby pointing to a previous
				   incarnation of the heap. */
	  /* Make a copy of the environment on the heap for 'environ'. */
	  /* In some systems, putenv() calls free() on the old item,
	     so we are careful to use malloc here, and not GC_malloc. */
	  environ0 = (char **)malloc((envc + 1)*sizeof(char *));
	  /* amazing but true:
	     On linux, malloc calls getenv to get values for tunable
	     parameters, so don't trash environ yet.
	     */
	  if (environ0 == NULL) fatal("out of memory");
	  for (i=0; i<envc; i++) {
	       environ0[i] = malloc(strlen(saveenvp[i]) + 1);
	       if (environ0[i] == NULL) fatal("out of memory");
	       strcpy(environ0[i],saveenvp[i]);
	  }
	  environ0[i] = NULL;
	  environ = environ0;
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
     for (p=progname; *p; p++) if (*p=='/') progname = p+1;
     if (getenv("GC_free_space_divisor")) {
	  GC_free_space_divisor = atoi(getenv("GC_free_space_divisor"));
	  if (GC_free_space_divisor <= 0) {
	       fprintf(stderr, "%s: non-positive GC_free_space_divisor value, %ld\n", 
		    progname, GC_free_space_divisor);
	       exit (1);
	       }
	  }
     if (getenv("GC_enable_incremental") && atoi(getenv("GC_enable_incremental"))==1) {
	  GC_enable_incremental();
	  fprintf(stderr,"GC_enable_incremental()\n");
	  }
     if (getenv("GC_expand_hp")) {
	  GC_expand_hp(atoi(getenv("GC_expand_hp")));
	  }
     mp_set_memory_functions(GC_malloc1,GC_realloc3,GC_free2);
     initrandom();
     system_newline = tostring(newline);
     system_envp = tostrings(envc,saveenvp);
     system_argv = tostrings(argc,saveargv);
     system_args = tostrings(argc == 0 ? 0 : argc - 1, saveargv + 1);
#ifdef includeX11
     display = XOpenDisplay(NULL);
     font = XLoadFont(display,"6x13");
#endif
     main_inits();
     actors4_setupargv();
     if (reserve == NULL) {
	  reserve = GC_MALLOC_ATOMIC(102400);
	  }
     if (setjmp(out_of_memory_jump)) {
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
	  }
     out_of_memory_jump_set = TRUE;
     setjmp(abort_jump);
     abort_jump_set = TRUE;
     interp_process();
     clean_up();
     fprintf(stderr,"heap size = %d, divisor = %ld, collections = %ld\n", 
	  GC_get_heap_size(), GC_free_space_divisor, GC_gc_no);
     exit(system_returncode);
     }

static void close_all_dbms();

static void close_all_links();

static void clean_up() {
     close_all_dbms();
     close_all_links();
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

/* interface routines */

char *tocharstar(s)
M2_string s;
{
     char *p = getmem(s->len + 1 + sizeof(int));
     memcpy(p,s->array,s->len);
     p[s->len] = 0;
     return p;
     }

char **tocharstarstar(p)
stringarray p;
{
     char **s = (char **)getmem((1 + p->len)*sizeof(char *));
     int i;
     for (i=0; i<p->len; i++) s[i] = tocharstar(p->array[i]);
     s[i] = NULL;
     return s;
     }

M2_string stdio_readfile(fd)
int fd;
{
     char *text;
     M2_string s;
     unsigned int size = 0, bufsize = 4096;
     text = getmem(bufsize);
     while (TRUE) {
	  int n = read(fd,text+size,bufsize-size);
	  if (ERROR == n) {
#ifdef EINTR
	       if (errno == EINTR) continue;
#endif
	       fatal("can't read file descriptor %d", fd);
	       }
	  if (0 == n) break;
	  size += n;
	  if (size == bufsize) {
	       char *p;
	       int newbufsize = 2 * bufsize;
	       p = getmem(newbufsize);
	       memcpy(p,text,size);
	       bufsize = newbufsize;
	       text = p;
	       }
	  }
     s = (M2_string)getmem(sizeofarray(s,size));
     s->len = size;
     memcpy(s->array,text,size);
     return s;
     }

M2_string strings_join(x,y)
M2_string x;
M2_string y;
{
     M2_string p;
     p = (M2_string) getmem(sizeofarray(p,x->len+y->len));
     p->len = x->len + y->len;
     memcpy(p->array,x->array,x->len);
     memcpy(p->array+x->len,y->array,y->len);
     return p;
     }

M2_string strings_substr(x,start,len)
M2_string x;
int start;
int len;
{
     M2_string p;
     if (start < 0) start = 0;
     if (start + len > x->len) len = x->len - start;
     if (len < 0) len = 0;
     p = (M2_string) getmem(sizeofarray(p,len));
     p->len = len;
     memcpy(p->array,x->array+start,len);
     return p;
     }

M2_string strings_substr_1(x,start)
M2_string x;
int start;
{
     return strings_substr(x,start,x->len - start);
     }
     
int SPINCOUNT = 10000;
int spincount = 10000;		/* this one is decremented during loops */

void spincursor(){
     spincount = SPINCOUNT;
     }

void system_setspinspan(int n){
     SPINCOUNT = spincount = n;
     }

int system_openin(filename)
M2_string filename;
{
     char *fname = tocharstar(filename);
     int fd;
     fd = open(fname, O_BINARY | O_RDONLY);
     GC_FREE(fname);
     return fd;
     }

int system_openout(filename)
M2_string filename;
{
     char *fname = tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_TRUNC
#ifndef __MWERKS__
	  , 0644
#endif
	  );
     GC_FREE(fname);
     return fd;
     }

int host_address(name)
char *name;
{
#if HAVE_SOCKETS
     if ('0' <= name[0] && name[0] <= '9') {
     	  int s;
	  s = inet_addr(name);
	  if (s == ERROR) return ERROR;
	  return s;
	  }
     else {
	  struct hostent *t = gethostbyname(name);
	  return t == NULL ? ERROR : *(int *)t->h_addr;
	  }
#else
     return ERROR;
#endif
     }

int serv_address(name)
char *name;
{
#if HAVE_SOCKETS
     if ('0' <= name[0] && name[0] <= '9') {
	  return htons(atoi(name));
	  }
     else {
	  struct servent *t = getservbyname(name,"tcp");
	  return t == NULL ? ERROR : t->s_port;
	  }
#else
     return ERROR;
#endif
     }

int opensocket(char *host, char *serv) {
#if HAVE_SOCKETS
     int sd = socket(AF_INET,SOCK_STREAM,0);
     int ha;
     struct sockaddr_in addr;
     addr.sin_family = PF_INET;
     ha = host_address(host);
     if (ha == ERROR) {
          close(sd);
	  return ERROR;
          }
     addr.sin_addr.s_addr = ha;
     addr.sin_port = serv_address(serv);
     if (ERROR == connect(sd,(struct sockaddr *)&addr,sizeof(addr))) {
	  close(sd);
	  return ERROR;
	  }
     else return sd;
#else
     return ERROR;
#endif
     }

int system_opensocket(host,serv)
M2_string host,serv;
{
     char *Host = tocharstar(host);
     char *Serv = tocharstar(serv);
     int sd = opensocket(Host,Serv);
     GC_FREE(Host);
     GC_FREE(Serv);
     return sd;
     }

M2_string system_syserrmsg()
{
     extern int errno, sys_nerr;
     extern char * sys_errlist[];
     return (
	  errno == 0 && errno < sys_nerr 
	  ? tostring("") 
	  : tostring(sys_errlist[errno])
	  );
     }

void scclib__prepare(){}

int system_run(M2_string command){
     char *c = tocharstar(command);
     int r = system(c);
     GC_FREE(c);
     return r >> 8;
     }

int system_pipe(fildes)
arrayint fildes;
{
     return pipe(fildes->array);
     }

int system_exec(argv)
stringarray argv;
{
     int i;
     char **av = tocharstarstar(argv);
     execvp(av[0],av);
     for (i=0; i<argv->len; i++) {
     	  GC_FREE(av[i]);
	  }
     GC_FREE(av);
     return ERROR;
     }

M2_string system_getcwd()
{
     char buf[700];
     char *x = getcwd(buf,sizeof(buf));
     if (x != NULL) return tostring(x);
     return tostring("");
     }

M2_string system_getenv(s)
M2_string s;
{
     char *ss = tocharstar(s);
     char *x = getenv(ss);
     GC_FREE(ss);
     if (x == NULL) return tostring("");
     else return tostring(x);
     }


int system_strcmp(s,t)
M2_string s,t;
{
     char *ss = tocharstar(s);
     char *tt = tocharstar(t);
     int r = strcmp(ss,tt);
     GC_FREE(ss);
     GC_FREE(tt);
     return r;
     }

int system_randomint() {
#if 0
     extern long random();
     return random();
#else
     extern long random00();
     return random00();
#endif
     }

int system_wait(pid)
int pid;
{
     int status;
     while (pid != wait(&status));
     return status>>8;
     }

unsigned int system_hash(x)
double x;
{
     unsigned int h = 0;
#if 0
     /* ieee version */
     x = scalbn(x,-ilogb(x)-1);	/* now x is less than 1 */
     x = scalbn(x,30);
     h = x;
     x = scalbn(x,30);
     h ^= (int) x;
#else
     unsigned char *p = (unsigned char *)&x;
     int i;
     for (i=0; i<sizeof(x); i++) {
	  h = 231*h + p[i];
	  }
#endif
     return h;
     }

extern etext, end;

#ifndef PAGESIZE
#define PAGESIZE 4096
#endif

static jmp_buf jumpbuffer;

static void handler(int k) 
{
     longjmp(jumpbuffer,1);
     }

static void handler2(int k) 
{
     longjmp(jumpbuffer,2);
     }

static void nothing (p)
char **p;
{}

#if !defined(__MWERKS__)
static void *first_rw_page_after_etext() {
     void (*oldhandler)(int) = signal(SIGSEGV,handler);
     char *p = (char *)RUP((long)&etext);
     for (;; p+=PAGESIZE) {
	  if (0 != setjmp(jumpbuffer))  {
	       signal(SIGSEGV,handler);	/* reset the handler */
	       }
	  else {
	       char *t = (char *)p;
	       char c = *t;
	       nothing(&p);	/* fool the optimizer */
	       *t = c;		/* try to write to page */
	       break;		/* break if writable */
	       }
	  }
     signal(SIGSEGV,oldhandler);
     return p;
     }
#endif

int probe();

int system_dumpdata(datafilename)
M2_string datafilename;
{
#if defined(__MWERKS__) || defined(_WIN32)
     return ERROR;
#else
     /* this routine should keep its data on the stack */
     char *datafilename_s = tocharstar(datafilename);
     int datafile = open(datafilename_s, O_BINARY | O_WRONLY | O_CREAT, 0666);
     if (datafile == ERROR) {
	  char buf[200];
	  sprintf(buf,"%s: dumpdata: couldn't open or create file %s for writing",
	       progname,datafilename_s);
	  perror(buf);
     	  GC_FREE(datafilename_s);
	  return ERROR;
	  }
     {
#if defined(STARTGAP) || defined(ENDGAP)
       	  if (ERROR == write(datafile,STARTDATA,STARTGAP - STARTDATA)
	       ||
	       ERROR == write(datafile,ENDGAP,ENDDATA - ENDGAP)
	       )
#else
	  if (ERROR == write(datafile,STARTDATA,ENDDATA-STARTDATA))
#endif
     	       {
	       char buf[200];
	       sprintf(buf,"%s: dumpdata: error while writing to file %s",
		    progname,datafilename_s);
	       perror(buf);
	       return ERROR;
     	       }
	  }
     if (ERROR == close(datafile)) {
	  char buf[200];
	  sprintf(buf,"%s: dumpdata: couldn't close file %s",
	       progname,datafilename_s);
	  perror(buf);
	  return ERROR;
	  }
     GC_FREE(datafilename_s);
     return 0;
#endif
     }

#undef min

int min(int i, int j) {
     return i<j ? i : j;
     }

#if !defined(__MWERKS__)
static void extend_memory(void *newbreak) {
     if (ERROR == brk(newbreak)) {
	  char buf[200];
	  sprintf(buf,"loaddata: out of memory (extending break from 0x%lx to 0x%lx)",
	       (long)sbrk(0), (long)newbreak);
	  perror(buf);
	  _exit(1);
	  }
     }
#endif

int probe() {
     int sig = -1;
     char c, *p, readable=FALSE, writable=FALSE;
     void (*oldhandler)(int) = signal(SIGSEGV,handler);
#ifdef SIGBUS
     void (*oldhandler2)(int) = signal(SIGBUS,handler2);
#endif
#if !defined(__MWERKS__)
     for (p=0; p<(char *)ENDDATA; p+=PAGESIZE) {
	  int oldsig = sig, oldreadable = readable, oldwritable = writable;
     	  signal(SIGSEGV,handler);
#ifdef SIGBUS
     	  signal(SIGBUS,handler2);
#endif
	  if (0 == (sig = setjmp(jumpbuffer)))  {
	       c = *p;		/* try reading a byte */
	       readable = TRUE;
     	       signal(SIGSEGV,handler);
#ifdef SIGBUS
     	       signal(SIGBUS,handler2);
#endif
	       if (0 == (sig = setjmp(jumpbuffer))) {
		    *p = c;	/* try writing a byte */
		    writable = TRUE;
		    }
	       else {
		    writable = FALSE;
		    }
	       }
	  else {
	       writable = readable = FALSE;
	       }
	  if (oldsig != sig || oldreadable != readable || oldwritable != writable) {
	       char buf[80];
	       sprintf(buf,"%16lx . %s%s%s",
	       	    (long)p,
	       	    readable ? "r" : "-", 
	       	    writable ? "w" : "-",
	       	    sig == 1 ? "  SEGV" : sig == 2 ? "  BUS" : ""
	       	    );
	       putstderr(buf);
	       }
	  }
     {
	  char buf[80];
	  sprintf(buf,"%16lx .",
	       (long)p
	       );
	  putstderr(buf);
	       }
#endif
     signal(SIGSEGV,oldhandler);
#ifdef SIGBUS
     signal(SIGBUS,oldhandler2);
#endif
     return 0;
     }

int system_loaddata(M2_string datafilename){
#if defined(__MWERKS__) || defined(_WIN32)
     return ERROR;
#else
     char *datafilename_s = tocharstar(datafilename);
     char savetimestamp[60];
     struct stat statbuf;
     jmp_buf save_loaddata_jump;
     int filelen;
     int reloaded = system_reloaded;
     int datafile = open(datafilename_s, O_BINARY | O_RDONLY);
     memcpy(save_loaddata_jump,loaddata_jump,sizeof(loaddata_jump));
     strcpy(savetimestamp,timestamp);
     if (datafile == ERROR) {
	  char buf[200];
	  sprintf(buf,"%s: couldn't open file %s for reading",
	       progname,datafilename_s);
	  putstderr(buf);
     	  GC_FREE(datafilename_s);
	  return ERROR;
	  }
     GC_FREE(datafilename_s);
     fstat(datafile,&statbuf);
     filelen = statbuf.st_size;
#if defined(STARTGAP) || defined(ENDGAP)
     {
     void *loc1 = STARTDATA;
     int len1 = STARTGAP - loc1 ;
     void *loc2 = ENDGAP;
     int len2 = filelen - len1;
     extend_memory(loc2 + len2);
     if ( len1 != read(datafile, loc1, len1) ||
	  len2 != read(datafile, loc2, len2)
	  ) {
          char buf[200];
          sprintf(buf,"loaddata: can't read file (%08x-%08x %08x-%08x, sbrk %08\
x)",
                 loc1, loc1+len1,
                 loc2, loc2+len2,
                 sbrk(0));
          putstderr(buf);
          probe();
	  _exit(1);
	  }
     }
#elif HAVE_MMAP
     {
     char *loc = STARTDATA;
     extend_memory(loc+filelen);
     if (loc != mmap( loc,
	       filelen,
	       PROT_READ|PROT_WRITE,MAP_FIXED|MAP_PRIVATE,
	       datafile, 0)) {
	  char buf[200];
	  sprintf(buf,"loaddata: error while mapping file (length 0x%x at 0x%lx)",
	       filelen, (long)loc);
	  putstderr(buf);
	  _exit(1);
	  }
     }
#else
     {
     char *loc = STARTDATA;
     extend_memory(loc+filelen);
     if (filelen != read(datafile,loc,filelen)) {
	  char buf[200];
	  sprintf(buf,"loaddata: error while reading file (length 0x%x at 0x%lx)",
	       filelen, (long)loc);
	  putstderr(buf);
	  _exit(1);
	  }
     }
#endif
     close(datafile);
     if (0 != strcmp(savetimestamp,timestamp)) {
	  putstderr("data file not created by this executable");
	  _exit(1);
	  }
     memcpy(loaddata_jump,save_loaddata_jump,sizeof(loaddata_jump));
     system_reloaded = reloaded + 1;
     longjmp(loaddata_jump,1);
#endif
     }

#define FAILURE 0

#ifdef includeX11
unsigned int X_XCreateWindow(parent,x,y,width,height,borderwidth,name)
unsigned int parent;
int x,y,width,height,borderwidth;
M2_string name;
{
     if (display != NULL) {
	  Colormap colormap = DefaultColormap(display, DefaultScreen(display));
     	  Window w;
	  XColor color;
     	  static XSetWindowAttributes attr;
	  char *sname = tocharstar(name);
	  if ( FAILURE != XParseColor(display,colormap,"red",&color)
	       &&
	       FAILURE != XAllocColor(display,colormap,&color)) {
	       attr.border_pixel = color.pixel;
	       }
	  w = XCreateWindow(
	       display,
	       parent,
	       x,y,
	       width,height,
	       borderwidth,
	       CopyFromParent,	/* depth */
	       InputOutput,		/* class */
	       CopyFromParent,	/* visual */
	       CWBorderPixel,     	/* attribute mask */
	       &attr			/* attribute structure */
	       );
	  XStoreName(display,w,sname);
     	  XSetWindowBackground(display,w,
	       BlackPixel(display,XDefaultScreen(display)));
	  XMapWindow(display,w);
	  XFlush(display);
     	  while (XPending(display) > 0) {
	       XEvent event;
	       XNextEvent(display, &event);
	       }
	  GC_FREE(sname);
	  return w;
	  }
     else return 0;
     }

unsigned int X_XDefaultRootWindow(){
     if (display != NULL) {
	  return XDefaultRootWindow(display);
	  }
     else return 0;
     }
#endif

/**********************************************
 *                  dbm stuff                 *
 **********************************************/

static int numfiles = 0;
static DBM_FILE *dbm_files = NULL;
static void close_all_dbms() {
     int i;
     for (i=0; i<numfiles; i++) {
	  if (dbm_files[i] != NULL) dbm_close(dbm_files[i]);
	  }
     }

int system_dbmopen(M2_string filename, bool mutable) {
     int dbm_handle;
     int flags = mutable ? DBM_WRCREAT : DBM_RD;
     int mode = 0666;
     char *FileName = tocharstar(filename);
#ifdef GDBM
     DBM_FILE f = dbm_open(FileName, 0, flags, mode, NULL);
#endif
#ifdef NDBM
     DBM_FILE f = dbm_open(FileName, flags, mode);
#endif
     GC_FREE(FileName);
     if (f == NULL) return ERROR;
     if (numfiles == 0) {
	  int i;
	  numfiles = 10;
	  dbm_files = (DBM_FILE *) getmem(numfiles * sizeof(DBM_FILE));
	  for (i=0; i<numfiles; i++) dbm_files[i] = NULL;
	  dbm_handle = 0;
	  }
     else {
	  for (dbm_handle=0; TRUE ; dbm_handle++) {
	       if (dbm_handle==numfiles) {
		    DBM_FILE *p;
		    int j;
		    numfiles *= 2;
		    p = (DBM_FILE *) getmem(numfiles * sizeof(DBM_FILE));
		    for (j=0; j<dbm_handle; j++) p[j] = dbm_files[j];
		    dbm_files = p;
	  	    for (j=dbm_handle; j<numfiles; j++) dbm_files[j] = NULL;
		    break;
		    }
	       else if (dbm_files[dbm_handle] == NULL) break;
	       }
	  }
     dbm_files[dbm_handle] = f;
     return dbm_handle;
     }

int system_dbmclose(int handle) {
     dbm_close(dbm_files[handle]);
     dbm_files[handle] = NULL;
     return 0;
     }

static datum todatum(M2_string x) {
     datum y;
     y.dptr = x->array;
     y.dsize = x->len;
     return y;
     }

static M2_string fromdatum(datum y) {
     M2_string x;
     if (y.dptr == NULL) return NULL;
     x = (M2_string)getmem(sizeofarray(x,y.dsize));
     x->len = y.dsize;
     memcpy(x->array, y.dptr, y.dsize);
     return x;
     }

int system_dbmstore(int handle, M2_string key, M2_string content) {
     return dbm_store(dbm_files[handle],todatum(key),todatum(content),DBM_REPLACE);
     }

M2_string /* or NULL */ system_dbmfetch(int handle, M2_string key) {
     return fromdatum(dbm_fetch(dbm_files[handle],todatum(key)));
     }

int system_dbmdelete(int handle, M2_string key) {
     return dbm_delete(dbm_files[handle],todatum(key));
     }

static datum lastkey;
static bool hadlastkey = FALSE;

M2_string /* or NULL */ system_dbmfirst(int handle) {
     lastkey = dbm_firstkey(dbm_files[handle]);
     hadlastkey = TRUE;
     return fromdatum(lastkey);
     }

M2_string /* or NULL */ system_dbmnext(int handle) {
     if (hadlastkey) {
	  lastkey = dbm_nextkey(dbm_files[handle]
#ifdef GDBM
	       ,lastkey
#endif
	       );
	  hadlastkey = TRUE;
	  return fromdatum(lastkey);
	  }
     else {
	  return system_dbmfirst(handle);
	  }
     }

int system_dbmreorganize(int handle) {
#ifdef GDBM
     return gdbm_reorganize(dbm_files[handle]);
#else
     return -1;
#endif
     }

M2_string system_dbmstrerror() {
#ifdef GDBM
#if 0
     return tostring(gdbm_strerror(gdbm_errno));
#else
     return tostring("database error");
#endif
#else
     return tostring("database error");
#endif
     }

/**********************************************
 *                  MP stuff                  *
 **********************************************/

#ifdef MP

static int numlinks = 0;
static MP_Link_pt *mp_links = NULL;

static int okay(n)
int n;
{
     return 0 <= n && n < numlinks && mp_links[n] != NULL;
     }

static void close_all_links() {
     int i;
     for (i=0; i<numlinks; i++) {
	  if (mp_links[i] != NULL) MP_CloseLink(mp_links[i]);
	  }
     }

static MP_Env_pt MP_env;

int mp_OpenLink(args) 
stringarray args;
{
     int link_handle;
     char **argv;
     int argc, i;
     MP_Link_pt f;
     if (MP_env == NULL) {
	  MP_env = MP_InitializeEnv(NULL);
	  }
     argv = tocharstarstar(args);
     argc = args->len;
     f = MP_OpenLink(MP_env,argc,argv);
     for (i=0; i<argc; i++) {
     	  GC_FREE(argv[i]);
	  }
     GC_FREE(argv);
     MP_SetLinkOption(f,MP_LINK_LOG_MASK_OPT,MP_LOG_ALL_EVENTS);
     if (f == NULL) return -1;
     if (numlinks == 0) {
	  numlinks = 10;
	  mp_links = (MP_Link_pt *) getmem(numlinks * sizeof(DBM_FILE));
	  for (i=0; i<numlinks; i++) mp_links[i] = NULL;
	  link_handle = 0;
	  }
     else {
	  for (link_handle=0; TRUE ; link_handle++) {
	       if (link_handle==numlinks) {
		    MP_Link_pt *p;
		    int j;
		    numlinks *= 2;
		    p = (MP_Link_pt *) getmem(numlinks * sizeof(DBM_FILE));
		    for (j=0; j<link_handle; j++) p[j] = mp_links[j];
		    mp_links = p;
	  	    for (j=link_handle; j<numlinks; j++) mp_links[j] = NULL;
		    break;
		    }
	       else if (mp_links[link_handle] == NULL) break;
	       }
	  }
     mp_links[link_handle] = f;
     return link_handle;
     }

int mp_CloseLink(handle) 
int handle;
{
     int r;
     if (!okay(handle)) return ERROR;
     r = MP_PutStringPacket(mp_links[handle],"MP:Quit",0);
     MP_EndMsgReset(mp_links[handle]);
     MP_CloseLink(mp_links[handle]);
     mp_links[handle] = NULL;
     return 0;
     }

int mp_EndMsgReset(handle)
int handle;
{
     if (okay(handle)) {
	  return MP_EndMsgReset(mp_links[handle]);
	  }
     else return ERROR;
     }

int mp_PutSint32(handle,n) 
int handle;
int n;
{
     if (okay(handle)) {
	  return MP_PutSint32Packet(mp_links[handle],n,0);
	  }
     else return ERROR;
     }

int mp_PutString(handle,s)
int handle;
M2_string s;
{
     if (okay(handle)) {
     	  char *t = tocharstar(s);
	  int r = MP_PutStringPacket(mp_links[handle],t,0);
	  GC_FREE(t);
	  return r;
	  }
     else return ERROR;
     }


int mp_PutIdentifier(handle,s)
int handle;
M2_string s;
{
     if (okay(handle)) {
     	  char *t = tocharstar(s);
	  int r = MP_PutIdentifierPacket(mp_links[handle],MP_ReceiverDict,t,0);
	  GC_FREE(t);
	  return r;
	  }
     else return ERROR;
     }

int mp_RawPutSint32(handle,n) 
int handle;
int n;
{
     if (okay(handle)) {
	  return IMP_PutSint32(mp_links[handle],n);
	  }
     else return ERROR;
     }

int mp_RawPutString(handle,s)
int handle;
M2_string s;
{
     if (okay(handle)) {
     	  char *t = tocharstar(s);
	  int r = IMP_PutString(mp_links[handle],t);
	  GC_FREE(t);
	  return r;
	  }
     else return ERROR;
     }


int mp_RawPutIdentifier(handle,s)
int handle;
M2_string s;
{
     if (okay(handle)) {
     	  char *t = tocharstar(s);
	  int r = IMP_PutIdentifier(mp_links[handle],t);
	  GC_FREE(t);
	  return r;
	  }
     else return ERROR;
     }

int mp_PutListOperator(handle,len)
int handle, len;
{
     if (okay(handle)) {
	  return MP_PutCommonOperatorPacket(mp_links[handle],
	       MP_BasicDict,MP_CopBasicList,0,len);
	  }
     else return ERROR;
     }

int mp_PutCommonOperator(handle,dict,oper,numannot,len)
int handle, dict, oper, numannot, len;
{
     if (okay(handle)) {
	  return MP_PutCommonOperatorPacket(
	       mp_links[handle],
	       dict,
	       oper,
	       numannot,
	       len);
	  }
     else return ERROR;
     }

int mp_PutAnnotationPacket(handle,dict,atype,aflags)
int handle, dict, atype, aflags;
{
     if (okay(handle)) {
	  return MP_PutAnnotationPacket(
	       mp_links[handle],
	       dict,
	       atype,
	       aflags);
	  }
     else return ERROR;
     }

int mp_PutCommonMetaOperatorPacket(handle,dict, oper,numannot,numchildren)
int handle, dict, oper, numannot, numchildren;
{
     if (okay(handle)) {
	  return MP_PutCommonMetaOperatorPacket(
	       mp_links[handle],
	       dict,
	       oper,
	       numannot,
	       numchildren
	       );
	  }
     else return ERROR;
     }

int mp_PutCommonMetaTypePacket(handle,dict, oper,numannot)
int handle, dict, oper, numannot;
{
     if (okay(handle)) {
	  return MP_PutCommonMetaTypePacket(
	       mp_links[handle],
	       dict,
	       oper,
	       numannot
	       );
	  }
     else return ERROR;
     }

int mp_PutOperatorPacket(handle,dict,oper1,numannot,len)
int handle, dict, numannot, len;
M2_string oper1;
{
     int r;
     char *oper = tocharstar(oper1);
     if (okay(handle)) {
	  r = MP_PutOperatorPacket(
	       mp_links[handle],
	       dict,
	       oper,
	       numannot,
	       len);
	  }
     else r = ERROR;
     GC_FREE(oper);
     return r;
     }

#else

static void close_all_links() {
     }

int mp_OpenLink(args) 
stringarray args;
{
     return ERROR;
     }

int mp_CloseLink(handle) 
int handle;
{
     return ERROR;
     }

int mp_EndMsgReset(handle)
int handle;
{
     return ERROR;
     }

int mp_PutSint32(handle,n) 
int handle;
int n;
{
     return ERROR;
     }

int mp_PutString(handle,s)
int handle;
M2_string s;
{
     return ERROR;
     }


int mp_PutIdentifier(handle,s)
int handle;
M2_string s;
{
     return ERROR;
     }

int mp_RawPutSint32(handle,n) 
int handle;
int n;
{
     return ERROR;
     }

int mp_RawPutString(handle,s)
int handle;
M2_string s;
{
     return ERROR;
     }


int mp_RawPutIdentifier(handle,s)
int handle;
M2_string s;
{
     return ERROR;
     }

int mp_PutListOperator(handle,len)
int handle, len;
{
	return ERROR;
     }

int mp_PutCommonOperator(handle,dict,oper,numannot,len)
int handle, dict, oper, numannot, len;
{
     return ERROR;
     }

int mp_PutAnnotationPacket(handle,dict,atype,aflags)
int handle, dict, atype, aflags;
{
    return ERROR;
     }

int mp_PutCommonMetaOperatorPacket(handle,dict, oper,numannot,numchildren)
int handle, dict, oper, numannot, numchildren;
{
     return ERROR;
     }

int mp_PutCommonMetaTypePacket(handle,dict, oper,numannot)
int handle, dict, oper, numannot;
{
	return ERROR;
     }

int mp_PutOperatorPacket(handle,dict,oper1,numannot,len)
int handle, dict, numannot, len;
M2_string oper1;
{
     return ERROR;
     }

#endif

void C__prepare() {}

int actors5_WindowWidth(int fd) {
#if defined(__DJGPP__) || defined(__alpha) || defined(__MWERKS__) || defined(_WIN32)
     return 0;
#else
     struct winsize x;
     ioctl(1,TIOCGWINSZ,&x);	/* see /usr/include/$SYSTEM/termios.h */
     return x.ws_col;
#endif
     }

#if 0
#include <regex.h>

regex_t regex;
M2_string last_pattern;

int actors5_rxmatch(M2_string text, M2_string pattern) {
     regmatch_t match;
     char *s_text;
     int ret;
     if (pattern != last_pattern) {
	  char *s_pattern = tocharstar(pattern);
	  ret = regcomp(&regex, s_pattern, REG_NEWLINE|REG_NOSUB);
	  GC_FREE(s_pattern);
	  if (ret != 0) return ERROR;
	  }
     s_text = tocharstar(text); /* end strings with 0's! */
     ret = regexec(&regex, s_text, 1, &match, REG_NOTEOL);
     GC_FREE(s_text);
     if (ret == 0) return match.rm_so;
     else if (ret == REG_NOMATCH) return -2;
     else return ERROR;
     }
#endif

#if defined(__DJGPP__) || defined(_WIN32)
double lgamma(double x) { return -1. ; }	/* sigh, fix later */
#endif

#ifdef _WIN32
#ifndef ENOSYS
#define ENOSYS 0
#endif
int fork() { return ERROR; }
int pipe(int v[2]) { return ERROR; }
int wait() { return ERROR; }
int alarm(int i) { return ERROR ; }
int sleep(int i) { return ERROR; }
int getpagesize() { return 4096; }
int brk() { return 0; }
void *sbrk(int i) { return 0; }
void *getprotobyname() { errno = ENOSYS; return 0; }
int accept() { errno = ENOSYS; return -1; }
int bind() { errno = ENOSYS; return -1; }
int listen() { errno = ENOSYS; return -1; }
int socket() { errno = ENOSYS; return -1; }
void *gethostbyname() { errno = ENOSYS; return (void *)0; }
int inet_addr() { errno = ENOSYS; return -1; }
void *getservbyname() { errno = ENOSYS; return (void *)0; }
void *authdes_create() { errno = ENOSYS; return (void *)0; }
void *xdrmem_create() { errno = ENOSYS; return (void *)0; }
int connect() { errno = ENOSYS; return -1; }
int setsockopt() { errno = ENOSYS; return -1; }
short htons(short x) { return x; }
#endif

#ifdef __MWERKS__
#ifndef ENOSYS
#define ENOSYS 0
#endif
#undef getpid
int getpid(){ return ERROR; }
int dup2() { return ERROR; }
int fork() { return ERROR; }
int pipe(int v[2]) { return ERROR; }
int wait() { return ERROR; }
int alarm(int i) { return ERROR ; }
int sleep(int i) { return ERROR; }
int getpagesize() { return 4096; }
int brk() { return 0; }
void *sbrk(int i) { return 0; }
void *getprotobyname() { errno = ENOSYS; return 0; }
int accept() { errno = ENOSYS; return -1; }
int bind() { errno = ENOSYS; return -1; }
int listen() { errno = ENOSYS; return -1; }
int socket() { errno = ENOSYS; return -1; }
void *gethostbyname() { errno = ENOSYS; return (void *)0; }
int inet_addr() { errno = ENOSYS; return -1; }
void *getservbyname() { errno = ENOSYS; return (void *)0; }
void *authdes_create() { errno = ENOSYS; return (void *)0; }
void *xdrmem_create() { errno = ENOSYS; return (void *)0; }
int connect() { errno = ENOSYS; return -1; }
int setsockopt() { errno = ENOSYS; return -1; }
short htons(short x) { return x; }
#endif
