#include "probe.h"
#include "../dumpdata/printmaps.h"

char *a_string = "this is a read-only string";
int j;
#if defined(__APPLE__) && defined(__MACH__) && defined(__POWERPC__)
#else
extern edata;
extern etext;
extern end;
#endif
extern __bss_start__, __bss_end__;
extern __data_start__, __data_end__;
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define PAGESIZE (4*1024)

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifdef __DJGPP__
#include "../msdos/std.h"
#endif

int printf(const char *,...);

#ifdef __MWERKS__
#include <Processes.h>
#include <Resources.h>
#include <Memory.h>
#include <LowMem.h>
typedef struct {
	unsigned long aboveA5;
	unsigned long belowA5;
	unsigned long JTSize;
	unsigned long JTOffset;
} *CodeZeroPtr, **CodeZeroHandle;
#endif

#if !defined(__MWERKS__)
#define HAVE_SBRK 1
#if defined(__APPLE__) && defined(__MACH__) && defined(__POWERPC__)
/* sbrk is already defined in unistd.h */
#else
void *sbrk(int);
#endif
#endif

static int sig = -1;
jmp_buf buf;
void handler(int k) { sig = 1; longjmp(buf,1); }
void handler2(int k) { sig = 2; longjmp(buf,2); }
char statmem[3 * PAGESIZE];
int strarrlen(char **p) {
     int i=0;
     while (*p++) i++;
     return i;
     }
void *a_function();
int an_int;
extern char **environ;
int main(int argc, char **argv, char **envp){
     int i1;
     FILE *mapfile;
     char mapfilename[100];
     char c1;
     int i;
     char **pp;
     char c, *p, readable=FALSE, writable=FALSE;
#ifdef __MWERKS__
     CodeZeroHandle c;
#endif
#ifdef HAVE_SBRK
     char *membrk;
#endif
     char *x = malloc(PAGESIZE);
     char *y = malloc(PAGESIZE);
     void (*oldhandler)(int) = signal(SIGSEGV,handler);
#ifdef SIGBUS
     void (*oldhandler2)(int) = signal(SIGBUS,handler2);
#endif
     char c2;
     int i2;
#ifdef HAVE_SBRK
     membrk = sbrk(0);
#endif
#ifdef __MWERKS__
     /* Metrowerks Code Warrior */
     printf("%010p   SystemZone()\n",(int)SystemZone());
     printf("%010p   SystemZone()->bkLim\n",(int)SystemZone()->bkLim);
     printf("%010p   GetZone()\n",(int)GetZone());
     printf("%010p   ApplicZone()\n",(int)ApplicZone());
     printf("%010p   ApplicZone()->bkLim\n",(int)ApplicZone()->bkLim);
     printf("%010p   LMGetCurStackBase()\n",(int)LMGetCurStackBase());
     printf("%010p   LMGetMemTop()\n",(int)LMGetMemTop());
     printf("%010p   LMGetCurrentA5()\n",(int)LMGetCurrentA5());
     c = (CodeZeroHandle)GetResource('CODE', 0);
     if (c != NULL) {
     	  printf("%010p   data start = LMGetCurrentA5()-belowA5Size\n",
	       (int)(LMGetCurrentA5()-belowA5Size));
     	  ReleaseResource((Handle)c);
	  }
     printf("%010p   TopMem()\n",(int)TopMem());
     printf("%010p   MFTopMem()\n",(int)MFTopMem());
     printf("%010p   GetApplLimit()\n",(int)GetApplLimit());
#endif
     printf("%010p   function\n",(int)&main);
     printf("%010p   string\n",(int)a_string);
#if defined(__APPLE__) && defined(__MACH__) && defined(__POWERPC__)
#else
#if defined(__CYGWIN__)
#else
     printf("%010p   edata\n",(int)&edata);
     printf("%010p   etext\n",(int)&etext);
#endif
     printf("%010p   end\n",(int)&end);
#endif
     printf("%010p   &malloc()\n",(int) &malloc);
     printf("%010p   &write()\n",(int) &write);
     printf("%010p   static variable, initialized\n",(int) &a_string);
     printf("%010p   static memory, three pages, uninitialized\n",(int) &statmem);
     printf("%010p   static variable, uninitialized\n",(int) &an_int);
     printf("%010p   mallocated page 1\n",(int)x);
     printf("%010p   mallocated page 2\n",(int)y);
#ifdef HAVE_SBRK
     printf("%010p   memory break\n",(int)membrk);
#endif
     printf("%010p   variable on stack (&c1)\n",(int)&c1);
     printf("%010p   variable on stack (&c2)\n",(int)&c2);
     printf("%010p   variable on stack (&i1)\n",(int)&i1);
     printf("%010p   variable on stack (&i2)\n",(int)&i2);
     printf("%010p   variable on stack (&argc)\n",(int)&argc);
     printf("%010p   variable on stack (&argv)\n",(int)&argv);
     printf("%010p   variable on stack (&envp)\n",(int)&envp);
#ifdef __DJGPP__
     {
     extern char *__dos_argv0; 
     extern char *__djgpp_stackbottom;
     printf("%010p   bottom of stack (highest address)\n", (int)__djgpp_stackbottom);
     printf("%010p   __dos_argv0\n",(int)__dos_argv0);
     }
#endif
     printf("%010p   argv\n",(int)argv);
     for (i=0,pp=argv; *pp; i++,pp++) printf("%010p   argv[%d] : %s\n",(int)*pp,i,*pp);
     printf("%010p   null pointer at end of argv\n",(int)pp);
#if !defined(__MWERKS__)
     printf("%010p   envp\n",(int)envp);
     printf("%010p   environ\n",(int)environ);
     for (i=0,pp=envp; *pp; i++,pp++) printf("%010p   envp[%d] : %s\n",(int)*pp,i,*pp);
     printf("%010p   null pointer at end of envp\n",(int)pp);
     if (envp[0] != 0) {
	  int last;
	  char *x;
	  long b;
	  int *y;
     	  printf("%010p   envp[0]+strlen(envp[0])+1\n",(int)(envp[0]+strlen(envp[0]) + 1));
     	  last = strarrlen(envp)-1;
	  x = envp[last]+strlen(envp[last]) + 1;
     	  printf("%010p   envp[%d]+strlen(envp[%d])+1 : %s\n",(int)x,last,last,x);
	  b = (long)x + strlen(x) + 1;
	  if (b % 8 == 0) {
	    y = (int *)b;
	    printf("%010p   %010p\n",(int)y,*y);
	  }
     }
#endif

     sprintf(mapfilename,"/proc/%d/maps",getpid());
     mapfile = fopen(mapfilename,"r");
     if (mapfile != NULL) {
       while (TRUE) {
	 int c = getc(mapfile);
	 if (c == -1) break;
	 putchar(c);
       }
       fclose(mapfile);
     }

     fflush(stdout);
#if defined(__APPLE__) && defined(__MACH__) && defined(__POWERPC__)
#else
     printmaps(1);
#endif

#if 0
     /* debugging maps */
     { 
       int fd = open("ld-2.2.5.so-map-1",O_CREAT|O_WRONLY,0755); 
       write(fd,(void *)0x40000000,0x14000); 
     }
     /* -------------- */
#endif

     fflush(stdout);
#if !defined(__MWERKS__)
     for (p=0; p<membrk; p+=PAGESIZE) {
	  int oldsig = sig, oldreadable = readable, oldwritable = writable;
     	  signal(SIGSEGV,handler);
#ifdef SIGBUS
     	  signal(SIGBUS,handler2);
#endif
	  if (0 == setjmp(buf))  {
	       c = *p;		/* try reading a byte */
	       readable = TRUE;
     	       signal(SIGSEGV,handler);
#ifdef SIGBUS
     	       signal(SIGBUS,handler2);
#endif
	       if (0 == setjmp(buf)) {
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
	       printf("%010p . %s%s%s\n",
	       	    (int)p,
	       	    readable ? "r" : "-", 
	       	    writable ? "w" : "-",
	       	    sig == 1 ? "  SEGV" : sig == 2 ? "  BUS" : ""
	       	    );
	       }
	  }
#endif
     signal(SIGSEGV,oldhandler);
#ifdef SIGBUS
     signal(SIGBUS,oldhandler2);
#endif
     return 0;
     }

     
#if 0

Here are some results:
-----------------------------------------------------------------------------
HP-UX btrzx4 A.09.05 A 9000/735 2007700500 two-user license
size probe
47016 + 4880 + 31056 = 82952
probe | sort
00000000 r-  BUS
00001a68 string
000022e0 function
0000c7b0 etext
0000d000 --  SEGV
40001000 rw
400010d8 static variable
40002320 edata
40004b00 static memory, three pages, uninitialized
40007b4c static variable, uninitialized
40009c74 end
4000bc80 mallocated page 1
4000cc88 mallocated page 2
4000e000 memory break
7b0331dc argv
7b0331e4 envp
7b033304 variable on stack
-----------------------------------------------------------------------------
Linux homotopy 1.2.1 #13 Tue Apr 18 23:26:45 CDT 1995 i486
size probe
text	data	bss	dec	hex	filename
4064   	4096   	8364   	16524  	408c   	probe
probe | sort
00000000 --  SEGV
00001000 r-  SEGV
000010a8 string
00001258 function
00001980 etext
00002000 rw
00002008 static variable
00002078 edata
00002078 static variable, uninitialized
00002098 static memory, three pages, uninitialized
000050ac end
00009000 mallocated page 1
0000a000 mallocated page 2
0000b000 memory break
bffffc98 variable on stack
bffffcbc argv
bffffcc4 envp

#endif
