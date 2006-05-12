#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>

#include "probe.h"
#if DUMPDATA
#include "../dumpdata/printmaps.h"
#endif

char *a_string = "this is a read-only string";
int j;

#define PFMT "%010p"

#pragma weak __bss_end__
#pragma weak __bss_start__
#pragma weak __data_end__
#pragma weak __data_start__
#pragma weak edata
#pragma weak end
#pragma weak etext
extern __bss_end__, __bss_start__, __data_end__, __data_start__, edata, end, etext;

#define PAGESIZE (4*1024)

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
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
     char c1;
     int i;
     char **pp;
     char c, *p, readable=FALSE, writable=FALSE;
     char *membrk;
     char *x = malloc(PAGESIZE);
     char *y = malloc(PAGESIZE);
#if 0
     void (*oldhandler)(int) = signal(SIGSEGV,handler);
#ifdef SIGBUS
     void (*oldhandler2)(int) = signal(SIGBUS,handler2);
#endif
#endif
     char c2;
     int i2;
     membrk = sbrk(0);
     printf(PFMT "   function\n",&main);
     printf(PFMT "   string\n",a_string);
     printf(PFMT "   edata\n",&edata);
     printf(PFMT "   etext\n",&etext);
     printf(PFMT "   end\n",&end);
     printf(PFMT "   &malloc()\n",&malloc);
     printf(PFMT "   &write()\n",&write);
     printf(PFMT "   static variable, initialized\n",&a_string);
     printf(PFMT "   static memory, three pages, uninitialized\n",&statmem);
     printf(PFMT "   static variable, uninitialized\n",&an_int);
     printf(PFMT "   mallocated page 1\n",x);
     printf(PFMT "   mallocated page 2\n",y);
     printf(PFMT "   memory break\n",membrk);
     printf(PFMT "   variable on stack (&c1)\n",&c1);
     printf(PFMT "   variable on stack (&c2)\n",&c2);
     printf(PFMT "   variable on stack (&i1)\n",&i1);
     printf(PFMT "   variable on stack (&i2)\n",&i2);
     printf(PFMT "   variable on stack (&argc)\n",&argc);
     printf(PFMT "   variable on stack (&argv)\n",&argv);
     printf(PFMT "   variable on stack (&envp)\n",&envp);
     printf(PFMT "   argv\n",argv);
     for (i=0,pp=argv; *pp; i++,pp++) printf(PFMT "   argv[%d] : %s\n",*pp,i,*pp);
     printf(PFMT "   null pointer at end of argv\n",pp);
     printf(PFMT "   envp\n",envp);
     printf(PFMT "   environ\n",environ);
     for (i=0,pp=envp; *pp; i++,pp++) printf(PFMT "   envp[%d] : %s\n",*pp,i,*pp);
     printf(PFMT "   null pointer at end of envp\n",pp);
     if (envp[0] != 0) {
	  int last;
	  char *x;
	  long b;
	  int *y;
     	  printf(PFMT "   envp[0]+strlen(envp[0])+1\n",(envp[0]+strlen(envp[0]) + 1));
     	  last = strarrlen(envp)-1;
	  x = envp[last]+strlen(envp[last]) + 1;
     	  printf(PFMT "   envp[%d]+strlen(envp[%d])+1 : %s\n",x,last,last,x);
	  b = (long)x + strlen(x) + 1;
	  if (b % 8 == 0) {
	    y = (int *)b;
	    printf(PFMT "   %p\n",y,*y);
	  }
     }

     mapfile = fopen("/proc/self/maps","r");
     if (mapfile != NULL) {
       while (TRUE) {
	 int c = getc(mapfile);
	 if (c == -1) break;
	 putchar(c);
       }
       fclose(mapfile);
     }

     fflush(stdout);
#if DUMPDATA
     printmaps(1);
#endif
     fflush(stdout);
#if 0
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
	       printf(PFMT " . %s%s%s\n", p, readable ? "r" : "-", writable ? "w" : "-", sig == 1 ? "  SEGV" : sig == 2 ? "  BUS" : "" );
	       }
	  }
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
