/*		Copyright 1993 by Daniel R. Grayson		*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#define TRUE 1
#define FALSE 0
#define ERROR (-1)
#define STDIN 0
#define STDOUT 1
#define STDERR 2
#define IN 0
#define OUT 1
#define TERMCHAR '\1'


static int tochild[2], fromchild[2], in, out, pid;

void startM2() {
     char buf[1];
     int r;
     pipe(tochild);
     pipe(fromchild);
     pid = fork();
     if (pid==0) {
	  char buf[100];
	  dup2(tochild[IN],STDIN);
	  close(tochild[IN]);
	  close(fromchild[IN]);
	  close(tochild[OUT]);
	  dup2(fromchild[OUT],STDOUT);
	  close(fromchild[OUT]);
	  system("M2 -q -tty -silent -n -x bare.m2");
	  exit(0);
	  }
     else {
	  char buf[4096];
	  in = fromchild[IN];
     	  close(fromchild[OUT]);
	  out = tochild[OUT];
     	  close(tochild[IN]);
	  r = read(in,buf,sizeof(buf));
	  if (r == 1 && buf[0] == TERMCHAR) return;
	  if (r == ERROR) perror("read failed from child");
	  else fprintf(stderr,"expected C-A from child\n");
	  kill(pid,9);
	  wait(NULL);
	  exit(1);
	  }
     }

void outofmem(){
     exit(1);
     }

char *evalM2(char *s) {
     int size = 1024, used = 0;
     char *buf = malloc(size);
     if (buf == NULL) outofmem();
     write(out,s,strlen(s));
     write(out,"\n",1);
     while (TRUE) {
     	  int r;
	  if (size == used) {
	       size *= 2;
	       buf = realloc(buf,size);
     	       if (buf == NULL) outofmem();
	       }
	  r = read(in,buf+used,size-used);
     	  if (r==ERROR) {
	       perror("read failed from child");
	       wait(NULL);
	       exit(1);
	       }
     	  else if (r==0 || buf[used+r-1] == TERMCHAR) {
	       if (r!=0) buf[used+r-1]=0;
	       return buf;
	       }
	  else used += r;
	  }
     }


void stopM2() {
     close(out);
     wait(NULL);
     }

void try(char *s) {
     char *t;
     /* printf("evaluating: \"%s\"\n", s); */
     t = evalM2(s);
     printf("  value is: \"%s\"\n", t);
     }

main(argc, argv)
int argc;
char **argv;
{
     startM2();
     try("2+2");
     try("100!");
     try("R = ZZ/101[a..c]");
     try("I = ideal symmetricPower(2,vars R)");
     try("C = res I");
     try("C.dd");
     try("C.dd_2");
     try("ZZ[x]");
     stopM2();
     }
