/*		Copyright 1993 by Daniel R. Grayson		*/

#include <stdio.h>
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

#define reroutestderr 0

main(argc, argv)
int argc;
char **argv;
{
     int tochild[2], fromchild[2], in, out;
     pipe(tochild);
     pipe(fromchild);
     if (fork()==0) {
	  char buf[100];
	  dup2(tochild[IN],STDIN);
	  close(tochild[IN]);
	  close(fromchild[IN]);
	  close(tochild[OUT]);
	  dup2(fromchild[OUT],STDOUT);
#if reroutestderr
	  dup2(STDERR,3);
	  dup2(fromchild[OUT],STDERR);
#endif
	  close(fromchild[OUT]);
	  argv++;
	  execvp(argv[0],argv);
	  sprintf(buf,"failed to exec %s", argv[0]);
#if reroutestderr
	  dup2(3,STDERR);
#endif
	  perror(buf);
	  exit(1);
	  }
     else {
	  char buf[4096];
	  in = fromchild[IN];
     	  close(fromchild[OUT]);
	  out = tochild[OUT];
     	  close(tochild[IN]);
	  while (TRUE) {
	       static int eof = FALSE;
	       int r = read(in,buf,sizeof(buf));
	       if (r==0) {
		    if (eof) {
			 int status;
			 wait(&status);
			 exit(WIFEXITED(status) ?
			      WEXITSTATUS(status) :
			      WTERMSIG(status)
			      );
			 }
		    fprintf(stderr,"child terminated before input exhausted\n");
		    wait(NULL);
		    exit(1);
		    }
	       else if (r==ERROR) {
		    perror("failed to read child");
		    wait(NULL);
		    exit(1);
		    }
	       else if (buf[r-1] == 4) {
	       	    write(STDOUT,buf,r-1);
		    if (eof) {
			 fprintf(stderr,"prompt received but input exhausted\n");
			 wait(NULL);
			 exit(2);
			 }
		    if (NULL == fgets(buf,sizeof(buf),stdin) || buf[0]==0) {
			 eof = TRUE;
			 close(out);
			 }
		    else {
			 write(STDOUT,buf,strlen(buf));
			 write(out,buf,strlen(buf));
			 }
		    }
	       else {
	       	    write(STDOUT,buf,r);
		    }
	       }
	  }
     }

     
