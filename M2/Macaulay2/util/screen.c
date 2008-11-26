/* screen.c */

/* Copyright 1997, Daniel R. Grayson */

#define usetermio
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <fcntl.h>
#ifdef usetermio
#include <termio.h>
#else
/* #include <sgtty.h> */
#include <bsd/sgtty.h>
#endif
#include <netdb.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>		/* needed for sys/socket.h */
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/time.h>		/* needed for sys/resource.h */
#include <sys/resource.h>	/* needed for third arg to wait3 */
#include <sys/file.h>
#include <sys/socket.h>		/* needed for args to socket(), bind() */
#include <sys/param.h>		/* for NOFILE */
#include <netinet/in.h>
#include <sys/ioctl.h>		/* for ioctl() */
#include <arpa/inet.h>		/* needed for inet_addr() */

typedef struct {
#    ifdef usetermio
     struct termios termios;
     struct winsize winsize;
#    else
     struct sgttyb sgttyb;
     struct tchars tchars;
     int cl;
     struct ltchars ltchars;
#    endif
     } terminal;

#define ckioctl(a,b,c) (check(ioctl(a,b,(char *)c),"ioctl",__LINE__))
#define MTU 1500		
     /* maximum transmission unit for your ethernet, as revealed by netstat -in */
#define BUFFER_SIZE MTU		
     /* must be less than the MTU */
#define ERROR  (-1)
#define CLOSED (-1)
#define TRUE   1
#define FALSE  0
#define NODATA 0
#define STDIN  0
#define STDOUT 1
#define STDERR 2
#ifdef MSDOS
#define endl "\n\r"
#else
#define endl "\n"
#endif
int argc;
char **argv;
void error(char *);
static struct select {
     fd_set readfds, writefds, exceptfds;
     } Selection, SelectionResult;
static void (*Handler[NOFILE])(	/* fd */ );   /* for EventManager() */
static int ChildPID;
static int stdin_tty_parmsaved = FALSE;
static terminal stdin_tty_parmsave;
static int stdout_tty_parmsaved = FALSE;
static terminal stdout_tty_parmsave;
static int master_stdin_tty_parmsaved = FALSE;
static terminal master_ttyparm;
static char *progname;
static char *slavename;
static int masterfd = CLOSED;
static char *termmsg;		/* msg to display at end */

void
warning(char *s)
{
     if (errno != 0) {
     	  char t[100];
     	  sprintf(t,"%s:%s ",__FILE__,s);
	  perror(t);
	  fprintf(stderr,endl);
	  }
     else {
     	  fprintf(stderr,"%s:%s%s",__FILE__,s,endl);
	  }
     }

int
check(int val,char *msg,int line)
{
     if (val==ERROR) {
     	  char buf[200];
     	  sprintf(buf,"%d: %s",line,msg);
	  error(buf);
	  }
     return val;
     }

void
getttystate(int fd,terminal *t)
{
#ifdef usetermio
     ckioctl(fd,TCGETS,&t->termios);
     ckioctl(fd,TIOCGWINSZ,&t->winsize);
#else
     ckioctl(fd,TIOCGETP,&t->sgttyb);
     ckioctl(fd,TIOCGETC,&t->tchars);
     ckioctl(fd,TIOCLGET,&t->cl);
     ckioctl(fd,TIOCGLTC,&t->ltchars);
#endif
     }

void
setttystate(int fd,terminal *t)
{
#ifdef usetermio
     ckioctl(fd,TCSETSW,&t->termios);
     ckioctl(fd,TIOCSWINSZ,&t->winsize);
#else
     ckioctl(fd,TIOCSETN,&t->sgttyb);
     ckioctl(fd,TIOCSETC,&t->tchars);
     ckioctl(fd,TIOCLSET,&t->cl);
     ckioctl(fd,TIOCSLTC,&t->ltchars);
#endif
     }

void
terminate(int s){
     static int firsttime = TRUE;
     if (firsttime) {
	  firsttime = FALSE;
	  if (isatty(STDIN)) {
	       setttystate(STDIN,&stdin_tty_parmsave);
	       }
	  if (termmsg != NULL) {
	       puts("");
	       puts(termmsg);
	       }
	  }
     exit(s);
     }

void error(char *s)
{
     warning(s);
     terminate(1);
     }

void stop(int sig){
     terminate(0);
     }

void
getpty(int *master,char **SlaveName)
{
     static char name[24];
     struct stat stb;
     int c, i, p;
     for (c = 'p'; c <= 'z' ; c++) {
	  for (i=0; i<16; i++) {
	       sprintf(name,"/dev/pty%c%x",c,i);
	       if (stat(name,&stb) < 0) goto done;
     	       *master = open (name, O_RDWR | O_NDELAY | O_NOCTTY, 0);
	       /* on some systems O_NONBLOCK is used instead of O_NDELAY */
	       if (*master == ERROR) continue;
	       sprintf(name, "/dev/tty%c%x",c,i);
	       if (access (name, R_OK | W_OK) != 0) {
		    close(*master);
		    continue;
		    }
	       *SlaveName = name;
	       p=1;
#ifdef FIONBIO
	       ckioctl(*master,FIONBIO,&p);
#endif
     	       return;
	       }
	  }
     done:
     error("can't open pseudo terminal");
     }

void PassSignalToChild(int sig)
{
     struct winsize winsize;
     ckioctl(STDOUT,TIOCGWINSZ,&winsize);
     printf("%d x %d\n",winsize.ws_row,winsize.ws_col);
     ckioctl(masterfd,TIOCSWINSZ,&winsize);
     if (ChildPID != 0) kill(-ChildPID,sig);
     }

void runchild()
{
     int slave;
     {
	  int j;
	  /* disassociate our process from the controlling tty, if present */
	  /* not needed if we do setsid() below */
     	  j = open("/dev/tty", O_RDWR, 0);
     	  if (j != ERROR) {
     	       ckioctl(j, TIOCNOTTY, 0);
     	       close (j);
	       }
	  }
     if (ERROR == (slave = open(slavename,O_RDWR,0))) {
	  error("can't open slave tty");
	  }
     if (ERROR == setsid()) error("setsid call failed");
#ifdef TIOCSCTTY
     if (ERROR == ioctl(slave,TIOCSCTTY,0)) {
	  warning("TIOCSCTTY ioctl fails");
	  }
#endif
#if 0
     if (ERROR == setpgrp( /* 0,0 */ )) error("setpgrp call failed");
     /* setpgrp seems to interact badly with setsid, and causes the process to
        die later */
#endif
     {
	  /* tell the tty driver to which process group to send the signals */
	  int pid = getpid();	  
     	  if (ERROR==ioctl(slave, TIOCSPGRP, (char *)&pid)) {
	       warning("TIOCSPGRP ioctl fails");
	       }
	  }
     if (stdin_tty_parmsaved) {
	  setttystate(slave,&stdin_tty_parmsave);
	  }
     else if (stdout_tty_parmsaved) {
	  setttystate(slave,&stdout_tty_parmsave);
	  }
     close(STDIN);
     close(STDOUT);
     close(STDERR);
     if (ERROR == dup2(slave,STDIN)) exit(81);
     dup2(slave,STDOUT);
     dup2(slave,STDERR);
     close(slave);
     execvp(argv[1],argv+1);
     {
	  char buf[100];
	  sprintf(buf,"%s: Couldn't exec the program '%s'%s", 
	       progname, argv[1], endl);
     	  int r = write(STDERR, buf, strlen(buf));
     	  _exit(r ? 1 : 1);
	  }
     }

void
HandleSIGCHLD(int sig)
{
     int pid, status;
     while (0 != (pid = waitpid(-1,&status,WNOHANG))) {
     	  if (pid == ERROR) {
	       if (errno == ECHILD) break;
	       error("wait");
	       }
     	  if (pid == ChildPID) {
	       if (status != 0) {
		    int code = (status >> 8) & 0xff;
		    int sig0 = status & 0xff;
		    unsigned int n;
     		    char buf[BUFFER_SIZE];
		    while (0 < (n = read(masterfd,buf,sizeof(buf)))) {
			 int r = write(STDOUT,buf,n);
			 if (r == ERROR) {
			   error("failed to write output");
			 }
			 }
		    if (code == 0) {
			 sprintf(buf,"process exited: signal %d", sig0);
			 }
		    else if (sig0 == 0) {
			 sprintf(buf,"process exited: return code %d", code);
			 }
		    else sprintf(buf,"process exited: return code %d, signal %d", code, sig0);
	       	    errno = 0;
	       	    error(buf);
		    }
	       else terminate(0);
	       }
	  }
     }

void heed(int fd,void (*fun)())
{
     FD_SET(fd,&Selection.readfds);
     FD_SET(fd,&Selection.exceptfds);
     Handler[fd] = fun;
     }

void unheed(int fd)
{
     if (fd == CLOSED || Handler[fd] == NULL) error("unheed");
     FD_CLR(fd,&Selection.readfds);
     FD_CLR(fd,&Selection.writefds);
     FD_CLR(fd,&Selection.exceptfds);
     FD_CLR(fd,&SelectionResult.readfds);
     FD_CLR(fd,&SelectionResult.writefds);
     FD_CLR(fd,&SelectionResult.exceptfds);
     Handler[fd] = NULL;
     }

void ignore(int fd)
{
     if (fd == CLOSED || Handler[fd] == NULL) error("ignore");
     FD_CLR(fd,&Selection.readfds);
     FD_CLR(fd,&Selection.writefds);
     FD_CLR(fd,&Selection.exceptfds);
     FD_CLR(fd,&SelectionResult.readfds);
     FD_CLR(fd,&SelectionResult.writefds);
     FD_CLR(fd,&SelectionResult.exceptfds);
     }

void reheed(int fd)
{
     assert(Handler[fd] != NULL);
     FD_SET(fd,&Selection.readfds);
     FD_SET(fd,&Selection.exceptfds);
     }     

void examine(fd_set *fds)
{
     /* it is expected that a file descriptor might raise both an exception
        and read-ability in a select call, and that handling the one instance
	might result in closing the file, thus making handling the other
	instance superfluous, or even damaging.  Thus, in this routine, we
	keep referring to the original words in fds, allowing actions taken
	in one handler to prevent us from calling another handler.
	In practice, when this routine is called, it is called with fds
	pointing to part of SelectionResult, which is a global variable;
	this way, the handler routines can affect what we are examining in
	this routine. */
     register int i, n;
     register void (*fun)();
     for (i=0; i<FD_SETSIZE; i++) {
	  if (FD_ISSET(i,fds)) {
	       n = i;
	       fun = Handler[n];
	       if (fun == NULL) {
		    char buf[100];
		    sprintf(buf,"no handler installed for file %d",n);
		    errno=0;
		    warning(buf);
		    }
	       else {
	       	    (*fun)(n);
		    }
	       FD_CLR(n,fds);
	       }
	  }
     }

void
EventManager()
{
     while (1) {
     	  static struct timeval *timeout=NULL;
     	  int n, i;
     	  SelectionResult = Selection;
     	  n = select(NOFILE, 
	       &SelectionResult.readfds, 
	       &SelectionResult.writefds, 
	       &SelectionResult.exceptfds, 
	       timeout);
     	  if (n==ERROR) {
	       if (errno == EINTR) continue;
	       error("select");
	       }
	  for (i=0; i<FD_SETSIZE; i++) {
	       if (FD_ISSET(i,&SelectionResult.exceptfds)) {
		    FD_CLR(i,&SelectionResult.readfds);
		    }
	       }
	  examine(&SelectionResult.exceptfds);
	  examine(&SelectionResult.readfds);
	  examine(&SelectionResult.writefds);
	  }
     }

void
Writemasterfd(char *buf,unsigned int n)
{
     /* even if n==0 we write, so ^D from typist gets passed through */
     n = write(masterfd,buf,n);
     if (n == ERROR) error("write");
     }

void
hFromKeyboard(int fd){
  int n;
  static char rdbuf[256];
  n = read(fd,rdbuf,sizeof rdbuf);
  if (n == ERROR) error("read from stdin");
  if (n == 0) {
    rdbuf[n++] = master_ttyparm.termios.c_cc[VEOF];
    unheed(fd);
  }
  Writemasterfd(rdbuf,(unsigned)n);
}

void
hmasterfd(int fd){
     char buf[BUFFER_SIZE];
     register int n;
     assert(fd == masterfd);
     n = read(masterfd,buf,sizeof(buf));
     if (n == NODATA) terminate(0);
     if (n == ERROR) terminate(0); /* the shell has terminated */
     check(write(STDOUT,buf,(unsigned)n),"write stdout",__LINE__);
     }

void
setraw(int fd){
     terminal t;
     getttystate(fd,&t);
#    ifdef usetermio
     t.termios.c_lflag &= ~(ICANON | ISIG);
     t.termios.c_cc[VMIN] = 1;
     t.termios.c_cc[VTIME] = 0;
     t.termios.c_iflag &= ~(INLCR |ICRNL |PARMRK |INPCK |IGNCR |IUCLC |IXON |IXOFF);
     t.termios.c_iflag |= IGNPAR;
#    else
     t.sgttyb.sg_flags |= RAW;
#    endif
     setttystate(fd,&t);
     }

void
clrecho(int fd){
     terminal t;
     getttystate(fd,&t);
#    ifdef usetermio
     t.termios.c_lflag &= ~ECHO;
#    else
     t.sgttyb.sg_flags &= ~ECHO;
#    endif
     setttystate(fd,&t);
     }

void runparent()
{
     heed(STDIN,hFromKeyboard);
     heed(masterfd,hmasterfd);
     signal(SIGPIPE,SIG_IGN);
     signal(SIGWINCH,PassSignalToChild);
     signal(SIGINT,stop);
     signal(SIGTERM,stop);
     signal(SIGQUIT,stop);
     if (isatty(STDIN)) {
     	  setraw(STDIN);
     	  clrecho(STDIN);
	  }
     EventManager();
     }

void show()
{
     /* if (!isatty(STDIN) || !isatty(STDOUT)) error("not a tty"); */
     getpty(&masterfd,&slavename);
     if (stdin_tty_parmsaved) {
	  setttystate(masterfd,&stdin_tty_parmsave);
	  }
     else if (stdout_tty_parmsaved) {
	  setttystate(masterfd,&stdout_tty_parmsave);
	  }
     getttystate(masterfd,&master_ttyparm), master_stdin_tty_parmsaved = TRUE;
     signal(SIGCHLD,HandleSIGCHLD);
     ChildPID = fork();
     if (ChildPID == 0) {
	  static char buf[100];
	  strncpy(buf,progname,sizeof(buf)-8);
	  strcat(buf,"[child]");
	  progname = buf;
	  runchild();
	  }
     else {
	  runparent();
	  }
     }

char *
BaseName(char *t)
{
     char *s;
     for (s=t; *t; t++) if (*t == '/') s = t+1;
     return s;
     }     

void
usage(){
     (void)fprintf(stderr,"usage : %s cmd [arg1 arg2 ...]\n",progname);
     terminate(0);
     }

int main(int ARGC, char **ARGV)
{
     argc = ARGC;
     argv = ARGV;
     progname = BaseName(argv[0]);
     if (argc < 2) usage();
     if (isatty(STDIN)) {
	  stdin_tty_parmsaved = TRUE;
	  getttystate(STDIN,&stdin_tty_parmsave);
	  }
     if (isatty(STDOUT)) {
	  stdout_tty_parmsaved = TRUE;
	  getttystate(STDOUT,&stdout_tty_parmsave);
	  }
     show();
     return 0;
     }
