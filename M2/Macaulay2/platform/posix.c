#include <tokens-exports.h>
#include "platform.h"
#include <M2mem.h>

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

/**
   This works on both OSX, Linux, and well supported POSIX tool chains.
   We can't just look at clock in general because that misbehaves with threads.
   Return the number of seconds elapsed of cputime.
   @return number of seconds or 0 on error.
**/
double system_cpuTime(void)
{
  struct rusage usage;
  int err = getrusage(RUSAGE_SELF, &usage);
  if(err)
    return 0;
  double u = usage.ru_utime.tv_sec + usage.ru_utime.tv_usec*1e-6;
  return u;
}
/**
    Initialize the system cpuTime.
    This should always be called on program start.
    All systems should implement this.
**/
extern void system_cpuTime_init(void)
{
}

/**
   Note posix spec guarentees existance of environ in unistd.
   We special case out any broken builds rather then making a massive ifdef tree.
**/
extern char **environ;

/**
   Returns the program global environment array.
   @return The program global enrionment array or NULL if not applicable on the platform.
**/
char** getEnviron()
{
  return environ;
}

/**
    Execute the given program in the current thread.
    All systems should implement this.
    @param argv Argument string with program as first argument.
    @return 0 on success, other on failure.
**/
int system_exec(M2_ArrayString argv)
{
  int i;
  char **av = M2_tocharstarstar(argv);
  execvp(av[0],av);
  for (i=0; i<(int)argv->len; i++) {
    GC_FREE(av[i]);
  }
  GC_FREE(av);
  return -1;
}

/**
    Wait on processes.
    This is for POSIX systems only.
    This should really be depracated.  Its not very compatible with threading.  
    @param pid M2 array of PIDs.
    @return Array of return codes from wait.
**/
M2_arrayint system_waitNoHang(M2_arrayint pids)
{
     int n = pids->len;
     int *pid = pids->array;
     {
	  int status[n], i;
	  M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
	  z->len = n;
	  for (i=0; i<n; i++) {
	       int ret = wait4(pid[i],&status[i],WNOHANG,NULL);
	       z->array[i] = ret == -1 ? -1 : WIFEXITED(status[i]) ? status[i] >> 8 : -2;
	  }
	  return z;
     }
}
/////////////////////////////////////////////////////////////////////////////////////////////////
// File IO
////////////////////////////////////////////////////////////////////////////////////////////////

/**
    Return if the file exists.
    All systems should implement this.
    @param name path of the file.
    @return True if the file exists, false otherwise.
 **/
M2_bool system_fileExists(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r = stat(cname,&buf);
  GC_FREE(cname);
  return r != -1;
}
/**
   Posix system only
**/
int system_fileMode(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r = stat(cname,&buf);
  GC_FREE(cname);
  return r == -1 ? -1 : buf.st_mode & ~S_IFMT;
}
/**
   Posix system only
**/
int system_fileModeFD(int fd) {
  struct stat buf;
  int r = fstat(fd,&buf);
  return r == -1 ? -1 : buf.st_mode & ~S_IFMT;
}
/**
    chmod the given file to a new mode.
    This is for POSIX systems only.
    @param name Path to file.
    @param mode New mode for the file.  See POSIX specs for details. 
    @return 0 on success, nonzero on error.
**/
int system_chmod(M2_string name,int mode) {
  char *cname = M2_tocharstar(name);
  int r = chmod(cname,mode);
  GC_FREE(cname);
  return r;
}
/**
   @param cname Path to directory.
   @return 1 if path is a directory, 0 otherwise or if the path does not exist.
**/
int platformIsDirectory(const char *cname) {
  struct stat buf;
  int r = lstat(cname,&buf);
  if(r != -1 && S_ISDIR(buf.st_mode))
    return 1;
  return 0;
}
/**
   Returns if the path is a directory.
   All systems should implement this.
   @param name Path of the directory.
   @return True if it is a directory, false otherwise or if path does not exist.
**/
int system_isDirectory(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = platformIsDirectory(cname);
  GC_FREE(cname);
  return r;
}
/**
    Change the directory of the current process.
    Note that this may be process or thread specific depending on the platform.
    On POSIX it is process specific and thus not thread safe.
    @param name Path of the new working directory.
    @return 0 on success, nonzero for failure.
**/
int system_chdir(M2_string filename) {
  char *fn = M2_tocharstar(filename);
  int ret = chdir(fn);
  GC_FREE(fn);
  return ret;
}
/**
    Return True if the file is a regular file.
    This is for POSIX systems only.
    @param name Path to file.
    @return True if the file is a regular file, false if it is not or the path does not exist.
**/
int system_isRegularFile(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r = lstat(cname,&buf);
  GC_FREE(cname);
  return r == -1 ? -1 : S_ISREG(buf.st_mode);
}
/**
    Return the contents of the directory as an array of strings.
    All systems should implement this.
    @param name Path of directory whose contents should be listed.
    @return An array of strings, NULL on error.
**/
M2_ArrayString system_readDirectory(M2_string name) {
  int n=0, i=0;
  M2_ArrayString a;
  char *cname = M2_tocharstar(name);
  struct dirent *entry;
  DIR *dir = opendir(cname);
  GC_FREE(cname);
  if (dir == NULL) return NULL;
  errno = 0;
  for (n=0; readdir(dir) != NULL; n++) ;
  if (errno != 0) {
    closedir(dir);
    return NULL;
  }
  rewinddir(dir);
  a = (M2_ArrayString) getmem (sizeofarray(a,n));
  a->len = n;
  for (i=0; i<n && (entry = readdir(dir)) != NULL; i++) a->array[i] = M2_tostring(entry->d_name);
  for (   ; i<n ; i++) a->array[i] = M2_tostring("");
  closedir(dir);
  if (errno != 0) return NULL;
  return a;
}
/**
    Returns the length of the file.
    All systems should implement this.
    @param fd File descriptor of file.
    @return Length of the file or -1(-1).
 **/
int system_fileLength(int fd) {
  struct stat statbuf;
  if (-1 == fstat(fd,&statbuf)) return -1;
  return statbuf.st_size;
}
/**
    Returns the length of the file.
    All systems should implement this.
    @param filename Path to file.
    @return Length of the file or -1(-1).
**/
int system_fileLength_1(M2_string filename) {
  char *cname = M2_tocharstar(filename);
  struct stat statbuf;
  int ret = stat(cname,&statbuf);
  GC_FREE(cname);
  if (-1 == ret) return -1;
  return statbuf.st_size;
}
/**
   Posix system only -- There is not a single notion of file time across all operating systems.
**/
int system_fileTime(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r;
  r = lstat(cname,&buf);
  GC_FREE(cname);
  if (r == -1) return -1;
  return buf.st_mtime;
}
/**
   Posix system only -- There is not a single notion of file time across all operating systems.
**/
int system_setFileTime(M2_string name, int modtime) {
  char *cname = M2_tocharstar(name);
  struct utimbuf buf = { time(NULL), modtime };
  int r;
  r = utime(cname,&buf);
  GC_FREE(cname);
  if (r == -1) return -1;
  return 0;
}
/**
    Make directory.
    All systems should implement this.
    @param name Path of the directory.
    @return 0 on success, nonzero for failure.
**/
int system_mkdir(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = mkdir(cname,0777);
  GC_FREE(cname);
  return r;
}
/**
    Remove the directory.
    All systems should implement this.
    @param name Path of the directory
    @return 0 on success, nonzero for failure.
**/
int system_rmdir(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = rmdir(cname);
  GC_FREE(cname);
  return r;
}
/**
    Unlink the given file.
    This is for POSIX systems only.
    @param name Path of the file to be unlinked.
    @return 0 on success, nonzero on error.
 **/
int system_unlink(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = unlink(cname);
  GC_FREE(cname);
  return r;
}
/**
    Link the old file name to the new file name.
    This is for POSIX systems only.
    @param oldfilename File to be linked to.
    @param newfilename Path of link to be created.
    @return 0 on success, nonzero on error.
**/
int system_link(M2_string oldfilename,M2_string newfilename) {
  char *old = M2_tocharstar(oldfilename);
  char *new = M2_tocharstar(newfilename);
  int r = link(old,new);
  GC_FREE(old);
  GC_FREE(new);
  return r;
}
/**
    Symlink the old file name to the new file name.
    This is for POSIX systems only.
    @param oldfilename File to be linked to.
    @param newfilename Path of link to be created.
    @return 0 on success, nonzero on error.
 **/
int system_symlink(M2_string oldfilename,M2_string newfilename) {
  char *old = M2_tocharstar(oldfilename);
  char *new = M2_tocharstar(newfilename);
  int r = symlink(old,new);
  GC_FREE(old);
  GC_FREE(new);
  return r;
}

M2_string system_readlink(M2_string filename) {
  char *fn = M2_tocharstar(filename);
  int size = 100;
  M2_string s = NULL;
  while (TRUE) {
    char buf[size];
    int r = readlink(fn,buf,sizeof buf);
    if (r == -1) {
      s = M2_tostring("");
      break;
    }
    if (r < size) {
      s = M2_tostringn(buf,r);
      break;
    }
    size *= 2;			/* r == size, try again */
  }
  GC_FREE(fn);
  return s;
}
/***
   Open the given file in binary/read only mode.
   @return The file descriptor, -1 on failure.
***/
int system_openin(M2_string filename) {
     char *fname = M2_tocharstar(filename);
     int fd;
     fd = open(fname, O_BINARY | O_RDONLY);
     GC_FREE(fname);
     return fd;
     }
/***
   Open the given file in binary/read/write/create/truncate mode.  
   @return The file descriptor, -1 on failure.
***/
int system_openout(M2_string filename) {
     char *fname = M2_tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_TRUNC, 0644);
     GC_FREE(fname);
     return fd;
     }
/***
    Open the given file in binary/read/write/create/append mode.
    @return The file descriptor, -1 on failure.
***/
int system_openoutappend(M2_string filename) {
     char *fname = M2_tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_APPEND, 0644);
     GC_FREE(fname);
     return fd;
     }

/////////////////////////////////////////////////////////////////////////////////////////////////
// Sockets
// Note - Current socket code is *highly* thread unsafe.
////////////////////////////////////////////////////////////////////////////////////////////////

static const char *hostname_error_message;

static int set_addrinfo(struct addrinfo **addr, struct addrinfo *hints, char *hostname, char *service) {
     int ret;
     ret = getaddrinfo(hostname, service, hints /* thanks to Dan Roozemond for pointing out this was NULL before, causing problems */, addr);
     hostname_error_message = ret != 0 ? gai_strerror(ret) : NULL;
     return ret;
}

const char *hstrerror(int herrno) {
     switch(herrno) {
     case HOST_NOT_FOUND: return "authoritive answer: host not found";
     case TRY_AGAIN: return "non-authoritive answer: host not found; or server failed";
     case NO_RECOVERY: return "nonrecoverable errors";
     case NO_DATA: return "valid name, no data record of requested type";
     case -1: return "malformed address or internal error";
     default: return "unknown error";
     }
}
int host_address(char* name)
{
     if ('0' <= name[0] && name[0] <= '9') {
     	  int s;
	  /* this function is obsolete, replaced by inet_aton(); we use it only if getaddrinfo is not available */
	  s = inet_addr(name);
	  if (s == -1) {
	       hostname_error_message = "IP address translation failed";
	       return -1;
	  }
	  return s;
	  }
     else {
	  struct hostent *t;
	  if (sigsetjmp(interrupt_jump,TRUE)) {
	       interrupt_jump_set = FALSE;
	       return -1;
	  }
	  else interrupt_jump_set = TRUE;
	  /* this function is obsolete because it doesn't handle IPv6; we use it only if getaddrinfo is not available */
	  t = gethostbyname(name); 
	  interrupt_jump_set = FALSE;
	  if (t == NULL) {
	       hostname_error_message = hstrerror(h_errno);
	       return -1;
	  }
	  else {
	       return *(int *)t->h_addr;
	  }
     }
}

int serv_address(name)
char *name;
{
     if ('0' <= name[0] && name[0] <= '9') {
	  return htons(atoi(name));
	  }
     else {
	  struct servent *t = getservbyname(name,"tcp");
	  if (t == NULL) {
	    errno = ENXIO;
	    return -1;
	  }
	  else {
	    return t->s_port;
	  }
     }
}
int system_acceptBlocking(int so) {
  struct sockaddr_in addr;
  socklen_t addrlen = sizeof addr;
  fcntl(so,F_SETFL,0);
  return accept(so,(struct sockaddr*)&addr,&addrlen);
}

int system_acceptNonblocking(int so) {
  struct sockaddr_in addr;
  socklen_t addrlen = sizeof addr;
  int sd;
  fcntl(so,F_SETFL,O_NONBLOCK);
  sd = accept(so,(struct sockaddr*)&addr,&addrlen);
  return sd;
}

const int INCOMING_QUEUE_LEN = 10;

int openlistener(char *interface, char *service) {
  struct addrinfo *addr = NULL;
  static struct addrinfo hints;	/* static so all parts get initialized to zero */
  int so;
  hints.ai_family = PF_UNSPEC;
  hints.ai_flags = AI_PASSIVE;
  if (0 != set_addrinfo(&addr,&hints,interface,service)) return -1;
  so = socket(addr->ai_family,SOCK_STREAM,0);
  if (-1 == so) { freeaddrinfo(addr); return -1; }
  if (-1 == bind(so,addr->ai_addr,addr->ai_addrlen) || -1 == listen(so, INCOMING_QUEUE_LEN)) { freeaddrinfo(addr); close(so); return -1; }
  freeaddrinfo(addr); 
  return so;
}

int opensocket(char *host, char *service) {
  struct addrinfo *addr;
  int so;
  if (sigsetjmp(interrupt_jump,TRUE)) {
       interrupt_jump_set = FALSE;
       return -1;
  }
  else interrupt_jump_set = TRUE;
  if (0 != set_addrinfo(&addr,NULL,host,service)) return -1;
  so = socket(addr->ai_family,SOCK_STREAM,0);
  if (-1 == so) { freeaddrinfo(addr); return -1; }
  if (-1 == connect(so,addr->ai_addr,addr->ai_addrlen)) { freeaddrinfo(addr); close(so); return -1; }
  interrupt_jump_set = FALSE;
  freeaddrinfo(addr);
  return so;
}

int system_opensocket(M2_string host,M2_string serv) {
     char *tmphost = M2_tocharstar(host);
     char *tmpserv = M2_tocharstar(serv);
     int sd = opensocket(tmphost,tmpserv);
     GC_FREE(tmphost);
     GC_FREE(tmpserv);
     return sd;
     }

int system_openlistener(M2_string interface,M2_string serv) {
     char *tmpinterface = M2_tocharstar(interface);
     char *tmpserv = M2_tocharstar(serv);
     int sd = openlistener(*tmpinterface ? tmpinterface : NULL,tmpserv);
     GC_FREE(tmpinterface);
     GC_FREE(tmpserv);
     return sd;
     }

M2_arrayint system_select(M2_arrayint v) {
  static fd_set r, w, e;
  int n = v->len;
  int *s = v->array;
  int i, j, max = 0, m;
  M2_arrayint z;
  if (n == 0) return v;
  for (i=0; i<n; i++) if (s[i] > max) max = s[i];
  for (i=0; i<n; i++) FD_SET(s[i], &r);
  m = select(max+1,&r,&w,&e,NULL);
  if (m == -1) {
       z = (M2_arrayint)getmem_atomic(sizeofarray(z,0));
       z->len = 0; 
  }
  else {
       z = (M2_arrayint)getmem_atomic(sizeofarray(z,m));
       z->len = m;
       for (i=j=0; i<n && j<m; i++) if (FD_ISSET(s[i],&r)) { z->array[j++] = i; FD_CLR(s[i],&r); }
  }
  return z;
}
