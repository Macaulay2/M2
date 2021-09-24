/*		Copyright 1994 by Daniel R. Grayson		*/

#include <dirent.h>

#include "expr-exports.h"
#include "types.h"
#include "M2mem.h"
#include "../c/compat.c"
#include "debug.h"

#include "../system/supervisorinterface.h"

extern void M2_stack_trace();

void
fatal(const char *s,...)   {
     va_list ap;
     va_start(ap,s);
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
#ifndef NDEBUG
     trap();
#endif
     M2_stack_trace();
     exit(1);
     }

void fatalarrayindex(int indx, int len, const char *file, int line, int column) {
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

void fatalarraylen(int len, const char *file, int line, int column)
{
     char msg[100];
     sprintf(msg,"new array length %d less than zero",len);
     if (column == -1) {
         fatal(errfmtnc,file,line,msg);
         }
     else {
         fatal(errfmt,file,line,column,msg);
         }
     }

void invalidTypeTag(int typecode, const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"internal error: unrecognized type code: %d\n",typecode);
     if (column == -1) {
         fatal(errfmtnc,file,line,msg);
         }
     else {
         fatal(errfmt,file,line,column,msg);
         }
     }

void invalidNullPointer(const char *file, int line, int column) {
     char msg[100];
     sprintf(msg,"internal error: invalid null pointer\n");
     if (column == -1) {
         fatal(errfmtnc,file,line,msg);
         }
     else {
         fatal(errfmt,file,line,column,msg);
         }
     }

int system_openin(M2_string filename) {
     char *fname = M2_tocharstar(filename);
     int fd;
     fd = open(fname, O_BINARY | O_RDONLY);
     freemem(fname);
     return fd;
     }

int system_openout(M2_string filename) {
     char *fname = M2_tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_TRUNC, 0644);
     freemem(fname);
     return fd;
     }

int system_openoutappend(M2_string filename) {
     char *fname = M2_tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_APPEND, 0644);
     freemem(fname);
     return fd;
     }

int system_exec(M2_ArrayString argv) {
     int i;
     char **av = M2_tocharstarstar(argv);
     execvp(av[0],av);
     for (i=0; i<(int)argv->len; i++) {
     	  freemem(av[i]);
	  }
     freemem(av);
     return ERROR;
     }

M2_string interp_dirname(M2_string s) {
  char *t = M2_tocharstar(s);
  char *u = t;
  char *v = u;
  for (; *u; u++) if (*u == '/') v=u+1;	/* on MacOS?? */
  if (v != NULL) *v = '\0';
  if (*t == '\0') return M2_tostring("./"); /* on MacOS?? */
  return M2_tostring(t);
}

M2_string system_getcwd()
{
     /* this function now adds a terminal / to the directory name */
     char buf[700];
     static const char slash[] = "/";
     char *x = getcwd(buf,sizeof(buf)-strlen(slash));
     if (0 != strcmp(buf,slash)) strcat(buf,slash);
     if (x != NULL) return M2_tostring(x);
     return M2_tostring("");
     }

M2_string system_getenv(M2_string s) {
     char *ss = M2_tocharstar(s);
     char *x = getenv(ss);
     freemem(ss);
     if (x == NULL) return M2_tostring("");
     else return M2_tostring(x);
     }

int system_strcmp(M2_string s, M2_string t) {
  int slen = s->len, tlen = t->len, i;
  int ret = 0;
  int len = slen < tlen ? slen : tlen;
  unsigned char *sarray = (unsigned char *)s->array;
  unsigned char *tarray = (unsigned char *)t->array;
  for (i=0; i<len; i++) {
    unsigned char c = sarray[i];
    unsigned char d = tarray[i];
    if (isalnum(c)) {
      if (isalnum(d)) {
	if (toupper(c) < toupper(d)) return -1;
	if (toupper(c) > toupper(d)) return 1;
	if (ret == 0) {
	  if (c < d) ret = -1;
	  if (c > d) ret = 1;
	}
      }
      else return 1;
    }
    else {
      if (isalnum(d)) return -1;
      else {
	if (c < d) return -1;
	if (c > d) return 1;
      }
    }
  }
  if (slen > tlen) return 1;
  if (slen < tlen) return -1;
  return ret;
}

int system_strnumcmp(M2_string s,M2_string t) {
     int slen = s->len, tlen = t->len, i;
     int ret = 0;
     int len = slen < tlen ? slen : tlen;
     int innumber = FALSE;
     unsigned char *sarray = (unsigned char *)s->array;
     unsigned char *tarray = (unsigned char *)t->array;
     for (i=0; i<len; i++) {
	  unsigned char c = sarray[i];
	  unsigned char d = tarray[i];

	  if (isdigit(c) && isdigit(d)) {
		 if (!innumber) {
		      int sn, tn;
		      sn=i+1; while(sn<slen && isdigit((int)sarray[sn])) sn++;
		      tn=i+1; while(tn<tlen && isdigit((int)tarray[tn])) tn++;
		      if (sn > tn) return  1;
		      if (sn < tn) return -1;
		      innumber = TRUE;
		 }
	  }
	  else innumber = FALSE;

	  if (isalnum(c)) {
	       if (isalnum(d)) {
		    unsigned char C = toupper(c), D = toupper(d);
		    innumber = FALSE;
		    if (C < D) return -1;
		    if (C > D) return 1;
		    if (ret == 0) {
			 if (c < d) ret = -1;
			 if (c > d) ret = 1;
		    }
	       }
	       else return 1;
	  }
	  else {
	       if (isalnum(d)) return -1;
	       else {
		    innumber = FALSE;
		    if (c < d) return -1;
		    if (c > d) return 1;
	       }
	  }
     }
     if (slen > tlen) return 1;
     if (slen < tlen) return -1;
     return ret;
}

int fix_status(int status) {
     /* We can't handle status codes bigger than 127 if the shell intervenes. */
     return
       status == ERROR ? ERROR :
       WIFSIGNALED(status) ?					  /* whether the process died due to a signal */
       WTERMSIG(status) + (WCOREDUMP(status) ? 128 : 0) :	  /* signal number n, plus 128 if core was dumped */
       WIFEXITED(status) ?					  /* whether the process exited */
       (
	    ((WEXITSTATUS(status) & 0x80) != 0) ?                 /* whether /bin/sh indicates a signal in a command */
	    (WEXITSTATUS(status) & 0x7f) :			  /* the signal number */
	    (WEXITSTATUS(status) << 8)				  /* status code times 256 */
	    ) :
       -2;						  	  /* still running (or stopped) */
     }

M2_arrayint system_waitNoHang(M2_arrayint pids)
{
     int n = pids->len;
     int *pid = pids->array;
     M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
     z->len = n;
     for (int i=0; i<n; i++) {
	  #ifdef HAVE_WAITPID
	      int status = 0, ret = waitpid(pid[i],&status,WNOHANG);
	      z->array[i] =
		ret == ERROR ? -1 : fix_status(status);
	  #else
	      z->array[i] = -1;                                            /* not implemented */
	  #endif
     }
     return z;
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

int system_hash(double x){
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
     unsigned int i;
     for (i=0; i<sizeof(x); i++) {
	  h = 231*h + p[i];
	  }
#endif
     return h;
     }

M2_string system_readlink(M2_string filename) {
  char *fn = M2_tocharstar(filename);
  int size = 100;
  M2_string s = NULL;
  while (TRUE) {
    char buf[size];
    int r = 
      #ifdef HAVE_READLINK
      readlink(fn,buf,sizeof buf)
      #else
      -1
      #endif
      ;
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
  freemem(fn);
  return s;
}

int system_chdir(M2_string filename) {
  char *fn = M2_tocharstar(filename);
  int ret = chdir(fn);
  freemem(fn);
  return ret;
}

static bool isDirectory(const char *cname) {
  struct stat buf;
  int r = 
    #ifdef HAVE_LSTAT
    lstat
    #else
    stat
    #endif
      (cname,&buf);
  return r != ERROR && S_ISDIR(buf.st_mode);
}

M2_string system_realpath(M2_string filename) {
 #ifdef HAVE_REALPATH
  char *fn = M2_tocharstar(filename);
  char buf[PATH_MAX+1];
  char *r = realpath(*fn ? fn : ".",buf);
  if (isDirectory(r) && r[1] != 0) strcat(r,"/");
  freemem(fn);
  return r == NULL ? NULL : M2_tostring(buf);
 #else
  return filename;
 #endif
}

M2_string system_errfmt(M2_string filename, int lineno, int colno, int loaddepth) {
	char *s = getmem_atomic(filename->len+strlen(posfmt)+10);
	char *fn = M2_tocharstar(filename);
	M2_string ret;
	sprintf(s,posfmt,fn,lineno,colno,loaddepth);
	ret = M2_tostring(s);
	freemem(s);
	freemem(fn);
	return ret;
}

/* stupid ANSI forces some systems to put underscores in front of useful identifiers */
#if !defined(S_ISREG)
#if defined(_S_ISREG)
#define S_ISREG _S_ISREG
#elif defined(S_IFREG)
#define S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#elif defined(_S_IFREG)
#define S_ISREG(m)	(((m) & _S_IFMT) == _S_IFREG)
#endif
#endif

M2_bool system_fileExists(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r = stat(cname,&buf);
  errno = 0;
  freemem(cname);
  return r != ERROR;
}

M2_bool system_fileReadable(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = access(cname,R_OK);
  errno = 0;
  freemem(cname);
  return r != ERROR;
}

M2_bool system_fileWritable(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = access(cname,W_OK);
  errno = 0;
  freemem(cname);
  return r != ERROR;
}

M2_bool system_fileExecutable(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = access(cname,X_OK);
  errno = 0;
  freemem(cname);
  return r != ERROR;
}

int system_fileMode(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r = stat(cname,&buf);
  freemem(cname);
  return r == ERROR ? -1 : buf.st_mode & ~S_IFMT;
}

int system_fileModeFD(int fd) {
  struct stat buf;
  int r = fstat(fd,&buf);
  return r == ERROR ? -1 : buf.st_mode & ~S_IFMT;
}

int system_chmod(M2_string name,int mode) {
  char *cname = M2_tocharstar(name);
  int r = chmod(cname,mode);
  freemem(cname);
  return r;
}

int system_isDirectory(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = isDirectory(cname);
  freemem(cname);
  return r;
}

int system_isRegularFile(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r = 
    #ifdef HAVE_LSTAT
    lstat
    #else
    stat
    #endif
    (cname,&buf);
  freemem(cname);
  return r == ERROR ? -1 : S_ISREG(buf.st_mode);
}

int always(const struct dirent *p) { return 1; }

M2_ArrayString system_readDirectory(M2_string name) {
  int n=0, i=0;
  M2_ArrayString a;
  char *cname = M2_tocharstar(name);
  struct dirent *entry;
  DIR *dir = opendir(cname);
  freemem(cname);
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

int system_fileLength(int fd) {
  struct stat statbuf;
  if (ERROR == fstat(fd,&statbuf)) return ERROR;
  return statbuf.st_size;
}

int system_fileLength_1(M2_string filename) {
  char *cname = M2_tocharstar(filename);
  struct stat statbuf;
  int ret = stat(cname,&statbuf);
  freemem(cname);
  if (ERROR == ret) return ERROR;
  return statbuf.st_size;
}

int system_fileTime(M2_string name) {
  char *cname = M2_tocharstar(name);
  struct stat buf;
  int r;
  r = 
    #ifdef HAVE_LSTAT
    lstat
    #else
    stat
    #endif
      (cname,&buf);
  freemem(cname);
  if (r == ERROR) return -1;
  return buf.st_mtime;
}

int system_setFileTime(M2_string name, int modtime) {
  char *cname = M2_tocharstar(name);
  struct utimbuf buf = { time(NULL), modtime };
  int r;
  r = utime(cname,&buf);
  freemem(cname);
  if (r == ERROR) return -1;
  return 0;
}

int system_mkdir(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = 
    #ifdef __MINGW32__
    mkdir(cname)
    #else
    mkdir(cname,0777)
    #endif
    ;
  freemem(cname);
  return r;
}

int system_rmdir(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = rmdir(cname);
  freemem(cname);
  return r;
}

int system_unlink(M2_string name) {
  char *cname = M2_tocharstar(name);
  int r = unlink(cname);
  freemem(cname);
  return r;
}

int system_link(M2_string oldfilename,M2_string newfilename) {
  char *old = M2_tocharstar(oldfilename);
  char *new = M2_tocharstar(newfilename);
  int r = 
    #ifdef HAVE_LINK
    link(old,new)
    #else
    -1
    #endif
    ;
  freemem(old);
  freemem(new);
  return r;
}

int system_symlink(M2_string oldfilename,M2_string newfilename) {
  char *old = M2_tocharstar(oldfilename);
  char *new = M2_tocharstar(newfilename);
  int r = 
    #ifdef HAVE_SYMLINK
    symlink(old,new)
    #else
    -1
    #endif
    ;
  freemem(old);
  freemem(new);
  return r;
}

M2_string system_readfile(int fd) {
     struct stat buf;
     size_t bufsize = 1024;
     char *text;
     size_t size = 0;
     if (!(ERROR == fstat(fd,&buf) || !S_ISREG(buf.st_mode) || 0 == buf.st_size)) {
       off_t filesize = buf.st_size;
       off_t pos = lseek(fd,0,SEEK_CUR);
       if (pos != (off_t)(-1)) bufsize -= pos;
       bufsize = (size_t)filesize;
       if ((off_t)(bufsize) != filesize || bufsize > SSIZE_MAX) return NULL; /* file too big */
     }
     text = getmem_atomic(bufsize);
     while (TRUE) {
	  int n = read(fd,text+size,bufsize-size);
	  if (ERROR == n) {
#ifdef EINTR
	       if (errno == EINTR) break;
#endif
	       return NULL;
	       }
	  if (0 == n) break;
	  size += n;
	  if (size == bufsize) {
	       char *p;
	       size_t newbufsize = 2 * bufsize;
	       p = getmem_atomic(newbufsize);
	       memcpy(p,text,size);
	       bufsize = newbufsize;
	       freemem(text);
	       text = p;
	       }
	  }
     M2_string s = (M2_string)getmem_atomic(sizeofarray(s,size));
     s->len = size;
     memcpy(s->array,text,size);
     freemem(text);
     return s;
}

static const char *hostname_error_message;

#if defined(HAVE_GETADDRINFO) && GETADDRINFO_WORKS
static int set_addrinfo(struct addrinfo **addr, struct addrinfo *hints, char *hostname, char *service) {
     int ret;
     ret = getaddrinfo(hostname, service, hints /* thanks to Dan Roozemond for pointing out this was NULL before, causing problems */, addr);
     hostname_error_message = ret != 0 ? gai_strerror(ret) : NULL;
     return ret;
}
#endif

#ifndef HAVE_HSTRERROR
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
#endif

#if !(defined(HAVE_GETADDRINFO) && GETADDRINFO_WORKS)
int host_address(name)
char *name;
{
#ifdef HAVE_SOCKET
     if ('0' <= name[0] && name[0] <= '9') {
     	  int s;
	  s = inet_addr(name);	/* this function is obsolete, replaced by inet_aton(); we use it only if getaddrinfo is not available */
	  if (s == ERROR) {
	       hostname_error_message = "IP address translation failed";
	       return ERROR;
	  }
	  return s;
	  }
     else {
	  struct hostent *t;
	  /* FIXME
	  if (SETJMP(interrupt_jump)) {
	       interrupt_jump_set = FALSE;
	       return ERROR;
	  }
	  else interrupt_jump_set = TRUE; */
	  t = gethostbyname(name); /* this function is obsolete because it doesn't handle IPv6; we use it only if getaddrinfo is not available */
	  // interrupt_jump_set = FALSE;
	  if (t == NULL) {
	       hostname_error_message = hstrerror(h_errno);
	       return ERROR;
	  }
	  else {
	       return *(int *)t->h_addr;
	  }
     }
#else
     return ERROR;
#endif
     }

int serv_address(name)
char *name;
{
#ifdef HAVE_SOCKET
     if ('0' <= name[0] && name[0] <= '9') {
	  return htons(atoi(name));
	  }
     else {
	  struct servent *t = getservbyname(name,"tcp");
	  if (t == NULL) {
	    errno = ENXIO;
	    return ERROR;
	  }
	  else {
	    return t->s_port;
	  }
     }
#else
     return ERROR;
#endif
     }
#endif

int system_acceptBlocking(int so) {
#ifdef HAVE_ACCEPT
  struct sockaddr_in addr;
  socklen_t addrlen = sizeof addr;
#ifdef HAVE_FCNTL
  fcntl(so,F_SETFL,0);
#endif
  return accept(so,(struct sockaddr*)&addr,&addrlen);
#else
  return ERROR;
#endif
}

int system_acceptNonblocking(int so) {
#if defined(HAVE_SOCKET) && defined(HAVE_FCNTL)
  struct sockaddr_in addr;
  socklen_t addrlen = sizeof addr;
  int sd;
  fcntl(so,F_SETFL,O_NONBLOCK);
  sd = accept(so,(struct sockaddr*)&addr,&addrlen);
  return sd;
#else
  return ERROR;
#endif
}

#define INCOMING_QUEUE_LEN 10

int openlistener(char *interface0, char *service) {
#ifdef HAVE_SOCKET
#if defined(HAVE_GETADDRINFO) && GETADDRINFO_WORKS
  struct addrinfo *addr = NULL;
  static struct addrinfo hints;	/* static so all parts get initialized to zero */
  int so;
  hints.ai_family = PF_UNSPEC;
  hints.ai_flags = AI_PASSIVE;
  if (0 != set_addrinfo(&addr,&hints,interface0,service)) return ERROR;
  so = socket(addr->ai_family,SOCK_STREAM,0);
  if (ERROR == so) { freeaddrinfo(addr); return ERROR; }
  if (ERROR == bind(so,addr->ai_addr,addr->ai_addrlen) || ERROR == listen(so, INCOMING_QUEUE_LEN)) { freeaddrinfo(addr); close(so); return ERROR; }
  freeaddrinfo(addr); 
  return so;
#else
  int sa = serv_address(service);
  int so = socket(AF_INET,SOCK_STREAM,0);
  struct sockaddr_in addr;
  addr.sin_family = PF_INET;
  addr.sin_port = sa;
  addr.sin_addr.s_addr = INADDR_ANY;
  if (ERROR == so ||
      ERROR == sa ||
      ERROR == bind(so,(struct sockaddr*)&addr,sizeof addr) ||
      ERROR == listen(so, INCOMING_QUEUE_LEN)) { close(so); return ERROR; }
  return so;
#endif
#else
  return ERROR;
#endif
}

int opensocket(char *host, char *service) {
  /* FIXME
  if (SETJMP(interrupt_jump)) {
    interrupt_jump_set = FALSE;
    return ERROR;
  } else { interrupt_jump_set = TRUE; }
  */
#ifdef HAVE_SOCKET
#if defined(HAVE_GETADDRINFO) && GETADDRINFO_WORKS
  struct addrinfo *addr;
  int so;
  if (0 != set_addrinfo(&addr,NULL,host,service)) return ERROR;
  so = socket(addr->ai_family,SOCK_STREAM,0);
  if (ERROR == so) { freeaddrinfo(addr); return ERROR; }
  if (ERROR == connect(so,addr->ai_addr,addr->ai_addrlen)) { freeaddrinfo(addr); close(so); return ERROR; }
  // interrupt_jump_set = FALSE;
  freeaddrinfo(addr);
  return so;
#else
  int sd = socket(AF_INET,SOCK_STREAM,0);
  struct sockaddr_in addr;
  int sa = serv_address(service);
  if (sa == ERROR) {
       /* strange but true, some systems don't list the common services:
		u24% uname -a
		SunOS u24.math.uiuc.edu 5.8 Generic_117350-34 sun4u sparc
		u24% grep http /etc/services
		u24% 
       */
       if (0 == strcmp(service,"http")) sa = 80;
       else if (0 == strcmp(service,"https")) sa = 443;
  }
  addr.sin_family = PF_INET;
  addr.sin_port = sa;
  addr.sin_addr.s_addr = host_address(host);
  if (ERROR == addr.sin_addr.s_addr ||
      ERROR == sa ||
      ERROR == connect(sd,(struct sockaddr *)&addr,sizeof(addr))) { close(sd); return ERROR; }
  return sd;
#endif
#else
  return ERROR;
#endif
}

int system_opensocket(M2_string host,M2_string serv) {
     char *tmphost = M2_tocharstar(host);
     char *tmpserv = M2_tocharstar(serv);
     int sd = opensocket(tmphost,tmpserv);
     freemem(tmphost);
     freemem(tmpserv);
     return sd;
     }

int system_openlistener(M2_string interface0,M2_string serv) {
     char *tmpinterface0 = M2_tocharstar(interface0);
     char *tmpserv = M2_tocharstar(serv);
     int sd = openlistener(*tmpinterface0 ? tmpinterface0 : NULL,tmpserv);
     freemem(tmpinterface0);
     freemem(tmpserv);
     return sd;
     }

int system_errno(void) {
  return 
#ifdef HAVE_HERROR
    h_errno > 0 ? h_errno : 
#endif
    errno;
}

char const *system_strerror(void) {
     if (hostname_error_message) {
	  char const *msg = hostname_error_message;
	  hostname_error_message = NULL;
	  return msg;
     }
     if (errno > 0) {
	  char const *msg = strerror(errno);
	  errno = 0;
	  return msg;
     }
     if (test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field))) return "interrupted";
     return "no system error indication found";
}

M2_string system_syserrmsg()
{
     return M2_tostring(system_strerror());
}

int system_run(M2_string command){
     char *c = M2_tocharstar(command);
     int r = fix_status(system(c));
     freemem(c);
     return r;
     }

struct FUNCTION_CELL *pre_final_list, *final_list, *thread_prepare_list;

void system_atend(void (*func)()){
     struct FUNCTION_CELL *this_final = (struct FUNCTION_CELL *)getmem(sizeof(struct FUNCTION_CELL));
     this_final -> func = func;
     this_final -> next = pre_final_list;
     pre_final_list = this_final;
     }

int system_strncmp(M2_string s,M2_string t,int n) {
     return strncmp((char *)s->array,(char *)t->array,n);
}

void do_nothing () { }

/*
// Local Variables:
// compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
