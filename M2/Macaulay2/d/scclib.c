/*		Copyright 1994 by Daniel R. Grayson		*/

#include <dirent.h>

#include "types.h"
#include "M2mem.h"
#include "../c/compat.c"
#include "debug.h"

void
#if defined(__STDC__)
fatal(char *s,...)   {
     va_list ap;
#else
fatal( va_alist  ) 
va_dcl
{
     va_list ap;
     char *s;
#endif
#if defined(__STDC__)
     va_start(ap,s);
#else
     va_start(ap);
     s = va_arg(ap, char *);
#endif
     vfprintf(stderr,s,ap);
     fprintf(stderr,"\n");
     fflush(stderr);
     va_end(ap);
#ifndef NDEBUG
     trap();
#endif
     exit(1);
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

int system_write(int fd, M2_string buffer, int len){
     if ((int)buffer->len < len) fatalarrayindex(len,buffer->len,__FILE__,__LINE__,-1);
     return write(fd,buffer->array,len);
     }

int system_sleep(int t) {
  return sleep(t);
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
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_TRUNC, 0644);
     GC_FREE(fname);
     return fd;
     }

int system_openoutappend(filename)
M2_string filename;
{
     char *fname = tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_APPEND, 0644);
     GC_FREE(fname);
     return fd;
     }

int system_pipe(fildes)
M2_arrayint fildes;
{
     return pipe(fildes->array);
     }

int system_exec(argv)
M2_stringarray argv;
{
     int i;
     char **av = tocharstarstar(argv);
     execvp(av[0],av);
     for (i=0; i<(int)argv->len; i++) {
     	  GC_FREE(av[i]);
	  }
     GC_FREE(av);
     return ERROR;
     }

int actors5_sizeofDouble(void) { return sizeof(double); }

double actors5_convertnettodouble(M2_string p,int pos) {
  double x;
  memcpy((char *)&x,p->array+pos,sizeof(double));
  return x;
}

M2_string actors5_convertdoubletonet(double x) {
  int n = sizeof (double);
  M2_string p = (M2_string)getmem_atomic(n);
  p->len = n;
  memcpy(p->array,(char *)&x,n);
  return p;
}

M2_string interp_dirname(M2_string s) {
  char *t = tocharstar(s);
  char *u = t;
  char *v = u;
  for (; *u; u++) if (*u == '/') v=u+1;	/* on MacOS?? */
  if (v != NULL) *v = '\0';
  if (*t == '\0') t = "./";	/* on MacOS?? */
  return tostring(t);
}

M2_string system_getcwd()
{
     /* this function now adds a terminal / to the directory name */
     char buf[700];
     /* We have to get the cwd each time, because otherwise we might pick up the
        cwd from when dumpdata was run, which could have been different from now. */
     char *x = getcwd(buf,sizeof(buf)-1);
#if defined(_WIN32)
     char *p;
     for (p=x; *p; p++) if (*p == '\\') *p = '/';
#endif
     if (0 != strcmp(buf,"/")) strcat(buf,"/");
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
  int slen = s->len, tlen = t->len, i;
  int ret = 0;
  int len = slen < tlen ? slen : tlen;
  char *sarray = s->array;
  char *tarray = t->array;
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

int system_strnumcmp(s,t)
M2_string s,t;
{
     int slen = s->len, tlen = t->len, i;
     int ret = 0;
     int len = slen < tlen ? slen : tlen;
     int innumber = FALSE;
     char *sarray = s->array;
     char *tarray = t->array;
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

int system_wait(pid)
int pid;
{
     int status;
     int ret = waitpid(pid,&status,0);
     if (ret == ERROR) return ERROR;
     return status>>8;
     }

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
	       z->array[i] = ret == ERROR ? -1 : WIFEXITED(status[i]) ? status[i] >> 8 : -2;
	  }
	  return z;
     }
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
     unsigned int i;
     for (i=0; i<sizeof(x); i++) {
	  h = 231*h + p[i];
	  }
#endif
     return h;
     }

M2_string system_readlink(M2_string filename) {
  char *fn = tocharstar(filename);
  int size = 100;
  M2_string s = NULL;
  while (TRUE) {
    char buf[size];
    int r = readlink(fn,buf,sizeof buf);
    if (r == -1) {
      s = tostring("");
      break;
    }
    if (r < size) {
      s = tostringn(buf,r);
      break;
    }
    size *= 2;			/* r == size, try again */
  }
  GC_FREE(fn);
  return s;
}

int system_chdir(M2_string filename) {
  char *fn = tocharstar(filename);
  int ret = chdir(fn);
  GC_FREE(fn);
  return ret;
}

M2_string system_realpath(M2_string filename) {
  char *fn = tocharstar(filename);
  char buf[PATH_MAX];
  char *r = realpath(fn,buf);
  GC_FREE(fn);
  return r == NULL ? filename : tostring(buf);
}

M2_string system_errfmt(M2_string filename, int lineno, int colno, int loaddepth) {
	char *s = getmem_atomic(filename->len+strlen(posfmt)+10);
	char *fn = tocharstar(filename);
	M2_string ret;
	sprintf(s,posfmt,fn,lineno,colno,loaddepth);
	ret = tostring(s);
	GC_FREE(s);
	GC_FREE(fn);
	return ret;
}

int system_read(fd,buffer,len)
int fd;
M2_string buffer;
int len;
{
     if ((int)buffer->len < len) fatalarrayindex(len,buffer->len,__FILE__,__LINE__,-1);
     if (len == 0) return 0;
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
     if ((int)buffer->len < len+offset) {
	  fatalarrayindex(len+offset,buffer->len,__FILE__,__LINE__,-1);
	  }
     if (len == 0) return 0;
     return read(fd,buffer->array+offset,len);
     }

#include <readline/readline.h>
#include <readline/history.h>

extern M2_stringarray objects_completions(M2_string);

static char *M2_completion_generator(const char *text, int state) {
  static int i;
  static char **v;
  char *p;
  if (state == 0) {
    M2_string s;
    M2_stringarray ret;
    i = 0;
    if (v != NULL) free(v);
    s = tostring(text);
    ret = objects_completions(s);
    GC_FREE(s);
    v = tocharstarstar_malloc(ret); /* readline will use free() to free these strings */
    GC_FREE(ret);
  }
  p = v[i];
  if (p != NULL) i++;
  return p;
}

static char **M2_completion(const char *text, int start, int end) {
  rl_attempted_completion_over = TRUE;
  /* if (start > 0 && rl_line_buffer[start-1] == '"') ... filename completion ... */
  return rl_completion_matches(text, M2_completion_generator);
}


void init_readline_variables(void) {
  extern char *_rl_comment_begin;
  _rl_comment_begin = "-- ";
  rl_readline_name = "M2";
  rl_attempted_completion_function = M2_completion;
  rl_basic_word_break_characters = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r";
  using_history();
}

static int read_via_readline(char *buf,int len,char *prompt) {
  static char *p;		/* buffer, NULL if newline has already been returned */
  static int plen;		/* number of chars in p */
  static int i;			/* number of chars in p already returned */
  int r;			/* number of chars to return this time */
  if (len == 0) return 0;
  if (p == NULL) {
    p = readline(prompt);
    if (p == NULL) return 0;	/* EOF */
    i = 0;
    plen = strlen(p);
    if (*p) add_history(p);
  }
  r = plen - i;
  if (r > len) r = len;
  memmove(buf,p+i,r), i+=r;
  if (i == plen && r < len) {
    free(p), p = NULL;
    buf[r++] = '\n';		/* readline() doesn't include the \n at the end */
  }
  return r;
}

int system_readline(M2_string buffer, int len, int offset, M2_string prompt) {
  char *p = tocharstar(prompt);
  int r;
  if (offset < 0 || (int)buffer->len - offset < len) fatalarrayindex(len,buffer->len,__FILE__,__LINE__,-1);
  r = read_via_readline(buffer->array + offset,len,p);
  GC_FREE(p);
  return r;
}

#if 0
M2_stringarray system_history(void) {
  M2_stringarray a;
  HIST_ENTRY **h = history_list();
  int i,n;
  for (n=0; h != NULL && h[n] != NULL && h[n]->data != NULL; n++);
  a = (M2_stringarray) getmem (sizeofarray(a,n));
  a->len = n;
  for (i=0; i<n; i++) a->array[i] = tostring(h[i]->line);
  return a;
}
#endif

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

int system_fileExists(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r = stat(cname,&buf);
  GC_FREE(cname);
  return r != ERROR;
}

int system_fileMode(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r = stat(cname,&buf);
  GC_FREE(cname);
  return r == ERROR ? -1 : buf.st_mode & ~S_IFMT;
}

int system_fileModeFD(int fd) {
  struct stat buf;
  int r = fstat(fd,&buf);
  return r == ERROR ? -1 : buf.st_mode & ~S_IFMT;
}

int system_chmod(M2_string name,int mode) {
  char *cname = tocharstar(name);
  int r = chmod(cname,mode);
  GC_FREE(cname);
  return r;
}

int system_isDirectory(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r = lstat(cname,&buf);
  GC_FREE(cname);
  return r == ERROR ? -1 : S_ISDIR(buf.st_mode);
}

int system_isRegularFile(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r = lstat(cname,&buf);
  GC_FREE(cname);
  return r == ERROR ? -1 : S_ISREG(buf.st_mode);
}

int always(const struct dirent *p) { return 1; }

M2_stringarray system_readDirectory(M2_string name) {
  int n=0, i=0;
  M2_stringarray a;
  char *cname = tocharstar(name);
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
  a = (M2_stringarray) getmem (sizeofarray(a,n));
  a->len = n;
  for (i=0; i<n && (entry = readdir(dir)) != NULL; i++) a->array[i] = tostring(entry->d_name);
  for (   ; i<n ; i++) a->array[i] = tostring("");
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
  char *cname = tocharstar(filename);
  struct stat statbuf;
  int ret = stat(cname,&statbuf);
  GC_FREE(cname);
  if (ERROR == ret) return ERROR;
  return statbuf.st_size;
}

int system_fileTime(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r;
  r = lstat(cname,&buf);
  GC_FREE(cname);
  if (r == ERROR) return -1;
  return buf.st_mtime;
}

int system_setFileTime(M2_string name, int modtime) {
  char *cname = tocharstar(name);
  struct utimbuf buf = { time(NULL), modtime };
  int r;
  r = utime(cname,&buf);
  GC_FREE(cname);
  if (r == ERROR) return -1;
  return 0;
}

int system_currentTime() {
  return time(NULL);
}

int system_mkdir(M2_string name) {
  char *cname = tocharstar(name);
  int r = mkdir(cname,0777);
  GC_FREE(cname);
  return r;
}

int system_rmdir(M2_string name) {
  char *cname = tocharstar(name);
  int r = rmdir(cname);
  GC_FREE(cname);
  return r;
}

int system_unlink(M2_string name) {
  char *cname = tocharstar(name);
  int r = unlink(cname);
  GC_FREE(cname);
  return r;
}

int system_link(M2_string oldfilename,M2_string newfilename) {
  char *old = tocharstar(oldfilename);
  char *new = tocharstar(newfilename);
  int r = link(old,new);
  GC_FREE(old);
  GC_FREE(new);
  return r;
}

int system_symlink(M2_string oldfilename,M2_string newfilename) {
  char *old = tocharstar(oldfilename);
  char *new = tocharstar(newfilename);
  int r = symlink(old,new);
  GC_FREE(old);
  GC_FREE(new);
  return r;
}

M2_string system_readfile(fd)
int fd;
{
     M2_string s;
     unsigned int filesize;
     struct stat buf;
     if (ERROR == fstat(fd,&buf) || !S_ISREG(buf.st_mode) || 0 == buf.st_size) {
       char *text;
       unsigned int bufsize = 1024;
       unsigned int size = 0;
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
		 int newbufsize = 2 * bufsize;
		 p = getmem_atomic(newbufsize);
		 memcpy(p,text,size);
		 bufsize = newbufsize;
		 GC_FREE(text);
		 text = p;
		 }
	    }
       s = (M2_string)getmem_atomic(sizeofarray(s,size));
       s->len = size;
       memcpy(s->array,text,size);
       GC_FREE(text);
       return s;
     }
     else {
       filesize = buf.st_size;
       s = (M2_string)getmem_atomic(sizeofarray(s,filesize));
       s->len = filesize;
       if (filesize != read(fd,s->array,filesize)) fatal("can't read entire file, file descriptor %d", fd);
       return s;
     }
}

static const char *hostname_error_message;

#if HAVE_GETADDRINFO && GETADDRINFO_WORKS
static int set_addrinfo(struct addrinfo **addr, struct addrinfo *hints, char *hostname, char *service) {
     int ret;
     ret = getaddrinfo(hostname, service, NULL, addr);
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

#if !(HAVE_GETADDRINFO && GETADDRINFO_WORKS)
int host_address(name)
char *name;
{
#if HAVE_SOCKET
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
	  if (sigsetjmp(interrupt_jump,TRUE)) {
	       interrupt_jump_set = FALSE;
	       return ERROR;
	  }
	  else interrupt_jump_set = TRUE;
	  t = gethostbyname(name); /* this function is obsolete because it doesn't handle IPv6; we use it only if getaddrinfo is not available */
	  interrupt_jump_set = FALSE;
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
#if HAVE_SOCKET
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
#if HAVE_SOCKET
  struct sockaddr_in addr;
  unsigned int addrlen = sizeof addr;
  fcntl(so,F_SETFL,0);
  return accept(so,(struct sockaddr*)&addr,&addrlen);
#else
  return ERROR;
#endif
}

int system_acceptNonblocking(int so) {
#if HAVE_SOCKET
  struct sockaddr_in addr;
  unsigned int addrlen = sizeof addr;
  int sd;
  fcntl(so,F_SETFL,O_NONBLOCK);
  sd = accept(so,(struct sockaddr*)&addr,&addrlen);
  return sd;
#else
  return ERROR;
#endif
}

#define INCOMING_QUEUE_LEN 10

int openlistener(char *service) {
#if HAVE_SOCKET
#if HAVE_GETADDRINFO && GETADDRINFO_WORKS
  struct addrinfo *addr;
  static struct addrinfo hints;
  int so;
  hints.ai_flags = AI_PASSIVE;
  if (0 != set_addrinfo(&addr,&hints,NULL,service)) return ERROR;
  so = socket(addr->ai_family,SOCK_STREAM,0);
  if (ERROR == so) return ERROR;
  if (ERROR == bind(so,addr->ai_addr,addr->ai_addrlen) || ERROR == listen(so, INCOMING_QUEUE_LEN)) { close(so); return ERROR; }
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
#if HAVE_SOCKET
#if HAVE_GETADDRINFO && GETADDRINFO_WORKS
  struct addrinfo *addr;
  int so;
  if (sigsetjmp(interrupt_jump,TRUE)) {
       interrupt_jump_set = FALSE;
       return ERROR;
  }
  else interrupt_jump_set = TRUE;
  if (0 != set_addrinfo(&addr,NULL,host,service)) return ERROR;
  so = socket(addr->ai_family,SOCK_STREAM,0);
  if (ERROR == so) return ERROR;
  if (ERROR == connect(so,addr->ai_addr,addr->ai_addrlen)) { close(so); return ERROR; }
  interrupt_jump_set = FALSE;
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

int system_openlistener(serv)
M2_string serv;
{
     char *Serv = tocharstar(serv);
     int sd = openlistener(Serv);
     GC_FREE(Serv);
     return sd;
     }

extern int errno;
#if defined(HAVE_DECL_SYS_NERR) && !HAVE_DECL_SYS_NERR
extern int sys_nerr;
#endif

#if HAVE_HERROR
extern int h_nerr;
#if !H_ERRLIST_IS_DECLARED
extern const char * const h_errlist[];
#endif
#endif

#if !SYS_ERRLIST_IS_DECLARED
extern const char * const sys_errlist[];
#endif

int system_errno(void) {
  return 
#if HAVE_HERROR
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
     if (system_interruptedFlag) return "interrupted";
     return "no system error indication found";
}

M2_string system_syserrmsg()
{
     return tostring(system_strerror());
}

int system_run(M2_string command){
     char *c = tocharstar(command);
     int r = system(c);
     GC_FREE(c);
     return r;
     }

struct FINAL *final_list, *pre_final_list;

void system_atend(void (*f)()){
     struct FINAL *this_final = (struct FINAL *)getmem(sizeof(struct FINAL));
     this_final -> final = f;
     this_final -> next = pre_final_list;
     pre_final_list = this_final;
     }

int system_strncmp(M2_string s,M2_string t,int n) {
  return strncmp(s->array,t->array,n);
}

#define re_compile_fastmap M2_re_compile_fastmap
#define re_compile_pattern M2_re_compile_pattern
#define re_match M2_re_match
#define re_match_2 M2_re_match_2
#define re_search M2_re_search
#define re_search_2 M2_re_search_2
#define re_set_registers M2_re_set_registers
#define re_set_syntax M2_re_set_syntax
#define regcomp M2_regcomp
#define regerror M2_regerror
#define regexec M2_regexec
#define regfree M2_regfree
#include "regex.h"

#define SYNTAX_FLAGS   (REG_EXTENDED | REG_NEWLINE)

struct M2_string_struct noErrorMessage;
M2_string system_noErrorMessage = &noErrorMessage;
M2_string system_regexmatchErrorMessage = &noErrorMessage;

static M2_string last_pattern = NULL;

static regex_t regex_pattern;

#define match_start(i) match[i].rm_so
#define match_end(i)   match[i].rm_eo
#define regexec_empty_return REG_NOMATCH
#define match_length(i) (match_end(i) - match_start(i))

M2_arrayint system_regexmatch(M2_string pattern, int offset, M2_string text) {
  static struct M2_arrayint_struct empty[1] = {{0}};
  int regcomp_return;
  system_regexmatchErrorMessage = &noErrorMessage;
  if (! (0 <= offset && offset <= text->len)) return empty;
  if (last_pattern != pattern) {
    if (last_pattern != NULL) regfree(&regex_pattern), last_pattern = NULL;
    {
	char *s_pattern;
	s_pattern = tocharstar(pattern);
	regcomp_return = regcomp(&regex_pattern, s_pattern, SYNTAX_FLAGS);
	GC_FREE(s_pattern);
    }
    if (regcomp_return != 0) {
	 char message[1024];
         regerror(regcomp_return,&regex_pattern,message,sizeof message);
         system_regexmatchErrorMessage = tostring(message);
	 regfree(&regex_pattern);
	 return empty;
    }
    last_pattern = pattern;
  }
  {
    int n = regex_pattern.re_nsub+1;
    int regexec_return;
    static M2_string prevtext;
    static char *s_text;
    if (prevtext != text) {
	 if (s_text != NULL) GC_FREE(s_text);
	 s_text = tocharstar(text), prevtext = text;
    }
    regmatch_t match[n];
    regexec_return = regexec(&regex_pattern, s_text + offset, n, match, offset != 0 && s_text[offset-1] != '\n' ? REG_NOTBOL : 0);
    if (regexec_return == regexec_empty_return) return empty;
    else {
      M2_arrayint m = makearrayint(2*n);
      int i;
      for (i = 0; i<n; i++) {
	m->array[2*i  ] = match_start(i) + offset;
	m->array[2*i+1] = match_length(i);
      }
      return m;
    }
  }
}

void grow(int *len, int off, char **str, int newlen) {
     int d = 2**len+1;
     if (newlen < d) newlen = d;
     char *p = getmem(newlen);
     memcpy(p,*str,off);
     GC_FREE(*str);
     *len = newlen;
     *str = p;
}

void cat(int *xlen, int *xoff, char **x, int ylen, char *y) {
     if (*xoff + ylen > *xlen) grow(xlen,*xoff,x,*xoff + ylen);
     memcpy(*x+*xoff,y,ylen);
     *xoff += ylen;
}

M2_string system_regexreplace(M2_string pattern, M2_string replacement, M2_string text, M2_string errorflag) {
  int regcomp_return;
  system_regexmatchErrorMessage = &noErrorMessage;
  if (last_pattern != pattern) {
    if (last_pattern != NULL) regfree(&regex_pattern), last_pattern = NULL;
    {
      char *s_pattern;
      s_pattern = tocharstar(pattern);
      regcomp_return = regcomp(&regex_pattern, s_pattern, SYNTAX_FLAGS);
      GC_FREE(s_pattern);
    }
    if (regcomp_return != 0) {
	 char message[1024];
         regerror(regcomp_return,&regex_pattern,message,sizeof message);
         system_regexmatchErrorMessage = tostring(message);
	 regfree(&regex_pattern);
	 return errorflag;
    }
    last_pattern = pattern;
  }
  {
    int n = regex_pattern.re_nsub+1;
    regmatch_t match[n];
    int offset = 0;
    int textlen = text->len;
    int buflen = text->len + 3 * replacement->len + 16;
    int bufct = 0;
    char *buf = getmem_atomic(buflen);
    int i;
    static M2_string prevtext;
    static char *s_text;
    if (prevtext != text) {
	 if (s_text != NULL) GC_FREE(s_text);
	 s_text = tocharstar(text), prevtext = text;
    }
    while (regexec(&regex_pattern, s_text + offset, n, match, offset != 0 && s_text[offset-1] != '\n' ? REG_NOTBOL : 0) != regexec_empty_return) {
	 char *p;
	 int plen;
	 /* copy the unmatched text up to the match */
	 cat(&buflen,&bufct,&buf, match_start(0),text->array+offset);
	 /* perform the replacement */
	 p = replacement->array;
	 plen = replacement->len;
	 while (TRUE) {
	      char *q = p;
	      while (TRUE) {
		   q = memchr(q,'\\',plen-(q-p));
		   if (q==NULL || isdigit((int)q[1])) break;
		   q++;
	      }
	      if (q==NULL) break;
	      cat(&buflen,&bufct,&buf,q-p,p);
	      plen -= q-p;
	      p = q;
	      i = q[1] - '0';
	      if (0 <= i && i < n && i <= 9) cat(&buflen,&bufct,&buf,match_length(i),text->array+offset+match_start(i));
	      p += 2;
	      plen -= 2;
	 }
	 cat(&buflen,&bufct,&buf,plen,p);
	 /* reset the offset after the matched part */
	 offset += match_end(0);
	 /* if the matched part was empty, move onward a bit */
	 if (match_end(0) == match_start(0)) {
	      if (offset == textlen) break;
	      cat(&buflen,&bufct,&buf, 1, text->array+offset);
	      offset += 1;
	 }
    }
    /* copy the last part of the text */
    cat(&buflen,&bufct,&buf, textlen-offset, text->array+offset);
    return tostringn(buf, bufct);
  }
}

M2_stringarray system_regexselect(M2_string pattern, M2_string replacement, M2_string text, M2_stringarray errorflag) {
  int regcomp_return;
  system_regexmatchErrorMessage = &noErrorMessage;
  if (last_pattern != pattern) {
    if (last_pattern != NULL) regfree(&regex_pattern), last_pattern = NULL;
    {
      char *s_pattern;
      s_pattern = tocharstar(pattern);
      regcomp_return = regcomp(&regex_pattern, s_pattern, SYNTAX_FLAGS);
      GC_FREE(s_pattern);
    }
    if (regcomp_return != 0) {
	 char message[1024];
         regerror(regcomp_return,&regex_pattern,message,sizeof message);
         system_regexmatchErrorMessage = tostring(message);
	 regfree(&regex_pattern);
	 return errorflag;
    }
    last_pattern = pattern;
  }
  {
    int n = regex_pattern.re_nsub+1;
    regmatch_t match[n];
    int offset = 0;
    int textlen = text->len;
    int buflen = 2 * replacement->len + 24;
    char *buf = getmem_atomic(buflen);
    int i;
    int retlen = 10, retct = 0;
    M2_stringarray ret = (M2_stringarray)getmem_atomic(sizeofarray(ret,retlen));
    static M2_string prevtext;
    static char *s_text;
    if (prevtext != text) {
	 if (s_text != NULL) GC_FREE(s_text);
	 s_text = tocharstar(text), prevtext = text;
    }
    while (regexec(&regex_pattern, s_text + offset, n, match, offset != 0 && s_text[offset-1] != '\n' ? REG_NOTBOL : 0) != regexec_empty_return) {
	 int bufct = 0;
	 char *p;
	 int plen;
	 /* perform the replacement */
	 p = replacement->array;
	 plen = replacement->len;
	 while (TRUE) {
	      char *q = p;
	      while (TRUE) {
		   q = memchr(q,'\\',plen-(q-p));
		   if (q==NULL || isdigit((int)q[1])) break;
		   q++;
	      }
	      if (q==NULL) break;
	      cat(&buflen,&bufct,&buf,q-p,p);
	      plen -= q-p;
	      p = q;
	      i = q[1] - '0';
	      if (0 <= i && i < n && i <= 9) cat(&buflen,&bufct,&buf,match_length(i),text->array+offset+match_start(i));
	      p += 2;
	      plen -= 2;
	 }
	 cat(&buflen,&bufct,&buf,plen,p);
	 /* reset the offset after the matched part */
	 offset += match_end(0);
	 /* make an M2_string and append it to the return list */
	 {
	      if (retct == retlen) {
		   int newlen = 2 * retlen;
		   int k;
		   M2_stringarray newret = (M2_stringarray)getmem_atomic(sizeofarray(ret,newlen));
		   for (k=0; k<retlen; k++) newret->array[k] = ret->array[k];
		   GC_FREE(ret);
		   ret = newret;
		   retlen = newlen;
	      }
	      ret->array[retct++] = tostringn(buf,bufct);
	 }
	 /* if the matched part was empty, move onward a bit */
	 if (match_end(0) == match_start(0)) {
	      if (offset == textlen) break;
	      offset += 1;
	 }
    }
    ret->len = retct;
    return ret;
  }
}

bool gotArg(char *arg, char **argv) {
  /* used in M2lib.c, but we put them here to prevent it from being optimized away: */
  for (; *argv; argv++) if (0 == strcmp(arg,*argv)) return TRUE;
  return FALSE;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
// End:
*/
