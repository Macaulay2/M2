/*		Copyright 1994 by Daniel R. Grayson		*/

#include "types.h"

#include "../c/compat.c"
#include <dirent.h>

extern void outofmem();

#if 0
char *GC_malloc_clear(n)
unsigned int n;
{
  char *p = GC_MALLOC(n);
  if (p == NULL) outofmem();
  memset(p,0,n);
  return p;
}
#endif

void trap(){}

void
#if defined(__STDC__) || defined(_WIN32) && !defined(__CYGWIN32__)
fatal(char *s,...)   {
     va_list ap;
#else
fatal( va_alist  ) 
va_dcl
{
     va_list ap;
     char *s;
#endif
#if defined(__STDC__) || defined(_WIN32) && !defined(__CYGWIN32__)
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

int system_getpid() {
  return getpid();
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
#if defined(__BUILDING_MPW__)
     int fd = open(fname, O_WRONLY);
#else
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_TRUNC
#ifndef __MWERKS__
	  , 0644
#endif
	  );
#endif
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

M2_string system_tostring(char const *s)
{
     int n = strlen(s);
     M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
     p->len = n;
     memcpy(p->array,s,n);
     return p;
     }

M2_string system_tostringn(char const *s, int n)
{
     M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
     p->len = n;
     memcpy(p->array,s,n);
     return p;
     }

int actors5_sizeofDouble() { return sizeof(double); }

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
  return system_tostring(t);
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
#if defined(__MWERKS__) && !defined(__BUILDING_MPW__)
     strcat(buf,":");
#else
     if (0 != strcmp(buf,"/")) strcat(buf,"/");
#endif
     if (x != NULL) return system_tostring(x);
     return system_tostring("");
     }

M2_string system_getenv(s)
M2_string s;
{
     char *ss = tocharstar(s);
     char *x = getenv(ss);
     GC_FREE(ss);
     if (x == NULL) return system_tostring("");
     else return system_tostring(x);
     }


int system_strcmp(s,t)
M2_string s,t;
{
  int slen = s->len, tlen = t->len, i;
  int ret = 0;
  int len = slen < tlen ? slen : tlen;
  for (i=0; i<len; i++) {
    unsigned char c = s->array[i];
    unsigned char d = t->array[i];
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
  if (ret != 0) return ret;
  if (slen > tlen) return 1;
  if (slen < tlen) return -1;
  return 0;
}

int system_wait(pid)
int pid;
{
#if defined(__MWERKS__)
     return ERROR;
#else
     int status;
     int ret = waitpid(pid,&status,0);
     if (ret == ERROR) return ERROR;
     return status>>8;
#endif
     }

M2_arrayint system_select(M2_arrayint v) {
#if defined(__MWERKS__)
  M2_arrayint z;
  z = (M2_arrayint)getmem_atomic(sizeofarray(z,0));
  return z;
#elif defined(_WIN32)
  return ERROR;
#else
  static fd_set r, w, e;
  int n = v->len;
  int *s = v->array;
  int i, j, max = 0, m;
  M2_arrayint z;
  if (n == 0) return v;
  for (i=0; i<n; i++) if (s[i] > max) max = s[i];
  for (i=0; i<n; i++) FD_SET(s[i], &r);
  m = select(max+1,&r,&w,&e,NULL);
  z = (M2_arrayint)getmem_atomic(sizeofarray(z,m));
  z->len = m;
  for (i=j=0; i<n && j<m; i++) if (FD_ISSET(s[i],&r)) { z->array[j++] = i; FD_CLR(s[i],&r); }
  return z;
#endif
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
      s = system_tostring("");
      break;
    }
    if (r < size) {
      s = system_tostringn(buf,r);
      break;
    }
    size *= 2;			/* r == size, try again */
  }
  GC_FREE(fn);
  return s;
}

M2_string system_realpath(M2_string filename) {
  char *fn = tocharstar(filename);
  char buf[PATH_MAX];
  char *r = realpath(fn,buf);
  GC_FREE(fn);
  return r == NULL ? filename : system_tostring(buf);
}

M2_string system_errfmt(M2_string filename, int lineno, int colno) {
	char *s = getmem_atomic(filename->len+strlen(posfmt)+10);
	char *fn = tocharstar(filename);
	M2_string ret;
	sprintf(s,posfmt,fn,lineno,colno);
	ret = system_tostring(s);
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
  if (start > 0 && rl_line_buffer[start-1] == '"') return NULL;	/* force filename completion */
  return rl_completion_matches(text, M2_completion_generator);
}


void init_readline_variables() {
  extern char *_rl_comment_begin;
  _rl_comment_begin = "-- ";
  rl_readline_name = "M2";
  rl_attempted_completion_function = M2_completion;
  rl_basic_word_break_characters = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r";
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

M2_stringarray system_history() {
  M2_stringarray a;
  HIST_ENTRY **h = history_list();
  int i,n;
  for (n=0; h[n]!=NULL; n++);
  a = (M2_stringarray) getmem (sizeofarray(a,n));
  a->len = n;
  for (i=0; i<n; i++) a->array[i] = tostring(h[i]->line);
  return a;
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

int system_fileExists(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r = lstat(cname,&buf);
  GC_FREE(cname);
  return r != ERROR;
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

int system_fileTime(M2_string name) {
  char *cname = tocharstar(name);
  struct stat buf;
  int r;
  r = lstat(cname,&buf);
  GC_FREE(cname);
  if (r == ERROR) return -1;
  return buf.st_mtime;
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
	  if (t == NULL) {
	    /* errno = ENXIO; */
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
#if HAVE_SOCKETS
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

int system_acceptBlocking(int so) {
#if HAVE_SOCKETS
  struct sockaddr_in addr;
  int addrlen = sizeof addr;
  fcntl(so,F_SETFL,0);
  return accept(so,(struct sockaddr*)&addr,&addrlen);
#else
  return ERROR;
#endif
}

int system_acceptNonblocking(int so) {
#if HAVE_SOCKETS
  struct sockaddr_in addr;
  int addrlen = sizeof addr;
  int sd;
  fcntl(so,F_SETFL,O_NONBLOCK);
  sd = accept(so,(struct sockaddr*)&addr,&addrlen);
  return sd;
#else
  return ERROR;
#endif
}

int openlistener(char *serv) {
#if HAVE_SOCKETS
  int sa = serv_address(serv);
  int so = socket(AF_INET,SOCK_STREAM,0);
  struct sockaddr_in addr;
  addr.sin_family = PF_INET;
  addr.sin_port = sa;
  addr.sin_addr.s_addr = INADDR_ANY;
  if (ERROR == so ||
      ERROR == sa ||
      ERROR == bind(so,(struct sockaddr*)&addr,sizeof addr) ||
      ERROR == listen(so,
		      10	/* length of queue of pending connections */
		      )) { close(so); return ERROR; }
  return so;
#else
  return ERROR;
#endif
}

int opensocket(char *host, char *serv) {
#if HAVE_SOCKETS
  int sd = socket(AF_INET,SOCK_STREAM,0);
  struct sockaddr_in addr;
  int sa = serv_address(serv);
  addr.sin_family = PF_INET;
  addr.sin_port = sa;
  addr.sin_addr.s_addr = host_address(host);
  if (ERROR == addr.sin_addr.s_addr ||
      ERROR == sa ||
      ERROR == connect(sd,(struct sockaddr *)&addr,sizeof(addr))) { close(sd); return ERROR; }
  return sd;
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

#ifdef __CYGWIN__
#define NO_HERROR
#endif

#ifndef NO_HERROR
extern int h_nerr;
#ifndef H_ERRLIST_IS_DECLARED
extern const char * const h_errlist[];
#endif
#endif

#ifndef HAVE_DECL_SYS_ERRLIST
extern const char * const sys_errlist[];
#endif

int system_errno() {
  return 
#ifndef NO_HERROR
    h_errno > 0 ? h_errno : 
#endif
    errno;
}

char const *system_strerror() {
     char const *msg =
#ifndef NO_HERROR
     h_errno > 0 && h_errno < h_nerr ? h_errlist[h_errno] : 
#endif
#if !defined(__MWERKS__) && !defined(__CYGWIN__)
#if 0
       /* old way: sys_errlist is deprecated */
     errno > 0 && errno < sys_nerr ? sys_errlist[errno] :
#else
       /* to be thread safe, use strerror_r instead */
     errno > 0 ? strerror(errno) :
#endif
#endif
     "no system error (scclib.c)";
#ifndef NO_HERROR
     h_errno = 0;
#endif
     errno = 0;
     return msg;
}

M2_string system_syserrmsg()
{
#if defined(__MWERKS__)
     return system_tostring("");
#else
     return system_tostring(system_strerror());
#endif
}

int system_run(M2_string command){
     char *c = tocharstar(command);
     int r = system(c);
     GC_FREE(c);
     return r >> 8;
     }

struct FINAL *final_list, *pre_final_list;

void system_atend(void (*f)()){
     struct FINAL *this_final = (struct FINAL *)getmem(sizeof(struct FINAL));
     this_final -> final = f;
     this_final -> next = pre_final_list;
     pre_final_list = this_final;
     }

#if defined(__MWERKS__) && defined(__BUILDING_MPW__)
/* There appears to be a bug in the open/close routines when linking with MPW stuff.
   So here we define these routines in a somewhat different way.
 */

int M2open(char *name, int flags)
{
    FILE *fil;
//    fprintf(stderr, "about to open file %s...", name);
    if ((flags & O_RDONLY) != 0) {
    	fil = fopen(name,"rb");
    } else {
    	fil = fopen(name, "wb");
    }
//    fprintf(stderr, "returned %d\n", (int)fil);
    if (fil == NULL)
        return -1; // ERROR, stdio_ERROR
    return (int)fil;	
}

static int prev_putc = '\0';
void M2putc(int c, FILE *fil)
{
  if (c == '\r') {
    if (prev_putc != '\n') 
      putc('\n',fil);
  } else 
    putc(c,fil);
  prev_putc = c;
}

int M2read(int fd, char *buffer, int len)
{
    char * result;
    switch (fd) {
    case 0:
    	result = fgets(buffer, len, stdin);
    	return strlen(result);
    case 1:
    	return fread(buffer, 1, len, stdout);
    case 2:
    	return fread(buffer, 1, len, stderr);
    default:
        return fread(buffer, 1, len, (FILE *)fd);
    }
}
int M2write(int fd, char *buffer, int len)
{
    int i, ret;
    switch (fd) {
    case 0:
    	fprintf(stderr, "error: attempt to write to stdin\n");
    	return fwrite(buffer, 1, len, stdin);
    case 1:
//    	return fprintf(stdout, buffer);
	//for (i=0; i<len; i++) M2putc(buffer[i], stdout);
	//return len;
	for (i=0; i<len; i++) 
	  if (buffer[i] == '\r') 
	    buffer[i] = '\n';
    	ret = fwrite(buffer, 1, len, stdout);
    	return ret;
    case 2:
//  	return fprintf(stderr, buffer);
	//for (i=0; i<len; i++) M2putc(buffer[i], stderr);
	//return len;
	for (i=0; i<len; i++) 
	  if (buffer[i] == '\r') 
	    buffer[i] = '\n'; 
    	ret = fwrite(buffer, 1, len, stderr);
    	return ret;
    default:
        return fwrite(buffer, 1, len, (FILE *)fd);
    }
}
int M2close(int fd)
{
    if (fd >= 0 && fd <= 2) return 0;
    return fclose((FILE *)fd);
}

int M2lseek(int fd, long offset, int whence)
{
    int ret = fseek((FILE *)fd, offset, whence);
    if (ret == -1) return -1;
    return ftell((FILE *)fd);
}

#endif

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

/* these next three functions are simply aliases for libcf in case it was configured 
   with --with-memman-old.  The three functions are declared in libcfmem.a, but we don't
   want to use their memory manager.  We make our own call to mp_set_memory_functions() 
   in any case, see M2lib.c.
   As a side effect, linking with libcfmem.a will cause an error about a duplicate definition,
   which is good, since we don't want to link with libcfmem.a. */
void* getBlock ( size_t size ) {
  return GC_malloc1(size); }
void* reallocBlock ( void * block, size_t oldsize, size_t newsize ) {
  return GC_realloc3(block,oldsize,newsize); }
void freeBlock ( void * block, size_t size ) {
  return GC_free2(block, size);
}

#if 0

void *malloc1 (size_t size_in_bytes) {
     void *p;
     p = malloc(size_in_bytes);
     if (p == NULL) outofmem();
     return p;
     }

void *realloc3 (void *s, size_t old, size_t new) {
     void *p = realloc(s,new);
     if (p == NULL) outofmem();
     return p;
     }

void free2 (void *s, size_t old) {
     free(s);
     }

#endif

#if 0

/* This routine is obtained by merging the code from
   versions of it found in the source for the new libg++ 2.0.8 and the old libc 5.4.23.
   The crucial line missing from the modern code was the one that initialized _fileno. */

void _IO_init(register _IO_FILE *fp, int flags) {
  fp->_flags = _IO_MAGIC|flags;
  fp->_IO_buf_base = NULL;
  fp->_IO_buf_end = NULL;
  fp->_IO_read_base = NULL;
  fp->_IO_read_ptr = NULL;
  fp->_IO_read_end = NULL;
  fp->_IO_write_base = NULL;
  fp->_IO_write_ptr = NULL;
  fp->_IO_write_end = NULL;
  fp->_chain = NULL; /* Not necessary. */
  fp->_IO_save_base = NULL;
  fp->_IO_backup_base = NULL;
  fp->_IO_save_end = NULL;
  fp->_markers = NULL;
  fp->_cur_column = 0;
  fp->_fileno = -1;
#ifdef _IO_MTSAFE_IO
  _IO_lock_init (*fp->_lock);
#endif
}
#endif

#if defined(HAVE_WORDEXP) && defined(HAVE_WORDEXP_H)
#include <wordexp.h>		/* gnu c library word expansion */
#endif

M2_stringarray system_wordexp(M2_string s) {
#if defined(HAVE_WORDEXP) && defined(HAVE_WORDEXP_H)
  wordexp_t buf;
  M2_stringarray val ;
  char *words = tocharstar(s);
  int ret = wordexp(words,&buf,WRDE_SHOWERR); /* warning : can execute commands, but there is a flag to prevent it */
  GC_FREE(words);
  if (ret != 0) return NULL;
  val = tostrings(buf.we_wordc,buf.we_wordv);
  wordfree(&buf);
  return val;
#else
  fprintf(stderr,"warning: wordexp() not installed on your system\n");
  return NULL;
#endif
}

int system_strncmp(M2_string s,M2_string t,int n) {
  return strncmp(s->array,t->array,n);
}

#include <regex.h>

M2_arrayint system_regexmatch(M2_string pattern, M2_string text) {
  static M2_string last_pattern = NULL;
  static regex_t regex;
  static struct M2_arrayint_struct empty[1] = {{0}};
  int ret;
  if (last_pattern != pattern) {
    char *s_pattern;
    if (last_pattern != NULL) regfree(&regex), last_pattern = NULL;
    s_pattern = tocharstar(pattern);
    ret = regcomp(&regex, s_pattern, REG_NEWLINE|REG_EXTENDED);
    GC_FREE(s_pattern);
    if (ret != 0) { regfree(&regex); return empty; }
    last_pattern = pattern;
  }
  {
    int n = regex.re_nsub+1;
    regmatch_t match[n];
    char *s_text = tocharstar(text);
    ret = regexec(&regex, s_text, n, match, 0);
    GC_FREE(s_text);
    if (ret != 0) return empty;
    else {
      M2_arrayint m = makearrayint(2*n);
      int i;
      for (i = 0; i<n; i++) {
	m->array[2*i  ] = match[i].rm_so;
	m->array[2*i+1] = match[i].rm_eo-match[i].rm_so;
      }
      return m;
    }
  }
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
// End:
*/
