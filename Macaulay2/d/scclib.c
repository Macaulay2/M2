/*		Copyright 1994 by Daniel R. Grayson		*/

#include "types.h"

#include "../c/compat.c"

char newline[] = NEWLINE;

extern void outofmem();

char *getmem(n)
unsigned int n;
{
     char *p;
     p = GC_MALLOC(n);
     if (p == NULL) outofmem();
     return p;
     }

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

char *getmem_atomic(n)
unsigned int n;
{
     char *p;
     p = GC_MALLOC_ATOMIC(n);
     if (p == NULL) outofmem();
     return p;
     }

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

static struct M2_string_struct system_newline_contents = { 1, { '\n' } };
M2_string system_newline = &system_newline_contents;

char *tocharstar(s)
M2_string s;
{
     char *p = getmem_atomic(s->len + 1);
     memcpy(p,s->array,s->len);
     p[s->len] = 0;
     return p;
     }

char **tocharstarstar(p)
M2_stringarray p;
{
     int n = p->len;
     char **s = (char **)getmem((n + 1)*sizeof(char *));
     unsigned int i;
     for (i=0; i<n; i++) s[i] = tocharstar(p->array[i]);
     s[n] = NULL;
     return s;
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

M2_string strings_substr(x,start,len)
M2_string x;
int start;
int len;
{
     M2_string p;
     if (start < 0) start = 0;
     if (start + len > (int)x->len) len = x->len - start;
     if (len < 0) len = 0;
     p = (M2_string) getmem_atomic(sizeofarray(p,len));
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

M2_string tostring(char const *s)
{
     int n = strlen(s);
     M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
     p->len = n;
     memcpy(p->array,s,n);
     return p;
     }

M2_string tostringn(s,n)
char *s;
int n;
{
     M2_string p = (M2_string)getmem_atomic(sizeofarray(p,n));
     p->len = n;
     memcpy(p->array,s,n);
     return p;
     }

M2_arrayint toarrayint(n,p)
int n;
int *p;
{
     M2_arrayint z = (M2_arrayint)getmem_atomic(sizeofarray(z,n));
     z->len = n;
     memcpy(z->array,p,n * sizeof(int));
     return z;
     }

M2_stringarray tostrings(n,s)
int n;
char **s;
{
     int i;
     M2_stringarray a;
     a = (M2_stringarray) getmem (sizeofarray(a,n));
     a->len = n;
     for (i=0; i<n; i++) a->array[i] = tostring(s[i]);
     return a;
     }

M2_string interp_dirname(M2_string s) {
  char *t = tocharstar(s);
  char *u = t;
  char *v = u;
  for (; *u; u++) if (*u == '/') v=u+1;	/* on MacOS?? */
  if (v != NULL) *v = '\0';
  return tostring(t);
}

M2_string system_getcwd()
{
     char buf[700];
     char *x = getcwd(buf,sizeof(buf));
#if defined(_WIN32)
     char *p;
     for (p=x; *p; p++) if (*p == '\\') *p = '/';
#endif
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

M2_string system_errfmt(M2_string filename, int lineno, int colno) {
	char *s = getmem_atomic(filename->len+strlen(posfmt)+10);
	char *fn = tocharstar(filename);
	M2_string ret;
	sprintf(s,posfmt,fn,lineno,colno);
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
     return read(fd,buffer->array+offset,len);
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

int system_fileLength(int fd) {
  struct stat statbuf;
  if (ERROR == fstat(fd,&statbuf)) return ERROR;
  return statbuf.st_size;
}

M2_string system_readfile(fd)
int fd;
{
     M2_string s;
     unsigned int filesize;
     struct stat buf;
     if (ERROR == fstat(fd,&buf) || !S_ISREG(buf.st_mode)) {
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

M2_string strings_join(x,y)
M2_string x;
M2_string y;
{
     M2_string p;
     p = (M2_string) getmem_atomic(sizeofarray(p,x->len+y->len));
     p->len = x->len + y->len;
     memcpy(p->array,x->array,x->len);
     memcpy(p->array+x->len,y->array,y->len);
     return p;
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

extern int errno, sys_nerr;

#ifndef NO_HERROR
extern int h_nerr;
#ifndef H_ERRLIST_IS_DECLARED
extern const char * const h_errlist[];
#endif
#endif

#ifndef SYS_ERRLIST_IS_DECLARED
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
#ifndef __MWERKS__
     errno > 0 && errno < sys_nerr ? sys_errlist[errno] :
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
     return tostring("");
#else
     return tostring(system_strerror());
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
