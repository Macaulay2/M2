/*		Copyright 1994 by Daniel R. Grayson		*/

#include "types.h"

#ifdef __MWERKS__
#include ".._c_compat.c"
#else
#include "../c/compat.c"
#endif

char newline[] = NEWLINE;

bool system_gc = GaCo;

extern void outofmem();

char *getmem(n)
unsigned int n;
{
     char *p;
     p = GC_malloc_ignore_off_page(n);
     if (p == NULL) outofmem();
     return p;
     }

char *GC_malloc_clear(n)
unsigned int n;
{
  char *p = GC_malloc_ignore_off_page(n);
  if (p == NULL) outofmem();
  memset(p,0,n);
  return p;
}

char *getmem_atomic(n)
unsigned int n;
{
     char *p;
     p = GC_malloc_atomic_ignore_off_page(n);
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

struct M2_string_struct system_newline_contents = { 1, { '\n' } };
M2_string system_newline = &system_newline_contents;

char *tocharstar(s)
M2_string s;
{
     char *p = getmem_atomic(s->len + 1 + sizeof(int));
     memcpy(p,s->array,s->len);
     p[s->len] = 0;
     return p;
     }

char **tocharstarstar(p)
stringarray p;
{
     char **s = (char **)getmem((1 + p->len)*sizeof(char *));
     unsigned int i;
     for (i=0; i<p->len; i++) s[i] = tocharstar(p->array[i]);
     s[i] = NULL;
     return s;
     }

int system_openin(filename)
M2_string filename;
{
     char *fname = tocharstar(filename);
     int fd;
     fd = open(fname, O_BINARY | O_RDONLY);
     GC_free(fname);
     return fd;
     }

int system_openout(filename)
M2_string filename;
{
     char *fname = tocharstar(filename);
     int fd = open(fname, O_BINARY | O_CREAT | O_WRONLY | O_TRUNC
#ifndef __MWERKS__
	  , 0644
#endif
	  );
     GC_free(fname);
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
arrayint fildes;
{
     return pipe(fildes->array);
     }

int system_exec(argv)
stringarray argv;
{
     int i;
     char **av = tocharstarstar(argv);
     execvp(av[0],av);
     for (i=0; i<(int)argv->len; i++) {
     	  GC_free(av[i]);
	  }
     GC_free(av);
     return ERROR;
     }

M2_string tostring(s)
char *s;
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

arrayint toarrayint(n,p)
int n;
int *p;
{
     arrayint z = (arrayint)getmem_atomic(sizeofarray(z,n));
     z->len = n;
     memcpy(z->array,p,n * sizeof(int));
     return z;
     }

stringarray tostrings(n,s)
int n;
char **s;
{
     int i;
     stringarray a;
     a = (stringarray) getmem (sizeofarray(a,n));
     a->len = n;
     for (i=0; i<n; i++) a->array[i] = tostring(s[i]);
     return a;
     }

M2_string system_getcwd()
{
     char buf[700];
     char *x = getcwd(buf,sizeof(buf));
     if (x != NULL) return tostring(x);
     return tostring("");
     }

M2_string system_getenv(s)
M2_string s;
{
     char *ss = tocharstar(s);
     char *x = getenv(ss);
     GC_free(ss);
     if (x == NULL) return tostring("");
     else return tostring(x);
     }


int system_strcmp(s,t)
M2_string s,t;
{
     char *ss = tocharstar(s);
     char *tt = tocharstar(t);
     int r = strcmp(ss,tt);
     GC_free(ss);
     GC_free(tt);
     return r;
     }

int system_wait(pid)
int pid;
{
     int status;
     while (pid != wait(&status));
     return status>>8;
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
	GC_free(s);
	GC_free(fn);
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

M2_string stdio_readfile(fd)
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
		 if (errno == EINTR) continue;
#endif
		 fatal("can't read file descriptor %d", fd);
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
	  return t == NULL ? ERROR : *(int *)t->h_addr;
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
	  return t == NULL ? ERROR : t->s_port;
	  }
#else
     return ERROR;
#endif
     }

int opensocket(char *host, char *serv) {
#if HAVE_SOCKETS
     int sd = socket(AF_INET,SOCK_STREAM,0);
     int ha;
     struct sockaddr_in addr;
     addr.sin_family = PF_INET;
     ha = host_address(host);
     if (ha == ERROR) {
          close(sd);
	  return ERROR;
          }
     addr.sin_addr.s_addr = ha;
     addr.sin_port = serv_address(serv);
     if (ERROR == connect(sd,(struct sockaddr *)&addr,sizeof(addr))) {
	  close(sd);
	  return ERROR;
	  }
     else return sd;
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
     GC_free(Host);
     GC_free(Serv);
     return sd;
     }

M2_string system_syserrmsg()
{
     extern int errno, sys_nerr;
     extern char * sys_errlist[];
     return (
	  errno == 0 && errno < sys_nerr 
	  ? tostring("") 
	  : tostring(sys_errlist[errno])
	  );
     }

int system_run(M2_string command){
     char *c = tocharstar(command);
     int r = system(c);
     GC_free(c);
     return r >> 8;
     }

struct FINAL *final_list, *pre_final_list;

void system_atend(void (*f)()){
     struct FINAL *this_final = (struct FINAL *)getmem(sizeof(struct FINAL));
     this_final -> final = f;
     this_final -> next = pre_final_list;
     pre_final_list = this_final;
     }

#ifdef GCMALLOC

void *GC_malloc1 (size_t size_in_bytes) {
     void *p;
     p = GC_malloc_uncollectable(size_in_bytes);
     if (p == NULL) outofmem();
     return p;
     }

void *GC_realloc3 (void *s, size_t old, size_t new) {
     void *p = GC_realloc(s,new);
     if (p == NULL) outofmem();
     return p;
     }

void GC_free2 (void *s, size_t old) {
     GC_free(s);
     }

#else

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
