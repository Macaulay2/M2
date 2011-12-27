#include <expr-exports.h>
#include "platform.h"
#include "M2mem.h"
#include <readline/readline.h>
#include <readline/history.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
static char *M2_completion_generator(const char *text, int state) {
  static int i;
  static char **v;
  char *p;
  if (state == 0) {
    M2_string s;
    M2_ArrayString ret;
    i = 0;
#ifdef free
#warning "'free' defined as macro, but we want to use the libc function"
#define free x
#endif
    if (v != NULL) free(v);
    s = M2_tostring(text);
    ret = expr_completions(s);
    GC_FREE(s);
    v = M2_tocharstarstarmalloc(ret); /* readline will use free() to free these strings */
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
  extern const char *_rl_comment_begin;
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
    reading_from_readline = TRUE; /* for the interrupt handler */
    p = readline(prompt);
    reading_from_readline = FALSE;
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
  char *p = M2_tocharstar(prompt);
  int r;
  if (offset < 0 || (int)buffer->len - offset < len) fatalarrayindex(len,buffer->len,__FILE__,__LINE__,-1);
  r = read_via_readline(buffer->array + offset,len,p);
  GC_FREE(p);
  return r;
}

M2_string system_readfile(int fd) {
     struct stat buf;
     size_t bufsize = 1024;
     char *text;
     size_t size = 0;
     if (!(-1 == fstat(fd,&buf) || !S_ISREG(buf.st_mode) || 0 == buf.st_size)) {
       off_t filesize = buf.st_size;
       off_t pos = lseek(fd,0,SEEK_CUR);
       if (pos != (off_t)(-1)) bufsize -= pos;
       bufsize = (size_t)filesize;
       if ((off_t)(bufsize) != filesize || bufsize > SSIZE_MAX) return NULL; /* file too big */
     }
     text = getmem_atomic(bufsize);
     while (TRUE) {
	  int n = read(fd,text+size,bufsize-size);
	  if (-1 == n) {
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
	       GC_FREE(text);
	       text = p;
	       }
	  }
     M2_string s = (M2_string)getmem_atomic(sizeofarray(s,size));
     s->len = size;
     memcpy(s->array,text,size);
     GC_FREE(text);
     return s;
}
