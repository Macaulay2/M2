#include <M2-exports.h>
#include "platform.h"
#include "M2mem.h"
#include "../regex/regex.h"
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

#define SYNTAX_FLAGS ((RE_SYNTAX_POSIX_EXTENDED | (ignorecase ? RE_ICASE : 0)) & ~RE_DOT_NEWLINE)

struct M2_string_struct noErrorMessage;
M2_string system_noErrorMessage = &noErrorMessage;
M2_string system_regexmatchErrorMessage = &noErrorMessage;

static M2_string last_pattern = NULL;

struct re_pattern_buffer regex_pattern;

#define match_num(match)      (match.num_regs-1)
#define match_start(match,i) match.start[i]
#define match_end(match,i)   match.end[i]
/* #define regexec_empty_return REG_NOMATCH */
#define re_search_empty_return (-1)
#define match_length(match,i) (match_end(match,i) - match_start(match,i))

M2_arrayint system_regexmatch(M2_string pattern, int start, int range, M2_string text, M2_bool ignorecase) {
  static struct M2_arrayint_struct empty[1] = {{0}};
  const char *regcomp_return;
  system_regexmatchErrorMessage = &noErrorMessage;
  if (! (0 <= start && start <= text->len)) return empty;
  re_set_syntax(SYNTAX_FLAGS);
  if (last_pattern != pattern) {
    if (last_pattern != NULL) regfree(&regex_pattern), last_pattern = NULL;
    regcomp_return = re_compile_pattern(pattern->array, pattern->len, &regex_pattern);
    if (regcomp_return != NULL) {
         system_regexmatchErrorMessage = M2_tostring(regcomp_return);
	 regfree(&regex_pattern);
	 return empty;
    }
    last_pattern = pattern;
  }
  {
    int regexec_return;
    static struct re_registers match;
    regexec_return = re_search(&regex_pattern, text->array, text->len, start, range, &match);
    if (regexec_return == re_search_empty_return) return empty;
    else {
      int n = match_num(match);
      M2_arrayint m = M2_makearrayint(2*n);
      int i;
      for (i = 0; i<n; i++) {
	m->array[2*i  ] = match_start(match,i);
	m->array[2*i+1] = match_length(match,i);
      }
      return m;
    }
  }
}

void grow(int *len, int off, char **str, int newlen) {
     int d = 2**len+1;
     if (newlen < d) newlen = d;
     *str = getmoremem_atomic(*str,*len,newlen);
     *len = newlen;
}

void cat(int *xlen, int *xoff, char **x, int ylen, char *y) {
     if (*xoff + ylen > *xlen) grow(xlen,*xoff,x,*xoff + ylen);
     memcpy(*x+*xoff,y,ylen);
     *xoff += ylen;
}

M2_string system_regexreplace(M2_string pattern, M2_string replacement, M2_string text, M2_string errorflag, M2_bool ignorecase) {
  const char *regcomp_return;
  system_regexmatchErrorMessage = &noErrorMessage;
  re_set_syntax(SYNTAX_FLAGS);
  if (last_pattern != pattern) {
    if (last_pattern != NULL) regfree(&regex_pattern), last_pattern = NULL;
    {
      regcomp_return = re_compile_pattern(pattern->array, pattern->len, &regex_pattern);
    }
    if (regcomp_return != NULL) {
         system_regexmatchErrorMessage = M2_tostring(regcomp_return);
	 return errorflag;
    }
    last_pattern = pattern;
  }
  {
    static struct re_registers match;
    int start = 0;
    int textlen = text->len;
    int buflen = text->len + 3 * replacement->len + 16;
    int bufct = 0;
    char *buf = getmem_atomic(buflen);
    int i;
    while (re_search(&regex_pattern, text->array, text->len, start, text->len - start, &match) != re_search_empty_return) {
         int n = match_num(match);
	 char *p;
	 int plen;
	 /* copy the unmatched text up to the match */
	 cat(&buflen,&bufct,&buf, match_start(match,0)-start,text->array+start);
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
	      if (0 <= i && i < n && i <= 9) cat(&buflen,&bufct,&buf,match_length(match,i),text->array+match_start(match,i));
	      p += 2;
	      plen -= 2;
	 }
	 cat(&buflen,&bufct,&buf,plen,p);
	 /* reset the start after the matched part */
	 start = match_end(match,0);
	 /* if the matched part was empty, move onward a bit */
	 if (match_end(match,0) == match_start(match,0)) {
	      if (start == textlen) break;
	      cat(&buflen,&bufct,&buf, 1, text->array+start);
	      start += 1;
	 }
    }
    /* copy the last part of the text */
    cat(&buflen,&bufct,&buf, textlen-start, text->array+start);
    return M2_tostringn(buf, bufct);
  }
}

M2_ArrayString system_regexselect(M2_string pattern, M2_string replacement, M2_string text, M2_ArrayString errorflag, M2_bool ignorecase) {
  const char *regcomp_return;
  system_regexmatchErrorMessage = &noErrorMessage;
  re_set_syntax(SYNTAX_FLAGS);
  if (last_pattern != pattern) {
    if (last_pattern != NULL) regfree(&regex_pattern), last_pattern = NULL;
    regcomp_return = re_compile_pattern(pattern->array, pattern->len, &regex_pattern);
    if (regcomp_return != NULL) {
         system_regexmatchErrorMessage = M2_tostring(regcomp_return);
	 return errorflag;
    }
    last_pattern = pattern;
  }
  {
    static struct re_registers match;
    int start = 0;
    int textlen = text->len;
    int buflen = 2 * replacement->len + 24;
    char *buf = getmem_atomic(buflen);
    int i;
    int retlen = 10, retct = 0;
    M2_ArrayString ret = (M2_ArrayString)getmem_atomic(sizeofarray(ret,retlen));
    while (re_search(&regex_pattern, text->array, text->len, start, text->len - start, &match) != re_search_empty_return) {
         int n = match_num(match);
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
	      if (0 <= i && i < n && i <= 9) cat(&buflen,&bufct,&buf,match_length(match,i),text->array+match_start(match,i));
	      p += 2;
	      plen -= 2;
	 }
	 cat(&buflen,&bufct,&buf,plen,p);
	 /* reset the start after the matched part */
	 start = match_end(match,0);
	 /* make an M2_string and append it to the return list */
	 {
	      if (retct == retlen) {
		   int newlen = 2 * retlen;
		   int k;
		   M2_ArrayString newret = (M2_ArrayString)getmem_atomic(sizeofarray(ret,newlen));
		   for (k=0; k<retlen; k++) newret->array[k] = ret->array[k];
		   GC_FREE(ret);
		   ret = newret;
		   retlen = newlen;
	      }
	      ret->array[retct++] = M2_tostringn(buf,bufct);
	 }
	 /* if the matched part was empty, move onward a bit */
	 if (match_end(match,0) == match_start(match,0)) {
	      if (start == textlen) break;
	      start += 1;
	 }
    }
    ret->len = retct;
    return ret;
  }
}
