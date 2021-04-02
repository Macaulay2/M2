/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

struct CURRENT cur;

int tabwidth = 8;

void advance(){
     char c = *cur.text++;
     switch (c) {
	  case '\n': {
	       cur.lineno++; 
	       cur.column = 0;
     	       break;
	       }
	  case '\t': {
	       cur.column = roundup(cur.column+1,tabwidth); 
	       break;
	       }
	  default: {
	       cur.column++;
	       }
	  }
     }

static node DoubleN(char *s, unsigned int len) {
     node q = newnode(DOUBLE_CONST,double_const_tag);
     q->body.double_const.contents = strnperm(s,len);
     return q;
     }

bool iswhite(int c) {
     return (c == ' ' || c == '\n' || c == 0
	  || c == '\t' || c == '\r' || c == '\f');
     }

static bool istokenfirst(int c) {
     return isalpha(c) || c=='_';
     }

static bool istoken(int c) {
     return isalnum(c) || c=='_';
     }

bool validtoken(char *s) {
     if (!istokenfirst(*s++)) return FALSE;
     while (*s != 0) if (!istoken(*s++)) return FALSE;
     return TRUE;
     }

static bool alldigits(char *s, unsigned int n){
     while (n > 0) {
	  if (!isdigit((int)*s)) return FALSE;
	  s++;
	  n--;
	  }
     return TRUE;
     }

static bool allhexdigits(char *s, unsigned int n){
     while (n > 0) {
	  if (isdigit((int)*s) 
	       || ('a' <= *s && *s <= 'f') 
	       || ('A' <= *s && *s <= 'F')
	       ) {
	       s++;
	       n--;
	       }
	  else return FALSE;
	  }
     return TRUE;
     }

static bool integerq(char *p, unsigned int n) {
     if (n==0) return FALSE;
     if (p[0] == '0' && p[1] == 'x') {
	  p+=2, n-=2;
	  if (n==0) return FALSE;
	  return allhexdigits(p,n);
	  }
     else {
	  if (*p == '-') p++, n--;
	  if (n==0) return FALSE;
	  return alldigits(p,n);
	  }
     }

static bool doubleq(char *p, unsigned int n) {
     if (n == 0) return FALSE;
     if (*p == '-') p++, n--;
     /* if (!isdigit(*p)) return FALSE; */
     while (TRUE) {
	  if (n == 0) return FALSE;
	  if (*p == '.') {
	       p++, n--;
	       break;
	       }
	  if (!isdigit((int)*p)) return FALSE;
	  p++, n--;
	  }
     while (TRUE) {
	  if (n==0) return TRUE;
	  if (!isdigit((int)*p)) return FALSE;
	  p++, n--;
	  }
     }

static char escaped(char c){
     switch(c){
     	  case 'b': return '\b';
     	  case 't': return '\t';
	  case 'n': return '\n';
	  case 'f': return '\f';
	  case 'r': return '\r';
	  default : return c;
	  }
     }

node gettoken(){
     char *p = cur.text;
     if (*p == '\'') {
	  char c;
	  node token;
	  advance();
	  if (cur.text == cur.eot) {
	       fatal("file ends in character constant");
	       }
	  if (*cur.text == '\\') {
	       advance();
	       if (cur.text == cur.eot) {
		    fatal("file ends in character constant");
		    }
	       c = escaped(*cur.text);
	       }
	  else c = *cur.text;
	  advance();
	  if (cur.text == cur.eot) fatal("file ends in character constant");
	  if (*cur.text != '\'') {
	       fatal("character constant not terminated");
	       }
	  advance();
	  token = newnode (CHAR_CONST,char_const_tag);
	  token->body.char_const.contents = c;
	  return token;
	  }
     if (*p == '"') {
	  char *s;
	  node token;
	  advance();
	  s = cur.text;
	  while (TRUE) {
	       if (cur.text == cur.eot) fatal("file ends before string");
	       if (cur.text[0]=='"') break;
	       if (*cur.text == '\\') {
		    advance();
		    if (cur.text == cur.eot) fatal("file ends before string");
		    }
	       advance();
	       }
	  token = newnode(STRING_CONST,string_const_tag);
	  token->body.string_const.characters = strnperm(s,cur.text-s); /* observe that escaped character sequences such as \n are not simplified here */
	  advance();
	  return token;
	  }
     if (*p == ',' || *p == '.') {
	  advance();
	  }
     else {
	  bool digitssofar = TRUE;
	  while (TRUE) {
	       if (!isdigit((int)*cur.text)) digitssofar = FALSE;
     	       advance();
	       if (cur.text == cur.eot) break;
	       if (*cur.text == '.' && digitssofar) continue;
	       if (istoken(*cur.text)) continue;
	       break;
	       }
	  if (integerq(p,cur.text-p)) {
	       return IntegerN(p,cur.text-p);
	       }
	  if (doubleq(p,cur.text-p)) {
	       return DoubleN(p,cur.text-p);
	       }
	  }
     return UniqueStringN(p,cur.text-p);
     }

/* keep the next lines together */
     char setup_filename[] = __FILE__;
     int  setup_lineno     = __LINE__;
     char *setup_text[]     = {"\n\
	leftOperator 3 \"^^\";      ","\n\
	leftOperator 3 \"<<\";      ","\n\
	leftOperator 3 \"|\";      ","\n\
	rightOperator 3 \">>\";      ","\n\
	leftOperator 4 \"&\";      ","\n\
	leftOperator 5 \"<=\";      ","\n\
	leftOperator 5 \"<\";      ","\n\
	leftOperator 5 \"===\";      ","\n\
	leftOperator 5 \"=>\";      ","\n\
	leftOperator 5 \"=!=\";      ","\n\
	leftOperator 5 \">=\";      ","\n\
	leftOperator 5 \">\";      ","\n\
	prefixOperator 5 \"~\";      ","\n\
	leftOperator 6 \"+\";      ","\n\
	leftOperator 7 \"//\";      ","\n\
	leftOperator 7 \"/\";      ","\n\
	leftOperator 7 \"*\";      ","\n\
	leftOperator 7 \"%\";      ","\n\
	leftOperator 8 \"^\";      ","\n\
	leftOperator 10 \".\";      ","\n\
	rightOperator 1 \"::=\";      ","\n\
	rightOperator 1 \":=\";      ","\n\
	leftOperator 2 \"=\";      ","\n\
	prefixOperator 4 \"!\";      ","\n\
	leftOperator 5 \"==\";      ","\n\
	leftOperator 5 \"!=\";      ","\n\
        " };

void read_setup(){
     node e;
     unsigned int i;
     struct CURRENT save;
     if (nomacros) return;
     save = cur;
     cur.filename = setup_filename;
     cur.lineno = setup_lineno + 1;
     cur.column = 0;
     for (i=0; i<numberof(setup_text); i++) {
	  int r;
	  char *s = setup_text[i];
	  cur.text = s;
	  cur.eot = cur.text + strlen(s);
     	  r = yyparse();
	  if (r == 1) fatal("terminating due to syntax errors");
     	  e = chklist(parservalue,global_scope);
     	  assertpos(e==NULL,e);
	  }
     cur = save;
     }

static char *readfile2(int fd, int *plen) {
     int bufsize = 1024 * 16;
     int len = 0;
     char *txt = getmem(bufsize);
     while (TRUE) {
	  int n = read(fd,txt+len,bufsize-len);
	  if (ERROR == n) fatal("error reading file %s", cur.filename);
	  if (0 == n) {
	       if (ERROR == close(fd)) {
	       	    fatal("can't close file %s", cur.filename);
		    }
	       break;
	       }
	  len += n;
	  if (len == bufsize) {
	       char *p = GC_MALLOC(bufsize *= 2);
	       memcpy(p,txt,len);
	       txt = p;
	       }
	  }
     *plen = len;
     return txt;
     }

node readfile(char *filename) {
     int size = 0;
     int fd, r;
     struct CURRENT save;
     save = cur;
     cur.filename = filename;
     fd = open(cur.filename,O_RDONLY);
     if (ERROR == fd) fatal("can't open file %s",cur.filename);
     cur.text = readfile2(fd,&size);
     cur.eot = cur.text+size, cur.lineno=1, cur.column=0;
     cur.wrapit = TRUE;
     r = yyparse();
     if (r == 1) fatal("terminating due to syntax errors");
     cur = save;
     return parservalue;
     }

static int pathopen(const char *path, const char *filename, char **pathopened){
     /* pathlist is a colon separated list */
     const char *eop;
     int f;
     char buf[300], *p;
     if (path != NULL) {
	  while (*path) {
	       for (eop = path; *eop && *eop != ':'; eop++) ;
	       p = buf;
	       if (eop > path) {
		    strncpy(p,path,eop-path), p += eop-path;
		    *p++ = '/';
		    }
	       strcpy(p,filename);
	       f = open(buf,O_RDONLY);
	       if (f != ERROR) {
		    p = buf;
		    if (p[0]=='.' && p[1]=='/') p += 2;
		    *pathopened = cur.filename = strperm(p);
		    return f;
		    }
	       path = eop;
	       if (*path == ':') path++;
	       }
     	  return ERROR;
	  }
     else {
	  f = open(filename,O_RDONLY);
	  if (f != ERROR) {
	       *pathopened = cur.filename = strperm(filename);
	       return f;
	       }
     	  return ERROR;
	  }
     }

char *sigpath =".";

bool sigreadfile(char *name, char **pathopened) {
     node e, sig;
     int size;
     char buf[100];
     int r, fd;
     struct CURRENT save;
     save = cur;
     strcpy(buf,name);
     strcat(buf,".sig");
     fd = pathopen(sigpath,buf,pathopened);
     if (fd == ERROR) return FALSE;
     cur.text = readfile2(fd,&size);
     cur.eot = cur.text+size, cur.lineno=1, cur.column=0;
     cur.wrapit = FALSE;
     r = yyparse();
     if (r == 1) fatal("terminating due to syntax errors");
     cur = save;
     sig = global_scope->signature;
     e = chklist(parservalue,global_scope);
     global_scope->signature = sig;
     assertpos(e == NULL,e);
     return TRUE;
     }     

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/

