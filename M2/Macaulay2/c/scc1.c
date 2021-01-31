/*		Copyright 1993,2010 by Daniel R. Grayson		*/

#include "scc.h"

scope global_scope;
FILE *dependfile;
char *targetname;
char *outfilename;
static bool stop_after_dep = FALSE;
bool do_cxx = FALSE;
bool do_this_cxx = FALSE;
bool noline = FALSE;
bool nomacros = FALSE;
bool arraychks = TRUE;
bool casechks = TRUE;
bool compilerThreadLocal = FALSE;
bool pthreadThreadLocal = TRUE;


static char Copyright[] = "Copyright 1993, 2010, by Daniel R. Grayson";
static char Version[]   = "Safe C - version 2.0";

char *getmem(unsigned n) {
     char *p = GC_MALLOC(n);	/* GC_MALLOC clears the memory */
     if (p == NULL) fatal("out of memory");
     return p;
     }

static char *progname;

node newnode1(unsigned int len, enum TAG tag) {
     node p = (node) getmem(len);
     memset(p,0x00,len);
     p->tag = tag;
     return p;
     }

char *strnperm(const char *s, unsigned n){
     char *t = getmem(n+1);
     memcpy(t,s,n);
     t[n]='\0';
     return t;
     }

char *strperm(const char *s){
     return strnperm(s,strlen(s));
     }

char *intToString(int n){
     int sign = 1;
     static char s[20];
     int i;
     if (n == 0) return "0";
     if (n < 0) sign = -1, n = -n;
     i = 19;
     s[i--]=0;
     while (n>0) {
	  s[i] = (char)(n%10 + '0');
	  n/=10;
	  if (n==0) break;
	  i--;
	  }
     if (sign == -1) s[--i] = '-';
     return s+i;
     }

int substr(char *s, char *t){
     assert(s != NULL);
     assert(t != NULL);
     while (*s) {
	  if (*t != *s) return FALSE;
	  s++;
	  t++;
	  }
     return TRUE;
     }

int strequaln(char *s, char *t, unsigned int tlen){ /* s is null-terminated, but tlen is the length of t */
     assert(s != NULL);
     assert(t != NULL);
     while (*s && tlen>0) {
	  if (*t != *s) return FALSE;
	  s++;
	  t++, tlen--;
	  }
     return *s==0 && tlen==0;
     }

char *tail(char *s){
     char *u = NULL;
     for (; *s; s++) if (*s == '.') u = s;
     return u == NULL ? s : u;
     }

char *BaseName(char *s) {
     char *u = s;
     for (; *s; s++) if (*s == '/') u = s+1;
     return u;
     }

char *newsuffix(char *s, char *suf){
     char *t = tail(s);
     unsigned int len = t-s;
     char *u = getmem(len+1+strlen(suf));
     strncpy(u,s,len);
     strcpy(u+len,suf);
     return u;
     }

char *newsuffixbase(char *s, char *suf){
     char *t, *u;
     unsigned int len;
     s = BaseName(s);
     t = tail(s);
     len = t-s;
     u = getmem(len+1+strlen(suf));
     strncpy(u,s,len);
     strcpy(u+len,suf);
     return u;
     }

const struct POS empty_pos;

static char declarations_header[] = "\
/* included from " __FILE__ "*/\n\
\n\
#ifdef __cplusplus\n\
  #define BASECLASS : public our_new_delete\n\
  #include \"newdelete.hpp\"\n\
#else\n\
  #define BASECLASS\n\
#endif\n\
\n\
#if defined(__cplusplus)\n\
  extern \"C\" {\n\
#endif\n\
\n\
#ifndef SAFEC_EXPORTS\n\
#define SAFEC_EXPORTS\n\
#endif\n\
#ifndef SAFEC_basic_typedefs_defined\n\
#define SAFEC_basic_typedefs_defined\n\
typedef char M2_bool;\n\
struct tagged_union { unsigned short type_; };\n\
#undef M2_basic_typedefs_defined\n\
#endif\n\
\n\
";

static char declarations_trailer[] = "\
\n\
#if defined(__cplusplus)\n\
  }\n\
#endif\n\
\n\
";

static char code_header[] = "\
#include \"scc-core.h\"\n\
#include \"../system/supervisorinterface.h\"\n\
";

static void readsetup(scope v){
     init_dictionary(v);
     read_setup();
     }

static void usage() {
  printf("%s [options] foo.d ...\n",progname);
  printf("    --help        display this usage message\n");
  printf("    -cxx          generate a C++ file foo.cc instead of foo.c\n");
  printf("    -v            show version\n");
  printf("    -pthreadlocal use get/set specific instead of __thread\n");
  printf("    -dep          stop after creating foo.dep.tmp and foo.sig.tmp\n");
  printf("    -noline       insert no source code line numbers\n");
  printf("    -sig          stop after creating signature file foo.sig.tmp\n");
  printf("    -typecodes    print typecodes (from typecode.db), then stop\n");
  printf("    -nomacros     do not parse internal macro definitions\n");
  printf("    -noarraychks  insert no array bound checking code\n");
  printf("    -nocasechks   insert no type case checking code\n");
  printf("    -O            optimize: no array bound checking, no type case checking\n");
  printf("    -tabwidth N   set tab width (default 8; affects column number in error messages)\n");
  printf("    -yydebug      debug the parser\n");
  printf("    -debug        set debugging mode on, write symbol table, list of types, and list of strings to foo.sym\n");
  printf("    -Ixxx         append xxx to the path used for finding *.sig files, initially \".\"\n");
}

int main(int argc, char **argv){
     int i;
     char *p;
     GC_INIT();
     progname = BaseName(argv[0]);
     yyinit();
     for (p=argv[0]; *p; p++) if (*p=='/') progname = p+1;
     for (i=1; i<argc; i++) {
     	  if (EQUAL == strcmp(argv[i],"--help")) {
	       usage();
	       exit(0);
	       }
     	  if (EQUAL == strcmp(argv[i],"-dep")) {
	       stop_after_dep = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-cxx")) {
	       do_cxx = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-noline")) {
	       noline = TRUE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-pthreadlocal")) {
	       pthreadThreadLocal=TRUE;
               compilerThreadLocal=FALSE;
               continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-typecodes")) {
	       printtypecodes();
	       return 0;
	       }
     	  if (EQUAL == strcmp(argv[i],"-noarraychks")) {
	       arraychks = FALSE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-nocasechks")) {
	       casechks = FALSE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-nomacros")) {
	       nomacros = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-O")) {
	       arraychks = FALSE;
	       casechks = FALSE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-tabwidth")) {
	       i++;
	       if (i < argc) tabwidth = atoi(argv[i]);
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-yydebug")) {
	       yydebug = 1;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-debug")) {
	       debug = TRUE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-v")) {
	       puts(Version);
     	       puts(Copyright);
	       continue;
	       }
     	  if ('-' == argv[i][0] && 'I' == argv[i][1]) {
	       if (argv[i][2] == 0) {
		    error("-I option: missing directory");
		    usage();
		    exit(1);
		    }
	       char buf[256];
	       strcpy(buf,sigpath);
	       strcat(buf,":");
	       strcat(buf,argv[i]+2);
	       sigpath = strperm(buf);
	       continue;
	       }
	  if ('-' == argv[i][0]) {
	       error("unrecognized option %s\n",argv[i]);
	       usage();
	       exit(1);
	       }
	  if ( EQUAL == strcmp(".d",tail(argv[i])) || EQUAL == strcmp(".dd",tail(argv[i])) ) {
	       node f;
	       do_this_cxx = do_cxx || EQUAL == strcmp(".dd",tail(argv[i]));
	       global_scope = new(struct SCOPE);
	       readsetup(global_scope);
	       targetname = newsuffixbase(argv[i],"");
	       f = readfile(argv[i]);
	       if (debug) {
		    char *n = newsuffixbase(argv[i],".out");
		    if (NULL == freopen(n,"w", stdout)) {
			 fatal("can't open file %s",n);
			 }
		    put("After parsing:\n");
		    pp(f);
		    fflush(stdout);
		    }
	       outfilename = newsuffixbase(argv[i], do_this_cxx ? "-tmp.cc" : "-tmp.c");
	       {
		    char *n = newsuffixbase(argv[i],".dep.tmp");
		    dependfile = fopen(n,"w");
		    if (dependfile == NULL) fatal("can't open file %s",n);
		    }
	       f = chkprogram(f);
	       if (debug) {
		    char *n = newsuffixbase(argv[i],".log");
		    if (NULL == freopen(n,"w", stdout)) {
			 fatal("can't open file %s",n);
			 }
		    pprintl(f);
		    }
	       {
		    node t = global_scope->signature;
		    char *n = newsuffixbase(argv[i],".sig.tmp");
		    if (NULL == freopen(n,"w", stdout)) {
			 fatal("can't open file %s",n);
			 }
		    printf("-- generated by %s\n\n",progname);
		    while (t != NULL) {
			 dprint(CAR(t));
			 put(";\n");
			 t = CDR(t);
			 }
		    }
	       if (stop_after_dep) quit();
	       checkfordeferredsymbols();
	       if (debug) {
		    char *n = newsuffixbase(argv[i],".sym");
		    if (NULL == freopen(n,"w", stdout)) {
			 fatal("can't open file %s",n);
			 }
		    printsymboltable();
		    printtypelist();
		    printstringlist();
		    }
	       if (n_errors > 0) {
		    quit();
		    }
	       if (TRUE) {
		    char *n = newsuffixbase(argv[i],"-exports.h.tmp");
		    if (NULL == freopen(n,"w", stdout)) {
			 fatal("can't open file %s",n);
			 }
		    printf("#ifndef %s_included\n",targetname);
		    printf("#define %s_included\n",targetname);
		    declarationsstrings = reverse(declarationsstrings);
		    while (declarationsstrings) {
			 node s = unpos(car(declarationsstrings));
			 assert(isstrconst(s));
			 put_unescape(s->body.string_const.characters);
			 put("\n");
			 declarationsstrings = cdr(declarationsstrings);
			 }
		    put(declarations_header);
		    /* printtypecodes(); */
		    cprinttypes();
		    put(declarations_trailer);
		    put("#endif\n");
		    }
	       if (TRUE) {
		    if (NULL == freopen(outfilename,"w", stdout)) {
			 fatal("can't open file %s",outfilename);
			 }
		    printf("#include \"%s\"\n",newsuffixbase(argv[i],"-exports.h"));
		    put(code_header);
		    headerstrings = reverse(headerstrings);
		    while (headerstrings) {
			 locn(car(headerstrings));
			 printpos();
			 node s = unpos(car(headerstrings));
			 assert(isstrconst(s));
			 put_unescape(s->body.string_const.characters);
			 put("\n");
			 locn(NULL);
			 headerstrings = cdr(headerstrings);
			 }
		    cprintsemi(f);
		    }
	       }
	  else {
	       fprintf(stderr,"unknown file type %s\n",argv[i]);
	       usage();
	       exit(1);
	       }
	  }
     quit();
     return 0;
     }

     

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
