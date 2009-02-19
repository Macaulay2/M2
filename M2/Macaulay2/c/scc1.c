/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

FILE *dependfile;
char *targetname;
bool do_dep = FALSE;
bool noline = FALSE;
bool do_sig = FALSE;
bool gc = FALSE;
bool noinits = FALSE;
bool arraychks = TRUE;
bool do_memstats = TRUE;
bool do_countstats = FALSE;
bool do_refctchks = TRUE;
bool debug = FALSE;

static char Copyright[] = "Copyright 1993 by Daniel R. Grayson";
static char Version[]   = "Safe C - version 0.1";

char *getmem(unsigned n) {
     char *p = malloc(n);
     if (p == NULL) fatal("out of memory");
     return p;
     }

static char *progname;

void myexit(int i){
     exit(i);
     }

node newnode1(unsigned int len, enum TAG tag) {
     node p = (node) getmem(len);
     memset(p,0x00,len);
     p->tag = tag;
     return p;
     }

char *strnperm(const char *s, unsigned n){
     char *t = getmem(n+1);
     strncpy(t,s,n);
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

#if 0
int atoi(char *s) {
     int sign = 1;
     int n = 0;
     if (*s == '-') sign = -1, s++;
     while (*s != 0) n = 10 * n + *s++ - '0';
     return n * sign;
     }
#endif

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


int strequaln(char *s, char *t, unsigned int tlen){
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

int tty(){
     return NULL != freopen("/dev/tty","w",stdout);
     }

bool oldc = FALSE;
bool gcc = TRUE;



const struct POS empty_pos;
struct ENV global;

char file_header[] = "\
#include \"compat.h\"\n\
extern char *getmem();\n\
extern void flush();\n\
extern void fatalrefctcheck();\n\
extern void fatalarraylen();\n\
extern void fatalarrayindex();\n\
extern void fatalarraylen();\n\
extern void destroy();\n\
extern void outofmem();\n\
extern void outofmem2(size_t);\n\
extern int do_memstats;\n\
extern int numchunks[2];\n\
extern int numbytes[2];\n\
extern struct FINAL {\n\
     void (*final)();\n\
     struct FINAL *next;\n\
     } *final_list;\n\
";

void readsetup(env v){
     one = IntegerN("1",1);
     zero = IntegerN("0",1);
     init_dictionary(v);
     read_setup();
#if 0
     C_K = NULL;		/* disables further C expresssions */
#endif
     }

#if defined(__APPLE__) && defined(__MACH__)
extern char *get_end(), *get_etext();
#endif

int main(int argc, char **argv){
     int i;
     char *p;
     struct test {char a;double b;};
     i = assert(0 == GRAIN % (sizeof(struct test) - sizeof(double)));
     GC_INIT();
#include "gc_fixes.h"
     progname = BaseName(argv[0]);
     yyinit();
     for (p=argv[0]; *p; p++) if (*p=='/') progname = p+1;
     for (i=1; i<argc; i++) {
     	  if (EQUAL == strcmp(argv[i],"-dep")) {
	       do_dep = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-noline")) {
	       noline = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-sig")) {
	       do_sig = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-nosetup")) {
	       do_setup = FALSE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-noarraychks")) {
	       arraychks = FALSE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-noinits")) {
	       noinits = TRUE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-O")) {
	       do_memstats = FALSE;
	       arraychks = FALSE;
	       do_refctchks = FALSE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"-memstats")) {
	       do_memstats = FALSE;
	       continue;
	       }
     	  if (EQUAL == strcmp(argv[i],"+countstats")) {
	       do_countstats = TRUE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-tabwidth")) {
	       i++;
	       if (i < argc) tabwidth = atoi(argv[i]);
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-noansi")) {
	       oldc = TRUE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"+gc")) {
	       gc = TRUE;	/* Boehm garbage collector in code produced */
	       do_memstats = FALSE;
	       continue;
	       }
	  if (EQUAL == strcmp(argv[i],"-nogcc")) {
	       gcc = FALSE;
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
     	  if ('-' == argv[i][0] && 'J' == argv[i][1]) {
	       char buf[256];
	       strcpy(buf,sigpath);
	       strcat(buf,":");
	       strcat(buf,argv[i]+2);
	       sigpath = strperm(buf);
	       continue;
	       }
	  if ('-' == argv[i][0]) {
	       error("unrecognized option %s\n",argv[i]);
	       continue;
	       }
	  if (EQUAL == strcmp(".b",tail(argv[i]))
	       ||
	       EQUAL == strcmp(".d",tail(argv[i]))
	       ) {
	       ZERO_MEM(&global);
	       readsetup(&global);
	       {
		    node f;
		    char *n;
     	       	    f = readfile(argv[i]);
	       	    if (debug && EQUAL == strcmp(".d",tail(argv[i]))) {
			 n = newsuffixbase(argv[i],".out");
		    	 if (NULL == freopen(n,"w", stdout)) {
			      fatal("can't open file %s",n);
			      }
			 put("After parsing:\n");
			 pp(f);
			 fflush(stdout);
			 }
		    targetname = newsuffixbase(argv[i],"");
		    if (do_dep) {
		    	 n = newsuffixbase(argv[i],".dp");
		    	 dependfile = fopen(n,"w");
		    	 if (dependfile == NULL) fatal("can't open file %s",n);
			 fprintf(dependfile,"%s %s : %s\n",
			      newsuffixbase(targetname,".oo"),
			      newsuffixbase(targetname,".loo"),
			      newsuffixbase(targetname,".d")
			      );
			 }
		    f = chkprogram(f);
		    if (do_dep && FALSE) quit();
		    if (debug) {
		    	 n = newsuffixbase(argv[i],".log");
		    	 if (NULL == freopen(n,"w", stdout)) {
			      fatal("can't open file %s",n);
			      }
     	       	    	 pprintl(f);
			 }
		    if (do_sig || TRUE) {
			 node t = global.signature;
		    	 n = newsuffixbase(argv[i],".sg");
		    	 if (NULL == freopen(n,"w", stdout)) {
		    	      fatal("can't open file %s",n);
			      }
     	       	    	 printf("-- generated by %s\n\n",progname);
			 while (t != NULL) {
     	       	    	      dprint(CAR(t));
			      put(";\n");
			      t = CDR(t);
			      }
			 if (FALSE || do_dep) quit();
			 }
     	       	    checkfordeferredsymbols();
		    if (debug) {
		    	 n = newsuffixbase(argv[i],".sym");
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
		    n = newsuffixbase(argv[i],".c");
		    if (NULL == freopen(n,"w", stdout)) {
			 fatal("can't open file %s",n);
			 }
		    put(file_header);
		    if (gc) {
			 put("static int memory_manager_gc;\n");
			 }
		    else put("static int memory_manager_rc;\n");
		    cprinttypes();
		    cprintsemi(f);
		    }
	       }
	  else {
	       fprintf(stderr,"unknown file type %s\n",argv[i]);
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
