/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"
#include "dictionary.h"

static node hash_buckets[7313];

const char *Csymbols[] = {
     "NULL", "stdout", "stdin", "stderr", "flush", "select", 
     "min", "max", "abort",
     "abs", "times", "erase", "frame", "index",
     "auto", "break", "case", "const", "continue", "default",
     "register", "signed", "sizeof", "static","struct","switch","typedef",
     "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "mkdir", "stat",
     "union", "unsigned", "volatile", "wait", "remove", "expm1", "log1p", "erfc", "erf",
     "dummy", "acos", "asin", "atan", "atan2", "cos", "cosh", "exp",
     "floor", "lgamma", "log", "sin", "sinh", "sqrt", "tan", "tanh", "cot",
     "characters"
     };

node type__T, keyword_T, int_T, double_T,
       bool_T, char_T, symbol_T, package_T;
node void_T, exits_T, returns_T, bad_or_undefined_T, null_T, deferred__T;
node complete_symbol_list;

const char *CXXkeywords[] = {
  /* avoid collisions with C++ keywords not allowed as identifiers */
  "this", "default",
};

const char *uniquify(const char *s){
     node ss = UniqueString(s);
     int seqno = ss->body.unique_string.seqno++;
     char buf[1000];
     if (seqno == 0) return s;
     sprintf(buf,"%s_%d",s,seqno);
     return strperm(buf);
     }

static void uniquifyCXX(const char *s){
     node ss = UniqueString(s);
     ss->body.unique_string.flags |= str_keyword_F;
     }

static void undefine(const node s){
     assertpos(issym(s),s);
     /* if (debug) fprintf(stderr,"undefining %s\n",tostring(s)); */
     pop(s->body.symbol.name->body.unique_string.symbol_list);
     }

#define f(x,y) node y ## _S, y ## _K;
#define g(y)   f(y,y)
#include "keywords.h"
#undef f
#undef g

void init_dictionary(scope v){
     unsigned int i;
     keyword_T = newtype(NULL,NULL,TRUE);
     interntype(keyword_T);
     type__T = newtype(NULL,NULL,TRUE);
     interntype(type__T);
     #define f(name,var) {					\
	  node sym = newsymbol(var##_S = UniqueString(name),	\
	       keyword_T,v,					\
	       intern_F|keyword_F|defined_F);		\
	  var##_K = sym;					\
	  }
     #define g(var) f(#var,var)
     #include "keywords.h"
     keyword_T->body.type.name = keyword__K;
     type__K->body.symbol.value = type__T;
     type__T->body.type.name = type__K;
     init_chk();
     int_T = basictype(int_K);
     one__K->body.symbol.type = int_T;
     zero__K->body.symbol.type = int_T;
     one__K->body.symbol.Cname = "1";
     zero__K->body.symbol.Cname = "0";
     char_T = basictype(char_K);
     double_T = basictype(double_K);
     package_T = basictype(package_K);
     bool_T = basictype(bool_K);
     bool_T->body.type.Cname = "char";
     true_K->body.symbol.type = bool_T;
     true_K->body.symbol.Cname = "1";
     false_K->body.symbol.type = bool_T;
     false_K->body.symbol.Cname = "0";
     void_T = basictype(void_K);
     returns_T = basictype(returns_K);
     exits_T = basictype(exits_K);
     _returnedThing_K->body.symbol.type = returns_T;
     _returnedThing_K->body.symbol.flags &= ~keyword_F;
     bad_or_undefined_T = basictype(undefined__K);
     deferred__T = basictype(deferred__K); /* the type of a symbol whose type is not known yet */
     undefine(deferred__K);
     symbol_T = basictype(symbol__K);
     null_T = basictype(null_K);
     null_T->body.type.Cname = "void *";
     exits_T->body.type.Cname = "void";
     bad__K->body.symbol.type = bad_or_undefined_T;
     double_T->body.type.flags |= arithmetic_type_F;
     int_T->body.type.flags |= arithmetic_type_F;
     char_T->body.type.flags |= arithmetic_type_F;
     int_T->body.type.flags |= integer_type_F;
     char_T->body.type.flags |= integer_type_F;
     for (i=0; i<numberof(Csymbols); i++) uniquify(Csymbols[i]);
     for (i=0; i<numberof(CXXkeywords); i++) uniquifyCXX(CXXkeywords[i]);
     }

static unsigned int hashn(const char *p, unsigned int len){
     unsigned int i = 0;
     while (len>0) {
	  i *= 47;
	  i += *p++;
	  len --;
	  }
     return i;
     }

node UniqueStringN(const char *s, unsigned int len){
     int h = hashn(s,len) % numberof(hash_buckets);
     node p;
     for (p = hash_buckets[h]; p != NULL; p = CDR(p)) {
	  node q = CAR(p);
	  assertpos(q->tag == unique_string_tag,q);
     	  if (strequaln(q->body.unique_string.characters,s,len)) {
	       return q;
	       }
	  }
     node q = newnode(UNIQUE_STRING,unique_string_tag);
     q->body.unique_string.characters = strnperm(s,len);
     q->body.unique_string.hash = h;
     push(hash_buckets[h],q);
     return q;
     }

node UniqueString(const char *s) {
     return UniqueStringN(s,strlen(s));
     }

node String(const char *s){
     node q = newnode(STRING,string_tag);
     q->body.unique_string.characters = s;
     return q;
     }

node lookupword(node f){
     node p;
     f = unpos(f);
     assertpos(f->tag == unique_string_tag,f);
     p = f->body.unique_string.symbol_list;
     return p != NULL ? car(p) : NULL;
     }

void printstringlist(){
     node p;
     unsigned int h;
     d_pput("String Table\n");
     for (h=0; h<numberof(hash_buckets); h++) {
	  for (p = hash_buckets[h]; p != NULL; p = CDR(p)) {
	       node str = CAR(p);
	       assertpos(isstr(str),str);
	       pprint(str);
	       d_pput(" : ");
	       pprint(str->body.unique_string.symbol_list);
	       d_pput("\n");
	       }
	  }
     d_pput("\n");
     }

void checkfordeferredsymbols(){
     node p;
     for (p = complete_symbol_list; p != NULL; p = CDR(p)) {
	  node s = CAR(p);
	  assertpos(s->tag == symbol_tag,s);
	  if (s->body.symbol.flags & errmsg_given_F) continue;
	  bool def = !!(s->body.symbol.flags & (defined_F|import_F));
	  bool dec = type(s) != deferred__T;
	  if (!def && !dec)
	       errorpos(s,"symbol never defined nor declared");
	  else if (!def)
	       errorpos(s,"symbol never defined");
	  else if (!dec)
	       errorpos(s,"symbol never declared");
	  }
     }

static void psymbol(node s){
     assertpos(s->tag == symbol_tag,s);
     cprint(s->body.symbol.name);
     if (s->body.symbol.cprintvalue) {
	  d_put("\n      cprintvalue => ");
	  cprint(s->body.symbol.cprintvalue);
	  d_put("\n      ");
	  }
     if (s->body.symbol.Cname != NULL) {
	  d_put("\n      Cname => ");
	  d_put(s->body.symbol.Cname);
	  }
     d_put("\n      type => ");
     pprint(s->body.symbol.type);
     d_put("\n      value => ");
     if (s->body.symbol.value != NULL) {
	  node val = s->body.symbol.value;
	  if (istype(val) && val->body.type.name == s) {
	       pprint(val->body.type.definition);
	       }
	  else pprint(val);
	  }
     else {
       d_put("none");
       }
     d_put("\n      flags:");
     if (s->body.symbol.flags & macro_function_F) d_put(" macro-function");
     if (s->body.symbol.flags & macro_variable_F) d_put(" macro-variable");
     if (s->body.symbol.flags & readonly_F) d_put(" readonly");
     if (s->body.symbol.flags & symbol_F) d_put(" symbol");
     if (s->body.symbol.flags & keyword_F) d_put(" keyword");
     if (s->body.symbol.flags & constant_F) d_put(" constant");
     if (s->body.symbol.flags & defined_F) d_put(" initialized");
     if (s->body.symbol.flags & export_F) d_put(" export");
     if (s->body.symbol.flags & import_F) d_put(" import");
     if (s->body.symbol.flags & threadLocal_F) d_put(" thread");
     if (s->body.symbol.flags & const_F) d_put(" const");
     if (s->body.symbol.flags & global_F) d_put(" global");
     if (s->body.symbol.flags & literal_F) d_put(" literal");
     if (s->body.symbol.flags & visible_F) d_put(" visible");
     if ( !(s->body.symbol.flags & defined_F) && !(s->body.symbol.flags & import_F) ) d_put(" (never initialized)");
     if (s->body.symbol.args != NULL) {
	  d_put("\n      args => ");
	  cprintlist(s->body.symbol.args);
	  }
     if (s->body.symbol.body != NULL) {
	  d_put("\n      body => ");
	  pprint(s->body.symbol.body);
	  }
     if (s->body.symbol.export_list != NULL) {
	  d_put("\n      export_list => ");
	  cprintlist(s->body.symbol.export_list);
	  }
     d_pput("\n");
     }

void printsymboltable(){
     node p;
     d_pput("Symbol Table\n");
     for (p = complete_symbol_list; p != NULL; p = CDR(p)) psymbol(CAR(p));
     d_pput("\n");
     }

static void laydown(const char *w, char **p){
     while (*w) {
	  *(*p)++ = *w++;
	  }
     *(*p)++ = '_';
     }

const char *totoken(const char *s){
     char buf[1000];
     char *p = buf;
     if ('0' <= *s && *s <= '9') *p++ = '_';
     for (;*s;s++) {
	  char c = *s;
	  if (('a'<=c && c<='z') || ('A'<=c && c<='Z') 
	       || ('0' <= c && c <= '9')) {
	       *p++ = c;
	       continue;
	       }
	  switch(c){
	       case '*': laydown("star",&p); break;
	       case '<': laydown("less",&p); break;
	       case '+': laydown("plus",&p); break;
	       case '-': laydown("minus",&p); break;
	       case '/': laydown("slash",&p); break;
	       case '_': laydown("underscore",&p); break;
	       case '>': laydown("greater",&p); break;
	       case '=': laydown("equal",&p); break;
	       case '$': laydown("dollar",&p); break;
	       case '@': laydown("at",&p); break;
	       case '!': laydown("pt",&p); break;
	       case '#': laydown("sharp",&p); break;
	       case '%': laydown("pct",&p); break;
	       case '^': laydown("circ",&p); break;
	       case '&': laydown("amp",&p); break;
	       case '`': laydown("bq",&p); break;
	       case '~': laydown("tilde",&p); break;
	       case '|': laydown("or",&p); break;
	       case '\\': laydown("backslash",&p); break;
	       case '[': laydown("lbracket",&p); break;
	       case ']': laydown("rbracket",&p); break;
	       case ':': laydown("colon",&p); break;
	       case ';': laydown("semicolon",&p); break;
	       case '"': laydown("quotes",&p); break;
	       case '\'': laydown("quote",&p); break;
	       case '?': laydown("question",&p); break;
	       case '{': laydown("lbrace",&p); break;
	       case '}': laydown("rbrace",&p); break;
	       case '.': laydown("period",&p); break;
	       case ',': laydown("comma",&p); break;
	       default : assert(FALSE);
	       }
	  assert(p < buf + sizeof(buf));
	  }
     *p = '\0';
     return strperm(buf);
     }

void reinternsymbol(node s, scope v){
     assertpos(issym(s),s);
     push(s->body.symbol.name->body.unique_string.symbol_list,s);
     push(v->symbols,s);
     }

static void setprefix(char *buf, node package){
     if (package == NULL) return;
     setprefix(buf,package->body.symbol.package);
     if (EQUAL!=strcmp("C",tostring(package))) {
          strcat(buf,tostring(package));
     	  strcat(buf,"_");
	  }
     }

const char *prefixify(node package, const char *name){
     char buf[500];
     if (package==NULL) return name;
     assertpos(issym(package),package);
     assertpos(isstr(package->body.symbol.name),package);
     assertpos(tostring(package) != NULL, package->body.symbol.name);
     buf[0]=0;
     setprefix(buf,package);
     strcat(buf,name);
     return strperm(buf);
     }

void exportit(node s, scope v){
     node package = NULL;
     if (v != NULL && v->previous != NULL) {
	  package = v->previous->current_package;
	  }
     if (package != NULL) {
	  assertpos(issym(package),package);
	  push(package->body.symbol.export_list,s);
	  }
     }

void internsymbol(node s, scope v){
     assertpos(issym(s),s);
     push(complete_symbol_list,s);
     if (s->body.symbol.flags & intern_F) {
	  errorpos(s,"symbol defined again ... ");
	  return;
	  }
     s->body.symbol.flags |= intern_F;
     if (v != NULL) reinternsymbol(s,v);
     /* if ( 0 == strcmp("x",tostring(s)) ) trap(); */
     if (s->body.symbol.type!=keyword_T) {
	  const char *Cname;
	  assertpos(issym(s),s);
	  if (s->body.symbol.flags & literal_F) {
	    Cname = tostring(s); /* no totoken here? */
	    if (!(s->body.symbol.flags & nouniquify_F))
	      Cname = uniquify(Cname);
	  }
	  else {
	    Cname = totoken(tostring(s));
	    if (s->body.symbol.flags & (export_F | import_F))
	      Cname = prefixify(s->body.symbol.package,Cname);
	    if (!(s->body.symbol.flags & nouniquify_F))
	      Cname = uniquify(Cname);
	    else if (s->body.symbol.name->body.unique_string.seqno == 0) 
	      s->body.symbol.name->body.unique_string.seqno++;
	  }
	  s->body.symbol.Cname = Cname;
	  }
     if (s->body.symbol.flags & (export_F|import_F)) exportit(s,v);
     }

node newsymbol(node p, node ptype, scope v, int flags){
     node name;
     node s;
     struct POS *ppos = NULL;
     assertpos(istype(ptype),ptype);
     if (issym(p)) {
	  /* defined previously so the Cname can be set by the translator */
	  return p;
	  }
     if (isstr(p)) {
	  name = p;
	  p = NULL;
	  }
     else if (isstrpos(p)) {
	  ppos = &p->body.position.pos;
     	  name = p->body.position.contents;
     	  assertpos(name->tag == unique_string_tag,name);
	  }
     else {
	  errorpos(p,"defining a nonsymbol");
	  return NULL;
	  }
     s = newnode(SYMBOL,symbol_tag); /* newnode clears the memory */
     assert(s->body.symbol.flags == 0);
     s->body.symbol.name = name;
     s->body.symbol.type = ptype;
     s->body.symbol.pos = ppos==NULL ? empty_pos : *ppos;
     if (!inside_defun(v) && !(flags & tmp_F)) flags |= global_F;
     s->body.symbol.flags = flags & ~intern_F;
     if (v!=NULL && v->previous != NULL) {
	  scope w = v->previous;
	  assert(w->current_package==NULL || issym(w->current_package));
	  s->body.symbol.package = w->current_package;
	  }
     if (flags & intern_F) internsymbol(s,v);
     return s;
     }

void setcprintvalue(node sym, node val){
     assert(issym(sym));
     sym->body.symbol.cprintvalue = val;
     }

node newtmp(node ttype, scope v, bool decl){
     node tmpsymb;
     assert(ttype != returns_T && ttype != exits_T);
     tmpsymb = newsymbol(tmp__S,totype(ttype),NULL,
	  intern_F|tmp_F|defined_F|literal_F);
     if (decl) push(v->tmpdecls,list(2,declare__S,tmpsymb));
     return tmpsymb;
     }

node newstmp(node ttype, scope v, bool decl){
     node tmpsymb;
     static int count = 0;
     char buf[20];
     tmpsymb = newsymbol(tmp__S,totype(ttype),NULL, intern_F|defined_F|tmp_F);
     sprintf(buf,"stmp%d_",count++);
     tmpsymb->body.symbol.Cname = strperm(buf);
     if (decl) push(v->tmpdecls,list(2,declare__S,tmpsymb));
     return tmpsymb;
     }

void unwind(node *symbols){
     while (*symbols != NULL) {
          undefine(car(*symbols));
          *symbols = cdr(*symbols);
     }
}

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/








