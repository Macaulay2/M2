/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

static node hash_buckets[7313];
node complete_symbol_list;
node type_T, keyword_T, int_T, double_T, long_T, short_T, bool_T, char_T;
node float_T, uint_T, ulong_T, ushort_T, uchar_T, package_T, pointer_T;
node void_T, returned_T, undefined_T, null_T, symbol_T, deferred_T;
node getmem_S, sizeof_S, function_K, define_S, define_recursive_types_K,
   chked_K, blank_S, function_S, package_K,
   use_K, export_K, import_K, export_S, import_S, package_S, use_S,
   signature_S, signature_K, array_len_check_S,
   nop_K, return_K, return_S, if_S, ptr_S, brace_list_S, dot_S, memcpy_S,
   refs__S, len__S, define_destroy_S, take_S, array_check_S, lt_S, gt_S,
   part_S, object_type_S, nothing_K, new_K, provide_K, plus_S, break_K,
   array_take_S, length_K, funcall_S, infix_S, prefix_S, type__S, array__S,
   len__S, setd_K, if_K, foreach_K, le_S, plusplus_S, clean_S, kindof_K, 
   foreach_reverse_K, minusminus_S, postfix_S, minus_S, for_K, ge_S,
   not_S, isnull_S, equal_S, when_K, until_K, while_K, object_S, andand_K, oror_K,
   label_S, keyword_K, release_S, releasec_S, reservec_S, reservenc_S,
   reserve_S, reserven_S, defun_S, equal_K, unequal_K, unequal_S,
   true_K, false_K, self_K, sizeof_K, comma_S, colon_S, space_S, deferred_K,
   declare_K, undefined_K, bad_K, void_K, returned_K, returnedThing_K, type_K, null_K, blockn_K, assign_S,
   cast_S, block1_K, or_K, object_K, array_K, block_K, float_K, symbol_K, open_fd_K,
   ushort_K, uint_K, ulong_K, uchar_K, andand_S, oror_S, or_S,
   Ccode_K, Ccode_S, dot_K, colon_K, colonequal_K, coloncolonequal_K, str_S,
   colonequal_S, pointer_K,
   double_K, int_K, goto_S, long_K, short_K, char_K, bool_K, tmp_S, zero, one;

struct {
     char *name; 
     node *var;
     } keywords[] = {
     {"return", &return_K}, {"bool",&bool_K},
     {"=", &setd_K}, {"if", &if_K}, {"kindof", &kindof_K},
     {"new", &new_K}, {"provide", &provide_K}, {"break", &break_K},
     {"when", &when_K}, {"nothing", &nothing_K},
     {"until", &until_K}, {"while", &while_K}, {"self", &self_K},
     {"foreach",&foreach_K},
     {"&&",&andand_K}, {"||", &oror_K},
     {"for", &for_K},
     {"true", &true_K},
     {"false", &false_K}, {"sizeof", &sizeof_K}, {",", &comma_S},
     {"define_recursive_types", &define_recursive_types_K},
     {"undefined", &undefined_K},
     {"deferred", &deferred_K},
     {"bad", &bad_K},
     {"int",&int_K}, {"short",&short_K}, {"long",&long_K}, {"char",&char_K},
     {"function",&function_K}, {"void",&void_K}, {"returned", &returned_K}, {"returnedThing", &returnedThing_K}, {"float",&float_K},
     {"File",&open_fd_K},
     {"double",&double_K}, {"null",&null_K}, {"symbol", &symbol_K},
     {"uchar", &uchar_K},
     {"pointer", &pointer_K},
     {"ushort", &ushort_K}, {"uint", &uint_K}, {"ulong", &ulong_K},
     {"==",&equal_K}, {".",&dot_K}, {":",&colon_K}, {":=",&colonequal_K},
     {"!=",&unequal_K}, {"Ccode", &Ccode_K}, {"::=",&coloncolonequal_K},
     {"object",&object_K}, {"array",&array_K}, {"length",&length_K},
     {"or",&or_K}, {"block", &block_K}, {"block1", &block1_K},
     {"checked_",&chked_K}, {"blockn", &blockn_K}, {"package", &package_K},
     {"use",&use_K}, {"export",&export_K}, {"signature", &signature_K},
     {"import", &import_K},
     },
     strings[] = {
     {":=",&colonequal_S}, {"package",&package_S}, {"signature", &signature_S},
     {" ",&blank_S}, {"function", &function_S}, {"use", &use_S},
     {"define", &define_S}, {"export",&export_S}, {"import",&import_S},
     {"||",&oror_S}, 
     {"or",&or_S}, 
     {"&&",&andand_S}, {"Ccode", &Ccode_S}, {":",&colon_S},
     {"isnull_", &isnull_S}, {"refs_", &refs__S},  {"goto_", &goto_S},
     {"return_", &return_S}, {"if", &if_S}, {"ptr_", &ptr_S},
     {"brace-list", &brace_list_S}, {".", &dot_S}, {"memcpy", &memcpy_S},
     {"funcall_",&funcall_S}, {"tmp_",&tmp_S}, {"!",&not_S},
     {"str_",&str_S},
     {"<SPACE>",&space_S}, {"defun", &defun_S},
     {"label_", &label_S}, {"getmem_", &getmem_S}, {"sizeof_", &sizeof_S},
     {"type_",&type__S}, {"array_",&array__S}, {"len_",&len__S},
     {"define-destroy",&define_destroy_S}, {"assign_",&assign_S},
     {"cast_",&cast_S}, {"reserve_",&reserve_S}, {"reserven_",&reserven_S},
     {"releasec_", &releasec_S}, {"reservec_", &reservec_S},
     {"reservenc_", &reservenc_S}, {"object_", &object_type_S},
     {"release_",&release_S}, {"take",&take_S}, {"<=", &le_S},
     {"++",&plusplus_S}, {"--",&minusminus_S}, {"sizeof", &sizeof_S},
     {"+",&plus_S}, {"-",&minus_S}, {"object", &object_S},
     {"<", &lt_S}, {">", &gt_S}, {">=", &ge_S},
     {"part", &part_S}, {"clean", &clean_S},
     {"array_take_",&array_take_S}, {"array_check_",&array_check_S},
     {"array_len_check_",&array_len_check_S},
     {"infix",&infix_S}, {"prefix",&prefix_S}, 
     {"==",&equal_S}, {"!=",&unequal_S}, 
     };

char *Csymbols[] = {
     "NULL", "stdout", "stdin", "stderr", "flush", "select", 
     /* avoid collisions under Windows NT */
     "min", "max",
     /* end Windows */
     /* avoid collisions on the Mac */
     "abs", "times", "erase", "frame", "index",
     /* end Mac */
     "auto", "break", "case", "const", "continue", "default",
     "register", "signed", "sizeof", "static","struct","switch","typedef",
     "do", "double", "else", "enum", "extern", "float", "for", "goto", "if",
     "union", "unsigned", "volatile", "wait", "remove", "expm1", "log1p", "erfc", "erf"
     };

char *uniquify(char *s){
     node ss = UniqueString(s);
     int seqno = ss->body.string.seqno++;
     char buf[1000];
     if (seqno == 0) return s;
     sprintf(buf,"%s_%d",s,seqno);
     return strperm(buf);
     }

void undefine(node s){
     assertpos(issym(s),s);
     pop(s->body.symbol.name->body.string.symbol_list);
     }

void init_dictionary(env v){
     unsigned int i;
     keyword_T = newtype(NULL,NULL,TRUE);
     interntype(keyword_T);
     type_T = newtype(NULL,NULL,TRUE);
     interntype(type_T);
     keyword_K = newsymbol(UniqueString("keyword"),keyword_T,v,
	  intern_F|keyword_F|initialized_F);
     keyword_T->body.type.name = keyword_K;
     type_K = newsymbol(UniqueString("type"),type_T,v,
	  intern_F|keyword_F|initialized_F);
     type_K->body.symbol.value = type_T;
     type_T->body.type.name = type_K;
     for (i=0; i<numberof(keywords); i++) {
	  node sym = newsymbol(UniqueString(keywords[i].name),
	       keyword_T,v,
	       intern_F|keyword_F|initialized_F);
	  *keywords[i].var = sym;
	  }
     init_chk();
     for (i=0; i<numberof(strings); i++) {
	  *strings[i].var = UniqueString(strings[i].name);
	  }
     int_T = basictype(int_K);
     char_T = basictype(char_K);
     short_T = basictype(short_K);
     long_T = basictype(long_K);
     uint_T = basictype(uint_K);
     uchar_T = basictype(uchar_K);
     ushort_T = basictype(ushort_K);
     pointer_T = basictype(pointer_K);
     ulong_T = basictype(ulong_K);
     double_T = basictype(double_K);
     float_T = basictype(float_K);
     package_T = basictype(package_K);
     bool_T = basictype(bool_K);
     bool_T->body.type.Cname = "char";
     true_K->body.symbol.type = bool_T;
     true_K->body.symbol.Cname = "1";
     false_K->body.symbol.type = bool_T;
     false_K->body.symbol.Cname = "0";
     void_T = basictype(void_K);
     returned_T = basictype(returned_K);
     returnedThing_K->body.symbol.type = returned_T;
     returnedThing_K->body.symbol.flags &= ~keyword_F;
     nothing_K->body.symbol.type = void_T;
     nothing_K->body.symbol.flags &= ~keyword_F;
     undefined_T = basictype(undefined_K);
     undefine(undefined_K);
     deferred_T = basictype(deferred_K);
     undefine(deferred_K);
     symbol_T = basictype(symbol_K);
     null_T = basictype(null_K);
     null_T->body.type.Cname = "void *";
     bad_K->body.symbol.type = undefined_T;
     double_T->body.type.arithmetic_type = TRUE;
     float_T->body.type.arithmetic_type = TRUE;
     int_T->body.type.arithmetic_type = TRUE;
     char_T->body.type.arithmetic_type = TRUE;
     long_T->body.type.arithmetic_type = TRUE;
     short_T->body.type.arithmetic_type = TRUE;
     int_T->body.type.integer_type = TRUE;
     char_T->body.type.integer_type = TRUE;
     long_T->body.type.integer_type = TRUE;
     short_T->body.type.integer_type = TRUE;
     uint_T->body.type.arithmetic_type = TRUE;
     uchar_T->body.type.arithmetic_type = TRUE;
     ulong_T->body.type.arithmetic_type = TRUE;
     ushort_T->body.type.arithmetic_type = TRUE;
     uint_T->body.type.Cname = "unsigned int";
     uchar_T->body.type.Cname = "unsigned char";
     ulong_T->body.type.Cname = "unsigned long";
     ushort_T->body.type.Cname = "unsigned short";
     uint_T->body.type.integer_type = TRUE;
     uchar_T->body.type.integer_type = TRUE;
     ulong_T->body.type.integer_type = TRUE;
     ushort_T->body.type.integer_type = TRUE;
     pointer_T->body.type.Cname = "void *";
     for (i=0; i<numberof(Csymbols); i++) uniquify(Csymbols[i]);
     }

unsigned int hash(char *p){
     unsigned int i = 0;
     while (*p) {
	  i *= 47;
	  i += *p++;
	  }
     return i;
     }

unsigned int hashn(char *p, unsigned int len){
     unsigned int i = 0;
     while (len>0) {
	  i *= 47;
	  i += *p++;
	  len --;
	  }
     return i;
     }

node UniqueStringN(char *s, unsigned int len){
     int h = hashn(s,len) % numberof(hash_buckets);
     node p;
     for (p = hash_buckets[h]; p != NULL; p = CDR(p)) {
	  node q = CAR(p);
	  assertpos(q->tag == string_tag,q);
     	  if (strequaln(q->body.string.contents,s,len)) {
	       return q;
	       }
	  }
     {
	  node q = newnode(STRING,string_tag);
	  q->body.string.contents = strnperm(s,len);
	  q->body.string.symbol_list = NULL;
	  q->body.string.seqno = 0;
	  q->body.string.token = 0;
	  q->body.string.hash = h;
	  push(hash_buckets[h],q);
	  return q;
	  }
     }

node String(char *s){
     node q = newnode(STRING,string_tag);
     q->body.string.contents = s;
     q->body.string.symbol_list = NULL;
     q->body.string.seqno = 0;
     q->body.string.token = 0;
     q->body.string.hash = 0;
     return q;
     }

int hashnode(node e){
     if (e == NULL) return 0;
     switch(e->tag) {
	  case string_tag: return e->body.string.hash;
	  case position_tag: return hashnode(e->body.position.contents);
	  case cons_tag: return hashnode(CAR(e)) + 31 + 47 * hashnode(CDR(e));
	  case symbol_tag: return hashnode(e->body.symbol.name);
	  case int_const_tag: return hash(e->body.int_const.contents);
	  case double_const_tag: return hash(e->body.double_const.contents);
	  case type_tag: return typeforward(e)->body.type.seqno;
	  case char_const_tag : return e->body.char_const.contents;
	  case string_const_tag : return hash(e->body.string_const.contents);
	  }
     return 0;
     }

#if 0
static struct SEQNO {
     int seqno; node this; struct SEQNO * next;
     } *seqno_buckets[7313];

int sequence(node e) {
     /* returns the number of times previously called with argument 
        "equal" to e */
     int h = hashnode(e) % numberof(seqno_buckets);
     struct SEQNO *p = seqno_buckets[h];
     for (; p!=NULL; p = p->next) if (equal(p->this,e)) return p->seqno++;
     p = new(struct SEQNO);
     p->seqno = 1;
     p->this = e;
     p->next = seqno_buckets[h];
     seqno_buckets[h] = p;
     return 0;
     }
#endif

node lookupword(node f, env v){
     node p;
     f = unpos(f);
     assertpos(f->tag == string_tag,f);
     p = f->body.string.symbol_list;
     if (p != NULL) return car(p);
     return NULL;
     }

void printstringlist(){
     node p;
     unsigned int h;
     pput("\nString Table\n");
     for (h=0; h<numberof(hash_buckets); h++) {
	  for (p = hash_buckets[h]; p != NULL; p = CDR(p)) {
	       node str = CAR(p);
	       assertpos(isstr(str),str);
	       if (str->body.string.symbol_list==NULL) continue;
	       pprint(str);
	       pput(" : ");
	       pprint(str->body.string.symbol_list);
	       pput("\n");
	       }
	  }
     }

void checkfordeferredsymbols(){
     node p;
     for (p = complete_symbol_list; p != NULL; p = CDR(p)) {
	  node s = CAR(p);
	  assertpos(s->tag == symbol_tag,s);
	  if ( !(s->body.symbol.flags & initialized_F)
	       &&
	       !(s->body.symbol.flags & import_F)
	       ) {
	       errorpos(s,"symbol never defined");
	       }
	  }
     }

void printsymboltable(){
     node p;
     pput("\n\nSymbol Table\n");
     for (p = complete_symbol_list; p != NULL; p = CDR(p)) {
	  node s = CAR(p);
	  assertpos(s->tag == symbol_tag,s);
	  if ( !(s->body.symbol.flags & initialized_F)
	       &&
	       !(s->body.symbol.flags & import_F)
	       ) {
	       errorpos(s,"never initialized");
	       }
	  cprint(s->body.symbol.name);
	  put(" : ");
	  if (s->body.symbol.cprintvalue) {
	       put("cprintvalue=");
	       cprint(s->body.symbol.cprintvalue);
	       put(" ");
	       }
	  if (s->body.symbol.Cname != NULL) {
	       put("Cname=");
	       put(s->body.symbol.Cname);
	       put(" ");
	       }
	  pprint(s->body.symbol.type);
	  if (s->body.symbol.value != NULL) {
	       node val = s->body.symbol.value;
	       put(" : ");
	       if (istype(val) && val->body.type.name == s) {
		    pprint(val->body.type.definition);
		    }
	       else pprint(val);
	       }
	  if (s->body.symbol.flags & macro_F) put(" macro");
	  if (s->body.symbol.flags & readonly_F) put(" readonly");
	  if (s->body.symbol.flags & symbol_F) put(" symbol");
	  if (s->body.symbol.flags & keyword_F) put(" keyword");
	  if (s->body.symbol.flags & constant_F) put(" constant");
	  if (s->body.symbol.flags & initialized_F) put(" initialized");
	  if (s->body.symbol.flags & export_F) put(" export");
	  if (s->body.symbol.flags & import_F) put(" import");
	  if (s->body.symbol.flags & global_F) put(" global");
	  if (s->body.symbol.flags & literal_F) put(" literal");
	  if (s->body.symbol.flags & visible_F) put(" visible");
	  if (s->body.symbol.args != NULL) {
	       put(" args=");
	       cprintlist(s->body.symbol.args);
	       }
	  if (s->body.symbol.body != NULL) {
	       put("\n     body=");
	       pprint(s->body.symbol.body);
	       }
     	  if (s->body.symbol.export_list != NULL) {
	       put("\n     export_list = ");
	       cprintlist(s->body.symbol.export_list);
	       }
	  pput("\n");
	  }
     }

static void laydown(char *w, char **p){
     while (*w) {
	  *(*p)++ = *w++;
	  }
     *(*p)++ = '_';
     }

char *totoken(char *s){
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

node cleaners = NULL;

void reinternsymbol(node s, env v){
     assertpos(issym(s),s);
     push(s->body.symbol.name->body.string.symbol_list,s);
     push(v->symbols,s);
     if (s->body.symbol.name == clean_S && 
	  isfunctiontype(type(s)) &&
	  length(functionargtypes(type(s))) == 1
	  ) {
	  push(cleaners,s);
	  }
     }

void setprefix(char *buf, node package){
     if (package == NULL) return;
     setprefix(buf,package->body.symbol.package);
     if (EQUAL!=strcmp("C",package->body.symbol.name->body.string.contents)) {
	  strcat(buf,package->body.symbol.name->body.string.contents);
     	  strcat(buf,"_");
	  }
     }

char *prefixify(node package, char *name){
     char buf[500];
     if (package==NULL) return name;
     assertpos(issym(package),package);
     assertpos(isstr(package->body.symbol.name),package);
     assertpos(package->body.symbol.name->body.string.contents!=NULL,package->body.symbol.name);
     buf[0]=0;
     setprefix(buf,package);
     strcat(buf,name);
     return strperm(buf);
     }

void exportit(node s, env v){
     node package = NULL;
     if (v != NULL && v->previous != NULL) {
	  package = v->previous->current_package;
	  }
     if (package != NULL) {
	  assertpos(issym(package),package);
	  push(package->body.symbol.export_list,s);
	  }
     }

void internsymbol(node s, env v){
     assertpos(issym(s),s);
     push(complete_symbol_list,s);
     if (s->body.symbol.flags & intern_F) {
	  errorpos(s,"symbol defined again ... ");
	  return;
	  }
     s->body.symbol.flags |= intern_F;
     if (v != NULL) reinternsymbol(s,v);
     if (s->body.symbol.type!=keyword_T) {
	  char *Cname;
	  assertpos(issym(s),s);
	  if (s->body.symbol.flags & literal_F) {
	       Cname = s->body.symbol.name->body.string.contents;
	       }
	  else {
	       Cname = totoken(s->body.symbol.name->body.string.contents);
	       if (s->body.symbol.flags & (export_F | import_F)) {
		    Cname = prefixify(s->body.symbol.package,Cname);
		    }
	       }
	  Cname = uniquify(Cname);
	  s->body.symbol.Cname = Cname;
	  }
     if (s->body.symbol.flags & (export_F|import_F)) exportit(s,v);
     }

node newsymbol(node p, node ptype, env v, int flags){
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
     	  assertpos(name->tag == string_tag,name);
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
	  env w = v->previous;
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

node newtmp(node ttype, env v, bool decl){
     node tmpsymb;
     assert(ttype != returned_T);
     tmpsymb = newsymbol(tmp_S,totype(ttype),NULL,
	  intern_F|tmp_F|initialized_F|literal_F);
     if (decl) push(v->tmpdecls,list(2,define_S,tmpsymb));
     return tmpsymb;
     }

node newstmp(node ttype, env v, bool decl){
     node tmpsymb;
     static int count = 0;
     char buf[20];
     tmpsymb = newsymbol(tmp_S,totype(ttype),NULL, intern_F|initialized_F|tmp_F);
     sprintf(buf,"stmp%d_",count++);
     tmpsymb->body.symbol.Cname = strperm(buf);
     if (decl) push(v->tmpdecls,list(2,define_S,tmpsymb));
     return tmpsymb;
     }

void unwind(node *symbols){
     while (*symbols != NULL) {
	  node s = pop_1(symbols);
	  node str;
	  assert(s->tag == symbol_tag);
	  str = s->body.symbol.name;
	  assert(str->tag == string_tag);
	  pop(str->body.string.symbol_list);
	  }
     }

chkfun getchkfun(node e){
     e = unpos(e);
     if (e!=NULL && e->tag == symbol_tag) return e->body.symbol.chk;
     else return NULL;
     }
