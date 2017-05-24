/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"


//are we currently printing a declaration?
//used only for when in pthread mode to tell symbols not to print workaround for symbol 
int threadLocalDeclarationFlag=0;


static void printstringconst(node p){
     char *s = tostring(p);
     putchar('"');
     while (*s) {
	  switch(*s) {
	       case '\n': put("\\n"); break;
	       case '"' : put("\"" ); break;
	       case '\r': put("\\r"); break;
	       case '\f': put("\\f"); break;
	       case '\t': put("\\t"); break;
	       default: putchar(*s);
	       }
	  s++;
	  }
     putchar('"');
     }

char *tostring(node e){
     if (e == NULL) return "<<NULL>>";
     switch(e->tag){
          case cons_tag: pp(e);return "<<cons>>";
	  case position_tag: return tostring(e->body.position.contents);
	  case unique_string_tag: return e->body.unique_string.characters;
	  case string_tag: return e->body.string.characters;
	  case int_const_tag: return e->body.int_const.contents;
          case double_const_tag: return e->body.double_const.contents;
          case type_tag: return tostring(e->body.type.name);
	  case symbol_tag: return tostring(e->body.symbol.name);
     	  case char_const_tag: return strperm(intToString(e->body.char_const.contents));
	  case string_const_tag: return e->body.string_const.characters;
	  default: assert(FALSE); return "<<unrecognized node type>>";
	  }
     }
static void printsymbolbasic(node p){
     assert(p != NULL);
     assert(p->tag == symbol_tag);
     if (p->body.symbol.cprintvalue) {
	  cprint(p->body.symbol.cprintvalue);
	  }
     else if (p->body.symbol.Cname != NULL) {
	  put(p->body.symbol.Cname);
	  }
     else {
          put(tostring(p));
	  }
     }

char* getsymbolbasicname(node p){
     assert(p != NULL);
     assert(p->tag == symbol_tag);
     if (p->body.symbol.Cname != NULL) {
	  return (p->body.symbol.Cname);
	  }
     else {
          return (tostring(p));
	  }
     }

void printsymbol(node p){
     if(pthreadThreadLocal && !threadLocalDeclarationFlag && p->body.symbol.flags & threadLocal_F)
	  {
	    //THIS IS AN INT EXAMPLE
	    //	    *((int*)TS_Get_Local(M2_gbTrace_id)) = 0;
    	    put("(*((");
	    node ltype = type(p);
	    cprint(ltype);
	    put("*)");
	    put("TS_Get_Local(");
	    printsymbolbasic(p);
	    put("_id)))");
	  }
     else
	  printsymbolbasic(p);
     }

void pput(char *s){
     while (*s) {
	  putchar(*s);
	  if (*s == '\n') { }
	  s++;
	  }
     }

int clevel=0;
static void cindent(){
     int cols = 2 * clevel;
     while (cols >= 8) {
	  cols -= 8;
	  pput("\t");
	  }
     while (cols >= 1) {
	  cols -= 1;
	  pput(" ");
	  }
     }     

void put(char *s){
     if (0 == strcmp(s,"{")) clevel++;
     if (0 == strcmp(s,"}")) clevel--;
     while (*s) {
	  putchar(*s);
	  if (*s == '\n') cindent();
	  s++;
	  }
     }

static struct POS *curpos = NULL;

struct POS *pos2(node n) {
     /* this is a version of pos() that ignores the position where a
        symbol was defined, and insists on seeing the position where it
	is being used */
     /* it also ignores "define" commands */
     struct POS *p;
     while (iscons(n)) {
     	  if (CAR(n) == declare__S) return NULL;
	  if (n->body.cons.pos.filename != NULL) return &n->body.cons.pos;
	  p = pos2(CAR(n));
	  if (p != NULL) return p;
	  n = CDR(n);
	  }
     return (
	  ispos(n) 
	  ? &n->body.position.pos 
	  : NULL
	  );
     }

void locn(node n){
     if (n == NULL) {
	  curpos = NULL;
	  return;
	  }
     struct POS *p = pos2(n);
     if (p != NULL) 
	  curpos = p;
     }

void printpos(){
     if (curpos != NULL && !noline) {
	  printf("# line %d \"%s\"",curpos->lineno,curpos->filename);
	  put("\n");
	  }
     }

void pprint(node p){
     if (p==NULL) {
	  pput("()");
	  return;
	  }
     switch (p->tag) {
	  case cons_tag: {
	       if (p->body.cons.pos.filename != NULL) {
	       	    printf("@%d:%d ", p->body.cons.pos.lineno, p->body.cons.pos.column);
		    }
	       pput("(");
	       while (p != NULL) {
		    pprint(CAR(p));
		    p = CDR(p);
		    if (p == NULL) break;
		    pput(" ");
		    }
	       pput(")");
	       break;
	       }
	  case type_tag: {
	       printf("{");
	       while (TRUE) {
		   if (p->body.type.name != NULL) {
			printf("TYPE[%d]:name => ",p->body.type.seqno);
			pprint(p->body.type.name);
			}
		   else if (p->body.type.definition != NULL) {
			printf("TYPE[%d]:definition => ",p->body.type.seqno);
			pprint(p->body.type.definition);
			}
		   else {
			printf("TYPE[%d]", p->body.type.seqno);
			}
		   if (p->body.type.flags) {
		     printf(" flags<");
		     if (p->body.type.flags & deferred_F) printf(" deferred");
		     if (p->body.type.flags & should_be_pointer_F) printf(" should_be_pointer");
		     if (p->body.type.flags & should_be_tagged_F) printf(" should_be_tagged");
		     if (p->body.type.flags & identified_F) printf(" identified");
		     printf(" >");
		   }
		   if (p->body.type.forward == NULL) break;
		   p = p->body.type.forward;
		   printf(" ===> "); /* forwarding */
	           }
	       printf("}");
	       break;
	       }
	  case position_tag: {
	       if (p->body.cons.pos.filename != NULL) {
	       	    printf("@%d:%d ", p->body.position.pos.lineno, p->body.position.pos.column);
		    }
	       pprint(p->body.position.contents);
	       break;
	       }
	  case unique_string_tag: case string_tag: {
	       pput(tostring(p));
	       break;
	       }
     	  case int_const_tag: {
	       pput(p->body.int_const.contents);
	       break;
	       }
     	  case char_const_tag: {
	       pput(intToString(p->body.char_const.contents));
	       break;
	       }
     	  case double_const_tag: {
	       pput(p->body.double_const.contents);
	       break;
	       }
	  case string_const_tag: {
     	       printstringconst(p);
	       break;
	       }
     	  case symbol_tag: {
	       printsymbol(p);
	       break;
	       }
	  }
     }

void pp(node n){
     pprint(n);
     pput("\n");
     }

void pprintl(node n){
     while (n != NULL) pp(CAR(n)), n = CDR(n);
     }

void cprintlist(node e){
     put("(");
     if (e != NULL) while (TRUE) {
	  cprint(CAR(e));
	  e = CDR(e);
	  if (e == NULL) break;
	  put(",");
	  }
     put(")");
     }

void cprintsemi(node e){
     while (e != NULL) {
	  node f = CAR(e);
	  node g = iscons(f) ? CAR(f) : NULL;
     	  locn(f);
	  printpos();
	  cprint(f);
	  if (g==block__K || g==define__S ) ;
	  else put(";\n");
	  e = CDR(e);
	  }
     }

static void cprintstructtag(node t,bool baseclass) {
  assert(istype(t));
  printf("struct ");
  if (t->body.type.name) {
    cprint(t->body.type.name);
    printf("_struct");
  }
  else printf("M2_%d",t->body.type.seqno);
  if (baseclass) printf(" BASECLASS");
}

#define type_decl "unsigned short type_;"

static void cprintarraydef(node t){
     bool tagged = istaggedarraytype(t);
     node m = typedeftail(t);
     node typ = CAR(m);
     node len;
     cprintstructtag(t,FALSE);
     if (length(m)==2) len = CADR(m); else len = NULL;
     put(" {");			/* the extra space prevents further indentation! */
     if (tagged) {
       put(type_decl);
     }
     if (length(m) == 1) put("int len;");
     cprint(typ);
     put(" array[");
     if (len!=NULL) cprint(len); else put("1");
     put("];};\n");
     }

static void cprintobjectdef(node t){
     node m;
     bool had_a_member = FALSE;
     cprintstructtag(t,TRUE);
     printf(" {");
     if (istaggedobjecttype(t)) {
       put(type_decl);
       had_a_member = TRUE;
     }
     for (m=typedeftail(t); m != NULL; m = CDR(m)) {
	  if (CADAR(m) == void_T) continue;
	  cprinttypevar(CADAR(m),CAAR(m));
	  put(";");
	  had_a_member = TRUE;
	  }
     if (!had_a_member) {
	  put("char _;");
	  }
     put("};\n");
     }

static void cprinttype(node t, bool withstar, bool usename){
     assert(istype(t));
     if (t == bad_or_undefined_T) {
       if (debug) warning("undefined type");
       printf("<<undefined type>> ");
       assert(FALSE);
     }
     t = typeforward(t);
     if (t->body.type.Cname != NULL) {
          assert(!(israwpointertype(t) && !withstar));
	  put(t->body.type.Cname);
	  }
     else if (t->body.type.name != NULL && withstar && usename) {
	  cprint(t->body.type.name);
	  }
     else if (isortype(t)) {
       if (t->body.type.flags & composite_F)
	 printf("struct tagged_union *");
       else {
	 node tt = typedeftail(t);
	 while (tt != NULL) {
	   if (car(tt) != null_T) {
	     cprinttype(car(tt),TRUE,TRUE);
	   }
	   tt = cdr(tt);
	 }
       }
     }
     else if (isarraytype(t) || istaggedarraytype(t) || isobjecttype(t) || istaggedobjecttype(t)) {
          cprintstructtag(t,FALSE);
	  if (withstar) put(" *");
	  }
     else if (t->body.type.definition != NULL) {
	  cprint(t->body.type.definition);
	  }
     else assert(FALSE);
     return;
     }

static void cprintsomesizeof(node t, node arraylen){
     assert(istype(t));
     put("sizeof(");
     if (israwpointertype(t)) {
       put("*((");
       cprinttype(t,TRUE,FALSE);
       put(")0)");
     }
     else cprinttype(t,FALSE,FALSE);
     put(")");
     if (isarraytype(t)||istaggedarraytype(t)) {
	  node m = typedeftail(t);
	  if (length(m) == 1) {
	       /* if m has length 2, the second element is the declared length, already checked, so we don't have to add anything here */
	       node typ = CAR(m);
	       assert(arraylen != NULL);
	       put(" + (");
	       cprint(arraylen);
	       put(")*sizeof(");
	       cprint(typ);
	       put(")-sizeof(");
	       cprint(typ);
	       put(")");
	       }
	  }
     }

static void cprintgetmem(node g){
  node s = CAR(g);
  node t = type(s);
  cprint(s);
  put(" = ");
  put("(");
  cprint(t);
  put(") ");
  put(pointer_to_atomic_memory(t) ? "GC_MALLOC_ATOMIC" : "GC_MALLOC");
  put("(");
  cprintsomesizeof(t, length(g)==2 ? CADR(g) : NULL);
  put(");\n");
  put("if (0 == ");
  cprint(s);
  put(") outofmem2((size_t)");
  cprintsomesizeof(t, length(g)==2 ? CADR(g) : NULL);
  put(")");
}

static void cprinttypedef(node t) {
     /* define the structure/union tag */
     assert(istype(t));
     if (t->body.type.name != NULL) {
	  put("typedef ");
	  if (isfunctiontype(t)) {
	       cprint(functionrettype(t));
	       put(" ");
	       put("(*");
	       cprint(t->body.type.name);
	       put(")");
	       cprintlist(functionargtypes(t));
	       }
	  else {
	       cprinttype(t,TRUE,FALSE);
	       put(" ");
	       cprint(t->body.type.name);
	       }
	  assert(issym(t->body.type.name));
	  assert(isstr(t->body.type.name->body.symbol.name));
	  put(";\n");
	  }
     }

static void cprintdefine(node t,bool definitions) {
     node typ = type(t);
     int flags = t->body.symbol.flags;
     assert( typ != void_T );
     t = unpos(t);
     assert(issym(t));
     /* assert( 0 != strcmp("node0",tostring(t)) ); */
     if (definitions && (flags & import_F)) return;
     if (definitions && (flags & macro_variable_F)) return;
     if (flags & (export_F|import_F)) {
       if (!definitions) 
	 put("extern ");
     }
     else if ( 
	  (
	       (flags & global_F)
	       || (
	       	    isfunctiontype(typ)
	       	    &&
	       	    (flags & constant_F)
		    )
	       )
	  &&
	  !(flags & visible_F)
	  ) {
	  put("static ");
	  }
     if (flags & threadLocal_F)
	  {
	       put("int ");
	       threadLocalDeclarationFlag = 1;
	       cprint(t);
	       threadLocalDeclarationFlag = 0;	      
	       put("_id;\n");
//	       put("__thread ");
          }
     if (flags & const_F) put("const ");
     if (isfunctiontype(typ)) {
	  if (flags & macro_function_F) return;
	  cprint(functionrettype(typ));
	  put(" ");
	  if (flags & constant_F) {
	       cprint(t);
	       }
	  else {
	       put("(*");
	       cprint(t);
	       put(")");
	       }
	  cprintlist(functionargtypes(typ));
	  }
     else if(!(flags & threadLocal_F)){
	  cprint(typ);
	  put(" ");
	  cprint(t);
	  }
     if (flags & constructor_F) put(" __attribute__ ((constructor))");
     if (flags & destructor_F) put(" __attribute__ ((destructor))");
     }

static void cprintreturn(node s){
     if (length(s)==0) {
	  put("return");
	  }
     else {
     	  put("return ");
     	  cprint(CAR(s));
	  }
     }

static void cprintassign(node s, node t){
     cprint(s);
     put(" = ");
     cprint(t);
     }

void cprinttypevar(node t, node v){
     if (isfunctiontype(t)) {
	  node r = functionrettype(t);
	  node a = functionargtypes(t);
	  cprint(r);
	  put(" (*");
	  cprint(v);
	  put(")");
	  cprintlist(a);
	  }
     else {
	  cprint(t);
	  put(" ");
	  cprint(v);
	  }
     }


static void cprintdefun(node g) {
     node fun = CAAR(g);
     node args = CDAR(g);
     node funtype = type(fun);
     node rettype = functionrettype(funtype);
     assert(issym(fun));
     locn(g);
     /* printpos(); */
     if (!(fun->body.symbol.flags & export_F)
	  &&
	  !(fun->body.symbol.flags & visible_F)
	  ) put("static ");
     cprint(rettype);
     put(" ");
     cprint(fun);
     put("(");
     if (args != NULL) while (TRUE) {
	  node w = CAR(args);
	  cprinttypevar(type(w),w);
	  args = CDR(args);
	  if (args == NULL) break;
	  put(",");
	  }
     put(")");
     put("{");
     put("\n");
     cprintsemi(CDDR(g));
     printpos();
     put("}");
     put("\n");
     }

static void cprintgoto(node l){
     put("goto ");
     cprint(l);
     }

static void cprintif(node g){
     put("if (");
     cprint(CAR(g));
     put(") ");
     cprint(CADR(g));
     if (length(g) == 3) {
	  put("; else ");
	  cprint(CADDR(g));
	  }
     }

static void cprintlabel(node l){
     cprint(l);
     put(":");
     }

static void cprintisnull(node e){
     put("0 == ");
     cprint(e);
     }

static void cprintcast(node typ, node e){
     put("((");
     cprint(typ);
     put(")");
     cprint(e);
     put(")");
     }

static void cprintarraycheck(node indx, node len){
     struct POS *p;
     if (!arraychks) return;
     locn(len);
     locn(indx);
     p = pos(indx);
     printpos();
     put("if (");
     cprint(indx);
     put(" < 0 || ");
     cprint(indx);
     put(" >= ");
     cprint(len);
     put(") fatalarrayindex(");
     cprint(indx);
     put(",");
     cprint(len);
     put(",");
     if (p == NULL) {
     	  put("__FILE__,__LINE__,-1");
	  }
     else {
	  printf("\"%s\",%d,%d", p->filename,p->lineno,p->column);
	  }
     put(")");
     }

static void cprintarraylencheck(node len){
     struct POS *p;
     if (!arraychks) return;
     p = pos(len);
     locn(len);
     printpos();
     put("if (0 > ");
     cprint(len);
     put(") fatalarraylen(");
     cprint(len);
     put(",");
     if (p == NULL) {
     	  put("__FILE__,__LINE__,-1");
	  }
     else {
	  printf("\"%s\",%d,%d", p->filename,p->lineno,p->column);
	  }
     put(")");
     }

static void cprintarraytake(node arr, node indx){
     cprint(arr);
     put("->array[");
     cprint(indx);
     put("]");
     }     

static void cprinttake(node f, node e){
     cprint(f);
     put("->");
     cprint(e);
     }

static void cprintprefix(node fun, node x){
     put("(");
     cprint(fun);
     put(" ");
     cprint(x);
     put(")");
     }

static void cprintpostfix(node fun, node x){
     put("(");
     cprint(x);
     put(" ");
     cprint(fun);
     put(")");
     }

static void cprintinfix(node fun, node x, node y){
     put("(");
     cprint(x);
     put(" ");
     cprint(fun);
     put(" ");
     cprint(y);
     put(")");
     }

static void cprintfuncall(node fun, node args){
     node g = args;
     cprint(fun);
     printf("(");
     if (g!=NULL) while (TRUE) {
	  cprint(CAR(g));
	  g = CDR(g);
	  if (g == NULL) break;
	  put(", ");
	  }
     put(")");
     }

void put_unescape(char *s) {
  while (*s) {
    if (*s == '\\') {
      s++;
      switch (*s) {
      case 'n': putchar('\n'); break;
      case '"' : putchar('"'); break;
      case 'b': putchar('\b'); break;
      case 't': putchar('\t'); break;
      case 'f': putchar('\f'); break;
      case 'r': putchar('\r'); break;
      case  0 : return       ;	/* shouldn't happen */
      default : putchar(*s)  ; break;
      }
    }
    else putchar(*s);
    s++;
  }
}

static void cprintCcode(node s){
     while (s != NULL) {
	  node a = CAR(s);
	  if (a->tag == string_const_tag) put_unescape(tostring(a));
	  else cprint(a);
	  s = CDR(s);
	  }
     }

static void cprintcons(node f, node g) {
     if (ispos(f)) f = f->body.position.contents;
     if (f == getmem__S) cprintgetmem(g);
     else if (f == array_len_check_S) cprintarraylencheck(CAR(g));
     else if (f == array_check_S) cprintarraycheck(CAR(g),CADR(g));
     else if (f == array_take_S) cprintarraytake(CAR(g),CADR(g));
     else if (f == assign__S) cprintassign(CAR(g),CADR(g));
     else if (f == cast__S) cprintcast(CAR(g),CADR(g));
     else if (f == declare__S) {
       node s = CAR(g);
       cprintdefine(s,TRUE);
     }
     else if (f == define__S) cprintdefun(g);
     else if (f == equalequal__S) cprintinfix(f,CAR(g),CADR(g));
     else if (f == unequal_S) cprintinfix(f,CAR(g),CADR(g));
     else if (f == funcall__S) cprintfuncall(CAR(g),CDR(g));
     else if (f == goto__S) cprintgoto(CAR(g));
     else if (f == if_S) cprintif(g);
     else if (f == infix__S) cprintinfix(CAR(g),CADR(g),CADDR(g));
     else if (f == isnull__S) cprintisnull(CAR(g));
     else if (f == label__S) cprintlabel(CAR(g));
     else if (f == prefix__S) cprintprefix(CAR(g),CADR(g));
     else if (f == postfix__S) cprintpostfix(CAR(g),CADR(g));
     else if (f == return_S) cprintreturn(g);
     else if (f == Ccode_S) cprintCcode(CDR(g));
     else if (f == take__S) cprinttake(CAR(g),CADR(g));
     else if (isbasictype(f)) {
	  put("((");
	  cprint(f);
	  put(")");
	  cprint(CAR(g));
	  put(")");
	  }
     else {
	  assert(FALSE);
	  }
     }

void cprint(node e){
     assert(e != NULL);
     switch(e->tag){
	  case type_tag: { 
	       cprinttype(e,TRUE,TRUE); 
	       return; 
	       }
	  case double_const_tag: {
	       put(e->body.double_const.contents); 
	       return;
	       }
	  case int_const_tag: {
	       put(e->body.int_const.contents); 
	       return;
	       }
	  case char_const_tag: {
	       put(intToString(e->body.char_const.contents)); 
	       return;
	       }
	  case unique_string_tag: case string_tag: {
	       put(tostring(e));
	       return;
	       }
	  case string_const_tag: {
	       printstringconst(e); 
	       return;
	       }
	  case symbol_tag: { printsymbol(e); return; }
	  case position_tag: { cprint(e->body.position.contents); return; }
	  case cons_tag: {
	       node f = CAR(e);
	       if (f == function_S) {
		    cprint(CADDR(e));
		    put(" (*)");
		    cprintlist(CADR(e));
		    }
	       else {
		 if (e->body.cons.pos.filename) curpos = &e->body.cons.pos;
		 cprintcons(f,CDR(e)); 
	         }
	       return; 
	       }
	  }
     }

static bool firsttype(node t) {
  return israwpointertypeexpr(t) || israwtypeexpr(t) || isobjecttype(t) || istaggedobjecttype(t) || isortype(t) || isarraytype(t) || istaggedarraytype(t);
}

void cprinttypes(){
     int i;
     node n;
     put("/* typedefs */\n");
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (firsttype(t)) {
	       cprinttypedef(t);
	       }
	  }
     put("/* array types */\n");
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isarraytype(t)||istaggedarraytype(t)) cprintarraydef(t);
	  }
     put("/* struct types */\n");
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isobjecttype(t) || istaggedobjecttype(t)) cprintobjectdef(t);
	  }
     put("/* basic types */\n");
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (!(firsttype(t) || isbasictype(t))) {
	       cprinttypedef(t); /* if things are too circular this won't work! */
	       }
	  }
     put("/* functions */\n");
     for (n=complete_symbol_list; n!=NULL; n=CDR(n)) {
	  node s = CAR(n);
	  node t = type(s);
	  /* assert( 0 != strcmp("node0",tostring(s)) ); */
	  if (
	      (isfunctiontype(t)
	       && (s->body.symbol.flags & constant_F)
	       && (s->body.symbol.flags & visible_F)
	       && !(s->body.symbol.flags & macro_function_F))
	      ||
	      ((s->body.symbol.flags & (export_F | import_F))
	       && t != type__T
	       && t != void_T)
	      ) {
	    cprintdefine(s,FALSE);
	    printf(";\n");
	    }
          }
     }     

static int signest = 0;

static void sigindent(){
     int i;
     for (i=0; i<signest; i++) put("     ");
     }

void dprinttype(node e){
     assert(istype(e));
     if (e->body.type.name != NULL) {
     	  dprint(e->body.type.name);
	  }
     else {
	  dprint(e->body.type.definition);
	  }
     }

void dprintlist(node e){
     if (e != NULL) while (TRUE) {
	  dprint(CAR(e));
	  e = CDR(e);
	  if (e == NULL) break;
	  put(",");
	  }
     }

static void dprintsig(node g){
     signest++;
     put("signature ");
     dprint(CAR(g)); g = CDR(g);
     put(" (\n");
     while (g != NULL) {
     	  sigindent();
     	  dprint(CAR(g)); g = CDR(g);
	  put(";\n");
	  }
     signest--;
     sigindent();
     put(")");
     }

static void dprintop(node f){
     if (isstr(f) && !validtoken(tostring(f))) put("op");
     dprint(f);
     }

static void dprintinfix(node op, node args){
     int i;
     put("(");
     for (i=1; ; ) {
	  dprint(nth(args,i));
	  i++;
	  if (i > length(args)) break;
	  put(" ");
	  dprint(op);
	  put(" ");
	  }
     put(")");
     }

static void dprintfunction(node g){
     put("function(");
     dprintlist(car(g));
     put("):");
     dprint(cadr(g));
     }

void dprintcons(node f, node g) {
     f = unpos(f);
     if (f == colon__S) { dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == threadLocal_S) { put("threadLocal "); dprint(CAR(g)); }
     else if (f == const_S) { put("const "); dprint(CAR(g)); }
     else if (f == export_S) { put("import "); dprint(CAR(g)); }
     else if (f == import_S) { put("import "); dprint(CAR(g)); }
     else if (f == use_S) { put("use "); dprint(CAR(g)); }
     else if (f == Pointer_S) { put("Pointer "); dprint(CAR(g)); }
     else if (f == atomicPointer_S) { put("atomicPointer "); dprint(CAR(g)); }
     else if (f == Type_S) { put("Type "); dprint(CAR(g)); }
     else if (f == atomicType_S) { put("atomicType "); dprint(CAR(g)); }
     else if (f == arithmeticType_S) { put("arithmeticType "); dprint(CAR(g)); }
     else if (f == integerType_S) { put("integerType "); dprint(CAR(g)); }
     else if (f == header_S) { put("header "); dprint(CAR(g)); }
     else if ( f == leftOperator_S || f == rightOperator_S || f == prefixOperator_S 
	       ) { put(tostring(f)); put(" "); dprint(CAR(g)); put(" "); dprint(CADR(g)); }
     else if (f == declarations_S) { put("declarations "); dprint(CAR(g)); }
     else if (f == colonequal__S) {dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == coloncolonequal__S) {dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == signature_S) dprintsig(g);
     else if (f == or_S) dprintinfix(f,g);
     else if (f == or_K) dprintinfix(f,g);
     else if (f == function_S) dprintfunction(g);
     else if (f == object__K) { put("{ "); dprintlist(g); put(" }"); }
     else if (f == tagged_object_K) { put("{+"); dprintlist(g); put(" }"); }
     else if (f == while_S) { put ("while("); dprint(car(g)); put(") do ("); dprint(cadr(g)); put(")"); }
     else { dprintop(f); put("("); dprintlist(g); put(")"); }
     }

void dprint(node e){
     if (e == NULL) put("NULL");
     switch(e->tag){
	  case type_tag: dprinttype(e); return;
	  case double_const_tag: put(e->body.double_const.contents); return;
	  case int_const_tag: put(e->body.int_const.contents); return;
	  case char_const_tag: put(intToString(e->body.char_const.contents)); return;
          case unique_string_tag: case string_tag: put(tostring(e)); return;
	  case string_const_tag: printstringconst(e); return;
	  case symbol_tag: dprint(e->body.symbol.name); return;
	  case position_tag: cprint(e->body.position.contents); return;
	  case cons_tag: dprintcons(CAR(e),CDR(e)); return;
          default: assert(FALSE);
	  }
     }

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
