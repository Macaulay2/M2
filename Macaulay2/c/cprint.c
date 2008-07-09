/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

void printstringconst(node p){
     char *s;
     assert(p!=NULL && p->tag == string_const_tag);
     s = p->body.string.contents;
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
     switch(e->tag){
	  case position_tag: return tostring(e->body.position.contents);
	  case symbol_tag: return tostring(e->body.symbol.name);
	  case int_const_tag: {
	       return e->body.int_const.contents;
	       }
     	  case char_const_tag: {
	       return strperm(intToString(e->body.char_const.contents));
	       }
	  case string_tag: return e->body.string.contents;
	  case string_const_tag: return e->body.string_const.contents;
	  default: assert(FALSE); return NULL;
	  }
     }

void printsymbol(node p){
     assert(p != NULL);
     assert(p->tag == symbol_tag);
     if (p->body.symbol.cprintvalue) {
	  cprint(p->body.symbol.cprintvalue);
	  }
     else if (p->body.symbol.Cname != NULL) {
	  put(p->body.symbol.Cname);
	  }
     else {
	  put(p->body.symbol.name->body.string.contents);
	  }
     }

int pcol=0;

void pput(char *s){
     while (*s) {
	  putchar(*s);
	  if ((*s) == '\n') pcol = 0;
	  else pcol++;
	  s++;
	  }
     }

void indent(int level){
     int cols = 3 * level;
     pput("\n");
     while (cols >= 8) {
	  cols -= 8;
	  pput("\t");
	  }
     while (cols >= 1) {
	  cols -= 1;
	  pput(" ");
	  }
     }

int clevel=0;
void cindent(){
     int cols = 5 * clevel;
     while (cols >= 8) {
	  cols -= 8;
	  pput("\t");
	  }
     while (cols >= 1) {
	  cols -= 1;
	  pput(" ");
	  }
     }     

static struct POS *curpos = NULL;

struct POS *pos2(node n) {
     /* this is a version of pos() which ignores the position where a
        symbol was defined, and insists on seeing the position where it
	is being used */
     /* it also ignores "define" commands */
     struct POS *p;
     while (iscons(n)) {
     	  if (CAR(n) == define_S) return NULL;
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

void foo() {}

void locn(node n){
     struct POS *p = pos2(n);
     if (p != NULL) {
	  curpos = p;
	  if (p->lineno == 83) foo();
	  }
     }

void printpos(){
     if (curpos != NULL && !noline) {
	  printf("\n# line %d \"%s\"\n",curpos->lineno,curpos->filename);
	  }
     cindent();
     }

void pprint(node p){
     if (p==NULL) {
	  pput("()");
	  return;
	  }
     switch (p->tag) {
	  case cons_tag: {
	       if (debug && p->body.cons.pos.filename != NULL) {
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
	       p = typeforward(p);
	       if (p->body.type.name != NULL) {
		    if (debug) printf("TYPE[%d]:name ",p->body.type.seqno);
	       	    pprint(p->body.type.name);
		    }
	       else if (p->body.type.definition != NULL) {
		    if (debug) printf("TYPE[%d]:definition ",p->body.type.seqno);
	       	    pprint(p->body.type.definition);
		    }
	       else {
		    printf("TYPE[%d]", p->body.type.seqno);
		    }
	       break;
	       }
	  case position_tag: {
	       if (debug && p->body.cons.pos.filename != NULL) {
	       	    printf("@%d:%d ", p->body.cons.pos.lineno, p->body.cons.pos.column);
		    }
	       pprint(p->body.position.contents);
	       break;
	       }
	  case string_tag: {
     	       pput(p->body.string.contents);
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

void d(node n){
     debug=1;
     tty();
     pp(n);
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

void cprintbracelist(node e){
     put("{");
     if (e != NULL) while (TRUE) {
	  cprint(CAR(e));
	  e = CDR(e);
	  if (e == NULL) break;
	  put(",");
	  }
     put("}");
     }

void cprintsemi(node e){
     while (e != NULL) {
	  node f = CAR(e);
	  node g = iscons(f) ? CAR(f) : NULL;
     	  locn(f);
	  printpos();
	  cindent();
	  cprint(f);
	  if (!(g==block_K || g==defun_S || (!gc && g==define_destroy_S))) {
	       put(";\n");
	       }
	  e = CDR(e);
	  }
     }

void cprinttypeseqno(node t){
     printf("%d",typeforward(t)->body.type.seqno);
     }

void cprintorstructdef(node t){
     bool composite = t->body.type.composite;
     int i;
     node m;
     if (composite) {
	  printf("struct SC%s%d_", couldbenull(t) ? "N" : "",
	       t->body.type.seqno);
	  printf(" {unsigned short type_; object_ ptr_;};\n");
	  }
     else {
	  bool had_a_member = FALSE;
	  printf("struct S%s%d_", couldbenull(t) ? "N" : "",
	       t->body.type.seqno);
	  put(" {");
	  if (!gc) {
	       put("unsigned int refs_;");
	       had_a_member = TRUE;
	       }
	  for (i=1, m=t->body.type.commons; m != NULL; m = CDR(m)) {
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
     }

void cprintarraydef(node t){
     node m = typedeftail(t);
     node typ = CAR(m);
     node len;
     printf("struct S%d_",t->body.type.seqno);
     if (length(m)==2) len = CADR(m); else len = NULL;
     put(" {");
     if (!gc) put("unsigned int refs_;");
     put("int len_;");
     cprint(typ);
     put(" array_[");
     if (len!=NULL) cprint(len); else put("1");
     put("];};\n");
     }

void cprintobjectdef(node t){
     int i;
     node m;
     bool had_a_member = FALSE;
     printf("struct S%d_ {",t->body.type.seqno);
     if (isobjecttype(t)) {
	  if (!gc) {
	       put("unsigned int refs_;");
	       had_a_member = TRUE;
	       }
	  }
     for (i=1, m=typedeftail(t); m != NULL; m = CDR(m)) {
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

void cprinttype(node t, bool withstar, bool usename){
     assert(istype(t));
     assert(t != undefined_T);
     t = typeforward(t);
     if (t->body.type.Cname != NULL) {
	  put(t->body.type.Cname);
	  }
     else if (t->body.type.name != NULL && withstar && usename) {
	  cprint(t->body.type.name);
	  }
     else if (isortype(t)) {
	  printf("struct S%s%s%d_", 
	       t->body.type.composite ? "C" : "", 
	       couldbenull(t) ? "N" : "",
	       t->body.type.seqno);
	  if (withstar && !iscompositeortype(t)) put(" *");
	  }
     else if (isarraytype(t)) {
	  printf("struct S%d_",t->body.type.seqno);
	  if (withstar) put(" *");
	  }
     else if (isobjecttype(t)) {
	  printf("struct S%d_",t->body.type.seqno);
	  if (withstar) put(" *");
     	  }
     else if (t->body.type.definition != NULL) {
	  cprint(t->body.type.definition);
	  }
     else assert(FALSE);
     return;
     }

void cprintsizeof(node e){
     node t = type(e);
     put("(sizeof(");
     cprinttype(t,FALSE,FALSE);
     put(")");
     if (isarraytype(t)) {
	  node m = typedeftail(t);
	  if (length(m)==1) {
	       node typ = CAR(m);
	       put(" + (");
	       cprint(e);
	       put("->len_-1)*sizeof(");
	       cprint(typ);
	       put(")");
	       }
	  }
     put(")");
     }

void cprintsomesizeof(node t, node arraylen){
     assert(istype(t));
     put("sizeof(");
     cprinttype(t,FALSE,FALSE);
     put(")");
     if (isarraytype(t)) {
	  node m = typedeftail(t);
	  if (length(m)==1) {
	       node typ = CAR(m);
	       put(" + (");
	       cprint(arraylen);
	       put("-1)*sizeof(");
	       cprint(typ);
	       put(")");
	       }
	  else assert(arraylen == NULL);
	  }
     }

#define CLEAR_MEMORY FALSE

void cprintgetmem(node g){
  node s = CAR(g);
  node t = type(s);
  cprint(s);
  put(" = ");
  put("(");
  cprint(t);
  put(") ");
  if (do_memstats) {
    put("getmem"); 
    put("(");
    cprintsomesizeof(t, length(g)==2 ? CADR(g) : NULL);
    put(")");
  }
  else {
    if (gc) {
      if (atomic(t)) put("GC_MALLOC_ATOMIC");
      else {
#if CLEAR_MEMORY
	put("GC_malloc_clear"); /* see comment below! */
#else
	put("GC_MALLOC");
#endif
      }
    }
    else put("malloc");
    put("(");
    cprintsomesizeof(t, length(g)==2 ? CADR(g) : NULL);
    put(");\n");
#if CLEAR_MEMORY
    /* we write GC_malloc_clear to include a test for return value zero */
    if (!( gc && !atomic(t))) 
#endif
      {
      put("     if (0 == ");
      cprint(s);
      put(") outofmem()");
      }
  }
}

void cprintdestroy(node t) {
     node w;
     if (gc) return;
     w = typedeftail(t);
     if (isortype(t) && couldbenull(t) && length(w)==2) {
	  node u;
     	  u = car(w);
	  if (u == null_T) u = cadr(w);
     	  printf("destroy");
     	  cprinttypeseqno(u);
     	  put("((");
     	  cprint(u);
	  put(")");
	  }
     else {
     	  printf("destroy");
     	  cprinttypeseqno(t);
     	  put("(");
	  }
     }

void cprintrelease(node s) {
     node t;
     bool cbnull;
     if (gc) return;
     t = type(s);
     assert(reservable(t));
     cbnull = isortype(t) && couldbenull(t);
     locn(s);
     printpos();
     if (do_refctchks) {
	  put("if (");
	  if (cbnull) {
	       cprint(s);
	       put(" != 0  &&  ");
	       }
	  put("0 == ");
	  cprint(s);
	  put("->refs_) fatalrefctcheck(__FILE__,__LINE__,-1);\n");
	  printpos();
	  put("     ");
	  }
     if (cbnull) put("if ("), cprint(s), put(" != 0) { ");
     put("if (0 == --");
     cprint(s);
     put("->refs_) ");
     cprintdestroy(t);     cprint(s);     put(")");
     if (do_refctchks) {put("; ");cprint(s);put(" = 0");}
     if (cbnull) put(";}");
     }

void cprintreleasec(node s) {
     node t;
     bool cbnull;
     if (gc) return;
     t = type(s);
     assert(reservable(t));
     locn(s);
     printpos();
     cbnull = couldbenull(t);
     if (do_refctchks) {
	  put("if (");
	  if (cbnull) {
	       cprint(s);
	       put(".ptr_ != 0  &&  ");
	       }
	  put("0 == ");
	  cprint(s);
	  put(".ptr_->refs_) fatalrefctcheck(__FILE__,__LINE__,-1);\n");
	  printpos();
	  put("     ");
     	  }
     if (cbnull) put("if ("), cprint(s), put(".ptr_ != 0) { ");
     put("if (0 == --");
     cprint(s);
     put(".ptr_->refs_) ");
     cprintdestroy(t);     cprint(s);     put(")");
     if (do_refctchks) {put("; ");cprint(s);put(".ptr_ = 0");}
     if (cbnull) put(";}");
     }

void cprintreserve(node g){
     node s;
     if (gc) return;
     s = CAR(g);
     locn(s);
     printpos();
     cprint(s);
     put("->refs_++");
     }

void cprintreserven(node g){
     node s;
     if (gc) return;
     s = CAR(g);
     locn(s);
     printpos();
     put("if (");
     cprint(s);
     put(" != 0) ");
     cprint(s);
     put("->refs_++");
     }

void cprintreservec(node g){
     node s;
     if (gc) return;
     s = CAR(g);
     locn(s);
     printpos();
     cprint(s);
     put(".ptr_->refs_++");
     }

void cprintreservenc(node g){
     node s;
     if (gc) return;
     s = CAR(g);
     locn(s);
     printpos();
     put("if (");
     cprint(s);
     put(".ptr_ != 0) ");
     cprint(s);
     put(".ptr_->refs_++");
     }

void cprinttypedef(node t) {
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
	       if (oldc) put("()");
	       else cprintlist(functionargtypes(t));
	       }
	  else {
	       cprinttype(t,TRUE,FALSE);
	       put(" ");
	       cprint(t->body.type.name);
	       }
	  put(";\n");
	  }
     }

void cprintdefine(node t) {
     node typ = type(t);
     t = unpos(t);
     assert(issym(t));
     assert(!istype(t));
     if (t->body.symbol.flags & import_F) put("extern ");
     else if (!(t->body.symbol.flags & export_F)
	  && 
	  (
	       (t->body.symbol.flags & global_F)
	       || (
	       	    isfunctiontype(typ)
	       	    &&
	       	    (t->body.symbol.flags & constant_F)
		    )
	       )
	  &&
	  !(t->body.symbol.flags & visible_F)
	  ) {
	  put("static ");
	  }
     if (isfunctiontype(typ)) {
	  if (t->body.symbol.flags & macro_F) return;
	  cprint(functionrettype(typ));
	  put(" ");
	  if (t->body.symbol.flags & constant_F) {
	       cprint(t);
	       }
	  else {
	       put("(*");
	       cprint(t);
	       put(")");
	       }
	  if (oldc) put("()");
	  else cprintlist(functionargtypes(typ));
	  }
     else {
	  cprint(typ);
	  put(" ");
	  cprint(t);
	  }
     }

void cprintdeclaredestroy(node t) {
     if (gc) return;
#if 0
     if (isortype(t) && couldbenull(t) && length(typedeftail(t))==2) {
	  return;
	  }
#endif
     if (gcc) put("__inline__ ");
     put("static ");
     if (oldc) {
     	  printf("void destroy%d();\n",t->body.type.seqno);
	  }
     else {
     	  printf("void destroy%d(",t->body.type.seqno);
     	  cprint(t);
     	  put(");\n");
	  }
     }

node cleaner(node t) {
     node slist;
     for (slist = cleaners; slist != NULL; slist=cdr(slist)) {
	  node sym = car(slist);
	  if (equal(t,car(functionargtypes(type(sym))))) return sym;
	  }
     return NULL;
     }

void cprintdefinedestroy(node t) {
     node m;
     /* define the destroy routine */
     if (gc) return;
     if (gcc) put("__inline__ ");
     put("static ");
     if (oldc) {
     	  printf("void destroy%d(p_)\n",t->body.type.seqno);
     	  cprint(t);
     	  put(" p_;\n");
	  }
     else {
     	  printf("void destroy%d(",t->body.type.seqno);
     	  cprint(t);
     	  put(" p_)\n");
	  }
     put("{\n");
     {
	  node clean = cleaner(t);
	  if (clean != NULL) {
	       put("    p_");
	       if (iscompositeortype(t)) put(".ptr_");
	       put("->refs_++;\n");
	       put("    "); cprint(clean); put("(p_);\n");
	       put("    if (0 < --p_");
	       if (iscompositeortype(t)) put(".ptr_");
	       put("->refs_) return;\n");
	       }
	  }
     if (isortype(t)) {
	  int i, j, nmems, numnulls = 0, numnonnulls = 0;
	  for (m=typedeftail(t), nmems=length(m); m!=NULL; m=CDR(m)) {
	       node typ = CAR(m);
	       if (typ == null_T) {
		    numnulls ++;
		    continue;
		    }
	       }
	  for (i=0,j=1, m=typedeftail(t); m != NULL; m = CDR(m),j++) {
	       node typ = CAR(m);
	       if (typ == null_T) continue;
	       numnonnulls ++;
	       put("    ");
	       if (i!=0) put("else ");
	       if (numnulls + numnonnulls < nmems) {
		    i++;
		    put("if (p_");
		    if (t->body.type.composite) put(".");
		    else put("->");
		    printf("type_ == %d) ",j);
		    }
	       put("destroy");
	       cprinttypeseqno(typ);
	       put("((");
	       cprint(typ);
	       put(")p_");
	       if (t->body.type.composite) put(".ptr_");
	       put(");\n");
	       }
	  }
     else if (isarraytype(t)) {
	  node len, typ;
	  m = typedeftail(t);
	  len = length(m)==2 ? CADR(m) : NULL;
	  typ = CAR(m);
	  if (reservable(typ)) {
	       put("    int i;\n");
	       put("    for (i=0; i<");
	       if (len==NULL) {
		    put("p_->len_");
		    }
	       else {
		    cprint(len);
		    }
	       put("; i++) {\n");
	       put("        if (");
	       if (isortype(typ) && couldbenull(typ)) {
		    if (iscompositeortype(typ)) {
		    	 put("0 != p_->array_[i].ptr_ && ");
			 }
		    else{
		    	 put("0 != p_->array_[i] && ");
			 }
		    }
	       if (iscompositeortype(typ)) {
	       	    put("0 >= --p_->array_[i].ptr_->refs_) {\n");
		    }
     	       else {
	       	    put("0 >= --p_->array_[i]->refs_) {\n");
		    }
	       put("            destroy");
	       cprinttypeseqno(typ);
	       put("(p_->array_[i]);\n");
	       put("        }\n");
	       put("    }\n");
	       }
#ifndef DEBUG
	  if (do_memstats) {
	       put("    numchunks[1]++;\n    numbytes[1]+=sizeof(");
	       cprinttype(t,FALSE,TRUE);
	       put(")");
	       if (isarraytype(t) && len == NULL) {
		    put(" + (p_->len_-1)*sizeof(");
		    cprint(typ);
		    put(")");
		    }
	       put(";\n");
	       }
	  put("    free((char *)p_);\n");
#else     
	  put("    destroy((char *)p_,sizeof(");
	  cprinttype(t,FALSE,TRUE);
	  put(")");
	  if (isarraytype(t) && len == NULL) {
	       put(" + (p_->len_-1)*sizeof(");
	       cprint(typ);
	       put(")");
	       }
	  put(");\n");
#endif
	  }
     else {
	  assert(isobjecttype(t));
	  for (m = typedeftail(t); m != NULL; m = CDR(m)) {
	       node typ = CADAR(m);
	       if (!reservable(typ)) continue;
	       }
	  for (m = typedeftail(t); m != NULL; m = CDR(m)) {
	       node typ = CADAR(m);
	       if (!reservable(typ)) continue;
	       if (reservable(typ)) {
		    put("    if (");
		    if (isortype(typ) && couldbenull(typ)) {
			 put("0 != p_->");
			 cprint(CAAR(m));
			 if (iscompositeortype(typ)) put(".ptr_");
			 put("  &&  ");
			 }
		    put("0 >= --p_->");
		    cprint(CAAR(m));
		    if (iscompositeortype(typ)) put(".ptr_");
		    put("->refs_) destroy");
		    cprinttypeseqno(typ);
		    put("(p_->");
		    cprint(CAAR(m));
		    put(");\n");
		    }
	       else {
		    assert(FALSE);
		    }
	       }
#ifndef DEBUG
     	  if (do_memstats) {
	       put("    numchunks[1]++;\n    numbytes[1]+=sizeof(");
	       cprinttype(t,FALSE,TRUE);
	       put(");\n");
	       }
	  if (!gc) put("    free((char *)p_);\n");
#else
     	  if (!gc) {
	       put("    destroy((char *)p_,sizeof(");
	       cprinttype(t,FALSE,TRUE);
	       put("));\n");
	       }
#endif
	  }
     put("}\n");
     }

void cprintreturn(node s){
     if (length(s)==0) {
	  put("return");
	  }
     else {
     	  put("return ");
     	  cprint(CAR(s));
	  }
     }

void cprintassign(node s, node t){
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


void cprintdefun(node g) {
     node fun = CAAR(g);
     node args = CDAR(g);
     node funtype = type(fun);
     node rettype = functionrettype(funtype);
     assert(issym(fun));
     locn(g);
     printpos();
     cindent();
     if (!(fun->body.symbol.flags & export_F)
	  &&
	  !(fun->body.symbol.flags & visible_F)
	  ) put("static ");
     cprint(rettype);
     put(" ");
     cprint(fun);
     if (oldc) {
	  put("(");
	  if (args != NULL) while (TRUE) {
	       node w = CAR(args);
	       cprint(w);
	       args = CDR(args);
	       if (args == NULL) break;
	       put(",");
	       }
	  put(")");
	  args = CDAR(g);
	  if (args != NULL) while (TRUE) {
	       node w = CAR(args);
	       cprinttypevar(type(w),w);
	       put(";");
	       args = CDR(args);
	       if (args == NULL) break;
	       }
	  }
     else {
	  put("(");
	  if (args != NULL) while (TRUE) {
	       node w = CAR(args);
	       cprinttypevar(type(w),w);
	       args = CDR(args);
	       if (args == NULL) break;
	       put(",");
	       }
	  put(")");
	  }
     put("{\n");
     clevel++;
     if (do_countstats) {
     	  printpos();
     	  put("     static int count_ = 0;\n");
     	  printpos();
	  put("     int junk_ = count_++ != 0 ? 0 : ");
	  printf(        "register_fun(&count_,\"%s\",%d,\"", 
	       (fun->body.symbol.pos.filename==NULL?
		    "(null)":fun->body.symbol.pos.filename),
	       fun->body.symbol.pos.lineno);
	  cprint(fun);
	  put(	   	 "\");\n");
	  }
     cprintsemi(CDDR(g));
     clevel--;
     cindent();
     printpos();
     put("}\n");
     }

void cprintgoto(node l){
     put("goto ");
     cprint(l);
     }

void cprintif(node g){
     put("if (");
     cprint(CAR(g));
     put(") ");
     cprint(CADR(g));
     if (length(g) == 3) {
	  put("; else ");
	  cprint(CADDR(g));
	  }
     }

void cprintlabel(node l){
     cprint(l);
     put(":");
     }

void cprintisnull(node e){
     put("0 == ");
     cprint(e);
     }

void cprintcast(node typ, node e){
     put("((");
     cprint(typ);
     put(")");
     cprint(e);
     put(")");
     }

void cprintarraycheck(node indx, node len){
     struct POS *p;
     if (!arraychks) return;
     locn(len);
     locn(indx);
     p = pos(indx);
     printpos();
     cindent();
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

void cprintarraylencheck(node len){
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

void cprintarraytake(node arr, node indx){
     cprint(arr);
     put("->array_[");
     cprint(indx);
     put("]");
     }     

void cprinttake(node f, node e){
     cprint(f);
     put("->");
     cprint(e);
     }

void cprintpart(node f, node e){
     cprint(f);
     put(".");
     cprint(e);
     }

void cprintprefix(node fun, node x){
     put("(");
     cprint(fun);
     put(" ");
     cprint(x);
     put(")");
     }

void cprintpostfix(node fun, node x){
     put("(");
     cprint(x);
     put(" ");
     cprint(fun);
     put(")");
     }

void cprintinfix(node fun, node x, node y){
     put("(");
     cprint(x);
     put(" ");
     cprint(fun);
     put(" ");
     cprint(y);
     put(")");
     }

void cprintfuncall(node fun, node args){
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

void cprintCcode(node s){
     while (s != NULL) {
	  node a = CAR(s);
	  if (a->tag == string_const_tag) put_unescape(a->body.string.contents);
	  else cprint(a);
	  s = CDR(s);
	  }
     }

void cprintcons(node f, node g) {
     if (ispos(f)) f = f->body.position.contents;
     if (f == getmem_S) cprintgetmem(g);
     else if (f == array_len_check_S) cprintarraylencheck(CAR(g));
     else if (f == array_check_S) cprintarraycheck(CAR(g),CADR(g));
     else if (f == array_take_S) cprintarraytake(CAR(g),CADR(g));
     else if (f == assign_S) cprintassign(CAR(g),CADR(g));
     else if (f == cast_S) cprintcast(CAR(g),CADR(g));
     else if (f == define_S) cprintdefine(CAR(g));
     else if (f == defun_S) cprintdefun(g);
     else if (f == equal_S) cprintinfix(f,CAR(g),CADR(g));
     else if (f == unequal_S) cprintinfix(f,CAR(g),CADR(g));
     else if (f == funcall_S) cprintfuncall(CAR(g),CDR(g));
     else if (f == goto_S) cprintgoto(CAR(g));
     else if (f == if_S) cprintif(g);
     else if (f == infix_S) cprintinfix(CAR(g),CADR(g),CADDR(g));
     else if (f == isnull_S) cprintisnull(CAR(g));
     else if (f == label_S) cprintlabel(CAR(g));
     else if (f == brace_list_S) cprintbracelist(g);
     else if (f == prefix_S) cprintprefix(CAR(g),CADR(g));
     else if (f == postfix_S) cprintpostfix(CAR(g),CADR(g));
     else if (f == release_S) cprintrelease(CAR(g));
     else if (f == reserve_S) cprintreserve(g);
     else if (f == reserven_S) cprintreserven(g);
     else if (f == releasec_S) cprintreleasec(CAR(g));
     else if (f == reservec_S) cprintreservec(g);
     else if (f == reservenc_S) cprintreservenc(g);
     else if (f == return_S) cprintreturn(g);
     else if (f == sizeof_S) cprintsizeof(CAR(g));
     else if (f == Ccode_S) cprintCcode(CDR(g));
     else if (f == take_S) cprinttake(CAR(g),CADR(g));
     else if (f == part_S) cprintpart(CAR(g),CADR(g));
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
	  case string_tag: {
	       put(e->body.string.contents); 
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
	       else cprintcons(f,CDR(e)); 
	       return; 
	       }
	  }
     }

void cprinttypes(){
     int i;
     node n;
     if (gc) {
	  put("typedef void * object_;\n");
	  }
     else {
     	  put("typedef struct OBJECT_ {unsigned int refs_;} * object_;\n");
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isobjecttype(t) || isortype(t) || isarraytype(t)) {
	       cprinttypedef(t);
	       }
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isortype(t) && t->body.type.composite) cprintorstructdef(t);
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isarraytype(t)) cprintarraydef(t);
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isobjecttype(t)) cprintobjectdef(t);
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isortype(t) && !t->body.type.composite) cprintorstructdef(t);
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (!(isobjecttype(t) || isortype(t) || isarraytype(t) || isbasictype(t))) {
	       cprinttypedef(t); /* if things are too circular this won't work! */
	       }
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isobjecttype(t) || isortype(t) || isarraytype(t)) {
	       cprintdeclaredestroy(t);
	       }
	  }
     for (n=complete_symbol_list; n!=NULL; n=CDR(n)) {
	  node s = CAR(n);
	  node t = type(s);
	  if (isfunctiontype(t) && (s->body.symbol.flags & constant_F)) {
	       if (!(s->body.symbol.flags & macro_F)) {
		    cprintdefine(s);
		    put(";\n");
		    }
	       }
	  }
     for (i=0; i<numtypes; i++) {
	  node t = typelist[i];
	  if (isobjecttype(t) || isortype(t) || isarraytype(t)) {
	       cprintdefinedestroy(t);
	       }
	  }
     }     

static int signest = 0;
void dprinttype(node);
void dprintcons(node,node);
void dprint(node);
void dprintlist(node);

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

void dprintsig(node g){
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

void dprintop(node f){
     if (isstr(f) && !validtoken(f->body.string.contents)) put("op");
     dprint(f);
     }

void dprintinfix(node op, node args){
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

void dprintfunction(node g){
     put("function(");
     dprintlist(car(g));
     put("):");
     dprint(cadr(g));
     }

void dprintcons(node f, node g) {
     f = unpos(f);
     if (f == colon_S) { dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == export_S) { put("import "); dprint(CAR(g)); }
     else if (f == import_S) { put("import "); dprint(CAR(g)); }
     else if (f == use_S) { put("use "); dprint(CAR(g)); }
     else if (f == colonequal_S) {dprint(CAR(g)); dprint(f); dprint(CADR(g));}
     else if (f == signature_S) dprintsig(g);
     else if (f == or_S) dprintinfix(f,g);
     else if (f == or_K) dprintinfix(f,g);
     else if (f == function_S) dprintfunction(g);
     else if (f == object_K) { put("{"); dprintlist(g); put("}"); }
     else { dprintop(f); put("("); dprintlist(g); put(")"); }
     }

void dprint(node e){
     assert(e != NULL);
     switch(e->tag){
	  case type_tag: { dprinttype(e); return; }
	  case double_const_tag: {put(e->body.double_const.contents); return;}
	  case int_const_tag: { put(e->body.int_const.contents); return; }
	  case char_const_tag: { 
	       put(intToString(e->body.char_const.contents)); return; }
	  case string_tag: { put(e->body.string.contents); return; }
	  case string_const_tag: { printstringconst(e); return; }
	  case symbol_tag: { dprint(e->body.symbol.name); return; }
	  case position_tag: { cprint(e->body.position.contents); return; }
	  case cons_tag: { dprintcons(CAR(e),CDR(e)); return; }
	  }
     }
