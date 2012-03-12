/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

node car(node x){
     node y = CAR(x);
     return y;
     }

node cdr(node x){
     node y = CDR(x);
     assert(islist(y));
     return y;
     }

node caadr(node x) {
     x = CDR(x);
     x = CAR(x);
     x = CAR(x);
     return x;
     }

node caar(node x) {
     x = CAR(x);
     x = CAR(x);
     return x;
     }

node cadadr(node x) {
     x = CDR(x);
     x = CAR(x);
     x = CDR(x);
     x = CAR(x);
     return x;
     }

node cadar(node x) {
     x = CAR(x);
     x = CDR(x);
     x = CAR(x);
     return x;
     }

node cadddr(node x) {
     x = CDR(x);
     x = CDR(x);
     x = CDR(x);
     x = CAR(x);
     return x;
     }

node caddr(node x) {
     x = CDR(x);
     x = CDR(x);
     x = CAR(x);
     return x;
     }

node caaddr(node x) {
     x = CDR(x);
     x = CDR(x);
     x = CAR(x);
     x = CAR(x);
     return x;
     }

node cadr(node x) {
     x = CDR(x);
     x = CAR(x);
     return x;
     }

node cdadr(node x) {
     x = CDR(x);
     x = CAR(x);
     x = CDR(x);
     return x;
     }

node cdar(node x) {
     x = CAR(x);
     x = CDR(x);
     return x;
     }

node cdddr(node x) {
     x = CDR(x);
     x = CDR(x);
     x = CDR(x);
     return x;
     }

node cddr(node x) {
     x = CDR(x);
     x = CDR(x);
     return x;
     }

node cons(node x, node y){
     node p;
     assert(y == NULL || iscons(y));
     p = newnode(CONS,cons_tag);
     p->body.cons.car = x;
     p->body.cons.cdr = y;
     p->body.cons.pos = empty_pos;
     return p;
     }

int length(node x){
     unsigned int n = 0;
     while (x != NULL) {
	  n++;
	  x = CDR(x);
	  }
     return n;
     }

node last(node l){
     while (CDR(l) != NULL) l = CDR(l);
     return car(l);
     }

node tailend(node l){
     while (CDR(l) != NULL) l = CDR(l);
     return l;
     }

node nth(node x, unsigned int n){
     assert(n >= 1);
     while (n>1) x=CDR(x), n--;
     return car(x);
     }

node list(int n,...){
     node l, e;
     va_list ap;
     va_start(ap,n);
     if (n==0) return NULL;
     e = l = cons(va_arg(ap,node),NULL);
     n--;
     while (n>0) {
	  e->body.cons.cdr = cons(va_arg(ap,node),NULL);
	  e = e->body.cons.cdr;
	  n--;
	  }
     va_end(ap);
     /* we make n new references in this routine : n new cons cells, and
        n new references to the members of the list */
     /* the cons cells we make here do not get handed to cons(), so
        their ref counts are all == 1 */
     return l;
     }

node join(node list1, node list2){
     node l;
     if (list1 == NULL) {
	  return list2;
	  }
     if (list2 == NULL) {
	  return list1;
	  }
     l = join(CDR(list1),list2);
     push(l,CAR(list1));
     return l;
     }

node pop_1(node *listcell){
     node n, c = *listcell, rest;
     assert(c != NULL);
     n = car(c);
     rest = cdr(c);
     *listcell = rest;
     return n;
     }

node allbutone(node e) {
     assert(e != NULL);
     if (CDR(e) == NULL) return NULL;
     return cons(CAR(e),allbutone(CDR(e)));
     }

node reverse(node x){
     node y = NULL;
     while (x != NULL) {
	  node z = pop(x);
	  push(y,z);
	  }
     return y;
     }

bool equal(node x, node y){
     if (ispos(x)) x = x->body.position.contents;
     if (ispos(y)) y = y->body.position.contents;
     if (x == NULL) return y == NULL;
     else if (y == NULL) return FALSE;
     if (x->tag != y->tag) return FALSE;
     switch(x->tag){
	  case cons_tag: 
	       return equal(CAR(x),CAR(y)) && equal(CDR(x),CDR(y));
	  case unique_string_tag:
	       return x == y;
	  case symbol_tag:
	       return x == y;
	  case type_tag:
	       return typeforward(x) == typeforward(y);
	  case char_const_tag:
	       return x->body.char_const.contents == y->body.char_const.contents;
	  case string_tag:
	       return EQUAL == strcmp(x->body.string_const.characters, y->body.string_const.characters);
	  case int_const_tag:
	       return EQUAL == strcmp(x->body.int_const.contents, y->body.int_const.contents);
	  case double_const_tag:
	       return EQUAL == strcmp(x->body.double_const.contents, y->body.double_const.contents);
	  case string_const_tag:
	       return EQUAL == strcmp(x->body.string_const.characters, y->body.string_const.characters);
	  default: case position_tag:
	       assert(FALSE);
	  }
     return FALSE;
     }

bool occursin(node a, node x) {
     if (ispos(a)) a = a->body.position.contents;
     if (ispos(x)) x = x->body.position.contents;
     if (!iscons(x)) return x==a;
     while (x != NULL) {
	  if (occursin(a,CAR(x))) return TRUE;
	  x = CDR(x);
	  }
     return FALSE;
     }

bool member(node e, node l){
     while (l != NULL) {
	  if (equal(e,CAR(l))) return TRUE;
	  l = CDR(l);
	  }
     return FALSE;
     }

int memberindex(node e, node l){
     int i = 1;
     while (l != NULL) {
	  if (equal(e,CAR(l))) return i;
	  l = CDR(l);
	  i++;
	  }
     return 0;
     }

node substitute(node fromlist, node tolist, node subject) {
     node a,b,aa,bb;
     int i;
     if (subject==NULL) return NULL;
     i = memberindex(subject,fromlist);
     if (i != 0) return nth(tolist,i);
     if (!iscons(subject)) return subject;
     a = CAR(subject);	   aa = substitute(fromlist,tolist,a);
     b = CDR(subject);	   bb = substitute(fromlist,tolist,b);
     if (a==aa && b==bb) return subject;
     return cons(aa,bb);
     }

     

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
