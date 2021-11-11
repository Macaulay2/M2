/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

node integer(int n){
     node q = newnode(INT_CONST,int_const_tag);
     q->body.int_const.contents = strperm(intToString(n));
     return q;
     }

node IntegerN(char *s, unsigned int len) {
     node q = newnode(INT_CONST,int_const_tag);
     q->body.int_const.contents = strnperm(s,len);
     return q;
     }

node positionof(node n){
     node t;
     t = newnode(POSITION,position_tag);
     t->body.position.contents = n;
     t->body.position.pos.filename = cur.filename;
     t->body.position.pos.lineno = cur.lineno;
     t->body.position.pos.column = cur.column;
     return t;
     }     

node SETPOS(node e, struct POS *p){
     /* upper case means it returns an uncounted reference */
     if (p==NULL || !iscons(e)) return e;
     e->body.cons.pos = *p;
     return e;
     }

node setpos(node e, struct POS *p){
     if (p==NULL || !iscons(e)) return e;
     assert(p->lineno != 0 || p->column != 0);
     e->body.cons.pos = *p;
     return e;
     }

node repos(node p,node n){
     node t;
     t = newnode(POSITION,position_tag);
     t->body.position.contents = n;
     t->body.position.pos = p->body.position.pos;
     return t;
     }     

node enpos(node e, struct POS *p){
     node t;
     if (p == NULL) return e;
     if (e == NULL) return NULL;
     t = newnode(POSITION,position_tag);
     t->body.position.contents = e;
     t->body.position.pos = *p;
     return t;
     }     

struct POS *pos(node n) {
     struct POS *p;
     while (iscons(n)) {
	  if (n->body.cons.pos.filename != NULL) return &n->body.cons.pos;
	  p = pos(CAR(n));
	  if (p != NULL) return p;
	  n = CDR(n);
	  }
     return (
	  ispos(n) ? &n->body.position.pos 
	  : issym(n) && n->body.symbol.pos.filename != NULL ? &n->body.symbol.pos 
	  : istype(n) && n->body.type.name != NULL ? pos(n->body.type.name)
	  : NULL );
     }

node unpos(node e){
     while (ispos(e)) e = e->body.position.contents;
     return e;
     }

node typeforward(node e){
     assert(istype(e));
     while (e->body.type.forward != NULL) {
       assert(istype(e->body.type.forward));
       e = e->body.type.forward;
     }
     return e;
     }

bool forwardtype(node old,node newn){
     assert(istype(old));
     assert(istype(newn));
     assert(old->body.type.flags & deferred_F);
     assert(old != deferred__T);
     old->body.type.flags &= ~deferred_F;
     if ((old->body.type.flags & should_be_pointer_F) && !ispointertype(newn)) {
       errorpos(newn,"expected a pointer type");
       return FALSE;
     }
     old->body.type.flags &= ~should_be_pointer_F;
     if ((old->body.type.flags & should_be_tagged_F) && !istaggedtype(newn)) {
       errorpos(newn,"expected a tagged pointer type");
       return FALSE;
     }
     old->body.type.flags &= ~should_be_tagged_F;
     old->body.type.forward = typeforward(newn);
     return TRUE;
     }

node typedefinition(node t){
     return typeforward(t)->body.type.definition;
     }

node typedeftail(node t){
     return cdr(typedefinition(t));
     }

node arrayElementType(node arraytype){
     node m, n;
     if (istype(arraytype)) m = typedeftail(arraytype);
     else m = cdr(arraytype);
     n = car(m);
     return typeforward(n);
     }

node arrayElementLength(node arraytype){
     node m;
     if (istype(arraytype)) m = typedeftail(arraytype);
     else m = cdr(arraytype);
     return length(m) == 1 ? NULL : cadr(m);
     }

node membertype(node structtype, node membername) {
     node m;
     membername = unpos(membername);
     if (membername == len_S) return int_T;
     if (membername == type__S) return int_T;
     if (istype(structtype)) m = typedeftail(structtype);
     else m = CDR(structtype);
     if (ispos(membername)) membername = membername->body.position.contents;
     while (m != NULL) {
	  if (equal(CAAR(m),membername)) {
	       node t = typeforward(CADAR(m));
	       return t;
	       }
	  m = CDR(m);
	  }
     return NULL;
     }

node ormembertype(node ortype, node membername) {
     node m;
     assert(isortype(ortype));
     membername = unpos(membername);
     m = ortype->body.type.commons;
     while (m != NULL) {
	  if (equal(CAAR(m),membername)) {
	       node t = typeforward(CADAR(m));
	       return t;
	       }
	  m = CDR(m);
	  }
     return NULL;
     }

node type(node e){		/* assume e is checked previously */
     /* this returns a unique TYPE */
     if (e == NULL) return void_T;
     again:
     switch(e->tag) {
	  case position_tag: e = e->body.position.contents; goto again;
	  case symbol_tag: return e->body.symbol.type;
     	  case string_const_tag: /* not implemented yet */ return bad_or_undefined_T;
     	  case char_const_tag: return char_T;
     	  case int_const_tag: return int_T;
     	  case double_const_tag: return double_T;
     	  case string_tag: assert(FALSE); 
     	  case unique_string_tag: assert(FALSE); /* was return bad_or_undefined_T; */
	  case cons_tag: {
	       node h, ht;
     	       h = unpos(CAR(e));
	       if (h->tag == unique_string_tag) {
		    if (h == equalequal__S || h == unequal_S) return bool_T;
		    if (h == cast__S) {
			 assert(istype(CADR(e)));
			 return cadr(e);
			 }
		    if (h == function_S) return type__T;
		    if (h == funcall__S || h == prefix__S || h == infix__S) {
			 return functionrettype(type(CADR(e)));
			 }
		    if (h == take__S) {
			 return membertype(type(CADR(e)),CADDR(e));
			 }
		    if (h == array_take_S) {
			 return arrayElementType(type(CADR(e)));
			 }
		    if (h == return_S) return returns_T;
		    if (h == exits_S) return exits_T;
		    if (h == Ccode_S) return cadr(e);
		    assert(FALSE);
		    }
	       ht = type(h);
	       if (ht == type__T) return totype(h);
	       if (ht == keyword_T) {
		    node w = ispos(h) ? h->body.position.contents : h;
		    if (w == block__K) return void_T;
     	       	    if (w == blockn__K) {
			 if (length(e) < 2) return void_T;
			 return type(last(e));
			 }
		    if ( w == object__K || w == tagged_object_K || w == array_K || w == tarray_K || w == or_K) {
			 return type__T;
			 }
		    if (w == label__S) return void_T;
		    if (w == goto__S) return void_T;
		    assert(FALSE); /* there must be some other keywords! */
		    }
	       ht = ht->body.type.definition;
	       if (iscons(ht)) {
		    if (equal(CAR(ht),function_S)) {
			 assert(FALSE);
		    	 return caddr(ht);
			 }
		    else {
			 assert(FALSE);
			 return NULL;
			 }
		    }
	       assert(FALSE); return NULL;
	       }
	  case type_tag: return type__T;
	  }
     assert(FALSE);
     return NULL;
     }

node chktypelist(node e,scope v) {
     node l = NULL, m;
     if (ispos(e)) e = e->body.position.contents;
     while (e != NULL) {
	  push(l,chktype(CAR(e),v));
	  e = CDR(e);
	  }
     m = reverse(l);
     return m;
     }

node newtype(node definition, node name, bool basic_type){
     node s = newnode (TYPE,type_tag);
     s->body.type.definition = definition;
     s->body.type.name = name;
     if (basic_type) s->body.type.flags |= basic_type_F;
     s->body.type.seqno = -1;
     s->body.type.runtime_type_code = -1;
     return s;
     }

node *typelist = NULL;		/* array of all unique types */
int numtypes = 0;
int typelistsize = 0;
node *newtypeslist;		/* array of the new ones */
int numnewtypes;
struct DISTIN { 
     struct PAIR {int i; int j; struct PAIR *next;} *listp;
				/*list of pairs that would be distinguishable*/
				/* if this one is */
     bool distinguishable; 
     }
    **ttable,		/* numnewtypes by (numtypes+numnewtypes) */
    distinguished = {NULL,TRUE};

void printtypelist(){
     int i;
     pput("Type List\n");
     for (i=0; i<numtypes; i++) {
	  printf("%3d : %3d : ",i,typelist[i]->body.type.seqno);
	  if (typelist[i]->body.type.name != NULL
	       && typelist[i]->body.type.definition != NULL) {
	       pprint(typelist[i]->body.type.name);
	       put(" : ");
	       pp(typelist[i]->body.type.definition);
	       }
	  else pp(typelist[i]);
	  }
     pput("\n");
     }

node thetype(int i){
     assert(i >= 0);
     if (i < numtypes) return typelist[i];
     i -= numtypes;
     assert(i < numnewtypes);
     return newtypeslist[i];
     }

static struct DISTIN *table(int i, int j) {
     if (i < j) {
	  int tmp = i;
	  i = j;
	  j = tmp;
	  }
     assert(j >= 0);
     if (i < numtypes) return &distinguished;
     assert(i < numnewtypes + numtypes);
     return &ttable[i-numtypes][j];
     }

static bool distinguishable(int i, int j){
     assert(i >= 0);
     assert(j >= 0);
     if (i == j) return FALSE;
     return table(i,j)->distinguishable;
     }

static void mark(int i, int j){
     struct DISTIN *d = table(i,j);
     struct PAIR *l = d->listp;
     assert(!d->distinguishable || d->listp==NULL);
     d->listp = NULL;
     d->distinguishable = TRUE;
     while (l != NULL) {
	  struct PAIR p;
	  p = *l;
	  mark(p.i,p.j);
	  l = p.next;
	  }
     }

static void appendlt(int i, int j, int ii, int jj) {
     /* assert that ii and jj would be distinguishable if i and j were */
     struct PAIR *p;
     struct DISTIN *d;
     if (i == j) return;
     p = new(struct PAIR);	/* this gets freed in mark() */
     d = table(i,j);
     p->i = ii;
     p->j = jj;
     p->next = d->listp;
     d->listp = p;
     }

void interntype(node t){
     assert(t->tag == type_tag);
     if (numtypes >= typelistsize) {
	  if (typelistsize == 0) {
	       typelistsize = 800;
	       typelist = newarray(node,typelistsize);
	       }
	  else {
	       int newtypelistsize = 2 * typelistsize;
	       node *newtypelist = newarray(node,newtypelistsize);
	       int i;
	       for (i=0; i<numtypes; i++) newtypelist[i] = typelist[i];
	       typelist = newtypelist;
	       typelistsize = newtypelistsize;
	       }
	  }
     typelist[numtypes] = t;
     t->body.type.seqno = numtypes;
     numtypes++;
     if (isortype(t)) {
	  int i, nonnulls = 0;
	  node m;
	  node commons = NULL;
	  for (i=1; ; i++) {
	       node common = NULL;
	       for (m = typedeftail(t); m != NULL; m = CDR(m)) {
		    node n, u = CAR(m);
	       	    if (u == null_T) goto out;
		    if (!(isobjecttype(u) || istaggedobjecttype(u))) goto out;
		    n = typedeftail(u);
		    if (i > length(n)) goto out;
		    u = nth(n,i);
		    if (common == NULL) common = u;
		    else if (!equal(common,u)) {
			 goto out;
			 }
		    }
	       if (common == NULL) break;
	       push(commons,common);
	       }
	  out:
	  commons = reverse(commons);
	  t->body.type.commons = commons;
	  for (m = typedeftail(t); m != NULL; m = CDR(m)) {
	       if (CAR(m) != null_T) nonnulls ++;
	       }
	  if (nonnulls >= 2) t->body.type.flags |= composite_F;
	  }
     }

node ExpandType(node t, node *f) {
     /* t should be a type expression that might need expanding.  Its expanded
        form gets returned, and also put on the top of the list f
	unless it's already a type or basic type */
     switch(t->tag) {
	  case position_tag: return ExpandType(t->body.position.contents,f);
     	  case type_tag: return t;
     	  case symbol_tag: {
	       if (t->body.symbol.type == type__T) {
	  	    assert(istype(t->body.symbol.value));
		    return t->body.symbol.value;
		    }
	       if (t == bad__K) return bad_or_undefined_T;
	       assert(FALSE); return NULL;
	       }
	  case cons_tag: {
	       node fun = CAR(t);
	       if (ispos(fun)) fun = fun->body.position.contents;
	       t = CDR(t);
	       if (fun == or_K) {
		    /* here we should sort! */
		    /* we should also merge sub-or's in, and eliminate
		       duplicates */
		    /* we really only handle (or null (object)) now! */
		    node newN = NULL;
		    node mems = NULL;
		    while (t != NULL) {
			 node u = ExpandType(CAR(t),f);
			 push(mems,u);
			 t = CDR(t);
			 }
		    apply(reverse,mems);
		    newN = newtype(cons(fun,mems),NULL,FALSE);
		    push(*f,newN);
		    return newN;
		    }
	       else if (fun == object__K || fun == tagged_object_K /* ? */ ) {
		    node newN = NULL;
		    while (t != NULL) {
			 node name = CAAR(t);
			 node u = CADAR(t);
			 push(newN, list(2, unpos(name), ExpandType(u,f)));
			 t = CDR(t);
			 }
		    apply(reverse,newN);
		    newN = newtype(cons(fun,newN),NULL,FALSE);
		    push(*f,newN);
		    return newN;
		    }
	       else if (fun == array_K || fun == tarray_K) {
		    node newN;
		    newN = cons(fun,cons(ExpandType(car(t),f),cdr(t)));
		    newN = newtype(newN,NULL,FALSE);
		    *f = cons(newN,*f);
		    return newN;
		    }
	       else if (fun == function_S) {
		    node argtypes = car(t);
		    node rettype = cadr(t);
		    node newargtypes = NULL;
		    node newN;
		    while (argtypes != NULL) {
			 newargtypes = cons( 
			      ExpandType(car(argtypes),f), newargtypes);
			 argtypes = cdr(argtypes);
			 }
		    newargtypes = reverse(newargtypes);
		    rettype = ExpandType(rettype,f);
		    newN = list(3,fun,newargtypes,rettype);
		    newN = newtype(newN,NULL,FALSE);
		    *f = cons(newN,*f);
		    return newN;
		    }
	       else {
		    assert(FALSE);
		    return NULL;
		    }
	       }
	  default: assert(FALSE); return NULL;
	  }
     }

static int typeseqno(node t){
     assert(istype(t));
     return totype(t)->body.type.seqno;
     }

void totypesRec(node e) {
     int i, j;
     /* 
	e is a list of TYPEs to be defined recursively

        The recursion is handled through the type fields of the symbols
	involved, which are assumed to be already set, or through TYPEs.
	TYPEs have no POSITIONs in them.
	Any type which turns out to be equivalent to a prior one has the
	address of the prior one inserted into its value field.
     	We assume that the value fields have been run through ExpandType,
	so that each value field is an expression constructed from other
	TYPEs.

	This routine probably has bugs, with the result that the order of
	declarations makes a difference.

        */
     numnewtypes = length(e);
     newtypeslist = newarray(node,numnewtypes);
     ttable = newarray(struct DISTIN *, numnewtypes);
     /* we could perform some hashing first */
     for (i=0; i<numnewtypes; i++) {
	  node t = nth(e,i+1);
	  assert(istype(t));
	  newtypeslist[i] = t;
	  assert(t->tag == type_tag);
	  assert(!(t->body.type.flags & deferred_F));
	  t->body.type.seqno = i + numtypes;
	  ttable[i] = newarray(struct DISTIN,numtypes+numnewtypes);
	  for (j=0; j<numtypes+numnewtypes; j++) {
	       ttable[i][j].listp = NULL;
	       ttable[i][j].distinguishable = FALSE;
	       }
	  }
     for (i=numtypes; i<numnewtypes+numtypes; i++) {
	  for (j=0; j<i; j++) {
	       struct DISTIN *dd = table(i,j);
	       node t = thetype(i), u = thetype(j), tval, uval, th, uh;
	       assertpos(istype(t),t);
	       assertpos(istype(u),u);
	       if ((t->body.type.flags & basic_type_F) || (u->body.type.flags & basic_type_F)) {
		    assert(t != u);
		    differ: mark(i,j);
		    continue;
		    }
	       tval = t -> body.type.definition;
	       uval = u -> body.type.definition;
	       assertpos(tval != NULL,t);
	       assertpos(uval != NULL,u);
	       assertpos(iscons(tval),t);
	       assertpos(iscons(uval),u);
	       assert(! dd->distinguishable );
	       th = car(tval);
	       uh = car(uval);
	       if (th == tagged_object_K || th == tarray_K) goto differ;
	       if (equal(tval,uval)) continue;
	       tval = cdr(tval);
	       uval = cdr(uval);
	       if (th != uh) goto differ;
	       if (th == or_K) {
		    for (;tval != NULL && uval != NULL;
			 tval = cdr(tval), uval = cdr(uval)) {
			 node ti = typeforward(car(tval));
			 node tj = typeforward(car(uval));
			 int ii = typeseqno(ti);
			 int jj = typeseqno(tj);
			 if (ti == tj) continue;
			 if (ii == -1 || jj == -1) goto differ;	/* not defined yet... */
			 if (distinguishable(ii,jj)) goto differ;
			 appendlt(ii,jj,i,j);			 
			 }
		    if (tval != NULL || uval != NULL) goto differ;
		    }
	       else if (th == object__K) {
		    for (;tval != NULL && uval != NULL;
			 tval = cdr(tval), uval = cdr(uval)) {
			 node tmem = car(tval);
			 node umem = car(uval);
			 int ii, jj;
			 node tt, uu;
			 if (car(tmem) != car(umem)) goto differ;
			 ii = typeseqno(tt=typeforward(cadr(tmem)));
			 jj = typeseqno(uu=typeforward(cadr(umem)));
			 if (tt==uu) continue;
			 if (ii == -1 || jj == -1) goto differ;	/* not defined yet... */
			 if (distinguishable(ii,jj)) goto differ;
			 appendlt(ii,jj,i,j);
			 }
		    if (tval != NULL || uval != NULL) goto differ;
		    }
	       else if (th == array_K) {
		    node tt,uu;
		    int ii = typeseqno(tt=typeforward(car(tval)));
		    int jj = typeseqno(uu=typeforward(car(uval)));
		    if (tt!=uu) {
		    	 if (ii == -1 || jj == -1) goto differ;	/* not defined yet... */
		    	 else if (distinguishable(ii,jj)) goto differ;
		    	 else if (!equal(cdr(tval),cdr(uval))) goto differ;
		    	 else appendlt(ii,jj,i,j);
			 }
		    else {
		         if (!equal(cdr(tval),cdr(uval))) goto differ;
		         }
		    }
	       else if (th == function_S) {
		    node ttt,uuu;
		    node targs = car(tval);
		    node uargs = car(uval);
		    int iii = typeseqno(ttt=typeforward(cadr(tval)));
		    int jjj = typeseqno(uuu=typeforward(cadr(uval)));
		    if (ttt!=uuu) {
		    	 if (iii == -1 || jjj == -1) goto differ;	/* not defined yet... */
		    	 if (distinguishable(iii,jjj)) goto differ;
			 }
		    for (;targs != NULL && uargs != NULL;
			 targs = cdr(targs), uargs = cdr(uargs)) {
			 node tt,uu;
			 int ii = typeseqno(tt=typeforward(car(targs)));
			 int jj = typeseqno(uu=typeforward(car(uargs)));
			 if (tt==uu) continue;
		    	 if (ii == -1 || jj == -1) goto differ;	/* not defined yet... */
			 if (distinguishable(ii,jj)) goto differ;
			 }
		    if (targs != NULL || uargs != NULL) goto differ;
		    }
	       else {
		    assert(FALSE); return;
		    }
	       }
	  }
     for (i=0; i<numnewtypes; i++) {
	  for (j=0;j<i+numtypes;j++) {
	       if (!distinguishable(i+numtypes,j)) {
		    /* indistinguishable types are identified here */
		    node t = newtypeslist[i];
		    node n = t->body.type.name;
		    if (n != NULL) {
			 assert(n->tag == symbol_tag);
			 n->body.symbol.value = thetype(j);
			 }
		    t->body.type.forward = thetype(j);
		    t->body.type.flags = identified_F;
		    newtypeslist[i] = NULL;
		    break;
		    }
	       }
	  }
     for (i=0; i<numnewtypes; i++) {
	  if (newtypeslist[i] != NULL) {
	       node t = newtypeslist[i];
	       interntype(t);
	       }
	  }
     }

bool isfunctiontype(node e){
     e = typedefinition(e);
     return iscons(e) && equal(car(e),function_S);
     }

bool isarraytype(node e){
     e = typedefinition(e);
     return iscons(e) && equal(car(e),array_K);
     }

bool istaggedarraytype(node e){
     e = typedefinition(e);
     return iscons(e) && equal(car(e),tarray_K);
     }

bool isobjecttype(node e){
     e = typedefinition(e);
     return iscons(e) && (equal(car(e),object__K));
     }

bool istaggedobjecttype(node e){
     e = typedefinition(e);
     return iscons(e) && equal(car(e),tagged_object_K);
     }

bool israwpointertype(node e) {
  assert(istype(e));
  return !!(e->body.type.flags & (raw_pointer_type_F | raw_atomic_pointer_type_F));
}

bool israwtype(node e) {
  assert(istype(e));
  return !!(e->body.type.flags & (raw_type_F | raw_atomic_type_F));
}

bool ispointertype(node e) {
  assert(istype(e));
  return israwpointertype(e) || isobjecttype(e) || istaggedobjecttype(e) || isarraytype(e) || istaggedarraytype(e);
}

bool istaggedtype(node e) {
  return istaggedobjecttype(e) || istaggedarraytype(e);
}

bool ispointertypeexpr(node e) {
  return israwpointertypeexpr(e) || isobjecttypeexpr(e) || istaggedobjecttypeexpr(e) || isarraytypeexpr(e) || istaggedarraytypeexpr(e);
}

bool israwpointertypeexpr(node e) {
     while (ispos(e)) e = e->body.position.contents;
     while (issym(e)) {
	  if (e->body.symbol.type != type__T) return FALSE;
	  e = e->body.symbol.value;
	  }
     if (!istype(e)) return FALSE;
     return ispointertype(e);
}

bool israwtypeexpr(node e) {
     while (ispos(e)) e = e->body.position.contents;
     while (issym(e)) {
	  if (e->body.symbol.type != type__T) return FALSE;
	  e = e->body.symbol.value;
	  }
     if (!istype(e)) return FALSE;
     return israwtype(e);
}

bool isobjecttypeexpr(node e){
     while (ispos(e)) e = e->body.position.contents;
     while (issym(e)) {
	  if (e->body.symbol.type != type__T) return FALSE;
	  e = e->body.symbol.value;
	  }
     if (istype(e)) return isobjecttype(e);
     if (!iscons(e)) return FALSE;
     return equal(car(e),object__K);
     }

bool istaggedobjecttypeexpr(node e){
     while (ispos(e)) e = e->body.position.contents;
     while (issym(e)) {
	  if (e->body.symbol.type != type__T) return FALSE;
	  e = e->body.symbol.value;
	  }
     if (istype(e)) return istaggedobjecttype(e);
     if (!iscons(e)) return FALSE;
     return equal(car(e),tagged_object_K);
     }

bool isarraytypeexpr(node e){
     while (ispos(e)) e = e->body.position.contents;
     while (issym(e)) {
	  if (e->body.symbol.type != type__T) return FALSE;
	  e = e->body.symbol.value;
	  }
     if (istype(e)) return isarraytype(e);
     if (!iscons(e)) return FALSE;
     return equal(car(e),array_K);
     }

bool istaggedarraytypeexpr(node e){
     while (ispos(e)) e = e->body.position.contents;
     while (issym(e)) {
	  if (e->body.symbol.type != type__T) return FALSE;
	  e = e->body.symbol.value;
	  }
     if (istype(e)) return istaggedarraytype(e);
     if (!iscons(e)) return FALSE;
     return equal(car(e),tarray_K);
     }

bool isortype(node e){
     node f = istype(e) ? typedefinition(e) : e;
     return iscons(f) && car(f) == or_K;
     }

bool isdeferredtype(node e) {
  return istype(e) && (e->body.type.flags & deferred_F) != 0;
}

bool iscompositeortype(node e){
     node f;
     assert(istype(e));
     e = typeforward(e);
     f = e->body.type.definition;
     return iscons(f) && car(f) == or_K && (e->body.type.flags & composite_F);
     }

node functionargtypes(node t){
     assert(istype(t));
     t = typeforward(t);
     assert(isfunctiontype(t));
     return cadr(t->body.type.definition);
     }

node functionrettype(node t){
     assert(istype(t));
     t = typeforward(t);
     assert(isfunctiontype(t));
     return typeforward(caddr(t->body.type.definition));
     }

node totype(node e){
     /* e is a type expression, return a unique TYPE */
     node f=NULL;
     if (e == NULL) return NULL;
     if (e->tag == type_tag) return typeforward(e);
     if (e->tag == position_tag) e = e->body.position.contents;
     if (issym(e)) {
          node t = type(e);
	  if (t == NULL) return NULL;
	  assert(t == type__T);
	  return typeforward(e->body.symbol.value);
	  }
     ExpandType(e,&f);
     totypesRec(f);
     return typeforward(car(f));
				/* a bit expensive */
				/* later, write a simple search through */
				/* existing types (the definition here is  */
				/* not recursive) */
     }

node chktype2(node e,scope v){
     node f, ftype;
     f = chk(e,v);
     if (f == bad__K) return bad_or_undefined_T;
     if (equal(f,type__K)) return type__T;
     ftype = type(f);
     if (ftype != type__T) {
	  node sym;
	  if (ftype != deferred__T) {
	    errorpos(e,"not valid type");
	    return NULL;
	  }
     	  sym = unpos(f);
	  assert(issym(sym));
	  if (sym->body.symbol.value == NULL) {
	       node t = newtype(f,NULL,FALSE);
	       t->body.type.flags = deferred_F;
	       assert(issym(sym));
	       sym->body.symbol.value = t;
	       t->body.type.name = sym;
	       }
	  return sym->body.symbol.value;
	  }
     return f;			/* was totype(f) */
     }

node chktype(node ee,scope v){
     node e = chktype2(ee,v);
     if (e==type__T) {
	  errorpos(ee,"not valid type");
	  return bad_or_undefined_T;
	  }
     return e;
     }

bool subtype(node s, node t){
     if (ispos(s)) s = s->body.position.contents;
     if (ispos(t)) t = t->body.position.contents;
     assert(istype(s) && istype(t));
     s = typeforward(s);
     t = typeforward(t);
     if (s == t) return TRUE;
     if (s == bad_or_undefined_T || t == bad_or_undefined_T) return TRUE;
     if (isortype(s) && isortype(t)) {
	  int i, j, slen, tlen;
	  s = typedeftail(s); slen = length(s);
	  t = typedeftail(t); tlen = length(t);
	  for (i=1; i<=slen; i++) {
	       for (j=1; j<=tlen; j++) {
		    if (subtype(nth(s,i),nth(t,j))) goto okay;
		    }
	       return FALSE;
	       okay:;
	       }
	  return TRUE;
	  }
     if (isortype(s)) return FALSE;
     if (isortype(t)) {
	  int j, tlen;
	  t = typedeftail(t); tlen = length(t);
	  for (j=1; j<=tlen; j++) {
	       if (subtype(s,nth(t,j))) return TRUE;
	       }
	  return FALSE;
	  }
     return FALSE; /* for other types, we assume that totypesRec has worked and made equivalent types identical */
     }

bool checkargtypes(node argtypes, node funargtypes){
     if (length(argtypes) != length(funargtypes)) return FALSE;
     while (argtypes != NULL) {
	  if (!subtype(car(argtypes),car(funargtypes))) return FALSE;
	  argtypes = cdr(argtypes), funargtypes = cdr(funargtypes);
	  }
     return TRUE;
     }

node returntype(node fun){
     node t = type(fun);
     if (iscons(t) && car(t) == function_S && length(t) == 3) {
	  return caddr(t);
	  }
     else return bad_or_undefined_T;
     }

bool couldbenull(node t){
     assert(istype(t));
     return t==null_T || (isortype(t) && member(null_T,typedeftail(t)));
     }

node nextfun(node fun){
     node sym, name, symbol_list, nextsym;
     if (fun->tag == position_tag) {
     	  sym = fun->body.position.contents;
	  }
     else {
	  sym = fun;
	  }
     if (sym->tag != symbol_tag) return NULL;
     name = sym->body.symbol.name;
     assert(isstr(name));
     symbol_list = name->body.unique_string.symbol_list;
     while (car(symbol_list) != sym) {
	  symbol_list = cdr(symbol_list);
	  }
     symbol_list = cdr(symbol_list);
     if (symbol_list == NULL) return NULL;
     nextsym = car(symbol_list);
     return nextsym;
     }

bool isbasictype(node e){
     return istype(e) && (e->body.type.flags & basic_type_F);
     }

bool isarithmetictype(node e){
     return istype(e) && (e->body.type.flags & arithmetic_type_F);
     }

node basictype(node n){
     node t;
     assert(n->tag == symbol_tag);
     n->body.symbol.type = type__T;
     t = newtype(NULL,n,TRUE);
     interntype(t);
     n->body.symbol.value = t;
     assert(n->body.symbol.name->tag == unique_string_tag);
     return t;
     }

node ormemberindex(node u, node t){
     int i;
     assert(iscompositeortype(t));
     i = memberindex(u,typedeftail(t));
     assert(i > 0);
     return integer(i);
     }

bool is_atomic_memory(node t){
     assert(istype(t));
     if (isobjecttype(t) || istaggedobjecttype(t)) return FALSE;
     if (isortype(t)) return FALSE;
     if (isarraytype(t)||istaggedarraytype(t)) return FALSE;
     if (t->body.type.flags & raw_pointer_type_F) return FALSE;
     if (t->body.type.flags & raw_atomic_pointer_type_F) return FALSE;
     if (t->body.type.flags & raw_type_F) return FALSE;
     if (t->body.type.flags & raw_atomic_type_F) return TRUE;
     if (isbasictype(t)) return TRUE;
     assert(FALSE);
     return FALSE;
     }

bool pointer_to_atomic_memory(node t){
     /* return true if the memory allocated for an object of type t contains no pointers */
     assert(istype(t));
     if (isobjecttype(t) || istaggedobjecttype(t)) {
	  node m;
	  for (m=typedeftail(t); m != NULL; m = CDR(m)) {
	       node k = CADAR(m);
	       assert(istype(k));
	       if (k == void_T) continue;
	       if (k->body.type.flags & raw_atomic_type_F) continue;
	       if (k->body.type.flags & (raw_pointer_type_F|raw_atomic_pointer_type_F)) return FALSE;
	       if (isbasictype(CADAR(m))) continue;
	       return FALSE;
	       }
	  return TRUE;
	  }
     if (isortype(t)) {
	  return FALSE;
	  }
     if (isarraytype(t)||istaggedarraytype(t)) {
	  node m = typedeftail(t);
	  assert(length(m) >= 1);
	  node typ = CAR(m);
	  assert(istype(typ));
	  if (typ->body.type.flags & (raw_pointer_type_F|raw_atomic_pointer_type_F)) {
	       return FALSE; /* can we redefine isbasictype? */
	       }
	  return isbasictype(typ);
	  }
     if (t->body.type.flags & raw_pointer_type_F) return FALSE;
     if (t->body.type.flags & raw_atomic_pointer_type_F) return TRUE;
     assert(!((t->body.type.flags & raw_type_F)));
     assert(!((t->body.type.flags & raw_atomic_type_F)));
     if (isbasictype(t)) return TRUE;
     assert(FALSE);
     return FALSE;
     }

/*
 Local Variables:
 compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
 End:
*/
