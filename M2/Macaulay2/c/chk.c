/*		Copyright 1993,2010 by Daniel R. Grayson		*/

#include "scc.h"

static node chklhscolon(node, scope);

static bool isjump(node e){
     return (iscons(e) && (equal(car(e),return_S) || equal(car(e),goto__S))) || equal(e,break_S);
     }

static bool iskeyword(node e){
     return issym(e) && (e->body.symbol.flags & keyword_F);
     }

static bool has_no_effect(node ee){
     node e = ee;
     if (e==NULL) return TRUE;
     if (ispos(e)) e = e->body.position.contents;
     if (issym(e)) {
	  /* these checks should really be somewhere else */
	  if (istype(e)) errorpos(ee,"type expression misplaced");
	  else if (e == bad__K) ;
	  else if (iskeyword(e)) errorpos(ee,"keyword misplaced");
	  else if (!(e->body.symbol.flags & intern_F)) {
	       errorpos(ee,"type expression misplaced");
	       return TRUE;
	       }
	  return TRUE;
	  }
     return FALSE;
     }

static node cons_maybe(node n, node l){
     if (has_no_effect(n))
	  return l;
     else return cons(n,l);
     }

static node pushdecl(node l, node d) {
     if (occursin(cadr(d),l)) return cons(d,l);
     else return l;
     }

static node pushdecllist(node l, node dl) {
     while (dl != NULL) l = pushdecl(l,car(dl)), dl = cdr(dl);
     return l;
     }

static node map_cons_maybe(node newitems, node previous){
     while (newitems != NULL) {
	  previous = cons_maybe(car(newitems),previous);
	  newitems = cdr(newitems);
	  }
     return previous;
     }

static bool isassignment(node f) {
     return iscons(f) && CAR(f) == assign__S;
     }

static void perform(node w, scope v) {
     if (w == NULL) return;
     if (v->deferred_definitions_active > 0 && isassignment(w)) {
	  errorpos(w,"assignment while previous deferred definition active");
	  errorpos(v->previous_deferred_definition,"... here is the previous deferred definition");
	  }
     v->before = cons_maybe(w,v->before);
     }

static void performlist(node w, scope v){
     v->before = map_cons_maybe(w,v->before);
     }

static void performafters(scope v){
     performlist(v->after,v);
     v->after = NULL;
     }

static void performfinals(scope v){
     performlist(v->finals,v);
     v->finals = NULL;
     }

static scope enternewscope(scope *vp){
     scope w = new(struct SCOPE);
     w->previous = *vp;
     *vp = w;
     return w;
     }

static void popscope(scope *vp){
     scope w = *vp;
     scope v = *vp = w->previous;
     assert(w->before == NULL);
     assert(w->after == NULL);
     assert(w->finals == NULL);
     assert(w->thread_inits == NULL);
     v->decls = join(w->decls,v->decls);
     v->tmpdecls = join(w->tmpdecls,v->tmpdecls);
     v->signature = join(w->signature,v->signature);
     unwind(&w->symbols);
     GC_FREE(w);
     }

static void errorpopscope(scope *vp){
     scope w = *vp;
     scope v = w->previous;
     *vp = v;
     unwind(&w->symbols);
     }

static void pushbackscope(scope *vp){
     scope v = *vp;
     v->previous->before = join(v->before,v->previous->before);
     v->before = NULL;
     v->previous->finals = join(v->previous->finals,v->finals);
     v->finals = NULL;
     v->previous->symbols = join(v->symbols,v->previous->symbols);
     v->symbols = NULL;
     popscope(vp);
     }

static bool typematch(node e, node f){
     return
       e == f || 
       e == bad_or_undefined_T ||
       f == bad_or_undefined_T || 
       e == returns_T || 
       f == returns_T ||
       e == exits_T || 
       f == exits_T ;
     }

node lookupfunction(node fun, node argtypes){
     fun = unpos(fun);
     if (equal(argtypes,functionargtypes(type(fun)))) return fun;
     if (fun->tag == symbol_tag) {
     	  node f = fun->body.symbol.name;
     	  assert(isstr(f));
	  node slist;
     	  for (slist = f->body.unique_string.symbol_list; slist!=NULL; slist=cdr(slist)) {
	       node sym = car(slist);
	       if (sym == fun) continue;
	       node t = type(sym);
	       if (!isfunctiontype(t)) continue;
	       if (equal(argtypes,functionargtypes(t))) return sym;
	       }
	  }
     return NULL;
     }

static node lookupexactfunction(node fun, node argtypes){
     fun = unpos(fun);
     assert(fun->tag == unique_string_tag);
     node slist;
     for (slist = fun->body.unique_string.symbol_list; slist!=NULL; slist=cdr(slist)) {
	  node sym = car(slist);
	  node t = type(sym);
	  if (!isfunctiontype(t)) continue;
	  if (equal(argtypes,functionargtypes(t))) return sym;
	  }
     return NULL;
     }

static void chklistn(node e, scope v) {
     for (; e != NULL; e = cdr(e)) {
	  node w = chk(car(e),v);
	  perform(w,v);
	  performafters(v);
	  }
     }

node chklist(node e, scope v) {
     chklistn(e,v);
     node r = reverse(v->before);
     v->before = NULL;
     return r;
     }

static node chkblock(node e, scope v){
     enternewscope(&v);
     node p = e;
     for (; p != NULL; p = cdr(p)) {
	  node w = chk(car(p),v);
	  perform(w,v);
	  performafters(v);
	  }
     performfinals(v);
     unwind(&v->symbols);
     pushbackscope(&v);
     return NULL;
     }

static node chkblockn(node e,scope v){
     node body = cdr(e);
     enternewscope(&v);
     node s = chklist(allbutone(body),v);
     v->previous->decls = join(v->decls,v->previous->decls);
     v->decls = NULL;
     performlist(s,v->previous);
     v->previous->after = join(v->finals,v->previous->after);
     v->finals = NULL;
     s = chk(last(body),v);
     v->previous->before = join(v->before,v->previous->before);
     v->before = NULL;
     v->previous->after = join(v->previous->after,v->after);
     v->after = NULL;
     v->previous->finals = join(v->previous->finals,v->finals);
     v->finals = NULL;
     popscope(&v);
     return s;
     }

static node enblock(node e){
     if (iscons(e) && ( equal(car(e),block__K) || equal(car(e),blockn__K))) return e;
     return list(2,blockn__K,e);
     }

static node newlabel(){
     static int seqno = 0;
     char buf[20];
     sprintf(buf,"L%d_",seqno++);
     return String(strperm(buf));
     }

static node take(node x, node a){
     return list(3,take__S,x,a);	/* x->a */
     }

static node array_take(node x, node a){
     node tx = type(x);
     assert(isarraytype(tx) || istaggedarraytype(tx));
     assert(type(a) == int_T);
     return list(3,array_take_S,x,a);
     }

static node arraylength(node arr){
     node t = type(arr), m;
     assert(isarraytype(t)||istaggedarraytype(t));
     m = typedeftail(t);
     return length(m)==1 ? take(arr,len_S) : cadr(m);
     }

static node cast(node t, node e, scope v) {
     node u;
     if (e == bad__K) return bad__K;
     u = type(e);
     if (u == bad_or_undefined_T) {
	  if (debug) warning("undefined expression in cast");
	  return bad__K;
	  }
     if (t == u) return e;
     return list(3,cast__S,t,e);
     }

static void assign(node lhs, node rhs, scope v){
     node ltype = type(lhs);
     node rtype = type(rhs);
     node crhs;
     if (ltype == void_T || rtype == void_T) return;
     if (ltype != rtype && !(rtype->body.type.flags & arithmetic_type_F)) {
     	  crhs = cast(ltype,rhs,v);
	  }
     else crhs = rhs;
     perform( list(3, assign__S, lhs, crhs), v);
     }

#include <gdbm.h>

static char *datumtostring(datum p) {
  char *buf;
  assert(p.dptr != NULL);
  buf = getmem(p.dsize+1);
  strncpy(buf,p.dptr,p.dsize);
  buf[p.dsize]=0;
  return buf;
}

static GDBM_FILE db;
static int numkeys;

static void opendb() {
  datum key;
  int maxn = 0;
  db = gdbm_open("typecode.db",0,GDBM_WRCREAT|GDBM_SYNC,0644,NULL);
  if (db == NULL) fatal("failed to open typecode.db");
  key = gdbm_firstkey(db);
  while (key.dptr != NULL) {
    datum value = gdbm_fetch(db,key);
    int n = atoi(datumtostring(value));
    if (maxn < n) maxn = n;
    free(value.dptr);
    numkeys++;
    key = gdbm_nextkey(db,key);
  }
  assert( numkeys == maxn );
}

void printtypecodes() {
  datum key;
  if (db == NULL) opendb();
  key = gdbm_firstkey(db);
  while (key.dptr != NULL) {
    datum value = gdbm_fetch(db,key);
    printf("#define %s_typecode %s\n",datumtostring(key),datumtostring(value));
    key = gdbm_nextkey(db,key);
  }
}

static int gettypecode(node t) {
     assert(istype(t));
     t = typeforward(t);
     if (t == null_T) return 0;
     if (t->body.type.runtime_type_code != -1) return t->body.type.runtime_type_code;
     if(!t->body.type.name){
         //unnamed type, warn about and give it an autogenerated name
         //we don't have a position at this point, so there's no way to give the user a line number.
         warning("Unnamed type requiring a typecode encountered, such a type is unlikely to be useful");
         char temp_name[30];
         snprintf(temp_name,sizeof(temp_name),"_%d_unnamed",numkeys+1);
         t->body.type.name = newsymbol(UniqueString(temp_name),type__T,global_scope,defined_F);
     }
     assert(t->body.type.name->body.symbol.name != NULL);
     assert(tostring(t) != NULL);
     if (db == NULL) opendb();
     datum key;
     key.dptr = tostring(t);
     key.dsize = strlen(key.dptr);
     datum value = gdbm_fetch(db,key);
     int n;
     if (value.dptr != NULL) {
	  n = atoi(datumtostring(value));
	  free(value.dptr);
	  }
     else {
	  numkeys++;
	  n = numkeys;
	  char buf[20];
	  sprintf(buf,"%d",n);
	  value.dptr = buf;
	  value.dsize = strlen(buf);
	  if (0 != gdbm_store(db,key,value,GDBM_INSERT)) fatal("failed to store item in typecode.db");
	  gdbm_sync(db);
	  }
     t->body.type.runtime_type_code = n;
     return n;
     }

static void setup(node s, scope v){
     node t = type(s);
     bool tagged = istaggedobjecttype(t) || istaggedarraytype(t);
     if (tagged) assign(take(s, type__S), integer(gettypecode(t)), v);
     }

static node chkcompare(node e, scope v) {
     node f,g,t,u;
     bool i,j;
     node test = unpos(car(e));
     assert( test == equalequal__S || test == unequal_S );
     if (length(e) != 3) return badnumargs(e,2);
     f = chk(cadr(e),v);
     g = chk(caddr(e),v);
     t = type(f);
     u = type(g);
     if (t != u && !subtype(t,u) && !subtype(u,t)) return typemismatch(e);
     /* actually, we want to see whether t and u have any types in common! */
     i = iscompositeortype(t);
     j = iscompositeortype(u);
     if (i || j) {
     	  if (i ^ j) f = list(3,cast__S,null_T,f), g = list(3,cast__S,null_T,g);
     	  return list(3,test,f,g);
	  }
     if (t != u && (isortype(t) || isortype(u))) {
	  f = list(3,cast__S,null_T,f);
	  g = list(3,cast__S,null_T,g);
     	  return list(3,test,f,g);
	  }
     else {
     	  return list(3,test,f,g);
	  }
     }

static node chkint(node e, scope v){
     node f = chk(e,v);
     if (type(f) != int_T) {
	  errorpos(e,"should be an int");
	  return bad__K;
	  }
     return f;
     }

static node chkstring(node e){
     node s = unpos(e);
     if (!isstr(s)) {
	  errorpos(e,"should be a symbol");
	  return bad__K;
	  }
     return s;
     }

static node entmp(node e, scope v){
     node tmp = newtmp(type(e),v,TRUE);
     perform(list(3,assign__S,tmp,e),v);
     return tmp;
     }

static bool isposint(node e){
     e = unpos(e);
     return e->tag == int_const_tag && e->body.int_const.contents[0] != '-';
     }

static bool isnegint(node e){
     e = unpos(e);
     return e->tag == int_const_tag && e->body.int_const.contents[0] == '-';
     }

static node chkfor(node e, scope v) {
#if 0
      (for (n) ... ) 	    	        n times 
      (for (i n) ... )      	   	i = 1,2,...n 
      (for (i m n) ... )	   	i = m,m+1,...,n
      (for (i m n s) ... ) 	        i = m,m+s,...,until i>n        [s>=0]
      	   	     	       	        i = m,m+s,...,until i<n        [s<0]
#endif
     node argblock, indx, init, final, body, step;
     node looplabel = newlabel();
     node breaklabel = newlabel(), skiplabel = newlabel();
     if (length(e) == 1) {
	  errorpos(e,"for without args");
	  return bad__K;
	  }
     argblock = cadr(e);
     body = cddr(e);
     if (!iscons(argblock)) {
	  errorpos(argblock,"argument block should be a list");
	  indx = NULL;
	  init = NULL;
	  final = one__K;
	  step = NULL;
     	  }
     else if (length(argblock) == 1) {
	  indx = NULL;
	  init = NULL;
	  final = car(argblock);
	  step = NULL;
	  }
     else if (length(argblock) == 2) {
	  indx = car(argblock);
	  init = NULL;
	  final = cadr(argblock);
	  step = NULL;
	  }
     else if (length(argblock) == 3) {
	  indx = car(argblock);
	  init = cadr(argblock);;
	  final = caddr(argblock);
	  step = NULL;
	  }
     else if (length(argblock) == 4) {
	  indx = car(argblock);
	  init = cadr(argblock);
	  final = caddr(argblock);
	  step = cadddr(argblock);
	  }
     else {
	  errorpos(argblock,"argument block should have at most 1, 2, 3, or 4 elements");
	  indx = NULL;
	  init = NULL;
	  final = one__K;
	  step = NULL;
	  }
     if (indx == NULL) {
     	  indx = newtmp(int_T,v,TRUE);
	  }
     else {
	  indx = chkstring(indx);
	  if (indx == bad__K) return bad__K;
     	  indx = newsymbol(indx,int_T,v,intern_F|defined_F);
     	  v->tmpdecls = cons(list(2,declare__S,indx), v->tmpdecls);
	  }
     if (init == NULL) {
	  init = one__K;
	  }
     else {
	  init = chkint(init,v);
	  if (init == bad__K) return bad__K;
	  }
     final = chkint(final,v);
     if (final == bad__K) return bad__K;
     final = entmp(final,v);
     if (step == NULL) {
	  step = one__K;
	  }
     else {
	  step = chkint(step,v);
	  if (step == bad__K) return bad__K;
	  step = entmp(step,v);
	  }
     assign(indx,init,v);
     perform(list(2,goto__S,skiplabel),v);
     perform(list(2,label__S,looplabel),v);
     perform(list(3,assign__S,indx,list(4,infix__S,plus_S,indx,step)),v);
     perform(list(2,label__S,skiplabel),v);
     if (isposint(step)) {
     	  perform(list(3,if_S,
	       	    list(4,infix__S,gt_S,indx,final),
	       	    list(2,goto__S,breaklabel)),v);
	  }
     else if (isnegint(step)) {
     	  perform(list(3,if_S,
	       	    list(4,infix__S,lessthan__S,indx,final),
	       	    list(2,goto__S,breaklabel)),v);
	  }
     else {
     	  perform(list(3,if_S,
		    list(4,infix__S,oror_S,
			 list(4,infix__S,andand_S,
			      list(4,infix__S,gt_S,indx,final),
			      list(4,infix__S,ge_S,step,zero__K)),
			 list(4,infix__S,andand_S,
			      list(4,infix__S,lessthan__S,indx,final),
			      list(4,infix__S,lessthan__S,step,zero__K))),
	       	    list(2,goto__S,breaklabel)),v);
	  }
     enternewscope(&v);
     v->loop = TRUE;
     v->break_loop_label = breaklabel;
     v->continue_loop_label = looplabel;
     chklistn(body,v);
     performfinals(v);
     unwind(&v->symbols);
     pushbackscope(&v);
     perform(list(2,goto__S,looplabel),v);
     perform(list(2,label__S,breaklabel),v);
     return NULL;
     }

static node chkforeach(node e, scope v) {
     node argblock, var, arr, arr2, tarr, tmem, code, arrtmp, indx, loop, veryend;
     node step, endlabel=newlabel();
     bool increasing=FALSE, decreasing=FALSE, breaklabelused;
     if (length(e) == 1) {
	  errorpos(e,"need at least one arguments");
	  return bad__K;
	  }
     argblock = cadr(e);
     code = cddr(e);
     if (!iscons(argblock)) {
	  errorpos(argblock,"argument block should be a list");
	  return bad__K;
	  }
     switch(length(argblock)) {
	  case 2: {
     	       indx = NULL;
	       var = car(argblock);
	       arr = cadr(argblock);
	       step = one__K;
	       break;
	       }
	  case 3: {
     	       indx = car(argblock);
	       var = cadr(argblock);
	       arr = caddr(argblock);
	       step = one__K;
	       break;
	       }
	  case 4: {
     	       indx = car(argblock);
	       var = cadr(argblock);
	       arr = caddr(argblock);
	       step = cadddr(argblock);
	       break;
	       }
	  default: {
     	       errorpos(argblock,"argument block should have 2, 3, or 4 elements");
	       return bad__K;
	       }
	  }
     if (!isstrpos(var)) {
	  errorpos(var,"should be a symbol");
	  return bad__K;
	  }
     if (isint(unpos(step))) {
	  increasing = isposint(step);
	  decreasing = isnegint(step);
	  }
     else {
	  step = chkint(step,v);
	  if (step == bad__K) return bad__K;
	  step = entmp(step,v);
	  }
     arr2 = arr;
     arr = chk(arr,v);
     if (arr == bad__K) return bad__K;
     tarr = type(arr);
     if (!(isarraytype(tarr)||istaggedarraytype(tarr))) {
	  errorpos(arr2, "should be an array or tarray");
	  return bad__K;
	  }
     enternewscope(&v);
     arrtmp = newtmp(tarr,v,TRUE);
     assign(arrtmp,arr,v);
     veryend = newlabel();
     perform(list(3,if_S,
	       list(4,infix__S,equalequal__S,arraylength(arrtmp),zero__K),
	       list(2,goto__S,veryend)),v);
     enternewscope(&v);
     tmem = arrayElementType(tarr);
     var = newsymbol(var,tmem,v,intern_F|defined_F);
     if (indx == NULL) {
     	  indx = newtmp(int_T,v,TRUE);
	  }
     else {
	  indx = chkstring(indx);
	  if (indx == bad__K) return bad__K;
     	  indx = newsymbol(indx,int_T,v,intern_F|readonly_F|defined_F);
     	  v->tmpdecls = cons(list(2,declare__S,indx), v->tmpdecls);
	  }
     setcprintvalue(var,array_take(arrtmp,indx));
     if (increasing) {
     	  perform(list(3,assign__S,indx,zero__K),v);
	  }
     else if (decreasing) {
	  perform(list(3,assign__S,indx,
		    list(4,infix__S,minus_S,arraylength(arrtmp),one__K)),v);
	  }
     else {
	  perform(list(4,if_S,
		    list(4,infix__S,gt_S,step,zero__K),
		    list(3,assign__S,indx,zero__K),
		    list(3,assign__S,indx,
		    	 list(4,infix__S,minus_S,arraylength(arrtmp),one__K))),v);
	  }
     loop = newlabel();
     perform(list(2,label__S,loop),v);
     v->loop = TRUE;
     v->break_loop_label = endlabel;
     chklistn(code,v);
     breaklabelused = v->break_loop_label_used;
     performfinals(v);
     unwind(&v->symbols);
     pushbackscope(&v);
     perform(list(3,assign__S,
	       indx,
	       list(4,infix__S,plus_S,indx,step)),v);
     if (increasing) {
     	  perform(list(3,if_S, 
	       	    list(4,infix__S,lessthan__S,indx,arraylength(arrtmp)), 
	       	    list(2,goto__S,loop)),v);
	  }
     else if (decreasing) {
	  perform(list(3,if_S,
		    list(4,infix__S,ge_S,indx,zero__K),
		    list(2,goto__S,loop)),v);
	  }
     else {
	  perform(list(3,if_S,
		    list(4,infix__S,oror_S,
		    	 list(4,infix__S,andand_S,
		    	      list(4,infix__S,gt_S,step,zero__K),
			      list(4,infix__S,lessthan__S,indx,arraylength(arrtmp))),
			 list(4,infix__S,andand_S,
			      list(4,infix__S,lessthan__S,step,zero__K),
			      list(4,infix__S,ge_S,indx,zero__K))),
		    list(2,goto__S,loop)),v);
     	  }
     if (breaklabelused) perform(list(2,label__S,endlabel),v);
     perform(list(2,label__S,veryend),v);
     performfinals(v);
     unwind(&v->symbols);
     pushbackscope(&v);
     return NULL;
     }

static node chkwhile(node e, scope v) {
     bool breaklabelused, afterlabelused = FALSE;
     node b,bafter,l = newlabel(), afterlabel=newlabel(), endlabel=newlabel();
     bool until = equal(car(e),until_S);
     assert( equal(car(e),until_S) || equal(car(e),while_S) );
     enternewscope(&v);
     perform(list(2,label__S,l),v);
     b = chk(enblock(cadr(e)),v);
     if (b == bad__K) return bad__K;
     if (type(b) != bool_T) {
	  errorpos(cadr(e),"condition should be of type bool");
	  return bad__K;
	  }
     bafter = v->after, v->after = NULL;
     if (equal(b,true_K)) {
	  if (until) {
	       perform(list(2,goto__S,afterlabel),v);
	       afterlabelused = TRUE;
	       }
	  }
     else if (equal(b,false_K)) {
	  if (!until) {
	       perform(list(2,goto__S,afterlabel),v);
	       afterlabelused = TRUE;
	       }
	  }
     else {
     	  perform(list(3,if_S, until ? b : list(3,prefix__S,not_S,b), list(2,goto__S,afterlabel)),v);
	  afterlabelused = TRUE;
	  }
     performlist(bafter,v);
     v->loop = TRUE;
     v->break_loop_label = endlabel;
     v->continue_loop_label = l;
     chk(cons(block__K,cddr(e)),v);
     breaklabelused = v->break_loop_label_used;
     pushbackscope(&v);
     perform(list(2,goto__S,l),v);
     if (afterlabelused) perform(list(2,label__S,afterlabel),v);
     performlist(bafter,v);
     if (breaklabelused) perform(list(2,label__S,endlabel),v);
     if (b == bad__K) return bad__K;
     return NULL;
     }

static node chkwhen(node e, scope v){
     int i, nulls = 0, nullindex = 0, ntypes;
     bool hadelse = FALSE, doswitch;
     node sym1, sym, typ, types, labels = NULL, firstcasecodetype = NULL,
	  cases, vtmp=NULL, casetypes = NULL, after = NULL, endlabel = NULL, nullcaselabel = NULL;
     sym1 = cadr(e);
     cases = cddr(e);
     enternewscope(&v);
     sym = chk(sym1,v);
     after = v->after, v->after = NULL;
     pushbackscope(&v);
     if (sym == bad__K) return bad__K;
     typ = type(sym);
     if (!isortype(typ)) {
	  errorpos(cadr(e),typ == deferred__T ? "not declared yet" : "when-clause requires a union type");
	  return bad__K;
	  }
     if (!issym(unpos(sym)))
	  sym = entmp(sym,v);
     if (debug) perform(list(5,Ccode_S,void_T,String("GC_CHECK_CLOBBER("),sym,String(")")),v);
     types = typedeftail(typ);
     ntypes = length(types);
     for (i=1; i<=ntypes; i++) if (nth(types,i)==null_T) ++nulls, nullindex=i;
     doswitch = nulls + 1 != ntypes;
     if (!doswitch) {
	  for (i=1; i<=ntypes; i++) labels = cons(newlabel(),labels);
	  labels = reverse(labels); /* just so the numbers appear to increase */
	  if (nulls > 0) 
	       perform( list(3,if_S, list(2,isnull__S,sym), list(2,goto__S,nth(labels,nullindex))),v);
	  }
     else {
	  if (nulls > 0) {
	       nullcaselabel = newlabel();
	       perform( list(3,if_S, list(2,isnull__S,sym), list(2,goto__S,nullcaselabel)),v);
	       }
	  }
     if (nulls == 0 && casechks) {
	  perform(list(5,Ccode_S, void_T, String("if ("), sym, String(" == 0) invalidNullPointer(__FILE__,__LINE__,-1)")),v);
	  }
     if (!doswitch) endlabel = newlabel();
     /* we don't examine the type tag when there is only one non-null type in the union, because it might not be there */
     if (doswitch) {
	  perform(list(6, Ccode_S, void_T, String("switch ("), take(sym,type__S), String(") "), String("{")),v);
	  }
     else {
     	  for (i=1; i<=ntypes; i++) {
	       if (nth(types,i) != null_T) {
		    perform(list(2,goto__S,nth(labels,i)),v);
		    break;
		    }
	       }
	  }
     for(; cases != NULL; cases = cdr(cases)) {
	  node cas, casecode, casetype=NULL, casesym=NULL, casecodetype;
	  if (length(CAR(cases))==1) {
	       /* this is the else clause at the end */
	       assert(CDR(cases) == NULL);
	       cas = NULL;
	       casecode = CAAR(cases);
	       casetype = NULL;
	       casesym = NULL;
	       hadelse = TRUE;
	       if (casechks || !doswitch) {
		    for(i=1; i<=length(types); i++) { /* this is a slow way to traverse a linked list! */
			 node nt = nth(types,i);
			 if (!member(nt,casetypes)) {
			      if (doswitch) {
				   if (nt != null_T)
					perform(list(5, Ccode_S, void_T, String("case "), integer(gettypecode(nth(types,i))), String(":")), v);
				   }
			      else
				   perform(list(2,label__S, nth(labels, i)),v);
			      }
			 }
		    }
	       else {
		    perform(list(3, Ccode_S, void_T, String("default:")), v);
	            }
	       if (nullcaselabel) {
		    perform(list(2,label__S, nullcaselabel),v);
		    nullcaselabel = NULL;
		    }
	       }
	  else {
	       cas = CAAR(cases);
	       casecode = CADAR(cases);
	       if (iscons(cas) && equal(car(cas),colon__S)) {
		    if (length(cas) != 3) return badnumargs(cas,2);
		    casesym = CADR(cas);
		    casetype = chktype(CADDR(cas),v);
		    if (casetype == bad__K) return bad__K;
		    casetype = totype(casetype);
		    }
	       else if (cas != NULL) {
		    casetype = chktype(cas,v);
		    if (casetype == bad__K) return bad__K;
		    casetype = totype(casetype);
		    casesym = NULL;
		    }
	       if (!member(casetype,types)) {
		    errorpos(iscons(cas)&&length(cas)==3 ? caddr(cas) : cas,
			     "type not among those represented by the when-clause");
		    }
	       else {
		    if (casetype == null_T && nullcaselabel) {
			 perform(list(2,label__S, nullcaselabel),v);
			 nullcaselabel = NULL;
			 }
		    else {
			 if (doswitch)
			      perform(list(5, Ccode_S, void_T, String("case "), integer(gettypecode(casetype)), String(":")), v);
			 else
			      perform(list(2,label__S, nth(labels, memberindex(casetype,types))),v);
			 }
		    }
	       casetypes = cons(casetype,casetypes);
	       }
	  enternewscope(&v);
	  if (casesym != NULL) {
	       casesym = newsymbol(casesym,casetype,v,intern_F|defined_F);
	       push(v->decls,list(2,declare__S,casesym));
	       assign(casesym,sym,v);
	       }
	  performlist(after,v);
	  casecode = chk(list(2,blockn__K,casecode),v);
	  casecodetype = type(casecode);
	  if (firstcasecodetype == NULL) {
	       if (casecodetype != returns_T && casecodetype != exits_T) {
		    firstcasecodetype = casecodetype;
		    if (firstcasecodetype != void_T) {
			 vtmp = newtmp(firstcasecodetype,v,TRUE);
			 }
		    }
	       }
	  else {
	       if (!typematch(firstcasecodetype,casecodetype)) {
		    errorpos(CAR(cases),"type mismatch between branches");
		    }
	       }
	  if (casecode != NULL) {
	       if (firstcasecodetype != void_T && casecodetype != returns_T && casecodetype != exits_T)
		    assign(vtmp,casecode,v);
	       else perform(casecode,v);
	       }
	  unwind(&v->symbols);
	  performafters(v);
	  performfinals(v);
	  pushbackscope(&v);
	  if (doswitch)
	       perform(break_S,v);
	  else
	       perform(list(2,goto__S,endlabel),v);
	  }
     if (!hadelse) {
	  /* check for a missing case */
	  int missing = 0;
     	  for(i=1; i<=length(types); i++)
	       if (!member(nth(types,i),casetypes)) missing++;
	  if (missing > 0) {
	       char buf[400];
	       strcpy(buf,missing > 1 ? "missing cases" : "missing case");
	       for(i=1; i<=length(types); i++) {
		    if (!member(nth(types,i),casetypes)) {
		    	 sprintf(buf + strlen(buf)," %d",i);
		    	 }
		    }
	       errorpos(e,buf);
	       return bad__K;
	       }
	  }
     assert(! nullcaselabel);
     if (doswitch && casechks)
	  perform(list(5, Ccode_S, void_T, String("default: invalidTypeTag("),take(sym,type__S),String(",__FILE__,__LINE__,-1)")), v);
     if (doswitch)
	  perform(list(3,Ccode_S,void_T,String("}")),v);
     else
	  perform(list(2,label__S,endlabel),v);
     return vtmp;
     }

static bool reachable(scope v) {
     return v->before == NULL || !isjump(car(v->before));
     }

static node chkandand(node e, scope v) {
     node b, bafter, l = newlabel(), c, vtmp = newtmp(bool_T,v,TRUE);
     enternewscope(&v);
     b = chk(cadr(e),v);
     if (b != bad__K && type(b) != bool_T) {
	  errorpos(cadr(e),"condition should be of type bool");
	  }
     bafter = v->after, v->after = NULL;
     assign(vtmp,b,v);
     perform(list(3,if_S,list(3,prefix__S,not_S,vtmp),list(2,goto__S,l)),v);
     c = chk(enblock(caddr(e)),v);
     if (c!=bad__K && type(c) != bool_T) {
	  errorpos(caddr(e),"condition should be of type bool");
	  return bad__K;
	  }
     assign(vtmp,c,v);
     performafters(v);
     perform(list(2,label__S,l), v);
     performlist(bafter,v);
     if (b==bad__K || c==bad__K) return bad__K;
     pushbackscope(&v);
     return vtmp;
     }

static node chkoror(node e, scope v) {
     node b, bafter, l = newlabel(), c, vtmp = newtmp(bool_T,v,TRUE);
     enternewscope(&v);
     b = chk(cadr(e),v);
     if (b != bad__K && type(b) != bool_T) {
	  errorpos(cadr(e),"condition should be of type bool");
	  }
     bafter = v->after, v->after = NULL;
     assign(vtmp,b,v);
     perform(list(3,if_S,vtmp,list(2,goto__S,l)),v);
     c = chk(enblock(caddr(e)),v);
     if (c!=bad__K && type(c) != bool_T) {
	  errorpos(caddr(e),"condition should be of type bool");
	  }
     assign(vtmp,c,v);
     performafters(v);
     perform(list(2,label__S,l), v);
     performlist(bafter,v);
     if (b==bad__K || c==bad__K) return bad__K;
     pushbackscope(&v);
     return vtmp;
     }

static node chkif(node e, scope v) {
     if (length(e) == 4 || length(e) == 3) {
	  node b,bafter,thenclause,elseclause = NULL,l = newlabel(), m=NULL;
	  node thentype = NULL, elsetype = NULL, vtmp = NULL;
     	  enternewscope(&v);
	  b = chk(enblock(cadr(e)),v);
	  if (b != bad__K && type(b) != bool_T) {
	       errorpos(cadr(e),"condition should be of type bool");
	       }
	  bafter = v->after, v->after = NULL;
	  perform(list(3,if_S,b,list(2,goto__S,l)),v);
	  performlist(bafter,v);
	  if (length(e)==4) elseclause = chk(enblock(cadddr(e)),v);
	  elsetype = type(elseclause);
	  if (elsetype != void_T && elsetype != returns_T && elsetype != exits_T) {
	       vtmp = newtmp(elsetype,v,TRUE);
	       assign(vtmp,elseclause,v);
	       }
	  else if (elseclause != NULL) perform(elseclause,v);
	  performafters(v);
	  if (reachable(v)) {
	       m = newlabel();
	       perform(list(2,goto__S,m), v);
	       }
	  perform(list(2,label__S,l), v);
	  performlist(bafter,v);
	  thenclause = chk(enblock(caddr(e)),v);
	  thentype = type(thenclause);
	  if (thenclause == bad__K || elseclause == bad__K || b == bad__K) return bad__K;
	  if (elsetype == deferred__T) {
	       errorpos(cadddr(e),"undefined");
	       return bad__K;
	       }
	  if (type(thenclause) == deferred__T) {
	       errorpos(caddr(e),"undefined");
	       return bad__K;
	       }
	  if (length(e) == 4 && !typematch(thentype,elsetype)) {
	       errorpos(e,"then/else clauses not of same type");
	       return bad__K;
	       }
	  if (thentype != void_T && thentype != returns_T && thentype != exits_T) {
	       if (vtmp == NULL) vtmp = newtmp(thentype,v,TRUE);
	       assign(vtmp,thenclause,v);
	       }
	  else perform(thenclause,v);
	  performafters(v);
	  if (m != NULL) perform(list(2,label__S,m), v);
	  pushbackscope(&v);
	  return vtmp;
	  }
     errorpos(e,"if-statement takes 2 or 3 arguments");
     return NULL;
     }

static node chkCcode(node e, scope v){
     bool bad = FALSE;
     node r = NULL, t, ee;
     if (length(e) < 2) {
	  errorpos(e,"Ccode takes at least one argument");
	  return bad__K;
	  }
     t = totype(chktype(cadr(e),v));
     for (ee = CDDR(e);ee != NULL;ee = CDR(ee)) {
	  node b = car(ee);
	  node u = unpos(b);
	  if (u->tag != string_const_tag) {
	       u = chk(b,v);
	       }
	  if (u == bad__K) bad=TRUE;
	  r = cons(u,r);
	  }
     if (bad) return bad__K;
     else {
	  node z;
	  r = reverse(r);
	  z = cons(Ccode_S,cons(t,r));
	  /* if (t != void_T && t != returns_T && t != exits_T) z = entmp(z,v); */
	  z = enpos(z,pos(e));
	  return z;
          }
     }

static void returngather(scope v){
     scope w = v;
     while (TRUE) {
	  assert( v != NULL );
	  performlist(v->after,w);
	  performlist(v->finals,w);
	  if (v->defun) return;
	  v = v->previous;
	  }
     }

static void breakgather(scope v){
     scope w = v;
     while (TRUE) {
	  assert( v != NULL );
	  performlist(v->after,w);
	  performlist(v->finals,w);
	  if (v->loop) return;
	  v = v->previous;
	  }
     }

static node defunrettype(scope v){
     while (v!=NULL && !v->defun) v = v->previous;
     if (v == NULL) return NULL;
     return v->rettype;
     }

bool inside_defun(scope v){
     scope w = v;
     for (w=v; w != NULL ; w = w->previous) {
	  if (w->defun) return TRUE;
	  }
     return FALSE;
     }

static bool returns_disabled(scope v){
     scope w = v;
     for (w=v; w!=NULL && !w->defun; w = w->previous) {
	  if (w->disable_breaks) return TRUE;
	  }
     return FALSE;
     }

static node chkreturn(node e,scope v){
     node rettype;
     if (!inside_defun(v)) {
	  errorpos(e,"return should be used in the code body of a function");
	  return bad__K;
	  }
     rettype = defunrettype(v);
     if (length(e) > 2) {
	  errorpos(e,"return takes at most one argument");
	  return bad__K;
	  }
     if (returns_disabled(v)) {
	  errorpos(e,"return not allowed in this context");
	  return bad__K;
	  }
     if (length(e) == 1) {
	  if (rettype != void_T) {
	       errorpos(e,"return value missing");
	       return bad__K;
	       }
     	  returngather(v);
	  perform(list(1,return_S),v);
	  return _returnedThing_K;
	  }
     else {
	  node r = chk(cadr(e),v);
	  node rtype = type(r);
	  if (!subtype(rtype,rettype)) return typemismatch(cadr(e));
          {				/* compare with chkfunctiontype */
	       node tmp;
	       tmp = newtmp(rettype,v,TRUE);
	       perform(list(3,assign__S,tmp,cast(rettype,r,v)),v);
     	       
	       tmp = enpos(tmp,pos(r));
	       r = tmp;
	       }
     	  returngather(v);
	  perform(list(2,return_S,r),v);
	  return _returnedThing_K;
	  }
     }

static bool inside_loop(scope v){
     scope w = v;
     for (w=v; w != NULL; w = w->previous) {
	  if (w->loop) return TRUE;
	  if (w->defun) return FALSE;
	  }
     return FALSE;
     }

static bool breaks_disabled(scope v){
     scope w = v;
     for (w=v; w!=NULL && !w->loop; w = w->previous) {
	  if (w->disable_breaks) return TRUE;
	  }
     return FALSE;
     }

static node getbreaklabel(scope v){
     scope w = v;
     for (w=v; !w->loop; w = w->previous) ;
     w->break_loop_label_used = TRUE;
     return w->break_loop_label;
     }

static scope package_scope(scope v){
     while (TRUE) {
	  if (v == NULL) return NULL;
	  if (v->current_package != NULL) return v;
	  v = v->previous;
	  }
     }

static node enclosing_package(scope v){
     scope w = package_scope(v);
     if (w == NULL) return NULL;
     return w->current_package;
     }

static bool withinsignature(scope v){
     node p = enclosing_package(v);
     return p != NULL && (p->body.symbol.flags & signature_F);
     }

static void pushsignature(node e, scope v){
     if (!withinsignature(v)) 
       push(v->signature,e);
     }

static node chkpackage1(node e, scope v, bool sig){
     node s, sym, body, initsymb, finalsymb, threadinitsymb;
     if (length(e) != 3) return badnumargs(e,2);
     s = CADR(e);
     body = CADDR(e);
     if (!isstrpos(s)) {
	  errorpos(s,"identifier required");
	  return bad__K;
	  }
     sym = newsymbol(s,package_T,v,package_active_F|intern_F|defined_F|(sig?signature_F:0));
     enternewscope(&v);
     assert(issym(sym));
     v->current_package = sym;
     chk(body,v);
     global_scope->decls = join(v->decls,global_scope->decls);
     v->decls = NULL;
     initsymb = newsymbol(
	  UniqueString(prefixify(sym,"_prepare")),
	  totype(list(3,function_S,NULL,void_T)),
	  v->previous,
	  intern_F|defined_F|literal_F|visible_F|constant_F|export_F|constructor_F
	  );
     threadinitsymb = newsymbol(
	  UniqueString(prefixify(sym,"_thread_prepare")),
	  totype(list(3,function_S,NULL,void_T)),
	  v->previous,
	  intern_F|defined_F|literal_F|constant_F
	  );
     finalsymb = newsymbol(
	  UniqueString(prefixify(sym,"_final")),
	  totype(list(3,function_S,NULL,void_T)),
	  v->previous,
	  intern_F|defined_F|literal_F|constant_F
	  );
     if (!sig) {
	  node code = NULL;
	  code = join(
	       list(3,
		    list(3,Ccode_S,void_T, String("static struct FUNCTION_CELL this_final, this_thread_prepare")),
		    list(3,Ccode_S,void_T, String("static int called_yet = 0")),
		    list(3,Ccode_S,void_T, String("if (called_yet) return; else called_yet = 1"))
		    ),
	       join(
		    reverse(v->before),
		    list(6,
			 list(4,Ccode_S,void_T, String("this_thread_prepare.func = "), threadinitsymb),
			 list(3,Ccode_S,void_T, String("this_thread_prepare.next = thread_prepare_list")),
			 list(3,Ccode_S,void_T, String("thread_prepare_list = &this_thread_prepare")),
			 list(4,Ccode_S,void_T, String("this_final.func = "), finalsymb),
			 list(3,Ccode_S,void_T, String("this_final.next = final_list")),
			 list(3,Ccode_S,void_T, String("final_list = &this_final"))
			 )));
	  v->before = NULL;
	  code = map_cons_maybe(v->tmpdecls,code);
	  v->tmpdecls = NULL;
	  push(global_scope->decls, list(2,declare__S,threadinitsymb));
	  push(global_scope->decls, list(2,declare__S,finalsymb));
	  push(global_scope->decls, join( list(3,define__S,list(1,initsymb),void_T), code));
	  push(global_scope->decls, join( list(3,define__S,list(1,threadinitsymb),void_T), reverse(global_scope->thread_inits)));
	  global_scope->thread_inits = NULL;
	  push(global_scope->decls, join( list(3,define__S,list(1,finalsymb),void_T), join(v->after,v->finals)));
	  v->after = v->finals = NULL;
     	  v->signature = list(1, cons(signature_S,cons(s,reverse(v->signature))));
	  }
     pushbackscope(&v);
     sym->body.symbol.flags &=~ package_active_F;
     return NULL;
     }

static node chkpackage(node e, scope v){
     return chkpackage1(e,v,FALSE);
     }

static node chksignature(node e, scope v){
     return chkpackage1(e,v,TRUE);
     }

static node chkbreak(node e, scope v){
     node break_label;
     if (!inside_loop(v)) {
	  errorpos(e,"break should be used inside a loop");
	  return bad__K;
	  }
     break_label = getbreaklabel(v);
     if (length(e) > 1) return badnumargs(e,1);
     if (breaks_disabled(v)) {
	  errorpos(e,"break not allowed in this context");
	  return bad__K;
	  }
     breakgather(v);
     perform(list(2,goto__S,break_label),v);
     return NULL;
     }

static node chkmalloc(node e, scope v) {
  node t = chktype(cadr(e),v), T, tmp;
  if (t == bad__K) return bad__K;
  T = totype(t);
  assert(istype(T));
  tmp = newtmp(T,v,TRUE);
  perform(list(2,getmem__S,tmp),v);
  return tmp;
}

static node chknewarray(node e, scope v){
     node arrtype = chktype(nth(e,2),v);
     if (arrtype == bad_or_undefined_T) return bad__K;
     if (!(isarraytypeexpr(arrtype)||istaggedarraytypeexpr(arrtype))) {
	  errorpos(nth(e,2),"should be an array or tarray");
	  return bad__K;
	  }
     arrtype = totype(arrtype);
     node elemtype = arrayElementType(arrtype);
     node len = nth(e,3);
     bool fixed_length = len == NULL;
     if (fixed_length) {
	  len = arrayElementLength(arrtype);
	  if (NULL == len) {
	       errorpos(e,"explicit length needed with array type of variable length");
	       return bad__K;
	       }
	  }
     else {
	  if (NULL != arrayElementLength(arrtype)) {
	       errorpos(nth(e,3),"explicit length not usable with array type of fixed length");
	       return bad__K;
	       }
	  len = chkint(len,v);
	  if (len == bad__K) return bad__K;
	  }
     node new_array_index_tmpvar = NULL;
     if (nth(e,4) != NULL) {
	  new_array_index_tmpvar = chkstring(nth(e,4));
	  if (new_array_index_tmpvar == bad__K) return bad__K;
	  }
     node body = list(1,nth(e,5));
     enternewscope(&v);
     v->new_array_element_type = elemtype;
     if (new_array_index_tmpvar == NULL) {
     	  v->new_array_index_tmpvar = newtmp(int_T,v,TRUE);
	  }
     else {
     	  v->new_array_index_tmpvar = newsymbol(new_array_index_tmpvar,int_T,v,intern_F|readonly_F|defined_F);
     	  push(v->decls, list(2,declare__S,v->new_array_index_tmpvar));
	  }
     assign(v->new_array_index_tmpvar,zero__K,v);
     node arr = v->new_array_tmpvar = newtmp(arrtype,v,TRUE);
     node continuelabel = newlabel();
     v->new_array_break_label = newlabel();     
     node olen = len;
     len = entmp(len,v);
     len = enpos(len,pos(olen));
     v->new_array_len = len;
     if (!fixed_length) perform(list(2,array_len_check_S,len),v);
     perform(list(3,getmem__S,v->new_array_tmpvar,len),v);
     setup(v->new_array_tmpvar,v);
     if (!fixed_length) assign(take(v->new_array_tmpvar, len_S), len, v);
     perform(list(3,if_S,
	       list(4,infix__S,equalequal__S,len,zero__K), 
	       list(2,goto__S,v->new_array_break_label)),v);
     perform(list(2,label__S,continuelabel),v);
     v->disable_breaks = TRUE;
     chk(cons(block__K,body),v);
     push(v->before,list(2,goto__S,continuelabel));
     perform(list(2,label__S,v->new_array_break_label),v);
     if (!v->new_array_value_provided) {
	  errorpos(e,"new array: no values provided by body");
	  }
     unwind(&v->symbols);
     pushbackscope(&v);
     return arr;
     }

static node chkprovide(node e, scope v){
     node arg, argtyp, around;
     scope w = v, vv;
     if (length(e) != 2) return badnumargs(e,2);
     while (w->new_array_tmpvar == NULL) {
	  w = w->previous;
	  if (w == NULL) {
	       errorpos(e,"provide not allowed in this context");
	       return bad__K;
	       }
	  }
     w->new_array_value_provided = TRUE;
     enternewscope(&v);
     arg = chk(cadr(e),v);
     argtyp = type(arg);
     if (!subtype(argtyp,w->new_array_element_type)) {
	  errorpos(cadr(e),"type mismatch");
	  return bad__K;
	  }
     assign(array_take(w->new_array_tmpvar,w->new_array_index_tmpvar),arg,v);
     performafters(v);
     pushbackscope(&v);
     around = newlabel();
     perform(list(3,if_S,
	       list(4,infix__S,lessthan__S,
		    list(3,prefix__S,plusplus_S,w->new_array_index_tmpvar),
		    w->new_array_len), 
	       list(2,goto__S,around)),v);
     for (vv = v; TRUE; vv = vv->previous) {
     	  performlist(vv->after,v);
     	  performlist(vv->finals,v);
	  if (vv->new_array_tmpvar != NULL) break;
	  }
     perform(list(2,goto__S,w->new_array_break_label),v);
     perform(list(2,label__S,around),v);
     return NULL;
     }

static node errorRedefined(node newsym,node oldsym) {
     errorpos(newsym,"symbol being redefined");
     if (pos(oldsym)->filename)
       errorpos(oldsym,"here is the previous definition");
     return bad__K;
     }

static node chklhs(node e, scope v){
     if (isstrpos(e)) {
	  node w = lookupword(e);
	  if (w == NULL) return newsymbol(e,deferred__T,v,none_F);
	  assert(issym(w));
	  if ((w->body.symbol.flags & (defined_F|import_F))
	      &&
	      type(w) != bad_or_undefined_T) {
	       return errorRedefined(e,w);
	       }
	  else return w;
	  }
     if (!iscons(e)) {
	  errorpos(e,"invalid left hand side of definition");
	  return bad__K;
	  }
     if (equal(CAR(e),threadLocal_S)){
	  node s = chklhs(CADR(e),v);
	  if (s == bad__K) return bad__K;
	  /* it would be nice to check whether the variable is static ... the C compiler will complain, if not */
	  s->body.symbol.flags |= threadLocal_F;
	  return s;
	  }
     if (equal(CAR(e),const_S)){
	  node s = chklhs(CADR(e),v);
	  if (s == bad__K) return bad__K;
	  s->body.symbol.flags |= const_F;
	  return s;
	  }
     if (equal(CAR(e),export_S)) {
     	  node s;
	  if (length(e) != 2) assert(FALSE);
	  s = chklhs(CADR(e),v);
	  if (s == bad__K) return bad__K;
	  assert(issym(s));
	  if (!(s->body.symbol.flags & export_F)) {
	       s->body.symbol.flags |= export_F;
     	       if (s->body.symbol.flags & intern_F) {
		    s->body.symbol.Cname = totoken(tostring(s));
		    s->body.symbol.Cname = prefixify(v->previous->current_package,s->body.symbol.Cname);
		    s->body.symbol.Cname = uniquify(s->body.symbol.Cname);
		    exportit(s,v);
		    }
	       }
	  return s;
	  }
     if (equal(CAR(e),import_S)) {
     	  node s;
	  if (length(e) != 2) assert(FALSE);
	  s = chklhs(CADR(e),v);
	  if (s == bad__K) return bad__K;
	  assert(issym(s));
	  s->body.symbol.flags |= import_F;
	  if ((s->body.symbol.flags & intern_F)) {
	       s->body.symbol.Cname = totoken(tostring(s));
	       s->body.symbol.Cname = prefixify(v->previous->current_package,s->body.symbol.Cname);
	       s->body.symbol.Cname = uniquify(s->body.symbol.Cname);
	       exportit(s,v);
	       }
	  return s;
	  }
     if (!equal(CAR(e),colon__S))
	  e = list(3,colon__S,e,deferred__K);
     return chklhscolon(e,v);
     }

static node chkimport(node e, scope v){
     node sym;
     if (length(e) != 2) return badnumargs(e,2);
     sym = chklhs(CADR(e),v);
     if (sym == bad__K) return bad__K;
     sym = unpos(sym);
     if (!issym(sym)) {
	  errorpos(e,"importing a nonsymbol");
	  return bad__K;
	  }
     if ((sym->body.symbol.flags & defined_F) && type(sym) != type__T) {
	  errorpos(e,"importing a symbol already initialized");
	  return bad__K;
	  }
     if (sym->body.symbol.flags & import_F) {
	  errorpos(e,"importing a previously defined symbol");
	  }
     else {
     	  sym->body.symbol.flags |= import_F;
     	  internsymbol(sym,v);
	  }
     if (type(sym) != type__T && 
	  !(
	       isfunctiontype(type(sym)) 
	       && 
	       (sym->body.symbol.flags & constant_F)
	       )
	  ) {
          /* push(v->decls,list(2,declare__S,sym)); */
	  }
     pushsignature(e,v);
     return sym;
     }

static node chkdefinition(node e, scope v){
     node lhs, rhs, ltype;
     bool lhs_thread_local = FALSE;
     if (length(e) != 3) return badnumargs(e,2);
     lhs = chklhs(cadr(e),v);
     if (lhs == bad__K) return bad__K;
     ltype = type(lhs);
     rhs = caddr(e);
     if (!issym(lhs)) {
	  errorpos(cadr(e),"expected left hand side of := to be a symbol");
	  return bad__K;
	  }
     if ( ltype == bad_or_undefined_T	/* just undefined */
	  && !(lhs->body.symbol.flags & defined_F)
	  && (lhs->body.symbol.flags & intern_F)
	  ) {
     	  node rhsvalue;
	  /* filling in a forward definition of a type variable */
     	  if (lhs->body.symbol.flags & export_F) {
	       pushsignature(cadr(e),v);
	       }
	  rhsvalue = chk(rhs,v);
	  if (rhsvalue != bad_or_undefined_T) {
	       node t = totype(rhsvalue);
	       assert(istype(t));
	       if (!(lhs->body.symbol.flags & defined_F)) {
		    lhs->body.symbol.value->body.type.forward = t;
		    lhs->body.symbol.flags |= defined_F;
		    }
	       else {
		    lhs->body.symbol.value = t;
		    }
	       if (t->body.type.name == NULL) {
		    t->body.type.name = lhs;
		    }
	       }
	  return lhs;
	  }
     if (isfunctiontype(ltype)
	  && (lhs->body.symbol.flags & constant_F)
	  ) {
     	  node args = lhs->body.symbol.args, t, body;
	  node rettype = functionrettype(ltype);
	  node parmsyms = NULL;
     	  if ((lhs->body.symbol.flags & export_F)) {
	       pushsignature(cadr(e),v);
	       }
	  if ((lhs->body.symbol.flags & defined_F)
	       && (lhs->body.symbol.flags & intern_F)) {
	       return errorRedefined(cadr(e),lhs);
	       }
	  lhs->body.symbol.flags |= defined_F;
	  if (!(lhs->body.symbol.flags & intern_F)) internsymbol(lhs,v);
     	  enternewscope(&v);
	  for (t=args; t!=NULL; t=CDR(t)) {
	       node parm = CAR(t);
	       assert(issym(parm));
	       parm->body.symbol.flags |= defined_F;
     	       internsymbol(parm,v);
	       parmsyms = cons(parm,parmsyms);
	       }
	  parmsyms = reverse(parmsyms);
     	  v->defun = TRUE;
     	  v->rettype = rettype;
     	  if (rettype==void_T) {
	       chk(list(2,block__K,rhs),v);
	       returngather(v);
	       }
	  else {
	       node w = chk(list(2,blockn__K,rhs),v);
	       node W = type(w);
	       if (w == bad__K) return bad__K;
	       if (W == returns_T || W == exits_T) {
		  perform(w,v);
	          }
	       else if (reachable(v)) {
		    if (equal(rettype,deferred__T)) {
		      /* now we have to fix the return type of the function we are declaring,
		         now that we know the type returned by the body */
		      assert(issym(lhs));
		      errorpos(cadr(e),"function definition without return type");
		      return NULL;
		    }
		    else if (!subtype(W,rettype)) {
			 errorpopscope(&v);
			 return typemismatch(last(e));
			 }
		    else if (W != rettype) {
		      node tmp = newtmp(rettype,v,TRUE);
		      perform(list(3,assign__S,tmp,cast(rettype,w,v)),v);
		      w = enpos(tmp,pos(w));
		      }
		    returngather(v);
		    perform(list(2,return_S,w),v);
		    }
	       }
	  body = reverse(v->before);
	  v->before = NULL;
	  v->after = NULL;
	  v->finals = NULL;
	  body = pushdecllist(body,v->tmpdecls), v->tmpdecls = NULL;
	  body = pushdecllist(body,v->decls), v->decls = NULL;
	  popscope(&v);
	  push(global_scope->decls, join( list(3, define__S,cons(lhs,parmsyms),rettype), body));
	  return lhs;
	  }
     else {
	  node rhsvalue, rtype;
     	  if (lhs->body.symbol.flags & defined_F) {
	       return errorRedefined(cadr(e),lhs);
	       }
	  else {
	       lhs->body.symbol.flags |= defined_F;
	       }
	  if (lhs->body.symbol.flags & intern_F) {
	       assertpos(ltype == deferred__T,cadr(e));
	       }
	  else {
     	       internsymbol(lhs,v);
	       }
	  if (lhs->body.symbol.flags & threadLocal_F) {
	       lhs_thread_local = TRUE;
	       enternewscope(&v);
	       }
	  rhsvalue = chk(rhs,v);
	  if (rhsvalue == bad__K) {
	    if (debug) errorpos(rhs,"(ignoring right hand side of assignment)");
	    return bad__K;
	  }
	  rtype = type(rhsvalue);
	  if (rtype == deferred__T) {
	       errorpos(rhs,"not declared yet");
	       if (ltype == deferred__T)
		    lhs->body.symbol.type = bad_or_undefined_T;
	       return bad__K;
	       }
	  if (ltype == deferred__T) {
	       lhs->body.symbol.type = ltype = rtype;
	       }
	  else {
	       if (!subtype(rtype,ltype)) {
		    errorpos(rhs, "type mismatch");
		    return bad__K;
		    }
	       }
	  if (ltype == type__T) {
	       node t = totype(rhsvalue);
	       if (t->body.type.name == NULL) t->body.type.name = lhs;
	       if (lhs->body.symbol.value != NULL) {
		      if (!forwardtype(lhs->body.symbol.value,t)) {
			errorpos(lhs, "internal error 22"); /* need a better error message here ... */
			return bad__K;
		      }
		    }
	       else {
		    lhs->body.symbol.value = t;
		    }
     	       if (lhs->body.symbol.flags & export_F) {
	       	    pushsignature(e,v);
		    }
	       }
	  else {
	       if (lhs_thread_local) {
		 if(!ispointertype(ltype) && ltype!=int_T && !isortype(ltype) && ltype!=bool_T) {
		   if(ltype->body.type.flags & integer_type_F) {
		     if(EQUAL!=strcmp("unsigned int",ltype->body.type.Cname) &&
			EQUAL!=strcmp("volatile int",ltype->body.type.Cname) &&
			EQUAL!=strcmp("volatile bool",ltype->body.type.Cname) &&
			EQUAL!=strcmp("char",ltype->body.type.Cname) &&
			EQUAL!=strcmp("signed char",ltype->body.type.Cname) &&
			EQUAL!=strcmp("unsigned char",ltype->body.type.Cname) &&
			EQUAL!=strcmp("short",ltype->body.type.Cname) &&
			EQUAL!=strcmp("unsigned short",ltype->body.type.Cname))
		       {
			 errorpos(lhs, "thread local variable not valid integer type");
			 return bad__K;
		       }
		   }
		   else {
		     errorpos(lhs, "thread local variable not pointer or integer type");
		     return bad__K;
		   }
		 }
		    if(compilerThreadLocal) {
			 assign(lhs,rhsvalue,v);
			 if (ltype != void_T  && !is_atomic_memory(ltype)) perform(list(9,Ccode_S,void_T, 
				 String("GC_add_roots(&("),
				 lhs,
				 String("),(char *)&("),
				 lhs,
				 String(")+sizeof("),
				 lhs,
				 String("))")),v);
		    }
		    if(pthreadThreadLocal) {
			 char* getsymbolbasicname(node);
			 char* name = getsymbolbasicname(lhs);			
			 if (ltype != void_T) perform(list(7,Ccode_S,void_T,
							   String("TS_Add_ThreadLocal(&"),
							   String(name),
							   String("_id,\""),
							   String(name),
							   String("\")")),v);		  
			 //perform(list(6,Ccode_S,void_T,lhs,String("= TS_ThreadLocal("),lhs,String("_id)")),v);			
			 assign(lhs,rhsvalue,v);
		    }
		    node funid = newsymbol(tmp__S, totype(list(3,function_S,NULL,void_T)),
			 global_scope, intern_F|defined_F|literal_F|constant_F);
		    node defn = join( 
			      list(3,define__S,list(1,funid),void_T),
			      join(reverse(v->tmpdecls),join(reverse(v->before),v->after)));
		    v->tmpdecls = v->before = v->after = NULL;
		    pushbackscope(&v);
		    push(v->decls, defn);
		    node callit = list(2,funcall__S,funid);
		    perform(callit,v);
		    push(global_scope->thread_inits, callit);
		    }
	       else
		    assign(lhs,rhsvalue,v);
	       if (ltype != void_T) {
		    node package = NULL;
		    if (v != NULL && v->previous != NULL) {
			 package = v->previous->current_package;
			 }
		    if (package != NULL)
			 push(global_scope->decls, list(2,declare__S,lhs));
		    else push(v->decls, list(2,declare__S,lhs));
		    }
	       if (lhs->body.symbol.flags & export_F) {
		    node d = list(3,colon__S,lhs,type(lhs));
		    if (lhs->body.symbol.flags & threadLocal_F) d = list(2,threadLocal_S,d);
		    if (lhs->body.symbol.flags & const_F) d = list(2,const_S,d);
		    pushsignature(list(2,import_S,d),v);
		    }
	       }
	  return lhs;
	  }
     }

static node chknewinstance(node e, scope v, node typ){
     node args, newargs=NULL;
     node objtype, argtypes = NULL;
     node membertypes, tmpsymb;
     bool badarg = FALSE;
     int  i, j, nargs, nmembers;
     objtype = totype(typ);
     if (objtype == bad_or_undefined_T) return bad__K;
     if (isbasictype(objtype)) {
	  node arg, t;
	  if (objtype == null_T) {
	       if (length(e) != 1) return badnumargs(e,0);
	       return cast(null_T,integer(0),v);
	       }
	  if (objtype == void_T && length(e) == 1) {
	       return NULL;
	       }
	  if (length(e) != 2) return badnumargs(e,1);
	  arg = cadr(e);
     	  arg = chk(arg,v);
	  t = type(arg);
	  if (t == bad_or_undefined_T) return bad__K;
	  if (isarithmetictype(t) && isarithmetictype(objtype)) {
	       return setpos(list(2,objtype,arg),pos2(e));
	       }
	  errorpos(e,"impossible type conversion");
	  return bad__K;
	  }
     if (isarraytype(objtype) || istaggedarraytype(objtype)) {
	  node m = typedeftail(objtype);
	  node tp = car(m);
	  node len = length(m)==2 ? cadr(m) : NULL;
     	  enternewscope(&v);
	  args = cdr(e);
	  nargs = length(args);
	  assert(len == NULL || len->tag == int_const_tag);
	  if (len != NULL && atoi(len->body.int_const.contents) != nargs) {
	       errorpos(e,"wrong number of initial values");
	       return bad__K;
	       }
	  for (i=1; i<=nargs; i++) {
	       node newarg = chk(nth(args,i),v);
	       if (newarg == bad__K) return bad__K;
	       if (!subtype(type(newarg),tp)) {
		    errorpos(nth(args,i),"type mismatch");
		    return bad__K;
		    }
	       newargs = cons(newarg,newargs);
	       argtypes = cons(type(newarg),argtypes);
	       }
	  newargs = reverse(newargs);
     	  tmpsymb = newtmp(objtype,v,TRUE);
	  perform(list(3,getmem__S,tmpsymb,integer(nargs)),v);
	  setup(tmpsymb,v);
     	  if (len == NULL) {
	       assign(take(tmpsymb, len_S), integer(nargs), v);
	       }
	  for (i=1; i<=nargs; i++) {
	       assign(array_take(tmpsymb, integer(i-1)), nth(newargs,i), v);
	       }
	  performlist(v->after,v->previous);
	  v->after = NULL;
	  pushbackscope(&v);
	  return tmpsymb;
	  }
     if (isortype(objtype)) {
	  node arg;
	  if (length(e) != 2) {
	       errorpos(e,"type conversion requires one argument");
	       return bad__K;
	       }
	  arg = chk(cadr(e),v);
	  if (arg == bad__K) return bad__K;
	  if (!subtype(type(arg),objtype)) {
	    errorpos(cadr(e),
		     type(arg) == deferred__T
		     ? "argument of deferred type"
		     : "type of argument not among those in the union");
	    return bad__K;
	    }
	  return cast(objtype,arg,v);
	  }
     if (isobjecttype(objtype) || istaggedobjecttype(objtype)) {
	  enternewscope(&v);
	  membertypes = typedeftail(objtype);
	  args = cdr(e);
	  nargs = length(args);
	  tmpsymb = newtmp(objtype,v,TRUE);
	  for (i=1; i<=nargs; i++) {
	       node newarg = chk(nth(args,i),v);
	       if (equal(newarg,self_K)) newarg = tmpsymb;
	       newargs = cons(newarg,newargs);
	       argtypes = cons(type(newarg),argtypes);
	       }
	  newargs = reverse(newargs);
	  argtypes = reverse(argtypes);
	  if (member(bad__K,newargs)) return bad__K;
	  nmembers = length(membertypes);
	  for (i=j=1; i<=nargs && j<=nmembers; ) {
	       if (equal(cadr(nth(membertypes,j)),void_T)) {
		    j++;
		    continue;
		    }
	       if (!subtype(nth(argtypes,i), 
			 cadr(nth(membertypes,j)))) {
		    badarg = TRUE;
		    if (nth(argtypes,i) == deferred__T) {
			 errorpos(nth(args,i),"not declared yet");
			 }
		    else {
			 errorpos(nth(args,i),"wrong type");
			 }
		    }
	       i++, j++;
	       }
	  if (i <= nargs) {
	       errorpos(e,"too many arguments");
	       return bad__K;
	       }
	  while (j<=nmembers && equal(cadr(nth(membertypes,j)),void_T)) j++;
	  if (j <= nmembers) {
	       errorpos(e,"too few arguments");
	       return bad__K;
	       }
	  if (badarg) return bad__K;
	  perform(list(2,getmem__S,tmpsymb),v);
	  setup(tmpsymb,v);
	  for (i=j=1; i<=nargs; j++) {
	       node r, l;
	       assert(NULL != nth(newargs,i));
	       if (equal(cadr(nth(membertypes,j)),void_T)) continue;
	       l = take(tmpsymb, car(nth(membertypes,j)));
	       r = nth(newargs,i++);
     #if 0
	       ... hmmm ... self references really have to be counted,
	       because when freeing the object it will recursively try to
	       free it again.
	       if (r == tmpsymb) perform( list(3, assign__S, l, r), v);
	       else 
     #endif
	       assign(l,r,v);
	       }
	  if (debug) perform(list(5,Ccode_S,void_T,String("GC_CHECK_CLOBBER("),tmpsymb,String(")")),v);
	  performlist(v->after,v->previous);
	  v->after = NULL;
	  pushbackscope(&v);
	  return tmpsymb;
	  }
     errorpos(car(e),"type without a construction method");
     return bad__K;
     }

static node chkarray(node e, scope v){
     node typ;
     if (length(e) > 3 || length(e) == 1) {
	  errorpos(e,"array takes a type and an optional length");
	  return bad__K;
	  }
     typ = chktype(cadr(e),v);
     if (typ == bad__K) return bad__K;
     if (length(e) == 3) {
	  node len = unpos(chk(caddr(e),v));
	  if (!isint(len)) {
	       errorpos(caddr(e),"array length should be an integer");
	       return bad__K;
	       }
	  if (len->body.int_const.contents[0]=='-') {
	       errorpos(caddr(e),"array length should be nonnegative");
	       return bad__K;
	       }
	  return list(3,array_K,typ,len);
	  }
     else {
	  return list(2,array_K,typ);
	  }
     }

static node chktarray(node e, scope v){
     node len = NULL;
     node typ;
     if (length(e) > 3 || length(e) == 1) {
	  errorpos(e,"tarray takes a type and an optional length");
	  return bad__K;
	  }
     if (length(e) == 3) {
	  len = unpos(caddr(e));
	  if (len->tag != int_const_tag) {
	       errorpos(caddr(e),"tarray length should be an integer");
	       return bad__K;
	       }
	  if (len->body.int_const.contents[0]=='-') {
	       errorpos(caddr(e),"tarray length should be nonnegative");
	       return bad__K;
	       }
	  }
     typ = chktype(cadr(e),v);
     return len==NULL ? list(2,tarray_K,typ) : list(3,tarray_K,typ,len);
     }

static node chkobject(node e, scope v){
     node ee = cons(car(e),NULL);
     e = cdr(e);
     while (e != NULL) {
	  node pair = car(e);
	  node var, vtype;
	  if (!iscons(pair) || 
	       length(pair) != 3 || 
	       !equal(colon__S,car(pair))) {
	       errorpos(pair,"invalid type");
	       return bad__K;
	       }
	  var = cadr(pair);
	  vtype = caddr(pair);
	  if (!isword(var)) {
	       errorpos(var,"part name should be a word");
	       var = bad__K;
	       vtype = bad_or_undefined_T;
	       }
	  else {
	       vtype = chktype(vtype,v);
	       }
	  ee = cons(list(2,var,vtype),ee);
	  e = cdr(e);
	  }
     return reverse(ee);
     }

static node chkfunction(node e, scope v){
     node args = cadr(e);
     node result = caddr(e);
     return list(3, function_S, chktypelist(args,v), chktype(result,v));
     }

static node chkfunctiontype(node e, scope v, node fun){
     node args = cdr(e);
     node argtypes = NULL, funargtypes = NULL;
     int  nargs = length(args);
     node newargs=NULL, newexpr=NULL;
     node funtype = type(fun);
     int  i;
     enternewscope(&v);
     for (i=1; i<=nargs; i++) {
	  node newarg = chk(nth(args,i),v);
	  newargs = cons(newarg,newargs);
	  assert(type(newarg) != NULL);
	  argtypes = cons(type(newarg),argtypes);
	  }
     newargs = reverse(newargs);
     argtypes = reverse(argtypes);
     if (member(bad__K,newargs) || member(bad_or_undefined_T,argtypes)) return bad__K;
     if (member(deferred__T,argtypes)) {
	  int j = memberindex(deferred__T,argtypes);
	  errorpos(nth(args,j),"not declared yet");
	  return bad__K;
	  }
     fun = lookupfunction(fun,argtypes);
     if (fun == NULL) {
	  errorpos(e, "unsuitable arguments");
	  return bad__K;
	  }
     funtype = type(fun);
     funargtypes = functionargtypes(funtype);
     if (!equal(funargtypes,argtypes)) {
	  node a = NULL;
	  for (i=1; i<=nargs; i++) {
	       a = cons( cast( nth(funargtypes,i),nth(newargs,i),v), a);
	       }
	  newargs = reverse(a);
	  }
     if (issym(fun) && (fun->body.symbol.flags & macro_function_F)) {
	  node a = NULL;
	  while (newargs != NULL) {
	       a = cons(list(2,chked__K,CAR(newargs)),a);
	       newargs = CDR(newargs);
	       }
	  newargs = reverse(a);
	  newexpr = substitute(fun->body.symbol.args,newargs,
	       fun->body.symbol.body);
	  newexpr = chk(newexpr,v);
	  newexpr = enpos(newexpr,pos(e));
	  }
     else {
     	  newexpr = cons( funcall__S, cons(fun,newargs));
	  setpos(newexpr,pos(e));
	  }
     v->previous->after = join(v->previous->after,v->after);
     v->after = NULL;
     pushbackscope(&v);
     return newexpr;
     }

static node chkchked(node e, scope v){
     return cadr(e);
     }

static bool islvalue(node s){
     s = unpos(s);
     return (
	  (issym(s) && !(s->body.symbol.flags & tmp_F))
	  || 
	  (iscons(s) && (
		    equal(car(s),take__S) 
		    || 
		    equal(car(s),array_take_S)
		    )
	       )
	  );
     }

static node chkassignment(node e, scope v){
     assert(length(e) == 3);
     node sym = chk(cadr(e),v);
     node val = chk(caddr(e),v);
     if (!islvalue(sym)) {
	  errorpos(cadr(e),"can't be set");
	  return bad__K;
	  }
     node typ = type(val);
     if (typ == deferred__T) {
	  errorpos(val,"not defined");
	  return bad__K;
	  }
     node t = type(sym);
     if (t == deferred__T) {
	  errorpos(cadr(e),"not declared");
	  return bad__K;
	  }
     if (!subtype(typ,t)) {
	  errorpos(e,"type mismatch");
	  return bad__K;
	  }
     if (occursin(sym,val)) {
	  if (iscons(unpos(val))
	       && equal(take__S,car(unpos(val))) 
	       && equal(sym,cadr(unpos(val)))) {
	       val = entmp(val,v);
     	       assign(sym,val,v);
	       }
	  else {
	       val = entmp(val,v);
     	       assign(sym,val,v);
	       }
	  }
     else {
     	  assign(sym,val,v);
	  }
     return NULL;		/* could consider returning val instead */
     }

static node chkor(node e,scope v){
     node types;
     node t, f;
     int numnonnull=0;
     bool hadbad = FALSE;
     e=cdr(e);
     types = chktypelist(e,v);
     for (f=e, t=types; t!=NULL; t=cdr(t),f=cdr(f)) {
	  node tt = car(t);
	  if (unpos(tt) == bad_or_undefined_T) return bad__K;
	  if (equal(tt,null_K)) continue;
	  if (ispointertypeexpr(tt)) { numnonnull ++; continue; }
	  if (isdeferredtype(tt)) { numnonnull ++; tt->body.type.flags |= should_be_pointer_F; continue; }
	  errorpos(car(f),"expected a pointer type");
	  hadbad = TRUE;
	  }
     for (f=e, t=types; t!=NULL; t=cdr(t),f=cdr(f)) {
	  node tt = car(t);
	  if (!(istaggedobjecttypeexpr(tt) || istaggedarraytypeexpr(tt) || equal(tt,null_K) || isdeferredtype(tt))) {
	    if (numnonnull != 1) {
	      node utt = unpos(tt);
	      errorpos(car(f),"untagged type not usable with other non-null types in a union type");
	      if (issym(utt)) errorpos(utt,"... here is the declaration");
	      hadbad = TRUE;
	    }
	  }
	  if (isdeferredtype(tt) && numnonnull > 1) { tt->body.type.flags |= should_be_tagged_F; continue; }
	  }
     if (hadbad) return bad__K;
     return cons(or_K,types);
     }

static node chklength(node e,scope v){
     node s,t;
     if (length(e) != 2) {
	  errorpos(e,"length takes one argument");
	  return bad__K;
	  }
     s = chk(cadr(e),v);
     t = type(s);
     if (!(isarraytype(t)||istaggedarraytype(t))) {
	  errorpos(e,"expected an array or tarray as argument to length");
	  return bad__K;
	  }
     return arraylength(s);
     }

static int truestrlen(char *s){
     int len = 0, i=3;
     while (*s) {
	  if (*s == '\\') {
	       s++;
	       if ('0'<=*s && *s<='7') {
	       	    while ('0'<=*s && *s<='7' && i>0) s++, i--;
		    }
	       else {
		    s++;
		    }
	       len++;
	       }
	  else {
	       s++;
	       len ++;
	       }
	  }
     return len;
     }

static node chkstringconst(node e, scope v) {
     node f = ispos(e) ? e->body.position.contents : e;
     int i;
     node len;
     node tmp = newsymbol(str__S,totype(list(2,array_K,char_K)),NULL,
	  intern_F|defined_F|literal_F);
     tmp->body.symbol.pos = *pos(e);
     push(global_scope->decls,list(2,declare__S,tmp));
     assert(f->tag == string_const_tag);
     i = truestrlen(f->body.string_const.characters);
     len = integer(i);
     perform(list(3, getmem__S,tmp,len),v);
     setup(tmp,v);
     assign(take(tmp, len_S), len, v);
     perform(list(5,funcall__S,memcpy_S, take(tmp, array_S), e, len),v);
     return enpos(tmp,pos(e));
     }

static node comma2(node, node);

static node comma1(node e, node fun){
     if (equal(CAR(e),comma_S)) {
	  return cons(cons(fun,comma2(CDR(e),fun)),NULL);
	  }
     else return cons(CAR(e),comma1(CDR(e),fun));
     }

static node comma2(node e, node fun){
     if (!member(comma_S,e)) return e;
     return comma1(e,fun);
     }

static node commaexpand(node e){
     if (!member(comma_S,e)) return e;
     return comma1(e,CAR(e));
     }

static node chklhscolon(node e, scope v){
     node x,t;
     if (length(e)!=3) return badnumargs(e,2);
     x = cadr(e);
     t = totype(chktype2(caddr(e),v));
     if (t == NULL) return NULL;
     if (isstrpos(x)) {
	  node sym = newsymbol(x,t,v,none_F);
	  node typ = newtype(NULL,sym,FALSE);
	  sym->body.symbol.value = typ;
	  typ->body.type.name = sym;
	  return sym;
	  }
     else {
	  bool bad = FALSE;
	  node funtype, s;
	  node f = car(x), args = cdr(x), newargs = NULL, argtypes = NULL;
	  if (!isstrpos(f)) {
	       errorpos(f,"should be a symbol");
	       return bad__K;
	       }
	  for (;args != NULL; args=cdr(args)) {
	       node arg = chklhs(car(args),v);
	       if (arg == bad__K) {
		    bad = TRUE;
		    continue;
		    }
	       if (arg->body.symbol.type == bad_or_undefined_T) {
		    bad = TRUE;
		    continue;
		    }
	       assert(issym(arg));
	       arg->body.symbol.flags |= nouniquify_F;
	       assert(isstr(arg->body.symbol.name));
	       if (arg->body.symbol.name->body.unique_string.flags & str_keyword_F) {
		 errorpos(car(args),"C++ or C keyword used as parameter");
		 bad = TRUE;
	       }
	       newargs = cons(arg,newargs);
	       argtypes = cons(type(arg),argtypes);
	       }
	  newargs = reverse(newargs);
	  argtypes = reverse(argtypes);
	  funtype = totype(list(3,function_S,argtypes,t));
	  s = lookupexactfunction(f,argtypes);
	  if (s != NULL) {
     	       if (type(s) != funtype) {
		    errorpos(f,"redeclaration of function with different type for return value");
     	       	    errorpos(s,"here is the previous definition");
		    return bad__K;
		    }
	       if (s->body.symbol.flags & (defined_F|import_F)) {
		    errorpos(f,"redeclaration");
     	       	    errorpos(s,"here is the previous definition");
		    }
	       else {
		    /* warningpos(f,"completion of previous deferred definition"); */
		    v->deferred_definitions_active--;
	            }
	       }
	  else {
	       s = newsymbol(f,funtype,v,/* intern_F| ??? */ constant_F);
	       }
	  s->body.symbol.args = newargs;
	  if (bad) return bad__K;
	  return s;
	  }
     }

static node chkcolon(node e, scope v){
     node s = chklhscolon(e,v);
     node sym = unpos(s);
     node t = type(sym);
     if (sym == bad__K) return bad__K;
     assert(issym(sym));
     if (isfunctiontype(t)) {
	  v->deferred_definitions_active++;
	  v->previous_deferred_definition = e;
	  internsymbol(sym,v);	/* deferred definition of a function! */
	  return s;
	  }
     if (israwtype(t)) {
	  /* eventually we'd like to allow raw types to be initialized to zero by a declaration without a definition */
	  }
     errorpos(s,"declaration without definition");
     return bad__K;
     }

static node chkcoloncolonequal(node e, scope v){
     /* macros! */
     node f, fsym;
     bool export = FALSE;
     int macro_flag;
     if (length(e) != 3) return badnumargs(e,2);
     f = cadr(e);
     if (iscons(f) && (equal(car(f),export_S) || equal(car(f),import_S))) {
       export = TRUE;
       f = cadr(f);
     }
     if (isstrpos(f)) {
	  macro_flag = macro_variable_F;
	  }
     else {
	  macro_flag = macro_function_F;
	  if (!( iscons(f) && isstrpos(car(f)) )) {
	       errorpos(e,"invalid macro function definition");
	       return bad__K;
	       }
	  if (equal(car(f),colon__S)) {
	       errorpos(e,"invalid macro function definition (no return type should be declared)");
	       return bad__K;
	       }
	  }
     f = list(3,colon__S,f,void_K);
     fsym = chklhs(f,v);
     if (!issym(fsym)) {
	  errorpos(e,"invalid macro function definition");
	  return bad__K;
	  }
     fsym->body.symbol.flags |= macro_flag | defined_F;
     fsym->body.symbol.body = caddr(e);
     fsym->body.symbol.Cname = NULL;
     if (macro_flag == macro_function_F) {
	  /* for some reason, with macros we treat the argument list as a list
	  of strings instead of a list of uninterned symbols */
	  node m = fsym->body.symbol.args, n = NULL;
	  while (m != NULL) {
	       node s = CAR(m);
	       assert(issym(s));
	       n = cons(s->body.symbol.name,n);
	       m = CDR(m);
	       }
	  fsym->body.symbol.args = reverse(n);
	  }
     internsymbol(fsym,v);
     if (export) {
       push(v->signature, e);
       exportit(fsym,v);
     }
     return NULL;
     }

static node chkdot(node e, scope v){
     node thing, thingtype, x, p;
     if (length(e)!=3) return badnumargs(e,2);
     x = cadr(e);
     p = caddr(e);
     thing = chk(x,v);
     thingtype = type(thing);
     if (isarraytype(thingtype)||istaggedarraytype(thingtype)) {
	  node indx = chk(p,v);
	  node t = type(indx);
	  if (t != int_T) {
	       if (t->body.type.flags & integer_type_F) {
		    indx = cast(int_T,indx,v);
		    }
	       else {
		    errorpos(p,"array access needs an integer argument");
		    return NULL;
		    }
	       }
	  if (arraychks) {
	       node tmp = newtmp(int_T,v,TRUE);
	       tmp = enpos(tmp,pos(indx));
	       assign(tmp,indx,v);
	       perform(list(3,array_check_S,tmp,arraylength(thing)),v);
	       return setpos(array_take(thing,tmp),pos2(e));
	       }
	  else {
	       return array_take(thing,indx);
	       }
	  }
     if (thingtype == bad_or_undefined_T) return bad__K;
     if (isortype(thingtype)) {
	  if (NULL==ormembertype(thingtype,p)) {
	       errorpos(p,"not a member");
	       return bad__K;
	       }
	  return setpos(
	       take(
		    cast(CAR(typedeftail(thingtype)),thing,v),
		    p),
	       pos2(e));
	  }
     if (isobjecttype(thingtype) || istaggedobjecttype(thingtype)) {
	  if (NULL==membertype(thingtype,p)) {
	       errorpos(p,"not a member");
	       return bad__K;
	       }
	  return setpos(take(thing,p),pos2(e));
	  }
     if (thingtype == deferred__T) {
	  errorpos(x,"undefined");
	  }
     else {
     	  errorpos(x,"should be an object, array, or union");
	  }
     return bad__K;
     }

static void setchkfun(node e,chkfun f){
     assert(issym(e));
     e->body.symbol.check = f;
     }

static chkfun getchkfun(node e){
     e = unpos(e);
     if (e!=NULL && e->tag == symbol_tag) return e->body.symbol.check;
     else return NULL;
     }

node chk(node e, scope v){
     node f = e;
     if (e == NULL) return NULL;
     if (ispos(f)) f = f->body.position.contents;
     assert (f != NULL);
     switch(f->tag){
	  case type_tag: return e;
     	  case unique_string_tag: {
	       node sym = lookupword(e);
	       if (sym == NULL)
		    sym = newsymbol(e,deferred__T,v,intern_F);
	       else if (sym->body.symbol.flags & macro_variable_F) {
		    sym = chk(sym->body.symbol.body,v);
		    }
	       return repos(e,sym);
	       }
     	  case string_const_tag: {
	       scope w = package_scope(v);
	       if (w == NULL) {
		    errorpos(e,"outside of a package");
		    return bad__K;
		    }
	       return chkstringconst(e,w);
	       }
	  case symbol_tag: {
	       return e;
	       }
	  case double_const_tag: return e;
	  case char_const_tag: return e;
	  case int_const_tag: return e;
	  case position_tag: assert(FALSE);return NULL;
	  case cons_tag: {
	       node fun = chk(car(e),v);
	       node funtype;
	       node (*cf)(node,scope) = getchkfun(fun);
     	       if (cf != NULL) return cf(e,v);
	       funtype = type(fun);
	       if (funtype == bad_or_undefined_T) return bad__K;
	       if (equal(fun,block__K)) return chkblock(cdr(e),v);
	       if (type__T==funtype)
		    return chknewinstance(commaexpand(e),v,fun);
     	       if (isfunctiontype(funtype))
		    return chkfunctiontype(commaexpand(e),v,fun);
	       errorpos(car(e),"not a function or type");
	       return bad__K;
	       }
	  default: assert(FALSE); return NULL;
	  }
     }

node headerstrings;
static node chkheader(node e, scope v) {
     headerstrings = cons(cadr(e),headerstrings);
     return NULL;
     }

node leftOperator(node e) {
     int prty = atoi(tostring(cadr(e)));
     char *str = tostring(caddr(e));
     if (ERROR == setopleft(prty,str)) {
	  errorpos(e,"invalid operator definition");
	  }
     return e;
     }

node rightOperator(node e) {
     int prty = atoi(tostring(cadr(e)));
     char *str = tostring(caddr(e));
     if (ERROR == setopright(prty,str))
	  errorpos(e,"invalid operator definition");
     return e;
     }

node prefixOperator(node e) {
     int prty = atoi(tostring(cadr(e)));
     char *str = tostring(caddr(e));
     if (ERROR == setopprefix(prty,str))
	  errorpos(e,"invalid operator definition");
     return e;
     }

static node chkoperator(node e, scope v) {
     /*
       operator definitions in a signature file would happen too late, because the parser parses the
       entire source file before "checking" it.

       push(v->signature,e); 
     */
     return NULL;
     }

node declarationsstrings;
static node chkdeclarations(node e, scope v) {
     declarationsstrings = cons(cadr(e),declarationsstrings);
     push(v->signature, list(2,declarations_S,cadr(e)));
     return NULL;
     }

static node chkrawtype(node e, scope v) {
     node name = unpos(cadr(e));
     node t = newtype(NULL,NULL,FALSE);
     assert(isstrconst(name));
     t->body.type.Cname = name->body.string_const.characters;
     t->body.type.flags |= raw_type_F;
     t->body.type.definition = list(2,Type_K,name);
     interntype(t);
     return t;
     }

static node chkatomicrawtype(node e, scope v) {
     node name = unpos(cadr(e));
     node t = newtype(NULL,NULL,TRUE);
     assert(isstrconst(name));
     t->body.type.Cname = name->body.string_const.characters;
     t->body.type.flags |= raw_atomic_type_F;
     t->body.type.definition = list(2,atomicType_K,name);
     interntype(t);
     return t;
     }

static node chkarithmetictype(node e, scope v) {
     node name = unpos(cadr(e));
     node t = newtype(NULL,NULL,TRUE);
     assert(isstrconst(name));
     t->body.type.Cname = name->body.string_const.characters;
     t->body.type.flags |= arithmetic_type_F;
     t->body.type.definition = list(2,arithmeticType_K,name);
     interntype(t);
     return t;
     }

static node chkintegertype(node e, scope v) {
     node name = unpos(cadr(e));
     node t = newtype(NULL,NULL,TRUE);
     assert(isstrconst(name));
     t->body.type.Cname = name->body.string_const.characters;
     t->body.type.flags |= integer_type_F | arithmetic_type_F;
     t->body.type.definition = list(2,integerType_K,name);
     interntype(t);
     return t;
     }

static node chkpointer(node e, scope v) {
     node name = unpos(cadr(e));
     node t = newtype(NULL,NULL,FALSE);
     assert(isstrconst(name));
     t->body.type.Cname = name->body.string_const.characters;
     t->body.type.flags |= raw_pointer_type_F;
     t->body.type.definition = list(2,Pointer_K,name);
     interntype(t);
     return t;
     }

static node chkatomicpointer(node e, scope v) {
     node name = unpos(cadr(e));
     node t = newtype(NULL,NULL,FALSE);
     assert(isstrconst(name));
     t->body.type.Cname = name->body.string_const.characters;
     t->body.type.flags |= raw_atomic_pointer_type_F;
     t->body.type.definition = list(2,atomicPointer_K,name);
     interntype(t);
     return t;
     }

static node chklvalue(node e, scope v) {
     if (length(e) != 2) {
	  errorpos(e,"expected 1 argument");
	  return bad__K;
	  }
     node x = chk(CADR(e),v);
     if (!islvalue(x)) {
	  errorpos(e,"lvalue expected");
	  return bad__K;
	  }
     return x;
     }

static node chk_use(node e, scope v){
     bool fresh;
     node p, syms, name, pp = enclosing_package(v);
     node namep = CADR(e);
     if (length(e) != 2) return badnumargs(e,2);
     assert(ispos(namep));
     name = unpos(namep);
     assert(isstr(name));
     p = lookupword(name);
     fresh = p == NULL;
     if (fresh) {
	  char *pathopened = NULL;
	  if (!sigreadfile(tostring(name),&pathopened)) {
	       errorpos(CADR(e), "undefined package (no *.sig file found)");
	       return bad__K;
	       }
	  assert(pathopened != NULL);
	  if (dependfile != NULL)
	       fprintf(dependfile,"%s %s %s : %s\n", 
		       outfilename, newsuffixbase(targetname,".o"), newsuffixbase(targetname,".sig"), 
		       newsuffixbase(pathopened,".sig")
		    );
	  p = lookupword(name);
     	  if (p == NULL) {
	       errorpos(CADR(e), "signature file read, but package remains undefined");
	       return bad__K;
	       }
	  assert(issym(p));
	  p->body.symbol.defining_statement = e;
	  }
     else {
	  assert(issym(p));
	  if (p->body.symbol.flags & package_active_F) {
	       errorpos(e,"circular package dependency");
	       return bad__K;
	       }
	  }
     if (type(p) != package_T) {
	  errorpos(p,"not a package");
	  return bad__K;
	  }
     p = unpos(p);
     assert(issym(p));
     if (!withinsignature(v)) {
     	  perform(list(4,Ccode_S,void_T, String(prefixify(p,"_prepare")),String("()")),v);
	  }
     for (syms = p->body.symbol.export_list; syms != NULL; syms = CDR(syms)) {
	  node sym = CAR(syms);
	  assert(issym(sym));
	  reinternsymbol(sym,v);
	  if (fresh && pp) pp->body.symbol.export_list = cons(sym,pp->body.symbol.export_list);
	  }
     pushsignature(e,v);
     return NULL;
     }

node chkprogram(node e){
     node f = chk(e, global_scope);
     assert(global_scope->tmpdecls == NULL);
     assert(global_scope->finals == NULL);
     f = join(reverse(global_scope->decls),f);
     return f;
     }

void init_chk(){
     setchkfun(return_K,chkreturn);
     setchkfun(break_K,chkbreak);
     setchkfun(new_K,chknewarray);
     setchkfun(use_K,chk_use);
     setchkfun(GCmalloc_K,chkmalloc);
     setchkfun(Pointer_K,chkpointer);
     setchkfun(atomicPointer_K,chkatomicpointer);
     setchkfun(Type_K,chkrawtype);
     setchkfun(atomicType_K,chkatomicrawtype);
     setchkfun(arithmeticType_K,chkarithmetictype);
     setchkfun(integerType_K,chkintegertype);
     setchkfun(header_K,chkheader);
     setchkfun(leftOperator_K,chkoperator);
     setchkfun(rightOperator_K,chkoperator);
     setchkfun(prefixOperator_K,chkoperator);
     setchkfun(declarations_K,chkdeclarations);
     setchkfun(package_K,chkpackage);
     setchkfun(signature_K,chksignature);
     setchkfun(provide_K,chkprovide);
     setchkfun(blockn__K,chkblockn);
     setchkfun(foreach_K,chkforeach);
     setchkfun(andand_K,chkandand);
     setchkfun(oror_K,chkoror);
     setchkfun(for_K,chkfor);
     setchkfun(equal__K,chkassignment);
     setchkfun(if_K,chkif);
     setchkfun(when_K,chkwhen);
     setchkfun(or_K,chkor);
     setchkfun(array_K,chkarray);
     setchkfun(tarray_K,chktarray);
     setchkfun(length_K,chklength);
     setchkfun(while_K,chkwhile);
     setchkfun(until_K,chkwhile);
     setchkfun(object__K,chkobject);
     setchkfun(tagged_object_K,chkobject);
     setchkfun(function_K,chkfunction);
     setchkfun(equalequal__K,chkcompare);
     setchkfun(unequal_K,chkcompare);
     setchkfun(Ccode_K,chkCcode);
     setchkfun(dot_K,chkdot);
     setchkfun(colon__K,chkcolon);
     setchkfun(colonequal__K,chkdefinition);
     setchkfun(coloncolonequal__K,chkcoloncolonequal);
     setchkfun(chked__K,chkchked);
     setchkfun(import_K,chkimport);
     setchkfun(lvalue_K,chklvalue);
     }

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
