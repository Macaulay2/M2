/*		Copyright 1993 by Daniel R. Grayson		*/

#include "scc.h"

node chkcolon(node, env);
node chklhscolon(node, env);

bool cancellable(node e, node f){
     return (iscons(e) && (
	       equal(car(e),reserve_S) || equal(car(e),reserven_S) ||
	       equal(car(e),reservec_S) || equal(car(e),reservenc_S)
	       )
	  && iscons(f) && (
	       equal(car(f),release_S) || equal(car(f),releasec_S)
	       )
	  && member(cadr(f),cdr(e)));
     }

bool isjump(node e){
     return iscons(e) && (equal(car(e),return_S) || equal(car(e),goto_S));
     }

bool islabel(node e){
     return iscons(e) && equal(car(e),label_S);
     }

bool elidable(node e, node f){
#if 0
     return isjump(e) && !islabel(f);
#else
     return FALSE;
#endif
     }

bool iskeyword(node e){
     return issym(e) && (e->body.symbol.flags & keyword_F);
     }

bool ignorable(node ee){
     node e = ee;
     if (e==NULL) return TRUE;
     if (ispos(e)) e = e->body.position.contents;
     if (issym(e)) {
	  /* these checks should really be somewhere else */
	  if (istype(e)) errorpos(ee,"type expression misplaced");
	  else if (e == bad_K) ;
	  else if (iskeyword(e)) errorpos(ee,"keyword misplaced");
	  else if (!(e->body.symbol.flags & intern_F)) {
	       errorpos(ee,"type expression misplaced");
	       return TRUE;
	       }
	  return TRUE;
	  }
     return FALSE;
     }

node pushopt(node l, node n){
     /* l is stored in reverse order, and we are pushing on the top */
     if (ignorable(n)) return l;
     else if (l == NULL) return cons(n,NULL);
     else if (cancellable(car(l), n)) return cdr(l);
     else if (elidable(car(l), n)) return l;
     else return cons(n,l);
     }

node pushdecl(node l, node d) {
     if (occursin(cadr(d),l)) return cons(d,l);
     else return l;
     }

node pushdecllist(node l, node dl) {
     while (dl != NULL) l = pushdecl(l,car(dl)), dl = cdr(dl);
     return l;
     }

node pushoptlist(node l, node ll){
     while (ll != NULL) l = pushopt(l,car(ll)), ll = cdr(ll);
     return l;
     }

static bool isassignment(node f) {
     return iscons(f) && CAR(f) == assign_S;
     }

void perform(node w, env v){
  if (v->partial_definitions_active > 0 && isassignment(w)) {
    warningpos(v->previous_partial_definition,"previous partial definition");
    errorpos(w,"assignment while previous partial definition active");
  }
  v->before = pushopt(v->before,w);
}

void performlist(node w, env v){
     v->before = pushoptlist(v->before,w);
     }

void performafters(env v){
     performlist(v->after,v);
     v->after = NULL;
     }

void performfinals(env v){
     performlist(v->finals,v);
     v->finals = NULL;
     }

void pushenvblock(env *vp){
     env w = new(struct ENV);	/* this gets freed in popenv */
     ZERO_MEM(w);
     w->previous = *vp;
     *vp = w;
     }

void pushenv(env *vp){
     env w = new(struct ENV);	/* this gets freed in popenv */
     ZERO_MEM(w);
     w->previous = *vp;
     *vp = w;
     w->before = w->previous->before;
     w->previous->before = NULL;
     }

void popenv(env *vp){
     env w = *vp;
     env v = w->previous;
     *vp = v;
     v->decls = join(w->decls,v->decls);
     v->tmpdecls = join(w->tmpdecls,v->tmpdecls);
     v->signature = join(w->signature,v->signature);
     assert(w->before == NULL);
     assert(w->after == NULL);
     assert(w->finals == NULL);
     unwind(&w->symbols);
     }

void errorpopenv(env *vp){
     env w = *vp;
     env v = w->previous;
     *vp = v;
     unwind(&w->symbols);
     }

void pushbackenv(env v){
     v->previous->before = join(v->before,v->previous->before);
     v->before = NULL;
     v->previous->finals = join(v->previous->finals,v->finals);
     v->finals = NULL;
     v->previous->symbols = join(v->symbols,v->previous->symbols);
     v->symbols = NULL;
     }

bool typematch(node e, node f){
     return e == f || e == undefined_T || f == undefined_T || e == returned_T || f == returned_T ;
     }

node lookupfunction(node fun, node argtypes){
     node slist;
     node f;
     fun = unpos(fun);
     if (equal(argtypes,functionargtypes(type(fun)))) return fun;
     if (fun->tag == symbol_tag) {
     	  f = fun->body.symbol.name;
     	  assert(isstr(f));
     	  for (slist = f->body.string.symbol_list; 
	       slist!=NULL; slist=cdr(slist)) {
	       node sym = car(slist);
	       node t;
	       if (sym == fun) continue;
	       t = type(sym);
	       if (!isfunctiontype(t)) continue;
	       if (equal(argtypes,functionargtypes(t))) return sym;
	       }
	  }
#if 0
     if (checkargtypes(argtypes,functionargtypes(type(fun)))) return fun;
     if (fun->tag == symbol_tag) {
     	  for (slist = f->body.string.symbol_list; 
	       slist!=NULL; slist=cdr(slist)) {
	       node sym = car(slist);
	       node t;
	       if (sym == fun) continue;
	       t = type(sym);
	       if (!isfunctiontype(t)) continue;
	       if (checkargtypes(argtypes,functionargtypes(t))) return sym;
	       }
	  }
#endif
     return NULL;
     }

node lookupexactfunction(node f, node argtypes){
     node slist;
     f = unpos(f);
     assert(f->tag == string_tag);
     for (slist = f->body.string.symbol_list; slist!=NULL; slist=cdr(slist)) {
	  node sym = car(slist);
	  node t = type(sym);
	  if (!isfunctiontype(t)) continue;
	  if (equal(argtypes,functionargtypes(t))) return sym;
	  }
     return NULL;
     }

void chklistn(node e, env v) {
     for (; e != NULL; e = cdr(e)) {
	  node w = chk(car(e),v);
	  perform(w,v);
	  performafters(v);
	  }
     }

node chklist(node e, env v) {
     node r = NULL;
     chklistn(e,v);
     r = reverse(v->before);
     v->before = NULL;
     return r;
     }

node chkblock(node e, env v){
     pushenv(&v);
     for (; e != NULL; e = cdr(e)) {
	  node w = chk(car(e),v);
	  perform(w,v);
	  performafters(v);
	  }
     performfinals(v);
     unwind(&v->symbols);
     pushbackenv(v);
     popenv(&v);
     return NULL;
     }

#if 1
node chkblockn(node e,env v){
     node body = cdr(e);
     node s;
     pushenv(&v);
     s = chklist(allbutone(body),v);
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
     popenv(&v);
     return s;
     }
#else
     /* old version, with the defect that variables defined in the last
     	  statement of the body get too broad a scope */
node chkblockn(node e,env v){
     node body = cdr(e);
     node s;
     pushenv(&v);
     s = chklist(allbutone(body),v);
     v->previous->decls = join(v->decls,v->previous->decls);
     v->decls = NULL;
     performlist(s,v->previous);
     v->previous->after = join(v->finals,v->previous->after);
     v->finals = NULL;
     s = chk(last(body),v->previous);
     popenv(&v);
     return s;
     }
#endif

node enblock(node e){
     if (iscons(e) && (
	       equal(car(e),block_K) ||
	       equal(car(e),blockn_K) ||
	       equal(car(e),block1_K))) return e;
     return list(2,blockn_K,e);
     }

node newlabel(){
     static int seqno = 0;
     char buf[20];
     sprintf(buf,"L%d_",seqno++);
     return UniqueString(buf);
     }

node take(node x, node a){
     return list(3,take_S,x,a);	/* x->a */
     }

node part(node x, node a){
     return list(3,part_S,x,a);	/* x.a */
     }

node array_take(node x, node a){
     assert(isarraytype(type(x)));
     assert(type(a) == uint_T || type(a) == int_T);
     return list(3,array_take_S,x,a);
     }

node arraylength(node arr){
     node t = type(arr), m;
     assert(isarraytype(t));
     m = typedeftail(t);
     return length(m)==1 ? take(arr,len__S) : cadr(m);
     }

node structcast(node typ, node typetag, node ptr,env v){
     assert(typetag != NULL);
     ptr = list(3,cast_S,object_type_S,ptr);
     if (gcc) {
	   return list(3,cast_S,typ, list(3,brace_list_S,typetag,ptr));
	   }
     else {
	  node tmp = newstmp(typ,v,TRUE);
	  perform(list(3,assign_S,part(tmp,type__S),typetag),v);
	  perform(list(3,assign_S,part(tmp,  ptr_S),ptr),v);
	  return tmp;
	  }
     }

node cast(node t, node e, env v) {
     node u;
     if (e == bad_K) return bad_K;
     u = type(e);
     if (u == undefined_T) return bad_K;
     if (t == u) return e;
     if (iscompositeortype(t)) {
	  if (iscompositeortype(u)) {
#if 1
     	       return notimpl(e);
#else
	       return structcast(t,part(e,type__S),part(e,ptr_S),v);
#endif
	       }
	  else {
	       return structcast(t,ormemberindex(u,t),e,v);
	       }
	  }
     else {
	  if (iscompositeortype(u)) {
	       return list(3,cast_S,t,part(e,ptr_S));
	       }
     	  else {
	       return list(3,cast_S,t,e);
	       }
	  }
     }

void assign(node lhs, node rhs, env v){
     node ltype = type(lhs);
     node rtype = type(rhs);
     node crhs;
     if (ltype == void_T || rtype == void_T) return;
     if (ltype != rtype && !rtype->body.type.arithmetic_type) {
     	  crhs = cast(ltype,rhs,v);
	  }
     else crhs = rhs;
     perform( list(3, assign_S, lhs, crhs), v);
     if (reservable(realtype(rhs)) && ltype!=null_T) {
	  if (couldbenull(ltype) && couldbenull(rtype)) {
	       if (iscompositeortype(ltype)) {
	       	    perform(list(3,reservenc_S,lhs,rhs),v);
		    }
	       else {
	       	    perform(list(3,reserven_S,lhs,rhs),v);
		    }
	       }
	  else {
	       if (iscompositeortype(ltype)) {
	       	    perform(list(3,reservec_S,lhs,rhs),v);
		    }
	       else {
	       	    perform(list(3,reserve_S,lhs,rhs),v);
		    }
	       }
	  }
     }

void reserve(node s, env v){
     node t;
     if (gc) return;
     t = type(s);
     if (reservable(t)) {
	  if (couldbenull(realtype(s))) {
	       if (iscompositeortype(t)) {
     	       	    perform(list(2, reservenc_S, s),v);
		    }
	       else {
     	       	    perform(list(2, reserven_S, s),v);
		    }
	       }
	  else {
	       if (iscompositeortype(t)) {
     	       	    perform(list(2, reservec_S, s),v);
		    }
	       else {
     	       	    perform(list(2, reserve_S, s),v);
		    }
	       }
	  }
     }

void releasenow(node sym, env v){
     node t;
     if (gc) return;
     t = type(sym);
     if (reservable(t)) {
	  if (iscompositeortype(t)) {
	       perform(list(2,releasec_S,sym),v);
	       }
	  else {
	       perform(list(2,release_S,sym),v);
	       }
	  }
     }

void releaseafter(node sym, env v){
     node t;
     if (gc) return;
     t = type(sym);
     if (reservable(t)) {
	  if (iscompositeortype(t)) {
	       push(v->after, list(2,releasec_S,sym));
	       }
	  else {
	       push(v->after, list(2,release_S,sym));
	       }
	  }
     }

void releaselater(node sym, env v){
     node t;
     if (gc) return;
     t = type(sym);
     if (reservable(t)) {
	  if (iscompositeortype(t)) {
	       push(v->previous->after, list(2,releasec_S,sym));
	       }
	  else {
	       push(v->previous->after, list(2,release_S,sym));
	       }
	  }
     }

void releasefinal(node sym, env v){
     node t;
     if (gc) return;
     t = type(sym);
     assert(issym(sym));
     if (reservable(t)) {
	  if (iscompositeortype(t)) {
	       if (sym->body.symbol.flags & export_F) {
	       	    push(v->previous->finals, list(2,releasec_S,sym));
		    }
	       else {
	       	    push(v->finals, list(2,releasec_S,sym));
		    }
	       }
	  else {
	       if (sym->body.symbol.flags & export_F) {
	       	    push(v->previous->finals, list(2,release_S,sym));
		    }
	       else {
	       	    push(v->finals, list(2,release_S,sym));
		    }
	       }
	  }
     }

void setup(node s, env v){
     node t;
     if (gc) return;
     t = type(s);
     if (isobjecttype(t) || isarraytype(t)) {
	  assign(take(s, refs__S), one, v);
	  }
     }

node chkcompare(node e, env v, node test) {
     node f,g,t,u;
     bool i,j;
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
     	  if (i) f = part(f,ptr_S);
     	  if (j) g = part(g,ptr_S);
     	  if (i ^ j) f = list(3,cast_S,null_T,f), g = list(3,cast_S,null_T,g);
     	  return list(3,test,f,g);
	  }
     if (t != u && (isortype(t) || isortype(u))) {
	  f = list(3,cast_S,null_T,f);
	  g = list(3,cast_S,null_T,g);
     	  return list(3,test,f,g);
	  }
     else {
     	  return list(3,test,f,g);
	  }
     }

node chkequal(node e, env v){
     return chkcompare(e,v,equal_S);
     }

node chkunequal(node e, env v){
     return chkcompare(e,v,unequal_S);
     }

node chkint(node e, env v){
     node f = chk(e,v);
     if (type(f) != int_T) {
	  errorpos(e,"should be an int");
	  return bad_K;
	  }
     return f;
     }

node chkstring(node e){
     node s = unpos(e);
     if (!isstr(s)) {
	  errorpos(e,"should be a symbol");
	  return bad_K;
	  }
     return s;
     }

node entmp(node e, env v){
     node tmp = newtmp(type(e),v,TRUE);
     perform(list(3,assign_S,tmp,e),v);
     return tmp;
     }

bool isposint(node e){
     e = unpos(e);
     return e->tag == int_const_tag && e->body.int_const.contents[0] != '-';
     }

bool isnegint(node e){
     e = unpos(e);
     return e->tag == int_const_tag && e->body.int_const.contents[0] == '-';
     }

node chkfor(node e, env v) {
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
	  return bad_K;
	  }
     argblock = cadr(e);
     body = cddr(e);
     if (!iscons(argblock)) {
	  errorpos(argblock,"argument block should be a list");
	  indx = NULL;
	  init = NULL;
	  final = one;
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
	  final = one;
	  step = NULL;
	  }
     if (indx == NULL) {
     	  indx = newtmp(int_T,v,TRUE);
	  }
     else {
	  indx = chkstring(indx);
	  if (indx == bad_K) return bad_K;
     	  indx = newsymbol(indx,int_T,v,intern_F|initialized_F);
     	  v->tmpdecls = cons(list(2,define_S,indx), v->tmpdecls);
	  }
     if (init == NULL) {
	  init = one;
	  }
     else {
	  init = chkint(init,v);
	  if (init == bad_K) return bad_K;
	  }
     final = chkint(final,v);
     if (final == bad_K) return bad_K;
     final = entmp(final,v);
     if (step == NULL) {
	  step = one;
	  }
     else {
	  step = chkint(step,v);
	  if (step == bad_K) return bad_K;
	  step = entmp(step,v);
	  }
     assign(indx,init,v);
     perform(list(2,goto_S,skiplabel),v);
     perform(list(2,label_S,looplabel),v);
     perform(list(3,assign_S,indx,list(4,infix_S,plus_S,indx,step)),v);
     perform(list(2,label_S,skiplabel),v);
     if (isposint(step)) {
     	  perform(list(3,if_S,
	       	    list(4,infix_S,gt_S,indx,final),
	       	    list(2,goto_S,breaklabel)),v);
	  }
     else if (isnegint(step)) {
     	  perform(list(3,if_S,
	       	    list(4,infix_S,lt_S,indx,final),
	       	    list(2,goto_S,breaklabel)),v);
	  }
     else {
     	  perform(list(3,if_S,
		    list(4,infix_S,oror_S,
			 list(4,infix_S,andand_S,
			      list(4,infix_S,gt_S,indx,final),
			      list(4,infix_S,ge_S,step,zero)),
			 list(4,infix_S,andand_S,
			      list(4,infix_S,lt_S,indx,final),
			      list(4,infix_S,lt_S,step,zero))),
	       	    list(2,goto_S,breaklabel)),v);
	  }
     pushenv(&v);
     v->loop = TRUE;
     v->break_loop_label = breaklabel;
     v->continue_loop_label = looplabel;
     chklistn(body,v);
     performfinals(v);
     unwind(&v->symbols);
     pushbackenv(v);
     popenv(&v);
     perform(list(2,goto_S,looplabel),v);
     perform(list(2,label_S,breaklabel),v);
     return NULL;
     }

node chksizeof(node e, env v){
     node x, t;
     if (length(e) != 2) return badnumargs(e,1);
     x = chk(CADR(e),v);
     if (x == bad_K) return bad_K;
     t = type(x);
     if (t == type_T) {
	  errorpos(e,"invalid argument to sizeof");
	  return bad_K;
	  }
     return list(2,sizeof_S,x);
     }

node chkforeach(node e, env v) {
#if 0
      (foreach (e a) ... )   		e = element of array a, as lvalue    
      (foreach (i e a) ... )		... and also i == index, readonly
      (foreach (i e a s) ... )		... ... and with stepsize s
#endif
     node argblock, var, arr, arr2, tarr, tmem, code, arrtmp, indx, loop, veryend;
     node step, endlabel=newlabel();
     bool increasing=FALSE, decreasing=FALSE, breaklabelused;
     if (length(e) == 1) {
	  errorpos(e,"need at least one arguments");
	  return bad_K;
	  }
     argblock = cadr(e);
     code = cddr(e);
     if (!iscons(argblock)) {
	  errorpos(argblock,"argument block should be a list");
	  return bad_K;
	  }
     switch(length(argblock)) {
	  case 2: {
     	       indx = NULL;
	       var = car(argblock);
	       arr = cadr(argblock);
	       step = one;
	       break;
	       }
	  case 3: {
     	       indx = car(argblock);
	       var = cadr(argblock);
	       arr = caddr(argblock);
	       step = one;
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
	       return bad_K;
	       }
	  }
     if (!isstrpos(var)) {
	  errorpos(var,"should be a symbol");
	  return bad_K;
	  }
     if (isint(unpos(step))) {
	  increasing = isposint(step);
	  decreasing = isnegint(step);
	  }
     else {
	  step = chkint(step,v);
	  if (step == bad_K) return bad_K;
	  step = entmp(step,v);
	  }
     arr2 = arr;
     arr = chk(arr,v);
     if (arr == bad_K) return bad_K;
     tarr = type(arr);
     if (!isarraytype(tarr)) {
	  errorpos(arr2, "should be an array");
	  return bad_K;
	  }
     pushenv(&v);
     arrtmp = newtmp(tarr,v,TRUE);
     assign(arrtmp,arr,v);
     releasefinal(arrtmp,v);
     veryend = newlabel();
     perform(list(3,if_S,
	       list(4,infix_S,equal_S,arraylength(arrtmp),zero),
	       list(2,goto_S,veryend)),v);
     pushenv(&v);
     tmem = elementtype(tarr);
     var = newsymbol(var,tmem,v,intern_F|initialized_F);
     if (indx == NULL) {
     	  indx = newtmp(int_T,v,TRUE);
	  }
     else {
	  indx = chkstring(indx);
	  if (indx == bad_K) return bad_K;
     	  indx = newsymbol(indx,int_T,v,intern_F|readonly_F|initialized_F);
     	  v->tmpdecls = cons(list(2,define_S,indx), v->tmpdecls);
	  }
     setcprintvalue(var,array_take(arrtmp,indx));
     if (increasing) {
     	  perform(list(3,assign_S,indx,zero),v);
	  }
     else if (decreasing) {
	  perform(list(3,assign_S,indx,
		    list(4,infix_S,minus_S,arraylength(arrtmp),one)),v);
	  }
     else {
	  perform(list(4,if_S,
		    list(4,infix_S,gt_S,step,zero),
		    list(3,assign_S,indx,zero),
		    list(3,assign_S,indx,
		    	 list(4,infix_S,minus_S,arraylength(arrtmp),one))),v);
	  }
     loop = newlabel();
     perform(list(2,label_S,loop),v);
     v->loop = TRUE;
     v->break_loop_label = endlabel;
     chklistn(code,v);
     breaklabelused = v->break_loop_label_used;
     performfinals(v);
     unwind(&v->symbols);
     pushbackenv(v);
     popenv(&v);
     perform(list(3,assign_S,
	       indx,
	       list(4,infix_S,plus_S,indx,step)),v);
     if (increasing) {
     	  perform(list(3,if_S, 
	       	    list(4,infix_S,lt_S,indx,arraylength(arrtmp)), 
	       	    list(2,goto_S,loop)),v);
	  }
     else if (decreasing) {
	  perform(list(3,if_S,
		    list(4,infix_S,ge_S,indx,zero),
		    list(2,goto_S,loop)),v);
	  }
     else {
	  perform(list(3,if_S,
		    list(4,infix_S,oror_S,
		    	 list(4,infix_S,andand_S,
		    	      list(4,infix_S,gt_S,step,zero),
			      list(4,infix_S,lt_S,indx,arraylength(arrtmp))),
			 list(4,infix_S,andand_S,
			      list(4,infix_S,lt_S,step,zero),
			      list(4,infix_S,ge_S,indx,zero))),
		    list(2,goto_S,loop)),v);
     	  }
     if (breaklabelused) perform(list(2,label_S,endlabel),v);
     perform(list(2,label_S,veryend),v);
     performfinals(v);
     unwind(&v->symbols);
     pushbackenv(v);
     popenv(&v);
     return NULL;
     }

node chkuntil(node e, env v, bool until) {
     bool breaklabelused, afterlabelused = FALSE;
     node b,bafter,l = newlabel(), afterlabel=newlabel(), endlabel=newlabel();
     if (length(e) == 1) {
     	  errorpos(e,"while-statement with no arguments");
     	  return NULL;
	  }
     pushenv(&v);
     perform(list(2,label_S,l),v);
     b = chk(enblock(cadr(e)),v);
     if (b != bad_K && type(b) != bool_T) {
	  errorpos(cadr(e),"condition should be of type bool");
	  }
     bafter = v->after, v->after = NULL;
     if (equal(b,true_K)) {
	  if (until) {
	       perform(list(2,goto_S,afterlabel),v);
	       afterlabelused = TRUE;
	       }
	  else {
	       }
	  }
     else if (equal(b,false_K)) {
	  if (until) {
	       }
	  else {
	       perform(list(2,goto_S,afterlabel),v);
	       afterlabelused = TRUE;
	       }
	  }
     else {
     	  perform(list(3,if_S,
	       	    until ? b : list(3,prefix_S,not_S,b),
	       	    list(2,goto_S,afterlabel)),v);
	  afterlabelused = TRUE;
	  }
     performlist(bafter,v);
     v->loop = TRUE;
     v->break_loop_label = endlabel;
     v->continue_loop_label = l;
     chk(cons(block_K,cddr(e)),v);
     breaklabelused = v->break_loop_label_used;
     pushbackenv(v);
     popenv(&v);
     perform(list(2,goto_S,l),v);
     if (afterlabelused) perform(list(2,label_S,afterlabel),v);
     performlist(bafter,v);
     if (breaklabelused) perform(list(2,label_S,endlabel),v);
     if (b == bad_K) return bad_K;
     return NULL;
     }

node chktypecase(node e, env v){
     int i, nulls = 0;
     bool hadelse = FALSE, doswitch;
     node sym1, sym, typ, types, labels = NULL, firstcasecodetype = NULL;
     node cases, vtmp=NULL;
     node casetypes = NULL, after = NULL;
     node endlabel = newlabel();
     int ntypes;
     if (length(e) == 1) {
	  errorpos(e,"should have at least one argument");
	  return bad_K;
	  }
     sym1 = cadr(e);
     cases = cddr(e);
     pushenv(&v);
     sym = chk(sym1,v);
     after = v->after, v->after = NULL;
     pushbackenv(v);
     popenv(&v);
     if (sym == bad_K) return bad_K;
     typ = type(sym);
     if (!isortype(typ)) {
	  errorpos(cadr(e),"when-clause requires an or-type");
	  return bad_K;
	  }
     types = typedeftail(typ);
     ntypes = length(types);
     for (i=1; i<=ntypes; i++) {
	  node lab = newlabel();
	  labels = cons(lab,labels);
	  if (nth(types,i)==null_T) {
	       nulls++;
	       if (nulls == 1) {
	       	    perform(
		    	 list(3,if_S, 
			      list(2,isnull_S,
				   typ->body.type.composite 
				   ? part(sym,ptr_S)
				   : sym), 
			      list(2,goto_S,lab)),v);
		    }
	       }
	  }
     labels = reverse(labels);
     doswitch = nulls + 1 != ntypes;
     /* we don't have a type tag when there is only one non-null type
        in the union */
     if (doswitch) {
	  perform(list(5,
		    Ccode_S,
		    void_T,
		    String("switch ("),
		    part(sym,type__S),
		    String(") {")),v);
	  }
     else {
     	  for (i=1; i<=ntypes; i++) {
	       if (nth(types,i) != null_T) {
		    perform(list(2,goto_S,nth(labels,i)),v);
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
	       for(i=1; i<=length(types); i++) {
		    if (!member(nth(types,i),casetypes)) {
			 if (doswitch) perform(list(5,
				   Ccode_S,
				   void_T, 
				   String("case "),
				   integer(i),
				   String(":")),
			      v);
			 perform(list(2,label_S, nth(labels, i)),v);
		    	 }
		    }
	       }
	  else {
	       cas = CAAR(cases);
	       casecode = CADAR(cases);
	       if (iscons(cas) && equal(car(cas),colon_S)) {
		    if (length(cas) != 3) return badnumargs(cas,2);
		    if (casetype == undefined_T) return bad_K;
		    casesym = CADR(cas);
		    casetype = totype(chktype(CADDR(cas),v));
		    }
	       else if (cas != NULL) {
		    casetype = totype(chktype(cas,v));
		    casesym = NULL;
		    }
	       if (casetype == undefined_T) return bad_K;
	       if (!member(casetype,types)) {
		    errorpos(iscons(cas)&&length(cas)==3 ? caddr(cas) : cas,
			 "type not among those represented by the when-clause");
		    }
	       else {
		    if (doswitch) perform(list(5,
			      Ccode_S,
			      void_T,
			      String("case "),
			      integer(memberindex(casetype,types)),
			      String(":")),
			 v);
		    perform(list(2,label_S,
			      nth(labels, memberindex(casetype,types))),v);
		    }
	       casetypes = cons(casetype,casetypes);
	       }
	  pushenv(&v);
	  if (casesym != NULL) {
	       casesym = newsymbol(casesym,casetype,v,intern_F|initialized_F);
	       push(v->decls,list(2,define_S,casesym));
	       assign(casesym,sym,v);
	       releasefinal(casesym,v);
	       }
	  performlist(after,v);
	  casecode = chk(list(2,blockn_K,casecode),v);
	  casecodetype = type(casecode);
	  if (firstcasecodetype == NULL) {
	       if (casecodetype != returned_T) {
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
	       if (firstcasecodetype != void_T && casecodetype != returned_T) assign(vtmp,casecode,v);
	       else perform(casecode,v);
	       }
	  unwind(&v->symbols);
	  performafters(v);
	  performfinals(v);
	  pushbackenv(v);
	  popenv(&v);
	  perform(list(2,goto_S,endlabel),v);
	  }
     if (!hadelse) {
	  /* check for a missing case */
	  int missing = 0;
     	  for(i=1; i<=length(types); i++) {
	       if (!member(nth(types,i),casetypes)) missing++;
	       }
	  if (missing > 0) {
	       char buf[400];
	       releasenow(vtmp,v);
	       strcpy(buf,missing > 1 ? "missing cases" : "missing case");
	       for(i=1; i<=length(types); i++) {
		    if (!member(nth(types,i),casetypes)) {
		    	 sprintf(buf + strlen(buf)," %d",i);
		    	 }
		    }
	       errorpos(e,buf);
	       return bad_K;
	       }
	  }
     releaseafter(vtmp,v);
     perform(list(2,label_S,endlabel),v);
     if (doswitch) perform(list(3,Ccode_S,void_T,String("}")),v);
     return vtmp;
     }

bool reachable(env v) {
     return v->before == NULL || !isjump(car(v->before));
     }

node chkandand(node e, env v) {
     node b, bafter, l = newlabel(), c, vtmp = newtmp(bool_T,v,TRUE);
     pushenv(&v);
     b = chk(cadr(e),v);
     if (b != bad_K && type(b) != bool_T) {
	  errorpos(cadr(e),"condition should be of type bool");
	  }
     bafter = v->after, v->after = NULL;
     assign(vtmp,b,v);
     perform(list(3,if_S,list(3,prefix_S,not_S,vtmp),list(2,goto_S,l)),v);
     c = chk(enblock(caddr(e)),v);
     if (c!=bad_K && type(c) != bool_T) {
	  errorpos(caddr(e),"condition should be of type bool");
	  }
     assign(vtmp,c,v);
     performafters(v);
     perform(list(2,label_S,l), v);
     performlist(bafter,v);
     if (b==bad_K || c==bad_K) return bad_K;
     pushbackenv(v);     
     popenv(&v);
     return vtmp;
     }

node chkoror(node e, env v) {
     node b, bafter, l = newlabel(), c, vtmp = newtmp(bool_T,v,TRUE);
     pushenv(&v);
     b = chk(cadr(e),v);
     if (b != bad_K && type(b) != bool_T) {
	  errorpos(cadr(e),"condition should be of type bool");
	  }
     bafter = v->after, v->after = NULL;
     assign(vtmp,b,v);
     perform(list(3,if_S,vtmp,list(2,goto_S,l)),v);
     c = chk(enblock(caddr(e)),v);
     if (c!=bad_K && type(c) != bool_T) {
	  errorpos(caddr(e),"condition should be of type bool");
	  }
     assign(vtmp,c,v);
     performafters(v);
     perform(list(2,label_S,l), v);
     performlist(bafter,v);
     if (b==bad_K || c==bad_K) return bad_K;
     pushbackenv(v);     
     popenv(&v);
     return vtmp;
     }

node chkif(node e, env v) {
     if (length(e) == 4 || length(e) == 3) {
	  node b,bafter,thenclause,elseclause = NULL,l = newlabel(), m=NULL;
	  node thentype = NULL, elsetype = NULL, vtmp = NULL;
     	  pushenv(&v);
	  b = chk(enblock(cadr(e)),v);
	  if (b != bad_K && type(b) != bool_T) {
	       errorpos(cadr(e),"condition should be of type bool");
	       }
	  bafter = v->after, v->after = NULL;
	  perform(list(3,if_S,b,list(2,goto_S,l)),v);
	  performlist(bafter,v);
	  if (length(e)==4) elseclause = chk(enblock(cadddr(e)),v);
	  elsetype = type(elseclause);
	  if (elsetype != void_T && elsetype != returned_T) {
	       vtmp = newtmp(elsetype,v,TRUE);
	       assign(vtmp,elseclause,v);
	       }
	  else if (elseclause != NULL) perform(elseclause,v);
	  performafters(v);
	  if (reachable(v)) {
	       m = newlabel();
	       perform(list(2,goto_S,m), v);
	       }
	  perform(list(2,label_S,l), v);
	  performlist(bafter,v);
	  thenclause = chk(enblock(caddr(e)),v);
	  thentype = type(thenclause);
	  if (thenclause == bad_K || elseclause == bad_K || b == bad_K) return bad_K;
	  if (elsetype == deferred_T) {
	       errorpos(caddr(e),"undefined");
	       return bad_K;
	       }
	  if (type(thenclause) == deferred_T) {
	       errorpos(caddr(e),"undefined");
	       return bad_K;
	       }
	  if (length(e) == 4 && !typematch(thentype,elsetype)) {
	       errorpos(e,"then/else clauses not of same type");
	       return bad_K;
	       }
	  if (elsetype != void_T && thentype != returned_T) {
	       if (vtmp == NULL && elsetype == returned_T) vtmp = newtmp(thentype,v,TRUE);
	       assign(vtmp,thenclause,v);
	       releaselater(vtmp,v);
	       }
	  else perform(thenclause,v);
	  performafters(v);
	  if (m != NULL) perform(list(2,label_S,m), v);
	  pushbackenv(v);
	  popenv(&v);
	  return vtmp;
	  }
     errorpos(e,"if-statement takes 2 or 3 arguments");
     return NULL;
     }

node chkblock1(node e, env v){
     v = NULL;
     errorpos(e,"not implemented yet");
     return NULL;
     }

node chkrecurs(node p,env v){
     node types=NULL;
     for (;p!=NULL;p = cdr(p)) {
	  node sym, typ;
	  if (!iscons(car(p)) || length(car(p))!= 2) {
	       errorpos(car(p),"invalid type declaration");
	       continue;
	       }
	  if (!isstrpos(caar(p))) {
	       errorpos(caar(p),"should be symbol");
	       continue;
	       }
	  typ = newtype(cadar(p),NULL,FALSE);
	  sym = newsymbol(caar(p),type_T,v,intern_F|initialized_F);
	  if (sym == NULL) return bad_K;
	  sym->body.symbol.value = typ;
	  typ->body.type.name = sym;
	  types = cons(typ,types);
	  }
     types = reverse(types);
     {
	  node m = types;
	  node f = NULL;
	  while (m != NULL) {
	       node val = car(m)->body.type.definition;
	       node t = chk(val,v);
	       int fl = length(f);
	       if (type(t) != type_T) {
		    errorpos(NULL /* sigh */ ,"not valid type");
		    return NULL;
		    }
	       t = ExpandType(t,&f);
	       if (fl < length(f)) {
		    assert(car(f) == t);
		    }
	       if (t->body.type.definition != NULL) {
		    car(m)->body.type.definition = t->body.type.definition;
		    }
	       else car(m)->body.type.definition = t;
	       if (fl < length(f)) f = cons(car(m),cdr(f));
	       m = cdr(m);
	       }
	  totypesRec(f);
	  }
     return NULL;
     }

node chkCcode(node e, env v){
     bool bad = FALSE;
     node r = NULL;
     node t;
     if (length(e) < 2) {
	  errorpos(e,"Ccode takes at least one argument");
	  return bad_K;
	  }
     t = totype(chktype(cadr(e),v));
     e = CDDR(e);
     while (e != NULL) {
	  node b = car(e);
	  node u = unpos(b);
	  if (u->tag != string_const_tag) {
	       u = chk(b,v);
	       }
	  if (u == bad_K) bad=TRUE;
	  r = cons(u,r);
	  e = CDR(e);
	  }
     if (bad) return bad_K;
     else {
	  node z;
	  r = reverse(r);
	  z = cons(Ccode_S,cons(t,r));
	  if (t != void_T) z = entmp(z,v);
	  return z;
          }
     }

void returngather(env v){
     env w = v;
     while (TRUE) {
	  assert( v != NULL );
	  performlist(v->after,w);
	  performlist(v->finals,w);
	  if (v->defun) return;
	  v = v->previous;
	  }
     }

void breakgather(env v){
     env w = v;
     while (TRUE) {
	  assert( v != NULL );
	  performlist(v->after,w);
	  performlist(v->finals,w);
	  if (v->loop) return;
	  v = v->previous;
	  }
     }

node defunrettype(env v){
     while (v!=NULL && !v->defun) v = v->previous;
     if (v == NULL) return NULL;
     return v->rettype;
     }

bool inside_defun(env v){
     env w = v;
     for (w=v; w != NULL ; w = w->previous) {
	  if (w->defun) return TRUE;
	  }
     return FALSE;
     }

bool returns_disabled(env v){
     env w = v;
     for (w=v; w!=NULL && !w->defun; w = w->previous) {
	  if (w->disable_breaks) return TRUE;
	  }
     return FALSE;
     }

node chkreturn(node e,env v){
     node rettype;
     if (!inside_defun(v)) {
	  errorpos(e,"return should be used in the code body of a function");
	  return bad_K;
	  }
     rettype = defunrettype(v);
     if (length(e) > 2) {
	  errorpos(e,"return takes at most one argument");
	  return bad_K;
	  }
     if (returns_disabled(v)) {
	  errorpos(e,"return not allowed in this context");
	  return bad_K;
	  }
     if (length(e) == 1) {
	  if (rettype != void_T) {
	       errorpos(e,"return value missing");
	       return bad_K;
	       }
     	  returngather(v);
	  perform(list(1,return_S),v);
	  return returnedThing_K;
	  }
     else {
	  node r = chk(cadr(e),v);
	  node rtype = type(r);
	  if (!subtype(rtype,rettype)) return typemismatch(cadr(e));
#if 1				/* compare with chkdefun, chkfunctiontype */
          {
	       node tmp;
	       tmp = newtmp(rettype,v,TRUE);
	       perform(list(3,assign_S,tmp,cast(rettype,r,v)),v);
     	       
#if 0
	       reserve(tmp,v);
#else
	       reserve(r,v);
#endif
	       tmp = enpos(tmp,pos(r));
	       r = tmp;
	       }
     	  returngather(v);
	  perform(list(2,return_S,r),v);
#else
	  reserve(r,v);
     	  returngather(v);
	  perform(list(2,return_S,cast(rettype,r,v)),v);
#endif
	  return returnedThing_K;
	  }
     }

bool inside_loop(env v){
     env w = v;
     for (w=v; w != NULL; w = w->previous) {
	  if (w->loop) return TRUE;
	  if (w->defun) return FALSE;
	  }
     return FALSE;
     }

bool breaks_disabled(env v){
     env w = v;
     for (w=v; w!=NULL && !w->loop; w = w->previous) {
	  if (w->disable_breaks) return TRUE;
	  }
     return FALSE;
     }

node getbreaklabel(env v){
     env w = v;
     for (w=v; !w->loop; w = w->previous) ;
     w->break_loop_label_used = TRUE;
     return w->break_loop_label;
     }

env package_env(env v){
     while (TRUE) {
	  if (v == NULL) return NULL;
	  if (v->current_package != NULL) return v;
	  v = v->previous;
	  }
     }

node enclosing_package(env v){
     env w = package_env(v);
     if (w == NULL) return NULL;
     return w->current_package;
     }

bool withinsignature(env v){
     node p = enclosing_package(v);
     if (p == NULL) return FALSE;
     if (p->body.symbol.flags & signature_F) return TRUE;
     return FALSE;
     }

void pushsignature(node e, env v){
     if (!withinsignature(v)) push(v->signature,e);
     }

node chkpackage1(node e, env v, bool sig){
     node s, sym, body, initsymb, finalsymb;
     if (length(e) != 3) return badnumargs(e,2);
     s = CADR(e);
     body = CADDR(e);
     if (!isstrpos(s)) {
	  errorpos(s,"identifier required");
	  return bad_K;
	  }
     sym = newsymbol(s,package_T,v,intern_F|initialized_F|(sig?signature_F:0));
     pushenvblock(&v);
     assert(issym(sym));
     v->current_package = sym;
     chk(body,v);
     global.decls = join(v->decls,global.decls);
     v->decls = NULL;
     initsymb = newsymbol(
	  UniqueString(prefixify(sym,"_prepare")),
	  totype(list(3,function_S,NULL,void_T)),
	  v->previous,
	  intern_F|initialized_F|literal_F|visible_F|constant_F|export_F
	  );
     finalsymb = newsymbol(
	  UniqueString(prefixify(sym,"_final")),
	  totype(list(3,function_S,NULL,void_T)),
	  v->previous,
	  intern_F|initialized_F|literal_F|visible_F|constant_F|export_F
	  );
     if (sig) {
	  push(global.decls,list(2,define_S,initsymb));
	  }
     else {
	  node p = enclosing_package(v->previous);
	  node code = NULL;
     	  push(code,
	       list(3,Ccode_S,void_T,
		    String("final_list = &this_final")));
     	  push(code,
	       list(3,Ccode_S,void_T,
		    String("this_final.next = final_list")));
     	  push(code,
	       list(4,Ccode_S,void_T,
		    String("this_final.final = "),finalsymb));
 	  code = join(reverse(v->before),code);
	  if (!gc && !do_memstats) {
	       push(code,
		    list(3,Ccode_S,void_T,
			 String("do_memstats = 0")));
	       }
	  push(code,
	       list(3,Ccode_S,void_T, 
		    String("if (called_yet) return; else called_yet = 1")));
	  if (p != NULL) {
	       push(code, list(4, Ccode_S,void_T, 
			 String(prefixify(p,"_prepare")),
			 String("()")));
	       }
	  push(code,list(3,Ccode_S,void_T,
		    String("static int called_yet")));
     	  push(code,
	       list(3,Ccode_S,void_T,
		    String("static struct FINAL this_final")));
	  code = pushoptlist(code,v->tmpdecls);
	  v->before = NULL;
	  v->tmpdecls = NULL;
	  push(global.decls,
	       join( list(3,defun_S,list(1,initsymb),void_T), code));
	  }
     if (sym->body.symbol.flags & signature_F) {
	  push(global.decls,list(2,define_S,finalsymb));
	  }
     else {
	  node code = join(v->after,v->finals);
	  push(global.decls,
	       join( list(3,defun_S,list(1,finalsymb),void_T), code));
	  }
     v->after = NULL;
     v->finals = NULL;
     if (!(sym->body.symbol.flags & signature_F)) {
     	  v->signature = list(1,
	       cons(signature_S,cons(s,reverse(v->signature))));
	  }
     pushbackenv(v);
     popenv(&v);
     return NULL;
     }

node chkpackage(node e, env v){
     return chkpackage1(e,v,FALSE);
     }

node chksignature(node e, env v){
     return chkpackage1(e,v,TRUE);
     }

node chkbreak(node e, env v){
     node break_label;
     if (!inside_loop(v)) {
	  errorpos(e,"break should be used inside a loop");
	  return bad_K;
	  }
     break_label = getbreaklabel(v);
     if (length(e) > 1) return badnumargs(e,1);
     if (breaks_disabled(v)) {
	  errorpos(e,"break not allowed in this context");
	  return bad_K;
	  }
     breakgather(v);
     perform(list(2,goto_S,break_label),v);
     return NULL;
     }

node chknewarray(node e, env v){
     node arr, arrtype, len, elemtype, continuelabel, args, make_index;
     node argblock, olen;
     /* (make ((array typ) len i) e1 ... en) */
     /*	    	 or	     	       	     */
     /* (make ((array typ) len  ) e1 ... en) */
     /* see chkprovide(), too. */
     if (length(e) <= 2) {
	  errorpos(e,"make needs at least two arguments");
	  return bad_K;
	  }
     argblock = cadr(e);
     if (! iscons(argblock) || length(argblock) > 3 || length(argblock) == 1) {
	  errorpos(e,"\"make\" specifier should be a list of length 2 or 3");
	  return bad_K;
	  }
     arrtype = chktype(car(argblock),v);
     if (arrtype == undefined_T) return bad_K;
     len = chkint(cadr(argblock),v);
     if (len == bad_K) return bad_K;
     args = cddr(e);
     if (!isarraytypeexpr(arrtype)) {
	  errorpos(car(argblock),"should be an array type");
	  return bad_K;
	  }
     arrtype = totype(arrtype);
     if (length(argblock) >= 3) {
	  make_index = chkstring(caddr(argblock));
	  if (make_index == bad_K) return bad_K;
	  }
     else make_index = NULL;
     elemtype = elementtype(arrtype);
     pushenv(&v);
     v->make_element_type = elemtype;
     if (make_index == NULL) {
     	  v->make_index = newtmp(int_T,v,TRUE);
	  }
     else {
     	  v->make_index = newsymbol(make_index,int_T,v,
	       intern_F|readonly_F|initialized_F);
     	  v->decls = cons(list(2,define_S,v->make_index), v->decls);
	  }
     assign(v->make_index,zero,v);
     arr = v->make_array = newtmp(arrtype,v,TRUE);
     releaselater(v->make_array,v);
     continuelabel = newlabel();
     v->make_break_label = newlabel();     
     olen = len;
     len = entmp(len,v);
     len = enpos(len,pos(olen));
     v->make_array_len = len;
     perform(list(2,array_len_check_S,len),v);
     perform(list(3,getmem_S,v->make_array,len),v);
     setup(v->make_array,v);
     assign(take(v->make_array, len__S), len, v);
     perform(list(3,if_S,
	       list(4,infix_S,equal_S,len,zero), 
	       list(2,goto_S,v->make_break_label)),v);
     perform(list(2,label_S,continuelabel),v);
     v->disable_breaks = TRUE;
     chk(cons(block_K,args),v);
     push(v->before,list(2,goto_S,continuelabel));
     perform(list(2,label_S,v->make_break_label),v);
     if (!v->make_used) {
	  errorpos(e,"no values provided by body");
	  }
     unwind(&v->symbols);
     pushbackenv(v);
     popenv(&v);
     return arr;
     }

node chkprovide(node e, env v){
     node arg, argtyp, around;
     env w = v, vv;
     if (length(e) != 2) return badnumargs(e,2);
     while (w->make_array == NULL) {
	  w = w->previous;
	  if (w == NULL) {
	       errorpos(e,"provide not allowed in this context");
	       return bad_K;
	       }
	  }
     w->make_used = TRUE;
     pushenv(&v);
     arg = chk(cadr(e),v);
     argtyp = type(arg);
     if (!subtype(argtyp,w->make_element_type)) {
	  errorpos(cadr(e),"type mismatch");
	  return bad_K;
	  }
     assign(array_take(w->make_array,w->make_index),arg,v);
     performafters(v);
     pushbackenv(v);
     popenv(&v);
     around = newlabel();
     perform(list(3,if_S,
	       list(4,infix_S,lt_S,
		    list(3,prefix_S,plusplus_S,w->make_index),
		    w->make_array_len), 
	       list(2,goto_S,around)),v);
     for (vv = v; TRUE; vv = vv->previous) {
     	  performlist(vv->after,v);
     	  performlist(vv->finals,v);
	  if (vv->make_array != NULL) break;
	  }
     perform(list(2,goto_S,w->make_break_label),v);
     perform(list(2,label_S,around),v);
     return NULL;
     }

node chkdefun(node e,env v){
     node q, argtypes = NULL;
     node sym, body = NULL;
     node rettype = undefined_T;
     node template = NULL;
     node funtype;
     pushenvblock(&v);
     v->defun = TRUE;
     if (length(e) < 3 || !iscons(cdr(e)) || !iscons(cadr(e))) {
	  bad:
	  errorpos(e,"invalid defun");
	  return NULL;
	  }
     sym = caadr(e);
     for (q = cdadr(e); q != NULL; q = cdr(q)) {
	  node argsym, argtype;
	  if (!iscons(q)||!iscons(car(q))||!iscons(cdar(q))) {
	       goto bad;
	       }
	  argtype = cadar(q);
	  argtype = totype(chktype(argtype,v));
	  argsym = caar(q);
	  argsym = newsymbol(argsym,argtype,v,intern_F|initialized_F);
	  if (argsym == NULL) return bad_K;
	  push(argtypes, argtype);
	  push(template, list(2,argsym,argtype));
	  reserve(argsym,v);
	  releasefinal(argsym,v);
	  }
     template = reverse(template);
     argtypes = reverse(argtypes);
     rettype = totype(chktype(caddr(e),v));
     v->rettype = rettype;
     funtype = totype(list(3,function_S,argtypes,rettype));
     {
	  node s = lookupexactfunction(sym,argtypes);
	  if (s != NULL) {
	       if (type(s) != funtype) {
		    errorpos(e,"declaration doesn't match previous definition");
		    return NULL;
		    }
	       sym = s;
	       }
	  else {
	       sym = newsymbol(sym, funtype,v->previous,
		    intern_F|initialized_F);
	       if (sym == NULL) return bad_K;
	       }
	  }
     template = cons(sym,template);
     assert(sym->tag == symbol_tag);
     body = cdddr(e);
     if (equal(rettype,void_T)) {
	  chk(cons(block_K,body),v);
	  returngather(v);
	  }
     else {
	  node w = chk(cons(blockn_K,body),v);
	  if (reachable(v)) {
	       node tmp;
	       if (!subtype(type(w),rettype)) {
		    return typemismatch(last(e));
		    }
	       reserve(w,v);
	       tmp = newtmp(rettype,v,TRUE);
	       perform(list(3,assign_S,tmp,cast(rettype,w,v)),v);
	       tmp = enpos(tmp,pos(w));
	       w = tmp;
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
     push(global.decls, join( list(3, defun_S,template,rettype), body));
     popenv(&v);
     return NULL;
     }

node redef(node newsym,node oldsym) {
     errorpos(newsym,"symbol being redefined");
     errorpos(oldsym,"here is the previous definition");
     return bad_K;
     }

node chklhs(node e, env v){
     if (isstrpos(e)) {
	  node w = lookupword(e,v);
	  if (w == NULL) return newsymbol(e,deferred_T,v,none_F);
	  assert(issym(w));
	  if ((w->body.symbol.flags & (initialized_F|import_F))
	       &&
	       type(w) != undefined_T) {
	       return redef(e,w);
	       }
	  else return w;
	  }
     if (!iscons(e)) {
	  errorpos(e,"invalid left hand side of definition");
	  return bad_K;
	  }
     if (equal(CAR(e),export_S)) {
     	  node s;
	  if (length(e) != 2) assert(FALSE);
	  s = chklhs(CADR(e),v);
	  if (s == bad_K) return bad_K;
	  assert(issym(s));
	  if (!(s->body.symbol.flags & export_F)) {
	       s->body.symbol.flags |= export_F;
     	       if (s->body.symbol.flags & intern_F) {
		    exportit(s,v);
		    }
	       }
	  return s;
	  }
     if (equal(CAR(e),import_S)) {
     	  node s;
	  if (length(e) != 2) assert(FALSE);
	  s = chklhs(CADR(e),v);
	  if (s == bad_K) return bad_K;
	  assert(issym(s));
	  s->body.symbol.flags |= import_F;
	  if (s->body.symbol.flags & intern_F) exportit(s,v);
	  return s;
	  }
     if (equal(CAR(e),colon_S)) {
	  return chklhscolon(e,v);
	  }
     errorpos(e,"invalid left hand side of definition");
     return bad_K;
     }

node chkimport(node e, env v){
     node sym;
     if (length(e) != 2) return badnumargs(e,2);
     sym = chklhs(CADR(e),v);
     if (sym == bad_K) return bad_K;
     sym = unpos(sym);
     if (!issym(sym)) {
	  errorpos(e,"importing a nonsymbol");
	  return bad_K;
	  }
     if ((sym->body.symbol.flags & initialized_F) && type(sym) != type_T) {
	  errorpos(e,"importing a symbol already initialized");
	  return bad_K;
	  }
     if (sym->body.symbol.flags & import_F) {
	  errorpos(e,"importing a previously defined symbol");
	  }
     else {
     	  sym->body.symbol.flags |= import_F;
     	  internsymbol(sym,v);
	  }
     if (type(sym) != type_T && 
	  !(
	       isfunctiontype(type(sym)) 
	       && 
	       (sym->body.symbol.flags & constant_F)
	       )
	  ) {
	  push(v->decls,list(2,define_S,sym));
	  }
     pushsignature(e,v);
     return sym;
     }

node chkexport(node e, env v){
     node sym;
     if (length(e) != 2) return badnumargs(e,2);
     sym = chk(CADR(e),v);
     sym = unpos(sym);
     if (sym == bad_K) return bad_K;
     if (!issym(sym)) {
	  errorpos(e,"exporting a nonsymbol");
	  return bad_K;
	  }
     sym->body.symbol.flags |= export_F;
     if (sym->body.symbol.type!=keyword_T) {
	  /* sigh, this duplicates code in internsymbol */
	  if (0 == (sym->body.symbol.flags & literal_F)) {
	       char *Cname = totoken(sym->body.symbol.name->body.string.contents);
	       if (sym->body.symbol.flags & (export_F | import_F)) {
		    Cname = prefixify(sym->body.symbol.package,Cname);
		    }
	       Cname = uniquify(Cname);
	       sym->body.symbol.Cname = Cname;
	       }
	  }
     pushsignature(list(2,import_S,cadr(e)),v);
     return sym;
     }

node chkcolonequal(node e, env v){
     node lhs, rhs, ltype;
     if (length(e) != 3) return badnumargs(e,2);
     lhs = chklhs(cadr(e),v);
     if (lhs == bad_K) return bad_K;
     ltype = type(lhs);
     rhs = caddr(e);
     if (!issym(lhs)) {
	  errorpos(cadr(e),"invalid target for :=");
	  return bad_K;
	  }
     if ( ltype == undefined_T
	  && !(lhs->body.symbol.flags & initialized_F)
	  && (lhs->body.symbol.flags & intern_F)
	  ) {
     	  node rhsvalue;
	  /* filling in a forward definition of a type variable */
     	  if (lhs->body.symbol.flags & export_F) {
	       pushsignature(cadr(e),v);
	       }
	  rhsvalue = chk(rhs,v);
	  if (rhsvalue != undefined_T) {
	       node t = totype(rhsvalue);
	       assert(istype(t));
	       if (!(lhs->body.symbol.flags & initialized_F)) {
		    lhs->body.symbol.value->body.type.forward = t;
		    lhs->body.symbol.flags |= initialized_F;
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
     	  if ((lhs->body.symbol.flags & export_F) && 
	       !(lhs->body.symbol.flags & intern_F)) {
	       pushsignature(cadr(e),v);
	       }
	  if ((lhs->body.symbol.flags & initialized_F)
	       && (lhs->body.symbol.flags & intern_F)) {
	       return redef(cadr(e),lhs);
	       }
	  lhs->body.symbol.flags |= initialized_F;
	  if (!(lhs->body.symbol.flags & intern_F)) internsymbol(lhs,v);
     	  pushenvblock(&v);
	  for (t=args; t!=NULL; t=CDR(t)) {
	       node parm = CAR(t);
	       assert(issym(parm));
	       parm->body.symbol.flags |= initialized_F;
     	       internsymbol(parm,v);
	       reserve(parm,v);
	       releasefinal(parm,v);
	       parmsyms = cons(parm,parmsyms);
	       }
	  parmsyms = reverse(parmsyms);
     	  v->defun = TRUE;
     	  v->rettype = rettype;
     	  if (rettype==void_T) {
	       chk(list(2,block_K,rhs),v);
	       returngather(v);
	       }
	  else {
	       node w = chk(list(2,blockn_K,rhs),v);
	       if (w == bad_K) return bad_K;
	       if (reachable(v)) {
		    node tmp;
		    if (!subtype(type(w),rettype)) {
			 errorpopenv(&v);
			 return typemismatch(last(e));
			 }
		    reserve(w,v);
		    tmp = newtmp(rettype,v,TRUE);
		    perform(list(3,assign_S,tmp,cast(rettype,w,v)),v);
		    tmp = enpos(tmp,pos(w));
		    w = tmp;
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
	  popenv(&v);
	  push(global.decls, 
	       join( 
		    list(3, defun_S,cons(lhs,parmsyms),rettype), 
		    body));
	  return lhs;
	  }
     else {
	  node rhsvalue, rtype;
     	  if (lhs->body.symbol.flags & initialized_F) {
	       return redef(cadr(e),lhs);
	       }
	  else {
	       lhs->body.symbol.flags |= initialized_F;
	       }
	  if (lhs->body.symbol.flags & intern_F) {
	       assertpos(ltype == deferred_T,cadr(e));
	       }
	  else {
     	       internsymbol(lhs,v);
	       }
	  rhsvalue = chk(rhs,v);
	  rtype = type(rhsvalue);
	  if (ltype == deferred_T) {
	       lhs->body.symbol.type = ltype = rtype;
	       }
	  else {
	       if (!subtype(rtype,ltype)) {
		    errorpos(rhs, "type mismatch");
		    return bad_K;
		    }
	       }
	  if (ltype == type_T) {
	       node t = totype(rhsvalue);
	       if (t->body.type.name == NULL) t->body.type.name = lhs;
	       if (lhs->body.symbol.value != NULL) {
		    forwardtype(lhs->body.symbol.value,t);
		    }
	       else {
		    lhs->body.symbol.value = t;
		    }
     	       if (lhs->body.symbol.flags & export_F) {
	       	    pushsignature(e,v);
		    }
	       }
	  else {
	       node package = NULL;
	       if (v != NULL && v->previous != NULL) {
		    package = v->previous->current_package;
		    }
	       if (package != NULL) {
		    push(global.decls, list(2,define_S,lhs));
		    assign(lhs,rhsvalue,v);
		    releaselater(lhs,v);
		    }
	       else {
		    push(v->decls, list(2,define_S,lhs));
		    assign(lhs,rhsvalue,v);
		    releasefinal(lhs,v);
		    }
	       if (lhs->body.symbol.flags & export_F) {
		    pushsignature(
			 list(2,import_S,list(3,colon_S,lhs,type(lhs))),v);
		    }
	       }
	  return lhs;
	  }
     }

node chknew(node e, env v, node typ){
     node args, newargs=NULL;
     node objtype, argtypes = NULL;
     node membertypes, tmpsymb;
     bool badarg = FALSE;
     int  i, j, nargs, nmembers;
     objtype = totype(typ);
     if (objtype == undefined_T) return bad_K;
     if (isbasictype(objtype)) {
	  node arg, t;
	  if (objtype == null_T) {
	       if (length(e) != 1) return badnumargs(e,0);
	       return cast(null_T,integer(0),v);
	       }
	  if (length(e) != 2) return badnumargs(e,1);
	  arg = cadr(e);
     	  arg = chk(arg,v);
	  t = type(arg);
	  if (t == undefined_T) return bad_K;
	  if (isarithmetictype(t) && isarithmetictype(objtype)) {
	       return setpos(list(2,objtype,arg),pos2(e));
	       }
	  errorpos(e,"impossible type conversion");
	  return bad_K;
	  }
     if (isarraytype(objtype)) {
	  node m = typedeftail(objtype);
	  node tp = car(m);
	  node len = length(m)==2 ? cadr(m) : NULL;
     	  pushenv(&v);
	  args = cdr(e);
	  nargs = length(args);
	  assert(len == NULL || len->tag == int_const_tag);
	  if (len != NULL && atoi(len->body.int_const.contents) != nargs) {
	       errorpos(e,"wrong number of initial values");
	       return bad_K;
	       }
	  for (i=1; i<=nargs; i++) {
	       node newarg = chk(nth(args,i),v);
	       if (newarg == bad_K) return bad_K;
	       if (!subtype(type(newarg),tp)) {
		    errorpos(nth(args,i),"type mismatch");
		    return bad_K;
		    }
	       newargs = cons(newarg,newargs);
	       argtypes = cons(type(newarg),argtypes);
	       }
	  newargs = reverse(newargs);
     	  tmpsymb = newtmp(objtype,v,TRUE);
	  perform(list(3,getmem_S,tmpsymb,integer(nargs)),v);
	  setup(tmpsymb,v);
     	  if (len == NULL) {
	       assign(take(tmpsymb, len__S), integer(nargs), v);
	       }
	  for (i=1; i<=nargs; i++) {
	       assign(array_take(tmpsymb, integer(i-1)), nth(newargs,i), v);
	       }
	  releaselater(tmpsymb,v);
	  pushbackenv(v);
	  performlist(v->after,v->previous);
	  v->after = NULL;
	  popenv(&v);
	  return tmpsymb;
	  }
     if (isortype(objtype)) {
	  node arg;
	  if (length(e) != 2) {
	       errorpos(e,"type conversion requires one argument");
	       return bad_K;
	       }
	  arg = chk(cadr(e),v);
	  if (arg == bad_K) return bad_K;
	  if (!subtype(type(arg),objtype)) {
	       errorpos(e,"type of argument not among typedeftail of or-type");
	       return bad_K;
	       }
	  return cast(objtype,arg,v);
	  }
     if (!isobjecttype(objtype)) {
	  errorpos(car(e),"invalid type");
	  return bad_K;
	  }
     pushenv(&v);
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
     if (member(bad_K,newargs)) return bad_K;
     nmembers = length(membertypes);
     for (i=j=1; i<=nargs && j<=nmembers; ) {
	  if (equal(cadr(nth(membertypes,j)),void_T)) {
	       j++;
	       continue;
	       }
	  if (!subtype(nth(argtypes,i), 
		    cadr(nth(membertypes,j)))) {
	       badarg = TRUE;
	       if (nth(argtypes,i) == deferred_T) {
		    errorpos(nth(args,i),"undefined");
		    }
	       else {
	       	    errorpos(nth(args,i),"wrong type");
		    }
	       }
	  i++, j++;
	  }
     if (i <= nargs) {
	  errorpos(e,"too many arguments");
	  return bad_K;
	  }
     while (j<=nmembers && equal(cadr(nth(membertypes,j)),void_T)) j++;
     if (j <= nmembers) {
	  errorpos(e,"too few arguments");
	  return bad_K;
	  }
     if (badarg) return bad_K;
     perform(list(2,getmem_S,tmpsymb),v);
     setup(tmpsymb,v);
     releaselater(tmpsymb,v);
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
	  if (r == tmpsymb) perform( list(3, assign_S, l, r), v);
	  else 
#endif
	  assign(l,r,v);
	  }
     pushbackenv(v);
     performlist(v->after,v->previous);
     v->after = NULL;
     popenv(&v);
     return tmpsymb;
     }

node chkarray(node e, env v){
     node len = NULL;
     node typ;
     if (length(e) > 3 || length(e) == 1) {
	  errorpos(e,"array takes a type and an optional length");
	  return bad_K;
	  }
     if (length(e) == 3) {
	  len = unpos(caddr(e));
	  if (len->tag != int_const_tag) {
	       errorpos(caddr(e),"array length should be an integer");
	       return bad_K;
	       }
	  if (len->body.int_const.contents[0]=='-') {
	       errorpos(caddr(e),"array length should be nonnegative");
	       return bad_K;
	       }
	  }
     typ = chktype(cadr(e),v);
     return len==NULL ? list(2,array_K,typ) : list(3,array_K,typ,len);
     }

node chkobject(node e, env v){
     node ee = cons(object_K,NULL);
     e = cdr(e);
     while (e != NULL) {
	  node pair = car(e);
	  node var, vtype;
	  if (!iscons(pair) || 
	       length(pair) != 3 || 
	       !equal(colon_S,car(pair))) {
	       errorpos(pair,"invalid type");
	       return bad_K;
	       }
	  var = cadr(pair);
	  vtype = caddr(pair);
	  if (!isword(var)) {
	       errorpos(var,"part name should be a word");
	       var = bad_K;
	       vtype = undefined_T;
	       }
	  else {
	       vtype = chktype(vtype,v);
	       }
	  ee = cons(list(2,var,vtype),ee);
	  e = cdr(e);
	  }
     return reverse(ee);
     }

node chkfunction(node e, env v){
     node args = cadr(e);
     node result = caddr(e);
     return list(3, function_S, chktypelist(args,v), chktype(result,v));
     }

node chkfunctiontype(node e, env v, node fun){
     node args = cdr(e);
     node argtypes = NULL, funargtypes = NULL;
     int  nargs = length(args);
     node newargs=NULL, newexpr=NULL;
     node funrettype = NULL, funtype = type(fun);
     int  i;
     pushenv(&v);
     for (i=1; i<=nargs; i++) {
	  node newarg = chk(nth(args,i),v);
	  newargs = cons(newarg,newargs);
	  assert(type(newarg) != NULL);
	  argtypes = cons(type(newarg),argtypes);
	  }
     newargs = reverse(newargs);
     argtypes = reverse(argtypes);
     if (member(bad_K,newargs) || member(undefined_T,argtypes)) return bad_K;
     if (member(deferred_T,argtypes)) {
	  int j = memberindex(deferred_T,argtypes);
	  errorpos(nth(args,j),"argument not defined yet");
	  return bad_K;
	  }
     fun = lookupfunction(fun,argtypes);
     if (fun == NULL) {
	  errorpos(e, "unsuitable arguments");
	  return bad_K;
	  }
     funtype = type(fun);
     funargtypes = functionargtypes(funtype);
     funrettype = functionrettype(funtype);
     if (!equal(funargtypes,argtypes)) {
	  node a = NULL;
	  for (i=1; i<=nargs; i++) {
	       a = cons( cast( nth(funargtypes,i),nth(newargs,i),v), a);
	       }
	  newargs = reverse(a);
	  }
     if (issym(fun) && (fun->body.symbol.flags & macro_F)) {
	  node a = NULL;
	  while (newargs != NULL) {
	       a = cons(list(2,chked_K,CAR(newargs)),a);
	       newargs = CDR(newargs);
	       }
	  newargs = reverse(a);
	  newexpr = substitute(fun->body.symbol.args,newargs,
	       fun->body.symbol.body);
	  newexpr = chk(newexpr,v);
	  }
     else {
     	  newexpr = cons( funcall_S, cons(fun,newargs));
	  }
     if (reservable(funrettype)) {
	  node tmpsymb = newtmp(funrettype,v->previous,TRUE);
	  perform(list(3,assign_S,tmpsymb,newexpr),v);
	  tmpsymb = enpos(tmpsymb,pos(e));
	  newexpr = tmpsymb;
	  releaselater(tmpsymb,v);
	  }
     pushbackenv(v);
     v->previous->after = join(v->previous->after,v->after);
     v->after = NULL;
     popenv(&v);
     return newexpr;
     }

node chkchked(node e, env v){
     return cadr(e);
     }

bool islvalue(node s){
     s = unpos(s);
     return (
	  (issym(s) && !(s->body.symbol.flags & tmp_F))
	  || 
	  (iscons(s) && (
		    equal(car(s),take_S) 
		    || 
		    equal(car(s),part_S) 
		    || 
		    equal(car(s),array_take_S)
		    )
	       )
	  );
     }

node chksetd(node e, env v){
     node sym,t, val,typ;
     if (length(e) != 3) {
	  errorpos(e,"set takes three arguments");
	  return NULL;
	  }
     sym = chk(cadr(e),v);
     val = chk(caddr(e),v);
     if (!islvalue(sym)) {
	  errorpos(cadr(e),"can't be set");
	  return NULL;
	  }
     typ = type(val);
     if (typ == deferred_T) {
	  errorpos(val,"not defined");
	  return NULL;
	  }
     t = type(sym);
     if (!subtype(typ,t)) {
	  errorpos(e,"type mismatch");
	  return NULL;
	  }
     if (occursin(sym,val)) {
	  if (iscons(unpos(val))
	       && equal(take_S,car(unpos(val))) 
	       && equal(sym,cadr(unpos(val)))) {
	       val = entmp(val,v);
	       reserve(val,v);
     	       releasenow(sym,v);
     	       assign(sym,val,v);
     	       releasenow(val,v);
	       }
	  else {
	       val = entmp(val,v);
     	       releasenow(sym,v);
     	       assign(sym,val,v);
	       }
	  }
     else {
	  releasenow(sym,v);
     	  assign(sym,val,v);
	  }
     return NULL;
     }

node chkkindof(node e, env v) {
  errorpos(e,"kindof: not implemented");
  return bad_K;
}

node chkor(node e,env v){
     node types = chktypelist(cdr(e),v);
     node t;
     for (t=types; t!=NULL; t=cdr(t)) {
	  node tt = car(t);
	  node utt = unpos(tt);
	  if (utt == undefined_T) return bad_K;
	  if (equal(tt,null_K)) continue;
	  if (equal(tt,pointer_K)) continue;
	  if (isobjecttypeexpr(tt)) continue;
	  if (isarraytypeexpr(tt)) continue;
	  if (tt->body.type.flags & deferred_F) continue;
	  return bad_K;
	  }
     return cons(or_K,types);
     }

node chklength(node e,env v){
     node s,t;
     if (length(e) != 2) {
	  errorpos(e,"length takes one argument");
	  return bad_K;
	  }
     s = chk(cadr(e),v);
     t = type(s);
     if (!isarraytype(t)) {
	  errorpos(e,"length's argument should be an array");
	  return bad_K;
	  }
     return arraylength(s);
     }

int truestrlen(char *s){
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

node chkstringconst(node e, env v) {
     node f = ispos(e) ? e->body.position.contents : e;
     int i;
     node len;
     node tmp = newsymbol(str_S,totype(list(2,array_K,char_K)),NULL,
	  intern_F|initialized_F|literal_F);
     push(global.decls,list(2,define_S,tmp));
     assert(f->tag == string_const_tag);
     i = truestrlen(f->body.string_const.contents);
     len = integer(i);
     perform(list(3, getmem_S,tmp,len),v);
     setup(tmp,v);
     assign(take(tmp, len__S), len, v);
     perform(list(5,funcall_S,memcpy_S, take(tmp, array__S), e, len),v);
     releasefinal(tmp,v);
     return tmp;
     }

node comma2(node, node);

node comma1(node e, node fun){
     if (equal(CAR(e),comma_S)) {
	  return cons(cons(fun,comma2(CDR(e),fun)),NULL);
	  }
     else return cons(CAR(e),comma1(CDR(e),fun));
     }

node comma2(node e, node fun){
     if (!member(comma_S,e)) return e;
     return comma1(e,fun);
     }

node commaexpand(node e){
     if (!member(comma_S,e)) return e;
     return comma1(e,CAR(e));
     }

node chklhscolon(node e, env v){
     node x,t;
     if (length(e)!=3) return badnumargs(e,2);
     x = cadr(e);
     t = totype(chktype2(caddr(e),v));
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
	       return bad_K;
	       }
	  for (;args != NULL; args=cdr(args)) {
	       node arg = chklhs(car(args),v);
	       if (arg == bad_K) {
		    bad = TRUE;
		    continue;
		    }
	       if (arg->body.symbol.type == undefined_T) {
#if 0
		    errorpos(car(args),"does not have a type");
#endif
		    bad = TRUE;
		    continue;
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
		    return bad_K;
		    }
	       if (s->body.symbol.flags & (initialized_F|import_F)) {
		    errorpos(f,"redeclaration");
     	       	    errorpos(s,"here is the previous definition");
		    }
	       else {
		    /* warningpos(f,"completion of previous partial definition"); */
		    v->partial_definitions_active--;
	            }
	       }
	  else {
	       s = newsymbol(f,funtype,v,/* intern_F| ??? */ constant_F);
	       }
	  s->body.symbol.args = newargs;
	  if (bad) return bad_K;
	  return s;
	  }
     }

node chkcolon(node e, env v){
     node s = chklhscolon(e,v);
     node sym = unpos(s);
     node t = type(sym);
     if (sym == bad_K) return bad_K;
     assert(issym(sym));
     if (t == type_T
	  && istype(sym->body.symbol.value)
	  && NULL == sym->body.symbol.value->body.type.definition
	  && NULL == sym->body.symbol.value->body.type.forward) {
     	  errorpos(e,"obsolete type declaration");
	  return NULL;
	  }
     if (isfunctiontype(t)) {
          /* warningpos(e,"partial definition"); */
	  v->partial_definitions_active++;
	  v->previous_partial_definition = e;
	  internsymbol(sym,v);	/* partial definition */
	  return s;
	  }
     errorpos(s,"illegal partial definition");
     return bad_K;
     }

node chkcoloncolonequal(node e, env v){
     /* macros! */
     node f, fsym;
     if (length(e) != 3) return badnumargs(e,2);
     f = cadr(e);
     if (!( iscons(f) && length(f)==3 && equal(car(f),colon_S)
	       && iscons(cadr(f)) && isstrpos(caadr(f)))) {
	  errorpos(e,"invalid macro definition");
	  return bad_K;
	  }
     fsym = chklhs(f,v);
     if (!issym(fsym)) {
	  errorpos(e,"invalid macro definition");
	  return bad_K;
	  }
     fsym->body.symbol.flags |= macro_F;
     fsym->body.symbol.body = caddr(e);
     fsym->body.symbol.Cname = NULL;
     fsym->body.symbol.flags |= initialized_F;
     {
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
     return NULL;
     }

node chkdot(node e, env v){
     node thing, thingtype, x, p;
     if (length(e)!=3) return badnumargs(e,2);
     x = cadr(e);
     p = caddr(e);
     thing = chk(x,v);
     thingtype = type(thing);
     if (isarraytype(thingtype)) {
	  node indx = chk(p,v);
	  node t = type(indx);
	  if (t != uint_T) {
	       if (t->body.type.integer_type) {
		    indx = cast(uint_T,indx,v);
		    }
	       else {
		    errorpos(p,"array access needs an integer argument");
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
     if (thingtype == undefined_T) return bad_K;
     if (isortype(thingtype)) {
	  if (NULL==ormembertype(thingtype,p)) {
	       errorpos(p,"not a member");
	       return bad_K;
	       }
	  return setpos(
	       take(
		    cast(CAR(typedeftail(thingtype)),thing,v),
		    p),
	       pos2(e));
	  }
     if (isobjecttype(thingtype)) {
	  if (NULL==membertype(thingtype,p)) {
	       errorpos(p,"not a member");
	       return bad_K;
	       }
	  return setpos(take(thing,p),pos2(e));
	  }
     if (thingtype == deferred_T) {
	  errorpos(x,"undefined");
	  }
     else {
     	  errorpos(x,"should be an object, array, or union");
	  }
     return bad_K;
     }

void perform_cleanup(node s,env v){
     push(package_env(v)->finals,s);
     }

node chk(node e, env v){
     node f = e;
     assert(e != NULL);
     if (ispos(f)) f = f->body.position.contents;
     assert (f != NULL);
     switch(f->tag){
	  case type_tag: return e;
     	  case string_tag: {
	       node sym = lookupword(e,v);
	       if (sym == NULL) {
     	       	    /* !!! errorpos(e,"undefined symbol"); */
	       	    sym = newsymbol(e,deferred_T,v,intern_F);
		    }
	       return repos(e,sym);
	       }
     	  case string_const_tag: {
	       env w = package_env(v);
	       if (w == NULL) {
		    errorpos(e,"outside of a package");
		    return bad_K;
		    }
	       return chkstringconst(e,w);
	       }
	  case symbol_tag: return e;
	  case double_const_tag: return e;
	  case char_const_tag: return e;
	  case int_const_tag: return e;
	  case position_tag: assert(FALSE);
	  case cons_tag: {
	       node fun = chk(car(e),v);
	       node funtype;
	       node (*cf)(node,env) = getchkfun(fun);
     	       if (cf != NULL) return cf(e,v);
	       funtype = type(fun);
	       if (funtype == undefined_T) return bad_K;
	       if (equal(fun,block_K)) return chkblock(cdr(e),v);
	       if (equal(fun,until_K)) return chkuntil(e,v,TRUE);
	       if (equal(fun,while_K)) return chkuntil(e,v,FALSE);
	       if (equal(fun,define_recursive_types_K)) {
		    return chkrecurs(cdr(e),v);
		    }
	       else if (type_T==funtype) {
		    return chknew(commaexpand(e),v,fun);
		    }
     	       else if (isfunctiontype(funtype)) {
		    return chkfunctiontype(commaexpand(e),v,fun);
		    }
	       else {
	       	    errorpos(car(e),"not a function or type");
		    return bad_K;
		    }
	       }
	  }
     assert(FALSE);
     return NULL;
     }

node chkuse(node e, env v){
     node p, syms, name;
     node namep = CADR(e);
     if (length(e) != 2) return badnumargs(e,2);
     assert(ispos(namep));
     name = unpos(namep);
     assert(isstr(name));
     p = lookupword(name,v);
     if (p == NULL) {
	  char *pathopened = NULL;
	  if (!sigreadfile(name->body.string.contents,&pathopened)) {
	       errorpos(CADR(e), "undefined package");
	       return bad_K;
	       }
	  assert(pathopened != NULL);
	  if (dependfile != NULL)
	  fprintf(dependfile,"%s %s %s : %s\n",
	       newsuffixbase(targetname,".oo"),
	       newsuffixbase(targetname,".loo"),
	       newsuffixbase(targetname,".sig"), 
	       newsuffixbase(pathopened,".sig")
	       );
	  p = lookupword(name,v);
     	  if (p == NULL) {
	       errorpos(CADR(e), "signature file read, but package remains undefined");
	       return bad_K;
	       }
	  }
     if (type(p) != package_T) {
	  errorpos(p,"not a package");
	  return bad_K;
	  }
     p = unpos(p);
     assert(issym(p));
     for (syms = p->body.symbol.export_list; syms != NULL; syms = CDR(syms)) {
	  node sym = CAR(syms);
	  assert(issym(sym));
	  reinternsymbol(sym,v);
	  }
     if (!withinsignature(v)) {
     	  perform(list(4,Ccode_S,void_T,
		    String(prefixify(p,"_prepare")),String("()")),v);
	  }
     pushsignature(e,v);
     return NULL;
     }

node chkprogram(node e){
     node f = chk(e, &global);
     assert(global.tmpdecls == NULL);
     assert(global.finals == NULL);
     f = join(reverse(global.decls),f);
     return f;
     }

void setchkfun(node e,chkfun f){
     assert(issym(e));
     e->body.symbol.chk = f;
     }

void init_chk(){
     setchkfun(return_K,chkreturn);
     setchkfun(break_K,chkbreak);
     setchkfun(new_K,chknewarray);
     setchkfun(use_K,chkuse);
     setchkfun(package_K,chkpackage);
     setchkfun(signature_K,chksignature);
     setchkfun(provide_K,chkprovide);
     setchkfun(blockn_K,chkblockn);
     setchkfun(foreach_K,chkforeach);
     setchkfun(andand_K,chkandand);
     setchkfun(oror_K,chkoror);
     setchkfun(sizeof_K,chksizeof);
     setchkfun(for_K,chkfor);
     setchkfun(setd_K,chksetd);
     setchkfun(if_K,chkif);
     setchkfun(when_K,chktypecase);
     setchkfun(block1_K,chkblock1);
     setchkfun(or_K,chkor);
     setchkfun(array_K,chkarray);
     setchkfun(length_K,chklength);
     setchkfun(object_K,chkobject);
     setchkfun(function_K,chkfunction);
     setchkfun(equal_K,chkequal);
     setchkfun(unequal_K,chkunequal);
     setchkfun(Ccode_K,chkCcode);
     setchkfun(dot_K,chkdot);
     setchkfun(colon_K,chkcolon);
     setchkfun(colonequal_K,chkcolonequal);
     setchkfun(coloncolonequal_K,chkcoloncolonequal);
     setchkfun(chked_K,chkchked);
     setchkfun(export_K,chkexport);
     setchkfun(import_K,chkimport);
     setchkfun(kindof_K,chkkindof);
     }

/*
# Local Variables:
# compile-command: "make -C $M2BUILDDIR/Macaulay2/c "
# End:
*/
