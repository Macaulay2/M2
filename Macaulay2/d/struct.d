--		Copyright 1994 by Daniel R. Grayson
use system;
use stdio;
use stdiop;
use binding;
use system;
use strings;
use nets;
use tokens;
use err;
use stdio;
use arith;
use basic;
use convertr;

export copy(e:Expr):Expr := (
     when e
     is a:List do if a.mutable then Expr(copy(a)) else e
     is o:HashTable do if o.mutable then Expr(copy(o)) else e
     else e);
setupfun("copy",copy);
reverse(e:Expr):Expr := (
     when e
     is a:Sequence do Expr(reverse(a))
     is a:List do Expr(reverse(a))
     else WrongArg("a list or sequence"));
setupfun("reverse",reverse);
export seq(e:Expr):Expr := Expr(Sequence(e));
-- setupfun("singleton",seq);
pairs(e:Expr):Expr := (
     when e
     is o:HashTable do list(
	  new Sequence len o.numEntries do
	  foreach bucket in o.table do (
	       p := bucket;
	       while p != bucketEnd do (
		    provide Expr(Sequence(p.key,p.value));
		    p = p.next;
		    )
	       )
	  )
     else WrongArg("a hash table"));
setupfun("pairs",pairs);
export splice(a:Sequence):Sequence := (
     -- warning - this function may return its argument without copying
     hadseq := false;
     newlen := length(a);
     if newlen == 0 then return(a);
     if newlen == 1 then (
	  when a.0
	  is s:Sequence do return(s)
	  else return(a); );
     foreach i in a do (
	  when i is ii:Sequence do (
	       hadseq = true; 
	       newlen = newlen + length(ii) - 1; )
     	  else nothing;
	  );
     if hadseq
     then new Sequence len newlen do
     foreach i in a do 
     when i is ii:Sequence 
     do foreach j in ii do provide j
     else provide i
     else a);
export splice(e:Expr):Expr := (
     when e
     is v:Sequence do Expr(splice(v))
     is a:List do list(
	  a.class,
	  if a.mutable then (
	       r := splice(a.v);
	       if r == a.v then copy(r) else r
	       )
	  else splice(a.v),
	  a.mutable)
     else e);
setupfun("splice",splice);
export accumulate(
     f0:function():Expr, f1:function(Expr):Expr,
     f2:function(Expr,Expr):Expr, e:Expr):Expr := (
     when e
     is a:Sequence do (
	  if length(a) == 0 then f0()
	  else if length(a) == 1 then f1(a.0)
	  else (
	       g := a.0;
	       for i from 1 to length(a)-1 do (
		    g = f2(g,a.i);
		    when g is Error do return(g) else nothing;
		    );
	       g))
     else f1(e));

export map(f:function(Expr):Expr,a:Sequence):Sequence := (
     new Sequence len length(a) do foreach x in a do provide f(x));
export join(v:Sequence,w:Sequence):Sequence := (
     new Sequence len length(v) + length(w) do (
	  foreach x in v do provide x;
	  foreach y in w do provide y));
export subarray(v:Sequence,start:int,leng:int):Sequence := (
     new Sequence len leng at i do provide v.(start+i));
export subarray(v:Sequence,leng:int):Sequence := subarray(v,0,leng);

export isInteger(e:Expr):bool := when e is Integer do true else false;
export isInt(e:Expr):bool := when e is i:Integer do isInt(i) else false;
export isIntArray(e:Sequence):bool := (
     foreach x in e do if !isInt(x) then return(false);
     true);
export isIntArray(e:Expr):bool := (
     when e
     is a:Sequence do isIntArray(a)
     is b:List do isIntArray(b.v)
     else false);     
export toInt(e:Expr):int := (
     when e 
     is i:Integer do toInt(i)
     else (fatal("internal error"); 0));
export toIntArray(e:Sequence):array(int) := (
     new array(int) len length(e) do foreach x in e do provide toInt(x));
export toIntArray(e:Expr):array(int) := (
     when e
     is a:Sequence do toIntArray(a)
     is b:List do toIntArray(b.v)
     else (
	  fatal("internal error: toIntArray expected an array of ints");
	  array(int)()
	  )
     );
export toArrayExpr(v:array(int)):Sequence := (
     new Sequence len length(v) do foreach i in v do provide Expr(toInteger(i))
     );

export newlist(class:HashTable,v:Sequence):List := (
     x := List(class,v,0,false);
     x.hash = hash(x);
     x);
export basictype(o:HashTable):HashTable := (
     while true do (
	  if o.parent == thingClass then return(o);
	  o = o.parent;
	  ));

export RecursionLimit():Expr := (
     buildErrorPacket("recursion limit of " + tostring(recursionlimit) + " exceeded"));
setrecursionlimit(e:Expr):Expr := (
     when e
     is i:Integer do (
	  if isInt(i)
	  then (
	       old := toInteger(recursionlimit);
	       recursionlimit = toInt(i); 
	       Expr(old))
	  else buildErrorPacket("'setrecursionlimit' expected a small integer")
	  )
     else buildErrorPacket("'setrecursionlimit' expected a small integer"));
setupfun("setrecursionlimit",setrecursionlimit);
errordepthfun(e:Expr):Expr := (
     when e
     is i:Integer do (
	  if isInt(i) then (
	       j := ErrorDepth;
	       ErrorDepth = toInt(i);
	       Expr(toInteger(j)))
	  else WrongArgSmallInteger())
     else WrongArgInteger());
setupfun("errorDepth",errordepthfun);
export backtr(z:Expr):Expr := (
     when z is err:Error do 
     if err.position == dummyPosition 
     || int(err.position.reloaded) < ErrorDepth
     || SuppressErrors
     then z
     else buildErrorPacket("--backtrace--",err.report)
     else z);
export backtr(z:Expr,report:Expr):Expr := (
     when z is err:Error do (
	  err.report = Expr(Sequence(report,err.report));
	  backtr(z))
     else z);
export backtrFunction(z:Expr):Expr := (
     when z is err:Error do (
	  if err.message == returnMessage 
	  then err.value
	  else backtr(z))
     else z);
export backtrFunction(z:Expr,report:Expr):Expr := (
     when z is err:Error do (
	  if err.message == returnMessage 
	  then err.value
	  else backtr(z,report))
     else z);
export backtrLoop(z:Expr):Expr := (
     when z is err:Error do (
	  if err.message == breakMessage then err.value
	  else backtr(z))
     else z);
export backtrLoop(z:Expr,report:Expr):Expr := (
     when z is err:Error do (
	  if err.message == breakMessage then err.value
	  else backtr(z,report))
     else z);
export WrongNumArgs(c:Code,wanted:int,got:int):Expr := (
     printErrorMessage(c, "expected " + tostring(wanted) + " argument"
	  + (if wanted == 1 then "" else "s") + ", but got "
	  + tostring(got)));
stash := new array(Frame) len 20 do provide dummyFrame;
report(c:FunctionClosure,v:Sequence):Expr := list(Expr(c),Expr(v));
export apply(c:FunctionClosure,v:Sequence):Expr := (
     previousFrame := c.frame;
     model := c.model;
     desc := model.desc;
     framesize := desc.framesize;
     if recursiondepth > recursionlimit then RecursionLimit()
     else if desc.restargs then (
	  -- assume numparms == 1
     	  if (!desc.hasClosure) && framesize < length(stash) then (
	       f := stash.framesize;
	       previousStashedFrame := f.next;
	       if f == previousStashedFrame then (
		    f = Frame(previousFrame,desc.scopenum,
		    	 new Sequence len framesize do (
			      provide v;
			      while true do provide nullE)
			 );
		    )
	       else (
		    stash.framesize = previousStashedFrame;
		    f.next = previousFrame;
		    f.scopenum = desc.scopenum;
		    f.values.0 = v;
		    );
	       recursiondepth = recursiondepth + 1;
	       saveLocalFrame := localFrame;
	       localFrame = f;
	       ret := eval(model.body);
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       foreach x in f.values do x = nullE;
	       f.next = stash.framesize;
	       stash.framesize = f;
	       when ret is err:Error do backtrFunction(ret,report(c,v)) else ret
	       )
	  else (
	       recursiondepth = recursiondepth + 1;
	       f := Frame(previousFrame,desc.scopenum,
		    new Sequence len framesize do (
			 provide v;
			 while true do provide nullE));
	       saveLocalFrame := localFrame;
	       localFrame = f;
	       ret := eval(model.body);
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       when ret is err:Error do backtrFunction(ret,report(c,v)) else ret
	       )
	  )
     else if desc.numparms != length(v)
     then backtr(WrongNumArgs(model.parms,desc.numparms,length(v)),report(c,v))
     else (
	  if framesize == 0 then (
	       recursiondepth = recursiondepth + 1;
	       saveLocalFrame := localFrame;
	       localFrame = previousFrame;
	       ret := eval(model.body);
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       when ret is err:Error do backtrFunction(ret,report(c,v)) else ret
	       )
	  else (
     	       if (!desc.hasClosure) && framesize < length(stash) then (
		    f := stash.framesize;
		    previousStashedFrame := f.next;
		    if f == previousStashedFrame then (
			 f = Frame(previousFrame,desc.scopenum,
			      new Sequence len framesize do (
			      	   foreach x in v do provide x;
			      	   while true do provide nullE)
			      );
			 )
		    else (
			 stash.framesize = previousStashedFrame;
			 f.next = previousFrame;
			 f.scopenum = desc.scopenum;
			 foreach x at i in v do f.values.i = x;
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    foreach x in f.values do x = nullE;
		    f.next = stash.framesize;
		    stash.framesize = f;
		    when ret is err:Error do backtrFunction(ret,report(c,v)) else ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    f := Frame(previousFrame,desc.scopenum,
			 new Sequence len framesize do (
			      foreach x in v do provide x;
			      while true do provide nullE));
		    localFrame = f;
		    ret := eval(model.body);
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    when ret is err:Error do backtrFunction(ret,report(c,v)) else ret
		    )
	       )
	  )
     );
report(c:FunctionClosure,e:Expr):Expr := list(Expr(c),e);
export apply(c:FunctionClosure,e:Expr):Expr := (
     -- single argument 'e' provided not a sequence, so framesize > 0
     previousFrame := c.frame;
     model := c.model;
     desc := model.desc;
     framesize := desc.framesize;
     if desc.numparms != 1
     then return(
	  backtr(
	       printErrorMessage(model.parms, "expected " +tostring(desc.numparms)
	       	    +" argument"
		    +(if desc.numparms == 1 then "" else "s")
		    +" but got 1"
	       	    ),
	       report(c,e)));
     if recursiondepth > recursionlimit then return(RecursionLimit());
     recursiondepth = recursiondepth + 1;
     if (!desc.hasClosure) && framesize < length(stash) then (
	  f := stash.framesize;
	  previousStashedFrame := f.next;
	  if f == previousStashedFrame then (
	       f = Frame(previousFrame,desc.scopenum,
		    new Sequence len framesize do (
			 provide e;
			 while true do provide nullE));
	       )
	  else (
	       stash.framesize = previousStashedFrame;
	       f.next = previousFrame;
	       f.scopenum = desc.scopenum;
	       f.values.0 = e;
	       );
     	  saveLocalFrame := localFrame;
	  localFrame = f;
	  ret := eval(model.body);
	  localFrame = saveLocalFrame;
	  foreach x in f.values do x = nullE;
	  f.next = stash.framesize;
	  stash.framesize = f;
	  recursiondepth = recursiondepth - 1;
	  when ret is err:Error do backtrFunction(ret,report(c,e)) else ret
	  	     -- this check takes time, too!
	  )
     else (
	  f := Frame(previousFrame,desc.scopenum,
	       new Sequence len framesize do (
		    provide e;
		    while true do provide nullE));
     	  saveLocalFrame := localFrame;
     	  localFrame = f;
	  ret := eval(model.body);
	  localFrame = saveLocalFrame;
	  recursiondepth = recursiondepth - 1;
	  when ret is err:Error do backtrFunction(ret,report(c,e)) else ret
	      -- this check takes time, too!
	  )
     );

errorreturn := nullE;

report(c:FunctionClosure,n:int):Expr := (
     list(Expr(c),
	  Expr(new Sequence len n 
	       do for i from 0 to n-1 do provide localFrame.values.i
	       )));
export apply(c:FunctionClosure,cs:CodeSequence):Expr := (
     -- in this version we try to avoid allocating an array of exprs to
     -- hold the parameters, preferring to stuff them into the frame
     -- directly
     model := c.model;
     desc := model.desc;
     if desc.restargs
     then (
	  e := evalSequence(cs);
	  when e is Error do backtr(e)
	  is v:Sequence do apply(c,v)
	  else nullE			  -- will not happen
	  )
     else if desc.numparms != length(cs)
     then backtr(WrongNumArgs(model.parms,desc.numparms,length(cs)),
	  list(Sequence(Expr(c))))
     else if recursiondepth > recursionlimit then RecursionLimit()
     else (
     	  previousFrame := c.frame;
     	  framesize := desc.framesize;
	  if framesize == 0 then (
	       recursiondepth = recursiondepth + 1;
	       saveLocalFrame := localFrame;
	       localFrame = previousFrame;
	       ret := eval(model.body);
	       when ret is err:Error do ret = backtrFunction(ret,report(c,emptySequence))
	       else nothing;
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       ret
	       )
	  else (
     	       if (!desc.hasClosure) && framesize < length(stash) then (
		    f := stash.framesize;
		    previousStashedFrame := f.next;
		    if f == previousStashedFrame then (
		    	 haderror := false;
			 f = Frame(previousFrame,desc.scopenum,
			      new Sequence len framesize do (
				   foreach code in cs do (
					codevalue := eval(code);
					when codevalue
					is Error do (
					     haderror = true;
					     errorreturn = codevalue;
					     while true do provide nullE;
					     )
					else provide codevalue;
					);
			      	   while true do provide nullE;));
			 if haderror then return(backtr(errorreturn));
			 )
		    else (
			 stash.framesize = previousStashedFrame;
			 f.next = previousFrame;
			 f.scopenum = desc.scopenum;
			 foreach code at i in cs do (
			      codevalue := eval(code);
			      when codevalue
			      is Error do return(backtr(codevalue))
			      else f.values.i = codevalue;
			      );
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = backtrFunction(ret,report(c,length(cs)))
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    foreach x in f.values do x = nullE;
		    f.next = stash.framesize;
		    stash.framesize = f;
		    ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    haderror := false;
		    f := Frame(previousFrame,desc.scopenum,
			 new Sequence len framesize do (
			      foreach code in cs do (
				   codevalue := eval(code);
				   when codevalue 
				   is Error do (
					haderror = true;
					errorreturn = codevalue;
					while true do provide nullE;
					)
				   else provide codevalue;
				   );
			      while true do provide nullE));
		    if haderror then return(backtr(errorreturn));
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = backtrFunction(ret,report(c,length(cs)))
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    ret
		    )
	       )
	  )
     );
export apply(f:Expr,v:Sequence):Expr := (
     when f
     is ff:CompiledFunction do (
	  ret := ff.fn(Expr(v));
	  when ret is Error do backtr(ret,list(f,Expr(v))) else ret)
     is ff:CompiledFunctionClosure do (
	  ret := ff.fn(Expr(v),ff.env);
	  when ret is Error do backtr(ret,list(f,Expr(v))) else ret)
     is c:FunctionClosure do apply(c,v)
     else buildErrorPacket("expected a function",list(f,Expr(v))));
export apply(f:Expr,e:Expr):Expr := (
     when f
     is ff:CompiledFunction do (
	  ret := ff.fn(e);
	  when ret is Error do backtr(ret,list(f,e)) else ret)
     is ff:CompiledFunctionClosure do (
	  ret := ff.fn(e,ff.env);
	  when ret is Error do backtr(ret,list(f,e)) else ret)
     is c:FunctionClosure do (
	  when e
	  is v:Sequence do apply(c,v)
	  else apply(c,e)
	  )
     else buildErrorPacket("expected a function",list(f,e)));
export apply2 := apply;
--------
-- could optimize later
export apply(g:Expr,e0:Expr,e1:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1)))
     when g
     is ff:CompiledFunction do (
	  ret := ff.fn(Expr(Sequence(e0,e1)));
	  when ret is Error do backtr(ret,list(g,seq(e0,e1))) else ret)
     is ff:CompiledFunctionClosure do (
	  ret := ff.fn(Expr(Sequence(e0,e1)),ff.env);
	  when ret is Error do backtr(ret,list(g,seq(e0,e1))) else ret)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then apply(c,Expr(Sequence(e0,e1)))
	  else if desc.numparms != 2
	  then backtr(WrongNumArgs(model.parms,desc.numparms,2))
	  else if recursiondepth > recursionlimit then RecursionLimit()
	  else (
	       previousFrame := c.frame;
	       framesize := desc.framesize;
	       if (!desc.hasClosure) && framesize < length(stash) then (
		    f := stash.framesize;
		    previousStashedFrame := f.next;
		    if f == previousStashedFrame then (
			 f = Frame(previousFrame,desc.scopenum,
			      new Sequence len framesize do (
				   provide e0;
				   provide e1;
				   while true do provide nullE;
				   )
			      );
			 )
		    else (
			 stash.framesize = previousStashedFrame;
			 f.next = previousFrame;
			 f.scopenum = desc.scopenum;
			 f.values.0 = e0;
			 f.values.1 = e1;
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = backtrFunction(ret,report(c,2)) 
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    foreach x in f.values do x = nullE;
		    f.next = stash.framesize;
		    stash.framesize = f;
		    ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    haderror := false;
		    f := Frame(previousFrame,desc.scopenum,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      while true do provide nullE));
		    if haderror then return(backtr(errorreturn));
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = backtrFunction(ret,report(c,2)) 
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    ret
		    )
	       )
	  )
     else buildErrorPacket("expected a function"));     
export apply(g:Expr,e0:Expr,e1:Expr,e2:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1,e2)));
     when g
     is ff:CompiledFunction do (
	  ret := ff.fn(Expr(Sequence(e0,e1,e2)));
	  when ret is Error do backtr(ret,list(g,seq(e0,e1,e2))) else ret)
     is ff:CompiledFunctionClosure do (
	  ret := ff.fn(Expr(Sequence(e0,e1,e2)),ff.env);
	  when ret is Error do backtr(ret,list(g,seq(e0,e1,e2))) else ret)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then apply(c,Expr(Sequence(e0,e1,e2)))
	  else if desc.numparms != 3
	  then backtr(WrongNumArgs(model.parms,desc.numparms,3))
	  else if recursiondepth > recursionlimit then RecursionLimit()
	  else (
	       previousFrame := c.frame;
	       framesize := desc.framesize;
	       if (!desc.hasClosure) && framesize < length(stash) then (
		    f := stash.framesize;
		    previousStashedFrame := f.next;
		    if f == previousStashedFrame then (
			 f = Frame(previousFrame,desc.scopenum,
			      new Sequence len framesize do (
				   provide e0;
				   provide e1;
				   provide e2;
				   while true do provide nullE;
				   )
			      );
			 )
		    else (
			 stash.framesize = previousStashedFrame;
			 f.next = previousFrame;
			 f.scopenum = desc.scopenum;
			 f.values.0 = e0;
			 f.values.1 = e1;
			 f.values.2 = e2;
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = backtrFunction(ret,report(c,3)) 
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    foreach x in f.values do x = nullE;
		    f.next = stash.framesize;
		    stash.framesize = f;
		    ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    haderror := false;
		    f := Frame(previousFrame,desc.scopenum,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      provide e2;
			      while true do provide nullE));
		    if haderror then return(backtr(errorreturn));
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = backtrFunction(ret,report(c,3)) 
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    ret
		    )
	       )
	  )
     else buildErrorPacket("expected a function"));     
export MissingMethod(name:string,method:string):Expr := (
     buildErrorPacket(quoteit(name) + " expected item to have a method for " + method));
export MissingMethod(method:SymbolClosure):Expr := (
     buildErrorPacket("expected a method for "+quoteit(method.symbol.word.name)));
export MissingMethodPair(method:string):Expr := (
     buildErrorPacket("expected pair to have a method for "+quoteit(method)));
export MissingMethodPair(method:SymbolClosure):Expr := (
     buildErrorPacket("expected pair to have a method for "+
	  quoteit(method.symbol.word.name)));
export MissingMethodPair(method:SymbolClosure,left:Expr,right:Expr):Expr := (
     backtr(
	  buildErrorPacket(
	       "expected pair to have a method for "
	       +quoteit(method.symbol.word.name)
	       ),
	  list(Expr(method),left,right)));
