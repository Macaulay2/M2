--		Copyright 1994-2003 by Daniel R. Grayson

use C;
use system;
use binding;
use parser;
use lex;
use gmp;
use nets;
use tokens;
use err;
use stdiop;
use common;
use ctype;
use stdio;
use varstrin;
use strings;
use basic;
use convertr;
use engine;
use objects;

-----------------------------------------------------------------------------
-- put bindings to variables before the forward references, for safety
export globalAssignmentHooks := newHashTable(mutableHashTableClass,nothingClass);
setupconst("globalAssignmentHooks",Expr(globalAssignmentHooks));
export evalSequenceHadError := false;
export evalSequenceErrorMessage := nullE;
errorreturn := nullE;
recycleBin := new array(Frame) len 20 do provide dummyFrame;
export trace := false;
NumberErrorMessagesShown := 0;
export recursiondepth := 0;
export recursionlimit := 300;
----------------------------------------------------------------------------------------
-- just one forward reference -- we have many mutually recursive functions in this file, too bad!
export eval(c:Code):Expr;
----------------------------------------------------------------------------------------
export RecursionLimit():Expr := buildErrorPacket("recursion limit of " + tostring(recursionlimit) + " exceeded");

export storeInHashTable(x:HashTable,i:Code,rhs:Code):Expr := (
     ival := eval(i);
     when ival is Error do ival else (
	  val := eval(rhs);
	  when val is Error do val else storeInHashTable(x,ival,val)));

assignvector(x:Sequence,i:Code,rhs:Code):Expr := (
     ival := eval(i);
     when ival
     is j:Integer do (
	  if isInt(j)
	  then (
	       k := toInt(j);
	       if k < -length(x) || k >= length(x)
	       then printErrorMessage(i,"subscript out of bounds 0 .. "+tostring(length(x)-1))
	       else (
		    val := eval(rhs);
		    when val is Error do val
		    else (
			 if k < 0
			 then x.(length(x) + k) = val
			 else x.k = val;
			 val)))
	  else printErrorMessage(i,"subscript out of bounds"))
     is Error do ival
     else printErrorMessage(i,"expected integer as subscript")
     );

dbmstore(f:Database,KEY:Code,CONTENT:Code):Expr := (
     Key := eval(KEY);
     when Key
     is Error do Key
     is key:string do (
	  Content := eval(CONTENT);
	  when Content
	  is Error do Content
	  is content:string do dbmstore(f,key,content)
	  is Nothing do (
	       if !f.isopen then return buildErrorPacket("database closed");
	       if !f.mutable then return buildErrorPacket("database not mutable");
	       if 0 == dbmdelete(f.handle,key)
	       then nullE
	       else buildErrorPacket(dbmstrerror() + " : " + f.filename))
	  else printErrorMessage(CONTENT,"expected a string or null"))
     else printErrorMessage(KEY,"expected a string"));

assignelemfun(lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(lhsarray);
     when x
     is x:List do (
	  if x.mutable then assignvector(x.v,lhsindex,rhs)
	  else buildErrorPacket("assignment attempted to element of immutable list")
	  )
     is x:Sequence do buildErrorPacket("assignment attempted to element of sequence")
     is x:HashTable do storeInHashTable(x,lhsindex,rhs)
     is x:Database do dbmstore(x,lhsindex,rhs)
     else printErrorMessage(lhsarray,"expected a list, sequence, hash table, or database")
     );
AssignElemFun = assignelemfun;

assignquotedobject(x:HashTable,i:Code,rhs:Code):Expr := (
     when i
     is c:globalSymbolClosureCode do (
	  ival := Expr(SymbolClosure(globalFrame,c.symbol));
	  val := eval(rhs);
	  when val is Error do val else storeInHashTable(x,ival,val))
     else printErrorMessage(i,"'.' expected right hand argument to be a symbol")
     );

assignquotedelemfun(lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(lhsarray);
     when x
     is x:HashTable do assignquotedobject(x,lhsindex,rhs)
     else printErrorMessage(lhsarray,"'.' expected left hand side to be a hash table")
     );
AssignQuotedElemFun = assignquotedelemfun;

evalWhileDoCode(c:whileDoCode):Expr := (
     while true do (
	  p := eval(c.predicate);
	  when p is err:Error 
	  do return if err.message == breakMessage then err.value else p
	  else if p == True then (
	       b := eval(c.doClause);
	       when b is err:Error 
	       do return if err.message == breakMessage then err.value else b 
	       else nothing;
	       )
	  else if p == False then break
	  else return printErrorMessage(c.predicate,"expected true or false"));
     nullE);

evalWhileListCode(c:whileListCode):Expr := (
     n := 1;
     r := new Sequence len n do provide nullE;
     i := 0;
     while true do (
	  p := eval(c.predicate);
	  when p is err:Error
	  do return if err.message == breakMessage then err.value else p
	  else if p == True then (
	       b := eval(c.listClause);
	       when b is err:Error 
	       do return if err.message == breakMessage then err.value else b 
	       else (
		    if i == n then (
			 n = 2*n;
			 r = new Sequence len n do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    );
	       )
	  else if p == False then break
	  else return printErrorMessage(c.predicate,"expected true or false"));
     Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == n then r
	       else new Sequence len i do foreach x in r do provide x)));

evalWhileListDoCode(c:whileListDoCode):Expr := (
     r := new Sequence len 1 do provide nullE;
     i := 0;
     while true do (
	  p := eval(c.predicate);
	  when p is err:Error do return if err.message == breakMessage then err.value else p
	  else if p == True then (
	       b := eval(c.listClause);
	       when b is err:Error
	       do return if err.message == breakMessage then err.value else b
	       else (
		    if i == length(r) then (
			 r = new Sequence len 2*length(r) do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    );
	       d := eval(c.doClause);
	       when d is err:Error
	       do return if err.message == breakMessage then err.value else d
	       else nothing;
	       )
	  else if p == False then break
	  else return printErrorMessage(c.predicate,"expected true or false"));
     Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == length(r) then r
	       else new Sequence len i do foreach x in r do provide x)));

evalForCode(c:forCode):Expr := (
     r := if c.listClause == dummyCode then emptySequence else new Sequence len 1 do provide nullE;
     i := 0;				    -- index in r
     j := 0;				    -- the value of the loop variable
     n := 0;				    -- the upper bound on j, if there is a toClause.
     if c.fromClause != dummyCode then (
	  fromvalue := eval(c.fromClause);
	  when fromvalue is f:Integer do (
	       if isInt(f) then j = toInt(f)
	       else return printErrorMessage(c.fromClause,"expected a small integer"))
	  else return printErrorMessage(c.fromClause,"expected an integer"));
     if c.toClause != dummyCode then (
	  tovalue := eval(c.toClause);
	  when tovalue is f:Integer do (
	       if isInt(f) then n = toInt(f)
	       else return printErrorMessage(c.toClause,"expected a small integer"))
	  else return printErrorMessage(c.toClause,"expected an integer"));
     localFrame = Frame(localFrame,c.frameID,c.framesize,false,new Sequence len c.framesize do provide nullE);
     while true do (
	  if c.toClause != dummyCode && j > n then break;
	  localFrame.values.0 = toInteger(j);		    -- should be the frame spot for the loop var!
	  j = j+1;
	  if c.whenClause != dummyCode then (
	       p := eval(c.whenClause);
	       when p is err:Error do (
		    localFrame = localFrame.outerFrame;
		    return if err.message == breakMessage then err.value else p)
	       else if p == False then break
	       else if p != True then (
		    localFrame = localFrame.outerFrame;
		    return printErrorMessage(c.whenClause,"expected true or false")));
	  if c.listClause != dummyCode then (
	       b := eval(c.listClause);
	       when b is err:Error do (
		    if err.message == continueMessage then b = err.value
		    else (
			 localFrame = localFrame.outerFrame;
			 return if err.message == breakMessage then err.value else b))
	       else nothing;
	       if i == length(r) then (
		    r = new Sequence len 2*length(r) do (
			 foreach x in r do provide x;
			 while true do provide nullE));
	       r.i = b;
	       i = i+1;
	       );
	  if c.doClause != dummyCode then (
	       b := eval(c.doClause);
	       when b is err:Error do (
		    if err.message != continueMessage then (
			 localFrame = localFrame.outerFrame;
			 return if err.message == breakMessage then err.value else b))
	       else nothing));
     localFrame = localFrame.outerFrame;
     if c.listClause == dummyCode then nullE
     else Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == length(r) then r
	       else new Sequence len i do foreach x in r do provide x)));

export evalSequence(v:CodeSequence):Sequence := (
     evalSequenceHadError = false;
     n := length(v);
     if n == 0 then emptySequence
     else if n == 1 then (
	  x := eval(v.0);
	  when x is Error do (
	       evalSequenceHadError = true;
	       evalSequenceErrorMessage = x;
	       emptySequence
	       )
	  else Sequence(x)
	  )
     else if n == 2 then (
	  x := eval(v.0);
	  when x is Error do (
	       evalSequenceHadError = true;
	       evalSequenceErrorMessage = x;
	       emptySequence
	       )
	  else (
	       y := eval(v.1);
	       when y is Error do (
		    evalSequenceHadError = true;
		    evalSequenceErrorMessage = y;
		    emptySequence
		    )
	       else Sequence(x,y)))
     else (
	  r := new Sequence len n do (
		    foreach c in v do (
			 value := eval(c);
			 when value 
			 is Error do (
			      evalSequenceHadError = true;
			      evalSequenceErrorMessage = value;
			      while true do provide nullE;
			      )
			 else nothing;
			 provide value;
			 ));
	  if evalSequenceHadError then r = emptySequence;
	  r));

export apply(c:FunctionClosure,v:Sequence):Expr := (
     previousFrame := c.frame;
     model := c.model;
     desc := model.desc;
     framesize := desc.framesize;
     if recursiondepth > recursionlimit then RecursionLimit()
     else if desc.restargs then (
	  -- assume numparms == 1
     	  if framesize < length(recycleBin) then (
	       f := recycleBin.framesize;
	       previousStashedFrame := f.outerFrame;
	       if f == previousStashedFrame then (
		    f = Frame(previousFrame,desc.frameID,framesize,false,
		    	 new Sequence len framesize do (
			      provide v;
			      while true do provide nullE)
			 );
		    )
	       else (
		    recycleBin.framesize = previousStashedFrame;
		    f.outerFrame = previousFrame;
		    f.frameID = desc.frameID;
		    f.values.0 = v;
		    );
	       recursiondepth = recursiondepth + 1;
	       saveLocalFrame := localFrame;
	       localFrame = f;
	       ret := eval(model.body);
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       if !f.notrecyclable then (
	       	    foreach x in f.values do x = nullE;
		    f.outerFrame = recycleBin.framesize;
		    f.frameID = -2;				    -- just to be tidy, not really needed
		    recycleBin.framesize = f;
		    );
	       when ret is err:Error do returnFromFunction(ret) else ret
	       )
	  else (
	       recursiondepth = recursiondepth + 1;
	       f := Frame(previousFrame,desc.frameID,framesize,false,
		    new Sequence len framesize do (
			 provide v;
			 while true do provide nullE));
	       saveLocalFrame := localFrame;
	       localFrame = f;
	       ret := eval(model.body);
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       when ret is err:Error do returnFromFunction(ret) else ret
	       )
	  )
     else if desc.numparms != length(v)
     then WrongNumArgs(model.parms,desc.numparms,length(v))
     else (
	  if framesize == 0 then (
	       recursiondepth = recursiondepth + 1;
	       saveLocalFrame := localFrame;
	       localFrame = previousFrame;
	       ret := eval(model.body);
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       when ret is err:Error do returnFromFunction(ret) else ret
	       )
	  else (
     	       if framesize < length(recycleBin) then (
		    f := recycleBin.framesize;
		    previousStashedFrame := f.outerFrame;
		    if f == previousStashedFrame then (
			 f = Frame(previousFrame,desc.frameID,framesize,false,
			      new Sequence len framesize do (
			      	   foreach x in v do provide x;
			      	   while true do provide nullE)
			      );
			 )
		    else (
			 recycleBin.framesize = previousStashedFrame;
			 f.outerFrame = previousFrame;
			 f.frameID = desc.frameID;
			 foreach x at i in v do f.values.i = x;
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    when ret is err:Error do returnFromFunction(ret) else ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      foreach x in v do provide x;
			      while true do provide nullE));
		    localFrame = f;
		    ret := eval(model.body);
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    when ret is err:Error do returnFromFunction(ret) else ret
		    )
	       )
	  )
     );
export apply(c:FunctionClosure,e:Expr):Expr := (
     -- single argument 'e' provided not a sequence, so framesize > 0
     previousFrame := c.frame;
     model := c.model;
     desc := model.desc;
     framesize := desc.framesize;
     if desc.numparms != 1
     then return printErrorMessage(model.parms, "expected " +tostring(desc.numparms)
	  +" argument"
	  +(if desc.numparms == 1 then "" else "s")
	  +" but got 1"
	  );
     if recursiondepth > recursionlimit then return RecursionLimit();
     recursiondepth = recursiondepth + 1;
     if framesize < length(recycleBin) then (
	  f := recycleBin.framesize;
	  previousStashedFrame := f.outerFrame;
	  if f == previousStashedFrame then (
	       f = Frame(previousFrame,desc.frameID,framesize,false,
		    new Sequence len framesize do (
			 provide e;
			 while true do provide nullE));
	       )
	  else (
	       recycleBin.framesize = previousStashedFrame;
	       f.outerFrame = previousFrame;
	       f.frameID = desc.frameID;
	       f.values.0 = e;
	       );
     	  saveLocalFrame := localFrame;
	  localFrame = f;
	  ret := eval(model.body);
	  localFrame = saveLocalFrame;
	  if !f.notrecyclable then (
	       foreach x in f.values do x = nullE;
	       f.outerFrame = recycleBin.framesize;
	       f.frameID = -2;				    -- just to be tidy, not really needed
	       recycleBin.framesize = f;
	       );
	  recursiondepth = recursiondepth - 1;
	  when ret is err:Error do returnFromFunction(ret) else ret
	  	     -- this check takes time, too!
	  )
     else (
	  f := Frame(previousFrame,desc.frameID,framesize,false,
	       new Sequence len framesize do (
		    provide e;
		    while true do provide nullE));
     	  saveLocalFrame := localFrame;
     	  localFrame = f;
	  ret := eval(model.body);
	  localFrame = saveLocalFrame;
	  recursiondepth = recursiondepth - 1;
	  when ret is err:Error do returnFromFunction(ret) else ret
	      -- this check takes time, too!
	  )
     );

export apply(c:FunctionClosure,cs:CodeSequence):Expr := (
     -- in this version we try to avoid allocating an array of exprs to
     -- hold the parameters, preferring to stuff them into the frame
     -- directly
     model := c.model;
     desc := model.desc;
     if desc.restargs
     then (
	  v := evalSequence(cs);
	  if evalSequenceHadError then evalSequenceErrorMessage else apply(c,v)
	  )
     else if desc.numparms != length(cs)
     then WrongNumArgs(model.parms,desc.numparms,length(cs))
     else if recursiondepth > recursionlimit then RecursionLimit()
     else (
     	  previousFrame := c.frame;
     	  framesize := desc.framesize;
	  if framesize == 0 then (
	       recursiondepth = recursiondepth + 1;
	       saveLocalFrame := localFrame;
	       localFrame = previousFrame;
	       ret := eval(model.body);
	       when ret is err:Error do ret = returnFromFunction(ret)
	       else nothing;
	       localFrame = saveLocalFrame;
	       recursiondepth = recursiondepth - 1;
	       ret
	       )
	  else (
     	       if framesize < length(recycleBin) then (
		    f := recycleBin.framesize;
		    previousStashedFrame := f.outerFrame;
		    if f == previousStashedFrame then (
		    	 haderror := false;
			 f = Frame(previousFrame,desc.frameID,framesize,false,
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
			 if haderror then return errorreturn;
			 )
		    else (
			 recycleBin.framesize = previousStashedFrame;
			 f.outerFrame = previousFrame;
			 f.frameID = desc.frameID;
			 foreach code at i in cs do (
			      codevalue := eval(code);
			      when codevalue
			      is Error do return codevalue
			      else f.values.i = codevalue;
			      );
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = returnFromFunction(ret)
		    else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    haderror := false;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
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
		    if haderror then return errorreturn;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = returnFromFunction(ret)
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
     is ff:CompiledFunction do ff.fn(Expr(v))
     is ff:CompiledFunctionClosure do ff.fn(Expr(v),ff.env)
     is c:FunctionClosure do apply(c,v)
     else buildErrorPacket("expected a function"));
export apply(f:Expr,e:Expr):Expr := (
     when f
     is ff:CompiledFunction do ff.fn(e)
     is ff:CompiledFunctionClosure do ff.fn(e,ff.env)
     is c:FunctionClosure do (
	  when e
	  is v:Sequence do apply(c,v)
	  else apply(c,e)
	  )
     else buildErrorPacket("expected a function"));
export apply2 := apply;
--------
-- could optimize later
export apply(g:Expr,e0:Expr,e1:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1)))
     when g
     is ff:CompiledFunction do ff.fn(Expr(Sequence(e0,e1)))
     is ff:CompiledFunctionClosure do ff.fn(Expr(Sequence(e0,e1)),ff.env)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then apply(c,Expr(Sequence(e0,e1)))
	  else if desc.numparms != 2
	  then WrongNumArgs(model.parms,desc.numparms,2)
	  else if recursiondepth > recursionlimit then RecursionLimit()
	  else (
	       previousFrame := c.frame;
	       framesize := desc.framesize;
	       if framesize < length(recycleBin) then (
		    f := recycleBin.framesize;
		    previousStashedFrame := f.outerFrame;
		    if f == previousStashedFrame then (
			 f = Frame(previousFrame,desc.frameID,framesize,false,
			      new Sequence len framesize do (
				   provide e0;
				   provide e1;
				   while true do provide nullE;
				   )
			      );
			 )
		    else (
			 recycleBin.framesize = previousStashedFrame;
			 f.outerFrame = previousFrame;
			 f.frameID = desc.frameID;
			 f.values.0 = e0;
			 f.values.1 = e1;
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    haderror := false;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      while true do provide nullE));
		    if haderror then return errorreturn;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = returnFromFunction(ret) 
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
     is ff:CompiledFunction do ff.fn(Expr(Sequence(e0,e1,e2)))
     is ff:CompiledFunctionClosure do ff.fn(Expr(Sequence(e0,e1,e2)),ff.env)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then apply(c,Expr(Sequence(e0,e1,e2)))
	  else if desc.numparms != 3
	  then WrongNumArgs(model.parms,desc.numparms,3)
	  else if recursiondepth > recursionlimit then RecursionLimit()
	  else (
	       previousFrame := c.frame;
	       framesize := desc.framesize;
	       if framesize < length(recycleBin) then (
		    f := recycleBin.framesize;
		    previousStashedFrame := f.outerFrame;
		    if f == previousStashedFrame then (
			 f = Frame(previousFrame,desc.frameID,framesize,false,
			      new Sequence len framesize do (
				   provide e0;
				   provide e1;
				   provide e2;
				   while true do provide nullE;
				   )
			      );
			 )
		    else (
			 recycleBin.framesize = previousStashedFrame;
			 f.outerFrame = previousFrame;
			 f.frameID = desc.frameID;
			 f.values.0 = e0;
			 f.values.1 = e1;
			 f.values.2 = e2;
			 );
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    recursiondepth = recursiondepth + 1;
		    saveLocalFrame := localFrame;
		    haderror := false;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      provide e2;
			      while true do provide nullE));
		    if haderror then return errorreturn;
		    localFrame = f;
		    ret := eval(model.body);
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    recursiondepth = recursiondepth - 1;
		    ret
		    )
	       )
	  )
     else buildErrorPacket("expected a function"));     

-----------------------------------------------------------------------------

export unarymethod(rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(rhs);
     when right is Error do right
     else (
	  method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
	  if method == nullE then MissingMethod(methodkey)
	  else apply(method,right)));
export binarymethod(lhs:Code,rhs:Code,methodkey:SymbolClosure):Expr := (
     left := eval(lhs);
     when left is Error do left
     else (
	  right := eval(rhs);
	  when right is Error do right
	  else (
	       method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),
		    methodkey.symbol.hash);
	       if method == nullE then MissingMethodPair(methodkey,left,right)
	       else apply(method,left,right))));
export binarymethod(left:Expr,rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(rhs);
     when right is Error do right
     else (
	  method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),
	       methodkey.symbol.hash);
	  if method == nullE then (
	       if methodkey == AdjacentS
	       then when left is f:SymbolClosure do buildErrorPacket("symbol '" + f.symbol.word.name + "' has not been defined as a function")
	       else MissingMethodPair(methodkey,left,right)
	       else MissingMethodPair(methodkey,left,right)
	       )
	  else apply(method,left,right)));

-----------------------------------------------------------------------------

evalAdjacentCode(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is c:FunctionClosure do (
	  when rhs
	  is cs:sequenceCode do apply(c,cs.x)
	  else (
	       e := eval(rhs);
	       when e is Error do e
	       is v:Sequence do apply(c,v)
	       else apply(c,e)))
     is ff:CompiledFunction do (
	  e := eval(rhs);
	  when e is Error do e
	  else ff.fn(e))
     is ff:CompiledFunctionClosure do (
	  e := eval(rhs);
	  when e is Error do e
	  else ff.fn(e,ff.env))
     is Error do left
     else binarymethod(left,rhs,AdjacentS));

-----------------------------------------------------------------------------

globalAssignmentHook(t:Symbol,oldvalue:Expr,newvalue:Expr):Expr := (
     method := lookup(Class(oldvalue),GlobalReleaseE);
     sym := Expr(SymbolClosure(globalFrame,t));
     -- top level hooks for assignment to a global variable
     g := lookup1(globalAssignmentHooks,sym);
     if g != notfoundE then (
	  y := when g
	  is s:List do (
	       foreach f in s.v do (
		    r := apply(f,sym,newvalue);
		    when r is Error do return r else nothing;
		    );
	       nullE)
	  is f:CompiledFunction do apply(g,sym,newvalue)
	  is f:CompiledFunctionClosure do apply(g,sym,newvalue)
	  is f:FunctionClosure do apply(g,sym,newvalue)
	  else buildErrorPacket("expected global assignment hook for " + t.word.name + " to be a function or list of functions");
	  when y is Error do return y else nothing;
	  );
     if method != nullE then (
	  y := apply(method,sym,oldvalue);
	  when y is Error do return y else nothing;
	  );
     method = lookup(Class(newvalue),GlobalAssignE);
     if method != nullE then (
	  y := apply(method,sym,newvalue);
	  when y is Error do return y else nothing;
	  );
     nullE
     );

localAssignment(nestingDepth:int,frameindex:int,newvalue:Expr):Expr := ( -- frameID != 0
     f := localFrame;
     if nestingDepth == 0 then nothing
     else if nestingDepth == 1 then f = f.outerFrame
     else if nestingDepth == 2 then f = f.outerFrame.outerFrame
     else (
	  f = f.outerFrame.outerFrame.outerFrame;
	  nestingDepth = nestingDepth - 3;
	  while nestingDepth > 0 do ( nestingDepth = nestingDepth - 1; f = f.outerFrame );
	  );
     f.values.frameindex = newvalue;
     newvalue);

globalAssignment(frameindex:int,t:Symbol,newvalue:Expr):Expr := ( -- frameID = 0
     if t.protected then return buildErrorPacket("assignment to protected variable '" + t.word.name + "'");
     vals := globalFrame.values;
     r := globalAssignmentHook(t,vals.frameindex,newvalue);
     when r is Error do return r else nothing;
     vals.frameindex = newvalue;
     newvalue);

assignment(nestingDepth:int,frameindex:int,t:Symbol,newvalue:Expr):Expr := (
     if nestingDepth == -1
     then globalAssignment(frameindex,t,newvalue)
     else localAssignment(nestingDepth,frameindex,newvalue));

globalAssignmentFun(x:globalAssignmentCode):Expr := (
     t := x.lhs;
     newvalue := eval(x.rhs);
     when newvalue is Error do return newvalue else nothing;
     globalAssignment(t.frameindex,t,newvalue));

parallelAssignmentFun(x:parallelAssignmentCode):Expr := (
     syms := x.lhs;
     nestingDepth := x.nestingDepth;
     frameindex := x.frameindex;
     nlhs := length(frameindex);
     foreach sym in syms do if sym.protected then return buildErrorPacket("assignment to protected variable '" + sym.word.name + "'");
     value := eval(x.rhs);
     when value 
     is Error do return value 
     is values:Sequence do (
	  nvals := length(values);
	  if nlhs == nvals
	  then (
	       for i from 0 to nlhs-1 do (
		    r := assignment(nestingDepth.i,frameindex.i,syms.i,values.i);
		    when r is Error do return r else nothing;
		    )
	       )
	  else if nlhs < nvals
	  then (
	       for i from 0 to nlhs-2 do (
		    r := assignment(nestingDepth.i,frameindex.i,syms.i,values.i);
		    when r is Error do return r else nothing;
		    );
	       m := nlhs-1;
	       r := assignment(nestingDepth.m,frameindex.m,syms.m, Expr(new Sequence len nvals-nlhs+1 do for i from nlhs-1 to nvals-1 do provide values.i));
	       when r is Error do return r else nothing;
	       )
	  else (
	       for i from 0     to nvals-1 do (
		    r := assignment(nestingDepth.i,frameindex.i,syms.i,values.i);
		    when r is Error do return r else nothing;
		    );
	       for i from nvals to nlhs-1 do (
		    r := assignment(nestingDepth.i,frameindex.i,syms.i,nullE);
		    when r is Error do return r else nothing;
		    );
	       )
	  )
     else (
	  r := assignment(nestingDepth.0,frameindex.0,syms.0,value);
	  when r is Error do return r else nothing;
	  for i from 1 to nlhs-1 do (
	       r = assignment(nestingDepth.i,frameindex.i,syms.i,nullE);
	       when r is Error do return r else nothing;
	       );
	  );
     value);

-----------------------------------------------------------------------------

export fullBacktrace := false;
export backtrace := true;

export eval(c:Code):Expr := (
     e := (
	  if interrupted then (
	       if alarmed then (
		    interrupted = false;
		    alarmed = false;
		    buildErrorPacket(alarmMessage))
	       else (
		    interrupted = false;
		    SuppressErrors = false;
		    buildErrorPacket(interruptMessage)))
	  else when c
	  is u:unaryCode do u.f(u.rhs)
	  is b:binaryCode do b.f(b.lhs,b.rhs)
	  is b:adjacentCode do evalAdjacentCode(b.lhs,b.rhs)
	  is m:functionCode do return Expr(FunctionClosure(noRecycle(localFrame),m))
	  is r:localMemoryReferenceCode do (
	       f := localFrame;
	       nd := r.nestingDepth;
	       if nd == 0 then nothing
	       else if nd == 1 then f = f.outerFrame
	       else if nd == 2 then f = f.outerFrame.outerFrame
	       else (
		    f = f.outerFrame.outerFrame.outerFrame;
		    nd = nd - 3;
		    while nd > 0 do ( nd = nd - 1; f = f.outerFrame );
		    );
	       return f.values.(r.frameindex))
	  is r:globalMemoryReferenceCode do return globalFrame.values.(r.frameindex)
	  is x:localAssignmentCode do (
	       newvalue := eval(x.rhs);
	       when newvalue is Error do return newvalue 
	       else localAssignment(x.nestingDepth,x.frameindex,newvalue))
	  is a:globalAssignmentCode do globalAssignmentFun(a)
	  is p:parallelAssignmentCode do parallelAssignmentFun(p)
	  is c:globalSymbolClosureCode do return Expr(SymbolClosure(globalFrame,c.symbol))
	  is c:tryCode do (
	       oldSuppressErrors := SuppressErrors;
	       SuppressErrors = true;
	       p := eval(c.code);
	       if !SuppressErrors then p		  -- eval could have turned it off
	       else (
		    SuppressErrors = oldSuppressErrors;
		    when p is err:Error do (
			 if err.message == breakMessage || err.message == returnMessage || err.message == continueMessage || err.message == unwindMessage then p
			 else eval(c.elseClause))
		    else if c.thenClause == NullCode then p else eval(c.thenClause)))
	  is c:ifCode do (
	       p := eval(c.predicate);
	       when p is Error do p
	       else if p == True then eval(c.thenClause)
	       else if p == False then eval(c.elseClause)
	       else printErrorMessage(c.predicate,"expected true or false"))
	  is r:localSymbolClosureCode do (
	       f := localFrame;
	       nd := r.nestingDepth;
	       if nd == 0 then nothing
	       else if nd == 1 then f = f.outerFrame
	       else if nd == 2 then f = f.outerFrame.outerFrame
	       else (
		    f = f.outerFrame.outerFrame.outerFrame;
		    nd = nd - 3;
		    while nd > 0 do ( nd = nd - 1; f = f.outerFrame );
		    );
	       noRecycle(f);
	       return Expr(SymbolClosure(f,r.symbol)))
	  is b:ternaryCode do b.f(b.arg1,b.arg2,b.arg3)
	  is b:multaryCode do b.f(b.args)
	  is n:newLocalFrameCode do (
	       localFrame = Frame(localFrame,n.frameID,n.framesize,false, new Sequence len n.framesize do provide nullE);
	       x := eval(n.body);
	       localFrame = localFrame.outerFrame;
	       x)
	  is c:forCode do return evalForCode(c)
	  is c:whileListDoCode do evalWhileListDoCode(c)
	  is c:whileDoCode do evalWhileDoCode(c)
	  is c:whileListCode do evalWhileListCode(c)
	  is c:newCode do NewFun(c.newClause)
	  is c:newOfCode do NewOfFun(c.newClause,c.ofClause)
	  is c:newFromCode do NewFromFun(c.newClause,c.fromClause)
	  is c:newOfFromCode do NewOfFromFun(c.newClause,c.ofClause,c.fromClause)
	  is nullCode do return nullE
	  is v:realCode do return Expr(Real(v.x))
	  is v:integerCode do return Expr(v.x)
	  is v:stringCode do return Expr(v.x)
	  is v:sequenceCode do (
	       if length(v.x) == 0 then return emptySequence;
	       r := evalSequence(v.x);
	       if evalSequenceHadError then evalSequenceErrorMessage else Expr(r))
	  is v:listCode do (
	       if length(v.y) == 0 then return emptyList;
	       r := evalSequence(v.y);
	       if evalSequenceHadError then evalSequenceErrorMessage else list(r))
	  is v:arrayCode do (
	       if length(v.z) == 0 then return emptyArray;
	       r := evalSequence(v.z);
	       if evalSequenceHadError then evalSequenceErrorMessage else Array(r)
	       ));
     when e is err:Error do (
	  if SuppressErrors then return e;
	  if err.message == returnMessage || err.message == continueMessage || err.message == breakMessage || err.message == unwindMessage
	  then (
	       if err.position == dummyPosition then err.position = codePosition(c); -- there will be no way to enter the debugger to figure out an unhandled break
	       return e;
	       );
	  p := codePosition(c);
     	  if int(p.loadDepth) >= errorDepth && !err.position === p then (
	       oldReportFrame := err.report.code.frame;
	       err.report = CodeClosureList(CodeClosure(noRecycle(localFrame),c),err.report);
	       err.position = p;
	       if !err.printed || backtrace && localFrame != oldReportFrame || fullBacktrace then (
		    if (! p.filename==="stdio") && debuggingMode && !stopIfError && stdIO.inisatty && stdIO.outisatty then (
			 if !err.printed then printError(err);
			 printErrorMessage(err.position,"--entering debugger--");
			 z := debuggerFun(err.report.code.frame,err.report.code.code);
			 stderr << "--leaving debugger--" << endl;
			 when z is z:Error do (
			      if z.message == breakMessage then return buildErrorPacket(unwindMessage);
			      if z.message == returnMessage then return z.value;
			      if z.message == continueMessage then return eval(c);
			      )
			 else nothing)
		    else (printError(err);)));
	  e)
     else e);

export eval(f:Frame,c:Code):Expr := (
     saveLocalFrame := localFrame;
     localFrame = f;
     ret := eval(c);
     localFrame = saveLocalFrame;
     ret);

shieldfun(a:Code):Expr := (
     if interruptShield then eval(a)
     else (
     	  interruptPending = interrupted;
     	  interruptShield = true;
     	  ret := eval(a);
     	  interruptShield = false;
     	  interrupted = interruptPending;
	  if interrupted && !stdIO.inisatty then (
     	       endLine(stderr);
	       stderr << "interrupted" << endl;
	       exit(1);
	       );
     	  ret));
setupop(shieldS,shieldfun);     

returnFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,returnMessage,dummyCodeClosureList,e,false)));
setupop(returnS,returnFun);

continueFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,continueMessage,dummyCodeClosureList,e,false)));
setupop(continueS,continueFun);

breakFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,breakMessage,dummyCodeClosureList,e,false)));
setupop(breakS,breakFun);

-- export toExpr(p:Position):Expr := list(Sequence(Expr(p.filename),Expr(toInteger(int(p.line))),Expr(toInteger(int(p.column))),Expr(toInteger(int(p.loadDepth)))));
-- codePositionFun(a:Code):Expr := toExpr(codePosition(a));
-- setupop(codePositionS,codePositionFun);

assigntofun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is q:SymbolClosure do (
	  if q.symbol.protected then (
	       printErrorMessage(lhs, "assignment to protected variable '" + q.symbol.word.name + "'")
	       )
	  else (
	       value := eval(rhs);
	       when value is Error do return value else nothing;
	       q.frame.values.(q.symbol.frameindex) = value;
	       value))
     is o:HashTable do (
	  if o.mutable then (
	       y := eval(rhs);
	       when y is p:HashTable do (
		    o.table = copy(p.table);
		    o.numEntries = p.numEntries;
		    left)
	       is Error do y
	       else printErrorMessage(rhs,"expected hash table on right"))
	  else printErrorMessage(lhs,"encountered read only hash table"))
     is l:List do (
	  if l.mutable then (
	       y := eval(rhs);
	       when y
	       is p:List do ( l.v = copy(p.v); left)
	       is s:Sequence do ( l.v = copy(s); left)
	       is Error do y
	       else printErrorMessage(rhs,"'<-' expected list or sequence on right"))
	  else printErrorMessage(lhs,"'<-' encountered read-only list"))
     is Error do left
     else printErrorMessage(lhs,"'<-' expected symbol or hash table on left")
     );
setup(LeftArrowS,assigntofun);

idfun(e:Expr):Expr := e;
setupfun("identity",idfun);
scanpairs(f:Expr,obj:HashTable):Expr := (
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := apply(f,p.key,p.value);
	       when v is Error do return v else nothing;
	       p = p.next;
	       ));
     nullE);
scanpairsfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do
     if	       o.mutable
     then      WrongArg("an immutable hash table")
     else      scanpairs(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("scanPairs",scanpairsfun);

mappairs(f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := apply(f,p.key,p.value);
	       when v 
	       is Error do return v 
	       is Nothing do nothing
	       is a:Sequence do (
		    if length(a) == 2
		    then (
			 ret := storeInHashTable(newobj,a.0,a.1);
			 when ret is Error do return ret else nothing;
			 )
		    else return 
			 buildErrorPacket(
			      "'applyPairs' expected return value to be a pair or 'null'");
		    )
	       else return 
		    buildErrorPacket(
			 "'applyPairs' expected return value to be a pair or 'null'");
	       p = p.next;
	       ));
     sethash(newobj,obj.mutable);
     Expr(newobj));
mappairsfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do
     if        o.mutable 
     then      WrongArg("an immutable hash table")
     else      mappairs(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyPairs",mappairsfun);

export mapkeys(f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       newkey := apply(f,p.key);
	       if newkey == nullE then return buildErrorPacket("null key encountered"); -- remove soon!!!
	       when newkey is Error do return newkey else nothing;
	       storeInHashTableNoClobber(newobj,newkey,p.value);
	       p = p.next;
	       ));
     sethash(newobj,obj.mutable);
     Expr(newobj));
mapkeysfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do        
     if        o.mutable
     then      WrongArg("an immutable hash table")
     else      mapkeys(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyKeys",mapkeysfun);

export mapvalues(f:Expr,obj:HashTable):Expr := (
     u := newHashTable(obj.class,obj.parent);
     hadError := false;
     errm := nullE;
     u.numEntries = obj.numEntries;
     u.table = new array(KeyValuePair) len length(obj.table) do (
	  foreach bucket in obj.table do (
	       p := bucket;
	       q := bucketEnd;
	       while p != p.next do (
		    newvalue := apply(f,p.value);
		    when newvalue is Error do (
			 errm = newvalue;
			 hadError = true;
			 while true do provide bucketEnd;
			 )
		    else nothing;
		    q = KeyValuePair(p.key,p.hash,newvalue,q);
		    p = p.next;
		    );
	       provide q;
	       );
	  );
     if hadError then return errm;
     sethash(u,obj.mutable);
     Expr(u));
mapvaluesfun(e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do        
     if        o.mutable
     then      WrongArg("an immutable hash table")
     else      mapvalues(a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyValues",mapvaluesfun);

merge(e:Expr):Expr := (
     when e is v:Sequence do (
	  if length(v) != 3 then return WrongNumArgs(3);
	  g := v.2;
	  when v.0 is x:HashTable do
	  if x.mutable then WrongArg("an immutable hash table") else
	  when v.1 is y:HashTable do
	  if y.mutable then WrongArg("an immutable hash table") else 
	  if length(x.table) >= length(y.table) then (
	       z := copy(x);
	       z.mutable = true;
	       foreach bucket in y.table do (
		    q := bucket;
		    while q != q.next do (
			 val := lookup1(z,q.key,q.hash);
			 if val != notfoundE then (
			      t := apply(g,val,q.value);
			      when t is Error do return t else nothing;
			      storeInHashTable(z,q.key,q.hash,t);
			      )
			 else (
			      storeInHashTable(z,q.key,q.hash,q.value);
			      );
			 q = q.next));
	       mut := false;
	       if x.class == y.class && x.parent == y.parent then (
		    z.class = x.class;
		    z.parent = x.parent;
		    mut = x.mutable;
		    )
	       else (
		    z.class = hashTableClass;
		    z.parent = nothingClass);
	       sethash(z,mut);
	       Expr(z))
	  else (
	       z := copy(y);
	       z.mutable = true;
	       foreach bucket in x.table do (
		    q := bucket;
		    while q != q.next do (
			 val := lookup1(z,q.key,q.hash);
			 if val != notfoundE then (
			      t := apply(g,q.value,val);
			      when t is Error do return t else nothing;
			      storeInHashTable(z,q.key,q.hash,t);
			      )
			 else (
			      storeInHashTable(z,q.key,q.hash,q.value);
			      );
			 q = q.next));
	       mut := false;
	       if x.class == y.class && x.parent == y.parent then (
		    z.class = x.class;
		    z.parent = x.parent;
		    mut = x.mutable;
		    )
	       else (
		    z.class = hashTableClass;
		    z.parent = nothingClass;
		    );
	       sethash(z,mut);
	       Expr(z))
	  else WrongArg(2,"a hash table")
	  else WrongArg(1,"a hash table"))
     else WrongNumArgs(3));
setupfun("merge",merge);		  -- see objects.d
combine(f:Expr,g:Expr,h:Expr,x:HashTable,y:HashTable):Expr := (
     z := newHashTable(x.class,x.parent);
     foreach pp in x.table do (
	  p := pp;
	  while p != p.next do (
	       foreach qq in y.table do (
		    q := qq;
		    while q != q.next do (
			 pqkey := apply(f,p.key,q.key);
			 when pqkey is Error do return pqkey else nothing;
			 pqvalue := apply(g,p.value,q.value);
			 when pqvalue is Error do return pqvalue else nothing;
			 pqhash := hash(pqkey);
			 previous := lookup1(z,pqkey,pqhash);
			 r := storeInHashTable(z,pqkey,pqhash,
			      if previous == notfoundE
			      then pqvalue
			      else (
				   t := apply(h,previous,pqvalue);
				   when t is Error do return t else nothing;
				   t));
			 when r is Error do return r else nothing;
			 q = q.next);
		    );
	       p = p.next));
     sethash(z,x.mutable | y.mutable);
     z);
combine(e:Expr):Expr := (
     when e
     is v:Sequence do
     if length(v) == 5 then 
     when v.0 is x:HashTable do
     if x.mutable then WrongArg(1,"an immutable hash table") else
     when v.1 is y:HashTable do
     if y.mutable then WrongArg(2,"an immutable hash table") else
     combine(v.2,v.3,v.4,x,y)
     else WrongArg(1+1,"a hash table")
     else WrongArg(0+1,"a hash table")
     else WrongNumArgs(5)
     else WrongNumArgs(5));
setupfun("combine",combine);


export unarymethod(right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethod(methodkey)
     else apply(method,right));

export binarymethod(left:Expr,right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethodPair(methodkey,left,right)
     else apply(method,left,right));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
