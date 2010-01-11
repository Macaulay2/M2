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
use libfac;

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
----------------------------------------------------------------------------------------
-- a forward reference: we have many mutually recursive functions in this file, too bad!
export backtrace := true;
stepCount := -1;
microStepCount := -1;
lastCode := dummyCode;
lastCodePosition := Position("",ushort(0),ushort(0),ushort(0));
export eval(localInterpState:threadLocalInterp,c:Code):Expr;
export applyEE(localInterpState:threadLocalInterp,f:Expr,e:Expr):Expr;
export evalAllButTail(localInterpState:threadLocalInterp,c:Code):Code := while true do c = (
     when c
     is i:ifCode do (
	  p := eval(localInterpState,i.predicate);
	  when p is e:Error do Code(e)
	  else if p == True then i.thenClause
	  else if p == False then i.elseClause
	  else (
	       return Code(Error(codePosition(i.predicate),"expected true or false",nullE,false,dummyFrame));
	       dummyCode))
     is v:semiCode do (
	  w := v.w;
	  n := length(w);				    -- at least 2
	  r := eval(localInterpState,w.0);
	  when r is e:Error do return Code(e) else nothing;
	  if n == 2 then w.1 else (
	       r = eval(localInterpState,w.1);
	       when r is e:Error do return Code(e) else nothing;
	       if n == 3 then w.2 else (
		    r = eval(localInterpState,w.2);
		    when r is e:Error do return Code(e) else nothing;
		    if n == 4 then w.3 else (
			 r = eval(localInterpState,w.3);
			 when r is e:Error do return Code(e) else nothing;
			 if n == 5 then w.4 else (
			      r = eval(localInterpState,w.4);
			      when r is e:Error do return Code(e) else nothing;
			      i := 5;
			      while i < n-1 do (
				   r = eval(localInterpState,w.i);
				   when r is e:Error do return Code(e) else i = i+1);
			      w.i)))))
     else (
	  return c;
	  dummyCode));
----------------------------------------------------------------------------------------
export RecursionLimit():Expr := buildErrorPacket("recursion limit of " + tostring(recursionLimit) + " exceeded");
export InternalRecursionLimit():Expr := buildErrorPacket("internal recursion limit of " + tostring(recursionLimit) + " exceeded");

export storeInHashTable(localInterpState:threadLocalInterp,x:HashTable,i:Code,rhs:Code):Expr := (
     ival := eval(localInterpState,i);
     when ival is Error do ival else (
	  val := eval(localInterpState,rhs);
	  when val is Error do val else storeInHashTable(x,ival,val)));

export storeInDictionary(localInterpState:threadLocalInterp,dc:DictionaryClosure,i:Code,rhs:Code):Expr := (
     ival := eval(localInterpState,i);
     when ival
     is newname:string do (
	  rhsval := eval(localInterpState,rhs);
	  when rhsval
	  is sc:SymbolClosure do (
	       if dc.frame != sc.frame then buildErrorPacket("expected a symbol with the same dictionary frame")
	       else (
     		    newword := makeUniqueWord(newname,sc.symbol.word.parse);
		    when lookup(newword,dc.dictionary.symboltable)
		    is Symbol do return buildErrorPacket("symbol already exists: " + newname)
		    else (
			 insert(dc.dictionary.symboltable,newword,sc.symbol);
			 rhsval)))
	  is Error do rhsval 
	  else printErrorMessageE(rhs,"expected a symbol"))
     is Error do ival
     else printErrorMessageE(i,"expected a string"));

assignvector(localInterpState:threadLocalInterp,m:List,i:Code,rhs:Code):Expr := (
     x := m.v;
     ival := eval(localInterpState,i);
     when ival
     is j:ZZ do (
	  if isInt(j)
	  then (
	       k := toInt(j);
	       if k < 0 then k = k + length(x);
	       if k < 0 then return printErrorMessageE(i,"negative subscript out of bounds 0 .. "+tostring(length(x)-1));
	       val := eval(localInterpState,rhs);
	       when val is Error do return val else (
		    if k >= length(x) then (
			 x = new Sequence len k+1 do (
			      foreach t in x do provide t;
			      while true do provide nullE;
			      );
			 m.v = x; );
	       	    x.k = val;
	       	    val))
	  else printErrorMessageE(i,"expected small integer"))
     is Error do ival
     else printErrorMessageE(i,"index not an integer")
     );

dbmstore(localInterpState:threadLocalInterp,f:Database,KEY:Code,CONTENT:Code):Expr := (
     Key := eval(localInterpState,KEY);
     when Key
     is Error do Key
     is key:string do (
	  Content := eval(localInterpState,CONTENT);
	  when Content
	  is Error do Content
	  is content:string do dbmstore(f,key,content)
	  is Nothing do buildErrorPacket("storing null database record, use 'remove' to remove records")
	  else printErrorMessageE(CONTENT,"expected a string"))
     else printErrorMessageE(KEY,"expected a string"));

assignelemfun(localInterpState:threadLocalInterp,lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(localInterpState,lhsarray);
     when x
     is Error do x
     is x:List do (
	  if x.mutable then assignvector(localInterpState,x,lhsindex,rhs)
	  else buildErrorPacket("assignment attempted to element of immutable list")
	  )
     is x:Sequence do buildErrorPacket("assignment attempted to element of sequence")
     is x:HashTable do storeInHashTable(localInterpState,x,lhsindex,rhs)
     is x:Database do dbmstore(localInterpState,x,lhsindex,rhs)
     is dc:DictionaryClosure do (
	  if dc.dictionary.protected then printErrorMessageE(lhsarray,"attempted to create symbol in protected dictionary")
	  else storeInDictionary(localInterpState,dc,lhsindex,rhs))
     else printErrorMessageE(lhsarray,"expected a list, hash table, database, or dictionary")
     );

assignquotedobject(localInterpState:threadLocalInterp,x:HashTable,i:Code,rhs:Code):Expr := (
     when i
     is c:globalSymbolClosureCode do (
	  ival := Expr(SymbolClosure(globalFrame,c.symbol));
	  val := eval(localInterpState,rhs);
	  when val is Error do val else storeInHashTable(x,ival,val))
     else printErrorMessageE(i,"'.' expected right hand argument to be a symbol")
     );

assignquotedelemfun(localInterpState:threadLocalInterp,lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(localInterpState,lhsarray);
     when x
     is x:HashTable do assignquotedobject(localInterpState,x,lhsindex,rhs)
     else printErrorMessageE(lhsarray,"'.' expected left hand side to be a hash table")
     );

evalWhileDoCode(localInterpState:threadLocalInterp,c:whileDoCode):Expr := (
     while true do (
	  p := eval(localInterpState,c.predicate);
	  when p is err:Error 
	  do return if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else p
	  else if p == True then (
	       b := eval(localInterpState,c.doClause);
	       when b is err:Error 
	       do if err.message == continueMessage then nothing
	       else return if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else b 
	       else nothing;
	       )
	  else if p == False then break
	  else return printErrorMessageE(c.predicate,"expected true or false"));
     nullE);

evalWhileListCode(localInterpState:threadLocalInterp,c:whileListCode):Expr := (
     n := 1;
     r := new Sequence len n do provide nullE;
     i := 0;
     while true do (
	  p := eval(localInterpState,c.predicate);
	  when p is err:Error
	  do return if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else p
	  else if p == True then (
	       b := eval(localInterpState,c.listClause);
	       useb := true;
	       when b is err:Error 
	       do if err.message == continueMessage then useb = false
	       else if err.message == continueMessageWithArg then b = err.value
	       else if err.message == breakMessage then return (
		    if err.value == dummyExpr then Expr(
			 list(
			      if i == 0 then emptySequence
			      else if i == length(r) then r
			      else new Sequence len i do foreach x in r do provide x))
		    else err.value)
	       else return b
	       else nothing;
	       if useb then (
		    if i == n then (
			 n = 2*n;
			 r = new Sequence len n do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    ))
	  else if p == False then break
	  else return printErrorMessageE(c.predicate,"expected true or false"));
     Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == n then r
	       else new Sequence len i do foreach x in r do provide x)));

evalWhileListDoCode(localInterpState:threadLocalInterp,c:whileListDoCode):Expr := (
     r := new Sequence len 1 do provide nullE;
     i := 0;
     while true do (
	  p := eval(localInterpState,c.predicate);
	  when p is err:Error do (
	       return if err.message == breakMessage then (
		    if err.value == dummyExpr then Expr(
			 list(
			      if i == 0 then emptySequence
			      else if i == length(r) then r
			      else new Sequence len i do foreach x in r do provide x))
		    else err.value)
	       else p)
	  else if p == True then (
	       b := eval(localInterpState,c.listClause);
	       useb := true;
	       when b is err:Error
	       do if err.message == continueMessage then useb = false
	       else if err.message == continueMessageWithArg then b = err.value
	       else if err.message == breakMessage then return (
		    if err.value == dummyExpr then Expr(
			 list(
			      if i == 0 then emptySequence
			      else if i == length(r) then r
			      else new Sequence len i do foreach x in r do provide x))
		    else err.value)
	       else return b
	       else nothing;
	       if useb then (
		    if i == length(r) then (
			 r = new Sequence len 2*length(r) do (
			      foreach x in r do provide x;
			      while true do provide nullE;
			      );
			 );
		    r.i = b;
		    i = i+1;
		    );
	       d := eval(localInterpState,c.doClause);
	       when d is err:Error
	       do return if err.message == breakMessage then (
		    if err.value == dummyExpr then Expr(
			 list(
			      if i == 0 then emptySequence
			      else if i == length(r) then r
			      else new Sequence len i do foreach x in r do provide x))
		    else err.value)
	       else d
	       else nothing;
	       )
	  else if p == False then break
	  else return printErrorMessageE(c.predicate,"expected true or false"));
     Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == length(r) then r
	       else new Sequence len i do foreach x in r do provide x)));

evalForCode(localInterpState:threadLocalInterp,c:forCode):Expr := (
     r := if c.listClause == dummyCode then emptySequence else new Sequence len 1 do provide nullE;
     i := 0;				    -- index in r
     j := 0;				    -- the value of the loop variable if it's an integer loop, else the index in the list if it's "for i in w ..."
     w := emptySequence;				    -- the list x when it's "for i in w ..."
     n := 0;				    -- the upper bound on j, if there is a toClause.
     listLoop := false;
     toLimit := false;
     if c.inClause != dummyCode then (
     	  listLoop = true;
	  invalue := eval(localInterpState,c.inClause);
	  when invalue is Error do return invalue
	  is ww:Sequence do w = ww
	  is vv:List do w = vv.v
	  else return printErrorMessageE(c.inClause,"expected a list or sequence");	  
	  )
     else (
	  if c.fromClause != dummyCode then (
	       fromvalue := eval(localInterpState,c.fromClause);
	       when fromvalue 
	       is Error do return fromvalue
	       is f:ZZ do (
		    if isInt(f) then j = toInt(f)
		    else return printErrorMessageE(c.fromClause,"expected a small integer"))
	       else return printErrorMessageE(c.fromClause,"expected an integer"));
	  if c.toClause != dummyCode then (
	       toLimit = true;
	       tovalue := eval(localInterpState,c.toClause);
	       when tovalue 
	       is Error do return tovalue
	       is f:ZZ do (
		    if isInt(f) then n = toInt(f)
		    else return printErrorMessageE(c.toClause,"expected a small integer"))
	       else return printErrorMessageE(c.toClause,"expected an integer"));
	  );
     localInterpState.localFrame = Frame(localInterpState.localFrame,c.frameID,c.framesize,false,new Sequence len c.framesize do provide nullE);
     while true do (
	  if toLimit && j > n then break;
	  if listLoop && j >= length(w) then break;
	  localInterpState.localFrame.values.0 = if listLoop then w.j else Expr(toInteger(j));		    -- should be the frame spot for the loop var
	  j = j+1;
	  if c.whenClause != dummyCode then (
	       p := eval(localInterpState,c.whenClause);
	       when p is err:Error do (
		    localInterpState.localFrame = localInterpState.localFrame.outerFrame;
		    return if err.message == breakMessage then (
			 if err.value == dummyExpr then (
			      if c.listClause == dummyCode then nullE
			      else Expr(
				   list(
					if i == 0 then emptySequence
					else if i == length(r) then r
					else new Sequence len i do foreach x in r do provide x)))
			 else err.value)
		    else p)
	       else if p == False then break
	       else if p != True then (
		    localInterpState.localFrame = localInterpState.localFrame.outerFrame;
		    return printErrorMessageE(c.whenClause,"expected true or false")));
	  if c.listClause != dummyCode then (
	       b := eval(localInterpState,c.listClause);
	       useb := true;
	       when b is err:Error
	       do if err.message == continueMessage then useb = false
	       else if err.message == continueMessageWithArg then b = err.value
	       else (
		    if err.message == breakMessage then b = (
			 if err.value == dummyExpr 
			 then Expr(
			      list(
				   if i == 0 then emptySequence
				   else if i == length(r) then r
				   else new Sequence len i do foreach x in r do provide x))
			 else err.value);
		    localInterpState.localFrame = localInterpState.localFrame.outerFrame;
		    return b;
		    )
	       else nothing;
	       if useb then (
		    if i == length(r) then (
			 r = new Sequence len 2*length(r) do (
			      foreach x in r do provide x;
			      while true do provide nullE));
		    r.i = b;
		    i = i+1;
		    ));
	  if c.doClause != dummyCode then (
	       b := eval(localInterpState,c.doClause);
	       when b is err:Error do (
		    if err.message != continueMessage then (
			 localInterpState.localFrame = localInterpState.localFrame.outerFrame;
			 return
			 if err.message == breakMessage then (
			      if err.value == dummyExpr then (
				   if c.listClause == dummyCode then nullE
				   else Expr(
					list(
					     if i == 0 then emptySequence
					     else if i == length(r) then r
					     else new Sequence len i do foreach x in r do provide x)))
			      else err.value)
		    	 else b))
	       else nothing));
     localInterpState.localFrame = localInterpState.localFrame.outerFrame;
     if c.listClause == dummyCode then nullE
     else Expr(
	  list(
	       if i == 0 then emptySequence
	       else if i == length(r) then r
	       else new Sequence len i do foreach x in r do provide x)));

export evalSequence(localInterpState:threadLocalInterp,v:CodeSequence):Sequence := (
     evalSequenceHadError = false;
     n := length(v);
     if n == 0 then emptySequence
     else if n == 1 then (
	  x := eval(localInterpState,v.0);
	  when x is Error do (
	       evalSequenceHadError = true;
	       evalSequenceErrorMessage = x;
	       emptySequence
	       )
	  else Sequence(x)
	  )
     else if n == 2 then (
	  x := eval(localInterpState,v.0);
	  when x is Error do (
	       evalSequenceHadError = true;
	       evalSequenceErrorMessage = x;
	       emptySequence
	       )
	  else (
	       y := eval(localInterpState,v.1);
	       when y is Error do (
		    evalSequenceHadError = true;
		    evalSequenceErrorMessage = y;
		    emptySequence
		    )
	       else Sequence(x,y)))
     else (
	  r := new Sequence len n do (
		    foreach c in v do (
			 value := eval(localInterpState,c);
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

export binarymethod(localInterpState:threadLocalInterp,left:Expr,rhs:Code,methodkey:SymbolClosure):Expr;
export applyFCCS(localInterpState:threadLocalInterp,c:FunctionClosure,cs:CodeSequence):Expr;

export applyFCS(localInterpState:threadLocalInterp,c:FunctionClosure,v:Sequence):Expr := (
     previousFrame := c.frame;
     model := c.model;
     desc := model.desc;
     framesize := desc.framesize;
     if recursionDepth > recursionLimit then RecursionLimit()
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
	       saveLocalFrame := localInterpState.localFrame;
	       localInterpState.localFrame = f;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(localInterpState,model.body);			    -- do tail recursion here
	       recursionDepth = recursionDepth - 1;
	       localInterpState.localFrame = saveLocalFrame;
	       if !f.notrecyclable then (
	       	    foreach x in f.values do x = nullE;
		    f.outerFrame = recycleBin.framesize;
		    f.frameID = -2;				    -- just to be tidy, not really needed
		    recycleBin.framesize = f;
		    );
	       when ret is err:Error do returnFromFunction(ret) else ret
	       )
	  else (
	       f := Frame(previousFrame,desc.frameID,framesize,false,
		    new Sequence len framesize do (
			 provide v;
			 while true do provide nullE));
	       saveLocalFrame := localInterpState.localFrame;
	       localInterpState.localFrame = f;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(localInterpState,model.body);
	       recursionDepth = recursionDepth - 1;
	       localInterpState.localFrame = saveLocalFrame;
	       when ret is err:Error do returnFromFunction(ret) else ret
	       )
	  )
     else if desc.numparms != length(v)
     then WrongNumArgs(model.arrow,desc.numparms,length(v))
     else (
	  if framesize == 0 then (
	       saveLocalFrame := localInterpState.localFrame;
	       localInterpState.localFrame = previousFrame;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(localInterpState,model.body);
	       recursionDepth = recursionDepth - 1;
	       localInterpState.localFrame = saveLocalFrame;
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
			 foreach x at i in v do f.values.i = x; -- f.values can be NULL here!!
			 );
		    saveLocalFrame := localInterpState.localFrame;
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    localInterpState.localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    when ret is err:Error do returnFromFunction(ret) else ret
		    )
	       else (
		    saveLocalFrame := localInterpState.localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      foreach x in v do provide x;
			      while true do provide nullE));
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    localInterpState.localFrame = saveLocalFrame;
		    when ret is err:Error do returnFromFunction(ret) else ret
		    )
	       )
	  )
     );

wrongModel1(model:functionCode):Expr := printErrorMessageE(
     model.arrow, 
     "expected " + tostring(model.desc.numparms) + " argument" + (if model.desc.numparms == 1 then "" else "s") + " but got 1"
     );

export applyFCC(localInterpState:threadLocalInterp,fc:FunctionClosure,ec:Code):Expr := (
     if recursionDepth > recursionLimit then return RecursionLimit();
     previousFrame := fc.frame;
     model := fc.model;
     desc := model.desc;
     framesize := desc.framesize;
     when ec is cs:sequenceCode do applyFCCS(localInterpState,fc,cs.x)
     else (
	  recursionDepth = recursionDepth + 1;
	  e := eval(localInterpState,ec);
	  recursionDepth = recursionDepth - 1;
	  when e is Error do return e 
	  is v:Sequence do applyFCS(localInterpState,fc,v)
	  else (
	       -- this is the case where the argument is one Expr, not a Sequence
	       if desc.numparms != 1 then return wrongModel1(model);
	       saveLocalFrame := localInterpState.localFrame;
	       if framesize < length(recycleBin) then (
		    f := recycleBin.framesize;
		    if f == f.outerFrame then (		    -- self-pointer distinguishes the last one
			 f = Frame(previousFrame,desc.frameID,framesize,false, new Sequence len framesize do ( provide e; while true do provide nullE));
			 )
		    else (
			 recycleBin.framesize = f.outerFrame;
			 f.outerFrame = previousFrame;
			 f.frameID = desc.frameID;
			 f.values.0 = e;
			 );
		    ret := nullE;
		    while true do (
			 localInterpState.localFrame = f;
	  		 recursionDepth = recursionDepth + 1;
			 tailCode := evalAllButTail(localInterpState,model.body);
	  		 recursionDepth = recursionDepth - 1;
			 -- formerly, just ret := eval(model.body); now do tail recursion instead
			 when tailCode
			 is e:Error do (
			      ret = Expr(e);
			      break;)
			 is b:adjacentCode do (			    -- this bit as in eval below, except for the tail recursion case
			      left := eval(localInterpState,b.lhs);
			      when left
			      is c2:FunctionClosure do (
				   rhs := b.rhs;
				   when rhs is cs:sequenceCode do (
					recursionDepth = recursionDepth + 1;
					ret = applyFCCS(localInterpState,c2,cs.x);
					recursionDepth = recursionDepth - 1;
					break)
				   else (
					recursionDepth = recursionDepth + 1;
					e = eval(localInterpState,rhs);
					recursionDepth = recursionDepth - 1;
					when e is Error do (
					     ret = e;
					     break;)
					is v:Sequence do (
				   	     recursionDepth = recursionDepth + 1;
					     ret = applyFCS(localInterpState,c2,v);
				   	     recursionDepth = recursionDepth - 1;
					     break)
					else (			    -- here is the tail recursion
					     -- get a new local frame, if necessary -- examine the new FunctionClosure to see
					     -- this repeats the code at the opening of this function.
					     previousFrame = c2.frame;
					     model = c2.model;
					     desc = model.desc;
					     if desc.numparms != 1 then return wrongModel1(model);
					     if desc.framesize > framesize || f.notrecyclable then (
						  -- get or make a new frame
						  framesize = desc.framesize;
						  if framesize < length(recycleBin) && recycleBin.framesize.outerFrame != recycleBin.framesize then (
						       -- use a stashed one
						       f = recycleBin.framesize;
						       recycleBin.framesize = f.outerFrame;
						       f.outerFrame = previousFrame;
						       f.frameID = desc.frameID;
						       f.values.0 = e;)
						  else (		    -- self-pointer distinguishes the last one, so ignore it and make a new one
						       f = Frame(previousFrame,desc.frameID,framesize,false, new Sequence len framesize do ( provide e; while true do provide nullE));
						       ))
					     else (
						  -- clean up the old one
						  -- leave framesize as is, the actual size of f
						  foreach x in f.values do x = nullE;
						  f.outerFrame = previousFrame;
						  f.frameID = desc.frameID;
						  f.values.0 = e;
						  );
					     -- was apply(c2,e), before
					     -- no break, looping
					     )))
			      is ff:CompiledFunction do (
				   recursionDepth = recursionDepth + 1;
				   z := eval(localInterpState,b.rhs);
				   recursionDepth = recursionDepth - 1;
				   when z is Error do (
					ret = z;
					break)
				   else (
				   	recursionDepth = recursionDepth + 1;
					ret = ff.fn(localInterpState,z);
				   	recursionDepth = recursionDepth - 1;
					break))
			      is ff:CompiledFunctionClosure do (
				   recursionDepth = recursionDepth + 1;
				   z := eval(localInterpState,b.rhs);
				   recursionDepth = recursionDepth - 1;
				   when z is Error do (
					ret = z;
					break)
				   else (
		    	      		recursionDepth = recursionDepth + 1;
					ret = ff.fn(localInterpState,z,ff.env);
		    	      		recursionDepth = recursionDepth - 1;
					break))
			      is s:SpecialExpr do (
				   z := eval(localInterpState,b.rhs);
				   when z is Error do (
					ret = z;
					break)
				   else (
		    	      		recursionDepth = recursionDepth + 1;
					ret = applyEE(localInterpState,s.e,z);
		    	      		recursionDepth = recursionDepth - 1;
					break))
			      is Error do (
				   ret = left;
				   break)
			      else (
				   ret = binarymethod(localInterpState,left,b.rhs,AdjacentS);
				   break))
			 else (
		    	      recursionDepth = recursionDepth + 1;
			      ret = eval(localInterpState,tailCode);
		    	      recursionDepth = recursionDepth - 1;
			      break));
		    localInterpState.localFrame = saveLocalFrame;
		    if !f.notrecyclable && framesize < length(recycleBin) then (
			 -- clean it and recycle it
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    when ret is err:Error do returnFromFunction(ret) else ret -- this check takes time, too!
		    )
	       else (
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e;
			      while true do provide nullE));
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);			    -- don't bother with tail recursion here -- 21 arguments hardly ever happen!
		    recursionDepth = recursionDepth - 1;
		    localInterpState.localFrame = saveLocalFrame;
		    when ret is err:Error do returnFromFunction(ret) else ret -- this check takes time, too!
		    ))));
export applyFCE(localInterpState:threadLocalInterp,fc:FunctionClosure,e:Expr):Expr := (
     -- single argument 'e' provided not a sequence, so framesize > 0 -- or should we check, for safety??
     if recursionDepth > recursionLimit then return RecursionLimit();
     previousFrame := fc.frame;
     model := fc.model;
     desc := model.desc;
     framesize := desc.framesize;
     if desc.numparms != 1 then return wrongModel1(model);
     saveLocalFrame := localInterpState.localFrame;
     if framesize < length(recycleBin) then (
	  f := recycleBin.framesize;
	  if f == f.outerFrame then (		    -- self-pointer distinguishes the last one
	       f = Frame(previousFrame,desc.frameID,framesize,false, new Sequence len framesize do ( provide e; while true do provide nullE));
	       )
	  else (
	       recycleBin.framesize = f.outerFrame;
	       f.outerFrame = previousFrame;
	       f.frameID = desc.frameID;
	       f.values.0 = e;
	       );
	  localInterpState.localFrame = f;
     	  recursionDepth = recursionDepth + 1;
	  ret := eval(localInterpState,model.body);
     	  recursionDepth = recursionDepth - 1;
	  localInterpState.localFrame = saveLocalFrame;
	  if !f.notrecyclable then (
	       -- clean it and recycle it
	       foreach x in f.values do x = nullE;
	       f.outerFrame = recycleBin.framesize;
	       f.frameID = -2;				    -- just to be tidy, not really needed
	       recycleBin.framesize = f;
	       );
	  when ret is err:Error do returnFromFunction(ret) else ret -- this check takes time, too!
	  )
     else (
	  f := Frame(previousFrame,desc.frameID,framesize,false,
	       new Sequence len framesize do (
		    provide e;
		    while true do provide nullE));
     	  localInterpState.localFrame = f;
     	  recursionDepth = recursionDepth + 1;
	  ret := eval(localInterpState,model.body);			    -- don't bother with tail recursion here -- 21 arguments hardly ever happen!
     	  recursionDepth = recursionDepth - 1;
	  localInterpState.localFrame = saveLocalFrame;
	  when ret is err:Error do returnFromFunction(ret) else ret -- this check takes time, too!
	  )
     );
export applyFCCS(localInterpState:threadLocalInterp,c:FunctionClosure,cs:CodeSequence):Expr := (
     -- in this version we try to avoid allocating an array of exprs to
     -- hold the parameters, preferring to stuff them into the frame
     -- directly
     model := c.model;
     desc := model.desc;
     if recursionDepth > recursionLimit then RecursionLimit()
     else if desc.restargs
     then (
	  recursionDepth = recursionDepth + 1;
	  v := evalSequence(localInterpState,cs);
	  recursionDepth = recursionDepth - 1;
	  if evalSequenceHadError then evalSequenceErrorMessage else applyFCS(localInterpState,c,v)
	  )
     else if desc.numparms != length(cs)
     then WrongNumArgs(model.arrow,desc.numparms,length(cs))
     else (
     	  previousFrame := c.frame;
     	  framesize := desc.framesize;
	  if framesize == 0 then (
	       saveLocalFrame := localInterpState.localFrame;
	       localInterpState.localFrame = previousFrame;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(localInterpState,model.body);			    -- do tail recursion here
	       recursionDepth = recursionDepth - 1;
	       when ret is err:Error do ret = returnFromFunction(ret)
	       else nothing;
	       localInterpState.localFrame = saveLocalFrame;
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
					codevalue := eval(localInterpState,code);
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
			      codevalue := eval(localInterpState,code);
			      when codevalue
			      is Error do return codevalue
			      else f.values.i = codevalue;
			      );
			 );
		    saveLocalFrame := localInterpState.localFrame;
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret)
		    else nothing;
		    localInterpState.localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    saveLocalFrame := localInterpState.localFrame;
		    haderror := false;
		    recursionDepth = recursionDepth + 1;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      foreach code in cs do (
				   codevalue := eval(localInterpState,code);
				   when codevalue 
				   is Error do (
					haderror = true;
					errorreturn = codevalue;
					while true do provide nullE;
					)
				   else provide codevalue;
				   );
			      while true do provide nullE));
		    recursionDepth = recursionDepth - 1;
		    if haderror then return errorreturn;
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret)
		    else nothing;
		    localInterpState.localFrame = saveLocalFrame;
		    ret
		    )
	       )
	  )
     );
export applyES(localInterpState:threadLocalInterp,f:Expr,v:Sequence):Expr := (
     when f
     is ff:CompiledFunction do ff.fn(localInterpState,Expr(v))
     is ff:CompiledFunctionClosure do ff.fn(localInterpState,Expr(v),ff.env)
     is c:FunctionClosure do applyFCS(localInterpState,c,v)
     is s:SpecialExpr do applyES(localInterpState,s.e,v)
     else buildErrorPacket("expected a function"));
export applyEE(localInterpState:threadLocalInterp,f:Expr,e:Expr):Expr := (
     when f
     is ff:CompiledFunction do ff.fn(localInterpState,e)
     is ff:CompiledFunctionClosure do ff.fn(localInterpState,e,ff.env)
     is c:FunctionClosure do (
	  when e
	  is v:Sequence do applyFCS(localInterpState,c,v)
	  else applyFCE(localInterpState,c,e)
	  )
     is s:SpecialExpr do applyEE(localInterpState,s.e,e)
     else buildErrorPacket("expected a function"));
--------
-- could optimize later
export applyEEE(localInterpState:threadLocalInterp,g:Expr,e0:Expr,e1:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1)))
     when g
     is ff:CompiledFunction do ff.fn(localInterpState,Expr(Sequence(e0,e1)))
     is ff:CompiledFunctionClosure do ff.fn(localInterpState,Expr(Sequence(e0,e1)),ff.env)
     is s:SpecialExpr do applyEEE(localInterpState,s.e,e0,e1)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then applyFCS(localInterpState,c,Sequence(e0,e1))
	  else if desc.numparms != 2
	  then WrongNumArgs(model.arrow,desc.numparms,2)
	  else if recursionDepth > recursionLimit then RecursionLimit()
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
		    saveLocalFrame := localInterpState.localFrame;
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localInterpState.localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    saveLocalFrame := localInterpState.localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      while true do provide nullE));
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localInterpState.localFrame = saveLocalFrame;
		    ret
		    )
	       )
	  )
     else buildErrorPacket("expected a function"));     
export applyEEE(localInterpState:threadLocalInterp,g:Expr,e0:Expr,e1:Expr,e2:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1,e2)));
     when g
     is ff:CompiledFunction do ff.fn(localInterpState,Expr(Sequence(e0,e1,e2)))
     is ff:CompiledFunctionClosure do ff.fn(localInterpState,Expr(Sequence(e0,e1,e2)),ff.env)
     is s:SpecialExpr do applyEEE(localInterpState,s.e,e0,e1,e2)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then applyFCS(localInterpState,c,Sequence(e0,e1,e2))
	  else if desc.numparms != 3
	  then WrongNumArgs(model.arrow,desc.numparms,3)
	  else if recursionDepth > recursionLimit then RecursionLimit()
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
		    saveLocalFrame := localInterpState.localFrame;
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localInterpState.localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret)
	       else (
		    saveLocalFrame := localInterpState.localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      provide e2;
			      while true do provide nullE));
		    localInterpState.localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(localInterpState,model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localInterpState.localFrame = saveLocalFrame;
		    ret)))
     else buildErrorPacket("expected a function"));     

-----------------------------------------------------------------------------

export unarymethod(localInterpState:threadLocalInterp,rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(localInterpState,rhs);
     when right is Error do right
     else (
	  method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
	  if method == nullE then MissingMethod(methodkey)
	  else applyEE(localInterpState,method,right)));
export binarymethod(localInterpState:threadLocalInterp,lhs:Code,rhs:Code,methodkey:SymbolClosure):Expr := (
     left := eval(localInterpState,lhs);
     when left is Error do left
     else (
	  right := eval(localInterpState,rhs);
	  when right is Error do right
	  else (
	       method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),
		    methodkey.symbol.hash);
	       if method == nullE then MissingMethodPair(methodkey,left,right)
	       else applyEEE(localInterpState,method,left,right))));
export binarymethod(localInterpState:threadLocalInterp,left:Expr,rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(localInterpState,rhs);
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
	  else applyEEE(localInterpState,method,left,right)));

-----------------------------------------------------------------------------

globalAssignmentHook(localInterpState:threadLocalInterp,t:Symbol,oldvalue:Expr,newvalue:Expr):Expr := (
     method := lookup(Class(oldvalue),GlobalReleaseE);
     sym := Expr(SymbolClosure(globalFrame,t));
     -- top level hooks for assignment to a global variable
     g := lookup1(globalAssignmentHooks,sym);
     if g != notfoundE then (
	  y := when g
	  is s:List do (
	       foreach f in s.v do (
		    r := applyEEE(localInterpState,f,sym,newvalue);
		    when r is Error do return r else nothing;
		    );
	       nullE)
	  is f:CompiledFunction do applyEEE(localInterpState,g,sym,newvalue)
	  is f:CompiledFunctionClosure do applyEEE(localInterpState,g,sym,newvalue)
	  is f:FunctionClosure do applyEEE(localInterpState,g,sym,newvalue)
	  is f:SpecialExpr do applyEEE(localInterpState,f.e,sym,newvalue)
	  else buildErrorPacket("expected global assignment hook for " + t.word.name + " to be a function or list of functions");
	  when y is Error do return y else nothing;
	  );
     if method != nullE then (
	  y := applyEEE(localInterpState,method,sym,oldvalue);
	  when y is Error do return y else nothing;
	  );
     method = lookup(Class(newvalue),GlobalAssignE);
     if method != nullE then (
	  y := applyEEE(localInterpState,method,sym,newvalue);
	  when y is Error do return y else nothing;
	  );
     nullE
     );

localAssignment(localInterpState:threadLocalInterp,nestingDepth:int,frameindex:int,newvalue:Expr):Expr := ( -- frameID != 0
     f := localInterpState.localFrame;
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

globalAssignment(localInterpState:threadLocalInterp,frameindex:int,t:Symbol,newvalue:Expr):Expr := ( -- frameID = 0
     if t.protected then return buildErrorPacket("assignment to protected variable '" + t.word.name + "'");
     vals := globalFrame.values;
     r := globalAssignmentHook(localInterpState,t,vals.frameindex,newvalue);
     when r is Error do return r else nothing;
     vals.frameindex = newvalue;
     newvalue);

assignment(localInterpState:threadLocalInterp,nestingDepth:int,frameindex:int,t:Symbol,newvalue:Expr):Expr := (
     if nestingDepth == -1
     then globalAssignment(localInterpState,frameindex,t,newvalue)
     else localAssignment(localInterpState,nestingDepth,frameindex,newvalue));

globalAssignmentFun(localInterpState:threadLocalInterp,x:globalAssignmentCode):Expr := (
     t := x.lhs;
     newvalue := eval(localInterpState,x.rhs);
     when newvalue is Error do return newvalue else nothing;
     globalAssignment(localInterpState,t.frameindex,t,newvalue));

parallelAssignmentFun(localInterpState:threadLocalInterp,x:parallelAssignmentCode):Expr := (
     syms := x.lhs;
     nestingDepth := x.nestingDepth;
     frameindex := x.frameindex;
     nlhs := length(frameindex);
     foreach sym in syms do if sym.protected then return buildErrorPacket("assignment to protected variable '" + sym.word.name + "'");
     value := eval(localInterpState,x.rhs);
     when value 
     is Error do return value 
     is values:Sequence do 
     if nlhs == length(values) then (
	  for i from 0 to nlhs-1 do (
	       r := assignment(localInterpState,nestingDepth.i,frameindex.i,syms.i,values.i);
	       when r is Error do return r else nothing;
	       );
	  value
	  )
     else buildErrorPacket("parallel assignment: expected a sequence of " + tostring(nlhs) + " values")
     else buildErrorPacket("parallel assignment: expected a sequence of " + tostring(nlhs) + " values"));

-----------------------------------------------------------------------------

export clearAlarm():void := (
     alarm(uint(0));
     );
export clearAllFlags():void := (
     exceptionFlag = false;
     interruptedFlag = false;
     steppingFlag = false;
     interruptflag = 0;					    -- libfac
     alarmedFlag = false;
     interruptPending = false;
     );
export setInterruptFlag():void := (
     interruptedFlag = true;
     exceptionFlag = true;
     );
export setAlarmedFlag():void := (
     interruptedFlag = true;				    -- an alarm is an interrupt, as far as the engine is concerned
     alarmedFlag = true;
     exceptionFlag = true;
     );
export setSteppingFlag():void := (
     steppingFlag = true;
     exceptionFlag = true;
     );
export clearInterruptFlag():void := (
     interruptedFlag = false;
     determineExceptionFlag();
     );
export clearAlarmedFlag():void := (
     interruptedFlag = false;
     alarmedFlag = false;
     determineExceptionFlag();
     );
export clearSteppingFlag():void := (
     stepCount = -1;
     microStepCount = -1;
     steppingFlag = false;
     determineExceptionFlag();
     );
steppingFurther(c:Code):bool := steppingFlag && (
     p := codePosition(c);
     if p == dummyPosition || p.loadDepth < errorDepth then return true;
     if stepCount >= 0 then (
	  if lastCodePosition.filename != p.filename
	  || lastCodePosition.line != p.line
	  then (
     	       stepCount = stepCount - 1;
     	       lastCodePosition.filename = p.filename;
	       lastCodePosition.line = p.line;
	       if debugLevel == 1001 && stepCount >= 0 then printErrorMessage(p,"--evaluating: "+present(tostring(c)));
	       );
	  stepCount >= 0)
     else if microStepCount >= 0 then (
     	  if lastCode != c then (
	       microStepCount = microStepCount - 1;
	       lastCode = c;
	       if microStepCount >= 0 then printErrorMessage(p,"--evaluating: "+present(tostring(c)));
	       );
	  microStepCount >= 0)
     else false);

handleError(localInterpState:threadLocalInterp,c:Code,e:Expr):Expr := (
     when e is err:Error do (
	  if SuppressErrors then return e;
	  if err.message == returnMessage
	  || err.message == continueMessage || err.message == continueMessageWithArg
	  || err.message == stepMessage || err.message == stepMessageWithArg
	  || err.message == breakMessage
	  || err.message == unwindMessage
	  || err.message == throwMessage
	  then (
	       -- an error message that is really being used to transfer control must be passed up the line
	       -- the position is plugged in just in case it's unhandled
	       if err.position == dummyPosition then err.position = codePosition(c);
	       return e;
	       );
	  p := codePosition(c);
	  clearAllFlags();
	  clearAlarm();
	  if p.loadDepth >= errorDepth && !err.position === p then (
	       oldReportFrame := err.frame;
	       err.frame = noRecycle(localInterpState.localFrame);
	       err.position = p;
	       if !err.printed || backtrace && localInterpState.localFrame != oldReportFrame then (
		    if debuggingMode && !stopIfError && (! (p.filename === "stdio")) then (
			 if !err.printed then printError(err);
			 printErrorMessage(err.position,"--entering debugger (type help to see debugger commands)");
			 z := debuggerFun(localInterpState,localInterpState.localFrame,c);
			 -- printErrorMessage(err.position,"--leaving debugger");
			 when z is z:Error do (
			      if z.message == breakMessage then buildErrorPacket(unwindMessage)
			      else if z.message == returnMessage then (
				   setSteppingFlag();
     	       	    	      	   lastCodePosition.filename = "";
				   z.value)
			      else if z.message == stepMessageWithArg || z.message == stepMessage then (
				   setSteppingFlag();
     	       	    	      	   lastCodePosition.filename = "";
				   stepCount = (
					when z.value is step:ZZ do
					if isInt(step) then toInt(step) else 1
					else 1);
				   if stepCount < 0 then (
					microStepCount = - stepCount;
					stepCount = -1;
					);
				   eval(localInterpState,c))
			      else if z.message == continueMessage then eval(localInterpState,c)
			      else e)
			 else e)
		    else (
			 printError(err);
			 e))
	       else e)
	  else e)
     else e);

export eval(localInterpState:threadLocalInterp,c:Code):Expr := (
     e := (
	  if exceptionFlag && !steppingFurther(c) then (    -- compare this code to the code in evalexcept() below
	       if steppingFlag then (
		    clearSteppingFlag();
		    buildErrorPacket(steppingMessage))
	       else if alarmedFlag then (
		    clearAlarmedFlag();
		    buildErrorPacket(alarmMessage))
	       else if interruptedFlag then (
		    SuppressErrors = false;
		    clearInterruptFlag();
		    buildErrorPacket(interruptMessage))
	       else (
		    SuppressErrors = false;
		    clearAllFlags();
		    buildErrorPacket("unknown exception")
		    ))
	  else when c
	  is u:unaryCode do u.f(localInterpState,u.rhs)
	  is b:binaryCode do b.f(localInterpState,b.lhs,b.rhs)
	  is b:adjacentCode do (
	       left := eval(localInterpState,b.lhs);
	       when left
	       is fc:FunctionClosure do applyFCC(localInterpState,fc,b.rhs)
	       is ff:CompiledFunction do (
		    z := eval(localInterpState,b.rhs);
		    when z is Error do z
		    else ff.fn(localInterpState,z))
	       is ff:CompiledFunctionClosure do (
		    z := eval(localInterpState,b.rhs);
		    when z is Error do z
		    else ff.fn(localInterpState,z,ff.env))
	       is s:SpecialExpr do (
		    when s.e
		    is fc:FunctionClosure do applyFCC(localInterpState,fc,b.rhs)
		    is ff:CompiledFunction do ( z := eval(localInterpState,b.rhs); when z is Error do z else ff.fn(localInterpState,z))
		    is ff:CompiledFunctionClosure do ( z := eval(localInterpState,b.rhs); when z is Error do z else ff.fn(localInterpState,z,ff.env))
     	       	    else binarymethod(localInterpState,left,b.rhs,AdjacentS))
	       is Error do left
	       else binarymethod(localInterpState,left,b.rhs,AdjacentS))
	  is m:functionCode do return Expr(FunctionClosure(noRecycle(localInterpState.localFrame),m))
	  is r:localMemoryReferenceCode do (
	       f := localInterpState.localFrame;
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
	       newvalue := eval(localInterpState,x.rhs);
	       when newvalue is Error do return newvalue 
	       else localAssignment(localInterpState,x.nestingDepth,x.frameindex,newvalue))
	  is a:globalAssignmentCode do globalAssignmentFun(localInterpState,a)
	  is p:parallelAssignmentCode do parallelAssignmentFun(localInterpState,p)
	  is c:globalSymbolClosureCode do return Expr(SymbolClosure(globalFrame,c.symbol))
	  is c:tryCode do (
	       oldSuppressErrors := SuppressErrors;
	       SuppressErrors = true;
	       p := eval(localInterpState,c.code);
	       if !SuppressErrors then p		  -- eval could have turned it off
	       else (
		    SuppressErrors = oldSuppressErrors;
		    when p is err:Error do (
			 if err.message == breakMessage || err.message == returnMessage || 
			 err.message == continueMessage || err.message == continueMessageWithArg || 
			 err.message == unwindMessage || err.message == throwMessage
			 then p
			 else eval(localInterpState,c.elseClause))
		    else if c.thenClause == NullCode then p else eval(localInterpState,c.thenClause)))
	  is c:catchCode do (
	       p := eval(localInterpState,c.code);
	       when p is err:Error do if err.message == throwMessage then err.value else p
	       else p)
	  is c:ifCode do (
	       p := eval(localInterpState,c.predicate);
	       when p is Error do p
	       else if p == True then eval(localInterpState,c.thenClause)
	       else if p == False then eval(localInterpState,c.elseClause)
	       else printErrorMessageE(c.predicate,"expected true or false"))
	  is r:localSymbolClosureCode do (
	       f := localInterpState.localFrame;
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
	  is b:ternaryCode do b.f(localInterpState,b.arg1,b.arg2,b.arg3)
	  is b:multaryCode do b.f(localInterpState,b.args)
	  is n:newLocalFrameCode do (
	       localInterpState.localFrame = Frame(localInterpState.localFrame,n.frameID,n.framesize,false, new Sequence len n.framesize do provide nullE);
	       x := eval(localInterpState,n.body);
	       localInterpState.localFrame = localInterpState.localFrame.outerFrame;
	       x)
	  is c:forCode do return evalForCode(localInterpState,c)
	  is c:whileListDoCode do evalWhileListDoCode(localInterpState,c)
	  is c:whileDoCode do evalWhileDoCode(localInterpState,c)
	  is c:whileListCode do evalWhileListCode(localInterpState,c)
	  is c:newCode do NewFun(localInterpState,c.newClause)
	  is c:newOfCode do NewOfFun(localInterpState,c.newClause,c.ofClause)
	  is c:newFromCode do NewFromFun(localInterpState,c.newClause,c.fromClause)
	  is c:newOfFromCode do NewOfFromFun(localInterpState,c.newClause,c.ofClause,c.fromClause)
	  is nullCode do return nullE
	  is v:realCode do return Expr(v.x)
	  is v:integerCode do return Expr(v.x)
	  is v:stringCode do return Expr(v.x)
     	  is v:Error do Expr(v)
	  is v:semiCode do (
	       w := v.w;
	       n := length(w);				    -- at least 2
	       r := eval(localInterpState,w.0);
	       when r is Error do r else (
	       	    r = eval(localInterpState,w.1);
	       	    when r is Error do r else (
	       		 if n == 2 then return r;
	       		 r = eval(localInterpState,w.2);
	       		 when r is Error do r else (
			      if n == 3 then return r;
			      r = eval(localInterpState,w.3);
			      when r is Error do r else (
				   if n == 4 then return r;
				   r = eval(localInterpState,w.4);
				   when r is Error do r else (
					i := 5;
					while i < n do (
					     r = eval(localInterpState,w.i);
					     i = when r is Error do n else i+1;
					     );
					r))))))
	  is v:sequenceCode do (
	       if length(v.x) == 0 then return emptySequence;
	       r := evalSequence(localInterpState,v.x);
	       if evalSequenceHadError then evalSequenceErrorMessage else Expr(r)) -- speed up
	  is v:listCode do (
	       if length(v.y) == 0 then return emptyList;
	       r := evalSequence(localInterpState,v.y);
	       if evalSequenceHadError then evalSequenceErrorMessage else list(r))
	  is v:arrayCode do (
	       if length(v.z) == 0 then return emptyArray;
	       r := evalSequence(localInterpState,v.z);
	       if evalSequenceHadError then evalSequenceErrorMessage else Array(r)
	       ));
     when e is Error do handleError(localInterpState,c,e) else e);

export evalexcept(localInterpState:threadLocalInterp,c:Code):Expr := (
     e := eval(localInterpState,c);
     if exceptionFlag then (				    -- compare this code to the code at the top of eval() above
	  if alarmedFlag then (
	       clearAlarmedFlag();
	       printErrorMessageE(c,alarmMessage))
	  else if interruptedFlag then (
	       SuppressErrors = false;
	       clearInterruptFlag();
	       printErrorMessageE(c,interruptMessage))
	  else if steppingFlag then (
	       clearSteppingFlag();
	       printErrorMessageE(c,"--done stepping, returning to top level");
	       e)
	  else (
	       SuppressErrors = false;
	       clearAllFlags();
	       printErrorMessageE(c,"unknown exception")))
     else e);

export eval(localInterpState:threadLocalInterp,f:Frame,c:Code):Expr := (
     saveLocalFrame := localInterpState.localFrame;
     localInterpState.localFrame = f;
     ret := evalexcept(localInterpState,c);
     localInterpState.localFrame = saveLocalFrame;
     ret);

shieldfun(localInterpState:threadLocalInterp,a:Code):Expr := (
     if interruptShield then eval(localInterpState,a)
     else (
     	  interruptPending = interruptedFlag;
     	  interruptShield = true;
     	  ret := eval(localInterpState,a);
     	  interruptShield = false;
     	  interruptedFlag = interruptPending;
     	  determineExceptionFlag();
	  if interruptedFlag && !stdIO.inisatty then (
     	       endLine(stderr);
	       stderr << "interrupted" << endl;
	       exit(1);
	       );
     	  ret));
setupop(shieldS,shieldfun);     

returnFun(localInterpState:threadLocalInterp,a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(localInterpState,a);
     when e is Error do e else Expr(Error(dummyPosition,returnMessage,e,false,dummyFrame)));
setupop(returnS,returnFun);

throwFun(localInterpState:threadLocalInterp,a:Code):Expr := (
     e := eval(localInterpState,a);
     when e is Error do e else Expr(Error(dummyPosition,throwMessage,e,false,dummyFrame)));
setupop(throwS,throwFun);

continueFun(localInterpState:threadLocalInterp,a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(localInterpState,a);
     when e is Error do e else Expr(Error(dummyPosition,
	       if a == dummyCode then continueMessage else continueMessageWithArg,
	       e,false,dummyFrame)));
setupop(continueS,continueFun);

stepFun(localInterpState:threadLocalInterp,a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(localInterpState,a);
     when e is Error do e else (
	  Expr(Error(dummyPosition,
	       if a == dummyCode then stepMessage else stepMessageWithArg,
	       e,false,dummyFrame))));
setupop(stepS,stepFun);

breakFun(localInterpState:threadLocalInterp,a:Code):Expr := (
     e := if a == dummyCode then dummyExpr else eval(localInterpState,a);
     when e is Error do e else Expr(Error(dummyPosition,breakMessage,e,false,dummyFrame)));
setupop(breakS,breakFun);

assigntofun(localInterpState:threadLocalInterp,lhs:Code,rhs:Code):Expr := (
     left := eval(localInterpState,lhs);
     when left
     is q:SymbolClosure do (
	  if q.symbol.protected then (
	       buildErrorPacket("assignment to protected variable '" + q.symbol.word.name + "'")
	       )
	  else (
	       value := eval(localInterpState,rhs);
	       when value is Error do return value else nothing;
	       q.frame.values.(q.symbol.frameindex) = value;
	       value))
     is Error do left
     else (
	  method := lookup(Class(left),LeftArrowE); -- method for x <- y is looked up under (symbol <-, class x)
	  if method == nullE then buildErrorPacket("'<-': no method for object on left")
	  else (
	       value := eval(localInterpState,rhs);
	       when value is Error do return value else nothing;
	       applyEEE(localInterpState,method,left,value))));
setup(LeftArrowS,assigntofun);

idfun(localInterpState:threadLocalInterp,e:Expr):Expr := e;
setupfun("identity",idfun);
scanpairs(localInterpState:threadLocalInterp,f:Expr,obj:HashTable):Expr := (
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := applyEEE(localInterpState,f,p.key,p.value);
	       when v is Error do return v else nothing;
	       p = p.next;
	       ));
     nullE);
scanpairsfun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do
     if	       o.mutable
     then      WrongArg("an immutable hash table")
     else      scanpairs(localInterpState,a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("scanPairs",scanpairsfun);

mpre():Expr := buildErrorPacket("applyPairs: expected function to return null, a sequence of length 2, or an option x=>y");
mappairs(localInterpState:threadLocalInterp,f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := applyEEE(localInterpState,f,p.key,p.value);
	       when v 
	       is Error do return v 
	       is Nothing do nothing
	       is b:List do (
		    if b.class != optionClass then return mpre();
		    a := b.v;
		    if length(a) != 2 then return mpre();
		    when storeInHashTable(newobj,a.0,a.1) is e:Error do return Expr(e) else nothing)
	       is a:Sequence do (
		    if length(a) == 2
		    then (
			 ret := storeInHashTable(newobj,a.0,a.1);
			 when ret is Error do return ret else nothing;
			 )
		    else return mpre();
		    )
	       else return mpre();
	       p = p.next;
	       ));
     sethash(newobj,obj.mutable);
     Expr(newobj));
mappairsfun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do
     if        o.mutable 
     then      WrongArg("an immutable hash table")
     else      mappairs(localInterpState,a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyPairs",mappairsfun);

export mapkeys(localInterpState:threadLocalInterp,f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       newkey := applyEE(localInterpState,f,p.key);
	       if newkey == nullE then return buildErrorPacket("null key encountered"); -- remove soon!!!
	       when newkey is Error do return newkey else nothing;
	       storeInHashTableNoClobber(newobj,newkey,p.value);
	       p = p.next;
	       ));
     sethash(newobj,obj.mutable);
     Expr(newobj));
mapkeysfun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do        
     if        o.mutable
     then      WrongArg("an immutable hash table")
     else      mapkeys(localInterpState,a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyKeys",mapkeysfun);

export mapvalues(localInterpState:threadLocalInterp,f:Expr,obj:HashTable):Expr := (
     u := newHashTable(obj.class,obj.parent);
     hadError := false;
     errm := nullE;
     u.numEntries = obj.numEntries;
     u.table = new array(KeyValuePair) len length(obj.table) do (
	  foreach bucket in obj.table do (
	       p := bucket;
	       q := bucketEnd;
	       while p != p.next do (
		    newvalue := applyEE(localInterpState,f,p.value);
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
mapvaluesfun(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when      e is a:Sequence do
     if        length(a) == 2
     then when a.0 is o:HashTable 
     do        
     if        o.mutable
     then      WrongArg("an immutable hash table")
     else      mapvalues(localInterpState,a.1,o)
     else      WrongArg(1,"a hash table")
     else      WrongNumArgs(2)
     else      WrongNumArgs(2));
setupfun("applyValues",mapvaluesfun);

merge(localInterpState:threadLocalInterp,e:Expr):Expr := (
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
			      t := applyEEE(localInterpState,g,val,q.value);
			      when t is err:Error do (
			      	   if err.message != continueMessage then return t else remove(z,q.key);
			      	   )
			      else (
				   storeInHashTable(z,q.key,q.hash,t);
				   )
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
			      t := applyEEE(localInterpState,g,q.value,val);
			      when t is err:Error do (
			      	   if err.message != continueMessage then return t else remove(z,q.key);
			      	   )
			      else (
				   storeInHashTable(z,q.key,q.hash,t);
				   )
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
combine(localInterpState:threadLocalInterp,f:Expr,g:Expr,h:Expr,x:HashTable,y:HashTable):Expr := (
     z := newHashTable(x.class,x.parent);
     foreach pp in x.table do (
	  p := pp;
	  while p != p.next do (
	       foreach qq in y.table do (
		    q := qq;
		    while q != q.next do (
			 pqkey := applyEEE(localInterpState,f,p.key,q.key);
			 when pqkey 
			 is err:Error do (
			      if err.message != continueMessage then return pqkey;
			      )
			 else (
			      pqvalue := applyEEE(localInterpState,g,p.value,q.value);
			      when pqvalue
			      is err:Error do (
				   if err.message != continueMessage then return pqvalue else nothing;
				   )
			      else (
				   pqhash := hash(pqkey);
				   previous := lookup1(z,pqkey,pqhash);
				   if previous == notfoundE
				   then (
					r := storeInHashTable(z,pqkey,pqhash,pqvalue);
					when r is Error do return r else nothing;
					)
				   else (
					t := applyEEE(localInterpState,h,previous,pqvalue);
					when t is err:Error do (
					     if err.message == continueMessage
					     then remove(z,pqkey)
					     else return t;
					     )
					else (
					     r := storeInHashTable(z,pqkey,pqhash,t);
					     when r is Error do return r else nothing;
					     ))));
			 q = q.next);
		    );
	       p = p.next));
     sethash(z,x.mutable | y.mutable);
     z);
combine(localInterpState:threadLocalInterp,e:Expr):Expr := (
     when e
     is v:Sequence do
     if length(v) == 5 then 
     when v.0 is x:HashTable do
     if x.mutable then WrongArg(1,"an immutable hash table") else
     when v.1 is y:HashTable do
     if y.mutable then WrongArg(2,"an immutable hash table") else
     combine(localInterpState,v.2,v.3,v.4,x,y)
     else WrongArg(1+1,"a hash table")
     else WrongArg(0+1,"a hash table")
     else WrongNumArgs(5)
     else WrongNumArgs(5));
setupfun("combine",combine);


export unarymethod(localInterpState:threadLocalInterp,right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethod(methodkey)
     else applyEE(localInterpState,method,right));

export binarymethod(localInterpState:threadLocalInterp,left:Expr,right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethodPair(methodkey,left,right)
     else applyEEE(localInterpState,method,left,right));

export binarymethod(localInterpState:threadLocalInterp,left:Expr,right:Expr,methodkey:Expr,methodkeyname:string):Expr := (
     method := lookupBinaryMethod(Class(left),Class(right),methodkey,hash(methodkey));
     if method == nullE then MissingMethodPair(methodkeyname,left,right)
     else applyEEE(localInterpState,method,left,right));

AssignElemFun = assignelemfun;
AssignQuotedElemFun = assignquotedelemfun;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
