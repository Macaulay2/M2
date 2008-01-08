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
export eval(c:Code):Expr;
export applyEE(f:Expr,e:Expr):Expr;
export evalAllButTail(c:Code):Code := while true do c = (
     when c
     is i:ifCode do (
	  p := eval(i.predicate);
	  when p is e:Error do Code(e)
	  else if p == True then i.thenClause
	  else if p == False then i.elseClause
	  else (
	       return Code(Error(codePosition(i.predicate),"expected true or false",nullE,false,dummyFrame));
	       dummyCode))
     is v:semiCode do (
	  w := v.w;
	  n := length(w);				    -- at least 2
	  r := eval(w.0);
	  when r is e:Error do return Code(e) else nothing;
	  if n == 2 then w.1 else (
	       r = eval(w.1);
	       when r is e:Error do return Code(e) else nothing;
	       if n == 3 then w.2 else (
		    r = eval(w.2);
		    when r is e:Error do return Code(e) else nothing;
		    if n == 4 then w.3 else (
			 r = eval(w.3);
			 when r is e:Error do return Code(e) else nothing;
			 if n == 5 then w.4 else (
			      r = eval(w.4);
			      when r is e:Error do return Code(e) else nothing;
			      i := 5;
			      while i < n-1 do (
				   r = eval(w.i);
				   when r is e:Error do return Code(e) else i = i+1);
			      w.i)))))
     else (
	  return c;
	  dummyCode));
----------------------------------------------------------------------------------------
export RecursionLimit():Expr := buildErrorPacket("recursion limit of " + tostring(recursionLimit) + " exceeded");

export storeInHashTable(x:HashTable,i:Code,rhs:Code):Expr := (
     ival := eval(i);
     when ival is Error do ival else (
	  val := eval(rhs);
	  when val is Error do val else storeInHashTable(x,ival,val)));

export storeInDictionary(dc:DictionaryClosure,i:Code,rhs:Code):Expr := (
     ival := eval(i);
     when ival
     is newname:string do (
	  rhsval := eval(rhs);
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

assignvector(m:List,i:Code,rhs:Code):Expr := (
     x := m.v;
     ival := eval(i);
     when ival
     is j:ZZ do (
	  if isInt(j)
	  then (
	       k := toInt(j);
	       if k < -length(x) then k = k + length(x);
	       if k < 0 then return printErrorMessageE(i,"negative subscript out of bounds 0 .. "+tostring(length(x)-1));
	       val := eval(rhs);
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

dbmstore(f:Database,KEY:Code,CONTENT:Code):Expr := (
     Key := eval(KEY);
     when Key
     is Error do Key
     is key:string do (
	  Content := eval(CONTENT);
	  when Content
	  is Error do Content
	  is content:string do dbmstore(f,key,content)
	  is Nothing do buildErrorPacket("storing null database record, use 'remove' to remove records")
	  else printErrorMessageE(CONTENT,"expected a string"))
     else printErrorMessageE(KEY,"expected a string"));

assignelemfun(lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(lhsarray);
     when x
     is Error do x
     is x:List do (
	  if x.mutable then assignvector(x,lhsindex,rhs)
	  else buildErrorPacket("assignment attempted to element of immutable list")
	  )
     is x:Sequence do buildErrorPacket("assignment attempted to element of sequence")
     is x:HashTable do storeInHashTable(x,lhsindex,rhs)
     is x:Database do dbmstore(x,lhsindex,rhs)
     is dc:DictionaryClosure do (
	  if dc.dictionary.protected then printErrorMessageE(lhsarray,"attempted to create symbol in protected dictionary")
	  else storeInDictionary(dc,lhsindex,rhs))
     else printErrorMessageE(lhsarray,"expected a list, hash table, database, or dictionary")
     );
AssignElemFun = assignelemfun;

assignquotedobject(x:HashTable,i:Code,rhs:Code):Expr := (
     when i
     is c:globalSymbolClosureCode do (
	  ival := Expr(SymbolClosure(globalFrame,c.symbol));
	  val := eval(rhs);
	  when val is Error do val else storeInHashTable(x,ival,val))
     else printErrorMessageE(i,"'.' expected right hand argument to be a symbol")
     );

assignquotedelemfun(lhsarray:Code,lhsindex:Code,rhs:Code):Expr := (
     x := eval(lhsarray);
     when x
     is x:HashTable do assignquotedobject(x,lhsindex,rhs)
     else printErrorMessageE(lhsarray,"'.' expected left hand side to be a hash table")
     );
AssignQuotedElemFun = assignquotedelemfun;

evalWhileDoCode(c:whileDoCode):Expr := (
     while true do (
	  p := eval(c.predicate);
	  when p is err:Error 
	  do return if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else p
	  else if p == True then (
	       b := eval(c.doClause);
	       when b is err:Error 
	       do if err.message == continueMessage then nothing
	       else return if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else b 
	       else nothing;
	       )
	  else if p == False then break
	  else return printErrorMessageE(c.predicate,"expected true or false"));
     nullE);

evalWhileListCode(c:whileListCode):Expr := (
     n := 1;
     r := new Sequence len n do provide nullE;
     i := 0;
     while true do (
	  p := eval(c.predicate);
	  when p is err:Error
	  do return if err.message == breakMessage then if err.value == dummyExpr then nullE else err.value else p
	  else if p == True then (
	       b := eval(c.listClause);
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

evalWhileListDoCode(c:whileListDoCode):Expr := (
     r := new Sequence len 1 do provide nullE;
     i := 0;
     while true do (
	  p := eval(c.predicate);
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
	       b := eval(c.listClause);
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
	       d := eval(c.doClause);
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

evalForCode(c:forCode):Expr := (
     r := if c.listClause == dummyCode then emptySequence else new Sequence len 1 do provide nullE;
     i := 0;				    -- index in r
     j := 0;				    -- the value of the loop variable if it's an integer loop, else the index in the list if it's "for i in w ..."
     w := emptySequence;				    -- the list x when it's "for i in w ..."
     n := 0;				    -- the upper bound on j, if there is a toClause.
     listLoop := false;
     toLimit := false;
     if c.inClause != dummyCode then (
     	  listLoop = true;
	  invalue := eval(c.inClause);
	  when invalue is Error do return invalue
	  is ww:Sequence do w = ww
	  is vv:List do w = vv.v
	  else return printErrorMessageE(c.inClause,"expected a list or sequence");	  
	  )
     else (
	  if c.fromClause != dummyCode then (
	       fromvalue := eval(c.fromClause);
	       when fromvalue 
	       is Error do return fromvalue
	       is f:ZZ do (
		    if isInt(f) then j = toInt(f)
		    else return printErrorMessageE(c.fromClause,"expected a small integer"))
	       else return printErrorMessageE(c.fromClause,"expected an integer"));
	  if c.toClause != dummyCode then (
	       toLimit = true;
	       tovalue := eval(c.toClause);
	       when tovalue 
	       is Error do return tovalue
	       is f:ZZ do (
		    if isInt(f) then n = toInt(f)
		    else return printErrorMessageE(c.toClause,"expected a small integer"))
	       else return printErrorMessageE(c.toClause,"expected an integer"));
	  );
     localFrame = Frame(localFrame,c.frameID,c.framesize,false,new Sequence len c.framesize do provide nullE);
     while true do (
	  if toLimit && j > n then break;
	  if listLoop && j >= length(w) then break;
	  localFrame.values.0 = if listLoop then w.j else Expr(toInteger(j));		    -- should be the frame spot for the loop var
	  j = j+1;
	  if c.whenClause != dummyCode then (
	       p := eval(c.whenClause);
	       when p is err:Error do (
		    localFrame = localFrame.outerFrame;
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
		    localFrame = localFrame.outerFrame;
		    return printErrorMessageE(c.whenClause,"expected true or false")));
	  if c.listClause != dummyCode then (
	       b := eval(c.listClause);
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
		    localFrame = localFrame.outerFrame;
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
	       b := eval(c.doClause);
	       when b is err:Error do (
		    if err.message != continueMessage then (
			 localFrame = localFrame.outerFrame;
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

export binarymethod(left:Expr,rhs:Code,methodkey:SymbolClosure):Expr;
export applyFCCS(c:FunctionClosure,cs:CodeSequence):Expr;

export applyFCS(c:FunctionClosure,v:Sequence):Expr := (
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
	       saveLocalFrame := localFrame;
	       localFrame = f;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(model.body);			    -- do tail recursion here
	       recursionDepth = recursionDepth - 1;
	       localFrame = saveLocalFrame;
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
	       saveLocalFrame := localFrame;
	       localFrame = f;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(model.body);
	       recursionDepth = recursionDepth - 1;
	       localFrame = saveLocalFrame;
	       when ret is err:Error do returnFromFunction(ret) else ret
	       )
	  )
     else if desc.numparms != length(v)
     then WrongNumArgs(model.arrow,desc.numparms,length(v))
     else (
	  if framesize == 0 then (
	       saveLocalFrame := localFrame;
	       localFrame = previousFrame;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(model.body);
	       recursionDepth = recursionDepth - 1;
	       localFrame = saveLocalFrame;
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
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    when ret is err:Error do returnFromFunction(ret) else ret
		    )
	       else (
		    saveLocalFrame := localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      foreach x in v do provide x;
			      while true do provide nullE));
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    when ret is err:Error do returnFromFunction(ret) else ret
		    )
	       )
	  )
     );

wrongModel1(model:functionCode):Expr := printErrorMessageE(
     model.arrow, 
     "expected " + tostring(model.desc.numparms) + " argument" + (if model.desc.numparms == 1 then "" else "s") + " but got 1"
     );

export applyFCC(fc:FunctionClosure,ec:Code):Expr := (
     if recursionDepth > recursionLimit then return RecursionLimit();
     previousFrame := fc.frame;
     model := fc.model;
     desc := model.desc;
     framesize := desc.framesize;
     when ec is cs:sequenceCode do applyFCCS(fc,cs.x)
     else (
	  recursionDepth = recursionDepth + 1;
	  e := eval(ec);
	  recursionDepth = recursionDepth - 1;
	  when e is Error do return e 
	  is v:Sequence do applyFCS(fc,v)
	  else (
	       -- this is the case where the argument is one Expr, not a Sequence
	       if desc.numparms != 1 then return wrongModel1(model);
	       saveLocalFrame := localFrame;
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
			 localFrame = f;
	  		 recursionDepth = recursionDepth + 1;
			 tailCode := evalAllButTail(model.body);
	  		 recursionDepth = recursionDepth - 1;
			 -- formerly, just ret := eval(model.body); now do tail recursion instead
			 when tailCode
			 is e:Error do (
			      ret = Expr(e);
			      break;)
			 is b:adjacentCode do (			    -- this bit as in eval below, except for the tail recursion case
			      left := eval(b.lhs);
			      when left
			      is c2:FunctionClosure do (
				   rhs := b.rhs;
				   when rhs is cs:sequenceCode do (
					recursionDepth = recursionDepth + 1;
					ret = applyFCCS(c2,cs.x);
					recursionDepth = recursionDepth - 1;
					break)
				   else (
					recursionDepth = recursionDepth + 1;
					e = eval(rhs);
					recursionDepth = recursionDepth - 1;
					when e is Error do (
					     ret = e;
					     break;)
					is v:Sequence do (
				   	     recursionDepth = recursionDepth + 1;
					     ret = applyFCS(c2,v);
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
				   z := eval(b.rhs);
				   recursionDepth = recursionDepth - 1;
				   when z is Error do (
					ret = z;
					break)
				   else (
				   	recursionDepth = recursionDepth + 1;
					ret = ff.fn(z);
				   	recursionDepth = recursionDepth - 1;
					break))
			      is ff:CompiledFunctionClosure do (
				   recursionDepth = recursionDepth + 1;
				   z := eval(b.rhs);
				   recursionDepth = recursionDepth - 1;
				   when z is Error do (
					ret = z;
					break)
				   else (
		    	      		recursionDepth = recursionDepth + 1;
					ret = ff.fn(z,ff.env);
		    	      		recursionDepth = recursionDepth - 1;
					break))
			      is s:SpecialExpr do (
				   z := eval(b.rhs);
				   when z is Error do (
					ret = z;
					break)
				   else (
		    	      		recursionDepth = recursionDepth + 1;
					ret = applyEE(s.e,z);
		    	      		recursionDepth = recursionDepth - 1;
					break))
			      is Error do (
				   ret = left;
				   break)
			      else (
				   ret = binarymethod(left,b.rhs,AdjacentS);
				   break))
			 else (
		    	      recursionDepth = recursionDepth + 1;
			      ret = eval(tailCode);
		    	      recursionDepth = recursionDepth - 1;
			      break));
		    localFrame = saveLocalFrame;
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
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);			    -- don't bother with tail recursion here -- 21 arguments hardly ever happen!
		    recursionDepth = recursionDepth - 1;
		    localFrame = saveLocalFrame;
		    when ret is err:Error do returnFromFunction(ret) else ret -- this check takes time, too!
		    ))));
export applyFCE(fc:FunctionClosure,e:Expr):Expr := (
     -- single argument 'e' provided not a sequence, so framesize > 0 -- or should we check, for safety??
     if recursionDepth > recursionLimit then return RecursionLimit();
     previousFrame := fc.frame;
     model := fc.model;
     desc := model.desc;
     framesize := desc.framesize;
     if desc.numparms != 1 then return wrongModel1(model);
     saveLocalFrame := localFrame;
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
	  localFrame = f;
     	  recursionDepth = recursionDepth + 1;
	  ret := eval(model.body);
     	  recursionDepth = recursionDepth - 1;
	  localFrame = saveLocalFrame;
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
     	  localFrame = f;
     	  recursionDepth = recursionDepth + 1;
	  ret := eval(model.body);			    -- don't bother with tail recursion here -- 21 arguments hardly ever happen!
     	  recursionDepth = recursionDepth - 1;
	  localFrame = saveLocalFrame;
	  when ret is err:Error do returnFromFunction(ret) else ret -- this check takes time, too!
	  )
     );
export applyFCCS(c:FunctionClosure,cs:CodeSequence):Expr := (
     -- in this version we try to avoid allocating an array of exprs to
     -- hold the parameters, preferring to stuff them into the frame
     -- directly
     model := c.model;
     desc := model.desc;
     if recursionDepth > recursionLimit then RecursionLimit()
     else if desc.restargs
     then (
	  recursionDepth = recursionDepth + 1;
	  v := evalSequence(cs);
	  recursionDepth = recursionDepth - 1;
	  if evalSequenceHadError then evalSequenceErrorMessage else applyFCS(c,v)
	  )
     else if desc.numparms != length(cs)
     then WrongNumArgs(model.arrow,desc.numparms,length(cs))
     else (
     	  previousFrame := c.frame;
     	  framesize := desc.framesize;
	  if framesize == 0 then (
	       saveLocalFrame := localFrame;
	       localFrame = previousFrame;
	       recursionDepth = recursionDepth + 1;
	       ret := eval(model.body);			    -- do tail recursion here
	       recursionDepth = recursionDepth - 1;
	       when ret is err:Error do ret = returnFromFunction(ret)
	       else nothing;
	       localFrame = saveLocalFrame;
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
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret)
		    else nothing;
		    localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    saveLocalFrame := localFrame;
		    haderror := false;
		    recursionDepth = recursionDepth + 1;
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
		    recursionDepth = recursionDepth - 1;
		    if haderror then return errorreturn;
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret)
		    else nothing;
		    localFrame = saveLocalFrame;
		    ret
		    )
	       )
	  )
     );
export applyES(f:Expr,v:Sequence):Expr := (
     when f
     is ff:CompiledFunction do ff.fn(Expr(v))
     is ff:CompiledFunctionClosure do ff.fn(Expr(v),ff.env)
     is c:FunctionClosure do applyFCS(c,v)
     is s:SpecialExpr do applyES(s.e,v)
     else buildErrorPacket("expected a function"));
export applyEE(f:Expr,e:Expr):Expr := (
     when f
     is ff:CompiledFunction do ff.fn(e)
     is ff:CompiledFunctionClosure do ff.fn(e,ff.env)
     is c:FunctionClosure do (
	  when e
	  is v:Sequence do applyFCS(c,v)
	  else applyFCE(c,e)
	  )
     is s:SpecialExpr do applyEE(s.e,e)
     else buildErrorPacket("expected a function"));
--------
-- could optimize later
export applyEEE(g:Expr,e0:Expr,e1:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1)))
     when g
     is ff:CompiledFunction do ff.fn(Expr(Sequence(e0,e1)))
     is ff:CompiledFunctionClosure do ff.fn(Expr(Sequence(e0,e1)),ff.env)
     is s:SpecialExpr do applyEEE(s.e,e0,e1)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then applyFCS(c,Sequence(e0,e1))
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
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret
		    )
	       else (
		    saveLocalFrame := localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      while true do provide nullE));
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    ret
		    )
	       )
	  )
     else buildErrorPacket("expected a function"));     
export applyEEE(g:Expr,e0:Expr,e1:Expr,e2:Expr):Expr := (
     -- was simply apply(f,Expr(Sequence(e0,e1,e2)));
     when g
     is ff:CompiledFunction do ff.fn(Expr(Sequence(e0,e1,e2)))
     is ff:CompiledFunctionClosure do ff.fn(Expr(Sequence(e0,e1,e2)),ff.env)
     is s:SpecialExpr do applyEEE(s.e,e0,e1,e2)
     is c:FunctionClosure do (
	  model := c.model;
	  desc := model.desc;
	  if desc.restargs
	  then applyFCS(c,Sequence(e0,e1,e2))
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
		    saveLocalFrame := localFrame;
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    if !f.notrecyclable then (
			 foreach x in f.values do x = nullE;
			 f.outerFrame = recycleBin.framesize;
			 f.frameID = -2;				    -- just to be tidy, not really needed
			 recycleBin.framesize = f;
			 );
		    ret)
	       else (
		    saveLocalFrame := localFrame;
		    f := Frame(previousFrame,desc.frameID,framesize,false,
			 new Sequence len framesize do (
			      provide e0;
			      provide e1;
			      provide e2;
			      while true do provide nullE));
		    localFrame = f;
		    recursionDepth = recursionDepth + 1;
		    ret := eval(model.body);
		    recursionDepth = recursionDepth - 1;
		    when ret is err:Error do ret = returnFromFunction(ret) else nothing;
		    localFrame = saveLocalFrame;
		    ret)))
     else buildErrorPacket("expected a function"));     

-----------------------------------------------------------------------------

export unarymethod(rhs:Code,methodkey:SymbolClosure):Expr := (
     right := eval(rhs);
     when right is Error do right
     else (
	  method := lookup(Class(right),Expr(methodkey),methodkey.symbol.hash);
	  if method == nullE then MissingMethod(methodkey)
	  else applyEE(method,right)));
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
	       else applyEEE(method,left,right))));
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
	  else applyEEE(method,left,right)));

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
		    r := applyEEE(f,sym,newvalue);
		    when r is Error do return r else nothing;
		    );
	       nullE)
	  is f:CompiledFunction do applyEEE(g,sym,newvalue)
	  is f:CompiledFunctionClosure do applyEEE(g,sym,newvalue)
	  is f:FunctionClosure do applyEEE(g,sym,newvalue)
	  is f:SpecialExpr do applyEEE(f.e,sym,newvalue)
	  else buildErrorPacket("expected global assignment hook for " + t.word.name + " to be a function or list of functions");
	  when y is Error do return y else nothing;
	  );
     if method != nullE then (
	  y := applyEEE(method,sym,oldvalue);
	  when y is Error do return y else nothing;
	  );
     method = lookup(Class(newvalue),GlobalAssignE);
     if method != nullE then (
	  y := applyEEE(method,sym,newvalue);
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
     is values:Sequence do 
     if nlhs == length(values) then (
	  for i from 0 to nlhs-1 do (
	       r := assignment(nestingDepth.i,frameindex.i,syms.i,values.i);
	       when r is Error do return r else nothing;
	       );
	  value
	  )
     else buildErrorPacket("parallel assignment: expected a sequence of " + tostring(nlhs) + " values")
     else buildErrorPacket("parallel assignment: expected a sequence of " + tostring(nlhs) + " values"));

-----------------------------------------------------------------------------

export backtrace := true;
export steppingFlag := false;
export determineExceptionFlag():void := (
     exceptionFlag = interruptedFlag || steppingFlag || alarmedFlag;
     );
export clearAlarm():void := (
     alarm(0);
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
     steppingFlag = false;
     determineExceptionFlag();
     );

lastCode := dummyCode;
stepCount := 0;
steppingFurther(c:Code):bool := (
     if !steppingFlag then return false;
     if stepCount == 0 then return false;
     if lastCode != c then (
     	  stepCount = stepCount - 1;
	  lastCode = c);
     stepCount > 0);

export eval(c:Code):Expr := (
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
	  is u:unaryCode do u.f(u.rhs)
	  is b:binaryCode do b.f(b.lhs,b.rhs)
	  is b:adjacentCode do (
	       left := eval(b.lhs);
	       when left
	       is fc:FunctionClosure do applyFCC(fc,b.rhs)
	       is ff:CompiledFunction do (
		    z := eval(b.rhs);
		    when z is Error do z
		    else ff.fn(z))
	       is ff:CompiledFunctionClosure do (
		    z := eval(b.rhs);
		    when z is Error do z
		    else ff.fn(z,ff.env))
	       is s:SpecialExpr do (
		    z := eval(b.rhs);
		    when z is Error do z
		    else applyEE(s.e,z))
	       is Error do left
	       else binarymethod(left,b.rhs,AdjacentS))
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
			 if err.message == breakMessage || err.message == returnMessage || 
			 err.message == continueMessage || err.message == continueMessageWithArg || 
			 err.message == unwindMessage || err.message == throwMessage
			 then p
			 else eval(c.elseClause))
		    else if c.thenClause == NullCode then p else eval(c.thenClause)))
	  is c:catchCode do (
	       p := eval(c.code);
	       when p is err:Error do if err.message == throwMessage then err.value else p
	       else p)
	  is c:ifCode do (
	       p := eval(c.predicate);
	       when p is Error do p
	       else if p == True then eval(c.thenClause)
	       else if p == False then eval(c.elseClause)
	       else printErrorMessageE(c.predicate,"expected true or false"))
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
	  is v:RRRCode do return Expr(v.x)
	  is v:integerCode do return Expr(v.x)
	  is v:stringCode do return Expr(v.x)
     	  is v:Error do Expr(v)
	  is v:semiCode do (
	       w := v.w;
	       n := length(w);				    -- at least 2
	       r := eval(w.0);
	       when r is Error do r else (
	       	    r = eval(w.1);
	       	    when r is Error do r else (
	       		 if n == 2 then return r;
	       		 r = eval(w.2);
	       		 when r is Error do r else (
			      if n == 3 then return r;
			      r = eval(w.3);
			      when r is Error do r else (
				   if n == 4 then return r;
				   r = eval(w.4);
				   when r is Error do r else (
					i := 5;
					while i < n do (
					     r = eval(w.i);
					     i = when r is Error do n else i+1;
					     );
					r))))))
	  is v:sequenceCode do (
	       if length(v.x) == 0 then return emptySequence;
	       r := evalSequence(v.x);
	       if evalSequenceHadError then evalSequenceErrorMessage else Expr(r)) -- speed up
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
	  if err.message == returnMessage || err.message == continueMessage || err.message == continueMessageWithArg || 
	  err.message == breakMessage || err.message == unwindMessage || err.message == throwMessage
	  then (
	       if err.position == dummyPosition then err.position = codePosition(c); -- there will be no way to enter the debugger to figure out an unhandled break
	       return e;
	       );
	  p := codePosition(c);
     	  clearAllFlags();
     	  clearAlarm();
     	  if int(p.loadDepth) >= errorDepth && !err.position === p then (
	       oldReportFrame := err.frame;
	       err.frame = noRecycle(localFrame);
	       err.position = p;
	       if !err.printed || backtrace && localFrame != oldReportFrame then (
		    if debuggingMode && !stopIfError && (! (p.filename === "stdio")) then (
			 if !err.printed then printError(err);
			 printErrorMessage(err.position,"--entering debugger--");
			 z := debuggerFun(localFrame,c);
			 stderr << "--leaving debugger--" << endl;
			 when z is z:Error do (
			      if z.message == breakMessage then return buildErrorPacket(unwindMessage);
			      if z.message == returnMessage then return z.value;
			      if z.message == continueMessageWithArg || z.message == continueMessage then (
				   when z.value is step:ZZ do (
					if isInt(step) then (
					     steppingFlag = true;
					     lastCode = c;
					     exceptionFlag = true;
					     stepCount = toInt(step);
					     ))
				   else nothing;
				   return eval(c)))
			 else nothing)
		    else (printError(err);)));
	  e)
     else e);

export evalexcept(c:Code):Expr := (
     e := eval(c);
     if exceptionFlag then (				    -- compare this code to the code at the top of eval() above
	  if alarmedFlag then (
	       clearAlarmedFlag();
	       printErrorMessageE(c,alarmMessage))
	  else if interruptedFlag then (
	       SuppressErrors = false;
	       clearInterruptFlag();
	       printErrorMessageE(c,interruptMessage))
	  else (
	       SuppressErrors = false;
	       clearAllFlags();
	       printErrorMessageE(c,"unknown exception")
	       )
	  )
     else e
     );

export eval(f:Frame,c:Code):Expr := (
     saveLocalFrame := localFrame;
     localFrame = f;
     ret := evalexcept(c);
     localFrame = saveLocalFrame;
     ret);

shieldfun(a:Code):Expr := (
     if interruptShield then eval(a)
     else (
     	  interruptPending = interruptedFlag;
     	  interruptShield = true;
     	  ret := eval(a);
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

returnFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,returnMessage,e,false,dummyFrame)));
setupop(returnS,returnFun);

throwFun(a:Code):Expr := (
     e := eval(a);
     when e is Error do e else Expr(Error(dummyPosition,throwMessage,e,false,dummyFrame)));
setupop(throwS,throwFun);

continueFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,
	       if a == dummyCode then continueMessage else continueMessageWithArg,
	       e,false,dummyFrame)));
setupop(continueS,continueFun);

breakFun(a:Code):Expr := (
     e := if a == dummyCode then dummyExpr else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,breakMessage,e,false,dummyFrame)));
setupop(breakS,breakFun);

assigntofun(lhs:Code,rhs:Code):Expr := (
     left := eval(lhs);
     when left
     is q:SymbolClosure do (
	  if q.symbol.protected then (
	       buildErrorPacket("assignment to protected variable '" + q.symbol.word.name + "'")
	       )
	  else (
	       value := eval(rhs);
	       when value is Error do return value else nothing;
	       q.frame.values.(q.symbol.frameindex) = value;
	       value))
     is Error do left
     else (
	  method := lookup(Class(left),LeftArrowE); -- method for x <- y is looked up under (symbol <-, class x)
	  if method == nullE then buildErrorPacket("'<-': no method for object on left")
	  else (
	       value := eval(rhs);
	       when value is Error do return value else nothing;
	       applyEEE(method,left,value))));
setup(LeftArrowS,assigntofun);

idfun(e:Expr):Expr := e;
setupfun("identity",idfun);
scanpairs(f:Expr,obj:HashTable):Expr := (
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := applyEEE(f,p.key,p.value);
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

mpre():Expr := buildErrorPacket("applyPairs: expected function to return null, a sequence of length 2, or an option x=>y");
mappairs(f:Expr,obj:HashTable):Expr := (
     newobj := newHashTable(obj.class,obj.parent);
     foreach bucket in obj.table do (
	  p := bucket;
	  while true do (
	       if p == p.next then break;
	       v := applyEEE(f,p.key,p.value);
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
	       newkey := applyEE(f,p.key);
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
		    newvalue := applyEE(f,p.value);
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
			      t := applyEEE(g,val,q.value);
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
			      t := applyEEE(g,q.value,val);
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
			 pqkey := applyEEE(f,p.key,q.key);
			 when pqkey is Error do return pqkey else nothing;
			 pqvalue := applyEEE(g,p.value,q.value);
			 when pqvalue is Error do return pqvalue else nothing;
			 pqhash := hash(pqkey);
			 previous := lookup1(z,pqkey,pqhash);
			 r := storeInHashTable(z,pqkey,pqhash,
			      if previous == notfoundE
			      then pqvalue
			      else (
				   t := applyEEE(h,previous,pqvalue);
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
     else applyEE(method,right));

export binarymethod(left:Expr,right:Expr,methodkey:SymbolClosure):Expr := (
     method := lookupBinaryMethod(Class(left),Class(right),Expr(methodkey),methodkey.symbol.hash);
     if method == nullE then MissingMethodPair(methodkey,left,right)
     else applyEEE(method,left,right));

export binarymethod(left:Expr,right:Expr,methodkey:Expr,methodkeyname:string):Expr := (
     method := lookupBinaryMethod(Class(left),Class(right),methodkey,hash(methodkey));
     if method == nullE then MissingMethodPair(methodkeyname,left,right)
     else applyEEE(method,left,right));

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
