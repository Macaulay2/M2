--		Copyright 1994 by Daniel R. Grayson
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
use ctype;
use stdio;
use varstrin;
use strings;
use basic;

dummyMultaryFun(c:CodeSequence):Expr := (
     error("dummy multary function called");
     nullE);
dummyForFun(c:forCode):Expr := (
     error("dummy for function called");
     nullE);
dummyGlobalAssignmentFun(x:globalAssignmentCode):Expr := (
     error("dummy global assignment function called");
     nullE);
dummyLocalAssignmentFun(x:localAssignmentCode):Expr := (
     error("dummy local assignment function called");
     nullE);
dummyParallelAssignmentFun(x:parallelAssignmentCode):Expr := (
     error("dummy parallel assignment function called");
     nullE);

export LocalAssignmentFun := dummyLocalAssignmentFun;	-- filled in later in actors2.d
export GlobalAssignmentFun := dummyGlobalAssignmentFun;	-- filled in later in actors2.d
export ParallelAssignmentFun := dummyParallelAssignmentFun;	-- filled in later in actors2.d
export AdjacentFun := dummyBinaryFun;	-- filled in later in actors.d
export AssignElemFun := dummyTernaryFun;	-- filled in later in actors.d
export AssignQuotedElemFun := dummyTernaryFun;	-- filled in later in actors.d
export TryElseFun := dummyBinaryFun; 	-- filled in later in actors.d
export TryFun := dummyUnaryFun; 	-- filled in later in actors.d
export IfThenFun := dummyBinaryFun;	-- filled in later in actors.d
export IfThenElseFun := dummyTernaryFun;-- filled in later in actors.d
export ForFun := dummyForFun;      -- filled in later in actors.d
export WhileDoFun := dummyBinaryFun;      -- filled in later in actors.d
export WhileListFun := dummyBinaryFun;      -- filled in later in actors.d
export WhileListDoFun := dummyTernaryFun;      -- filled in later in actors.d
export QuoteFun := dummyUnaryFun;       -- filled in later in actors.d
export NewFun := dummyUnaryFun;	  -- filled in later in actors.d
export NewFromFun := dummyBinaryFun;	  -- filled in later in actors.d
export NewOfFun := dummyBinaryFun;	  -- filled in later in actors.d
export NewOfFromFun := dummyTernaryFun;	  -- filled in later in actors.d

export AssignNewFun := dummyBinaryFun;
export AssignNewOfFun := dummyTernaryFun;
export AssignNewFromFun := dummyTernaryFun;
export AssignNewOfFromFun := dummyMultaryFun;

export InstallMethodFun := dummyMultaryFun;
export UnaryInstallMethodFun := dummyTernaryFun;

export InstallValueFun := dummyMultaryFun;
export UnaryInstallValueFun := dummyTernaryFun;

export braceFun := dummyMultaryFun;     -- filled in later in actors.d
export bracketFun := dummyMultaryFun;   -- filled in later in actors.d

export convert(e:ParseTree):Code;
CodeSequenceLength(e:ParseTree):int := (
     i := 0;
     while true do (
     	  when e
     	  is b:Binary do (
	       if b.operator.word == commaW
	       then ( i = i + CodeSequenceLength(b.lhs); e = b.rhs )
	       else return(i+1))
	  is u:Unary do (
	       if u.operator.word == commaW
	       then ( i = i + 1; e = u.rhs )
	       else return(i+1))
	  else return(i+1)));
fillCodeSequence(e:ParseTree,v:CodeSequence,m:int):int := (
     -- starts filling v at position m, returns the next available position
     while true do (
     	  when e
     	  is b:Binary do (
	       if b.operator.word == commaW
	       then ( m = fillCodeSequence(b.lhs,v,m); e = b.rhs )
	       else ( v.m = convert(e); return(m+1)))
	  is u:Unary do (
	       if u.operator.word == commaW
	       then ( 
		    v.m = exprCode(nullE,treePosition(e));
		    m = m + 1; 
		    e = u.rhs )
	       else ( v.m = convert(e); return(m+1)))
	  is p:EmptyParentheses do (
	       (v.m = convert(e); return(m+1)))
	  is dummy do (
	       v.m = exprCode(nullE,treePosition(e));
	       return(m+1);
	       )
	  is p:Parentheses do (
	       ( v.m = convert(e); return(m+1)))
	  else ( v.m = convert(e); return(m+1))));
makeCodeSequence(e:ParseTree):CodeSequence := (
     v := new CodeSequence len CodeSequenceLength(e) do provide dummyCode;
     fillCodeSequence(e,v,0);
     v);
SymbolSequenceLength(e:ParseTree):int := (
     i := 0;
     while true do (
     	  when e
	  is p:Parentheses do e = p.contents
     	  is b:Binary do (
	       i = i+1;
	       e = b.lhs;
	       )
	  else (					    -- should be the first token
	       i = i+1;
	       return(i);
	       )
	  )
     );
makeSymbolSequence(e:ParseTree):SymbolSequence := (	    -- but replace local symbols by dummySymbol
     m := SymbolSequenceLength(e);
     v := new SymbolSequence len m do provide dummySymbol;
     while true do (
     	  when e
	  is p:Parentheses do e = p.contents
     	  is b:Binary do (
	       when b.rhs is t:Token do (
		    m = m-1;
		    v.m = t.entry;
		    )
	       else nothing;				    -- shouldn't happen
	       e = b.lhs;
	       )
	  is t:Token do (
	       m = m-1;
	       v.m = t.entry;
	       break;
	       )
	  else break;					    -- shouldn't happen
	  );
     v);
export showFrames(f:Frame):void := (
     stdout << " frames bound :";
     while (
	  stdout << " " << f.frameID << " [" << f.valuesUsed;
	  if f.valuesUsed != length(f.values) then stdout << "<" << length(f.values);
	  stdout << "]";
	  f != f.outerFrame ) do (
	  stdout << ",";
	  f = f.outerFrame;
	  );
     stdout << endl;
     );
export frame(frameID:int):Frame := (			    -- old version, becoming obsolete
     if frameID == 0 then globalFrame
     else (
     	  f := localFrame;
     	  while f.frameID > frameID do f = f.outerFrame;
     	  if f.frameID != frameID 
	  then (
	       stderr << "frame for dictionary " << frameID << " not found" << endl;
	       stderr << "frames active for dictionaries: ";
	       f = localFrame;
	       while f.frameID > -1 do (
		    stderr << " " << f.frameID;
		    f = f.outerFrame;
		    );
	       stderr << endl;
	       fatal("exiting");
	       );
     	  f));
export frameWithNestingDepth(frameID:int,nestingDepth:int):Frame := (	    -- transitional version, just to verify nestingDepth
     if frameID == 0 then (
	  stderr << "warning: nesting depth not applicable to global frame" << endl;
	  globalFrame)
     else (
	  n := 0;
     	  f := localFrame;
     	  while f.frameID > frameID do (
	       f = f.outerFrame;
	       n = n+1;
	       );
     	  if f.frameID != frameID 
	  then (
	       stderr << "frame for dictionary " << frameID << " not found" << endl;
	       stderr << "frames active for dictionaries: ";
	       f = localFrame;
	       while f.frameID > -1 do (
		    stderr << " " << f.frameID;
		    f = f.outerFrame;
		    );
	       stderr << endl;
	       fatal("exiting");
	       );
	  if n != nestingDepth then (
	       stderr << "estimated nesting depth for frame " << frameID << " incorrect: " << nestingDepth << endl;
	       showFrames(localFrame);
	       );
     	  f));

export frameByNestingDepth(nestingDepth:int):Frame := (	    -- new version
     f := localFrame;
     while nestingDepth > 0 do (
	  f = f.outerFrame;
	  nestingDepth = nestingDepth - 1;
	  );
     f);

allExprCodes(cs:CodeSequence):bool := (
     foreach c in cs do when c is exprCode do nothing else return(false);
     return(true);
     );
combine(cs:CodeSequence):Sequence := (
     new Sequence len length(cs) do (
	  foreach c in cs do
	  when c is z:exprCode do provide z.v
	  else provide nullE		  -- will not happen
	  ));

nestingDepth(frameID:int,d:Dictionary):int := (
     if frameID == 0 then return(-1);
     n := 0;
     while d.frameID != frameID do (
	  if !d.transient || d.framesize != 0 then n = n+1; -- empty transient frames will not appear at runtime
	  if d == d.outerDictionary then (
	       error("internal error during conversion: frameID " + tostring(frameID) + " not found");
	       break;
	       );
	  d = d.outerDictionary;
	  );
     n);

tokenAssignment(e:ParseTree,b:Binary,t:Token):Code := (
     if t.entry.frameID == 0
     then Code(globalAssignmentCode(t.entry,convert(b.rhs),treePosition(e)))
     else Code(localAssignmentCode(nestingDepth(t.entry.frameID,t.dictionary),t.entry.frameindex,convert(b.rhs),treePosition(e)))
     );

parallelAssignment(e:ParseTree,b:Binary,p:Parentheses):Code := (
     symbols := makeSymbolSequence(b.lhs);
     n := length(symbols);
     nd := new array(int) len n do foreach x in symbols do provide nestingDepth(x.frameID,b.operator.dictionary);
     fr := new array(int) len n do foreach x in symbols do provide x.frameindex;
     foreach x in symbols do if x.frameID != 0 then x = dummySymbol;
     Code(parallelAssignmentCode(
	       nd,
	       fr,
	       symbols,
	       convert(b.rhs),
	       treePosition(e)
	       ))
     );

export makeSymbolClosure(s:Symbol):SymbolClosure := SymbolClosure(frame(s.frameID),s);

export convert(e:ParseTree):Code := (
     when e
     is s:StartDictionary do (
	  if s.dictionary.framesize != 0
	  then Code(openDictionaryCode(s.dictionary,convert(s.body)))
	  else convert(s.body)
	  )
     is w:For do Code(
	  forCode(
	       convert(w.fromClause), convert(w.toClause),
	       convert(w.whenClause), convert(w.listClause), 
	       convert(w.doClause),
	       w.dictionary,
	       treePosition(e)))
     is w:WhileDo do Code(
	  binaryCode(WhileDoFun,convert(w.predicate),convert(w.doClause),
	       treePosition(e)))
     is w:WhileList do Code(
	  binaryCode(WhileListFun,convert(w.predicate),convert(w.listClause),
	       treePosition(e)))
     is w:WhileListDo do Code(
	  ternaryCode(WhileListDoFun,convert(w.predicate),convert(w.listClause),convert(w.doClause),
	       treePosition(e)))
     is n:New do (
	  if n.newparent == dummyTree
	  then if n.newinitializer == dummyTree
	       then Code(unaryCode(NewFun,convert(n.newclass),
		    treePosition(e)))
	       else Code(binaryCode(NewFromFun,convert(n.newclass),convert(n.newinitializer),
		    treePosition(e)))
	  else if n.newinitializer == dummyTree
	       then Code(binaryCode(NewOfFun,convert(n.newclass),convert(n.newparent),
		    treePosition(e)))
	       else Code(ternaryCode(NewOfFromFun,
		    convert(n.newclass),convert(n.newparent),convert(n.newinitializer),
		    treePosition(e))))
     is i:IfThen do Code(
	  binaryCode(IfThenFun,
	       convert(i.predicate),convert(i.thenclause),
	       treePosition(e)))
     is i:IfThenElse do Code(
	  ternaryCode(IfThenElseFun,
	       convert(i.predicate),
	       convert(i.thenclause),convert(i.elseClause),
	       treePosition(e)))
     is token:Token do (
	  var := token.entry;
	  wrd := token.word;
	  pos := token.position;
	  if wrd.typecode == TCdouble
	  then Code(exprCode(Real(parseDouble(wrd.name)),pos))
	  else if wrd.typecode == TCint
	  then Code(exprCode(parseInt(wrd.name),pos))
 	  else if wrd.typecode == TCstring
	  then Code(exprCode(parseString(wrd.name), pos))
	  else Code(variableCode(var,pos)))
     is a:Adjacent do Code(
	  binaryCode(AdjacentFun, convert(a.lhs),convert(a.rhs),
	       treePosition(e)))
     is p:EmptyParentheses do (
	  if p.left.word == leftparen then Code(exprCode(emptySequenceE,treePosition(e)))
	  else if p.left.word == leftbrace then Code(exprCode(emptyList,treePosition(e)))
	  else if p.left.word == leftbracket then Code(exprCode(emptyArray, treePosition(e)))
	  else dummyCode			  -- should not happen
	  )
     is p:Parentheses do (
	  if p.left.word == leftparen then convert(p.contents)
	  else if p.left.word == leftbrace 
	  then (
	       cs := makeCodeSequence(p.contents);
	       if allExprCodes(cs)
	       then Code(exprCode(list(combine(cs)),treePosition(e)))
	       else Code(multaryCode(braceFun,cs,treePosition(e)))
	       )
	  else 
	  if p.left.word == leftbracket 
	  then (
	       cs := makeCodeSequence(p.contents);
	       if allExprCodes(cs)
	       then Code(exprCode(Array(combine(cs)),treePosition(e)))
	       else Code(multaryCode(bracketFun,cs,treePosition(e)))
	       )
	  else 
	  dummyCode			  -- should not happen
	  )
     is b:Binary do (
	  if b.operator.entry == DotS.symbol
	  || b.operator.entry == DotQuestionS.symbol
	  then (
	       when b.rhs
	       is token:Token do (
	  	    wrd := token.word;
		    var := token.entry;
		    if wrd.typecode == TCid
		    then (
	       		 Code(binaryCode(
			 	   b.operator.entry.binary,
			 	   convert(b.lhs),
	       	    	 	   Code(exprCode(
					     Expr(SymbolClosure(globalFrame,var)),
					     treePosition(b.rhs))),
			 	   treePosition(e)
				   )
			      )
			 )
		    else dummyCode	  -- should not occur
		    )
	       else dummyCode		  -- should not occur
	       )
	  else if b.operator.word == commaW
	  then (
	       cs := makeCodeSequence(e);
	       if allExprCodes(cs)
	       then Code(exprCode(Expr(combine(cs)),treePosition(e)))
	       else Code(cs)
	       )
	  else if b.operator.word == EqualW
	  then (
	       when b.lhs
	       is a:Adjacent do (
		    Code(multaryCode(
			      InstallValueFun,
			      CodeSequence(
			      	   Code(exprCode(AdjacentS,dummyPosition)),
			      	   convert(a.lhs),
			      	   convert(a.rhs),
			      	   convert(b.rhs)),
			      treePosition(e))))
	       is u:Unary do Code(ternaryCode(
			 UnaryInstallValueFun,
			 Code(exprCode(makeSymbolClosure(u.operator.entry), dummyPosition)),
			 convert(u.rhs), convert(b.rhs), treePosition(e)))
	       is u:Postfix do Code(ternaryCode(
			 UnaryInstallValueFun,
			 Code(exprCode(makeSymbolClosure(u.operator.entry), dummyPosition)),
			 convert(u.lhs), convert(b.rhs), treePosition(e)))
	       is c:Binary do (
		    if c.operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, convert(c.lhs),
			      convert(c.rhs), convert(b.rhs), treePosition(e)))
		    else if c.operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   convert(c.lhs),
				   Code(exprCode(Expr(SymbolClosure( globalFrame, crhs.entry)), 
					     treePosition(c.rhs))),
				   convert(b.rhs),
				   treePosition(e)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallValueFun,
			      CodeSequence(
				   Code(exprCode( Expr(makeSymbolClosure(c.operator.entry)), 
					     dummyPosition)),
				   convert(c.lhs),
				   convert(c.rhs),
				   convert(b.rhs)),
			      treePosition(e))))
	       is t:Token do tokenAssignment(e,b,t)
	       is p:Parentheses do parallelAssignment(e,b,p)
	       else dummyCode		  -- should not happen
	       )
	  else if b.operator.word == ColonEqualW
	  then (
	       when b.lhs
	       is n:New do (
		    if n.newparent == dummyTree 
		    then if n.newinitializer == dummyTree 
		    then Code(binaryCode(
			      AssignNewFun,
			      convert(n.newclass),
			      convert(b.rhs), 
			      treePosition(e)))
		    else Code(ternaryCode(
			      AssignNewFromFun,
			      convert(n.newclass),
			      convert(n.newinitializer),
			      convert(b.rhs),
			      treePosition(e)))
     	       	    else if n.newinitializer == dummyTree 
		    then Code(ternaryCode(
			      AssignNewOfFun,
			      convert(n.newclass),
			      convert(n.newparent),
			      convert(b.rhs),
			      treePosition(e)))
		    else Code(multaryCode(
			      AssignNewOfFromFun,
			      CodeSequence(
				   convert(n.newclass),
				   convert(n.newparent),
				   convert(n.newinitializer),
				   convert(b.rhs)),
			      treePosition(e))))
	       is a:Adjacent do (
		    Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			      	   Code(exprCode(AdjacentS,dummyPosition)),
			      	   convert(a.lhs),
			      	   convert(a.rhs),
			      	   convert(b.rhs)),
			      treePosition(e))))
	       is u:Unary do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(exprCode(makeSymbolClosure(u.operator.entry), dummyPosition)),
			 convert(u.rhs), convert(b.rhs), treePosition(e)))
	       is u:Postfix do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(exprCode(makeSymbolClosure(u.operator.entry), dummyPosition)),
			 convert(u.lhs), convert(b.rhs), treePosition(e)))
	       is c:Binary do (
		    if c.operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, convert(c.lhs),
			      convert(c.rhs), convert(b.rhs), treePosition(e)))
		    else if c.operator.entry == UnderscoreS.symbol
		    then Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence( 
			      	   Code(exprCode(UnderscoreS,dummyPosition)),
				   convert(c.lhs),
				   convert(c.rhs),
			      	   convert(b.rhs)),
			      treePosition(e)))
		    else if c.operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   convert(c.lhs),
				   Code(exprCode(Expr(SymbolClosure(
						       globalFrame,
						       crhs.entry)),
					     treePosition(c.rhs))),
				   convert(b.rhs),
				   treePosition(e)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
				   Code(
					exprCode(
					     Expr(makeSymbolClosure(c.operator.entry)),
					     dummyPosition)),
				   convert(c.lhs),
				   convert(c.rhs),
				   convert(b.rhs)),
			      treePosition(e))))
	       is t:Token do tokenAssignment(e,b,t)
	       is p:Parentheses do parallelAssignment(e,b,p)
	       else dummyCode		  -- should not happen
	       )
	  else Code(binaryCode(b.operator.entry.binary,convert(b.lhs),
	       	    convert(b.rhs),treePosition(e)))
	  )
     is a:Arrow do Code(functionCode(
	       convert(a.lhs),		  -- just for display purposes!
	       convert(a.rhs),a.desc
	       ))
     is u:Unary do (
	  if u.operator.word == commaW
	  then (
	       cs := makeCodeSequence(e);
	       if allExprCodes(cs)
	       then Code(exprCode(Expr(combine(cs)),treePosition(e)))
	       else Code(cs)
	       )
	  else Code(unaryCode(u.operator.entry.unary,convert(u.rhs),treePosition(e))))
     is q:Quote do (
	  Code(unaryCode(
		    QuoteFun,
		    Code(variableCode(q.rhs.entry,q.rhs.position)),
		    treePosition(e)
		    )
	       )
	  )
     is q:GlobalQuote do (
	  Code(unaryCode(
	       	    QuoteFun,
	       	    Code(variableCode(q.rhs.entry,q.rhs.position)),
	            treePosition(e))
	       )
	  )
     is q:LocalQuote do (
	  Code(unaryCode(
	       	    QuoteFun,
	       	    Code(variableCode(q.rhs.entry,q.rhs.position)),
		    treePosition(e))))
     is i:TryElse do Code(
	  binaryCode(TryElseFun,
	       convert(i.primary),convert(i.alternate),
	       treePosition(e)))
     is i:Try do Code(
	  unaryCode(TryFun,
	       convert(i.primary),
	       treePosition(e)))
     is u:Postfix do Code(
	  unaryCode(u.operator.entry.postfix,convert(u.lhs),treePosition(e)))
     is d:dummy do (
     	  -- was Code(exprCode(nullE,d.position)), but forfun() is looking for dummyCode
	  dummyCode
	  ));
export codePosition(e:Code):Position := (
     when e
     is f:globalAssignmentCode do f.position
     is f:localAssignmentCode do f.position
     is f:localMemoryReferenceCode do f.position
     is f:staticMemoryReferenceCode do f.position
     is f:parallelAssignmentCode do f.position
     is f:exprCode do f.position
     is f:variableCode do f.position
     is f:unaryCode do f.position
     is f:binaryCode do f.position
     is f:ternaryCode do f.position
     is f:multaryCode do f.position
     is f:forCode do f.position
     is f:openDictionaryCode do codePosition(f.body)
     is f:functionCode do codePosition(f.parms)
     is v:CodeSequence do codePosition(v.0)-- it would be better to get the surrounding parens...
     );

export returnMessage := "return value";
export breakMessage := "break value";

export buildErrorPacket(message:string):Expr := Expr(Error(dummyPosition,message,emptySequence,nullE));
export buildErrorPacket(message:string,report:Expr):Expr := Expr(Error(dummyPosition,message,report,nullE));
export quoteit(name:string):string := "'" + name + "'";
export NotYet(desc:string):Expr := buildErrorPacket(desc + " not implemented yet");
export WrongArg(desc:string):Expr := buildErrorPacket("expected " + desc);
export WrongArg(n:int,desc:string):Expr := (
     buildErrorPacket("expected argument " + tostring(n) + " to be " + desc));
export WrongArgInteger():Expr := WrongArg("an integer");
export WrongArgInteger(n:int):Expr := WrongArg(n,"an integer");
export WrongArgSmallInteger():Expr := WrongArg("a small integer");
export WrongArgSmallInteger(n:int):Expr := WrongArg(n,"a small integer");
export WrongArgString():Expr := WrongArg("a string");
export WrongArgString(n:int):Expr := WrongArg(n,"a string");
export WrongArgBoolean():Expr := WrongArg("true or false");
export WrongArgBoolean(n:int):Expr := WrongArg(n,"true or false");
export ArgChanged(name:string,n:int):Expr := (
     buildErrorPacket(quoteit(name) + " expected argument " + tostring(n)
	  + " not to change its type during execution"));
export WrongNumArgs(name:string,n:int):Expr := (
     if n == 0
     then buildErrorPacket(quoteit(name) + " expected no arguments")
     else if n == 1
     then buildErrorPacket(quoteit(name) + " expected " + tostring(n) + " argument")
     else buildErrorPacket(quoteit(name) + " expected " + tostring(n) + " arguments")
     );
export WrongNumArgs(n:int):Expr := buildErrorPacket(
     if n == 0 then "expected no arguments"
     else if n == 1 then "expected " + tostring(n) + " argument"
     else "expected " + tostring(n) + " arguments"
     );
export WrongNumArgs(name:string,m:int,n:int):Expr := (
     if n == m+1
     then buildErrorPacket(quoteit(name) + " expected " 
	  + tostring(m) + " or "
	  + tostring(n) + " arguments")
     else buildErrorPacket(quoteit(name) + " expected " 
	  + tostring(m) + " to "
	  + tostring(n) + " arguments"));
export WrongNumArgs(m:int,n:int):Expr := (
     if n == m+1
     then buildErrorPacket("expected " + tostring(m) + " or " + tostring(n) + " arguments")
     else buildErrorPacket("expected " + tostring(m) + " to " + tostring(n) + " arguments"));
export TooFewArgs(name:string,m:int):Expr := (
     if m == 1
     then buildErrorPacket(quoteit(name) + " expected at least 1 argument")
     else buildErrorPacket(quoteit(name) + " expected at least " 
	  + tostring(m) + " arguments"));
export TooManyArgs(name:string,m:int):Expr := (
     if m == 1
     then buildErrorPacket(quoteit(name) + " expected at most 1 argument")
     else buildErrorPacket(quoteit(name) + " expected at most " 
	  + tostring(m) + " arguments"));
export ErrorDepth := 0;
export printErrorMessage(e:Code,message:string):Expr := (
     p := codePosition(e);
     if int(p.LoadDepth) >= ErrorDepth
     then (
     	  printErrorMessage(p,message);
     	  Expr(Error(p,message,emptySequence,nullE)))
     else buildErrorPacket(message));
export printErrorMessage(e:Code,message:string,report:Expr):Expr := (
     p := codePosition(e);
     if int(p.LoadDepth) >= ErrorDepth
     then (
     	  printErrorMessage(p,message);
     	  Expr(Error(p,message,report,nullE)))
     else buildErrorPacket(message));
use engine;


dummyBreakLoop(f:Frame):bool := false;
export breakLoopFun := dummyBreakLoop;
export debuggingMode := false;

export eval(c:Code):Expr;
hadError := false;
errm := nullE;
export evalSequence(v:CodeSequence):Expr := (
     n := length(v);
     if n == 0 then Expr(emptySequence)
     else if n == 1 then (
	  x := eval(v.0);
	  when x is Error do x
	  else Expr(Sequence(x))
	  )
     else if n == 2 then (
	  x := eval(v.0);
	  when x is Error do x
	  else (
	       y := eval(v.1);
	       when y is Error do y
	       else Expr(Sequence(x,y))))
     else (
	  r := Expr(new Sequence len n do (
		    foreach c in v do (
			 value := eval(c);
			 when value 
			 is Error do (
			      hadError = true;
			      errm = value;
			      while true do provide nullE;
			      )
			 else nothing;
			 provide value;
			 )));
	  if hadError then (
	       r = errm;
	       errm = nullE;
	       hadError = false;
	       );
	  r));
export trace := false;
NumberErrorMessagesShown := 0;
export recursionlimit := 300;
export recursiondepth := 0;

printtop := 13;
printbottom := 7;

export eval(c:Code):Expr := (
     spincursor();
     e := 
     if interrupted then
     if alarmed then (
	  interrupted = false;
	  alarmed = false;
	  printErrorMessage(c,"alarm occurred"))
     else (
	  interrupted = false;
     	  SuppressErrors = false;
	  printErrorMessage(c,"interrupted"))
     else when c
     is n:exprCode do (
	  --couldtrace=false; 
	  n.v)
     is var:variableCode do frame(var.v.frameID).values.(var.v.frameindex)
     is r:localMemoryReferenceCode do frameWithNestingDepth(r.frameID,r.nestingDepth).values.(r.frameindex)
     is r:staticMemoryReferenceCode do globalFrame.values.(r.frameindex)
     is u:unaryCode do u.f(u.rhs)
     is b:binaryCode do b.f(b.lhs,b.rhs)
     is m:functionCode do Expr(FunctionClosure(localFrame, m))
     is a:localAssignmentCode do LocalAssignmentFun(a)
     is a:globalAssignmentCode do GlobalAssignmentFun(a)
     is p:parallelAssignmentCode do ParallelAssignmentFun(p)
     is b:ternaryCode do b.f(b.arg1,b.arg2,b.arg3)
     is b:multaryCode do b.f(b.args)
     is n:forCode do (
	  localFrame = Frame(localFrame,n.dictionary.frameID,n.dictionary.framesize,
	       new Sequence len n.dictionary.framesize do provide nullE);
	  x := ForFun(n);
	  localFrame = localFrame.outerFrame;
	  x)
     is n:openDictionaryCode do (
	  localFrame = Frame(localFrame,n.dictionary.frameID,n.dictionary.framesize,
	       new Sequence len n.dictionary.framesize do provide nullE);
	  x := eval(n.body);
	  localFrame = localFrame.outerFrame;
	  x)
     is v:CodeSequence do evalSequence(v);
     when e is err:Error do (
	  -- stderr << "err: " << err.position << " : " << err.message << endl;
	  if err.message == returnMessage || err.message == breakMessage then return(e);
	  p := codePosition(c);
	  -- stderr << "pos: " << p << endl;
	  err.report = seq(
	       list(Expr(p.filename),
		    Expr(toInteger(int(p.line))),
		    Expr(toInteger(int(p.column)+1))),
	       err.report);
     	  if err.position == dummyPosition
	  && int(p.LoadDepth) >= ErrorDepth 
	  && !SuppressErrors then (
	       interrupted = false;
	       alarmed = false;
	       if NumberErrorMessagesShown < printtop || recursiondepth < printbottom then (
		    printErrorMessage(p,err.message);
		    if recursiondepth < printbottom
		    then NumberErrorMessagesShown = 0 
		    else NumberErrorMessagesShown = NumberErrorMessagesShown + 1;
		    )
	       else if recursiondepth == printbottom then (
		    flush(stdout);
		    stderr << "..." << endl;);
	       err.position = p;
	       );
	  e)
     else e);
export setup(word:Word):void := (
     makeSymbol(word,dummyPosition,globalDictionary);
     );
export setup(word:Word,fn:unop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fn;
     );
export setup(word:Word,fn:binop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.binary = fn;
     );
export setup(word:Word,fun1:unop,fun2:binop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fun1;
     e.binary = fun2;
     );
export setup(word:Word,fun1:unop,fun2:unop):void := (
     e := makeSymbol(word,dummyPosition,globalDictionary);
     e.unary = fun1;
     e.postfix = fun2;
     );
export setup(e:SymbolClosure,fn:unop):void := (
     e.symbol.unary = fn;
     );
export setuppostfix(e:SymbolClosure,fn:unop):void := (
     e.symbol.postfix = fn;
     );
export setup(e:SymbolClosure,fn:binop):void := (
     e.symbol.binary = fn;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:binop):void := (
     e.symbol.unary = fun1;
     e.symbol.binary = fun2;
     );
export setup(e:SymbolClosure,fun1:unop,fun2:unop):void := (
     e.symbol.unary = fun1;
     e.symbol.postfix = fun2;
     );
export setupop(s:SymbolClosure,fun:unop):void := s.symbol.unary = fun;
export setupfun(name:string,fun:unop):void := (
     word := makeUniqueWord(name,
	  parseinfo(precSpace,precSpace,precSpace,parsefuns(unaryop, defaultbinary)));
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     entry.unary = fun;
     entry.protected = true;
     );     
export setupfun(name:string,value:fun):void := (
     word := makeUniqueWord(name,parseWORD);
     entry := makeSymbol(word,dummyPosition,globalDictionary);
     globalFrame.values.(entry.frameindex) = Expr(CompiledFunction(value,nextHash()));
     entry.protected = true;
     );
export setupvar(name:string,value:Expr):Symbol := (
     word := makeUniqueWord(name,parseWORD);
     when lookup(word,globalDictionary)
     is null do (
     	  entry := makeSymbol(word,dummyPosition,globalDictionary);
     	  globalFrame.values.(entry.frameindex) = value;
	  entry)
     is entry:Symbol do (
	  -- we are doing it again after loading data with loaddata()
	  -- or we are reassigning to o or oo in interpret.d
     	  globalFrame.values.(entry.frameindex) = value;
	  entry));
export setupconst(name:string,value:Expr):Symbol := (
     s := setupvar(name,value);
     s.protected = true;
     s);
setup(commaW,dummyBinaryFun);

shieldfun(a:Code):Expr := (
     if interruptShield then eval(a)
     else (
     	  interruptPending = interrupted;
     	  interruptShield = true;
     	  ret := eval(a);
     	  interruptShield = false;
     	  interrupted = interruptPending;
	  if interrupted && !stdIO.inisatty then (
	       stderr << "interrupted" << endl;
	       exit(1);
	       );
     	  ret));
setupop(shieldS,shieldfun);     

returnFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,returnMessage,emptySequenceE,e)));
setupop(returnS,returnFun);

breakFun(a:Code):Expr := (
     e := if a == dummyCode then nullE else eval(a);
     when e is Error do e else Expr(Error(dummyPosition,breakMessage,emptySequenceE,e)));
setupop(breakS,breakFun);
