--		Copyright 1994 by Daniel R. Grayson
use binding;

dummyMultaryFun(c:CodeSequence):Expr := (
     error("dummy multary function called");
     nullE);

export AssignElemFun := dummyTernaryFun;	-- filled
export AssignQuotedElemFun := dummyTernaryFun;	-- filled
export NewFun := dummyUnaryFun;	  -- filled in later
export NewFromFun := dummyBinaryFun;	  -- filled in later
export NewOfFun := dummyBinaryFun;	  -- filled in later
export NewOfFromFun := dummyTernaryFun;	  -- filled in later

export AssignNewFun := dummyBinaryFun;
export AssignNewOfFun := dummyTernaryFun;
export AssignNewFromFun := dummyTernaryFun;
export AssignNewOfFromFun := dummyMultaryFun;

export InstallMethodFun := dummyMultaryFun;
export UnaryInstallMethodFun := dummyTernaryFun;

export InstallValueFun := dummyMultaryFun;
export UnaryInstallValueFun := dummyTernaryFun;

convert(e:ParseTree):Code;
CodeSequenceLength(e:ParseTree,separator:Word):int := (
     i := 0;
     while true do (
     	  when e
     	  is b:Binary do (
	       if b.Operator.word == separator
	       then ( i = i + CodeSequenceLength(b.rhs,separator); e = b.lhs )
	       else return i+1)
	  is u:Unary do (
	       if u.Operator.word == separator
	       then ( i = i + 1; e = u.rhs )
	       else return i+1)
	  else return i+1));
fillCodeSequence(e:ParseTree,v:CodeSequence,m:int,separator:Word):int := (
     -- Start filling v, in reverse, at position m-1, return the index of the last position filled.
     -- We do it in reverse, because our comma operator is left associative (to prevent filling the parser stack),
     -- and because we don't want to fill the stack with recursive calls to this function.
     while true do
     when e
     is b:Binary do (
	  if b.Operator.word == separator
	  then (m=fillCodeSequence(b.rhs,v,m,separator); e=b.lhs )
	  else (m=m-1; v.m=convert(e); return m))
     is u:Unary do (
	  if u.Operator.word == separator
	  then (m=fillCodeSequence(u.rhs,v,m,separator); m=m-1; v.m=Code(nullCode()); return m)
	  else (m=m-1; v.m=convert(e); return m))
     is p:EmptyParentheses do ((m=m-1; v.m=convert(e); return m))
     is dummy do (m=m-1; v.m=Code(nullCode()); return m)
     is p:Parentheses do ((m=m-1; v.m=convert(e); return m))
     else (m=m-1; v.m=convert(e); return m));
makeCodeSequence(e:ParseTree,separator:Word):CodeSequence := (
     v := new CodeSequence len CodeSequenceLength(e,separator) do provide dummyCode;
     fillCodeSequence(e,v,length(v),separator);
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
	       return i;
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

nestingDepth(frameID:int,d:Dictionary):int := (
     if frameID == 0 then return -1;
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
     nd := new array(int) len n do foreach x in symbols do provide nestingDepth(x.frameID,b.Operator.dictionary);
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

export convert(e:ParseTree):Code := (
     when e
     is s:StartDictionary do (
	  if s.dictionary.framesize != 0
	  then Code(newLocalFrameCode(s.dictionary.frameID,s.dictionary.framesize,convert(s.body)))
	  else convert(s.body)
	  )
     is w:For do Code(
	  forCode(
	       convert(w.inClause), convert(w.fromClause), convert(w.toClause),
	       convert(w.whenClause), convert(w.listClause), 
	       convert(w.doClause),
	       w.dictionary.frameID,
	       w.dictionary.framesize,
	       treePosition(e)))
     is w:WhileDo do Code(whileDoCode(convert(w.predicate),convert(w.doClause),treePosition(e)))
     is w:WhileList do Code(whileListCode(convert(w.predicate),convert(w.listClause),treePosition(e)))
     is w:WhileListDo do Code(whileListDoCode(convert(w.predicate),convert(w.listClause),convert(w.doClause),treePosition(e)))
     is n:New do (
	  if n.newparent == dummyTree
	  then if n.newinitializer == dummyTree
	       then Code(newCode(convert(n.newclass),treePosition(e)))
	       else Code(newFromCode(convert(n.newclass),convert(n.newinitializer),treePosition(e)))
	  else if n.newinitializer == dummyTree
	       then Code(newOfCode(convert(n.newclass),convert(n.newparent),treePosition(e)))
	       else Code(newOfFromCode(convert(n.newclass),convert(n.newparent),convert(n.newinitializer),treePosition(e))))
     is i:IfThen do Code(ifCode(convert(i.predicate),convert(i.thenclause),NullCode,treePosition(e)))
     is i:IfThenElse do Code(ifCode(convert(i.predicate),convert(i.thenclause),convert(i.elseClause),treePosition(e)))
     is token:Token do (
	  var := token.entry;
	  wrd := token.word;
	  if wrd.typecode == TCRR
	  then Code(realCode(parseRR(wrd.name),position(token)))
	  else if wrd.typecode == TCint
	  then Code(integerCode(parseInt(wrd.name),position(token)))
 	  else if wrd.typecode == TCstring
	  then (
	       s := parseString(wrd.name);
	       Code(stringCode(s))
	       )
	  else (
	       if var.frameID == 0
	       then (
		    if var.thread
		    then Code(threadMemoryReferenceCode(var.frameindex,position(token)))
		    else Code(globalMemoryReferenceCode(var.frameindex,position(token)))
		    )
	       else Code(localMemoryReferenceCode(nestingDepth(var.frameID,token.dictionary),var.frameindex,position(token)))
	       )
	  )
     is a:Adjacent do Code(adjacentCode(convert(a.lhs),convert(a.rhs),treePosition(e)))
     is p:EmptyParentheses do (
	  if p.left.word == leftparen then Code(sequenceCode(CodeSequence(),treePosition(e)))
	  else if p.left.word == leftbrace then Code(listCode(CodeSequence(),treePosition(e)))
	  else if p.left.word == leftbracket then Code(arrayCode(CodeSequence(),treePosition(e)))
	  else if p.left.word == leftAngleBar then Code(angleBarListCode(CodeSequence(),treePosition(e)))
	  else dummyCode			  -- should not happen
	  )
     is p:Parentheses do (
	  if p.left.word == leftparen then convert(p.contents)
	  else if p.left.word == leftbrace 
	  then Code(listCode(makeCodeSequence(p.contents,CommaW),treePosition(e)))
	  else 
	  if p.left.word == leftbracket 
	  then Code(arrayCode(makeCodeSequence(p.contents,CommaW),treePosition(e)))
	  else 
	  if p.left.word == leftAngleBar
	  then Code(angleBarListCode(makeCodeSequence(p.contents,CommaW),treePosition(e)))
	  else 
	  dummyCode			  -- should not happen
	  )
     is b:Binary do (
	  if b.Operator.entry == DotS.symbol
	  || b.Operator.entry == DotQuestionS.symbol
	  then (
	       when b.rhs
	       is token:Token do (
	  	    wrd := token.word;
		    var := token.entry;
		    if wrd.typecode == TCid
		    then (
	       		 Code(binaryCode(
			 	   b.Operator.entry.binary,
			 	   convert(b.lhs),
	       	    	 	   Code(globalSymbolClosureCode(var,treePosition(b.rhs))),
			 	   treePosition(e)
				   )
			      )
			 )
		    else dummyCode	  -- should not occur
		    )
	       else dummyCode		  -- should not occur
	       )
	  else if b.Operator.word == CommaW
	  then Code(sequenceCode(makeCodeSequence(e,CommaW),treePosition(e)))
	  else if b.Operator.word == SemicolonW
	  then Code(semiCode(makeCodeSequence(e,SemicolonW),treePosition(e)))
	  else if b.Operator.word == EqualW
	  then (
	       when b.lhs
	       is a:Adjacent do (
		    Code(
			 multaryCode(
			      InstallValueFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyPosition)),
			      	   convert(a.lhs),
			      	   convert(a.rhs),
			      	   convert(b.rhs)),
			      treePosition(e))))
	       is u:Unary do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,position(u.Operator))),
			 convert(u.rhs),
			 convert(b.rhs),
			 treePosition(e)))
	       is u:Postfix do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,position(u.Operator))),
			 convert(u.lhs),
			 convert(b.rhs),
			 treePosition(e)))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, convert(c.lhs),
			      convert(c.rhs), convert(b.rhs), treePosition(e)))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,position(crhs))),
				   convert(b.rhs),
				   treePosition(e)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallValueFun,
			      CodeSequence(
				   Code(globalSymbolClosureCode(c.Operator.entry,position(c.Operator))), 
				   convert(c.lhs),
				   convert(c.rhs),
				   convert(b.rhs)),
			      treePosition(e))))
	       is t:Token do tokenAssignment(e,b,t)
	       is p:Parentheses do parallelAssignment(e,b,p)
	       else dummyCode		  -- should not happen
	       )
	  else if b.Operator.word == ColonEqualW
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
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyPosition)),
			      	   convert(a.lhs),
			      	   convert(a.rhs),
			      	   convert(b.rhs)),
			      treePosition(e))))
	       is u:Unary do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,position(u.Operator))),
			 convert(u.rhs), convert(b.rhs), treePosition(e)))
	       is u:Postfix do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,position(u.Operator))),
			 convert(u.lhs), convert(b.rhs), treePosition(e)))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, convert(c.lhs),
			      convert(c.rhs), convert(b.rhs), treePosition(e)))
		    else if c.Operator.entry == UnderscoreS.symbol
		    then Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence( 
			      	   Code(globalSymbolClosureCode(UnderscoreS.symbol,dummyPosition)),
				   convert(c.lhs),
				   convert(c.rhs),
			      	   convert(b.rhs)),
			      treePosition(e)))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,position(crhs))),
				   convert(b.rhs),
				   treePosition(e)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			 	   Code(globalSymbolClosureCode(c.Operator.entry,position(c.Operator))),
				   convert(c.lhs),
				   convert(c.rhs),
				   convert(b.rhs)),
			      treePosition(e))))
	       is t:Token do tokenAssignment(e,b,t)
	       is p:Parentheses do parallelAssignment(e,b,p)
	       else dummyCode		  -- should not happen
	       )
	  else Code(binaryCode(b.Operator.entry.binary,convert(b.lhs),
	       	    convert(b.rhs),treePosition(e)))
	  )
     is a:Arrow do Code(functionCode(
	       a.Operator,		  -- just for display purposes!
	       convert(a.rhs),a.desc,nextHash()))
     is u:Unary do (
	  if u.Operator.word == CommaW
	  then Code(sequenceCode(makeCodeSequence(e,CommaW),treePosition(e)))
	  else if u.Operator.word == SemicolonW
	  then Code(semiCode(makeCodeSequence(e,SemicolonW),treePosition(e)))
	  else Code(unaryCode(u.Operator.entry.unary,convert(u.rhs),treePosition(e))))
     is q:Quote do (
	  token := q.rhs;
	  sym := token.entry;
	  pos := treePosition(e);
	  if sym.frameID == 0
	  then (
	       if sym.thread
	       then Code(threadSymbolClosureCode(sym,pos))
	       else Code(globalSymbolClosureCode(sym,pos))
	       )
	  else Code(localSymbolClosureCode(nestingDepth(sym.frameID,token.dictionary),sym,pos)))
     is q:GlobalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  pos := treePosition(e);
     	  Code(globalSymbolClosureCode(sym,pos)))
     is q:ThreadQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  pos := treePosition(e);
     	  Code(threadSymbolClosureCode(sym,pos)))
     is q:LocalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  pos := treePosition(e);
	  nd := nestingDepth(sym.frameID,token.dictionary);
	  Code(localSymbolClosureCode(nd,sym,pos)))
     is i:TryThenElse do Code(tryCode(convert(i.primary),convert(i.sequel),convert(i.alternate),treePosition(e)))
     is i:TryElse do Code(tryCode(convert(i.primary),NullCode,convert(i.alternate),treePosition(e)))
     is i:Try do Code(tryCode(convert(i.primary),NullCode,NullCode,treePosition(e)))
     is i:Catch do Code(catchCode(convert(i.primary),treePosition(e)))
     is u:Postfix do Code(unaryCode(u.Operator.entry.postfix,convert(u.lhs),treePosition(e)))
     is d:dummy do dummyCode
     );

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d convertr.o "
-- End:
