--		Copyright 1994 by Daniel R. Grayson
use binding;
use common;
use util;

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

convert0(e:ParseTree):Code;
convert(e:ParseTree):Code;
unseq(c:Code):Code;
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

tokenAssignment(t:Token,p:ParseTree):Code := (
     c:=convert0(p);
     l:=combinePositionR(t.position, codePosition(c));
     c=unseq(c);
     if t.entry.frameID == 0
     then Code(globalAssignmentCode(t.entry,c,l))
     else Code(localAssignmentCode(nestingDepth(t.entry.frameID,t.dictionary),t.entry.frameindex,c,l))
     );

parallelAssignment(par:Parentheses,rhs:ParseTree,d:Dictionary):Code := (
     symbols := makeSymbolSequence(ParseTree(par)); -- silly -- rethink
     n := length(symbols);
     nd := new array(int) len n do foreach x in symbols do provide nestingDepth(x.frameID,d); -- rethink dictionary
     fr := new array(int) len n do foreach x in symbols do provide x.frameindex;
     foreach x in symbols do if x.frameID != 0 then x = dummySymbol;
     Code(parallelAssignmentCode(
	       nd,
	       fr,
	       symbols,
	       unseq(c:=convert0(rhs)),
	       combinePositionR(par.left.position, codePosition(c))
	       ))
     );

export unseq(c:Code):Code := (
     when c
     is s:sequenceCode do (
     if length(s.x)==1 then s.x.0 else c
     )
     else c
     );

convertParentheses(seq:CodeSequence, word:Word, pos:Position):Code := (
    if word == leftparen    then Code(sequenceCode(seq,     pos)) else
    if word == leftbrace    then Code(listCode(seq,         pos)) else
    if word == leftbracket  then Code(arrayCode(seq,        pos)) else
    if word == leftAngleBar then Code(angleBarListCode(seq, pos)) else
    dummyCode -- should not happen
    );

export convert0(e:ParseTree):Code := (
     when e
     is w:For do (
       c:=convert0(w.doClause);
       cc:=convert0(w.listClause);
       loc:=codePosition(c);
       when c is
         nullCode do loc=codePosition(cc)
	 else nothing;
       Code(
	  forCode(
	       convert(w.inClause), convert(w.fromClause), convert(w.toClause),
	       convert(w.whenClause), unseq(cc),
	       unseq(c),
	       w.dictionary.frameID,
	       w.dictionary.framesize,
	       combinePositionR(w.forToken.position, loc)
	  )))
    is w:WhileDo     do Code(whileDoCode(    convert(w.predicate), unseq(c:=convert0(w.doClause)),                                  combinePositionR(w.whileToken.position, codePosition(c))))
    is w:WhileList   do Code(whileListCode(  convert(w.predicate), unseq(c:=convert0(w.listClause)),                                combinePositionR(w.whileToken.position, codePosition(c))))
    is w:WhileListDo do Code(whileListDoCode(convert(w.predicate),           convert(w.listClause), unseq(c:=convert0(w.doClause)), combinePositionR(w.whileToken.position, codePosition(c))))
    is n:New do (
	if n.newparent == dummyTree
	then if n.newinitializer == dummyTree
	then Code(newCode(unseq(c:=convert0(n.newclass)),                                      combinePositionR(n.newtoken.position, codePosition(c))))
	else Code(newFromCode(      convert(n.newclass), unseq(c:=convert0(n.newinitializer)), combinePositionR(n.newtoken.position, codePosition(c))))
	else if n.newinitializer == dummyTree
	then Code(newOfCode(    convert(n.newclass), unseq(c:=convert0(n.newparent)),                                      combinePositionR(n.newtoken.position, codePosition(c))))
	else Code(newOfFromCode(convert(n.newclass),           convert(n.newparent), unseq(c:=convert0(n.newinitializer)), combinePositionR(n.newtoken.position, codePosition(c)))))
     is token:Token do (
	  var := token.entry;
	  wrd := token.word;
	  if wrd.typecode == TCRR
	  then (
	       x:= parseRR(wrd.name);
	       when x
	       is y:RR do Code(realCode(y, token.position))
	       is null do Code(Error(token.position,
		       "expected precision to be a small non-negative integer",
		       nullE,false,dummyFrame)))
	  else if wrd.typecode == TCint
	  then Code(integerCode(parseInt(wrd.name),token.position))
 	  else if wrd.typecode == TCstring
	  then (
	       s := parseString(wrd.name);
	       Code(stringCode(s,token.position))
	       )
	  else (
	       if var.frameID == 0
	       then (
		    if var.thread
		    then Code(threadMemoryReferenceCode(var.frameindex,token.position))
		    else Code(globalMemoryReferenceCode(var.frameindex,token.position))
		    )
	       else Code(localMemoryReferenceCode(nestingDepth(var.frameID,token.dictionary),var.frameindex,token.position))
	       )
	  )
    is p:EmptyParentheses do convertParentheses(CodeSequence(),
	p.left.word, treePosition(e))
    is p:Parentheses do convertParentheses(makeCodeSequence(p.contents, CommaW),
	p.left.word, treePosition(e))
    is a:Adjacent do Code(adjacentCode(unseq(c:=convert0(a.lhs)), unseq(cc:=convert0(a.rhs)), combinePositionAdjacent(codePosition(c), codePosition(cc))))
    is a:Arrow do (
	fc := functionCode(convert(a.rhs), a.desc, hash_t(0), treePosition(e));
	fc.hash = hashFromAddress(Expr(fc));
	Code(fc))
     is b:Binary do (
	  if b.Operator.entry == DotS.symbol
	  || b.Operator.entry == DotQuestionS.symbol
	  then (
	       when b.rhs
	       is token:Token do (
	  	    wrd := token.word;
		    var := token.entry;
		    p := token.position;
		    if wrd.typecode == TCid
		    then (
	       		 Code(binaryCode(
			 	   b.Operator.entry.binary,
			 	   unseq(c:=convert0(b.lhs)),
	       	    	 	   Code(globalSymbolClosureCode(var,p)),
				   combinePositionC(codePosition(c), p, b.Operator.position)
				   )
			      )
			 )
		    else dummyCode	  -- should not occur
		    )
	       else dummyCode		  -- should not occur
	       )
	  else if b.Operator.word == CommaW
	  then Code(sequenceCode(s:=makeCodeSequence(e, CommaW),     combinePositionL(codePosition(s.0), codePosition(s.(length(s)-1)))))
	  else if b.Operator.word == SemicolonW
	  then Code(semiCode(    s:=makeCodeSequence(e, SemicolonW), combinePositionL(codePosition(s.0), codePosition(s.(length(s)-1)))))
	  else if b.Operator.word == EqualW
	  then (
	       when b.lhs
	       is a:Adjacent do (
		    Code(
			 multaryCode(
			      InstallValueFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyPosition)),
			      	   unseq(c:=convert0(a.lhs)),
			      	   convert(a.rhs),
			      	   unseq(cc:=convert0(b.rhs))),
			      combinePositionC(codePosition(c), codePosition(cc), b.Operator.position))))
	       is u:Unary do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,p:=u.Operator.position)),
			 convert(u.rhs),
			 unseq(cc:=convert0(b.rhs)),
			 combinePositionC(p, codePosition(cc), b.Operator.position)))
	       is u:Postfix do Code(
		    ternaryCode(
			 UnaryInstallValueFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,u.Operator.position)),
			 unseq(c:=convert0(u.lhs)),
			 unseq(cc:=convert0(b.rhs)),
			 combinePositionC(codePosition(c), codePosition(cc), b.Operator.position)))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, c1:=convert(c.lhs),
			    convert(c.rhs), unseq(c2:=convert0(b.rhs)), combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   c1:=convert(c.lhs),
			 	   Code(globalSymbolClosureCode(crhs.entry,crhs.position)),
				   unseq(c2:=convert0(b.rhs)),
				   combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallValueFun,
			      CodeSequence(
				   Code(globalSymbolClosureCode(c.Operator.entry,c.Operator.position)), 
				   c1:=convert(c.lhs),
				   convert(c.rhs),
				   unseq(c2:=convert0(b.rhs))),
			      combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position))))
	       is t:Token do tokenAssignment(t,b.rhs)
	       is p:Parentheses do parallelAssignment(p,b.rhs,b.Operator.dictionary)
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
			      unseq(c:=convert0(b.rhs)), 
			      combinePositionC(n.newtoken.position, codePosition(c), b.Operator.position)))
		    else Code(ternaryCode(
			      AssignNewFromFun,
			      convert(n.newclass),
			      convert(n.newinitializer),
			      unseq(c:=convert0(b.rhs)),
			      combinePositionC(n.newtoken.position, codePosition(c), b.Operator.position)))
     	       	    else if n.newinitializer == dummyTree 
		    then Code(ternaryCode(
			      AssignNewOfFun,
			      convert(n.newclass),
			      convert(n.newparent),
			      unseq(c:=convert0(b.rhs)),
			      combinePositionC(n.newtoken.position, codePosition(c), b.Operator.position)))
		    else Code(multaryCode(
			      AssignNewOfFromFun,
			      CodeSequence(
				   convert(n.newclass),
				   convert(n.newparent),
				   convert(n.newinitializer),
				   unseq(c:=convert0(b.rhs))),
			      combinePositionC(n.newtoken.position, codePosition(c), b.Operator.position))))
	       is a:Adjacent do (
		    Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			      	   Code(globalSymbolClosureCode(AdjacentS.symbol,dummyPosition)),
			      	   unseq(c:=convert0(a.lhs)),
			      	   convert(a.rhs),
			      	   unseq(cc:=convert0(b.rhs))),
			      combinePositionC(codePosition(c), codePosition(cc), b.Operator.position))))
	       is u:Unary do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,p:=u.Operator.position)),
			 convert(u.rhs), unseq(c:=convert0(b.rhs)), combinePositionC(p, codePosition(c), b.Operator.position)))
	       is u:Postfix do Code(ternaryCode(
			 UnaryInstallMethodFun,
			 Code(globalSymbolClosureCode(u.Operator.entry,u.Operator.position)),
			 unseq(c:=convert0(u.lhs)), unseq(cc:=convert0(b.rhs)), combinePositionC(codePosition(c), codePosition(cc), b.Operator.position)))
	       is c:Binary do (
		    if c.Operator.entry == SharpS.symbol
		    then Code(ternaryCode( AssignElemFun, c1:=convert(c.lhs),
			    convert(c.rhs), unseq(c2:=convert0(b.rhs)), combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
		    else if c.Operator.entry == UnderscoreS.symbol
		    then Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence( 
			      	   Code(globalSymbolClosureCode(UnderscoreS.symbol,dummyPosition)),
				   unseq(c1:=convert0(c.lhs)),
				   convert(c.rhs),
			      	   unseq(c2:=convert0(b.rhs))),
			      combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
		    else if c.Operator.entry == DotS.symbol
		    then (
			 when c.rhs
			 is crhs:Token do
			 Code(ternaryCode(
				   AssignElemFun,
				   unseq(c1:=convert0(c.lhs)),
			 	   Code(globalSymbolClosureCode(crhs.entry,crhs.position)),
				   unseq(c2:=convert0(b.rhs)),
				   combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
			 else dummyCode --should not happen
			 )
		    else Code(multaryCode(
			      InstallMethodFun,
			      CodeSequence(
			 	   Code(globalSymbolClosureCode(c.Operator.entry,c.Operator.position)),
				   unseq(c1:=convert0(c.lhs)),
				   convert(c.rhs),
				   unseq(c2:=convert0(b.rhs))),
			      combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	       )
	       is t:Token do tokenAssignment(t,b.rhs)
	       is p:Parentheses do parallelAssignment(p,b.rhs,b.Operator.dictionary)
	       else dummyCode		  -- should not happen
	       )
	  else if isAugmentedAssignmentOperatorWord(b.Operator.word)
	  then (
	      when b.lhs
	      is a:Adjacent
	      do Code(augmentedAssignmentCode(b.Operator.entry, unseq(c1:=convert0(b.lhs)),
		      unseq(c2:=convert0(b.rhs)), AdjacentS.symbol, combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	      is u:Unary
	      do Code(augmentedAssignmentCode(b.Operator.entry, unseq(c1:=convert0(b.lhs)),
		      unseq(c2:=convert0(b.rhs)), u.Operator.entry, combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	      is u:Postfix
	      do Code(augmentedAssignmentCode(b.Operator.entry, unseq(c1:=convert0(b.lhs)),
		      unseq(c2:=convert0(b.rhs)), u.Operator.entry, combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	      is c:Binary
	      do Code(augmentedAssignmentCode(b.Operator.entry, unseq(c1:=convert0(b.lhs)),
		      unseq(c2:=convert0(b.rhs)), c.Operator.entry, combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	      is t:Token
	      do Code(augmentedAssignmentCode(b.Operator.entry, unseq(c1:=convert0(b.lhs)),
		      unseq(c2:=convert0(b.rhs)),          t.entry, combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	      else Code(augmentedAssignmentCode(b.Operator.entry, dummyCode,
		      dummyCode, dummySymbol, dummyPosition)) -- CHECK
		      )
	  else Code(binaryCode(b.Operator.entry.binary,unseq(c1:=convert0(b.lhs)),
		  unseq(c2:=convert0(b.rhs)), combinePositionC(codePosition(c1), codePosition(c2), b.Operator.position)))
	  )
     is u:Unary do (
	  if u.Operator.word == CommaW
	  then Code(sequenceCode(s:=makeCodeSequence(e, CommaW),     combinePositionL(u.Operator.position, codePosition(s.(length(s)-1)))))
	  else if u.Operator.word == SemicolonW
	  then Code(semiCode(    s:=makeCodeSequence(e, SemicolonW), combinePositionL(u.Operator.position, codePosition(s.(length(s)-1)))))
	  else (
	    c:=convert0(u.rhs);
	    loc:=codePosition(c);
	    loc2:=u.Operator.position;
	    when c is
              nullCode do nothing
	    else loc2 = combinePositionL(loc2, loc);
	  Code(unaryCode(u.Operator.entry.unary,unseq(c),loc2))))
     is q:Quote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combinePositionR(q.Operator.position, token.position);
	  if sym.frameID == 0
	  then (
	       if sym.thread
	       then Code(threadSymbolClosureCode(sym,p))
	       else Code(globalSymbolClosureCode(sym,p))
	       )
	  else Code(localSymbolClosureCode(nestingDepth(sym.frameID,token.dictionary),sym,p)))
     is q:GlobalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combinePositionR(q.Operator.position, token.position);
     	  Code(globalSymbolClosureCode(sym,p)))
     is q:ThreadQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combinePositionR(q.Operator.position, token.position);
     	  Code(threadSymbolClosureCode(sym,p)))
     is q:LocalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  p := combinePositionR(q.Operator.position, token.position);
	  nd := nestingDepth(sym.frameID,token.dictionary);
	  Code(localSymbolClosureCode(nd,sym,p)))
    is i:IfThen      do Code(ifCode(convert(i.predicate), convert(i.thenClause), NullCode,              treePosition(e)))
    is i:IfThenElse  do Code(ifCode(convert(i.predicate), convert(i.thenClause), convert(i.elseClause), treePosition(e)))
    is i:Try         do Code(tryCode(convert(i.primary), NullCode,          NullCode,             treePosition(e)))
    is i:TryThen     do Code(tryCode(convert(i.primary), convert(i.sequel), NullCode,             treePosition(e)))
    is i:TryThenElse do Code(tryCode(convert(i.primary), convert(i.sequel), convert(i.alternate), treePosition(e)))
    is i:TryElse     do Code(tryCode(convert(i.primary), NullCode,          convert(i.alternate), treePosition(e)))
    is i:Catch       do Code(catchCode(convert(i.primary), treePosition(e)))
    is u:Postfix     do Code(unaryCode(u.Operator.entry.postfix, unseq(c:=convert0(u.lhs)), combinePositionR(codePosition(c), u.Operator.position)))
     is d:dummy do dummyCode
     );

export convert(e:ParseTree):Code := unseq(convert0(e));


-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d convertr.o "
-- End:
