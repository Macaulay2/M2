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

convertTokenReference(token:Token):Code := (
    wrd := token.word;
    var := token.entry;
    pos := token.position;
    if wrd.typecode == TCint    then Code(integerCode(parseInt(wrd.name),   pos)) else
    if wrd.typecode == TCstring then Code(stringCode(parseString(wrd.name), pos)) else
    if wrd.typecode == TCRR
    then (
	when parseRR(wrd.name)
	is y:RR do Code(realCode(y, pos))
	is null do Code(Error(
		pos, "expected precision to be a small non-negative integer", nullE, false, dummyFrame)))
    else (
	if var.frameID == 0 then
	if var.thread
	then Code(threadMemoryReferenceCode(var.frameindex, pos))
	else Code(globalMemoryReferenceCode(var.frameindex, pos))
	else Code(localMemoryReferenceCode(nestingDepth(var.frameID, token.dictionary), var.frameindex, pos)))
    );

convertTokenAssignment(token:Token, rhs:ParseTree):Code := (
    sym := token.entry;
    val := convert(rhs);
    pos := combinePositionR(token.position, treePosition(rhs));
    if sym.frameID == 0
    then Code(globalAssignmentCode(sym, val, pos))
    else Code(localAssignmentCode(
	    nestingDepth(sym.frameID, token.dictionary), sym.frameindex, val, pos))
    );

convertParallelAssignment(par:Parentheses,rhs:ParseTree,d:Dictionary):Code := (
    syms := makeSymbolSequence(ParseTree(par)); -- silly -- rethink
    vals := convert(rhs);
    pos := combinePositionR(par.left.position, treePosition(rhs));
    n := length(syms);
    nd := new array(int) len n do foreach x in syms do provide nestingDepth(x.frameID, d); -- rethink dictionary
    fr := new array(int) len n do foreach x in syms do provide x.frameindex;
    foreach x in syms do if x.frameID != 0 then x = dummySymbol;
    Code(parallelAssignmentCode(nd, fr, syms, vals, pos))
    );

export convertGlobalOperator(oper:Token):Code := Code(globalSymbolClosureCode(oper.entry, oper.position));
export convertGlobalOperator(sym:Symbol):Code := Code(globalSymbolClosureCode(sym, dummyPosition));

convertUnaryInstallCode(f:ternop, opcode:Code, lhs:Code, rhs:Code, pos:Position):Code := (
    Code(ternaryCode(f, opcode, lhs, rhs, pos)));

convertBinaryInstallCode(f:ternop, lhs:Code, key:Code, rhs:Code, pos:Position):Code := (
    Code(ternaryCode(f, lhs, key, rhs, pos)));

convertMultaryInstallCode(f:multop, opcode:Code, lhs:Code, key:Code, rhs:Code, pos:Position):Code := (
    Code(multaryCode(f, CodeSequence(opcode, lhs, key, rhs), pos)));

convertParentheses(seq:CodeSequence, word:Word, pos:Position):Code := (
    if word == leftparen    then Code(sequenceCode(seq,     pos)) else
    if word == leftbrace    then Code(listCode(seq,         pos)) else
    if word == leftbracket  then Code(arrayCode(seq,        pos)) else
    if word == leftAngleBar then Code(angleBarListCode(seq, pos)) else
    dummyCode -- should not happen
    );

export convert0(e:ParseTree):Code := (
    pos := treePosition(e);
    when e
    is token:Token do convertTokenReference(token)
    is p:EmptyParentheses do convertParentheses(CodeSequence(),                  p.left.word, pos)
    is p:Parentheses do convertParentheses(makeCodeSequence(p.contents, CommaW), p.left.word, pos)
    is a:Adjacent do Code(adjacentCode(convert(a.lhs), convert(a.rhs), pos))
    is a:Arrow do (
	fc := functionCode(convert(a.rhs), a.desc, hash_t(0), pos);
	fc.hash = hashFromAddress(Expr(fc));
	Code(fc))
    is b:Binary do (
	-- e.g. X.y and X.?y
	if b.Operator.entry == DotS.symbol
	|| b.Operator.entry == DotQuestionS.symbol
	then (
	    when b.rhs is token:Token do (
		rhs := convertGlobalOperator(token);
		-- TODO: is this check necessary?
		if token.word.typecode == TCid
		then Code(binaryCode(b.Operator.entry.binary, convert(b.lhs), rhs, pos))
		else dummyCode -- should not occur
		)
	    else dummyCode -- should not occur
	    )
	-- e.g. 1,2,3 or (0;1,2,3) but not (1,2,3) or {1,2,3}
	else if b.Operator.word == CommaW
	then Code(sequenceCode(makeCodeSequence(e, CommaW),     pos))
	-- e.g. (1;2;3) but not 1;2;3
	else if b.Operator.word == SemicolonW
	then Code(semiCode(    makeCodeSequence(e, SemicolonW), pos))
	-- global value assignment code
	else if b.Operator.word == EqualW
	then (
	    when b.lhs
	    -- e.g. x = ...
	    is t:Token       do convertTokenAssignment(t, b.rhs)
	    -- e.g. (x,y,z) = (...)
	    is p:Parentheses do convertParallelAssignment(p, b.rhs, b.Operator.dictionary)
	    -- Note: usable, but not used anywhere yet
	    is u:Unary   do convertUnaryInstallCode(UnaryInstallValueFun,
		convertGlobalOperator(u.Operator), convert(u.rhs), convert(b.rhs), pos)
	    -- e.g. RingFamily_* = (RR,e) -> RR#(symbol _*) = e
	    is u:Postfix do convertUnaryInstallCode(UnaryInstallValueFun,
		convertGlobalOperator(u.Operator), convert(u.lhs), convert(b.rhs), pos)
	    is c:Binary  do (
		-- e.g. X#key = v
		if c.Operator.entry == SharpS.symbol
		then convertBinaryInstallCode(AssignElemFun,
		    convert(c.lhs), convert(c.rhs), convert(b.rhs), pos)
		-- e.g. X.sym = v
		else if c.Operator.entry == DotS.symbol
		then (
		    when c.rhs is crhs:Token do (
			convertBinaryInstallCode(AssignElemFun,
			    convert(c.lhs), convertGlobalOperator(crhs), convert(b.rhs), pos))
		    else dummyCode -- should not happen
		    )
		-- e.g. M_(i,j) = x
		-- e.g. MutableMatrix _ Sequence = (M,ij,val) -> (...)
		-- FIXME: should these be separated?
		else convertMultaryInstallCode(InstallValueFun,
		    convertGlobalOperator(c.Operator), convert(c.lhs), convert(c.rhs), convert(b.rhs), pos))
	    -- e.g. poincare M = f
	    is a:Adjacent do convertMultaryInstallCode(InstallValueFun,
		convertGlobalOperator(AdjacentS.symbol), convert(a.lhs), convert(a.rhs), convert(b.rhs), pos)
	    else dummyCode -- should not happen
	    )
	else if b.Operator.word == ColonEqualW
	then (
	    when b.lhs
	    -- e.g. x := ...
	    is t:Token       do convertTokenAssignment(t, b.rhs)
	    -- e.g. (x,y,z) := (...)
	    is p:Parentheses do convertParallelAssignment(p, b.rhs, b.Operator.dictionary)
	    -- e.g. - Matrix := ...
	    -- TODO: can #T be implemented here?
	    is u:Unary   do convertUnaryInstallCode(UnaryInstallMethodFun,
		convertGlobalOperator(u.Operator), convert(u.rhs), convert(b.rhs), pos)
	    -- e.g. Ring_* := ...
	    is u:Postfix do convertUnaryInstallCode(UnaryInstallMethodFun,
		convertGlobalOperator(u.Operator), convert(u.lhs), convert(b.rhs), pos)
	    -- e.g. MutableMatrix _ Sequence := (M, ij) -> (...)
	    is c:Binary  do convertMultaryInstallCode(InstallMethodFun,
		convertGlobalOperator(c.Operator), convert(c.lhs), convert(c.rhs), convert(b.rhs), pos)
	    -- e.g. resolution Module := ...
	    is a:Adjacent do convertMultaryInstallCode(InstallMethodFun,
		convertGlobalOperator(AdjacentS.symbol), convert(a.lhs), convert(a.rhs), convert(b.rhs), pos)
	    is n:New do (
		if n.newParent      == dummyTree then
		if n.newInitializer == dummyTree
		-- e.g. new ChainComplex := ChainComplex => T -> ...
		then Code(binaryCode(AssignNewFun,
			convert(n.newClass),
			convert(b.rhs), pos))
		-- e.g. new Set from List := Set => ...
		else Code(ternaryCode(AssignNewFromFun,
			convert(n.newClass),
			convert(n.newInitializer),
			convert(b.rhs), pos))
		else if n.newInitializer == dummyTree
		-- Note: not used anywhere yet
		then Code(ternaryCode(AssignNewOfFun,
			convert(n.newClass),
			convert(n.newParent),
			convert(b.rhs), pos))
		-- e.g. new RealField of Nothing' from ZZ := ...
		else Code(multaryCode(AssignNewOfFromFun,
			CodeSequence(
			    convert(n.newClass),
			    convert(n.newParent),
			    convert(n.newInitializer),
			    convert(b.rhs)), pos)))
	    else dummyCode -- should not happen
	    )
	  else if isAugmentedAssignmentOperatorWord(b.Operator.word)
	  then (
	      when b.lhs
	      is a:Adjacent do Code(augmentedAssignmentCode(
		      b.Operator.entry, convert(b.lhs), convert(b.rhs), AdjacentS.symbol, pos))
	      is o:Unary do Code(augmentedAssignmentCode(
		      b.Operator.entry, convert(b.lhs), convert(b.rhs), o.Operator.entry, pos))
	      is o:Postfix do Code(augmentedAssignmentCode(
		      b.Operator.entry, convert(b.lhs), convert(b.rhs), o.Operator.entry, pos))
	      is o:Binary do Code(augmentedAssignmentCode(
		      b.Operator.entry, convert(b.lhs), convert(b.rhs), o.Operator.entry, pos))
	      is t:Token do Code(augmentedAssignmentCode(
		      b.Operator.entry, convert(b.lhs), convert(b.rhs), t.entry, pos))
	      else Code(augmentedAssignmentCode(
		      b.Operator.entry, convert(b.lhs), convert(b.rhs), dummySymbol, pos))
		      )
	  else Code(binaryCode(b.Operator.entry.binary, convert(b.lhs), convert(b.rhs), pos))
	  )
    is u:Unary do (
	if u.Operator.word == CommaW     then Code(sequenceCode(makeCodeSequence(e, CommaW),     pos)) else
	if u.Operator.word == SemicolonW then Code(semiCode(    makeCodeSequence(e, SemicolonW), pos))
	else Code(unaryCode(u.Operator.entry.unary, convert(u.rhs), pos)))
    is u:Postfix do Code(unaryCode(u.Operator.entry.postfix, convert(u.lhs), pos))
    is q:Quote do (
	  token := q.rhs;
	  sym := token.entry;
	  if sym.frameID == 0 then
	  if sym.thread
	  then Code(threadSymbolClosureCode(sym, pos))
	  else Code(globalSymbolClosureCode(sym, pos))
	  else Code(localSymbolClosureCode(nestingDepth(sym.frameID, token.dictionary), sym, pos)))
    is q:GlobalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  Code(globalSymbolClosureCode(sym, pos)))
    is q:ThreadQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  Code(threadSymbolClosureCode(sym, pos)))
    is q:LocalQuote do (
	  token := q.rhs;
	  sym := token.entry;
	  Code(localSymbolClosureCode(nestingDepth(sym.frameID, token.dictionary), sym, pos)))
    is i:IfThen      do Code(ifCode(convert(i.predicate), convert(i.thenClause), NullCode,              pos))
    is i:IfThenElse  do Code(ifCode(convert(i.predicate), convert(i.thenClause), convert(i.elseClause), pos))
    is i:Try         do Code(tryCode(convert(i.primary), NullCode,          NullCode,             pos))
    is i:TryThen     do Code(tryCode(convert(i.primary), convert(i.sequel), NullCode,             pos))
    is i:TryThenElse do Code(tryCode(convert(i.primary), convert(i.sequel), convert(i.alternate), pos))
    is i:TryElse     do Code(tryCode(convert(i.primary), NullCode,          convert(i.alternate), pos))
    is i:Catch       do Code(catchCode(convert(i.primary), pos))
    is w:WhileDo     do Code(whileDoCode(    convert(w.predicate),                        convert(w.doClause), pos))
    is w:WhileListDo do Code(whileListDoCode(convert(w.predicate), convert(w.listClause), convert(w.doClause), pos))
    is w:WhileList   do Code(whileListCode(  convert(w.predicate), convert(w.listClause),                      pos))
    is f:For         do Code(
	forCode(
	    convert(f.inClause),   convert(f.fromClause), convert(f.toClause),
	    convert(f.whenClause), convert(f.listClause), convert(f.doClause),
	    f.dictionary.frameID, f.dictionary.framesize, pos))
    is n:New do (
	if n.newParent      == dummyTree then
	if n.newInitializer == dummyTree
	then Code(newCode(      convert(n.newClass),                            pos))
	else Code(newFromCode(  convert(n.newClass), convert(n.newInitializer), pos))
	else if n.newInitializer == dummyTree
	then Code(newOfCode(    convert(n.newClass), convert(n.newParent),                            pos))
	else Code(newOfFromCode(convert(n.newClass), convert(n.newParent), convert(n.newInitializer), pos)))
    is d:dummy do dummyCode
    );

export unseq(c:Code):Code := (
    when c is s:sequenceCode do if length(s.x) == 1
    then s.x.0 else c else c);

export convert(e:ParseTree):Code := unseq(convert0(e));

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d convertr.o "
-- End:
