--		Copyright 2002,2010 by Daniel R. Grayson
-- helper routines for checking arguments
use basic;
export isBoolean(e:Expr):bool := e == True || e == False;
export toBoolean(e:Expr):bool := e == True;

export isSmallInt(e:Expr):bool := (
     when e is i:ZZcell do 
     if isInt(i) then true else false
     else false
     );

export getSmallInt(e:Expr):int := when e is i:ZZcell do toInt(i) else -333;

export isSequenceOfPairsOfSmallIntegers(v:Sequence) : (string or null) := (
     foreach i in v do 
     when i 
     is pair:Sequence do 
     if length(pair) != 2
     then return "a list of pairs of integers" 
     else foreach j in pair do
     when j 
     is a:ZZcell do
     if isInt(a) 
     then nothing
     else return "a list of pairs of small integers"
     else return "a list of pairs of integers"
     else return "a list of pairs of integers";
     null());

export isSequenceOfPairsOfSmallIntegers(e:Expr) : (string or null) := (
     when e
     is x:List do isSequenceOfPairsOfSmallIntegers(x.v)
     is x:Sequence  do isSequenceOfPairsOfSmallIntegers(x)
     else (string or null)("a list of pairs of integers")
     );

export getSequenceOfPairsOfSmallIntegers(v:Sequence) : array(int) := (
     new array(int) len 2*length(v) do (
	  foreach i in v do (
	       when i
	       is pair:Sequence 
	       do foreach j in pair do when j is a:ZZcell do provide toInt(a) else nothing
	       else nothing
	       );
	  provide -333;					    -- shouldn't happen
	  ));

export getSequenceOfPairsOfSmallIntegers(e:Expr) : array(int) := (
     when e
     is x:List do getSequenceOfPairsOfSmallIntegers(x.v)
     is x:Sequence  do getSequenceOfPairsOfSmallIntegers(x)
     else array(int)()
     );

export getReverseSequenceOfPairsOfSmallIntegers(v:Sequence) : array(int) := (
     new array(int) len 2*length(v) do 
     for k from length(v)-1 to 0 by -1 do 
     when v.k is pair:Sequence do foreach j in pair do when j is a:ZZcell do provide toInt(a) else provide(0)
     else provide(0)
     );

export isSequenceOfSmallIntegers(s:Sequence) : bool := (
     foreach i in s do (
	  when i is a:ZZcell do (
	       if !isInt(a) then return false;
	       )
	  else return false);
     true);
export isSequenceOfSmallIntegers(e:Expr) : bool := (
     when e
     is s:Sequence do isSequenceOfSmallIntegers(s)
     is l:List do isSequenceOfSmallIntegers(l.v)
     else false);
export isNullOrSequenceOfSmallIntegers(e:Expr) : bool := (
     if e == nullE then return(true);
     when e
     is s:Sequence do isSequenceOfSmallIntegers(s)
     is l:List do isSequenceOfSmallIntegers(l.v)
     else false);
export isNullOrSmallInt(e:Expr) : bool := (e == nullE || isSmallInt(e));
export getLengthOfSequence(e:Expr) : int := (
     when e
     is s:Sequence do length(s)
     is l:List do length(l.v)
     else 0						    -- shouldn't happen
     );
export getSequenceOfSmallIntegers(s:Sequence) : array(int) := (
     new array(int) len length(s) do (
	  foreach i in s do 
	  when i 
	  is a:ZZcell do provide toInt(a) 
	  else anywhereAbort("internal error: getSequenceOfSmallIntegers");
	  anywhereAbort("internal error: getSequenceOfSmallIntegers");
	  )
     );
export getSequenceOfSmallIntegers(e:Expr) : array(int) := (
     when e
     is s:Sequence do getSequenceOfSmallIntegers(s)
     is l:List do getSequenceOfSmallIntegers(l.v)
     else (
	  anywhereAbort("internal error: getSequenceOfSmallIntegers.");
	  array(int)()));
export getNullOrSequenceOfSmallIntegers(e:Expr) : RawArrayIntOrNull := (
     if e == nullE then return(NULL);
     when e
     is s:Sequence do getSequenceOfSmallIntegers(s)
     is l:List do getSequenceOfSmallIntegers(l.v)
     else (
	  anywhereAbort("internal error: getSequenceOfSmallIntegers.");
	  array(int)()));

export isSequenceOfStrings(e:Expr) : bool := (
     when e is s:Sequence do (
     	  foreach i in s do when i is stringCell do nothing else return false;
	  true)
     else false);
export getSequenceOfStrings(e:Expr) : array(string) := (
     when e is s:Sequence do 
     new array(string) len length(s) do (
	  foreach i in s do when i is a:stringCell do provide a.v else nothing;
	  provide "";
	  )
     else array(string)()
     );

export isSequenceOfMonomialOrderings(s:Sequence) : bool := (
     foreach i in s do when i is RawMonomialOrderingCell do nothing else return false;
     true);
export getSequenceOfMonomialOrderings(s:Sequence) : RawMonomialOrderingArray := (
     new RawMonomialOrderingArray len length(s) do (
	  foreach i in s do 
	  when i 
	  is a:RawMonomialOrderingCell do provide a.p 
	  else anywhereAbort("internal error : getSequenceOfMonomialOrderings")));

export isSequenceOfRingElements(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawRingElementCell do nothing else return false;
	  true)
     is RawRingElementCell do true
     else false);
export getSequenceOfRingElements(e:Expr) : RawRingElementArray := (
     when e
     is s:Sequence do (
	  new RawRingElementArray len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:RawRingElementCell do provide a.p 
	       else anywhereAbort("internal error : getSequenceOfRingElements")))
     is a:RawRingElementCell do RawRingElementArray(a.p)
     else RawRingElementArray());

export isSequenceOfMatrices(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawMatrixCell do nothing else return false;
	  true)
     is RawMatrixCell do true
     else false);
export isTrue(e:Boolean):bool := e.v;
export isTrue(e:Expr):bool := e == True;
export getSequenceOfMatrices(e:Expr) : RawMatrixArray := (
     when e
     is s:Sequence do (
	  new RawMatrixArray len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:RawMatrixCell do provide a.p
	       else anywhereAbort("internal error : getSequenceOfMatrices")))
     is a:RawMatrixCell do RawMatrixArray(a.p)
     else RawMatrixArray());

export isSequenceOfMutableMatrices(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawMutableMatrixCell do nothing else return false;
	  true)
     is RawMutableMatrixCell do true
     else false);
export getSequenceOfMutableMatrices(e:Expr) : RawMutableMatrixArray := (
     when e
     is s:Sequence do (
	  new RawMutableMatrixArray len length(s) do (
	       foreach i in s do
	       when i
	       is a:RawMutableMatrixCell do provide a.p
	       else anywhereAbort("internal error : getSequenceOfMutableMatrices")))
     is a:RawMutableMatrixCell do RawMutableMatrixArray(a.p)
     else RawMutableMatrixArray());

-----------------------------------------------------------------------------
-- helper routines for checking and converting return values

export engineErrorMessage():Expr := buildErrorPacket(EngineError("unknown engine error"));
export possibleEngineError(ret:bool):Expr := if ret then nullE else engineErrorMessage();

export toExpr(h:int):Expr := Expr(ZZcell(toInteger(h)));
export twoE := toExpr(2);
export threeE := toExpr(3);
export toExpr(h:long):Expr := Expr(ZZcell(toInteger(h)));
export toExpr(h:ulong):Expr := Expr(ZZcell(toInteger(h)));
export toExpr(h:ushort):Expr := Expr(ZZcell(toInteger(h)));
export toExpr(s:string):Expr := Expr(stringCell(s));
export emptyString := toExpr("");
export toExpr(x:ZZ):Expr := Expr(ZZcell(x));
export toExpr(x:QQ):Expr := Expr(QQcell(x));
export toExpr(x:RR):Expr := Expr(RRcell(x));
export toExpr(x:RRi):Expr := Expr(RRicell(x));
export toExpr(x:CC):Expr := Expr(CCcell(x));
export toExpr(x:float):Expr := Expr(RRcell(toRR(x,ulong(24))));
export toExpr(x:double):Expr := Expr(RRcell(toRR(x,ulong(53))));
export toExpr(x:RawComputation):Expr := Expr(RawComputationCell(x));
export toExpr(x:RawFreeModule):Expr := Expr(RawFreeModuleCell(x));
export toExpr(x:RawMatrix):Expr := Expr(RawMatrixCell(x));
export toExpr(x:RawMonoid):Expr := Expr(RawMonoidCell(x));
export toExpr(x:RawMonomial):Expr := Expr(RawMonomialCell(x));
export toExpr(x:RawMonomialIdeal):Expr := Expr(RawMonomialIdealCell(x));
export toExpr(x:RawMonomialOrdering):Expr := Expr(RawMonomialOrderingCell(x));
export toExpr(x:RawMutableMatrix):Expr := Expr(RawMutableMatrixCell(x));
export toExpr(x:RawMutableComplex):Expr := Expr(RawMutableComplexCell(x));
-- NAG begin
export toExpr(x:RawHomotopy):Expr := Expr(RawHomotopyCell(x));
export toExpr(x:RawSLEvaluator):Expr := Expr(RawSLEvaluatorCell(x));
export toExpr(x:RawSLProgram):Expr := Expr(RawSLProgramCell(x));
export toExpr(x:RawStraightLineProgram):Expr := Expr(RawStraightLineProgramCell(x));
export toExpr(x:RawPathTracker):Expr := Expr(RawPathTrackerCell(x));
export toExpr(x:RawPointArray):Expr := Expr(RawPointArrayCell(x));
-- NAG end
export toExpr(x:RawRing):Expr := Expr(RawRingCell(x));
export toExpr(x:RawRingElement):Expr := Expr(RawRingElementCell(x));
export toExpr(x:RawRingMap):Expr := Expr(RawRingMapCell(x));
export toExpr(x:RawRingOrNull):Expr := when x is r:RawRing do Expr(RawRingCell(r)) is null do engineErrorMessage();
export toExpr(x:RawMonoidOrNull):Expr := when x is r:RawMonoid do Expr(RawMonoidCell(r)) is null do engineErrorMessage();
export toExpr(x:RawMonomialPairOrNull):Expr := when x is r:RawMonomialPair do Expr(Sequence(Expr(RawMonomialCell(r.a)), Expr(RawMonomialCell(r.b)))) else engineErrorMessage();
export toExprOrNull(x:RawRingOrNull):Expr := when x is r:RawRing do Expr(RawRingCell(r)) is null do nullE;
export toExpr(x:RawMonomialIdealOrNull):Expr := when x is r:RawMonomialIdeal do Expr(RawMonomialIdealCell(r)) is null do engineErrorMessage();
export toExprSeq(x:RawArrayInt):Expr := Expr(new Sequence len length(x) do foreach i in x do provide Expr(ZZcell(toInteger(i))));
export toExpr(x:RawArrayInt):Expr := Expr(list(new Sequence len length(x) do foreach i in x do provide Expr(ZZcell(toInteger(i)))));
export toExpr(e:RawArrayIntOrNull):Expr := when e is x:RawArrayInt do toExpr(x) is null do engineErrorMessage();
export toExpr(x:RawMatrixAndInt):Expr := Expr(new Sequence len 2 do ( provide Expr(RawMatrixCell(x.M)); provide Expr(ZZcell(toInteger(x.i))); ));
export toExpr(x:ZZpair):Expr := Expr(new Sequence len 2 do (provide ZZcell(x.a); provide ZZcell(x.b)));
export toExpr(x:ZZpairOrNull):Expr := when x is r:ZZpair do toExpr(r) is null do engineErrorMessage();
export toExpr(x:RawRingElementArray):Expr := new Sequence len length(x) do foreach r in x do provide Expr(RawRingElementCell(r));
export toExpr(x:RawRingElementArrayOrNull):Expr := when x is s:RawRingElementArray do toExpr(s) is null do engineErrorMessage();
export toExpr(x:RawRingElementPairOrNull):Expr := when x is p:RawRingElementPair do seq(Expr(RawRingElementCell(p.a)),Expr(RawRingElementCell(p.b))) is null do engineErrorMessage();
export toExpr(x:RawMonomialOrNull):Expr := when x is r:RawMonomial do Expr(RawMonomialCell(r)) is null do engineErrorMessage();
export toExpr(x:RawRingElementOrNull):Expr := when x is r:RawRingElement do Expr(RawRingElementCell(r)) is null do engineErrorMessage();
export toExpr(x:RawFreeModuleOrNull):Expr := when x is r:RawFreeModule do Expr(RawFreeModuleCell(r)) is null do engineErrorMessage();
export toExpr(x:RawMatrixOrNull):Expr := when x is r:RawMatrix do Expr(RawMatrixCell(r)) is null do engineErrorMessage();
export toExpr(x:RawMutableMatrixOrNull):Expr := when x is r:RawMutableMatrix do Expr(RawMutableMatrixCell(r)) is null do engineErrorMessage();
export toExpr(x:RawMutableComplexOrNull):Expr := when x is r:RawMutableComplex do Expr(RawMutableComplexCell(r)) is null do engineErrorMessage();
-- NAG begin
export toExpr(x:RawHomotopyOrNull):Expr := when x is r:RawHomotopy do Expr(RawHomotopyCell(r)) is null do engineErrorMessage();
export toExpr(x:RawSLEvaluatorOrNull):Expr := when x is r:RawSLEvaluator do Expr(RawSLEvaluatorCell(r)) is null do engineErrorMessage();
export toExpr(x:RawSLProgramOrNull):Expr := when x is r:RawSLProgram do Expr(RawSLProgramCell(r)) is null do engineErrorMessage();
export toExpr(x:RawStraightLineProgramOrNull):Expr := when x is r:RawStraightLineProgram do Expr(RawStraightLineProgramCell(r)) is null do engineErrorMessage();
export toExpr(x:RawPathTrackerOrNull):Expr := when x is r:RawPathTracker do Expr(RawPathTrackerCell(r)) is null do engineErrorMessage();
export toExpr(x:RawPointArrayOrNull):Expr := when x is r:RawPointArray do Expr(RawPointArrayCell(r)) is null do engineErrorMessage();
-- NAG end
export toExpr(x:ZZorNull):Expr := when x is i:ZZ do Expr(ZZcell(i)) is null do engineErrorMessage();
export toExpr(x:QQorNull):Expr := when x is i:QQ do Expr(QQcell(i)) is null do engineErrorMessage();
export toExpr(x:RRorNull):Expr := when x is i:RR do Expr(RRcell(i)) is null do engineErrorMessage();
export toExpr(x:RRiorNull):Expr := when x is i:RRi do Expr(RRicell(i)) is null do engineErrorMessage();
export toExpr(x:CCorNull):Expr := when x is i:CC do Expr(CCcell(i)) is null do engineErrorMessage();
export toExpr(x:RawMatrixPairOrNull):Expr := when x is p:RawMatrixPair do seq(Expr(RawMatrixCell(p.a)),Expr(RawMatrixCell(p.b))) is null do engineErrorMessage();
export toExpr(x:RawMatrixArray):Expr := Expr( list( new Sequence len length(x) do foreach m in x do provide Expr(RawMatrixCell(m)) ) );
export toExpr(x:RawMatrixArrayOrNull):Expr := when x is r:RawMatrixArray do toExpr(r) is null do engineErrorMessage();
export toExpr(x:RawMutableMatrixArray):Expr := Expr( list( new Sequence len length(x) do foreach m in x do provide Expr(RawMutableMatrixCell(m)) ) );
export toExpr(x:RawMutableMatrixArrayOrNull):Expr := when x is r:RawMutableMatrixArray do toExpr(r) is null do engineErrorMessage();
export toExpr(x:array(string)):Expr := Expr( list( new Sequence len length(x) do foreach s in x do provide Expr(stringCell(s)) ) );
export toExpr(x:RawComputationOrNull):Expr := when x is r:RawComputation do Expr(RawComputationCell(r)) is null do engineErrorMessage();
export toExpr(x:RawArrayPairOrNull):Expr := (
     when x
     is r:RawArrayPair do Expr(
	  Sequence(
	       new Sequence len length(r.coeffs) at i do foreach x in r.coeffs do provide Expr(RawRingElementCell(x)),
	       new Sequence len length(r.monoms) at i do foreach x in r.monoms do provide Expr(RawMonomialCell(x))
	       ))
     is null do engineErrorMessage());
export toExpr(x:RawArrayIntPairOrNull):Expr := when x is p:RawArrayIntPair do seq(toExpr(p.a),toExpr(p.b)) is null do engineErrorMessage();

export toExpr(x:xmlNode):Expr := Expr(xmlNodeCell(x));
export toExpr(x:xmlAttr):Expr := Expr(xmlAttrCell(x));
export toExpr(x:constcharstar) ::= toExpr(tostring(x));
export toExpr(x:constcharstarOrNull):Expr := (
     when x
     is null do nullE
     is s:constcharstar do toExpr(s)
     );
export toExpr(x:constucharstar) ::= toExpr(Ccode(constcharstar,"((const char *)",x,")"));
export toExpr(x:constucharstarOrNull) ::= toExpr(Ccode(constcharstarOrNull,"((const char *)",x,")"));
export toExpr(x:arrayZZ):Expr := new Sequence len length(x) do foreach i in x do provide toExpr(i);
export arrayarrayZZ := array(arrayZZ);
export toExpr(x:arrayarrayZZ):Expr := new Sequence len length(x) do foreach i in x do provide toExpr(i);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d util.o "
-- End:
