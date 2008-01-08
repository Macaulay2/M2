--		Copyright 2002 by Daniel R. Grayson
use C;
use system;
use strings;
use varstrin;
use vararray;
use nets;
use stdio;
use ctype;
use getline;
use stdiop;
use err;
use common;
use gmp;
use engine;
use tokens;
use lex;
use parser;
use binding;
use basic;
use convertr;


-----------------------------------------------------------------------------
-- helper routines for checking arguments

export isBoolean(e:Expr):bool := e == True || e == False;
export toBoolean(e:Expr):bool := e == True;

export isSmallInt(e:Expr):bool := (
     when e is i:Integer do 
     if isInt(i) then true else false
     else false
     );

export getSmallInt(e:Expr):int := when e is i:Integer do toInt(i) else -333;

export isSequenceOfPairsOfSmallIntegers(v:Sequence) : (string or null) := (
     foreach i in v do 
     when i 
     is pair:Sequence do 
     if length(pair) != 2
     then return "a list of pairs of integers" 
     else foreach j in pair do
     when j 
     is a:Integer do
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
	       do foreach j in pair do when j is a:Integer do provide toInt(a) else nothing
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
     when v.k is pair:Sequence do foreach j in pair do when j is a:Integer do provide toInt(a) else provide(0)
     else provide(0)
     );

export isSequenceOfSmallIntegers(s:Sequence) : bool := (
     foreach i in s do (
	  when i is a:Integer do (
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
export getSequenceOfSmallIntegers(s:Sequence) : array(int) := (
     new array(int) len length(s) do (
	  foreach i in s do 
	  when i 
	  is a:Integer do provide toInt(a) 
	  else abort("internal error: getSequenceOfSmallIntegers");
	  abort("internal error: getSequenceOfSmallIntegers");
	  )
     );
export getSequenceOfSmallIntegers(e:Expr) : array(int) := (
     when e
     is s:Sequence do getSequenceOfSmallIntegers(s)
     is l:List do getSequenceOfSmallIntegers(l.v)
     else (
	  abort("internal error: getSequenceOfSmallIntegers.");
	  array(int)()));
export getNullOrSequenceOfSmallIntegers(e:Expr) : RawArrayIntOrNull := (
     if e == nullE then return(NULL);
     when e
     is s:Sequence do getSequenceOfSmallIntegers(s)
     is l:List do getSequenceOfSmallIntegers(l.v)
     else (
	  abort("internal error: getSequenceOfSmallIntegers.");
	  array(int)()));

export isSequenceOfStrings(e:Expr) : bool := (
     when e is s:Sequence do (
     	  foreach i in s do when i is string do nothing else return false;
	  true)
     else false);
export getSequenceOfStrings(e:Expr) : array(string) := (
     when e is s:Sequence do 
     new array(string) len length(s) do (
	  foreach i in s do when i is a:string do provide a else nothing;
	  provide "";
	  )
     else array(string)()
     );

export isSequenceOfMonomialOrderings(s:Sequence) : bool := (
     foreach i in s do when i is RawMonomialOrdering do nothing else return false;
     true);
export getSequenceOfMonomialOrderings(s:Sequence) : RawMonomialOrderingArray := (
     new RawMonomialOrderingArray len length(s) do (
	  foreach i in s do 
	  when i 
	  is a:RawMonomialOrdering do provide a 
	  else abort("internal error : getSequenceOfMonomialOrderings")));

export isSequenceOfRingElements(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawRingElement do nothing else return false;
	  true)
     is RawRingElement do true
     else false);
export getSequenceOfRingElements(e:Expr) : RawRingElementArray := (
     when e
     is s:Sequence do (
	  new RawRingElementArray len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:RawRingElement do provide a 
	       else abort("internal error : getSequenceOfRingElements")))
     is a:RawRingElement do RawRingElementArray(a)
     else RawRingElementArray());

export isSequenceOfMatrices(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawMatrix do nothing else return false;
	  true)
     is RawMatrix do true
     else false);
export isTrue(e:Boolean):bool := e.v;
export isTrue(e:Expr):bool := e == True;
export getSequenceOfMatrices(e:Expr) : RawMatrixArray := (
     when e
     is s:Sequence do (
	  new RawMatrixArray len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:RawMatrix do provide a 
	       else abort("internal error : getSequenceOfMatrices")))
     is a:RawMatrix do RawMatrixArray(a)
     else RawMatrixArray());

-----------------------------------------------------------------------------
-- helper routines for checking and converting return values

export engineErrorMessage():Expr := buildErrorPacket(EngineError("unknown engine error"));
export possibleEngineError(ret:bool):Expr := if ret then nullE else engineErrorMessage();

export toExpr(x:double):Expr := Expr(toRR(x));
export toExpr(x:RawRingOrNull):Expr := when x is r:RawRing do Expr(r) is null do engineErrorMessage();
export toExpr(x:RawMonomialPairOrNull):Expr := when x is r:RawMonomialPair do Expr(Sequence(Expr(r.a), Expr(r.b))) else engineErrorMessage();
export toExprOrNull(x:RawRingOrNull):Expr := when x is r:RawRing do Expr(r) is null do nullE;
export toExpr(x:RawMonomialIdealOrNull):Expr := when x is r:RawMonomialIdeal do Expr(r) is null do engineErrorMessage();
export toExprSeq(x:RawArrayInt):Expr := Expr(new Sequence len length(x) do foreach i in x do provide Expr(toInteger(i)));
export toExpr(x:RawArrayInt):Expr := Expr(list(new Sequence len length(x) do foreach i in x do provide Expr(toInteger(i))));
export toExpr(e:RawArrayIntOrNull):Expr := when e is x:RawArrayInt do toExpr(x) is null do engineErrorMessage();
export toExpr(x:RawMatrixAndInt):Expr := Expr(new Sequence len 2 do ( provide Expr(x.M); provide Expr(toInteger(x.i)); ));
export toExpr(x:IntegerPair):Expr := Expr(new Sequence len 2 do (provide x.a; provide x.b));
export toExpr(x:IntegerPairOrNull):Expr := when x is r:IntegerPair do toExpr(r) is null do engineErrorMessage();
export toExpr(x:RawRingElementArray):Expr := new Sequence len length(x) do foreach r in x do provide Expr(r);
export toExpr(x:RawRingElementArrayOrNull):Expr := when x is s:RawRingElementArray do toExpr(s) is null do engineErrorMessage();
export toExpr(x:RawRingElementPairOrNull):Expr := when x is p:RawRingElementPair do seq(Expr(p.a),Expr(p.b)) is null do engineErrorMessage();
export toExpr(x:RawMonomialOrNull):Expr := when x is r:RawMonomial do Expr(r) is null do engineErrorMessage();
export toExpr(x:RawRingElementOrNull):Expr := when x is r:RawRingElement do Expr(r) is null do engineErrorMessage();
export toExpr(x:RawFreeModuleOrNull):Expr := when x is r:RawFreeModule do Expr(r) is null do engineErrorMessage();
export toExpr(x:RawMatrixOrNull):Expr := when x is r:RawMatrix do Expr(r) is null do engineErrorMessage();
export toExpr(x:RawMutableMatrixOrNull):Expr := when x is r:RawMutableMatrix do Expr(r) is null do engineErrorMessage();
export toExpr(x:IntegerOrNull):Expr := when x is i:Integer do Expr(i) is null do engineErrorMessage();
export toExpr(x:RationalOrNull):Expr := when x is i:Rational do Expr(i) is null do engineErrorMessage();
export toExpr(x:RRRorNull):Expr := when x is i:RR do Expr(i) is null do engineErrorMessage();
export toExpr(x:RawMatrixPairOrNull):Expr := when x is p:RawMatrixPair do seq(Expr(p.a),Expr(p.b)) is null do engineErrorMessage();
export toExpr(x:RawMatrixArray):Expr := Expr( list( new Sequence len length(x) do foreach m in x do provide Expr(m) ) );
export toExpr(x:RawMatrixArrayOrNull):Expr := when x is r:RawMatrixArray do toExpr(r) is null do engineErrorMessage();
export toExpr(x:array(string)):Expr := Expr( list( new Sequence len length(x) do foreach s in x do provide Expr(s) ) );
export toExpr(x:RawComputationOrNull):Expr := when x is r:RawComputation do Expr(r) is null do engineErrorMessage();
export toExpr(x:RawArrayPairOrNull):Expr := (
     when x
     is r:RawArrayPair do Expr(
	  Sequence(
	       new Sequence len length(r.coeffs) at i do foreach x in r.coeffs do provide Expr(x),
	       new Sequence len length(r.monoms) at i do foreach x in r.monoms do provide Expr(x)
	       ))
     is null do engineErrorMessage());

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d "
-- End:
