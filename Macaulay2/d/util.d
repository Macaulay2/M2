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
     then return("a list of pairs of integers") 
     else foreach j in pair do
     when j 
     is a:Integer do
     if isInt(a) 
     then nothing
     else return("a list of pairs of small integers")
     else return("a list of pairs of integers")
     else return("a list of pairs of integers");
     null());

export getSequenceOfPairsOfSmallIntegers(v:Sequence) : array(int) := (
     new array(int) len 2*length(v) do (
	  foreach i in v do (
	       when i
	       is pair:Sequence 
	       do foreach j in pair do when j is a:Integer do provide toInt(a) else nothing
	       else nothing
	       );
	  provide -333;
	  ));

export isListOfSmallIntegers(e:Expr) : bool := (
     when e is l:List do (
     	  foreach i in l.v do (
	       when i is a:Integer do (
		    if !isInt(a) then return(false);
		    )
	       else return(false));
	  true)
     else false);
export getListOfSmallIntegers(e:Expr) : array(int) := (
     when e is l:List do 
     new array(int) len length(l.v) do (
	  foreach i in l.v do when i is a:Integer do provide toInt(a) else nothing;
	  provide 0;
	  )
     else array(int)(-333)
     );

export isSequenceOfSmallIntegers(s:Sequence) : bool := (
     foreach i in s do (
	  when i is a:Integer do (
	       if !isInt(a) then return(false);
	       )
	  else return(false));
     true);
export getSequenceOfSmallIntegers(s:Sequence) : array(int) := (
     new array(int) len length(s) do (
	  foreach i in s do 
	  when i 
	  is a:Integer do provide toInt(a) 
	  else abort("internal error: getSequenceOfSmallIntegers");
	  abort("internal error: getSequenceOfSmallIntegers");
	  )
     );

export isSequenceOfSmallIntegers(e:Expr) : bool := (
     when e
     is s:Sequence do (
	  foreach i in s do (
	       when i is a:Integer do (
		    if !isInt(a) then return(false);
		    )
	       else return(false));
	  true)
     is a:Integer do isInt(a)
     else false);
export getSequenceOfSmallIntegers(e:Expr) : array(int) := (
     when e
     is s:Sequence do (
	  new array(int) len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:Integer do provide toInt(a) 
	       else abort("internal error: getSequenceOfSmallIntegers");
	       abort("internal error: getSequenceOfSmallIntegers");
	       )
	  )
     is a:Integer do array(int)(toInt(a))
     else array(int)());

export isListOfStrings(e:Expr) : bool := (
     when e is l:List do (
     	  foreach i in l.v do when i is string do nothing else return(false);
	  true)
     else false);
export getListOfStrings(e:Expr) : array(string) := (
     when e is l:List do 
     new array(string) len length(l.v) do (
	  foreach i in l.v do when i is a:string do provide a else nothing;
	  provide "";
	  )
     else array(string)()
     );

export isSequenceOfMonomialOrderings(s:Sequence) : bool := (
     foreach i in s do when i is RawMonomialOrdering do nothing else return(false);
     true);
export getSequenceOfMonomialOrderings(s:Sequence) : RawMonomialOrderingArray := (
     new RawMonomialOrderingArray len length(s) do (
	  foreach i in s do 
	  when i 
	  is a:RawMonomialOrdering do provide a 
	  else abort("internal error : getSequenceOfMonomialOrderings")));

export isSequenceOfVectors(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawVector do nothing else return(false);
	  true)
     is RawVector do true
     else false);
export getSequenceOfVectors(e:Expr) : RawVectorArray := (
     when e
     is s:Sequence do (
	  new RawVectorArray len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:RawVector do provide a 
	       else abort("internal error : getSequenceOfVectors")))
     is a:RawVector do RawVectorArray(a)
     else RawVectorArray());

export isSequenceOfRingElements(e:Expr) : bool := (
     when e is s:Sequence do (
	  foreach i in s do when i is RawRingElement do nothing else return(false);
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
	  foreach i in s do when i is RawMatrix do nothing else return(false);
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

export toExpr(x:RawRingOrNull):Expr := (
     when x
     is r:RawRing do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw ring engine error"))
     );

export toExpr(x:RawMonomialIdealOrNull):Expr := (
     when x
     is r:RawMonomialIdeal do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw monomial ideal engine error"))
     );

export toExpr(x:RawArrayInt):Expr := Expr(
     list(new Sequence len length(x) do foreach i in x do provide Expr(toInteger(i))));

export toExpr(e:RawArrayIntOrNull):Expr := (
     when e
     is x:RawArrayInt do toExpr(x)
     is null do buildErrorPacket(EngineError("unknown raw int array engine error")));

export toExpr(x:RawMatrixAndInt):Expr := Expr(new Sequence len 2 do (
	  provide Expr(x.M);
	  provide Expr(toInteger(x.i));
	  ));

export toExpr(x:IntegerPair):Expr := Expr(new Sequence len 2 do (provide x.a; provide x.b));
export toExpr(x:IntegerPairOrNull):Expr := (
     when x
     is r:IntegerPair do toExpr(r)
     is null do buildErrorPacket(EngineError("unknown raw ring element engine error"))
     );

export toExpr(x:RawRingElementArray):Expr := (
     new Sequence len length(x) do foreach r in x do provide Expr(r)
     );

export toExpr(x:RawRingElementPairOrNull):Expr := (
     when x
     is p:RawRingElementPair do seq(Expr(p.a),Expr(p.b))
     is null do buildErrorPacket(EngineError("unknown raw ring element pair engine error"))
     );

export toExpr(x:RawMonomialOrNull):Expr := (
     when x
     is r:RawMonomial do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw monomial engine error"))
     );
export toExpr(x:RawRingElementOrNull):Expr := (
     when x
     is r:RawRingElement do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw ring element engine error"))
     );
export toExpr(x:RawFreeModuleOrNull):Expr := (
     when x
     is r:RawFreeModule do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw ring element engine error"))
     );
export toExpr(x:RawVectorOrNull):Expr := (
     when x
     is r:RawVector do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw ring element engine error"))
     );
export toExpr(x:RawMatrixOrNull):Expr := (
     when x
     is r:RawMatrix do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw ring element engine error"))
     );

export toExpr(x:RawArrayPairOrNull):Expr := (
     when x
     is r:RawArrayPair do Expr(
	  Sequence(
	       new Sequence len length(r.coeffs) at i do foreach x in r.coeffs do provide Expr(x),
	       new Sequence len length(r.monoms) at i do foreach x in r.monoms do provide Expr(x)
	       ))
     is null do buildErrorPacket(EngineError("unknown raw monomial engine error"))
     );

export toExpr(x:IntegerOrNull):Expr := (
     when x
     is i:Integer do Expr(i)
     is null do buildErrorPacket(EngineError("unknown raw monomial engine error"))
     );
export toExpr(x:array(string)):Expr := Expr(
     list(
     	  new Sequence len length(x) do foreach s in x do provide Expr(s)
	  )
     );
export toExpr(x:RawComputationOrNull):Expr := (
     when x
     is r:RawComputation do Expr(r)
     is null do buildErrorPacket(EngineError("unknown raw computation engine error"))
     );

------------------------------
-- for lapack
------------------------------
export isSequenceOfReals(e:Expr) : bool := (
     when e
     is s:Sequence do (
	  foreach i in s do (
	       when i is a:Real do nothing else return(false));
	  true)
     is Real do true
     else false);
export getSequenceOfReals(e:Expr) : array(double) := (
     when e
     is s:Sequence do (
	  new array(double) len length(s) do (
	       foreach i in s do 
	       when i 
	       is a:Real do provide a.v
	       else abort("internal error: getSequenceOfReals");
	       abort("internal error: getSequenceOfReals");
	       )
	  )
     is a:Real do array(double)(a.v)
     else array(double)());

export isListOfReals(e:Expr) : bool := (
     when e is l:List do (
     	  foreach i in l.v do (
	       when i is a:Real do nothing else return(false));
	  true)
     else false);
export getListOfReals(e:Expr) : array(double) := (
     when e is l:List do 
     new array(double) len length(l.v) do (
	  foreach i in l.v do when i is a:Real do provide a.v else nothing;
	  )
     else array(double)()
     );

export isListOfComplex(e:Expr) : bool := (
     when e is l:List do (
     	  foreach i in l.v do (
	       when i is a:Complex do nothing else return(false));
	  true)
     else false);
export getListOfComplex(e:Expr) : array(Complex) := (
     when e is l:List do 
     new array(Complex) len length(l.v) do (
	  foreach i in l.v do when i is a:Complex do provide a else nothing;
	  )
     else array(Complex)()
     );


export toExpr(x:LMatrixRROrNull):Expr := (
     when x
     is M:LMatrixRR do Expr(M)
     is null do buildErrorPacket(EngineError("unknown lapack engine error"))
     );
export toExpr(x:LMatrixCCOrNull):Expr := (
     when x
     is M:LMatrixCC do Expr(M)
     is null do buildErrorPacket(EngineError("unknown lapack engine error"))
     );
