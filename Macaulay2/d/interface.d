--		Copyright 1994-2002 by Daniel R. Grayson

-- this file contains top level routines that call the C++ code in the engine

use C;
use system; 
use util;
use convertr;
use binding;
use nets;
use parser;
use lex;
use gmp;
use engine;
use util;
use tokens;
use err;
use stdiop;
use ctype;
use stdio;
use varstrin;
use strings;
use basic;
use struct;
use objects;
use evaluate;
use common;

-----------------------------------------------------------------------------
-- types


-----------------------------------------------------------------------------
-- monomials

rawVar(a:Expr):Expr := (
     when a
     is v:Integer do 
     if isInt(v) then toExpr(
	  Ccode(RawMonomialOrNull, 
	       "(engine_RawMonomialOrNull)IM2_Monomial_var(", toInt(v), ",1)" ))
     else WrongArgSmallInteger()
     is s:Sequence do 
     if length(s) == 2 then 
     when s.0 is v:Integer do 
     if isInt(v) then 
     when s.1 is e:Integer do 
     if isInt(e) then toExpr(Ccode(RawMonomialOrNull, 
	       "(engine_RawMonomialOrNull)IM2_Monomial_var(",
	       toInt(v), ",", toInt(e), ")" ))
     else WrongArgSmallInteger(2)
     else WrongArgInteger(2)
     else WrongArgSmallInteger(1)
     else WrongArgInteger(1)
     else WrongArg("an integer or a pair of integers")
     else WrongArg("an integer or a pair of integers")
     );
setupfun("rawVar",rawVar);

rawMonomialSparseListForm(e:Expr):Expr := (
     when e 
     is x:RawMonomial do (
	  y := Ccode(RawArrayInt, "(engine_RawArrayInt)IM2_Monomial_to_arrayint((Monomial*)",x,")" );
	  n := length(y)/2;
	  list(new Sequence len n do (
		    for i from length(y)-2 to 0 by -2 do (
			 provide new Sequence len 2 do (
			      provide Expr(toInteger(y.i));
			      provide Expr(toInteger(y.(i+1))))))))
     else WrongArg("a raw monomial")
     );
setupfun("rawMonomialSparseListForm",rawMonomialSparseListForm);

rawMonomialMake(e:Expr):Expr := (
     -- accepts a list of pairs : {(5,4),(3,7),(2,1)}
     when e
     is l:List do (
	  when isSequenceOfPairsOfSmallIntegers(l.v) is s:string do return WrongArg(s) else nothing;
	  when Ccode(RawMonomialOrNull, 
	       "(engine_RawMonomialOrNull)IM2_Monomial_make(",
	          "(M2_arrayint)", getSequenceOfPairsOfSmallIntegers(l.v), 
	       ")" )
	  is x:RawMonomial do Expr(x)
	  is null do buildErrorPacket(EngineError("raw monomial overflow"))
	  )
     else WrongArg("a list of pairs of integers"));
setupfun("rawMonomialMake",rawMonomialMake);

rawMonomialIsOne(e:Expr):Expr := (
     when e is s:Sequence 
     do if length(s) == 2 
     then when s.0 is x:RawMonomial 
     do when s.1 is t:Integer 
     do if t === 1 
     then if Ccode(bool, "IM2_Monomial_is_one((Monomial*)",x,")") then True else False
     else WrongArg(2,"the integer 1")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw monomial")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
installMethod(Expr(EqualEqualS), rawMonomialClass,integerClass,
     Expr(CompiledFunction(rawMonomialIsOne,nextHash())));

rawRadical(e:Expr):Expr := (
     when e
     is x:RawMonomial do Expr(Ccode(RawMonomial, "(engine_RawMonomial)",
	       "IM2_Monomial_radical(", "(Monomial*)", x, ")" ) )
     is I:RawMonomialIdeal do Expr(
	  Ccode(RawMonomialIdeal, "(engine_RawMonomialIdeal)",
	       "IM2_MonomialIdeal_radical(", "(MonomialIdeal *)", I, ")" ) )
     else WrongArg("a raw monomial or monomial ideal"));
setupfun("rawRadical",rawRadical);

rawGCD(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is x:RawMonomial do
     when s.1 is y:RawMonomial do Expr(
	       Ccode(RawMonomial, "IM2_Monomial_gcd((Monomial*)",x,",","(Monomial*)",y,")"))
     else WrongArg(2,"a raw monomial")
     else WrongArg(1,"a raw monomial")
     else WrongArg("a pair of raw monomials")
     );
setupfun("rawGCD",rawGCD);

rawLCM(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is x:RawMonomial do
     when s.1 is y:RawMonomial do Expr(
	       Ccode(RawMonomial, "IM2_Monomial_lcm((Monomial*)",x,",","(Monomial*)",y,")"))
     else WrongArg(2,"a raw monomial")
     else WrongArg(1,"a raw monomial")
     else WrongArg("a pair of raw monomials")
     );
setupfun("rawLCM",rawLCM);

rawSaturate(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is x:RawMonomial do
     when s.1 is y:RawMonomial do Expr(
	       Ccode(RawMonomial, "IM2_Monomial_sat((Monomial*)",x,",","(Monomial*)",y,")"))
     else WrongArg(2,"a raw monomial")
     else when s.0 is I:RawMonomialIdeal do 
     when s.1
     is y:RawMonomial do Expr(
	  Ccode(RawMonomialIdeal, "(engine_RawMonomialIdeal)IM2_MonomialIdeal_sat1(",
	       "(MonomialIdeal *)", I, ",", "(Monomial *)", y, ")" ))
     is J:RawMonomialIdeal do toExpr(
	  Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)IM2_MonomialIdeal_sat(",
	       "(MonomialIdeal *)", I, ",", "(MonomialIdeal *)", J, ")" ))
     else WrongArg(2,"a raw monomial or monomial ideal")
     else WrongArg(1,"a raw monomial or monomial ideal")
     else WrongNumArgs(2)
     );
setupfun("rawSaturate",rawSaturate);

rawSyzygy(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is x:RawMonomial do
     when s.1 is y:RawMonomial do (
	  r := Ccode(RawMonomialPair, 
	       "(engine_RawMonomialPair)IM2_Monomial_syz((Monomial*)",
	       x,",","(Monomial*)",y,
	       ")");
	  Expr(list(Expr(r.a), Expr(r.b))))
     else WrongArg(2,"a raw monomial")
     else WrongArg(1,"a raw monomial")
     else WrongArg("a pair of raw monomials")
     );
setupfun("rawSyzygy",rawSyzygy);

rawQuotient(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is x:RawMonomial do 
     when a.1 is y:RawMonomial do toExpr(
	  Ccode(RawMonomialOrNull, "(engine_RawMonomialOrNull)IM2_Monomial_quotient(",
	       "(Monomial *)", x, ",", "(Monomial *)", y, ")" ))
     else WrongArg(2,"a raw monomial")
     else when a.0 is I:RawMonomialIdeal do 
     when a.1
     is y:RawMonomial do Expr(
	  Ccode(RawMonomialIdeal, "(engine_RawMonomialIdeal)IM2_MonomialIdeal_quotient1(",
	       "(MonomialIdeal *)", I, ",", "(Monomial *)", y, ")" ))
     is J:RawMonomialIdeal do toExpr(
	  Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)IM2_MonomialIdeal_quotient(",
	       "(MonomialIdeal *)", I, ",", "(MonomialIdeal *)", J, ")" ))
     else WrongArg(2,"a raw monomial or monomial ideal")
     else WrongArg(1,"a raw monomial or monomial ideal")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawQuotient",rawQuotient);

-----------------------------------------------------------------------------
-- monomial orderings

PositionS := makeProtectedSymbolClosure("Position");
UpS := makeProtectedSymbolClosure("Up");
DownS := makeProtectedSymbolClosure("Down");
PositionFun(b:bool):RawMonomialOrdering := (		    -- b is true for Up, false for Down
     Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_position(",b,")")
     );

Lex      := makeProtectedSymbolClosure("Lex");
LexSmall := makeProtectedSymbolClosure("LexSmall");
LexTiny  := makeProtectedSymbolClosure("LexTiny");
LexFun     (n:int):RawMonomialOrdering := Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_lex(",n,",1)");
LexSmallFun(n:int):RawMonomialOrdering := Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_lex(",n,",2)");
LexTinyFun (n:int):RawMonomialOrdering := Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_lex(",n,",4)");

RevLex := makeProtectedSymbolClosure("RevLex");
RevLexFun(n:int):RawMonomialOrdering := Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_revlex(",n,")");

GroupLex := makeProtectedSymbolClosure("GroupLex");
GroupLexFun(n:int):RawMonomialOrdering := Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_laurent(",n,")");

NCLex := makeProtectedSymbolClosure("NCLex");
NCLexFun(n:int):RawMonomialOrdering := Ccode(RawMonomialOrdering, "(engine_RawMonomialOrdering)IM2_MonomialOrdering_NClex(",n,")");

GRevLex := makeProtectedSymbolClosure("GRevLex");
GRevLexSmall := makeProtectedSymbolClosure("GRevLexSmall");
GRevLexTiny := makeProtectedSymbolClosure("GRevLexTiny");
GRevLexFun(n:array(int)):RawMonomialOrdering := Ccode(RawMonomialOrdering,
     "(engine_RawMonomialOrdering)IM2_MonomialOrdering_grevlex((M2_arrayint)",n,",1)");
GRevLexSmallFun(n:array(int)):RawMonomialOrdering := Ccode(RawMonomialOrdering,
     "(engine_RawMonomialOrdering)IM2_MonomialOrdering_grevlex((M2_arrayint)",n,",2)");
GRevLexTinyFun(n:array(int)):RawMonomialOrdering := Ccode(RawMonomialOrdering,
     "(engine_RawMonomialOrdering)IM2_MonomialOrdering_grevlex((M2_arrayint)",n,",4)");

Weights := makeProtectedSymbolClosure("Weights");
WeightsFun(n:array(int)):RawMonomialOrdering := Ccode(RawMonomialOrdering,
     "(engine_RawMonomialOrdering)IM2_MonomialOrdering_weights((M2_arrayint)",n,")");

join(s:RawMonomialOrderingArray):RawMonomialOrdering := (
     Ccode(RawMonomialOrdering,
     	  "(engine_RawMonomialOrdering)IM2_MonomialOrdering_join((MonomialOrdering_array)",s,")")
     );

arrayint := array(int);
funtype := fun1 or fun2 or fun3 or fun4;
funtypeornull := fun1 or fun2 or fun3 or fun4 or null;
fun1 := function():RawMonomialOrdering;
fun2 := function(int):RawMonomialOrdering;
fun3 := function(arrayint):RawMonomialOrdering;;
fun4 := function(bool):RawMonomialOrdering;;

Maker := { sym:SymbolClosure, fun:funtype };

makers := array(Maker)(
     Maker(PositionS,PositionFun),
     Maker(Lex,LexFun),
     Maker(LexSmall,LexSmallFun),
     Maker(LexTiny,LexTinyFun),
     Maker(RevLex,RevLexFun),
     Maker(GroupLex,GroupLexFun),
     Maker(NCLex,NCLexFun),
     Maker(GRevLex,GRevLexFun),
     Maker(GRevLexSmall,GRevLexSmallFun),
     Maker(GRevLexTiny,GRevLexTinyFun),
     Maker(Weights,WeightsFun)
     );

getmaker(sym:SymbolClosure):funtypeornull := (
     foreach pair in makers do if sym == pair.sym then (
	  when pair.fun
	  is f:fun1 do return f
	  is f:fun2 do return f
	  is f:fun3 do return f
     	  is f:fun4 do return f
	  );
     null());

-- trivialMonomial := Ccode(RawMonomial, 
--      "(engine_RawMonomial)IM2_Monomial_make(", "(M2_arrayint)", array(int)(), ")" 
--      );

rawMonomialOrdering(e:Expr):Expr := (
     -- This routine gets an expression like this:
     -- { GRevLexSmall => {1,2,3}, PositionS, LexTiny => 4, Lex => 5, Weights => {1,2,3} }
     -- For GRevLex, the weights are already provided by top level code.
     -- Each member of the sequence results in one monomial ordering, and they sequence
     -- is then "joined".
     -- The weights for grevlex have to be > 0.
     -- Limit the total number of variables to 2^15-1.
     when e is s:List do (
	  -- first check it
	  foreach spec in s.v do (
	       when spec is sp:List do
	       if sp.class == optionClass && length(sp.v)==2 then 
	       when sp.v.0 
	       is sym:SymbolClosure do (
		    when getmaker(sym)
		    is g:fun1 do (
			 if sp.v.1 != nullE
			 then return buildErrorPacket("expected option value to be 'null'");
			 )
		    is g:fun2 do (
			 if !isSmallInt(sp.v.1)
			 then return buildErrorPacket("expected option value to be a small integer");
			 )
		    is g:fun3 do (
			 if !isSequenceOfSmallIntegers(sp.v.1)
			 then return buildErrorPacket("expected option value to be a sequence of small integers");
			 )
		    is g:fun4 do (
			 if g == PositionFun then (
			      if !(sp.v.1 == UpS || sp.v.1 == DownS)
			      then return buildErrorPacket("expected option value to be Up or Down");
			      )
			 else (
			      if !(sp.v.1 == True || sp.v.1 == False)
			      then return buildErrorPacket("expected option value to be true or false");
			      ))
		    is null do return buildErrorPacket("expected option key to be a monomial ordering key")
		    )
	       else return buildErrorPacket("expected option key to be a symbol")
	       else return WrongArg("a list of options")
	       else return WrongArg("a list of options"));
	  -- then accumulate it
     	  Expr(join(new RawMonomialOrderingArray len length(s.v) do (
	       foreach spec in s.v do
	       when spec is sp:List do
	       when sp.v.0 is sym:SymbolClosure do (
		    when getmaker(sym)
		    is g:fun1 do provide g()
		    is g:fun2 do provide g(getSmallInt(sp.v.1))
		    is g:fun3 do provide g(getSequenceOfSmallIntegers(sp.v.1))
		    is g:fun4 do provide g(if g == PositionFun then sp.v.1 == UpS else sp.v.1 == True)
		    is null do nothing
		    )
	       else nothing
	       else nothing;
	       provide PositionFun(true);		    -- just in case, to prevent a loop
	       ))))
     else WrongArg("a list of options"));
setupfun("rawMonomialOrdering",rawMonomialOrdering);

rawMonomialOrderingProduct(e:Expr):Expr := (
     when e
     is m:RawMonomialOrdering do e
     is s:Sequence do 
     if !isSequenceOfMonomialOrderings(s) 
     then WrongArg("a sequence of raw monomial orderings") 
     else Expr(Ccode(
	       RawMonomialOrdering, 
	       "(engine_RawMonomialOrdering)IM2_MonomialOrdering_product(",
	       "(MonomialOrdering_array)", getSequenceOfMonomialOrderings(s),
	       ")"
	       ))
     else WrongArg("a sequence of raw monomial orderings"));
setupfun("rawMonomialOrderingProduct",rawMonomialOrderingProduct);

rawNumberOfVariables(e:Expr):Expr := (
     when e
     is m:RawMonomialOrdering do toExpr(Ccode( int, "IM2_MonomialOrdering_nvars(", "(MonomialOrdering *)", m, ")" ))
     else WrongArg("a monomial ordering"));
setupfun("rawNumberOfVariables",rawNumberOfVariables);

rawNumberOfInvertibleVariables(e:Expr):Expr := (
     when e
     is m:RawMonomialOrdering do toExpr(Ccode( int, "IM2_MonomialOrdering_n_invertible_vars(", "(MonomialOrdering *)", m, ")" ))
     else WrongArg("a monomial ordering"));
setupfun("rawNumberOfInvertibleVariables",rawNumberOfInvertibleVariables);

-----------------------------------------------------------------------------
-- monoids

rawMonoid(mo:RawMonomialOrdering,names:array(string),degreesMonoid:RawMonoid,degs:array(int)):Expr := (
     when Ccode(RawMonoidOrNull, 
	  "(engine_RawMonoidOrNull)IM2_Monoid_make(",
	      "(MonomialOrdering *)", mo, ",",
	      "(M2_stringarray)", names, ",",
	      "(Monoid *)", degreesMonoid, ",",
	      "(M2_arrayint)", degs,
	  ")")
     is m:RawMonoid do Expr(m)
     is null do buildErrorPacket(EngineError("internal error: unexplained failure to make raw monoid"))
     );
rawMonoid(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 0 then Expr(Ccode(RawMonoid,"(engine_RawMonoid)IM2_Monoid_trivial()"))
     else if length(s) == 4 then 
     when s.0 is mo:RawMonomialOrdering do
     if isListOfStrings(s.1) then (
	  names := getListOfStrings(s.1);
	  when s.2 is degreesMonoid:RawMonoid do
	  if isSequenceOfSmallIntegers(s.3) then (
	       degs := getSequenceOfSmallIntegers(s.3);
	       rawMonoid(mo,names,degreesMonoid,degs))
	  else WrongArg(4,"a sequence of small integers (flattened degrees)")
	  else WrongArg(3,"the degrees monoid"))
     else WrongArg(2,"a list of strings to be used as names")
     else WrongArg(1,"a monomial ordering")
     else buildErrorPacket("expected 0 or 4 arguments")
     else buildErrorPacket("expected 0 or 4 arguments")
     );
setupfun("rawMonoid",rawMonoid);

-----------------------------------------------------------------------------
-- rings

rawZZ(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 0
     then Expr(Ccode(RawRing,"(engine_RawRing)IM2_Ring_ZZ()"))
     else WrongNumArgs(0)
     else WrongNumArgs(0)
     );
setupfun("rawZZ", rawZZ);

rawQQ(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 0
     then Expr(Ccode(RawRing,"(engine_RawRing)IM2_Ring_QQ()"))
     else WrongNumArgs(0)
     else WrongNumArgs(0)
     );
setupfun("rawQQ", rawQQ);

rawZZp(e:Expr):Expr := (
     when e is p:Integer do if !isInt(p) then WrongArgSmallInteger(1) else toExpr(Ccode(RawRingOrNull, "(engine_RawRingOrNull)IM2_Ring_ZZp(", toInt(p), ")" ))
     else WrongArgInteger());
setupfun("rawZZp", rawZZp);

rawRR(e:Expr):Expr := (
     when e is epsilon:Real do toExpr(Ccode(RawRingOrNull, "(engine_RawRingOrNull)IM2_Ring_RR(", epsilon.v, ")" ))
     else WrongArg("a real number"));
setupfun("rawRR",rawRR);

rawCC(e:Expr):Expr := (
     when e is epsilon:Real do toExpr(Ccode(RawRingOrNull, "(engine_RawRingOrNull)IM2_Ring_CC(", epsilon.v, ")" ))
     else WrongArg("a real number"));
setupfun("rawCC",rawCC);

rawBigRR(e:Expr):Expr := when e is s:Sequence do if length(s) != 0 then WrongNumArgs(0) else toExpr(Ccode(RawRingOrNull, "(engine_RawRingOrNull)IM2_Ring_bigRR()" )) else WrongNumArgs(0);
setupfun("rawBigRR",rawBigRR);

rawBigCC(e:Expr):Expr := when e is s:Sequence do if length(s) != 0 then WrongNumArgs(0) else toExpr(Ccode(RawRingOrNull, "(engine_RawRingOrNull)IM2_Ring_bigCC()" )) else WrongNumArgs(0);
setupfun("rawBigCC",rawBigCC);

rawPolynomialRing(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is K:RawRing do 
     when a.1 is M:RawMonoid do toExpr(Ccode(RawRingOrNull,
	       "(engine_RawRingOrNull)IM2_Ring_polyring(",
	       "(Ring *)", K, ",",
	       "(Monoid *)", M,
	       ")"
	       ))
     else WrongArg(2,"a raw monoid")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawPolynomialRing",rawPolynomialRing);

rawSkewPolynomialRing(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do 
     if !isSequenceOfSmallIntegers(a.1) then WrongArg(2,"a sequence of small integers")
     else toExpr(Ccode(RawRingOrNull,
 	       "(engine_RawRingOrNull)IM2_Ring_skew_polyring(",
 	       "(Ring *)", R, ",",
 	       "(M2_arrayint)", getSequenceOfSmallIntegers(a.1), -- skew variables
 	       ")"
 	       ))
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawSkewPolynomialRing",rawSkewPolynomialRing);

rawWeylAlgebra(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 4 then 
     when a.0 is R:RawRing do 
     if !isSequenceOfSmallIntegers(a.1) then WrongArg(2,"a sequence of small integers") else
     if !isSequenceOfSmallIntegers(a.2) then WrongArg(3,"a sequence of small integers") else
     if !isSmallInt(a.3) then WrongArgSmallInteger(4) else
     toExpr(Ccode(RawRingOrNull,
	       "(engine_RawRingOrNull)IM2_Ring_weyl_algebra(",
	       "(Ring *)", R, ",",
	       "(M2_arrayint)", getSequenceOfSmallIntegers(a.1), ",",  -- commvars
	       "(M2_arrayint)", getSequenceOfSmallIntegers(a.2), ",", -- diff vars
	       getSmallInt(a.3), -- homog var
	       ")"
	       ))
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("rawWeylAlgebra",rawWeylAlgebra);

rawSolvableAlgebra(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do 
     when a.1 is Q:RawMatrix do toExpr(Ccode(RawRingOrNull,
 	       "(engine_RawRingOrNull)IM2_Ring_solvable_algebra(",
 	       "(Ring *)", R, ",",
 	       "(Matrix*)", Q,				    -- how to rewrite x_j*x_i
 	       ")"
 	       ))
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawSolvableAlgebra",rawSolvableAlgebra);

rawLocalRing(e:Expr):Expr := (			    -- localization at a prime ideal
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do 
     when a.1 is P:RawMatrix do toExpr(Ccode(RawRingOrNull,
 	       "(engine_RawRingOrNull)IM2_Ring_localization(",
 	       "(Ring *)", R, ",",
 	       "(Matrix*)", P,				    -- 1 by n matrix generating the prime ideal
 	       ")"
 	       ))
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawLocalRing",rawLocalRing);

rawQuotientRing(e:Expr):Expr := (			    -- localization at a prime ideal
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do 
     when a.1 is I:RawMatrix do toExpr(Ccode(RawRingOrNull,
 	       "(engine_RawRingOrNull)IM2_Ring_quotient(",
 	       "(Ring *)", R, ",",
 	       "(Matrix*)", I,				    -- 1 by n matrix generating the ideal
 	       ")"
 	       ))
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawQuotientRing",rawQuotientRing);

rawFractionRing(e:Expr):Expr := (
     when e is R:RawRing do Expr(
	  Ccode(RawRing,"(engine_RawRing)IM2_Ring_frac(",
	       "(Ring *)", R,
	       ")"))
     else WrongArg("a raw ring")
     );
setupfun("rawFractionRing", rawFractionRing);

rawSchurRing(e:Expr):Expr := (
     when e is R:RawRing do toExpr(Ccode(RawRingOrNull, "(engine_RawRingOrNull)IM2_Ring_schur(", "(Ring *)", R, ")" ) )
     else WrongArg("a raw ring"));
setupfun("rawSchurRing",rawSchurRing);

rawIsField(e:Expr):Expr := (
     when e is K:RawRing do toExpr(Ccode(bool, "IM2_Ring_is_field(", "(Ring *)", K, ")" ))
     else WrongArg("a raw ring"));
setupfun("rawIsField",rawIsField);

rawDeclareField(e:Expr):Expr := (
     when e is K:RawRing do (
	  Ccode(void, "IM2_Ring_declare_field(", "(Ring *)", K, ")" );
	  nullE)
     else WrongArg("a raw ring"));
setupfun("rawDeclareField",rawDeclareField);

rawGetZeroDivisor(e:Expr):Expr := (
     when e is K:RawRing do Expr(Ccode(RawRingElement, 
	       "(engine_RawRingElement)IM2_Ring_get_zero_divisor(", "(Ring *)", K, ")" ))
     else WrongArg("a raw ring"));
setupfun("rawGetZeroDivisor",rawGetZeroDivisor);

-----------------------------------------------------------------------------
-- ring elements

rawRingVar(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do
     when a.1 is v:Integer do
     if !isInt(v) then WrongArgSmallInteger(2) else
     toExpr(Ccode(RawRingElementOrNull,
	       "(engine_RawRingElementOrNull)IM2_RingElement_make_var(",
	       "(Ring *)", R, ",",
	       toInt(v), ",",
	       1,
	       ")"
	       ))
     else WrongArgInteger(2)
     else WrongArg(1,"a raw ring")
     else if length(a) == 3 then
     when a.0 is R:RawRing do
     when a.1 is v:Integer do
     if !isInt(v) then WrongArgSmallInteger(2) else
     when a.2 is e:Integer do
     if !isInt(e) then WrongArgSmallInteger(3) else
     toExpr(Ccode(RawRingElementOrNull,
	       "(engine_RawRingElementOrNull)IM2_RingElement_make_var(",
	       "(Ring *)", R, ",",
	       toInt(v), ",",
	       toInt(e),
	       ")"
	       ))
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("rawRingVar",rawRingVar);

rawFromNumber(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 2 then
     when s.0
     is R:RawRing do
     when s.1
     is n:Integer do Expr(Ccode(
	       RawRingElement, "IM2_RingElement_from_Integer(",
	       "(Ring*)",R,",",
	       "(M2_Integer)",n,
	       ")"))
     is x:Real do (
	  when Ccode(RawRingElementOrNull, "IM2_RingElement_from_double((Ring*)",R,",",x.v,")")
	  is r:RawRingElement do Expr(r)
	  is null do buildErrorPacket(EngineError("promoting real number to ring element: not implemented yet")))
     is x:Complex do (
	  when Ccode(RawRingElementOrNull, "IM2_RingElement_from_complex((Ring*)",R,",(M2_CC)", x,")")
	  is r:RawRingElement do Expr(r)
	  is null do buildErrorPacket(EngineError("promoting real number to ring element: not implemented yet")))
     is x:BigReal do (
	  when Ccode(RawRingElementOrNull, "IM2_RingElement_from_BigReal((Ring*)",R,",(M2_BigReal)",x,")")
	  is r:RawRingElement do Expr(r)
	  is null do
	  buildErrorPacket(EngineError("can't promote big real number to ring element")))
     else WrongArg(2,"an integer or real number or complex or big real")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2)
     );
setupfun("rawFromNumber", rawFromNumber);

rawMultiDegree(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr(
	  Ccode(RawArrayIntOrNull, "(engine_RawArrayIntOrNull)IM2_RingElement_multidegree(",
	       "(RingElement*)",x, ")" ) )
     is x:RawFreeModule do toExpr(
	  Ccode(RawArrayIntOrNull, "(engine_RawArrayIntOrNull)IM2_FreeModule_get_degrees(",
	       "(FreeModule*)",x, ")" ) )
     is x:RawMatrix do toExpr(
	  Ccode(RawArrayIntOrNull, "(engine_RawArrayIntOrNull)IM2_Matrix_get_degree(",
	       "(Matrix*)",x, ")" ) )
     else WrongArg("a raw ring element or vector")
     );
setupfun("rawMultiDegree",rawMultiDegree);

rawDegree(e:Expr):Expr := (
     when e
     is x:RawMonomial do Expr(toInteger(Ccode(int, "IM2_Monomial_degree((Monomial*)",x,")")))
     is s:Sequence do 
     if length(s) != 2 then buildErrorPacket("expected 1 or 2 arguments") else
     when s.0 is x:RawRingElement do 
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else
     toExpr(
	  Ccode( IntegerPairOrNull, "(engine_IntegerPairOrNull)IM2_RingElement_degree(",
	       "(RingElement*)",x, ",",
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.1),
	       ")"
	       ))
     else WrongArg(1,"a raw ring element")
     else WrongArg("a raw monomial or a pair: raw ring element, list of weights")
     );
setupfun("rawDegree",rawDegree);

rawTermCount(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( Ccode( int, "IM2_RingElement_n_terms(", "(RingElement*)",x, ")" ))
     else WrongArg("a raw ring element or vector")
     );
setupfun("rawTermCount",rawTermCount);

rawIsHomogeneous(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( Ccode( bool, "IM2_RingElement_is_graded(", "(RingElement*)",x, ")" ))
     is x:RawMatrix do toExpr( Ccode( bool, "IM2_Matrix_is_graded(", "(Matrix*)",x, ")" ))
     else WrongArg("a raw ring element or vector")
     );
setupfun("rawIsHomogeneous",rawIsHomogeneous);

isMutable(x:RawMatrix):bool := Ccode( bool, "IM2_Matrix_is_mutable(", "(Matrix*)",x, ")" );
rawIsMutable(e:Expr):Expr := when e is x:RawMatrix do toExpr(isMutable(x)) else WrongArg("a raw ring element or vector");
setupfun("rawIsMutable",rawIsMutable);

rawIsZero(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( Ccode( bool, "IM2_RingElement_is_zero(", "(RingElement*)",x, ")" ))
     is x:RawMatrix do toExpr( Ccode( bool, "IM2_Matrix_is_zero(", "(Matrix*)",x, ")" ))
     else WrongArg("a raw ring element or vector")
     );
setupfun("rawIsZero",rawIsZero);

rawToInteger(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( 
	  Ccode( IntegerOrNull, "(engine_IntegerOrNull)IM2_RingElement_to_Integer(", "(RingElement*)",x, ")" ))
     else WrongArg("a raw ring element")
     );
setupfun("rawToInteger",rawToInteger);

rawLeadCoefficient(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( 
	  Ccode( RawRingElementOrNull, 
	       "(engine_RawRingElementOrNull)IM2_RingElement_lead_coeff(", "(RingElement *)",x, ")" ))
     else WrongArg("a raw ring element or vector")
     );
setupfun("rawLeadCoefficient",rawLeadCoefficient);

rawLeadMonomial(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( 
	  Ccode( RawMonomialOrNull, 
	       "(engine_RawMonomialOrNull)IM2_RingElement_lead_monomial(",
	       "(RingElement*)",x, 
	       ")" ))
     else WrongArg("a raw ring element")
     );
setupfun("rawLeadMonomial",rawLeadMonomial);

rawPairs(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( 
	  Ccode( RawArrayPairOrNull, "(engine_RawArrayPairOrNull)IM2_RingElement_list_form(",
	       "(RingElement*)",x, 
	       ")" ))
     else WrongArg("a raw ring element")
     );
setupfun("rawPairs",rawPairs);

ringElementMod(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is x:RawRingElement do 
     when a.1 is y:RawRingElement do toExpr(x % y)
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw ring element")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
installMethod(PercentS,rawRingElementClass,rawRingElementClass,ringElementMod);

rawDivMod(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is x:RawRingElement do 
     when a.1 is y:RawRingElement do toExpr(
	  Ccode(RawRingElementPairOrNull,
	       "(engine_RawRingElementPairOrNull)",
	       "IM2_RingElement_divmod(",
	       "(RingElement *)", x, ",",
	       "(RingElement *)", y,
	       ")"
	       )
	  )
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw ring element")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawDivMod",rawDivMod);

rawPromote(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do 
     when a.1 is x:RawRingElement do toExpr(
	  Ccode(RawRingElementOrNull, 
		    "(engine_RawRingElementOrNull)IM2_RingElement_promote(",
		    "(Ring *)", R,
		    ",(RingElement *)", x,
		    ")" ))
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawPromote", rawPromote);

rawLift(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is R:RawRing do 
     when a.1 is x:RawRingElement do toExpr(
	  Ccode(RawRingElementOrNull, 
		    "(engine_RawRingElementOrNull)IM2_RingElement_lift(",
		    "(Ring *)", R,
		    ",(RingElement *)", x,
		    ")" ))
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawLift", rawLift);

rawRing(e:Expr):Expr := (
     when e
     is x:RawRingElement do Expr(
	  Ccode(RawRing, "IM2_RingElement_ring(", "(RingElement *)",x, ")" ))
     is x:RawFreeModule do Expr(
	  Ccode(RawRing, "IM2_FreeModule_ring(", "(FreeModule *)",x, ")" ))
     else WrongArg("a raw ring element or free module")
     );
setupfun("rawRing", rawRing);

rawHomogenize(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 3 then (
	  when s.0
	  is a:RawRingElement do (
	       if !isSmallInt(s.1) then WrongArgSmallInteger(2) else
	       if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else
	       toExpr(Ccode(RawRingElementOrNull,
			 "(engine_RawRingElementOrNull)IM2_RingElement_homogenize(",
			 "(RingElement *)", a, ",",
			 getSmallInt(s.1), ",",		    -- var number v
			 "(M2_arrayint)", getSequenceOfSmallIntegers(s.2), -- weights
			 ")"
			 )
		    )
	       )
	  is M:RawMatrix do (
	       if !isSmallInt(s.1) then WrongArgSmallInteger(2) else
	       if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else
	       toExpr(Ccode(RawMatrixOrNull,
			 "(engine_RawMatrixOrNull)IM2_Matrix_homogenize(",
			 "(Matrix *)", M, ",",
			 getSmallInt(s.1), ",",		    -- var number v
			 "(M2_arrayint)", getSequenceOfSmallIntegers(s.2), -- weights
			 ")"
			 )
		    )
	       )
	  else WrongArg(1,"a raw ring element, matrix, or vector")
	  )
     else if length(s) == 4 then (
	  when s.0
	  is a:RawRingElement do (
	       if !isSmallInt(s.1) then WrongArgSmallInteger(2) else
	       if !isSmallInt(s.2) then WrongArgSmallInteger(3) else
	       if !isSequenceOfSmallIntegers(s.3) then WrongArg(4,"a sequence of small integers") else
	       toExpr(Ccode(RawRingElementOrNull,
			 "(engine_RawRingElementOrNull)IM2_RingElement_homogenize_to_degree(",
			 "(RingElement *)", a, ",",
			 getSmallInt(s.1), ",",		    -- var number v
			 getSmallInt(s.2), ",",		    -- target degree
			 "(M2_arrayint)", getSequenceOfSmallIntegers(s.3), -- weights
			 ")"
			 )
		    )
	       )
	  else WrongArg(1,"a raw ring element or vector")
	  )
     else buildErrorPacket("expected 3 or 4 arguments")
     else buildErrorPacket("expected 3 or 4 arguments"));
setupfun("rawHomogenize",rawHomogenize);

rawTerm(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) == 3 then 
     when s.0 is R:RawRing do 
     when s.1 is a:RawRingElement do
     when s.2 is m:RawMonomial do toExpr(Ccode(RawRingElementOrNull,
	       "(engine_RawRingElementOrNull)IM2_RingElement_term(",
	       "(Ring *)", R, ",", "(RingElement *)", a, ",", "(Monomial *)", m, ")" ))
     else WrongArg(3,"a raw monomial")
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw ring")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawTerm",rawTerm);

rawGetTerms(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) == 3 then 
     when s.0 is f:RawRingElement do (
	  when s.1 is lo:Integer do if !isInt(lo) then WrongArgSmallInteger(2) else
	  when s.2 is hi:Integer do if !isInt(hi) then WrongArgSmallInteger(3) else
	  Expr(Ccode(RawRingElement,
		    "(engine_RawRingElement)IM2_RingElement_get_terms(",
		    "(RingElement *)", f, ",", toInt(lo), ",", toInt(hi), ")" ))
	  else WrongArgInteger(3)
	  else WrongArgInteger(2))
     else WrongArg(1,"a ring element")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGetTerms",rawGetTerms);

rawCoefficient(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is x:RawRingElement do 
     when a.1 is m:RawMonomial do toExpr(
	  Ccode(RawRingElementOrNull,"(engine_RawRingElementOrNull)",
	       "IM2_RingElement_get_coeff(",
	       "(RingElement *)", x, ",",
	       "(Monomial *)", m,
	       ")"))
     else WrongArg(2,"a raw monomial")
     else WrongArg(1,"a raw ring element")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawCoefficient",rawCoefficient);

rawNumerator(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( 
	  Ccode( RawRingElementOrNull, 
	       "(engine_RawRingElementOrNull)IM2_RingElement_numerator(",
	       "(RingElement*)",x, ")" ))
     else WrongArg("a raw ring element"));
setupfun("rawNumerator",rawNumerator);

rawDenominator(e:Expr):Expr := (
     when e
     is x:RawRingElement do toExpr( 
	  Ccode( RawRingElementOrNull, 
	       "(engine_RawRingElementOrNull)IM2_RingElement_denominator(",
	       "(RingElement*)",x, ")" ))
     else WrongArg("a raw ring element")
     );
setupfun("rawDenominator",rawDenominator);

rawFraction(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) == 3 then 
     when s.0 is F:RawRing do 
     when s.1 is x:RawRingElement do
     when s.2 is y:RawRingElement do toExpr(Ccode(RawRingElementOrNull,
	       "(engine_RawRingElementOrNull)IM2_RingElement_fraction(",
	       "(Ring *)", F, ",", "(RingElement *)", x, ",", "(RingElement *)", y, ")" ))
     else WrongArg(3,"a raw ring element")
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw fraction ring")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawFraction",rawFraction);

-----------------------------------------------------------------------------
-- free modules

rawRank(e:Expr):Expr := (
     when e
     is x:RawFreeModule do toExpr( Ccode( int, "IM2_FreeModule_rank(", "(FreeModule*)",x, ")" ))
     else WrongArg("a raw free module")
     );
setupfun("rawRank",rawRank);

rawFreeModule(e:Expr):Expr := (
     when e
     is m:RawMatrix do toExpr(
	  Ccode(RawFreeModuleOrNull, "(engine_RawFreeModuleOrNull)IM2_FreeModule_make_schreyer(",
	       "(Matrix *)", m, ")" ) )
     is s:Sequence do
     if length(s) == 2 then (
	  when s.0
	  is R:RawRing do (
	       when s.1
	       is rank:Integer do (
		    if isInt(rank)
		    then toExpr( Ccode( RawFreeModuleOrNull, 
			      "(engine_RawFreeModuleOrNull)IM2_FreeModule_make(",
			      "(Ring*)",R, ",", toInt(rank), ")" ))
		    else WrongArg(2,"a small integer or a sequence of small integers"))
	       is degs:Sequence do (
		    if isSequenceOfSmallIntegers(degs)
		    then toExpr( Ccode( RawFreeModuleOrNull, 
			      "(engine_RawFreeModuleOrNull)IM2_FreeModule_make_degs(",
			      "(Ring*)", R, ",", 
			      "(M2_arrayint)", getSequenceOfSmallIntegers(degs),
			      ")" ))
		    else WrongArg(2,"a small integer or a sequence of small integers"))
	       else WrongArg(2,"a small integer or a sequence of small integers"))
	  else WrongArg(1,"a raw ring"))
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawFreeModule",rawFreeModule);

rawGetSchreyer(e:Expr):Expr := (
     when e
     is F:RawFreeModule do Expr(
	  Ccode(RawMatrix, "(engine_RawMatrix)IM2_FreeModule_get_schreyer(", "(FreeModule *)", F, ")" ) )
     else WrongArg("a raw free module"));
setupfun("rawGetSchreyer",rawGetSchreyer);

rawZero(e:Expr):Expr := (
     when e
     is s:Sequence do
     if length(s) != 2 then WrongNumArgs(1,2) else
     when s.0 is F:RawFreeModule do
     when s.1 is G:RawFreeModule do toExpr(Ccode(RawMatrixOrNull,"(engine_RawMatrixOrNull)",
		    "IM2_Matrix_zero(",
		    "(FreeModule *)", F, ",",
		    "(FreeModule *)", G,
		    ")" ) )
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw free module")
     else WrongArg("a raw free module or a pair of raw free modules"));
setupfun("rawZero",rawZero);

rawExteriorPower(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 2 then
     when s.0 is n:Integer do
     if !isInt(n) then WrongArgSmallInteger(1) else
     when s.1 is F:RawFreeModule do Expr(Ccode(RawFreeModule, "(engine_RawFreeModule)",
	       "IM2_FreeModule_exterior(",
	       toInt(n), ",",
	       "(FreeModule *)", F,
	       ")" ))
     else WrongArg(2,"a raw free module")
     else WrongArgInteger(1)
     else if length(s) == 3 then
     when s.0 is n:Integer do
     if !isInt(n) then WrongArgSmallInteger(1) else
     when s.1 is M:RawMatrix do
     when s.2 is strategy:Integer do
     if !isInt(strategy) then WrongArgSmallInteger(3) 
     else toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
	       "IM2_Matrix_exterior(",
	       toInt(n), ",",
	       "(Matrix *)", M, ",",
	       toInt(strategy),
	       ")" ))
     else WrongArgInteger(3)
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("rawExteriorPower",rawExteriorPower);

rawSymmetricPower(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is n:Integer do
     if !isInt(n) then WrongArgSmallInteger(1) else
     when s.1
     is F:RawFreeModule do Expr(Ccode(RawFreeModule, "(engine_RawFreeModule)",
	       "IM2_FreeModule_symm(",
	       toInt(n), ",",
	       "(FreeModule *)", F,
	       ")" ))
     is M:RawMatrix do toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
	       "IM2_Matrix_symm(",
	       toInt(n), ",",
	       "(Matrix *)", M,
	       ")" ))
     else WrongArg(2,"a raw matrix or free module")
     else WrongArgInteger(1)
     else WrongNumArgs(2));
setupfun("rawSymmetricPower",rawSymmetricPower);

rawDual(e:Expr):Expr := (
     when e
     is F:RawFreeModule do Expr(Ccode(RawFreeModule, "(engine_RawFreeModule)",
	       "IM2_FreeModule_dual(", "(FreeModule *)", F, ")" ))
     is M:RawMatrix do Expr(Ccode(RawMatrix, "(engine_RawMatrix)",
	       "IM2_Matrix_transpose(", "(Matrix *)", M, ")" ))
     else WrongArg("a raw free module or matrix"));
setupfun("rawDual",rawDual);

rawDirectSum(e:Expr):Expr := (
     if isSequenceOfMatrices(e) then toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
	       "IM2_Matrix_direct_sum(", 
	       "(Matrix_array *)", getSequenceOfMatrices(e),
	       ")"))
     else when e is s:Sequence do
     if length(s) == 2 then
     when s.0 is F:RawFreeModule do
     when s.1 is G:RawFreeModule do toExpr(Ccode(RawFreeModuleOrNull, "(engine_RawFreeModuleOrNull)",
	       "IM2_FreeModule_sum(", "(FreeModule *)", F, ",(FreeModule *)", G, ")" ))
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw matrix or free module")
     else WrongArg("a sequence of raw matrices or a pair of raw free modules")
     else WrongArg("a sequence of raw matrices or a pair of raw free modules")
     );
setupfun("rawDirectSum",rawDirectSum);

rawSubmodule(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is M:RawFreeModule do
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else (
	  selection := getSequenceOfSmallIntegers(s.1);
	  toExpr(Ccode(RawFreeModuleOrNull, "(engine_RawFreeModuleOrNull)",
		    "IM2_FreeModule_submodule(",
		    "(FreeModule *)", M, ",",
		    "(M2_arrayint)", selection,
		    ")" ) ) )
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(2));
setupfun("rawSubmodule",rawSubmodule);

-----------------------------------------------------------------------------
-- matrices

rawSource(e:Expr):Expr := (
     when e
     is M:RawMatrix do Expr( Ccode( RawFreeModule, "IM2_Matrix_get_source(", "(Matrix*)",M, ")" ))
     else WrongArg("a raw matrix")
     );
setupfun("rawSource",rawSource);

rawTarget(e:Expr):Expr := (
     when e
     is M:RawMatrix do Expr( Ccode( RawFreeModule, "IM2_Matrix_get_target(", "(Matrix*)",M, ")" ))
     is F:RawRingMap do Expr( Ccode( RawRing, "IM2_RingMap_target(", "(RingMap *)",F, ")" ))
     else WrongArg("a raw matrix")
     );
setupfun("rawTarget",rawTarget);

rawMatrix1(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) != 4 then WrongNumArgs(4) else
     when s.0 is target:RawFreeModule do 
     when s.1 is ncols:Integer do if !isInt(ncols) then WrongArgSmallInteger(2) else 
     if isSequenceOfRingElements(s.2) then 
     when s.3 is mutable:Boolean do 
     toExpr(Ccode(RawMatrixOrNull, 
	       "(engine_RawMatrixOrNull)IM2_Matrix_make1(",
	       "(FreeModule*)", target, ",",
	       toInt(ncols), ",",
	       "(RingElement_array *)", getSequenceOfRingElements(s.2), ",", -- entries
	       mutable == True, ")"))
     else WrongArgBoolean(4)
     else WrongArg(3,"a sequence of ring elements")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(4));
setupfun("rawMatrix1",rawMatrix1);

rawMatrix2(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) != 5 then WrongNumArgs(5) else
     when s.0 is target:RawFreeModule do 
     when s.1 is source:RawFreeModule do
     if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else
     if !isSequenceOfRingElements(s.3) then WrongArg(4,"a sequence of ring elements") else
     when s.4 is mutable:Boolean do toExpr(Ccode(RawMatrixOrNull, 
	       "(engine_RawMatrixOrNull)IM2_Matrix_make2(",
	       "(FreeModule*)", target, ",",
	       "(FreeModule*)", source, ",",
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.2), ",", -- deg
	       "(RingElement_array *)", getSequenceOfRingElements(s.3), ",", -- entries
	       mutable == True, ")"))
     else WrongArgBoolean(5)
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(5));
setupfun("rawMatrix2",rawMatrix2);

rawMatrixRemake(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) != 5 then WrongNumArgs(5) else
     when s.0 is target:RawFreeModule do 
     when s.1 is source:RawFreeModule do
     if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else
     when s.3 is M:RawMatrix do
     when s.4 is mutable:Boolean do toExpr(Ccode(RawMatrixOrNull, 
	       "(engine_RawMatrixOrNull)IM2_Matrix_remake(",
	       "(FreeModule*)", target, ",",
	       "(FreeModule*)", source, ",",
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.2), ",", -- deg
	       "(Matrix*)", M, ",",
	       mutable == True, ")"))
     else WrongArgBoolean(5)
     else WrongArg(4,"a raw matrix")
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(5));
setupfun("rawMatrixRemake",rawMatrixRemake);

rawSparseMatrix1(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) != 6 then WrongNumArgs(6) else
     when s.0 is target:RawFreeModule do 
     when s.1 is ncols:Integer do if !isInt(ncols) then WrongArgSmallInteger(2) else 
     if isSequenceOfSmallIntegers(s.2) then
     if isSequenceOfSmallIntegers(s.3) then
     if isSequenceOfRingElements(s.4) then 
     when s.5 is mutable:Boolean do 
     toExpr(Ccode(RawMatrixOrNull, 
	       "(engine_RawMatrixOrNull)IM2_Matrix_make_sparse1(",
	       "(FreeModule*)", target, ",",
	       toInt(ncols), ",",
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.2), ",", -- rows
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.3), ",", -- cols
	       "(RingElement_array *)", getSequenceOfRingElements(s.4), ",", -- entries
	       mutable == True, ")"))
     else WrongArgBoolean(6)
     else WrongArg(5,"a sequence of ring elements")
     else WrongArg(4,"a sequence of small integers")
     else WrongArg(3,"a sequence of small integers")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(6));
setupfun("rawSparseMatrix1",rawSparseMatrix1);

rawSparseMatrix2(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) != 7 then WrongNumArgs(7) else
     when s.0 is target:RawFreeModule do 
     when s.1 is source:RawFreeModule do
     if isSequenceOfSmallIntegers(s.2) then
     if isSequenceOfSmallIntegers(s.3) then
     if isSequenceOfSmallIntegers(s.4) then
     if isSequenceOfRingElements(s.5) then 
     when s.6 is mutable:Boolean do 
     toExpr(Ccode(RawMatrixOrNull, 
	       "(engine_RawMatrixOrNull)IM2_Matrix_make_sparse2(",
	       "(FreeModule*)", target, ",",
	       "(FreeModule*)", source, ",",
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.2), ",", -- deg
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.3), ",", -- rows
	       "(M2_arrayint)", getSequenceOfSmallIntegers(s.4), ",", -- cols
	       "(RingElement_array *)", getSequenceOfRingElements(s.5), ",", -- entries
	       mutable == True, ")"))
     else WrongArgBoolean(7)
     else WrongArg(6,"a sequence of ring elements")
     else WrongArg(5,"a sequence of small integers")
     else WrongArg(4,"a sequence of small integers")
     else WrongArg(3,"a sequence of small integers")
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(7));
setupfun("rawSparseMatrix2",rawSparseMatrix2);

rawConcat(e:Expr):Expr := (
     if isSequenceOfMatrices(e) then
     toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
	       "IM2_Matrix_concat(", 
	       "(Matrix_array *)", getSequenceOfMatrices(e),
	       ")"))
     else WrongArg("a raw matrix or a sequence of raw matrices")
     );
setupfun("rawConcat",rawConcat);

rawMatrixEntry(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 4 then 
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is r:Integer do if !isInt(r) then WrongArgSmallInteger(2) else
     when s.2 is c:Integer do if !isInt(c) then WrongArgSmallInteger(3) else 
     when s.3 is x:RawRingElement do (
	  if Ccode(bool, 
	       "IM2_MutableMatrix_set_entry(", "(Matrix *)", M, ",", 
	       toInt(r), ",", toInt(c), ",", "(RingElement *)", x, ")" 
	       )
	  then nullE
	  else buildErrorPacket(EngineError("error setting raw matrix entry")))
     else WrongArg(4,"an raw ring element")
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw (mutable) matrix")
     else if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do
     when s.1 is r:Integer do 
     if !isInt(r) then WrongArgSmallInteger(2) else
     when s.2 is c:Integer do 
     if !isInt(c) then WrongArgSmallInteger(3) else (
	  Expr(Ccode(RawRingElement,
		    "(engine_RawRingElement)",
		    "IM2_Matrix_get_entry(", "(Matrix *)", M, ",", toInt(r), ",", toInt(c), ")" ) ) )
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3,4)
     );
setupfun("rawMatrixEntry",rawMatrixEntry);

rawSortColumns(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do
     when s.1 is deg_order:Integer do 
     if !isInt(deg_order) then WrongArgSmallInteger(2) else
     when s.2 is mon_order:Integer do 
     if !isInt(mon_order) then WrongArgSmallInteger(3) else (
	  toExpr(Ccode(RawArrayInt, "(engine_RawArrayInt)",
		    "IM2_Matrix_sort_columns(",
		    "(Matrix *)", M, ",",
		    toInt(deg_order), ",",
		    toInt(mon_order),
		    ")"
		    )
	       )
	  )
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3)
     );
setupfun("rawSortColumns",rawSortColumns);

rawEliminateVariables(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is nparts:Integer do if !isInt(nparts) then WrongArgSmallInteger(1) else 
     when s.1 is M:RawMatrix do (
	  toExpr(Ccode(RawArrayInt, "(engine_RawArrayInt)",
		    "IM2_Matrix_elim_vars(",
		    toInt(nparts), ",",
		    "(Matrix *)", M,
		    ")" ) ) )
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     );
setupfun("rawEliminateVariables",rawEliminateVariables);

rawKeepVariables(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is nparts:Integer do if !isInt(nparts) then WrongArgSmallInteger(1) else 
     when s.1 is M:RawMatrix do (
	  toExpr(Ccode(RawArrayInt, "(engine_RawArrayInt)",
		    "IM2_Matrix_keep_vars(",
		    toInt(nparts), ",",
		    "(Matrix *)", M,
		    ")" ) ) )
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     );
setupfun("rawKeepVariables",rawKeepVariables);

-- rawRemoveContent(e:Expr):Expr := (
--      when e is M:RawMatrix do (
-- 	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
-- 		    "IM2_Matrix_remove_content(",
-- 		    "(Matrix *)", M,
-- 		    ")" ) ) )
--      else WrongArg("a raw matrix"));
-- setupfun("rawRemoveContent",rawRemoveContent);

rawDivideByVariable(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do
     when s.1 is var:Integer do 
     if !isInt(var) then WrongArgSmallInteger(2) else
     when s.2 is maxdegree:Integer do 
     if !isInt(maxdegree) then WrongArgSmallInteger(3) else (
	  toExpr(Ccode(RawMatrixAndInt, "(engine_RawMatrixAndInt)",
		    "IM2_Matrix_divide_by_var(",
		    "(Matrix *)", M, ",",
		    toInt(var), ",",
		    toInt(maxdegree),
		    ")" ) ) )
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3)
     );
setupfun("rawDivideByVariable",rawDivideByVariable);

rawMinors(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is p:Integer do if !isInt(p) then WrongArgSmallInteger(1) else
     when s.1 is M:RawMatrix do
     when s.2 is strategy:Integer do 
     if !isInt(strategy) then WrongArgSmallInteger(3) else (
	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
		    "IM2_Matrix_minors(",
		    toInt(p), ",",
		    "(Matrix *)", M, ",",
		    toInt(strategy),
		    ")"
		    )
	       )
	  )
     else WrongArgInteger(3)
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(3)
     );
setupfun("rawMinors",rawMinors);

rawInitial(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is p:Integer do if !isInt(p) then WrongArgSmallInteger(1) else
     when s.1 is M:RawMatrix do (
	  Expr(Ccode(RawMatrix, "(engine_RawMatrix)",
		    "IM2_Matrix_initial(",
		    toInt(p), ",",
		    "(Matrix *)", M,
		    ")"
		    )
	       )
	  )
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     );
setupfun("rawInitial",rawInitial);

rawPfaffians(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 is p:Integer do if !isInt(p) then WrongArgSmallInteger(1) else
     when s.1 is M:RawMatrix do (
	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
		    "IM2_Matrix_pfaffians(", toInt(p), ",", "(Matrix *)", M, ")" ) ) )
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     );
setupfun("rawPfaffians",rawPfaffians);

rawTensor(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0
     is f:RawMatrix do
     when s.1 is g:RawMatrix do (
	  toExpr(Ccode(RawMatrixOrNull,"(engine_RawMatrixOrNull)",
		    "IM2_Matrix_tensor(",
		    "(Matrix *)", f, ",",
		    "(Matrix *)", g,
		    ")"
		    )
	       )		    
	  )
     else WrongArg(2,"a raw matrix")
     is M:RawFreeModule do
     when s.1 is N:RawFreeModule do (
	  toExpr(Ccode(RawFreeModuleOrNull,"(engine_RawFreeModuleOrNull)",
		    "IM2_FreeModule_tensor(",
		    "(FreeModule *)", M, ",",
		    "(FreeModule *)", N,
		    ")"
		    )
	       )		    
	  )
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw matrix or free module")
     else WrongNumArgs(2)     
     );
setupfun("rawTensor",rawTensor);

rawMatrixDiff(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is f:RawMatrix do
     when s.1 is g:RawMatrix do (
	  toExpr(Ccode(RawMatrixOrNull,"(engine_RawMatrixOrNull)",
		    "IM2_Matrix_diff(",
		    "(Matrix *)", f, ",",
		    "(Matrix *)", g,
		    ")"
		    )
	       )		    
	  )
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(2)     
     );
setupfun("rawMatrixDiff",rawMatrixDiff);

rawMatrixContract(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is f:RawMatrix do
     when s.1 is g:RawMatrix do (
	  toExpr(Ccode(RawMatrixOrNull,"(engine_RawMatrixOrNull)",
		    "IM2_Matrix_contract(",
		    "(Matrix *)", f, ",",
		    "(Matrix *)", g,
		    ")"
		    )
	       )		    
	  )
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(2)     
     );
setupfun("rawMatrixContract",rawMatrixContract);

rawIdentity(e:Expr):Expr := (
     when e
     is F:RawFreeModule do Expr(Ccode(RawMatrix, "(engine_RawMatrix)",
	       "IM2_Matrix_identity(", "(FreeModule *)", F, ")" ))
     else WrongArg("a raw free module"));
setupfun("rawIdentity",rawIdentity);

rawMonomials(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     if !isSequenceOfSmallIntegers(s.0) then WrongArg(1,"a sequence of small integers") else 
     when s.1 is M:RawMatrix do (
	  vars := getSequenceOfSmallIntegers(s.0);
	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
		    "IM2_Matrix_monomials(",
		    "(M2_arrayint)", vars, ",",
		    "(Matrix *)", M,
		    ")" ))
	  )
     else WrongArg(2,"a raw matrix")
     else WrongNumArgs(2));
setupfun("rawMonomials",rawMonomials);

rawCoefficients(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     if !isSequenceOfSmallIntegers(s.0) then WrongArg(1,"a sequence of small integers") else
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else
     when s.2 is M:RawMatrix do (
	  vars := getSequenceOfSmallIntegers(s.0);
	  monoms := getSequenceOfSmallIntegers(s.1);
	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
		    "IM2_Matrix_get_coeffs(",
		    "(M2_arrayint)", vars, ",",
		    "(M2_arrayint)", monoms, ",",
		    "(Matrix *)", M,
		    ")" ))
	  )
     else WrongArg(3,"a raw matrix")
     else WrongNumArgs(3));
setupfun("rawCoefficients",rawCoefficients);

rawSubmatrix(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 3 then
     when s.0 is M:RawMatrix do 
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else
     if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else (
	  rows := getSequenceOfSmallIntegers(s.1);
	  cols := getSequenceOfSmallIntegers(s.2);
	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
		    "IM2_Matrix_submatrix(",
		    "(Matrix *)", M, ",",
		    "(M2_arrayint)", rows, ",",
		    "(M2_arrayint)", cols,
		    ")" ) ) )
     else WrongArg(1,"a raw matrix")
     else if length(s) == 2 then
     when s.0 is M:RawMatrix do 
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else (
	  cols := getSequenceOfSmallIntegers(s.1);
	  toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
		    "IM2_Matrix_submatrix1(",
		    "(Matrix *)", M, ",",
		    "(M2_arrayint)", cols,
		    ")" ) ) )
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(2,3)
     else WrongNumArgs(2,3));
setupfun("rawSubmatrix",rawSubmatrix);

rawReshape(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) == 3 then 
     when s.0 is M:RawMatrix do 
     when s.1 is F:RawFreeModule do
     when s.2 is G:RawFreeModule do toExpr(Ccode(RawMatrixOrNull,
	       "(engine_RawMatrixOrNull)IM2_Matrix_reshape(",
	       "(Matrix *)", M, ",", "(FreeModule *)", F, ",", "(FreeModule *)", G, ")" ))
     else WrongArg(3,"a raw free module")
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawReshape",rawReshape);

rawFlip(e:Expr):Expr := (
     when e is s:Sequence do 
     if length(s) == 2 then 
     when s.0 is F:RawFreeModule do
     when s.1 is G:RawFreeModule do toExpr(Ccode(RawMatrixOrNull,
	       "(engine_RawMatrixOrNull)IM2_Matrix_flip(",
	       "(FreeModule *)", F, ",", "(FreeModule *)", G, ")" ))
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw free module")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawFlip",rawFlip);

rawKoszul(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is n:Integer do
     if !isInt(n) then WrongArgSmallInteger(1) else
     when s.1
     is F:RawMatrix do toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
	       "IM2_Matrix_koszul(", toInt(n), ",", "(Matrix *)", F, ")" ))
     else WrongArg(2,"a raw matrix")
     else WrongArgInteger(1)
     else WrongNumArgs(2));
setupfun("rawKoszul",rawKoszul);

rawKoszulMonomials(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is F:RawMatrix do 
     when s.1 is G:RawMatrix do 
     toExpr(Ccode(RawMatrixOrNull, "(engine_RawMatrixOrNull)",
	       "IM2_Matrix_koszul_monoms(", "(Matrix *)", F, ",", "(Matrix *)", G, ")" ))
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(2));
setupfun("rawKoszulMonomials",rawKoszulMonomials);


rawHilbert(e:Expr):Expr := (
     when e is M:RawMatrix do (
	  toExpr(Ccode(RawRingElementOrNull,"(engine_RawRingElementOrNull)",
		    "IM2_Matrix_Hilbert(",
		    "(Matrix *)", M, ")" ) ) )
     else WrongArg("a raw matrix")
     );
setupfun("rawHilbert",rawHilbert);


-----------------------------------------------------------------------------
-- monomial ideals

rawMonomialIdeal(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is m:RawMatrix do
     when s.1 is n:Integer do 
     if !isInt(n) then WrongArgSmallInteger(2) else 
     toExpr(Ccode(RawMonomialIdealOrNull, "(engine_RawMonomialIdealOrNull)",
	       "IM2_MonomialIdeal_make(", "(Matrix *)", m, ",", toInt(n), ")" ) )
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(2)
     );
setupfun("rawMonomialIdeal",rawMonomialIdeal);

rawNumgens(e:Expr):Expr := (
     when e
     is I:RawMonomialIdeal do Expr( toInteger(
	       Ccode(int, "IM2_MonomialIdeal_n_gens(", "(MonomialIdeal *)", I, ")" )))
     else WrongArg("a raw free module"));
setupfun("rawNumgens",rawNumgens);

rawIntersect(e:Expr):Expr := (
     when e is s:Sequence do
     when s.0 is I:RawMonomialIdeal do
     when s.1 is J:RawMonomialIdeal do
     toExpr(Ccode(RawMonomialIdealOrNull,"(engine_RawMonomialIdealOrNull)",
	       "IM2_MonomialIdeal_intersect(",
	       "(MonomialIdeal *)", I, ",",
	       "(MonomialIdeal *)", J,
	       ")"
	       )
	  )
     else WrongArg(2,"a raw monomial ideal")
     else WrongArg(1,"a raw monomial ideal")
     else WrongNumArgs(2)     
     );
setupfun("rawIntersect",rawIntersect);

rawStronglyStableClosure(e:Expr):Expr := (
     when e is I:RawMonomialIdeal do Expr(
	  Ccode(RawMonomialIdeal,"(engine_RawMonomialIdeal)",
	       "IM2_MonomialIdeal_borel(", "(MonomialIdeal *)", I, ")" ) )
     else WrongArg("a raw monomial ideal"));
setupfun("rawStronglyStableClosure",rawStronglyStableClosure);

rawIsStronglyStable(e:Expr):Expr := (
     when e is I:RawMonomialIdeal do toExpr(
	  Ccode(bool, "IM2_MonomialIdeal_is_borel(", "(MonomialIdeal *)", I, ")" ) )
     else WrongArg("a raw monomial ideal"));
setupfun("rawIsStronglyStable",rawIsStronglyStable);

rawCodimension(e:Expr):Expr := (
     when e is I:RawMonomialIdeal do toExpr(
	  Ccode(int, "IM2_MonomialIdeal_codim(", "(MonomialIdeal *)", I, ")" ) )
     else WrongArg("a raw monomial ideal"));
setupfun("rawCodimension",rawCodimension);

rawAssociatedPrimes(e:Expr):Expr := (
     when e is I:RawMonomialIdeal do Expr(
	  Ccode(RawMonomialIdeal,"(engine_RawMonomialIdeal)",
	       "IM2_MonomialIdeal_assprimes(", "(MonomialIdeal *)", I, ")" ) )
     else WrongArg("a raw monomial ideal"));
setupfun("rawAssociatedPrimes",rawAssociatedPrimes);

-----------------------------------------------------------------------------
-- ring maps

rawRingMap(e:Expr):Expr := (
     when e
     is M:RawMatrix do Expr( Ccode( RawRingMap, "IM2_RingMap_make1(", "(Matrix*)",M, ")" ))
     else WrongArg("a raw matrix")
     );
setupfun("rawRingMap",rawRingMap);

rawRingMapEval(e:Expr):Expr := (
     when e
     is s:Sequence do
     if length(s) == 2 then 
     when s.0 is F:RawRingMap do 
     when s.1 is a:RawRingElement do (
	  toExpr(Ccode(RawRingElementOrNull,
		    "(engine_RawRingElementOrNull)",
		    "IM2_RingMap_eval_ringelem(",
		    "(RingMap *)", F, ",",
		    "(RingElement *)", a,
		    ")"
		    )
	       )
	  )
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw ring map")
     else if length(s) == 3 then
     when s.0 is F:RawRingMap do 
     when s.1 is newTarget:RawFreeModule do
     when s.2
     is M:RawMatrix do (
	  toExpr(Ccode(RawMatrixOrNull,
		    "(engine_RawMatrixOrNull)",
		    "IM2_RingMap_eval_matrix(",
		    "(RingMap *)", F, ",",
		    "(FreeModule *)", newTarget, ",",
		    "(Matrix *)", M,
		    ")"
		    )
	       )
	  )
     else WrongArg(3,"a matrix")
     else WrongArg(2,"a raw free module")
     else WrongArg(1,"a raw ring map")
     else buildErrorPacket("expected 2 or 3 arguments")
     else buildErrorPacket("expected 2 or 3 arguments")
     );
setupfun("rawRingMapEval",rawRingMapEval);

rawNumberOfRows(e:Expr):Expr := (
     when e
     is M:RawMatrix do toExpr(Ccode( int, "IM2_Matrix_n_rows(", "(Matrix *)", M, ")" ))
     else WrongArg("a raw matrix"));
setupfun("rawNumberOfRows",rawNumberOfRows);

rawNumberOfColumns(e:Expr):Expr := (
     when e
     is M:RawMatrix do toExpr(Ccode( int, "IM2_Matrix_n_cols(", "(Matrix *)", M, ")" ))
     else WrongArg("a raw matrix"));
setupfun("rawNumberOfColumns",rawNumberOfColumns);

rawRowChange(e:Expr):Expr := (
     when e
     is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     Expr(Ccode( RawMatrix, "(engine_RawMatrix)", "IM2_MutableMatrix_get_row_change(", "(Matrix *)", M, ")" ))
     is s:Sequence do
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is rowChange:RawMatrix do ( Ccode( void, "IM2_MutableMatrix_set_row_change(", "(Matrix *)", M, ",", "(Matrix *)", rowChange, ")" ); nullE)
     else WrongArg("a raw matrix")
     else WrongArg("a raw matrix")
     else WrongNumArgs(1,2));
setupfun("rawRowChange",rawRowChange);

rawColumnChange(e:Expr):Expr := (
     when e
     is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     Expr(Ccode( RawMatrix, "(engine_RawMatrix)", "IM2_MutableMatrix_get_col_change(", "(Matrix *)", M, ")" ))
     is s:Sequence do
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is colChange:RawMatrix do ( Ccode( void, "IM2_MutableMatrix_set_col_change(", "(Matrix *)", M, ",", "(Matrix *)", colChange, ")" ); nullE)
     else WrongArg("a raw matrix")
     else WrongArg("a raw matrix")
     else WrongNumArgs(1,2));
setupfun("rawColumnChange",rawColumnChange);

rawMatrixRowSwap(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is i:Integer do if !isInt(i) then WrongArgSmallInteger(2) else
     when s.2 is j:Integer do if !isInt(j) then WrongArgSmallInteger(3) else ( Ccode(void, "IM2_MutableMatrix_row_swap(", "(Matrix *)", M, ",", toInt(i), ",", toInt(j), ")" ); nullE)
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3));
setupfun("rawMatrixRowSwap",rawMatrixRowSwap);

rawMatrixColumnSwap(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is i:Integer do if !isInt(i) then WrongArgSmallInteger(2) else
     when s.2 is j:Integer do if !isInt(j) then WrongArgSmallInteger(3) else ( Ccode(void, "IM2_MutableMatrix_column_swap(", "(Matrix *)", M, ",", toInt(i), ",", toInt(j), ")" ); nullE)
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3));
setupfun("rawMatrixColumnSwap",rawMatrixColumnSwap);

rawMatrixRowChange(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 4 then WrongNumArgs(4) else
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is targetRow:Integer do if !isInt(targetRow) then WrongArgSmallInteger(2) else
     when s.2 is r:RawRingElement do
     when s.3 is sourceRow:Integer do if !isInt(sourceRow) then WrongArgSmallInteger(4) else (
	  if Ccode(bool, "IM2_MutableMatrix_row_operation(", "(Matrix *)", M, ",", toInt(targetRow), ",", "(RingElement *)", r, ",", toInt(sourceRow), ")" )
	  then nullE
	  else buildErrorPacket(EngineError("error changing raw matrix row")))
     else WrongArgInteger(4)
     else WrongArg(3,"a raw ring element")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(4));
setupfun("rawMatrixRowChange",rawMatrixRowChange);

rawMatrixColumnChange(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 4 then WrongNumArgs(4) else
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is targetColumn:Integer do if !isInt(targetColumn) then WrongArgSmallInteger(2) else
     when s.2 is r:RawRingElement do
     when s.3 is sourceColumn:Integer do if !isInt(sourceColumn) then WrongArgSmallInteger(4) else (
	  if Ccode(bool, "IM2_MutableMatrix_column_operation(", "(Matrix *)", M, ",", toInt(targetColumn), ",", "(RingElement *)", r, ",", toInt(sourceColumn), ")" )
	  then nullE
	  else buildErrorPacket(EngineError("error changing raw matrix column")))
     else WrongArgInteger(4)
     else WrongArg(3,"a raw ring element")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(4));
setupfun("rawMatrixColumnChange",rawMatrixColumnChange);

rawMatrixRowScale(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is targetRow:Integer do if !isInt(targetRow) then WrongArgSmallInteger(2) else
     when s.2 is r:RawRingElement do (
	  if Ccode(bool, "IM2_MutableMatrix_row_scale(", "(Matrix *)", M, ",", toInt(targetRow), ",", "(RingElement *)", r, ")" )
	  then nullE
	  else buildErrorPacket(EngineError("error scaling raw matrix row")))
     else WrongArg(3,"a raw ring element")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3));
setupfun("rawMatrixRowScale",rawMatrixRowScale);

rawMatrixColumnScale(e:Expr):Expr := (
     when e is s:Sequence do if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is M:RawMatrix do if !isMutable(M) then WrongArg("a mutable raw matrix") else
     when s.1 is targetColumn:Integer do if !isInt(targetColumn) then WrongArgSmallInteger(2) else
     when s.2 is r:RawRingElement do (
	  if Ccode(bool, "IM2_MutableMatrix_column_scale(", "(Matrix *)", M, ",", toInt(targetColumn), ",", "(RingElement *)", r, ")" )
	  then nullE
	  else buildErrorPacket(EngineError("error scaling raw matrix column")))
     else WrongArg(3,"a raw ring element")
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(3));
setupfun("rawMatrixColumnScale",rawMatrixColumnScale);

-----------------------------------------------------------------------------
-- Groebner bases and resolutions
-----------------------------------------------------------------------------

rawGB(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 7 then WrongNumArgs(7) else
     when s.0 is m:RawMatrix do
     when s.1 is collectSyz:Boolean do
     when s.2 is nRowsToKeep:Integer do if !isInt(nRowsToKeep) then WrongArgSmallInteger(3) else
     when s.3 is useMaxDegree:Boolean do
     when s.4 is maxDegree:Integer do if !isInt(maxDegree) then WrongArgSmallInteger(5) else
     when s.5 is algorithm:Integer do if !isInt(algorithm) then WrongArgSmallInteger(6) else
     when s.6 is strategy:Integer do if !isInt(strategy) then WrongArgSmallInteger(7) else
     if !isInt(algorithm) then WrongArgSmallInteger(8) else
     toExpr(
	  Ccode(RawComputationOrNull,
	       "(engine_RawComputationOrNull)IM2_GB_make(",
		   "(Matrix*)",m,",",
		   isTrue(collectSyz),",",
		   toInt(nRowsToKeep),",",
		   isTrue(useMaxDegree),",",
		   toInt(maxDegree),",",
		   toInt(algorithm),",",
		   toInt(strategy),
	       ")"
	       )
	  )
     else WrongArgInteger(7)
     else WrongArgInteger(6)
     else WrongArgInteger(5)
     else WrongArgBoolean(4)
     else WrongArgInteger(3)
     else WrongArgBoolean(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(7)
     );
setupfun("rawGB",rawGB);

rawResolution(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 7 then WrongNumArgs(7) else
     when s.0 is m:RawMatrix do
     when s.1 is resolveCokernel:Boolean do
     when s.2 is maxLevel:Integer do if !isInt(maxLevel) then WrongArgSmallInteger(3) else
     when s.3 is useMaxSlantedDegree:Boolean do
     when s.4 is maxSlantedDegree:Integer do if !isInt(maxSlantedDegree) then WrongArgSmallInteger(5) else
     when s.5 is algorithm:Integer do if !isInt(algorithm) then WrongArgSmallInteger(6) else
     when s.6 is strategy:Integer do if !isInt(strategy) then WrongArgSmallInteger(7) else
     if !isInt(algorithm) then WrongArgSmallInteger(8) else
     toExpr(
	  Ccode(RawComputationOrNull,
	       "(engine_RawComputationOrNull)IM2_res_make(",
		   "(Matrix*)",m,",",
		   isTrue(resolveCokernel),",",
		   toInt(maxLevel),",",
		   isTrue(useMaxSlantedDegree),",",
		   toInt(maxSlantedDegree),",",
		   toInt(algorithm),",",
		   toInt(strategy),
	       ")"
	       )
	  )
     else WrongArgInteger(7)
     else WrongArgInteger(6)
     else WrongArgInteger(5)
     else WrongArgBoolean(4)
     else WrongArgInteger(3)
     else WrongArgBoolean(2)
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(7)
     );
setupfun("rawResolution",rawResolution);

rawGBSetHilbertFunction(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is G:RawComputation do 
     when a.1 is h:RawRingElement do toExpr(
	  Ccode(RawComputationOrNull, 
		    "(engine_RawComputationOrNull)IM2_GB_set_hilbert_function(",
		    "(Computation *)", G,
		    ",(RingElement *)", h,
		    ")" ))
     else WrongArg(2,"a raw ring element")
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawGBSetHilbertFunction", rawGBSetHilbertFunction);

rawGBForce(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is m:RawComputation do 
     when a.1 is gb:RawMatrix do 
     when a.2 is change:RawMatrix do toExpr(
	  Ccode(RawComputationOrNull, 
		    "(engine_RawComputationOrNull)IM2_GB_force(",
		    "(Matrix *)", m,
		    ",(Matrix *)", gb,
		    ",(Matrix *)", change,
		    ")" ))
     else WrongArg(3,"a raw matrix")
     else WrongArg(2,"a raw matrix")
     else WrongArg(1,"a raw matrix")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawGBForce", rawGBForce);

rawGBSetStop(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 11 then WrongNumArgs(11) else
     when s.0 is G:RawComputation do
     when s.1 is always_stop:Boolean do
     when s.2 is stop_after_degree:Boolean do
     if !isSequenceOfSmallIntegers(s.3) then WrongArg(4,"a sequence of small integers") else -- degree_limit
     when s.4 is basis_element_limit:Integer do
     if !isInt(basis_element_limit) then WrongArgSmallInteger(5) else
     when s.5 is syzygy_limit:Integer do
     if !isInt(syzygy_limit) then WrongArgSmallInteger(6) else
     when s.6 is pair_limit:Integer do
     if !isInt(pair_limit) then WrongArgSmallInteger(7) else
     when s.7 is codim_limit:Integer do
     if !isInt(codim_limit) then WrongArgSmallInteger(8) else
     when s.8 is subring_limit:Integer do
     if !isInt(subring_limit) then WrongArgSmallInteger(9) else
     when s.9 is just_min_gens:Boolean do
     if !isSequenceOfSmallIntegers(s.10) then WrongArg(11,"a sequence of small integers") else -- length_limit
     toExpr(
	  Ccode(RawComputationOrNull,"(engine_RawComputationOrNull)IM2_GB_set_stop(",
		    "(Computation *)", G, ",",
		    True == always_stop, ",",
		    True == stop_after_degree, ",",
		    "(M2_arrayint)", getSequenceOfSmallIntegers(s.3), ",",
		    toInt(basis_element_limit), ",",
		    toInt(syzygy_limit), ",",
		    toInt(pair_limit), ",",
		    toInt(codim_limit), ",",
		    toInt(subring_limit), ",",
		    True == just_min_gens, ",",
		    "(M2_arrayint)", getSequenceOfSmallIntegers(s.10),
	       ")"
	       )
	  )
     else WrongArgBoolean(10)
     else WrongArgInteger(9)
     else WrongArgInteger(8)
     else WrongArgInteger(7)
     else WrongArgInteger(6)
     else WrongArgInteger(5)
     else WrongArgBoolean(3)
     else WrongArgBoolean(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(11)
     );
setupfun("rawGBSetStop", rawGBSetStop);

rawGBGetMatrix(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is G:RawComputation do 
     when a.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     when a.2 is minimize:Boolean do toExpr(
	  Ccode(RawMatrixOrNull, 
		    "(engine_RawMatrixOrNull)IM2_GB_get_matrix(",
		    "(Computation *)", G, ",",
		    toInt(level), ",",
		    True == minimize,
		    ")" ))
     else WrongArgBoolean(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGBGetMatrix", rawGBGetMatrix);

rawGBStatus(e:Expr):Expr := (
     when e is G:RawComputation do (
	  completionDegree := 0;
	  completionLevel := 0;
	  ret := Ccode(int,"IM2_GB_status(",
	       "(Computation*)",G,",",
	       "&",completionDegree,",",
	       "&",completionLevel,
	       ")"
	       );
	  if ret == -1 then buildErrorPacket(EngineError("unknown raw computation status error")) 
	  else Expr(Sequence(toInteger(ret),toInteger(completionDegree),toInteger(completionLevel)))
	  )
     else WrongArg("a raw computation")
     );
setupfun("rawGBStatus", rawGBStatus);

rawGBStatusLevel(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 is G:RawComputation do
     when s.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     when s.2 is minimize:Boolean do (
	  completionDegree := 0;
	  ret := Ccode(int,"IM2_GB_status_level(",
	       "(Computation*)",G,",",
	       toInt(level), ",",
	       True == minimize, ",",
	       "&",completionDegree,
	       ")"
	       );
	  if ret == -1 then buildErrorPacket(EngineError("unknown raw computation status/level error")) 
	  else Expr(Sequence(toInteger(ret),toInteger(completionDegree)))
	  )
     else WrongArgBoolean(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     );
setupfun("rawGBStatusLevel", rawGBStatusLevel);

rawGBGetChange(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is G:RawComputation do 
     when a.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     toExpr(
	  Ccode(RawMatrixOrNull, 
		    "(engine_RawMatrixOrNull)IM2_GB_get_change(",
		    "(Computation *)", G, ",",
		    toInt(level),
		    ")" ))
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawGBGetChange", rawGBGetChange);

rawGBGetLeadTerms(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is G:RawComputation do 
     when a.1 is nparts:Integer do
     if !isInt(nparts) then WrongArgSmallInteger(2) else
     when a.2 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(3) else
     toExpr(
	  Ccode(RawMatrixOrNull, 
		    "(engine_RawMatrixOrNull)IM2_GB_get_leadterms(",
		    "(Computation *)", G, ",",
		    toInt(nparts), ",",
		    toInt(level),
		    ")" ))
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGBGetLeadTerms", rawGBGetLeadTerms);

rawGBGetFree(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is G:RawComputation do 
     when a.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     when a.2 is minimal:Boolean do toExpr(
	  Ccode(RawFreeModuleOrNull, 
		    "(engine_RawFreeModuleOrNull)IM2_GB_get_free(",
		    "(Computation *)", G, ",",
		    toInt(level), ",",
		    True == minimal,
		    ")" ))
     else WrongArgBoolean(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGBGetFree", rawGBGetFree);

rawGBMatrixRemainder(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is G:RawComputation do 
     when a.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     when a.2 is m:RawMatrix do toExpr(
	  Ccode(RawMatrixOrNull, 
		    "(engine_RawMatrixOrNull)IM2_GB_matrix_remainder(",
		    "(Computation *)", G, ",",
		    toInt(level),",",
		    "(Matrix*)", m,		    
		    ")" ))
     else WrongArgBoolean(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGBMatrixRemainder", rawGBMatrixRemainder);

toList(a:RawMatrixOrNull,b:RawMatrixOrNull):Expr := (
     when a
     is null do buildErrorPacket(EngineError("unknown raw matrix lift engine error"))
     is A:RawMatrix do
     when b
     is null do buildErrorPacket(EngineError("unknown raw matrix lift engine error"))
     is B:RawMatrix do
     list(Expr(A),Expr(B)));     

rawGBMatrixLift(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is G:RawComputation do 
     when a.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     when a.2 is m:RawMatrix do (
	  resultRemainder := RawMatrixOrNull(NULL);
	  resultQuotient := RawMatrixOrNull(NULL);
	  Ccode(void, "IM2_GB_matrix_lift(",
	       "(Computation *)", G, ",",
	       toInt(level),",",
	       "(Matrix*)", m, ",",
	       "(Matrix**)&", resultRemainder, ",",
	       "(Matrix**)&", resultQuotient,
     	       -- I'm ignoring these messages for now:
	       --  ../../../Macaulay2/d/interface.d:2391: warning: dereferencing type-punned pointer will break strict-aliasing rules
	       --  ../../../Macaulay2/d/interface.d:2391: warning: dereferencing type-punned pointer will break strict-aliasing rules
	       --  similar error messages come from "gcc -O3 -Wall" on this:
	       --  struct A;
	       --  struct B;
	       --  extern void h();
	       --  void f(struct B *p) { h((struct A**)&p); }
	       ")" );
	  toList(resultQuotient,resultRemainder))
     else WrongArgBoolean(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGBMatrixLift", rawGBMatrixLift);

rawGBContains(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 3 then 
     when a.0 is G:RawComputation do 
     when a.1 is level:Integer do
     if !isInt(level) then WrongArgSmallInteger(2) else
     when a.2 is m:RawMatrix do toExpr(
	  Ccode(int, 
		    "IM2_GB_contains(",
		    "(Computation *)", G, ",",
		    toInt(level),",",
		    "(Matrix*)", m,		    
		    ")" ))
     else WrongArgBoolean(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawGBContains", rawGBContains);

rawGBBetti(e:Expr):Expr := (
     when e is a:Sequence do 
     if length(a) == 2 then 
     when a.0 is G:RawComputation do 
     when a.1 is type:Integer do
     if !isInt(type) then WrongArgSmallInteger(2) else
     toExpr(
	  Ccode(RawArrayInt, 
		    "(engine_RawArrayInt)IM2_GB_betti(",
		    "(Computation *)", G, ",",
		    toInt(type),
		    ")" ))
     else WrongArgInteger(2)
     else WrongArg(1,"a raw computation")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawGBBetti", rawGBBetti);

rawGBverbose(e:Expr):Expr := (
     when e is level:Integer do
     if !isInt(level) then WrongArgSmallInteger() 
     else toExpr(Ccode(int, "IM2_GB_verbose(", toInt(level), ")"))
     else WrongArgInteger());
setupfun("rawGBverbose", rawGBverbose);

-----------------------------------------------------------------------------
-- LAPACK 
-----------------------------------------------------------------------------

M2CC(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 2 then
     when s.0 is re:Real do 
     when s.1 is im:Real do (
	  Expr(
	       Ccode(Complex, "(M2_CC *)LP_make_M2_Complex(", re.v, ",", 
		                     	         	      im.v, ")"))
	  )
     else WrongArg(2, "a double")
     else WrongArg(1, "a double")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("M2CC", M2CC);

rawMatrixRR(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 2 then
     when s.0 is nrows:Integer do if !isInt(nrows) then WrongArgSmallInteger(1) else
     when s.1 is ncols:Integer do if !isInt(ncols) then WrongArgSmallInteger(2) else (
	  Expr(
	       Ccode(LMatrixRR, "(LMatrixRR *)LP_LMatrixRR_make(", toInt(nrows), ",", 
		                                                   toInt(ncols), ")"))
	  )
     else WrongArgInteger(2)
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawMatrixRR", rawMatrixRR);

rawMatrixCC(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 2 then
     when s.0 is nrows:Integer do if !isInt(nrows) then WrongArgSmallInteger(1) else
     when s.1 is ncols:Integer do if !isInt(ncols) then WrongArgSmallInteger(2) else (
	  Expr(
	       Ccode(LMatrixCC, "(LMatrixCC *)LP_LMatrixCC_make(", toInt(nrows), ",", 
		                                                   toInt(ncols), ")"))
	  )
     else WrongArgInteger(2)
     else WrongArgInteger(1)
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawMatrixCC", rawMatrixCC);

rawGetEpsilonMatrixRR(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 0
     then Expr(Ccode(Real,"LP_LMatrixRR_get_epsilon()"))
     else WrongNumArgs(0)
     else WrongNumArgs(0)
     );
setupfun("rawGetEpsilonMatrixRR", rawGetEpsilonMatrixRR);

rawGetEpsilonMatrixCC(e:Expr):Expr := (
     when e is s:Sequence do if length(s) == 0
     then Expr(Ccode(Real,"LP_LMatrixCC_get_epsilon()"))
     else WrongNumArgs(0)
     else WrongNumArgs(0)
     );
setupfun("rawGetEpsilonMatrixCC", rawGetEpsilonMatrixCC);

rawSetEpsilonMatrixRR(e:Expr):Expr := (
     when e is eps:Real do (
     	Ccode(void,"LP_LMatrixRR_set_epsilon(", eps.v, ")");
	nullE
	)
     else WrongArg("a real")
     );
setupfun("rawSetEpsilonMatrixRR", rawSetEpsilonMatrixRR);

rawSetEpsilonMatrixCC(e:Expr):Expr := (
     when e is eps:Real do (
     	Ccode(void,"LP_LMatrixCC_set_epsilon(", eps.v, ")");
	nullE
	)
     else WrongArg("a real")
     );
setupfun("rawSetEpsilonMatrixCC", rawSetEpsilonMatrixCC);

rawNumberOfRowsRR(e:Expr):Expr := (
     when e is M:LMatrixRR do
	  toExpr(
	       Ccode(int, "LP_LMatrixRR_nrows(", "(LMatrixRR *)", M, ")") 
	  )
     else WrongArg(1, "a raw matrixRR"));
setupfun("rawNumberOfRowsRR", rawNumberOfRowsRR);

rawNumberOfColumnsRR(e:Expr):Expr := (
     when e is M:LMatrixRR do
	  toExpr(
	       Ccode(int, "LP_LMatrixRR_ncols(", "(LMatrixRR *)", M, ")") 
	  )
     else WrongArg(1, "a raw matrixRR"));
setupfun("rawNumberOfColumnsRR", rawNumberOfColumnsRR);

rawNumberOfRowsCC(e:Expr):Expr := (
     when e is M:LMatrixCC do
	  toExpr(
	       Ccode(int, "LP_LMatrixCC_nrows(", "(LMatrixCC *)", M, ")") 
	  )
     else WrongArg(1, "a raw matrixCC"));
setupfun("rawNumberOfRowsCC", rawNumberOfRowsCC);

rawNumberOfColumnsCC(e:Expr):Expr := (
     when e is M:LMatrixCC do
	  toExpr(
	       Ccode(int, "LP_LMatrixCC_ncols(", "(LMatrixCC *)", M, ")") 
	  )
     else WrongArg(1, "a raw matrixCC"));
setupfun("rawNumberOfColumnsCC", rawNumberOfColumnsCC);

rawSetMatrixEntryRR(e:Expr):Expr := (
     -- rawSetEntry(M, r, c, double)
     when e is s:Sequence do
     if length(s) == 4 then 
     when s.0 is M:LMatrixRR do
     when s.1 is r:Integer do if !isInt(r) then WrongArgSmallInteger(2) else
     when s.2 is c:Integer do if !isInt(c) then WrongArgSmallInteger(3) else 
     when s.3 is x:Real do (
	  Ccode(void, 
	       "LP_LMatrixRR_set_entry(", "(LMatrixRR *)", M, ",", 
	       toInt(r), ",", toInt(c), ",", x.v, ")" 
	       );
	  nullE
          )
     else WrongArg(4,"a double")
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrixRR")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("rawSetMatrixEntryRR", rawSetMatrixEntryRR);

rawSetMatrixEntryCC(e:Expr):Expr := (
     -- rawSetEntry(M, r, c, Complex)
     when e is s:Sequence do
     if length(s) == 4 then 
     when s.0 is M:LMatrixCC do
     when s.1 is r:Integer do if !isInt(r) then WrongArgSmallInteger(2) else
     when s.2 is c:Integer do if !isInt(c) then WrongArgSmallInteger(3) else 
     when s.3 is x:Complex do (
	  Ccode(void, 
	       "LP_LMatrixCC_set_entry(", "(LMatrixCC *)", M, ",", 
	       toInt(r), ",", toInt(c), ",", "(M2_CC)", x,")" 
	       );
	  nullE
          )
     else WrongArg(4,"a complex")
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrixCC")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("rawSetMatrixEntryCC", rawSetMatrixEntryCC);

rawGetMatrixEntryRR(e:Expr):Expr := (
     -- rawGetEntry(M, r, c, double)
     when e is s:Sequence do
     if length(s) == 4 then 
     when s.0 is M:LMatrixRR do
     when s.1 is r:Integer do if !isInt(r) then WrongArgSmallInteger(2) else
     when s.2 is c:Integer do if !isInt(c) then WrongArgSmallInteger(3) else 
     when s.3 is x:Real do (
	  Ccode(void, 
	       "LP_LMatrixRR_get_entry(", "(LMatrixRR *)", M, ",", 
	       toInt(r), ",", toInt(c), ",", "(double *)", x, ")" 
	       );
	  nullE
          )
     else WrongArg(4,"a double")
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrixRR")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("rawGetMatrixEntryRR", rawGetMatrixEntryRR);

rawGetMatrixEntryCC(e:Expr):Expr := (
     -- rawGetEntry(M, r, c, complex)
     when e is s:Sequence do
     if length(s) == 4 then 
     when s.0 is M:LMatrixCC do
     when s.1 is r:Integer do if !isInt(r) then WrongArgSmallInteger(2) else
     when s.2 is c:Integer do if !isInt(c) then WrongArgSmallInteger(3) else 
     when s.3 is x:Complex do (
	  Ccode(void, 
	       "LP_LMatrixCC_get_entry(", "(LMatrixCC *)", M, ",", 
	       toInt(r), ",", toInt(c), ",", "(M2_CC)", x,")" 
	       );
	  nullE
          )
     else WrongArg(4,"a complex")
     else WrongArgInteger(3)
     else WrongArgInteger(2)
     else WrongArg(1,"a raw matrixCC")
     else WrongNumArgs(4)
     else WrongNumArgs(4));
setupfun("rawGetMatrixEntryCC", rawGetMatrixEntryCC);

rawSetMatrixValues(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 3 then
     when s.1 is l:List do (
     when isSequenceOfPairsOfSmallIntegers(l.v) is t:string do return WrongArg(t) else nothing;
     	when s.0 
     	is M:LMatrixRR do (
		if isListOfReals(s.2) then (
			Ccode(void,
			"LP_LMatrixRR_set_values(",
			"(LMatrixRR *)", M, ",",
			"(M2_arrayint)", getSequenceOfPairsOfSmallIntegers(l.v), ",",
			"(M2_double_array *)", getListOfReals(s.2), ")"
			);
			nullE
			)
		else WrongArg(3, "a list of reals")
		)
	is M:LMatrixCC do (
		if isListOfComplex(s.2) then (
			Ccode(void,
			"LP_LMatrixCC_set_values(",
			"(LMatrixCC *)", M, ",",
			"(M2_arrayint)", getSequenceOfPairsOfSmallIntegers(l.v), ",",
			"(M2_complex_array *)", getListOfComplex(s.2), ")"
			);
			nullE
			)
		else WrongArg(3, "a list of complexes")
		)
	else WrongArg(1, "a matrixRR or matrixCC")
	)
     else WrongArg(2, "a list of pairs of integers")
     else WrongNumArgs(3)
     else WrongNumArgs(3)
     );
setupfun("rawSetMatrixValues",rawSetMatrixValues);

rawGetSubmatrix(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) == 3 then
     when s.0
     is M:LMatrixRR do (
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else
     if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else (
	  rows := getSequenceOfSmallIntegers(s.1);
	  cols := getSequenceOfSmallIntegers(s.2);
	  toExpr(Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)",
		    "LP_LMatrixRR_get_submatrix(",
		    "(LMatrixRR *)", M, ",",
		    "(M2_arrayint)", rows, ",",
		    "(M2_arrayint)", cols,
		    ")" ) ) )
	)
     is M:LMatrixCC do (
     if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else
     if !isSequenceOfSmallIntegers(s.2) then WrongArg(3,"a sequence of small integers") else (
	  rows := getSequenceOfSmallIntegers(s.1);
	  cols := getSequenceOfSmallIntegers(s.2);
	  toExpr(Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)",
		    "LP_LMatrixCC_get_submatrix(",
		    "(LMatrixCC *)", M, ",",
		    "(M2_arrayint)", rows, ",",
		    "(M2_arrayint)", cols,
		    ")" ) ) )
	)
     else WrongArg(1,"a raw matrixRR or raw matrixCC")
     else WrongNumArgs(3)
     else WrongNumArgs(3)
     );
setupfun("rawGetSubmatrix",rawGetSubmatrix);

rawSolve(e:Expr):Expr := (
     -- rawSolve(A, b, x)
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0
     is A:LMatrixRR do (
	when s.1 is b:LMatrixRR do
	when s.2 is x:LMatrixRR do (
	  toExpr(
	  Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_solve(", "(LMatrixRR *)", A, ",", 
		       "(LMatrixRR *)", b, ",", "(LMatrixRR *)", x, ")"))
	  )
	else WrongArg(3,"a raw matrixRR")
	else WrongArg(2,"a raw matrixRR")
	)
      is A:LMatrixCC do (
	when s.1 is b:LMatrixCC do
	when s.2 is x:LMatrixCC do (
	  toExpr(
	  Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_solve(", "(LMatrixCC *)", A, ",", 
		       "(LMatrixCC *)", b, ",", "(LMatrixCC *)", x, ")"))
	  )
	else WrongArg(3,"a raw matrixCC")
	else WrongArg(2,"a raw matrixCC")
	)
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(3));
setupfun("rawSolve", rawSolve);

rawLU(e:Expr):Expr := (
     -- rawLU(M, L, U, P)
     when e is s:Sequence do
     if length(s) != 4 then WrongNumArgs(4) else
     when s.0
     is M:LMatrixRR do (
	when s.1 is L:LMatrixRR do
	when s.2 is U:LMatrixRR do
	when s.3 is P:LMatrixRR do (
	  toExpr(
	  Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_LU(", "(LMatrixRR *)", M, ",", 
			"(LMatrixRR *)", L, ",", "(LMatrixRR *)", U, ",",
			"(LMatrixRR *)", P, ")"))
	  )
	else WrongArg(4,"a raw matrixRR")
	else WrongArg(3,"a raw matrixRR")
	else WrongArg(2,"a raw matrixRR")
	)
      is M:LMatrixCC do (
	when s.1 is L:LMatrixCC do
	when s.2 is U:LMatrixCC do
	when s.3 is P:LMatrixRR do (
	  toExpr(
	  Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_LU(", "(LMatrixCC *)", M, ",", 
			"(LMatrixCC *)", L, ",", "(LMatrixCC *)", U, ",",
			"(LMatrixRR *)", P, ")"))
	  )
	else WrongArg(4,"a raw matrixRR")
	else WrongArg(3,"a raw matrixCC")
	else WrongArg(2,"a raw matrixCC")
	)
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(4));
setupfun("rawLU", rawLU);

rawEigenvalues(e:Expr):Expr := (
     -- rawEigenvalues(M, eigs)
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2) else
     when s.0 
     is M:LMatrixRR do (
        when s.1 is eigs:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixRR_eigenvalues(", "(LMatrixRR *)", M, ",", 
		            	"(LMatrixCC *)", eigs, ")"))
	)
        else WrongArg(2, "a raw matrixCC"))
     is N:LMatrixCC do (
        when s.1 is eigs:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_eigenvalues(", "(LMatrixCC *)", N, ",", 
		            	"(LMatrixCC *)", eigs, ")"))
	)
        else WrongArg(2, "a raw matrixCC"))
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(2));
setupfun("rawEigenvalues", rawEigenvalues);

rawEigenvectors(e:Expr):Expr := (
     -- rawEigenvalues(M, eigvals, eigvecs)
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 
     is M:LMatrixRR do (
     	when s.1 is eigvals:LMatrixCC do 
     	when s.2 is eigvecs:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixRR_eigenvectors(", "(LMatrixRR *)", M, ",", 
		       "(LMatrixCC *)", eigvals, ",", "(LMatrixCC *)", eigvecs, ")"))
	)
	else WrongArg(3,"a raw matrixCC")
	else WrongArg(2,"a raw matrixCC"))
     is N:LMatrixCC do (
     	when s.1 is eigvals:LMatrixCC do 
     	when s.2 is eigvecs:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_eigenvectors(", "(LMatrixCC *)", N, ",", 
		       "(LMatrixCC *)", eigvals, ",", "(LMatrixCC *)", eigvecs, ")"))
	)
	else WrongArg(3,"a raw matrixCC")
	else WrongArg(2,"a raw matrixCC"))
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(3));
setupfun("rawEigenvectors", rawEigenvectors);

rawEigenvaluesSymmetric(e:Expr):Expr := (
     -- rawEigenvaluesSymmetric(M, eigs)
     when e is s:Sequence do
     if length(s) == 2 then
     when s.0 
     is M:LMatrixRR do (
     	when s.1 is eigs:LMatrixRR do (
	  toExpr(
	  Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_eigenvalues_symmetric(", "(LMatrixRR *)", 
		M, ",", "(LMatrixRR *)", eigs, ")"))
	  )
       else WrongArg(2, "a raw matrixRR"))
     else WrongArg(1, "a raw matrixRR")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawEigenvaluesSymmetric", rawEigenvaluesSymmetric);

rawEigenvectorsSymmetric(e:Expr):Expr := (
     -- rawEigenvectorsSymmetric(M, eigvals, eigvecs)
     when e is s:Sequence do
     if length(s) == 3 then
     when s.0 
     is M:LMatrixRR do (
     	when s.1 is eigvals:LMatrixRR do 
     	when s.2 is eigvecs:LMatrixRR do (
	  toExpr(
	  Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_eigenvectors_symmetric(", "(LMatrixRR *)", 
		M, ",", "(LMatrixRR *)", eigvals, ",", "(LMatrixRR *)", eigvecs, ")"))
	  )
       	else WrongArg(3, "a raw matrixRR")
       	else WrongArg(2, "a raw matrixRR"))
     else WrongArg(1,"a raw matrixRR")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawEigenvectorsSymmetric", rawEigenvectorsSymmetric);

rawEigenvaluesHermitian(e:Expr):Expr := (
     -- rawEigenvaluesHermitian(M, eigs)
     when e is s:Sequence do
     if length(s) == 2 then
     when s.0 is M:LMatrixCC do
     when s.1 is eigs:LMatrixRR do (
	toExpr(
	Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixCC_eigenvalues_hermitian(", "(LMatrixCC *)", 
		M, ",", "(LMatrixRR *)", eigs, ")"))
	)
     else WrongArg(2, "a raw matrixRR")
     else WrongArg(1, "a raw matrixCC")
     else WrongNumArgs(2)
     else WrongNumArgs(2));
setupfun("rawEigenvaluesHermitian", rawEigenvaluesHermitian);

rawEigenvectorsHermitian(e:Expr):Expr := (
     -- rawEigenvectorsHermitian(M, eigvals, eigvecs)
     when e is s:Sequence do
     if length(s) == 3 then
     when s.0 is M:LMatrixCC do
     when s.1 is eigvals:LMatrixRR do 
     when s.2 is eigvecs:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_eigenvectors_hermitian(", "(LMatrixCC *)", 
		M, ",", "(LMatrixRR *)", eigvals, ",", "(LMatrixCC *)", eigvecs, ")"))
	)
     else WrongArg(3, "a raw matrixCC")
     else WrongArg(2, "a raw matrixRR")
     else WrongArg(1, "a raw matrixCC")
     else WrongNumArgs(3)
     else WrongNumArgs(3));
setupfun("rawEigenvectorsHermitian", rawEigenvectorsHermitian);

rawSVD(e:Expr):Expr := (
     -- rawSVD(M, Sigma, U, VT)
     when e is s:Sequence do
     if length(s) != 4 then WrongNumArgs(4) else
     when s.0 
     is M:LMatrixRR do (
	when s.1 is Sigma:LMatrixRR do
     	when s.2 is U:LMatrixRR do
     	when s.3 is VT:LMatrixRR do
	toExpr(
	Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_SVD(", "(LMatrixRR *)", M, ",", 
		       "(LMatrixRR *)", Sigma, ",", "(LMatrixRR *)", U, ",",
		       "(LMatrixRR *)", VT, ")"))
	else WrongArg(4, "a raw matrixRR")
	else WrongArg(3, "a raw matrixRR")
	else WrongArg(2, "a raw matrixRR")
	)
     is M:LMatrixCC do (
	when s.1 is Sigma:LMatrixRR do
     	when s.2 is U:LMatrixCC do
     	when s.3 is VT:LMatrixCC do
	toExpr(
	Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixCC_SVD(", "(LMatrixCC *)", M, ",", 
		       "(LMatrixRR *)", Sigma, ",", "(LMatrixCC *)", U, ",",
		       "(LMatrixCC *)", VT, ")"))
	else WrongArg(4, "a raw matrixCC")
	else WrongArg(3, "a raw matrixCC")
	else WrongArg(2, "a raw matrixRR")
	)
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(4));
setupfun("rawSVD", rawSVD);

rawLeastSquares(e:Expr):Expr := (
     -- rawLeastSquares(M, b, x)
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 
     is M:LMatrixRR do (
     	when s.1 is b:LMatrixRR do 
     	when s.2 is x:LMatrixRR do (
	toExpr(
	Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_least_squares(", "(LMatrixRR *)", M, ",", 
		       "(LMatrixRR *)", b, ",", "(LMatrixRR *)", x, ")"))
	)
	else WrongArg(3,"a raw matrixRR")
	else WrongArg(2,"a raw matrixRR"))
     is M:LMatrixCC do (
     	when s.1 is b:LMatrixCC do 
     	when s.2 is x:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_least_squares(", "(LMatrixCC *)", M, ",", 
		       "(LMatrixCC *)", b, ",", "(LMatrixCC *)", x, ")"))
	)
	else WrongArg(3,"a raw matrixCC")
	else WrongArg(2,"a raw matrixCC"))
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(3));
setupfun("rawLeastSquares", rawLeastSquares);

rawLeastSquaresDeficient(e:Expr):Expr := (
     -- rawLeastSquaresDeficient(M, b, x)
     when e is s:Sequence do
     if length(s) != 3 then WrongNumArgs(3) else
     when s.0 
     is M:LMatrixRR do (
     	when s.1 is b:LMatrixRR do 
     	when s.2 is x:LMatrixRR do (
	toExpr(
	Ccode(LMatrixRROrNull, "(engine_LMatrixRROrNull)LP_LMatrixRR_least_squares_deficient(", 
			"(LMatrixRR *)", M, ",", 
		        "(LMatrixRR *)", b, ",", "(LMatrixRR *)", x, ")"))
	)
	else WrongArg(3,"a raw matrixRR")
	else WrongArg(2,"a raw matrixRR"))
     is M:LMatrixCC do (
     	when s.1 is b:LMatrixCC do 
     	when s.2 is x:LMatrixCC do (
	toExpr(
	Ccode(LMatrixCCOrNull, "(engine_LMatrixCCOrNull)LP_LMatrixCC_least_squares_deficient(", 
			"(LMatrixCC *)", M, ",", 
		        "(LMatrixCC *)", b, ",", "(LMatrixCC *)", x, ")"))
	)
	else WrongArg(3,"a raw matrixCC")
	else WrongArg(2,"a raw matrixCC"))
     else WrongArg(1, "a raw matrixRR or raw matrixCC")
     else WrongNumArgs(3));
setupfun("rawLeastSquaresDeficient", rawLeastSquaresDeficient);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/d"
-- End:
