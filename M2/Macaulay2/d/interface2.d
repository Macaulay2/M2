--		Copyright 2008, 2009 by Daniel R. Grayson

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


-- straight line programs

export rawSLP(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2)
     else when s.0 is M:RawMatrix do (
	  if !isSequenceOfSmallIntegers(s.1) then WrongArg(2,"a sequence of small integers") else
	  toExpr(Ccode(RawStraightLineProgramOrNull,
		    "(engine_RawStraightLineProgramOrNull)rawSLP(",
		    "(Matrix *)", M, ",",
		    "(M2_arrayint)", getSequenceOfSmallIntegers(s.1),
		    ")"
		    )))
     else WrongArgMatrix(1)
     else WrongNumArgs(2));
setupfun("rawSLP",rawSLP);

export rawEvaluateSLP(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2)
     else when s.0 is slp:RawStraightLineProgram do (
	  when s.1 is M:RawMatrix do (
	       toExpr(Ccode(RawMatrixOrNull,
		    	 "(engine_RawMatrixOrNull)rawEvaluateSLP(",
		    	 "(StraightLineProgram *)", slp, ",",
		    	 "(Matrix *)", M,
		    	 ")"
		    	 )))
	  else WrongArgMatrix(1))
     else WrongArg(2,"a raw straight line program")
     else WrongNumArgs(2)
     );
setupfun("rawEvaluateSLP",rawEvaluateSLP);

