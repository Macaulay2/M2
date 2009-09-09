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

export rawTrackPaths(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 13 then WrongNumArgs(13)
     else when s.0 is slp1:RawStraightLineProgram do 
     	  when s.1 is slp2:RawStraightLineProgram do
	  when s.2 is startSols:RawMatrix do
	  when s.3 is isProj:Boolean do
	  when s.4 is initDt:RR do
	  when s.5 is minDt:RR do
	  when s.6 is maxDt:RR do
	  when s.7 is dtIncreaseFactor:RR do 
	  when s.8 is dtDecreeaseFactor:RR do
	  when s.9 is numSuccessesBeforeIncrease:ZZ do
	  when s.10 is epsilon:RR do 
	  when s.11 is maxCorrSteps:ZZ do
	  when s.12 is predType:ZZ do
	       toExpr(Ccode(RawMatrixOrNull,
		    	 "(engine_RawMatrixOrNull)rawTrackPaths(",
		    	 "(StraightLineProgram *)", slp1, ",",
		    	 "(StraightLineProgram *)", slp2, ",",
		    	 "(Matrix *)", startSols,",",
			 toBoolean(s.3),",",
			 "(M2_RRR)", initDt,",",
			 "(M2_RRR)", minDt,",",
			 "(M2_RRR)", maxDt,",",
			 "(M2_RRR)", dtIncreaseFactor,",",
			 "(M2_RRR)", dtDecreeaseFactor,",",
			 toInt(s.9),",",
			 "(M2_RRR)", epsilon,",",
			 toInt(s.11),",",
			 toInt(s.12),
		    	 ")"
		    	 ))
	  else WrongArgZZ(13)
	  else WrongArgRR(12)
	  else WrongArgRR(11)
	  else WrongArgZZ(10)
	  else WrongArgRR(9)
	  else WrongArgRR(8)
	  else WrongArgRR(7)
	  else WrongArgRR(6)
	  else WrongArgRR(5)
	  else WrongArgBoolean(4)
	  else WrongArgMatrix(3)
     	  else WrongArg(2,"a raw straight line program")
     	  else WrongArg(1,"a raw straight line program")
     else WrongNumArgs(13)
     );
setupfun("rawTrackPaths",rawTrackPaths);

export rawPathTracker(e:Expr):Expr := (
     when e is HH:RawMatrix do 
		toExpr(Ccode(RawPathTrackerOrNull,
		    "(engine_RawPathTrackerOrNull)rawPathTracker(",
		    "(Matrix *)", HH, 
		    ")"
		    ))
     else WrongArgMatrix()
     );
setupfun("rawPathTracker",rawPathTracker);

export rawSetParametersPT(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 11 then WrongNumArgs(11)
     else when s.0 is PT:RawPathTracker do 
	  when s.1 is isProj:Boolean do
	  when s.2 is initDt:RR do
	  when s.3 is minDt:RR do
	  when s.4 is maxDt:RR do
	  when s.5 is dtIncreaseFactor:RR do 
	  when s.6 is dtDecreeaseFactor:RR do
	  when s.7 is numSuccessesBeforeIncrease:ZZ do
	  when s.8 is epsilon:RR do 
	  when s.9 is maxCorrSteps:ZZ do
	  when s.10 is predType:ZZ do (
	       Ccode(void,
		    	 "rawSetParametersPT(",
		    	 "(PathTracker *)", PT, ",",
			 toBoolean(s.1),",",
			 "(M2_RRR)", initDt,",",
			 "(M2_RRR)", minDt,",",
			 "(M2_RRR)", maxDt,",",
			 "(M2_RRR)", dtIncreaseFactor,",",
			 "(M2_RRR)", dtDecreeaseFactor,",",
			 toInt(s.7),",",
			 "(M2_RRR)", epsilon,",",
			 toInt(s.9),",",
			 toInt(s.10),
		    	 ")"
		    	 );
	       nullE)
	  else WrongArgZZ(11)
	  else WrongArgRR(10)
	  else WrongArgRR(9)
	  else WrongArgZZ(8)
	  else WrongArgRR(7)
	  else WrongArgRR(6)
	  else WrongArgRR(5)
	  else WrongArgRR(4)
	  else WrongArgRR(3)
	  else WrongArgBoolean(2)
     	  else WrongArg(1,"a path tracker")
     else WrongNumArgs(11)
     );
setupfun("rawSetParametersPT",rawSetParametersPT);

export rawLaunchPT(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2)
     else when s.0 is PT:RawPathTracker do 
	  when s.1 is startSols:RawMatrix do (
	       Ccode(void,
		    	 "rawLaunchPT(",
		    	 "(PathTracker *)", PT, ",",
	                 "(Matrix *)", startSols, 
		    	 ")"
	       );
	       nullE)
	  else WrongArgMatrix(2)
     	  else WrongArg(1,"a path tracker")
     else WrongNumArgs(2)
     );
setupfun("rawLaunchPT",rawLaunchPT);

export rawGetSolutionPT(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2)
     else when s.0 is PT:RawPathTracker do 
          when s.1 is solN:ZZ do 
		toExpr(Ccode(RawMatrixOrNull,
		    "(engine_RawMatrixOrNull)rawGetSolutionPT(",
		    "(PathTracker *)", PT, ",",
		    toInt(s.1), 		    
		    ")"
		    ))
          else WrongArgZZ(2) 
          else WrongArg(1,"a path tracker")
     else WrongNumArgs(2)
     );
setupfun("rawGetSolutionPT",rawGetSolutionPT);

export rawGetAllSolutionsPT(e:Expr):Expr := (
     when e is PT:RawPathTracker  do 
		toExpr(Ccode(RawMatrixOrNull,
		    "(engine_RawMatrixOrNull)rawGetAllSolutionsPT(",
		    "(PathTracker *)", PT, 
		    ")"
		    ))
     else WrongArg("a path tracker")
     );
setupfun("rawGetAllSolutionsPT",rawGetAllSolutionsPT);

export rawGetSolutionStatusPT(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2)
     else when s.0 is PT:RawPathTracker do 
          when s.1 is solN:ZZ do 
		toExpr(Ccode(int,
		    "rawGetSolutionStatusPT(",
		    "(PathTracker *)", PT, ",",
		    toInt(s.1), 		    
		    ")"
		    ))
          else WrongArgZZ(2) 
          else WrongArg(1,"a path tracker")
     else WrongNumArgs(2)
     );
setupfun("rawGetSolutionStatusPT",rawGetSolutionStatusPT);

export rawGetSolutionLastTvaluePT(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 2 then WrongNumArgs(2)
     else when s.0 is PT:RawPathTracker do 
          when s.1 is solN:ZZ do 
		toExpr(Ccode(RRorNull, "(engine_RRorNull)rawGetSolutionLastTvaluePT(",
		    "(PathTracker *)", PT, ",",
		    toInt(s.1), 		    
		    ")"
		    ))
          else WrongArgZZ(2) 
          else WrongArg(1,"a path tracker")
     else WrongNumArgs(2)
     );
setupfun("rawGetSolutionLastTvaluePT",rawGetSolutionLastTvaluePT);

export rawRefinePT(e:Expr):Expr := (
     when e is s:Sequence do
     if length(s) != 4 then WrongNumArgs(4)
     else when s.0 is PT:RawPathTracker do 
	  when s.1 is sols:RawMatrix do 
	  when s.2 is tolerance:RR do
          when s.3 is maxSteps:ZZ do 
	       toExpr(Ccode(RawMatrixOrNull,
		    "(engine_RawMatrixOrNull)rawRefinePT(",
		    	 "(PathTracker *)", PT, ",",
	                 "(Matrix *)", sols, ",",
			 "(M2_RRR)", tolerance,",",
                         toInt(s.3),
		    	 ")"
	       ))
          else WrongArgZZ(4)
          else WrongArgRR(3)
	  else WrongArgMatrix(2)
     	  else WrongArg(1,"a path tracker")
     else WrongNumArgs(4)
     );
setupfun("rawRefinePT",rawRefinePT);

