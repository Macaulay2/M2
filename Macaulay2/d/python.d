-- Copyright 2009 by Daniel R. Grayson
use C;
use gmp;
use strings;
use tokens;
use common;

import RunSimpleString(s:string):void;
PyRun(e:Expr):Expr := (
     when e is s:string do (
	  RunSimpleString(s);
	  nullE)
     else WrongArgString());
setupfun("runSimpleString",PyRun);

import Main():int;
PyMain(e:Expr):Expr := (
     -- ignore e for now
     -- later, e can be a list of strings to serve as argv
     Expr(toInteger(Main())));
setupfun("pythonMain",PyMain);