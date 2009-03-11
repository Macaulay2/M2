-- Copyright 2009 by Daniel R. Grayson
use C;
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
