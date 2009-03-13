-- Copyright 2009 by Daniel R. Grayson
use C;
use objects;
use gmp;
use strings;
use tokens;
use common;

import RunSimpleString(s:string):int;
PyRun(e:Expr):Expr := (
     when e is s:string do if 0 == RunSimpleString(s) then nullE else buildErrorPacket("python error")
     else WrongArgString());
setupfun("runSimpleString",PyRun);

import Main():int;
PyMain(e:Expr):Expr := (
     -- ignore e for now
     -- later, e can be a list of strings to serve as argv
     toExpr(Main()));
setupfun("pythonMain",PyMain);