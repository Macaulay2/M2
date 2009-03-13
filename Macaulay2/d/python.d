-- Copyright 2009 by Daniel R. Grayson
use C;
use objects;
use gmp;
use strings;
use tokens;
use common;

pythonObjectOrNull := pythonObject or null;
toExpr(r:pythonObjectOrNull):Expr := when r is null do buildErrorPacket("python error") is po:pythonObject do Expr(po);

import RunSimpleString(s:string):int;
PyRunSimpleString(e:Expr):Expr := (
     when e is s:string do if 0 == RunSimpleString(s) then nullE else buildErrorPacket("python error")
     else WrongArgString());
setupfun("runSimpleString",PyRunSimpleString);

import RunString(s:string):pythonObjectOrNull;
PyRunString(e:Expr):Expr := when e is s:string do toExpr(RunString(s)) else WrongArgString();
setupfun("runString",PyRunString);

import Main():int;
PyMain(e:Expr):Expr := toExpr(Main());
setupfun("pythonMain",PyMain);

import SysGetObject(s:string):pythonObjectOrNull;
PySysGetObject(e:Expr):Expr := when e is s:string do toExpr(SysGetObject(s)) else WrongArgString();
setupfun("sysGetObject",PySysGetObject);
