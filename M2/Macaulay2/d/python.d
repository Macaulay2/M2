-- Copyright 2009,2010 by Daniel R. Grayson
use common;
use util;

pythonObjectOrNull := pythonObject or null;
toExpr(r:pythonObjectOrNull):Expr := when r is null do buildErrorPacket("python error") is po:pythonObject do Expr(pythonObjectCell(po));

import RunSimpleString(s:string):int;
PyRunSimpleString(e:Expr):Expr := (
     when e is s:stringCell do if 0 == RunSimpleString(s.v) then nullE else buildErrorPacket("python error")
     else WrongArgString());
setupfun("runSimpleString",PyRunSimpleString);

import RunString(s:string):pythonObjectOrNull;
PyRunString(e:Expr):Expr := when e is s:stringCell do toExpr(RunString(s.v)) else WrongArgString();
setupfun("runPythonString",PyRunString);

import Main():int;
PyMain(e:Expr):Expr := toExpr(Main());
setupfun("pythonMain",PyMain);

import SysGetObject(s:string):pythonObjectOrNull;
PySysGetObject(e:Expr):Expr := when e is s:stringCell do toExpr(SysGetObject(s.v)) else WrongArgString();
setupfun("sysGetObject",PySysGetObject);

import ObjectType(o:pythonObject):pythonObjectOrNull;
PyObjectType(e:Expr):Expr := when e is o:pythonObjectCell do toExpr(ObjectType(o.v)) else WrongArg("a python object");
setupfun("objectType",PyObjectType);

import initspam():void;
runinitspam(e:Expr):Expr := (initspam(); nullE);
setupfun("initspam",runinitspam);

import NumberAdd(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberAdd(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberAdd(x.v, y.v))
	else WrongArg(2, "a python object")
    else WrongArg(1, "a python object");
PyNumberAdd(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberAdd(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberAdd",PyNumberAdd);

import NumberSubtract(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberSubtract(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberSubtract(x.v, y.v))
	else WrongArg(2, "a python object")
    else WrongArg(1, "a python object");
PyNumberSubtract(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberSubtract(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberSubtract",PyNumberSubtract);

import NumberMultiply(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberMultiply(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberMultiply(x.v, y.v))
	else WrongArg(2, "a python object")
    else WrongArg(1, "a python object");
PyNumberMultiply(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberMultiply(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberMultiply",PyNumberMultiply);

import NumberTrueDivide(o1:pythonObject,o2:pythonObject):pythonObjectOrNull;
PyNumberTrueDivide(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:pythonObjectCell do toExpr(NumberTrueDivide(x.v, y.v))
	else WrongArg(2, "a python object")
    else WrongArg(1, "a python object");
PyNumberTrueDivide(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyNumberTrueDivide(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonNumberTrueDivide",PyNumberTrueDivide);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python.o "
-- End:
