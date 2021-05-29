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

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python.o "
-- End:
