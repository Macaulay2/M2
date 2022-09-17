-- Copyright 2009,2010 by Daniel R. Grayson
use common;
use util;

WrongArgPythonObject():Expr := WrongArg("a python object");
WrongArgPythonObject(n:int):Expr := WrongArg(n,"a python object");

import ErrOccurred():int;
import ErrPrint():void;

buildPythonErrorPacket():Expr :=
    if ErrOccurred() == 1 then (
	ErrPrint();
	buildErrorPacket("python error")
    ) else nullE;

pythonObjectOrNull := pythonObject or null;
toExpr(r:pythonObjectOrNull):Expr := when r is null do buildPythonErrorPacket() is po:pythonObject do Expr(pythonObjectCell(po));

import RunSimpleString(s:string):int;
PyRunSimpleString(e:Expr):Expr := (
     when e is s:stringCell do if 0 == RunSimpleString(s.v) then nullE else buildPythonErrorPacket()
     else WrongArgString());
setupfun("runSimpleString",PyRunSimpleString);

import RunString(s:string):pythonObjectOrNull;
PyRunString(e:Expr):Expr := when e is s:stringCell do toExpr(RunString(s.v)) else WrongArgString();
setupfun("pythonRunString",PyRunString);

import Main():int;
PyMain(e:Expr):Expr := toExpr(Main());
setupfun("pythonMain",PyMain);

-------------
-- objects --
-------------

import ObjectType(o:pythonObject):pythonObjectOrNull;
PyObjectType(e:Expr):Expr := (
    when e
    is o:pythonObjectCell do toExpr(ObjectType(o.v))
    is s:SpecialExpr do PyObjectType(s.e)
    else WrongArgPythonObject());
setupfun("objectType",PyObjectType);

import ObjectRichCompareBool(o1:pythonObject,o2:pythonObject,opid:int):int;
PyObjectRichCompareBool(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:ZZcell do (
		r := ObjectRichCompareBool(x.v, y.v, toInt(z));
		if r == -1 then buildPythonErrorPacket()
		else toExpr(r == 1))
	    else WrongArgZZ(3)
	is s:SpecialExpr do PyObjectRichCompareBool(e1, s.e, e3)
	else WrongArgPythonObject(2)
    is s:SpecialExpr do PyObjectRichCompareBool(s.e, e2, e3)
    else WrongArgPythonObject(1);
PyObjectRichCompareBool(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyObjectRichCompareBool(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonObjectRichCompareBool",PyObjectRichCompareBool);

import ObjectHasAttrString(o:pythonObject,attr:charstar):int;
PyObjectHasAttrString(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:stringCell do
	    toExpr(ObjectHasAttrString(x.v, tocharstar(y.v)) == 1)
	else WrongArgString(2)
    is s:SpecialExpr do PyObjectHasAttrString(s.e, rhs)
    else WrongArgPythonObject(1);
PyObjectHasAttrString(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyObjectHasAttrString(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonObjectHasAttrString",PyObjectHasAttrString);

import ObjectGetAttrString(o:pythonObject,attr:charstar):pythonObjectOrNull;
PyObjectGetAttrString(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:stringCell do toExpr(ObjectGetAttrString(x.v, tocharstar(y.v)))
	else WrongArgString(2)
    is s:SpecialExpr do PyObjectGetAttrString(s.e, rhs)
    else WrongArgPythonObject(1);
PyObjectGetAttrString(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyObjectGetAttrString(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonObjectGetAttrString",PyObjectGetAttrString);

import ObjectSetAttrString(o:pythonObject,attr:charstar,v:pythonObject):int;
PyObjectSetAttrString(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:stringCell do
	    when e3
	    is z:pythonObjectCell do (
		if ObjectSetAttrString(x.v, tocharstar(y.v), z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    is s:SpecialExpr do PyObjectSetAttrString(e1, e2, s.e)
	    else WrongArgPythonObject(3)
	else WrongArgString(2)
    is s:SpecialExpr do PyObjectSetAttrString(s.e, e2, e3)
    else WrongArgPythonObject(1);
PyObjectSetAttrString(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyObjectSetAttrString(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonObjectSetAttrString",PyObjectSetAttrString);

import ObjectStr(o:pythonObject):pythonObjectOrNull;
PyObjectStr(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(ObjectStr(x.v))
    is s:SpecialExpr do PyObjectStr(s.e)
    else WrongArgPythonObject();
setupfun("pythonObjectStr",PyObjectStr);

import initspam():void;
runinitspam(e:Expr):Expr := (initspam(); nullE);
setupfun("initspam",runinitspam);

----------
-- bools -
----------

import True:pythonObjectOrNull;
setupconst("pythonTrue", toExpr(True));

import False:pythonObjectOrNull;
setupconst("pythonFalse", toExpr(False));

----------
-- ints --
----------

import LongAsLong(o:pythonObject):long;
PyLongAsLong(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do (
	y := LongAsLong(x.v);
	if ErrOccurred() == 1 then buildPythonErrorPacket()
	else toExpr(y))
    is s:SpecialExpr do PyLongAsLong(s.e)
    else WrongArgPythonObject();
setupfun("pythonLongAsLong",PyLongAsLong);

import LongFromLong(v:long):pythonObjectOrNull;
PyLongFromLong(e:Expr):Expr :=
    when e
    is x:ZZcell do toExpr(LongFromLong(toLong(x)))
    else WrongArgPythonObject();
setupfun("pythonLongFromLong",PyLongFromLong);

------------
-- floats --
------------

import FloatAsDouble(o:pythonObject):double;
PyFloatAsDouble(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do (
	y := FloatAsDouble(x.v);
	if ErrOccurred() == 1 then buildPythonErrorPacket()
	else toExpr(y))
    is s:SpecialExpr do PyFloatAsDouble(s.e)
    else WrongArgPythonObject();
setupfun("pythonFloatAsDouble",PyFloatAsDouble);

import FloatFromDouble(v:double):pythonObjectOrNull;
PyFloatFromDouble(e:Expr):Expr :=
    when e
    is x:RRcell do toExpr(FloatFromDouble(toDouble(x)))
    else WrongArgRR();
setupfun("pythonFloatFromDouble",PyFloatFromDouble);

---------------
-- complexes --
---------------

import ComplexFromDoubles(real:double,imag:double):pythonObjectOrNull;
PyComplexFromDoubles(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:RRcell do
	when rhs
	is y:RRcell do toExpr(ComplexFromDoubles(toDouble(x), toDouble(y)))
	else WrongArgRR(2)
    else WrongArgRR(1);
PyComplexFromDoubles(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 2 then PyComplexFromDoubles(a.0, a.1)
	else WrongNumArgs(2)
    else WrongNumArgs(2);
setupfun("pythonComplexFromDoubles",PyComplexFromDoubles);

-------------
-- strings --
-------------

import UnicodeAsUTF8(o:pythonObject):constcharstar;
PyUnicodeAsUTF8(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(UnicodeAsUTF8(x.v))
    is s:SpecialExpr do PyUnicodeAsUTF8(s.e)
    else WrongArgPythonObject();
setupfun("pythonUnicodeAsUTF8",PyUnicodeAsUTF8);

import UnicodeFromString(u:charstar):pythonObjectOrNull;
PyUnicodeFromString(e:Expr):Expr :=
    when e
    is x:stringCell do toExpr(UnicodeFromString(tocharstar(x.v)))
    else WrongArgString();
setupfun("pythonUnicodeFromString",PyUnicodeFromString);

------------
-- tuples --
------------

import TupleNew(n:int):pythonObjectOrNull;
PyTupleNew(e:Expr):Expr :=
    when e
    is n:ZZcell do toExpr(TupleNew(toInt(n)))
    else WrongArgZZ();
setupfun("pythonTupleNew",PyTupleNew);

import TupleSetItem(L:pythonObject,i:int,item:pythonObject):int;
PyTupleSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:ZZcell do
	    when e3
	    is z:pythonObjectCell do (
		if TupleSetItem(x.v, toInt(y), z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    is s:SpecialExpr do PyTupleSetItem(e1, e2, s.e)
	    else WrongArgPythonObject(3)
	else WrongArgZZ(2)
    else WrongArgPythonObject(1);
PyTupleSetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyTupleSetItem(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonTupleSetItem",PyTupleSetItem);

-----------
-- lists --
-----------

import ListNew(n:int):pythonObjectOrNull;
PyListNew(e:Expr):Expr :=
    when e
    is n:ZZcell do toExpr(ListNew(toInt(n)))
    else WrongArgZZ();
setupfun("pythonListNew",PyListNew);

import ListSetItem(L:pythonObject,i:int,item:pythonObject):int;
PyListSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:ZZcell do
	    when e3
	    is z:pythonObjectCell do (
		if ListSetItem(x.v, toInt(y), z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    is s:SpecialExpr do PyListSetItem(e1, e2, s.e)
	    else WrongArgPythonObject(3)
	else WrongArgZZ(2)
    else WrongArgPythonObject(1);
PyListSetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyListSetItem(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonListSetItem",PyListSetItem);

------------------
-- dictionaries --
------------------

import DictNew():pythonObjectOrNull;
PyDictNew(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 0 then toExpr(DictNew())
	else WrongNumArgs(0)
    else WrongNumArgs(0);
setupfun("pythonDictNew",PyDictNew);

import DictSetItem(p:pythonObject,key:pythonObject,val:pythonObject):int;
PyDictSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:pythonObjectCell do (
		if DictSetItem(x.v, y.v, z.v) == -1 then
		    buildPythonErrorPacket()
		else nullE)
	    is s:SpecialExpr do PyDictSetItem(e1, e2, s.e)
	    else WrongArgPythonObject(3)
	is s:SpecialExpr do PyDictSetItem(e1, s.e, e3)
	else WrongArgPythonObject(2)
    else WrongArgPythonObject(1);
PyDictSetItem(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyDictSetItem(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonDictSetItem",PyDictSetItem);

----------
-- sets --
----------

import SetNew(o:pythonObject):pythonObjectOrNull;
PySetNew(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(SetNew(x.v))
    else WrongArgPythonObject();
setupfun("pythonSetNew",PySetNew);

---------------
-- callables --
---------------

import ObjectCall(
    o:pythonObject,args:pythonObject,kwargs:pythonObject):pythonObjectOrNull;
PyObjectCall(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:pythonObjectCell do toExpr(ObjectCall(x.v, y.v, z.v))
	    else WrongArgPythonObject(3)
	else WrongArgPythonObject(2)
    is s:SpecialExpr do PyObjectCall(s.e, e2, e3)
    else WrongArgPythonObject(1);
PyObjectCall(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 3 then PyObjectCall(a.0, a.1, a.2)
	else WrongNumArgs(3)
    else WrongNumArgs(3);
setupfun("pythonObjectCall",PyObjectCall);

----------
-- none --
----------

import None:pythonObjectOrNull;
setupconst("pythonNone", toExpr(None));

---------------
-- importing --
---------------
import ImportImportModule(name:charstar):pythonObjectOrNull;
PyImportImportModule(e:Expr):Expr :=
    when e
    is x:stringCell do toExpr(ImportImportModule(tocharstar(x.v)))
    else WrongArgPythonObject();
setupfun("pythonImportImportModule",PyImportImportModule);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python.o "
-- End:
