-- Copyright 2009,2010 by Daniel R. Grayson
use common;
use util;

header "#include <Python.h>";

WrongArgPythonObject():Expr := WrongArg("a python object");
WrongArgPythonObject(n:int):Expr := WrongArg(n,"a python object");

import ErrOccurred():int;

buildPythonErrorPacket():Expr := (
    e := ErrOccurred();
    if e == 1 then (
	Ccode(void, "PyErr_Print()");
	buildErrorPacket("python error"))
    else if e == -1 then StopIterationE
    else nullE);

pythonObjectOrNull := pythonObject or null;
toExpr(r:pythonObjectOrNull):Expr := (
    when r
    is null do buildPythonErrorPacket()
    is po:pythonObject do (
	h := int(Ccode(long, "PyObject_Hash(", po, ")"));
	if h == -1 then ( -- unhashable object (e.g., a list)
	    Ccode(void, "PyErr_Clear()");
	    h = nextHash());
	Expr(pythonObjectCell(po, h))));

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

PyObjectType(e:Expr):Expr := (
    when e
    is o:pythonObjectCell do toExpr(
	Ccode(pythonObjectOrNull, "PyObject_Type(", o.v, ")"))
    is s:SpecialExpr do PyObjectType(s.e)
    else WrongArgPythonObject());
setupfun("objectType",PyObjectType);

PyObjectRichCompareBool(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:ZZcell do (
		r := Ccode(int, "PyObject_RichCompareBool(",
		    x.v, ", ", y.v, ", ",  toInt(z), ")");
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

PyObjectHasAttrString(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:stringCell do
	    toExpr(Ccode(int, "PyObject_HasAttrString(",
		    x.v , ", ", tocharstar(y.v), ")") == 1)
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

PyObjectGetAttrString(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:pythonObjectCell do
	when rhs
	is y:stringCell do toExpr(Ccode(pythonObjectOrNull,
		"PyObject_GetAttrString(", x.v , ", ", tocharstar(y.v), ")"))
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

PyObjectSetAttrString(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:stringCell do
	    when e3
	    is z:pythonObjectCell do (
		if Ccode(int, "PyObject_SetAttrString(",
		    x.v, ", ", tocharstar(y.v), ", ",  z.v, ")") == -1
		then buildPythonErrorPacket()
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

PyObjectStr(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(
	Ccode(pythonObjectOrNull, "PyObject_Str(", x.v, ")"))
    is s:SpecialExpr do PyObjectStr(s.e)
    else WrongArgPythonObject();
setupfun("pythonObjectStr",PyObjectStr);

import initspam():void;
runinitspam(e:Expr):Expr := (initspam(); nullE);
setupfun("initspam",runinitspam);

----------
-- bools -
----------

setupconst("pythonTrue",
    Expr(pythonObjectCell(Ccode(pythonObject, "Py_True"), 1)));
setupconst("pythonFalse",
    Expr(pythonObjectCell(Ccode(pythonObject, "Py_False"), 0)));

----------
-- ints --
----------

import LongAsZZ(z:ZZmutable, x:pythonObject):ZZmutable;
PyLongAsLong(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do (
	overflow := 0;
	y := Ccode(long,
	    "PyLong_AsLongAndOverflow(", x.v, ", &", overflow, ")");
	if ErrOccurred() == 1 then buildPythonErrorPacket()
	else if overflow == 0 then toExpr(y)
	else (
	    z := newZZmutable();
	    toExpr(moveToZZandclear(LongAsZZ(z, x.v)))))
    is s:SpecialExpr do PyLongAsLong(s.e)
    else WrongArgPythonObject();
setupfun("pythonLongAsLong",PyLongAsLong);

import LongFromZZ(x:ZZ):pythonObjectOrNull;
PyLongFromLong(e:Expr):Expr :=
    when e
    is x:ZZcell do (
	if isLong(x) then toExpr(Ccode(pythonObjectOrNull,
		"PyLong_FromLong(", toLong(x), ")"))
	else if isULong(x) then toExpr(Ccode(pythonObjectOrNull,
		"PyLong_FromUnsignedLong(", toULong(x), ")"))
	else toExpr(LongFromZZ(x.v)))
    else WrongArgPythonObject();
setupfun("pythonLongFromLong",PyLongFromLong);

------------
-- floats --
------------

PyFloatAsDouble(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do (
	y := Ccode(double, "PyFloat_AsDouble(", x.v, ")");
	if ErrOccurred() == 1 then buildPythonErrorPacket()
	else toExpr(y))
    is s:SpecialExpr do PyFloatAsDouble(s.e)
    else WrongArgPythonObject();
setupfun("pythonFloatAsDouble",PyFloatAsDouble);

PyFloatFromDouble(e:Expr):Expr :=
    when e
    is x:RRcell do toExpr(Ccode(pythonObjectOrNull,
	    "PyFloat_FromDouble(", toDouble(x), ")"))
    else WrongArgRR();
setupfun("pythonFloatFromDouble",PyFloatFromDouble);

---------------
-- complexes --
---------------

PyComplexFromDoubles(lhs:Expr,rhs:Expr):Expr :=
    when lhs
    is x:RRcell do
	when rhs
	is y:RRcell do toExpr(Ccode(pythonObjectOrNull,
		"PyComplex_FromDoubles(", toDouble(x), ", ", toDouble(y), ")"))
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

PyUnicodeAsUTF8(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(Ccode(constcharstar,
	    "PyUnicode_AsUTF8(", x.v, ")"))
    is s:SpecialExpr do PyUnicodeAsUTF8(s.e)
    else WrongArgPythonObject();
setupfun("pythonUnicodeAsUTF8",PyUnicodeAsUTF8);

PyUnicodeFromString(e:Expr):Expr :=
    when e
    is x:stringCell do toExpr(Ccode(pythonObjectOrNull,
	    "PyUnicode_FromString(", tocharstar(x.v), ")"))
    else WrongArgString();
setupfun("pythonUnicodeFromString",PyUnicodeFromString);

------------
-- tuples --
------------

PyTupleSetItem(t:pythonObject, i:int, y:Expr):int := (
    when y
    is x:pythonObjectCell do Ccode(int,
	"PyTuple_SetItem(", t, ", ", i, ", ", x.v, ")")
    is x:SpecialExpr do PyTupleSetItem(t, i, x.e)
    else -2);

PyTupleNew(e:Expr):Expr := (
    when e
    is a:Sequence do (
	n := length(a);
	x := Ccode(pythonObjectOrNull, "PyTuple_New(", n, ")");
	when x
	is null do return buildPythonErrorPacket()
	is t:pythonObject do foreach y at i in a do (
	    r := PyTupleSetItem(t, i, y);
	    if r == -1 then return buildPythonErrorPacket()
	    else if r == -2 then return WrongArgPythonObject(i + 1));
	toExpr(x))
    else WrongArg("a sequence"));
setupfun("pythonTupleNew", PyTupleNew);

-----------
-- lists --
-----------

PyListNew(e:Expr):Expr :=
    when e
    is n:ZZcell do toExpr(Ccode(pythonObjectOrNull,
	    "PyList_New(", toInt(n), ")"))
    else WrongArgZZ();
setupfun("pythonListNew",PyListNew);

PyListSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:ZZcell do
	    when e3
	    is z:pythonObjectCell do (
		if Ccode(int, "PyList_SetItem(",
		    x.v, ", ", toInt(y), ", ", z.v, ")") == -1
		then buildPythonErrorPacket()
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

PyDictNew(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) == 0 then toExpr(Ccode(pythonObjectOrNull, "PyDict_New()"))
	else WrongNumArgs(0)
    else WrongNumArgs(0);
setupfun("pythonDictNew",PyDictNew);

PyDictSetItem(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:pythonObjectCell do (
		if Ccode(int, "PyDict_SetItem(",
		    x.v, ", ", y.v, ", ", z.v, ")") == -1
		then buildPythonErrorPacket()
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

PySetNew(e:Expr):Expr :=
    when e
    is x:pythonObjectCell do toExpr(Ccode(pythonObjectOrNull,
	    "PySet_New(", x.v, ")"))
    else WrongArgPythonObject();
setupfun("pythonSetNew",PySetNew);

---------------
-- callables --
---------------

PyObjectCall(e1:Expr,e2:Expr,e3:Expr):Expr :=
    when e1
    is x:pythonObjectCell do
	when e2
	is y:pythonObjectCell do
	    when e3
	    is z:pythonObjectCell do toExpr(Ccode(pythonObjectOrNull,
		    "PyObject_Call(", x.v, ", ", y.v, ", ", z.v, ")"))
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

PyNone(e:Expr):Expr := toExpr(Ccode(pythonObjectOrNull, "Py_None"));
setupfun("getPythonNone", PyNone);

---------------
-- importing --
---------------

PyImportImportModule(e:Expr):Expr :=
    when e
    is x:stringCell do toExpr(Ccode(pythonObjectOrNull,
	    "PyImport_ImportModule(", tocharstar(x.v), ")"))
    else WrongArgPythonObject();
setupfun("pythonImportImportModule",PyImportImportModule);

-- Local Variables:
-- compile-command: "echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && echo \"make: Entering directory \\`$M2BUILDDIR/Macaulay2/d'\" && make -C $M2BUILDDIR/Macaulay2/d python.o "
-- End:
