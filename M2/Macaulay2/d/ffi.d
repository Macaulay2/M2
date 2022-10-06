use system;
use common;
use hashtables;
use evaluate;

header "#include <dlfcn.h>
	#include <ffi.h>
	/* FFI_BAD_ARGTYPE not introduced until libffi 3.4 in 2021 */
	#ifndef FFI_BAD_ARGTYPE
	  #define FFI_BAD_ARGTYPE 3
	#endif
	#include <M2mem.h>";

voidPointerOrNull := voidPointer or null;
toExpr(x:voidPointer):Expr := Expr(pointerCell(x));
WrongArgPointer():Expr := WrongArg("a pointer");
WrongArgPointer(n:int):Expr := WrongArg(n, "a pointer");
setupconst("nullPointer", toExpr(nullPointer()));

getMem(n:int):voidPointer := Ccode(voidPointer, "getmem(", n, ")");
getMemAtomic(n:int):voidPointer := Ccode(voidPointer, "getmem_atomic(", n, ")");

dlerror0():Expr:= buildErrorPacket(tostring(Ccode(charstar, "dlerror()")));

dlopen0(e:Expr):Expr:=
    when e
    is s:stringCell do (
	r := Ccode(voidPointerOrNull,
	    "dlopen(", tocharstar(s.v), ", RTLD_LAZY)");
	when r
	is null do dlerror0()
	is handle:voidPointer do toExpr(handle))
    else WrongArgString();
setupfun("dlopen", dlopen0);

dlsym0(e:Expr):Expr :=
    when e
    is y:stringCell do (
	r := Ccode(voidPointerOrNull,
	    "dlsym(RTLD_DEFAULT, ", tocharstar(y.v), ")");
	when r
	is null do dlerror0()
	is addr:voidPointer do toExpr(addr))
    is a:Sequence do
	if length(a) != 2 then WrongNumArgs(1,2)
	else when a.0
	    is x:pointerCell do when a.1
		is y:stringCell do (
		    r := Ccode(voidPointerOrNull,
			"dlsym(", x.v, ", ", tocharstar(y.v), ")");
		    when r
		    is null do dlerror0()
		    is addr:voidPointer do toExpr(addr))
		else WrongArgString(2)
	    else WrongArgPointer(1)
    else WrongArg("a string or a pointer and a string");
setupfun("dlsym", dlsym0);

ffiTypeVoid := Ccode(voidPointer, "&ffi_type_void");
ffiOk := Ccode(int, "FFI_OK");

ffiTypeSize(e:Expr):Expr := (
    when e
    is x:pointerCell do toExpr(Ccode(int, "((ffi_type *)", x.v, ")->size"))
    else WrongArgPointer());
setupfun("ffiTypeSize", ffiTypeSize);

ffiError(r:int):Expr:=
    if r == Ccode(int, "FFI_BAD_TYPEDEF")
    then buildErrorPacket("libffi: bad typedef")
    else if r == Ccode(int, "FFI_BAD_ABI")
    then buildErrorPacket("libffi: bad ABI")
    else if r == Ccode(int, "FFI_BAD_ARGTYPE")
    then buildErrorPacket("libffi: bad argtype")
    else buildErrorPacket("libffi: unknown");

ffiPrepCif(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) != 2 then WrongNumArgs(2)
	else when a.0
	    is x:pointerCell do when a.1
		is y:List do (
		    argtypes := new array(voidPointer) len length(y.v) at i
			do provide when y.v.i is z:pointerCell do z.v
			    else ffiTypeVoid;
		    cif := Ccode(voidPointer, "getmem(sizeof(ffi_cif))");
		    r := Ccode(int, "ffi_prep_cif((ffi_cif *)", cif,
			", FFI_DEFAULT_ABI, ",
			argtypes, "->len, ",
			"(ffi_type *) ", x.v, ", ",
			"(ffi_type **) ", argtypes, "->array)");
		    if r != ffiOk then ffiError(r)
		    else toExpr(cif))
		else WrongArg(2, "a list")
	    else WrongArgPointer(1)
    else WrongNumArgs(2);
setupfun("ffiPrepCif", ffiPrepCif);

ffiPrepCifVar(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) != 3 then WrongNumArgs(3)
	else when a.0
	    is n:ZZcell do when a.1
		is x:pointerCell do when a.2
		    is y:List do (
			argtypes := new array(voidPointer) len length(y.v) at i
				do provide when y.v.i is z:pointerCell do z.v
				else ffiTypeVoid;
			cif := Ccode(voidPointer, "getmem(sizeof(ffi_cif))");
			r := Ccode(int, "ffi_prep_cif_var((ffi_cif *)", cif,
			    ", FFI_DEFAULT_ABI, ",
			    toInt(n), ", ",
			    argtypes, "->len, ",
			    "(ffi_type *) ", x.v, ", ",
			    "(ffi_type **) ", argtypes, "->array)");
			if r != ffiOk then ffiError(r)
			else toExpr(cif))
		    else WrongArg(3, "a list")
		else WrongArgPointer(2)
	    else WrongArgZZ(1)
	else WrongNumArgs(3);
setupfun("ffiPrepCifVar", ffiPrepCifVar);

ffiCall(e:Expr):Expr :=
    when e
    is a:Sequence do
	if length(a) != 4 then WrongNumArgs(4)
	else when a.0
	    is cif:pointerCell do when a.1
		is fn:pointerCell do when a.2
		    is n:ZZcell do when a.3
			is z:List do (
			    rvalue := getMem(toInt(n));
			    avalues := new array(voidPointer)
				len length(z.v) at i
				    do provide when z.v.i is p:pointerCell
					do p.v
					else nullPointer();
			    Ccode(void, "ffi_call((ffi_cif *)", cif.v, ", ",
				fn.v, ", ",
				rvalue, ", ",
				avalues, "->array)");
			    toExpr(rvalue))
			else WrongArg(4, "a list")
		    else WrongArgZZ(3)
		else WrongArgPointer(2)
	    else WrongArgPointer(1)
    else WrongNumArgs(4);
setupfun("ffiCall", ffiCall);

---------------
-- void type --
---------------
setupconst("ffiVoidType", toExpr(Ccode(voidPointer, "&ffi_type_void")));

-------------------
-- integer types --
-------------------

ffiIntegerType(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is n:ZZcell do (
		when a.1
		is signed:Boolean do (
		    bits := toInt(n);
		    if signed.v then (
			if bits == 8
			then toExpr(Ccode(voidPointer, "&ffi_type_sint8"))
			else if bits == 16
			then toExpr(Ccode(voidPointer, "&ffi_type_sint16"))
			else if bits == 32
			then toExpr(Ccode(voidPointer, "&ffi_type_sint32"))
			else if bits == 64
			then toExpr(Ccode(voidPointer, "&ffi_type_sint64"))
			else buildErrorPacket("expected 8, 16, 32, or 64 bits"))
		    else (
			if bits == 8
			then toExpr(Ccode(voidPointer, "&ffi_type_uint8"))
			else if bits == 16
			then toExpr(Ccode(voidPointer, "&ffi_type_uint16"))
			else if bits == 32
			then toExpr(Ccode(voidPointer, "&ffi_type_uint32"))
			else if bits == 64
			then toExpr(Ccode(voidPointer, "&ffi_type_uint64"))
			else buildErrorPacket("expected 8, 16, 32, or 64 bits"))
		    )
		else WrongArgBoolean(2))
	    else WrongArgZZ(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("ffiIntegerType", ffiIntegerType);

ffiIntegerAddress(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 3 then (
	    when a.0
	    is x:ZZcell do (
		when a.1
		is y:ZZcell do (
		    when a.2
		    is signed:Boolean do (
			bits := toInt(y);
			ptr := getMemAtomic(bits / 8);
			if signed.v then (
			    if bits == 8
			    then Ccode(void, "*(int8_t *)", ptr, " = ",
				toInt(x))
			    else if bits == 16
			    then Ccode(void, "*(int16_t *)", ptr, " = ",
				toInt(x))
			    else if bits == 32
			    then Ccode(void, "*(int32_t *)", ptr, " = ",
				toLong(x))
			    else if bits == 64
			    then Ccode(void, "*(int64_t *)", ptr, " = ",
				toInt64(x))
			    else return buildErrorPacket(
				"expected 8, 16, 32, or 64 bits");
			    toExpr(ptr))
			else (
			    if bits == 8
			    then Ccode(void, "*(uint8_t *)", ptr, " = ",
				toUInt(x))
			    else if bits == 16
			    then Ccode(void, "*(uint16_t *)", ptr, " = ",
				toUInt(x))
			    else if bits == 32
			    then Ccode(void, "*(uint32_t *)", ptr, " = ",
				toULong(x))
			    else if bits == 64
			    then Ccode(void, "*(uint64_t *)", ptr, " = ",
				toUInt64(x))
			    else return buildErrorPacket(
				"expected 8, 16, 32, or 64 bits");
			    toExpr(ptr)))
		    else WrongArgBoolean(3))
		else WrongArgZZ(2))
	    else WrongArgZZ(1))
	else WrongNumArgs(3))
    else WrongNumArgs(3));
setupfun("ffiIntegerAddress", ffiIntegerAddress);

ffiIntegerValue(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 3 then (
	    when a.0
	    is x:pointerCell do (
		when a.1
		is y:ZZcell do (
		    when a.2
		    is signed:Boolean do (
			bits := toInt(y);
			if signed.v then (
			    if bits == 8
			    then toExpr(Ccode(int8_t, "*(int8_t *)", x.v))
			    else if bits == 16
			    then toExpr(Ccode(int16_t, "*(int16_t *)", x.v))
			    else if bits == 32
			    then toExpr(Ccode(int32_t, "*(int32_t *)", x.v))
			    else if bits == 64
			    then toExpr(Ccode(int64_t, "*(int64_t *)", x.v))
			    else buildErrorPacket(
				"expected 8, 16, 32, or 64 bits"))
			else (
			    if bits == 8
			    then toExpr(Ccode(uint8_t, "*(uint8_t *)", x.v))
			    else if bits == 16
			    then toExpr(Ccode(uint16_t, "*(uint16_t *)", x.v))
			    else if bits == 32
			    then toExpr(Ccode(uint32_t, "*(uint32_t *)", x.v))
			    else if bits == 64
			    then toExpr(Ccode(uint64_t, "*(uint64_t *)", x.v))
			    else buildErrorPacket(
				"expected 8, 16, 32, or 64 bits")))
		    else WrongArgBoolean(3))
		else WrongArgZZ(2))
	    else WrongArgPointer(1))
	else WrongNumArgs(3))
    else WrongNumArgs(3));
setupfun("ffiIntegerValue", ffiIntegerValue);

----------------
-- real types --
----------------

ffiRealType(e:Expr):Expr := (
    when e
    is n:ZZcell do (
	bits := toInt(n);
	if bits == 32 then toExpr(Ccode(voidPointer, "&ffi_type_float"))
	else if bits == 64 then toExpr(Ccode(voidPointer, "&ffi_type_double"))
	else buildErrorPacket("expected 32 or 64 bits"))
    else WrongArgZZ());
setupfun("ffiRealType", ffiRealType);

ffiRealAddress(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is x:RRcell do (
		when a.1
		is y:ZZcell do (
		    bits := toInt(y);
		    ptr := getMemAtomic(bits / 8);
		    if bits == 32
		    then (
			z := toFloat(x);
			Ccode(void, "*(float *)", ptr, " = ", z))
		    else if bits == 64
		    then (
			z := toDouble(x);
			Ccode(void, "*(double *)", ptr, " = ", z))
		    else return buildErrorPacket("expected 32 or 64 bits");
		    toExpr(ptr))
		else WrongArgZZ(2))
	    else WrongArgRR(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("ffiRealAddress", ffiRealAddress);

ffiRealValue(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is x:pointerCell do (
		when a.1
		is y:ZZcell do (
		    bits := toInt(y);
		    if bits == 32
		    then toExpr(Ccode(float, "*(float *)", x.v))
		    else if bits == 64
		    then toExpr(Ccode(double, "*(double *)", x.v))
		    else buildErrorPacket("expected 32 or 64 bits"))
		else WrongArgZZ(2))
	    else WrongArgPointer(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("ffiRealValue", ffiRealValue);

-------------------
-- pointer types --
-------------------

pointerSize := Ccode(int, "sizeof(void *)");

setupconst("ffiPointerType", toExpr(Ccode(voidPointer, "&ffi_type_pointer")));

ffiPointerAddress(e:Expr):Expr := (
    when e
    is x:pointerCell do (
	y := getMem(pointerSize);
	Ccode(void, "*(void **)", y, " = ", x.v);
	toExpr(y))
    is x:stringCell do (
	y := getMem(pointerSize);
	Ccode(void, "*(void **)", y, " = ", tocharstar(x.v));
	toExpr(y))
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is x:pointerCell do (
		when a.1
		is y:List do (
		    n := length(y.v);
		    bytes := Ccode(int, "((ffi_type *)", x.v, ")->size");
		    arr := getMem(n * bytes);
		    for i from 0 to n - 1 do (
			when y.v.i
			is z:pointerCell do Ccode(void,
			    "memcpy(", arr, " + ", i * bytes, ", ",
			    z.v, ", ", bytes, ")")
			else return buildErrorPacket(
			    "expected a list of pointers"));
		    ptr := getMem(pointerSize);
		    Ccode(void, "*(void **)", ptr, " = ", arr);
		    toExpr(ptr))
		else WrongArg(2, "a list"))
	    else WrongArgPointer(1))
	else WrongNumArgs(1, 2))
    else WrongArg("a pointer, string, or a pointer and a list"));
setupfun("ffiPointerAddress", ffiPointerAddress);

ffiPointerValue(e:Expr):Expr := (
    when e
    is x:pointerCell do toExpr(Ccode(voidPointer, "*(void **)", x.v))
    else WrongArgPointer());
setupfun("ffiPointerValue", ffiPointerValue);

ffiStringValue(e:Expr):Expr := (
    when e
    is x:pointerCell do toExpr(tostring(Ccode(charstar, "*(char **)", x.v)))
    else WrongArgPointer());
setupfun("ffiStringValue", ffiStringValue);

ptrfinalizer(x:voidPointer, a:Sequence):void := (
    if length(a) == 2 then (
	applyEE(a.0, a.1);
	nothing)
    else nothing);

registerFinalizerForPointer(e:Expr):Expr := (
    when e is s:Sequence do (
	if length(s) == 3 then (
	    when s.0
	    is x:pointerCell do (
		a := new Sequence len 2 at i do provide s.(i + 1);
		Ccode(void, "GC_REGISTER_FINALIZER(", x.v, ", ",
		    "(GC_finalization_proc)", ptrfinalizer, ", ", a, ", 0, 0)");
		nullE)
	    else WrongArgPointer(1))
	else WrongNumArgs(3))
    else WrongNumArgs(3));
setupfun("registerFinalizerForPointer", registerFinalizerForPointer);

------------------
-- struct types --
------------------

ffiStructType(e:Expr):Expr := (
    when e
    is a:List do (
	x := getMem(Ccode(int, "sizeof(ffi_type)"));
	Ccode(void, "((ffi_type *)", x, ")->size = 0");
	Ccode(void, "((ffi_type *)", x, ")->alignment = 0");
	Ccode(void, "((ffi_type *)", x, ")->type = FFI_TYPE_STRUCT");
	n := length(a.v);
	elmnts := new array(voidPointer) len n + 1 at i do provide (
	    if i < n then (
		when a.v.i
		is p:pointerCell do p.v
		else nullPointer())
	    else nullPointer());
	Ccode(void, "((ffi_type *)", x, ")->elements = (ffi_type **)",
	    elmnts, "->array");
	toExpr(x))
    else WrongArg("a list"));
setupfun("ffiStructType", ffiStructType);

ffiGetStructOffsets(e:Expr):Expr :=
    when e
    is x:pointerCell do (
	n := 0;
	while Ccode(voidPointer, "(((ffi_type *)", x.v, ")->elements)[", n,
	    "]") != nullPointer() do n = n + 1;
	offsets := Ccode(voidPointer, "getmem((", n , ") * sizeof(size_t))");
	r := Ccode(int, "ffi_get_struct_offsets(FFI_DEFAULT_ABI, ",
	    "(ffi_type *)", x.v, ", (size_t *)", offsets, ")");
	if r != ffiOk then return ffiError(r);
	Expr(list(new Sequence len n at i do provide
	    toExpr(Ccode(int, "((size_t *)", offsets, ")[", i, "]")))))
    else WrongArgPointer();
setupfun("ffiGetStructOffsets", ffiGetStructOffsets);

ffiStructAddress(e:Expr):Expr := (
    when e
    is a:Sequence do (
	if length(a) == 2 then (
	    when a.0
	    is x:pointerCell do (
		when a.1 is y:List do (
		    n := 0;
		    while Ccode(voidPointer, "(((ffi_type *)", x.v,
			")->elements)[", n, "]") != nullPointer() do n = n + 1;
		    offsets := Ccode(voidPointer, "getmem((", n ,
			") * sizeof(size_t))");
		    r := Ccode(int, "ffi_get_struct_offsets(FFI_DEFAULT_ABI, ",
			"(ffi_type *)", x.v, ", (size_t *)", offsets, ")");
		    if r != ffiOk then return ffiError(r);
		    result := getMem(Ccode(int, "((ffi_type *)", x.v,
			    ")->size"));
		    for i from 0 to n - 1 do (
			when y.v.i
			is elmnt:pointerCell do (
			    tocopy := Ccode(int, "(((ffi_type *)", x.v,
				")->elements)[", i, "]->size");
			    Ccode(void, "memcpy(", result, " + ((size_t *)",
				offsets, ")[", i, "],", elmnt.v, ", ",
				tocopy, ")"))
			else nothing);
		    toExpr(result))
		else WrongArg(2, "a list"))
	    else WrongArgPointer(1))
	else WrongNumArgs(2))
    else WrongNumArgs(2));
setupfun("ffiStructAddress", ffiStructAddress);

-----------------
-- union types --
-----------------

-- ffi_prep_cif trick from libffi manual
ffiUnionType(e:Expr):Expr := (
    when e
    -- expects a list of pointers to ffi_type's
    is a:List do (
	x := getMem(Ccode(int, "sizeof(ffi_type)"));
	Ccode(void, "((ffi_type *)", x, ")->size = 0");
	Ccode(void, "((ffi_type *)", x, ")->alignment = 0");
	Ccode(void, "((ffi_type *)", x, ")->type = FFI_TYPE_STRUCT");
	elmnts := new array(voidPointer) len 2 do provide nullPointer();
	Ccode(void, "((ffi_type *)", x, ")->elements = (ffi_type **)",
	    elmnts, "->array");
	cif := getMem(Ccode(int, "sizeof(ffi_cif)"));
	for i from 0 to length(a.v) - 1 do (
	    when a.v.i
	    is p:pointerCell do (
		if Ccode(int, "ffi_prep_cif((ffi_cif *)", cif,
		    ", FFI_DEFAULT_ABI, 0, ", p.v, ", NULL)") == ffiOk then (
		    if Ccode(int, "((ffi_type *)", p.v , ")->size"
			) > Ccode(int, "((ffi_type *)", x, ")->size"
			) then (Ccode(void, "((ffi_type *)", x,
			    ")->size = ((ffi_type *)", p.v, ")->size"));
		    if Ccode(int, "((ffi_type *)", p.v, ")->alignment"
			) > Ccode(int, "((ffi_type *)", x, ")->alignment"
			) then (Ccode(void, "((ffi_type *)", x,
			    ")->alignment = ((ffi_type *)", p.v,
			    ")->alignment"))))
	    else return WrongArgPointer(i + 1));
	toExpr(x))
    else WrongArg("a list"));
setupfun("ffiUnionType", ffiUnionType);
