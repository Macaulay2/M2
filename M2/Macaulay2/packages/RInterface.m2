newPackage("RInterface",
    Headline => "interface to R for statistical computing",
    Version => "0.1",
    Date => "January 28, 2024",
    Authors => {{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"}},
    Keywords => {"Interfaces"},
    OptionalComponentsPresent => run("command -v R > /dev/null") == 0,
    AuxiliaryFiles => true,
    PackageImports => {"ForeignFunctions"})


endpkg = msg -> (
    document {Key => RInterface,
	Headline => (options currentPackage).Headline,
	"Warning: RInterface was loaded without key components."};
    printerr("warning: ", msg, "; ending");
    end)

if not (options currentPackage).OptionalComponentsPresent
then endpkg "R cannot be found"

if not ForeignFunctions#"private dictionary"#?"foreignFunction"
then endpkg "foreign function interface is not available"

export {
    -- types
    "RObject",
    "RFunction",

    -- methods
    "RSymbol",

    -- objects
    "NA"
    }

----------
-- TODO --
----------

-- are there any low-level errors that might still crash M2?
-- help(RObject)
-- graphics device is buggy (can't close window; image disappears)

--------------------
-- initialization --
--------------------
RHOME = last lines get "!R RHOME"
Rlib = openSharedLibrary("R", FileName => RHOME | "/lib/libR." |
    if version#"operating system" == "Darwin" then "dylib" else "so")

-- issue: if we try to re-initialize R w/o restarting M2, then we get an
-- error and R kicks us out of M2 completely.  this is a problem when reloading
-- the package.
-- hacky solution: store something inside some mutable hash table that won't
-- be dismissed when we reload the package and check there
if not ZZ#?"Rinitialized" then (
    putenv := foreignFunction("putenv", int, charstar);
    initializeR := foreignFunction(Rlib, "Rf_initialize_R", int,
	{int, charstarstar});
    RCStackLimit := foreignSymbol(Rlib, "R_CStackLimit", uint);
    setupRmainloop := foreignFunction(Rlib, "setup_Rmainloop", void, void);
    putenv concatenate("R_HOME=", RHOME);
    initializeR(3, {"M2::RInterface", "--silent", "--vanilla"});
    *address RCStackLimit = -1;
    setupRmainloop();
    ZZ#"Rinitialized" = true)

-------------
-- RObject --
-------------

SEXP = voidstar

RObject = new SelfInitializingType of SEXP
RObject.synonym = "R object"

length' = value @@ (foreignFunction(Rlib, "LENGTH", int, SEXP))

---------------
-- functions --
---------------

RFunction = new SelfInitializingType of FunctionClosure

lcons = foreignFunction(Rlib, "Rf_lcons", SEXP, {SEXP, SEXP})
PROTECT = foreignFunction(Rlib, "Rf_protect", SEXP, SEXP)
UNPROTECT = foreignFunction(Rlib, "Rf_unprotect", void, int)
RtryEval = foreignFunction(Rlib, "R_tryEval", SEXP, {SEXP, SEXP, voidstar})
RGlobalEnv = foreignSymbol(Rlib, "R_GlobalEnv", SEXP)

tryEval = x -> (
    err := int 0;
    result := RtryEval(x, RGlobalEnv, address err);
    if value err != 0 then error "R error"
    else RObject result)

new RFunction from RObject := (T, f) -> (
    x -> (
	if not instance(x, Sequence) then x = 1 : x;
	call := PROTECT lcons(f, RObject x);
	result := tryEval call;
	UNPROTECT 1;
	result))
new RFunction from String := (T, s) -> RFunction RSymbol s

install = foreignFunction(Rlib, "Rf_install", SEXP, charstar)
RSymbol = method()
RSymbol String := s -> RObject install s

net RObject := stack @@ value @@ (RFunction "capture.output")

typeof = value @@ (RFunction "typeof");
RObject.AfterPrint = x -> (RObject, " of type ", typeof x)

--------------------
-- Macaulay2 -> R --
--------------------

-- TODO: do I need to worry about PROTECT?

NilValue = foreignSymbol(Rlib, "R_NilValue", SEXP)
new RObject from Nothing := (T, x) -> T NilValue

cons = foreignFunction(Rlib, "Rf_cons", SEXP, {SEXP, SEXP})
setTag = foreignFunction(Rlib, "SET_TAG", void, {SEXP, SEXP})
new RObject from Sequence := (T, x) -> T fold(append(x, NilValue),
    (a, b) -> (
	if instance(a, Option) then (
	    y := PROTECT cons(T last a, b);
	    setTag(y, install(toString first a));
	    UNPROTECT 1;
	    y)
	else cons(T a, b)))

scalarLogical = foreignFunction(Rlib, "Rf_ScalarLogical", SEXP, int)
new RObject from Boolean := (T, x) -> T scalarLogical if x then 1 else 0
NA = RObject scalarLogical foreignSymbol(Rlib, "R_NaInt", int)

scalarInteger = foreignFunction(Rlib, "Rf_ScalarInteger", SEXP, int)
new RObject from ZZ := (T, x) -> T scalarInteger x

scalarReal = foreignFunction(Rlib, "Rf_ScalarReal", SEXP, double)
new RObject from RR := (T, x) -> T scalarReal x
new RObject from Number := (T, x) -> T numeric x

Rcomplex = foreignStructType("Rcomplex", {"r" => double, "i" => double})
scalarComplex = foreignFunction(Rlib, "Rf_ScalarComplex", SEXP, Rcomplex)
new RObject from CC := (T, x) -> T scalarComplex {
    "r" => realPart x, "i" => imaginaryPart x}

mkString = foreignFunction(Rlib, "Rf_mkString", SEXP, charstar)
new RObject from String := (T, x) -> T mkString x
new RObject from Symbol := (T, x) -> T toString x

new RObject from List := (RFunction "c") @@ ((T, x) -> toSequence x)

new RObject from  Matrix := (RFunction "matrix") @@ ((T, x) -> (
	flatten entries transpose x,
	"nrow" => numRows x,
	"ncol" => numColumns x))

--------------------
-- R -> Macaulay2 --
--------------------

TYPEOF = value @@ (foreignFunction(Rlib, "TYPEOF", int, SEXP))
CHAR = foreignFunction(Rlib, "R_CHAR", charstar, SEXP)
LOGICAL = foreignFunction(Rlib, "LOGICAL", voidstar, SEXP)
INTEGER = foreignFunction(Rlib, "INTEGER", voidstar, SEXP)
REAL = foreignFunction(Rlib, "REAL", voidstar, SEXP)
COMPLEX = foreignFunction(Rlib, "COMPLEX", voidstar, SEXP)
stringElt = foreignFunction(Rlib, "STRING_ELT", SEXP, {SEXP, int})
vectorElt = foreignFunction(Rlib, "VECTOR_ELT", SEXP, {SEXP, int})
getAttrib = foreignFunction(Rlib, "Rf_getAttrib", SEXP, {SEXP, SEXP})
NamesSymbol = foreignSymbol(Rlib, "R_NamesSymbol", SEXP)
DimSymbol = foreignSymbol(Rlib, "R_DimSymbol", SEXP)

-- SEXPTYPE's from Rinternals.h
NILSXP     = 0
SYMSXP     = 1
LISTSXP    = 2
CLOSXP     = 3
LANGSXP    = 6
SPECIALSXP = 7
BUILTINSXP = 8
CHARSXP    = 9
LGLSXP     = 10
INTSXP     = 13
REALSXP    = 14
CPLXSXP    = 15
STRSXP     = 16
VECSXP     = 19

isIterable = set {LGLSXP, INTSXP, REALSXP, CPLXSXP, STRSXP}

valueFunctions = hashTable {
    NILSXP     => x -> null,
    SYMSXP     => x -> value tryEval x,
    CHARSXP    => x -> value CHAR x,
    LGLSXP     => x -> value(int * LOGICAL x) == 1,
    INTSXP     => x -> value(int * INTEGER x),
    REALSXP    => x -> value(double * REAL x),
    CPLXSXP    => x -> (
	z := value(Rcomplex * COMPLEX  x);
	z#"r" + ii * z#"i"),
    STRSXP     => x -> (
	y := stringElt(x, 0);
	T := TYPEOF y;
	if T == CHARSXP then value CHAR y
	else if T == NILSXP then null
	else error "unexpected element of STRSXP")}

value RObject := x -> (
    T := TYPEOF x;
    y := (if T == LISTSXP then value \ toSequence x
	else if T == VECSXP or isIterable#?T and length' x > 1
	then value \ toList x
	else if valueFunctions#?T then valueFunctions#T x
	else return x);
    dims := RObject getAttrib(x, DimSymbol);
    -- TODO - do we transpose back to row-major, at least for matrices?
    if TYPEOF dims =!= NILSXP
    then y = fold(pack, splice {y, drop(toSequence value dims, -1)});
    names := RObject getAttrib(x, NamesSymbol);
    if TYPEOF names === NILSXP then y
    else (
	if not instance(y, VisibleList) then {value names => y}
	else (if T == LISTSXP then toSequence else identity) apply(
	    value names, y, (name, elt) ->
	    if #name > 0 then name => elt else elt)))

---------------
-- iteration --
---------------

CAR = foreignFunction(Rlib, "CAR", SEXP, SEXP)
CDR = foreignFunction(Rlib, "CDR", SEXP, SEXP)

-- logical, integer, real, and complex vectors are just wrappers around
-- basic C arrays, but we don't want to expose these types to the user,
-- so we extract each element and turn it back into an R scalar

getEltFunctions = hashTable {
    LGLSXP  => (int, LOGICAL, scalarLogical),
    INTSXP  => (int, INTEGER, scalarInteger),
    REALSXP => (double, REAL, scalarReal),
    CPLXSXP => (Rcomplex, COMPLEX, scalarComplex)}

iterator RObject := x -> runHooks((iterator, RObject), (x, TYPEOF x))

addHook((iterator, RObject),
    (x, T) -> if T == NILSXP then Iterator(() -> StopIteration),
    Strategy => "NILSXP")

addHook((iterator, RObject),
    (x, T) -> if getEltFunctions#?T then Iterator (
	n := length' x;
	(ctype, toptr, tosexp) := getEltFunctions#T;
	ptr := value toptr x;
	i := 0;
	() -> (
	    if i >= n then StopIteration
	    else (
		r := RObject tosexp(ctype * voidstar ptr);
		i = i + 1;
		ptr = ptr + size ctype;
		r))),
    Strategy => "LGLSXP/INTSXP/REALSXP/CPLXSXP")

addHook((iterator, RObject),
    (x, T) -> if T == LISTSXP or T == LANGSXP then Iterator (
	() -> (
	    if TYPEOF x == NILSXP then StopIteration
	    else (
		r := RObject CAR x;
		x = CDR x;
		r))),
    Strategy => "LISTSXP")

addHook((iterator, RObject),
    (x, T) -> if T == STRSXP then Iterator (
	n := length' x;
	i := 0;
	() -> (
	    if i >= n then StopIteration
	    else (
		-- convert element back to STRSXP instead of CHARSXP
		r := RObject mkString CHAR stringElt(x, i);
		i = i + 1;
		r))),
    Strategy => "STRSXP")

addHook((iterator, RObject),
    (x, T) -> if T == VECSXP then Iterator (
	n := length' x;
	i := 0;
	() -> (
	    if i >= n then StopIteration
	    else (
		r := RObject vectorElt(x, i);
		i = i + 1;
		r))),
    Strategy => "VECSXP")

--------------------------
-- exported R functions --
--------------------------

exportRFunction = f -> (
    getGlobalSymbol(currentPackage#"private dictionary", f) <- RFunction f;
    export {toString f})

exportRFunction "library"

-- TODO: any other functions worthwhile to export? or let user deal w/ this?

---------------------
-- RObject methods --
---------------------

-- comparison
rlt = RFunction "<"
rgt = RFunction ">"
req = RFunction "=="
RObject == RObject := (x, y) -> (
    r := value req(x, y);
    if instance(r, List) then all(r, identity) else r)
RObject == Thing := (x, y) -> x == RObject y
Thing == RObject := (x, y) -> RObject x == y
RObject ? RObject := (x, y) -> (
    if value rlt(x, y) then symbol <
    else if value rgt(x, y) then symbol >
    else if value req(x, y) then symbol ==
    else incomparable)
RObject ? Thing := (x, y) -> x ? RObject y
Thing ? RObject := (x, y) -> RObject x ? y

-- subscripting
RObject_Thing := (RFunction "[[") @@ splice
RObject Array := (RFunction "[") @@ ((x, y) -> splice(x,
	if #y > 1 then 1:splice toList y else toSequence y))

-- subscript assignment (creates new object; original is not modified)
RObject_Thing = (RFunction "[[<-") @@ splice
RObject Array = (RFunction "[<-") @@ ((x, y, e) -> splice(x,
	if #y > 1 then 1:splice toList y else toSequence y, e))

-- unary
for f in {
    symbol +,
    symbol -,
    abs,
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atanh,
    ceiling,
    cos,
    cosh,
    exp,
    expm1,
    floor,
    log,
    log1p,
    max,
    min,
    round,
    sin,
    sinh,
    sqrt,
    sum,
    tan,
    tanh
    } do (
    installMethod(f, RObject, RFunction toString f))

length RObject := value @@ (RFunction "length") -- needs to return a ZZ

-- unary w/ different names
scan({
	(symbol not, "!"),
	(symbol !, "factorial"),
	(symbol ~, "bitwNot"),
	(conjugate, "Conj"),
	(Digamma, "digamma"),
	(Gamma, "gamma"),
	(imaginaryPart, "Im"),
	(lngamma, "lgamma"),
	(product, "prod"),
	(realPart, "Re")
	}, (m2f, rf) ->
    installMethod(m2f, RObject, RFunction rf))

-- unary w/ options
scan({
	(truncate, "trunc")
	}, (m2f, rfstr) -> (
	rf := RFunction rfstr;
	installMethod(m2f, RObject, {} >> o -> x -> rf x)))

-- binary
scan({
	(symbol +, "+"),
	(symbol -, "-"),
	(symbol *, "*"),
	(symbol /, "/"),
	(symbol ^, "^"),
	(symbol %, "%%"),
	(symbol //, "%/%"),
	(symbol and, "&"),
	(symbol or, "|"),
	(symbol xor, "xor"),
	(symbol :, ":"),
	(symbol .., ":"),
	(symbol &, "bitwAnd"),
	(symbol |, "bitwOr"),
	(symbol ^^, "bitwXor"),
	(symbol <<, "bitwShiftL"),
	(symbol >>, "bitwShiftR"),
	(atan2, "atan2"),
	(Beta, "beta"),
	(binomial, "choose"),
	(round, "round") -- also unary
	}, (m2f, rfstr) -> (
	rf := RFunction rfstr;
	installMethod(m2f, RObject, RObject, rf);
	installMethod(m2f, RObject, Thing, rf);
	installMethod(m2f, Thing, RObject, rf)))

?? RObject := x -> if TYPEOF x > 0 then x

load "./RInterface/test.m2"
load "./RInterface/doc.m2"
