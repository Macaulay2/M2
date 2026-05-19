-- MRDI package for Macaulay2
-- Copyright (C) 2025-2026 Doug Torrance

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program; if not, see <https://www.gnu.org/licenses/>.

-- This package was originally written for use by the Macaulean project and its
-- development was funded by Renaissance Philantropy's AI for Math Fund.
-- https://www.renaissancephilanthropy.org/ai-for-math-fund

newPackage(
    "MRDI",
    Version => "0.1",
    Date => "April 25, 2026",
    Headline => "serializing algebraic data with .mrdi files",
    Authors => {
	{
	    Name => "Doug Torrance",
	    Email => "dtorrance@piedmont.edu",
	    HomePage => "https://webwork.piedmont.edu/~dtorrance"
	    }},
    PackageImports => {"JSON"},
    Keywords => {"System"})

export {
    -- methods
    "addLoadMethod",
    "addNamespace",
    "addSaveMethod",
    "loadMRDI",
    "saveMRDI",
    "validateMRDI",

    -- symbols
    "Namespace",
    "ToString",
    "UseID",
    }

importFrom(Core, {
	"noMethod",
	"nullf"})

------------
-- saving --
------------

-- universally unique identifiers
-- https://www.rfc-editor.org/rfc/rfc9562
uuidsByThing = new MutableHashTable
thingsByUuid = new MutableHashTable
pad0 = (n, s) -> concatenate((n - #s):"0", s)
randnibbles = k -> pad0(k, changeBase(random 2^(4*k), 16))
thingToUuid = x -> uuidsByThing#x ??= (
    i := concatenate(
	randnibbles 8, "-", randnibbles 4, "-4", randnibbles 3, "-",
	changeBase(8 + random 4, 16), randnibbles 3, "-", randnibbles 12);
    thingsByUuid#i = x;
    i)
uuidToThing = (i, f) -> thingsByUuid#i ??= (
    x := f();
    uuidsByThing#x = i;
    x)
isUuid = i -> match("^[0-9a-fA-F]{8}-([0-9a-fA-F]{4}-){3}[0-9a-fA-F]{12}$", i)

namespaces =  new MutableHashTable
loadMethods = new MutableHashTable

addNamespace = method()
addNamespace(String, String, String) := (ns, url, v) -> (
    namespaces#ns = {url, v};
    loadMethods#ns = new MutableHashTable;
    Thing#{ns, UseID} = false;)

addNamespace("Macaulay2", "https://macaulay2.com", version#"VERSION")
addNamespace("Oscar", "https://github.com/oscar-system/Oscar.jl", "1.6.0")

saveMRDI = method(
    Dispatch => Thing,
    Options => {
	FileName => null,
	ToString => true,
	Namespace => "Macaulay2"})
saveMRDI Thing := o -> x -> (
    if not namespaces#?(o.Namespace)
    then error("unknown namespace: ", o.Namespace);
    refs := new MutableHashTable;
    mrdi := toMRDI(o.Namespace, x, refs);
    r := (if o.ToString then toJSON else identity) merge(
	hashTable {
	    "_ns" => hashTable {
		o.Namespace => namespaces#(o.Namespace)},
	    if useID(o.Namespace, x) then "id" => thingToUuid x,
	    if #refs > 0 then "_refs" => new HashTable from refs},
	mrdi,
	(x, y) -> error "unexpected key collision");
    if o.FileName =!= null then o.FileName << r << endl << close;
    r)

-- low-level unexported function
-- input: ns: string (namespace)
--        x: the object to serialize
--        refs: mutable hash table (keys = uuids of refs)
-- output: hash table representing x (type & data only)
-- side effect: new refs are added to refs
-- use addSaveMethod to define for a given class
toMRDI = (ns, x, refs) -> (
    if (f := lookup({ns, saveMRDI}, class x)) === null
    then error noMethod({ns, saveMRDI}, x,)
    else f(x, refs))

useID = (ns, x) -> (
    if (u := lookup({ns, UseID}, class x)) === null
    then error noMethod({ns, UseID}, x,)
    else if not instance(u, Boolean)
    then error("expected ", {ns, UseID}, " for ", class x,
	" to be true or false")
    else u)

toMRDIorUuid = (ns, x, refs) -> (
    r := toMRDI(ns, x, refs);
    if useID(ns, x) then (
	i := thingToUuid x;
	refs#i = r;
	i)
    else r)

-- low-level unexported method
-- same interface as toMRDI, but attempts to separate out objects we'd like
-- to serialize from json-level objects that we're using to describe
-- other objects
processMRDI = method()
processMRDI(String, Thing, MutableHashTable) := toMRDIorUuid
processMRDI(String, String, MutableHashTable) := (ns, x, refs) -> x
processMRDI(String, Nothing, MutableHashTable) := (ns, x, refs) -> null
processMRDI(String, ZZ, MutableHashTable) := (ns, x, refs) -> toString x
processMRDI(String, List, MutableHashTable) := (ns, x, refs) -> (
    if class x === List then apply(x, y -> processMRDI(ns, y, refs))
    else toMRDIorUuid(ns, x, refs))
processMRDI(String, HashTable, MutableHashTable) := (ns, x, refs) -> (
    if class x === HashTable then applyValues(x, v -> processMRDI(ns, v, refs))
    else toMRDIorUuid(ns, x, refs))


addSaveMethod = method(Options => {
	UseID => false,
	Name => toString @@ class,
	Namespace => "Macaulay2"})

getType = method()
getType(Function, Thing) := (f, x) -> f x
getType(String,   Thing) := (s, x) -> s


addSaveMethod Type := o -> T -> (
    addSaveMethod(T, nullf, nullf, o))
addSaveMethod(Type, Function) := o -> (T, dataf) -> (
    addSaveMethod(T, nullf, dataf, o))
addSaveMethod(Type, Function, Function) := o -> (T, paramsf, dataf) -> (
    T#{o.Namespace, saveMRDI} = (x, refs) -> (
	if o.UseID then thingToUuid x; -- save uuid
	params := processMRDI(o.Namespace, paramsf x, refs);
	data := processMRDI(o.Namespace, dataf x, refs);
	hashTable {
	    "_type" => (
		if params =!= null then hashTable {
		    "name" => getType(o.Name, x),
		    "params" => params}
		else getType(o.Name, x)),
	    if data =!= null then "data" => data});
    T#{o.Namespace, UseID} = o.UseID;)

addSaveMethod(ZZ, identity)

addSaveMethod(Ring,
    R -> (
	if isMember(R, {ZZ, QQ}) then toString R
	else error "not implemented yet"))

addSaveMethod(QuotientRing,
    R -> (
	if isFinitePrimeField R then char R
	else error "not implemented yet"))

addSaveMethod(GaloisField,
    F -> hashTable {
	"char"   => F.char,
	"degree" => F.degree},
    UseID => true)

addSaveMethod(PolynomialRing,
    coefficientRing,
    R -> hashTable {
	"variables" => toString \ gens R},
    UseID => true)

mrdiCoefficient = method()
mrdiCoefficient ZZ := identity
mrdiCoefficient QQ := x -> {numerator x, denominator x}

mrdiListForm = f -> apply(listForm f,
    (mon, coeff) -> {mon, mrdiCoefficient coeff})

addSaveMethod(RingElement,
    ring,
    mrdiListForm,
    Name => "RingElement")

addSaveMethod(Ideal,
    ring,
    I -> apply(I_*, mrdiListForm))

addSaveMethod(Matrix, ring, A -> apply(entries A, row -> mrdiListForm \ row))

-------------
-- loading --
-------------

uuidsToCreate = new MutableHashTable

loadMRDI = method()
-- TODO: schema validation
loadMRDI String := loadMRDI @@ fromJSON
loadMRDI HashTable := r -> (
    ns := first keys r#"_ns";
    if not loadMethods#?ns then error("unknown namespace: ", ns);
    -- save info about refs we haven't created yet
    if r#?"_refs" then scanPairs(r#"_refs",
	(i, s) -> if not thingsByUuid#?i then uuidsToCreate#i = s);
    if r#?"id" then uuidToThing(r#"id", () -> fromMRDI(ns, r))
    else fromMRDI(ns, r))

-- unexported helper function
-- inputs: string (namespace) and object to de-serialize
-- outputs: a de-serialized M2 object
fromMRDI = method()
fromMRDI(String, HashTable) := (ns, r) -> (
    -- if it has a _type key, then it's an object to de-serialize
    if r#?"_type" then (
	(name, params) := (
	    if instance(r#"_type", HashTable)
	    then (r#"_type"#"name", r#"_type"#"params")
	    else (r#"_type", null));
	if not loadMethods#ns#?name then error ("unknown type: ", name);
	loadMethods#ns#name(
	    if params =!= null then fromMRDI(ns, params),
	    if r#?"data" then fromMRDI(ns, r#"data")))
    -- otherwise, de-serialize its values
    else applyValues(r, fromMRDI_ns))
fromMRDI(String, String) := (ns, s) -> (
    -- if the string is a uuid, then return the corresponding object
    if isUuid s then uuidToThing(s, () -> (
	    if uuidsToCreate#?s
	    then fromMRDI(ns, remove(uuidsToCreate, s))
	    else error("unknown uuid: ", s)))
    -- otherwise, just return the string
    else s)
fromMRDI(String, List) := (ns, x) -> apply(x, fromMRDI_ns)

-- input function takes two args: params (de-serialized) & data
addLoadMethod = method(Options => {Namespace => "Macaulay2"})
addLoadMethod(String, Function) := o -> (type, f) -> (
    if not loadMethods#?(o.Namespace)
    then error("unknown namespace: ", o.Namespace);
    loadMethods#(o.Namespace)#type = f)
addLoadMethod(List, Function) := o -> (types, f) -> (
    scan(types, type -> addLoadMethod(type, f, o)))

addLoadMethod("ZZ", (params, data) -> value data)
addLoadMethod("Ring", (params, data) -> (
	if data == "ZZ" then ZZ
	else if data == "QQ" then QQ
	else error "unknown ring"))
addLoadMethod("QuotientRing", (params, data) -> ZZ/(value data))
addLoadMethod("GaloisField", (params, data) -> (
	GF(value data#"char", value data#"degree")))
addLoadMethod("PolynomialRing", (params, data) -> (
	params[Variables => data#"variables"]))

mrdiToCoefficient = method(Dispatch => Type)
mrdiToCoefficient ZZ := R -> value
mrdiToCoefficient QQ := R -> a -> value a#0 / value a#1

mrdiToPolynomial = (R, f) -> (
    if #f == 0 then 0_R
    else sum(f, term -> times(
	    (mrdiToCoefficient coefficientRing R) term#1,
	    R_(value \ toList term#0))))
addLoadMethod("RingElement", (params, data) -> (
	mrdiToPolynomial(params, data)))
addLoadMethod("Ideal", (params, data) -> (
	ideal apply(data, mrdiToPolynomial_params)))
addLoadMethod("Matrix", (params, data) -> (
	matrix applyTable(data, mrdiToPolynomial_params)))

-- for debugging w/ "methods"
LoadMethod = new SelfInitializingType of List
net LoadMethod := lookup(net, Sequence)
locate LoadMethod := x -> locate loadMethods#(x#0#0)#(x#1)
code LoadMethod := code @@ locate

importFrom(Core, "previousMethodsFound")
oldmethods = lookup(methods, List)
methods List := x -> (
    if #x == 2 and x#1 === loadMRDI then (
	previousMethodsFound = new NumberedVerticalList from (
	    if loadMethods#?(x#0) then (
		apply(keys loadMethods#(x#0), k -> LoadMethod(x, k)))
	    ?? {}))
    else oldmethods x)

-----------
-- Oscar --
-----------

oscarRings = hashTable {
    ZZ => "ZZRing",
    QQ => "QQField",
    }
addSaveMethod(Ring,
    Name => R -> oscarRings#R ?? error "unknown ring",
    Namespace => "Oscar")

addSaveMethod(ZZ,
    x -> ZZ,
    toString,
    Name => "ZZRingElem",
    Namespace => "Oscar")

addSaveMethod(QQ,
    x -> QQ,
    x -> concatenate(toString numerator x, "//", toString denominator x),
    Name => "QQFieldElem",
    Namespace => "Oscar")

-- Oscar differentiates between univariate and multivariate polynomial rings,
-- but multivariate rings can have just 1 variable, so we just always use that
addSaveMethod(PolynomialRing,
    coefficientRing,
    R -> hashTable {"symbols" => toString \ gens R},
    Name => "MPolyRing",
    UseID => true,
    Namespace => "Oscar")

addSaveMethod(RingElement,
    ring,
    f -> apply(listForm f, mon -> {mon#0, mon#1}),
    Name => "MPolyRingElem",
    Namespace => "Oscar")

addLoadMethod("Base.Int", (params, data) -> value data, Namespace => "Oscar")
addLoadMethod("ZZRingElem",
    (params, data) -> value data,
    Namespace => "Oscar")
addLoadMethod("QQFieldElem",
    (params, data) -> (
	x := separate("//", data);
	if #x == 2 then value x#0 / value x#1
	else value x#0 / 1),
    Namespace => "Oscar")
addLoadMethod("String", (params, data) -> data, Namespace => "Oscar")
addLoadMethod("Float64", (params, data) -> value data, Namespace => "Oscar")
addLoadMethod("ZZRing", (params, data) -> ZZ, Namespace => "Oscar")
addLoadMethod("QQField", (params, data) -> QQ, Namespace => "Oscar")
addLoadMethod("FiniteField",
    (params, data) -> (
	if params =!= null then error "not implemented yet"
	else ZZ/(value data)),
    Namespace => "Oscar")
addLoadMethod({"PolyRing", "MPolyRing"},
    (params, data) -> (
	-- TODO: handled indexed variables, e.g., x[1], x[2], x[3]
	params[Variables => data#"symbols"]),
    Namespace => "Oscar")
addLoadMethod({"PolyRingElem", "MPolyRingElem"},
    (params, data) -> mrdiToPolynomial(params, data),
    Namespace => "Oscar")

----------------
-- validating --
----------------

-- https://www.oscar-system.org/schemas/mrdi.json

-- all JSON objects must have keys as strings
validateObject = x -> scanKeys(x, k -> (
	if not instance(k, String)
	then error("expected all keys to be strings, but got ", k)))

validateData = method()
validateData Thing := x -> error("invalid data: ", x)
validateData String := x -> null
validateData HashTable := x -> (
    validateObject x;
    scanKeys(x, k -> if not match("^[a-zA-Z0-9_]*", k)
	then error("expected an alphanumeric key, but got ", k)))
validateData List := x -> validateData \ x
-- TODO: validate polymake schema

validateMRDI = method()
validateMRDI Thing := x -> error("expected an object, but got ", x)
validateMRDI String := validateMRDI @@ fromJSON
validateMRDI HashTable := x -> (
    validateObject x;
    if not x#?"_type" then error "expected a '_type' key";
    if instance(x#"_type", String) then null
    else if instance(x#"_type", HashTable) then (
	validateObject x#"_type";
	if x#"_type"#?"name" then (
	    if not instance(x#"_type"#"name", String)
	    then error("expected value of 'name' to be a string"));
	if x#"_type"#?"params" then validateData x#"_type"#"params")
    else error("expected value of '_type' to be a string or object");
    scan({"_ns", "_refs"}, k -> (
	    if x#?k then (
		if not instance(x#k, HashTable)
		then error("expected value of '", k, "' to be an object");
		validateObject x#k)));
    if x#?"_refs" then scanPairs(x#"_refs", (k, v) -> (
	    if not isUuid k
	    then error("expected all keys of '_refs' to be UUID's, but got ", k);
	    validateMRDI v));
    )

-------------------
-- documentation --
-------------------

beginDocumentation()

doc ///
Key
  MRDI
Headline
  serialization using the mrdi file format
Description
  Text
    The @EM "MRDI"@ package provides tools for serializing and deserializing
    mathematical objects in Macaulay2 using the MRDI file format, a JSON-based
    format for storing and sharing results in computer algebra without losing
    accuracy.  The format was developed as part of the
    @HREF("https://www.mardi4nfdi.de/",
	"Mathematics Research Data Initiative (MaRDI)")@
    and is described in the paper:

    Antony Della Vecchia, Michael Joswig, and Benjamin Lorenz,
    @HREF("https://doi.org/10.1007/978-3-031-64529-7_25",
	"A FAIR file format for mathematical software")@,
    @EM "Mathematical software—ICMS 2024"@, 234–244, Lecture Notes in
    Comput. Sci., 14749, Springer, Cham.

    Each serialized object carries a namespace (@TT "_ns"@) identifying
    the originating software system and version, a type descriptor
    (@TT "_type"@) that may include recursive parameters, the actual
    data, and optionally a set of references (@TT "_refs"@) keyed by
    UUIDs.

    The package can serialize and deserialize integers, rings
    (@TO ZZ@, @TO QQ@, finite prime fields, Galois fields), polynomial rings,
    ring elements, ideals, and matrices.  It can also load and save objects
    using the OSCAR namespace, enabling interoperability with the
    @HREF("https://www.oscar-system.org/", "OSCAR")@ computer algebra system.

    The namespace mechanism also makes it possible to define custom
    serialization formats for exchanging data with other software systems.
  Example
    R = QQ[x,y,z,w]
    I = monomialCurveIdeal(R, {1,2,3})
    s = saveMRDI I
    loadMRDI s
Subnodes
  saveMRDI
  loadMRDI
  Namespace
  validateMRDI
///

doc ///
Key
  saveMRDI
  (saveMRDI, Thing)
  [saveMRDI, FileName]
  [saveMRDI, Namespace]
  [saveMRDI, ToString]
  ToString
Headline
  serialize a Macaulay2 object to MRDI JSON format
Usage
  saveMRDI x
Inputs
  x:Thing
    a Macaulay2 object to serialize (must have a save method
    registered via @TO addSaveMethod@)
  FileName => String
    if given, the JSON output is written to this file
  Namespace => String
    the namespace to use for serialization
  ToString => Boolean
    if @TO true@, then the output is a JSON string;
    if @TO false@, then the output is @ofClass HashTable@ representing
    the JSON structure
Outputs
  :{String, HashTable}
Description
  Text
    This function serializes a Macaulay2 object into a JSON string following
    the MRDI file format specification.  The output includes a namespace
    (@TT "_ns"@) identifying the software system and its version, a type
    descriptor (@TT "_type"@), and the data.

    For parametric types such as @TO PolynomialRing@,
    @TO RingElement@, @TO Ideal@, and @TO Matrix@, the type
    descriptor includes a @TT "params"@ field referencing the
    parent ring.  Rings and other objects marked with
    @TO UseID@ are assigned UUIDs so that multiple objects
    sharing the same ring refer to it by UUID rather than
    repeating its full description.
  Text
    We may serialize a simple integer.
  Example
    saveMRDI 5
  Text
    We can also serialize a polynomial ring element.
  Example
    R = QQ[x,y,z];
    f = x^2 + y*z - 3*x
    saveMRDI f
  Text
    Ideals are serialized similarly.
  Example
    I = ideal(x^2 - y, y^2 - z)
    saveMRDI I
  Text
    The output can be written directly to a file using the @TO FileName@ option.
  Example
    fn = temporaryFileName() | ".mrdi"
    saveMRDI(f, FileName => fn)
    removeFile fn
  Text
    The @TO Namespace@ option selects a different namespace for
    serialization, such as OSCAR.
  Example
    saveMRDI(5, Namespace => "Oscar")
  Text
    Setting @TT "ToString => false"@ returns the hash table
    representation instead of a JSON string.  This 
  Example
    saveMRDI(5, ToString => false)
  Text
    To see which types have built-in save methods for a given namespace,
    call @TO methods@ as follows.
  Example
    methods {"Macaulay2", saveMRDI}
    methods {"Oscar", saveMRDI}
  Text
    Additional types can be supported by calling @TO addSaveMethod@.
Caveat
  Not all Macaulay2 types have save methods defined.  Attempting
  to serialize an unsupported type will produce an error.
  Quotient rings other than finite prime fields are not yet
  supported.
Subnodes
  addSaveMethod
SeeAlso
  loadMRDI
///

doc ///
Key
  loadMRDI
  (loadMRDI, String)
  (loadMRDI, HashTable)
Headline
  deserialize a Macaulay2 object from MRDI format
Usage
  loadMRDI s
Inputs
  s:{String, HashTable}
    a JSON string in the MRDI file format or
    parsed JSON hash table (e.g., from @TO "JSON::fromJSON"@)
Outputs
  :Thing -- the deserialized Macaulay2 object
Description
  Text
    This method parses an MRDI-formatted JSON string (or hash table) and
    reconstructs the corresponding Macaulay2 object.  The namespace field
    (@TT "_ns"@) in the JSON determines which set of load methods is used for
    deserialization.

    This function handles the @TT "_refs"@ section of the JSON
    to reconstruct shared references via UUIDs.  For example,
    when an ideal and a ring element both refer to the same
    polynomial ring, the ring is constructed once and shared.
  Text
    A polynomial ring element can be round-tripped through the format.
  Example
    R = QQ[x,y,z,w];
    f = x^2 + y*z
    s = saveMRDI f
    g = loadMRDI s
    f === g
  Text
    The same works for ideals.
  Example
    I = monomialCurveIdeal(R, {1,2,3})
    s = saveMRDI I
    J = loadMRDI s
    I === J
  Text
    Objects can be loaded from a file as well using @TO get@.
  Example
    fn = temporaryFileName() | ".mrdi"
    saveMRDI(I, FileName => fn)
    J = loadMRDI get fn
    I === J
    removeFile fn
  Text
    A parsed hash table can also be passed directly, for instance
    when using @TT "ToString => false"@ with @TO saveMRDI@.
  Example
    h = saveMRDI(42, ToString => false)
    loadMRDI h
  Text
    The MRDI format supports cross-system interoperability.
    Objects serialized by the OSCAR computer algebra system
    can also be loaded.
  Example
    loadMRDI "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.5.0\"]},\"_type\":\"ZZRingElem\",\"data\":\"42\"}"
  Text
    To see which types have built-in load methods for a given namespace,
    call @TO methods@ as follows.
  Example
    methods {"Macaulay2", loadMRDI}
    methods {"Oscar", loadMRDI}
  Text
    Additional types can be supported by calling @TO addLoadMethod@.
Caveat
  If the JSON string references a namespace or type for which
  no load method has been registered, an error is produced.
  Use @TO addLoadMethod@ to register handlers for custom types
  and @TO addNamespace@ to register new namespaces.
Subnodes
  addLoadMethod
SeeAlso
  saveMRDI
///

doc ///
Key
  addSaveMethod
  (addSaveMethod, Type)
  (addSaveMethod, Type, Function)
  (addSaveMethod, Type, Function, Function)
  UseID
  [addSaveMethod, UseID]
  [addSaveMethod, Name]
  [addSaveMethod, Namespace]
Headline
  register a method for serializing a type to MRDI format
Usage
  addSaveMethod T
  addSaveMethod(T, dataFunc)
  addSaveMethod(T, paramsFunc, dataFunc)
Inputs
  T:Type
    the Macaulay2 type to be serialized
  dataFunc:Function
    a function that takes an object of type @TT "T"@ and
    returns the data to be stored in the @TT "data"@ field
    of the MRDI JSON
  paramsFunc:Function
    a function that takes an object of type @TT "T"@ and
    returns the parameter object (e.g., the coefficient ring
    of a polynomial ring), which will itself be serialized
    recursively
  UseID => Boolean
    if @TT "true"@, objects of this type are assigned UUIDs
    so they can be referenced rather than duplicated
  Name => Function
    a function (or string) that determines the type name string
    for the @TT "_type"@ field.  By default, the @TO class@ of
    each object (as a string) is used.
  Namespace => String
    the namespace to register this save method under
Description
  Text
    This function registers a serialization method for the given type so
    that @TO saveMRDI@ knows how to convert objects of that
    type into MRDI JSON format.

    The zero-argument form @TT "addSaveMethod T"@
    is for types with no data and no parameters (the MRDI
    output will contain only a @TT "_type"@ field).

    The one-argument form @TT "addSaveMethod(T, dataFunc)"@
    is for basic (non-parametric) types whose MRDI
    representation needs only a @TT "_type"@ name and
    @TT "data"@.  The @TT "dataFunc"@ receives the object and
    should return the data portion.

    The two-argument form @TT "addSaveMethod(T, paramsFunc, dataFunc)"@
    is for parametric types.  The @TT "paramsFunc"@ returns an
    object representing the type's parameter (which will be
    recursively serialized into the @TT "_type.params"@ field),
    and @TT "dataFunc"@ returns the data.

    The @TO UseID@ option causes objects of this type to be
    assigned RFC 9562 version 4 UUIDs upon serialization.
    This is important for types like polynomial rings
    that may be shared by many objects: the ring is stored
    once in the @TT "_refs"@ section and referenced by UUID
    elsewhere.

    The @TO Namespace@ option allows registering save methods
    for different namespaces.  This is how OSCAR serialization
    support is implemented alongside Macaulay2's own namespace.

    Here we register a save method for a custom namespace.
  Example
    addNamespace("MySystem", "https://example.com", "1.0")
    addSaveMethod(ZZ, identity, Name => "MyInt", Namespace => "MySystem")
    saveMRDI(42, Namespace => "MySystem")
SeeAlso
  saveMRDI
///

doc ///
Key
  addLoadMethod
  (addLoadMethod, String, Function)
  (addLoadMethod, List, Function)
  [addLoadMethod, Namespace]
Headline
  register a method for deserializing a type from MRDI format
Usage
  addLoadMethod(typeName, f)
Inputs
  typeName:{String, List}
    the type name as it appears in the @TT "_type"@ field
    of the MRDI JSON, or a list of strings to add multiple
    load methods at the same time
  f:Function
    a function @TT "(params, data) -> Thing"@ that
    reconstructs the object
  Namespace => String
    the namespace to register this method under
Description
  Text
    This methods registers a deserialization method so that @TO loadMRDI@
    knows how to reconstruct objects of a given type within a
    given namespace.

    The namespace allows the same file format to be used across
    different computer algebra systems.  The Macaulay2 namespace
    is @TT "\"Macaulay2\""@ and the OSCAR namespace is
    @TT "\"Oscar\""@.  Each namespace maintains its own type
    registry.  Use @TO addNamespace@ to register a new namespace
    before adding load methods to it.

    The loading function @VAR "f"@ receives two
    arguments:

    @UL {
	LI {TT "params", ": For parametric types (e.g., ", TO RingElement,
	    "), this is the already-deserialized parameter (e.g., the parent ",
	    "ring).  For non-parametric types (e.g., ", TO ZZ, "), this is ",
	    TO null, "."},
	LI {TT "data", ":  The contents of the ", TT "data", " field from the ",
	    "JSON, recursively deserialized."}}@

    The @TO List@ form allows registering the same function for
    multiple type names at once, which is useful when systems
    use different names for equivalent types.
  Text
    Here we register a load method for a custom namespace.
  Example
    addNamespace("MySystem", "https://example.com", "1.0")
    addLoadMethod("MyInt",
        (params, data) -> value data,
        Namespace => "MySystem")
    loadMRDI "{\"_ns\":{\"MySystem\":[\"https://example.com\",\"1.0\"]},\"_type\":\"MyInt\",\"data\":\"42\"}"
SeeAlso
  loadMRDI
///

doc ///
Key
  addNamespace
  (addNamespace, String, String, String)
Headline
  register a namespace for MRDI serialization
Usage
  addNamespace(ns, url, ver)
Inputs
  ns:String -- the namespace identifier
  url:String -- the URL associated with the namespace
  ver:String -- the version string for the namespace
Description
  Text
    This method registers a new namespace for use with @TO saveMRDI@ and
    @TO loadMRDI@.  A namespace must be registered before any save or load methods
    can be added to it using @TO addSaveMethod@ or @TO addLoadMethod@.

    The namespace, URL, and version are written into the
    @TT "_ns"@ field of the MRDI JSON output.

    The @TT "\"Macaulay2\""@ and @TT "\"Oscar\""@ namespaces
    are registered automatically when the package is loaded.
  Example
    addNamespace("MySystem", "https://example.com", "1.0")
    addLoadMethod("MyInt",
        (params, data) -> value data,
        Namespace => "MySystem")
    loadMRDI "{\"_ns\":{\"MySystem\":[\"https://example.com\",\"1.0\"]},\"_type\":\"MyInt\",\"data\":\"42\"}"
///

doc ///
Key
  validateMRDI
  (validateMRDI, Thing)
  (validateMRDI, String)
  (validateMRDI, HashTable)
Headline
  validate an MRDI JSON object against the format specification
Usage
  validateMRDI s
Inputs
  s:{String, HashTable}
    a JSON string or parsed JSON hash table to validate
Description
  Text
    Validates that a JSON string or hash table conforms to the
    @HREF("https://www.oscar-system.org/schemas/mrdi.json",
	"MRDI file format specification")@.
    This checks structural requirements such as:

    @UL {
	LI {"the presence of a ", TT "_type", " key"},
	LI {"that ", TT "_type", " is a string or an object with string-valued ", TT "name"},
	LI {"that ", TT "_ns", " and ", TT "_refs", " are objects (if present)"},
	LI {"that all keys of ", TT "_refs", " are valid UUIDs"},
	LI {"that referenced objects are themselves valid MRDI"}}@

    The function produces an error if validation fails and returns
    @TO null@ on success.
  Example
    s = saveMRDI 5
    validateMRDI s
///

doc ///
Key
  Namespace
Headline
  option specifying the namespace for MRDI serialization
Description
  Text
    A symbol used as an option to @TO addSaveMethod@,
    @TO addLoadMethod@, and @TO saveMRDI@ to specify which
    namespace to use.

    Different namespaces allow the same MRDI file format to
    represent objects from different computer algebra systems.
    The @TT "\"Macaulay2\""@ and @TT "\"Oscar\""@ namespaces
    are built in.  Custom namespaces can be registered with
    @TO addNamespace@.

    When passed to @TO saveMRDI@, this determines which set of
    save methods is used and what appears in the @TT "_ns"@ field
    of the JSON output.

    When passed to @TO addSaveMethod@ or @TO addLoadMethod@, this
    determines which namespace the method is registered under.
  Text
    The same object can be serialized under different namespaces.
  Example
    saveMRDI 5
    saveMRDI(5, Namespace => "Oscar")
Subnodes
  addNamespace
///


-----------
-- tests --
-----------

TEST ///
-- loadMRDI saveMRDI x should return x
checkMRDI = x -> (
    assert BinaryOperation(symbol ===, loadMRDI saveMRDI x, x);
    -- check FileName option
    fn := temporaryFileName() | ".mrdi";
    saveMRDI(x, FileName => fn);
    assert BinaryOperation(symbol ===, loadMRDI get fn, x);
    removeFile fn;
    -- check ToString option
    h := saveMRDI(x, ToString => false);
    assert instance(h, HashTable);
    assert BinaryOperation(symbol ===, loadMRDI h, x))
checkMRDI 5
checkMRDI ZZ
checkMRDI QQ
checkMRDI(ZZ/101)
checkMRDI GF(2, 3)
checkMRDI(QQ[x])
checkMRDI(QQ[x][y][z])
R = QQ[x,y,z,w]
I = monomialCurveIdeal(R, {1, 2, 3})
checkMRDI I_0
checkMRDI I
checkMRDI gens I
-- rational coefficients
checkMRDI((1/2)*x + (3/4)*y - 7/3)
checkMRDI ideal((1/2)*x^2 - (3/5)*y, (7/11)*x*y + 1)
checkMRDI matrix {{(1/2)*x, (3/4)*y}, {(5/6)*x*y, (7/8)*x^2}}
-- identity elements
checkMRDI 1_R
checkMRDI id_(R^3)
-- zero elements
checkMRDI 0_R
checkMRDI ideal 0_R
checkMRDI map(R^2, R^3, 0)
///

-* code to generate strings for the next test:

printWidth = 0
getFormattedMRDI = x -> (
    format replace(regexQuote version#"VERSION", "@VERSION@", saveMRDI x))
scan({
	5,
	ZZ,
	QQ,
	ZZ/101,
	GF(2, 3),
	QQ[x],
	QQ[x][y][z],
	(R = QQ[x,y,z,w]; I = monomialCurveIdeal(R, {1, 2, 3}); I_0),
	I,
	gens I
	}, x -> << "checkMRDI " << getFormattedMRDI x << endl)

*-

TEST ///
-- saveMRDI loadMRDI x should return x (possibly up to reordering of elements)
needsPackage "JSON"
checkMRDI = x -> (
    x = replace("@VERSION@", version#"VERSION", x);
    y := saveMRDI loadMRDI x;
    assert BinaryOperation(symbol ===, fromJSON x, fromJSON y))
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"ZZ\", \"data\": \"5\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"Ring\", \"data\": \"ZZ\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"Ring\", \"data\": \"QQ\"}"
checkMRDI "{\"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_type\": \"QuotientRing\", \"data\": \"101\"}"
checkMRDI "{\"_type\": \"GaloisField\", \"data\": {\"degree\": \"3\", \"char\": \"2\"}, \"id\": \"366eef8c-095b-4675-bc4c-c815a6706f52\", \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}}"
checkMRDI "{\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\"]}, \"id\": \"31292984-9503-4034-9a78-7badbc3d5710\", \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}}"
checkMRDI "{\"_type\": {\"params\": \"8731803f-89bd-4ff7-a599-79375b33cf4c\", \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"z\"]}, \"id\": \"27447205-6c41-4ed5-91ba-f7b96c0a65ce\", \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"8731803f-89bd-4ff7-a599-79375b33cf4c\": {\"_type\": {\"params\": \"81e005bb-a348-423a-a627-e96ff29a3597\", \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"y\"]}}, \"81e005bb-a348-423a-a627-e96ff29a3597\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\"]}}}}"
checkMRDI "{\"_type\": {\"params\": \"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\", \"name\": \"RingElement\"}, \"data\": [[[\"0\", \"0\", \"2\", \"0\"], [\"1\", \"1\"]], [[\"0\", \"1\", \"0\", \"1\"], [\"-1\", \"1\"]]], \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\", \"y\", \"z\", \"w\"]}}}}"
checkMRDI "{\"_type\": {\"params\": \"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\", \"name\": \"Ideal\"}, \"data\": [[[[\"0\", \"0\", \"2\", \"0\"], [\"1\", \"1\"]], [[\"0\", \"1\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"1\", \"1\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"2\", \"0\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"1\", \"0\"], [\"-1\", \"1\"]]]], \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\", \"y\", \"z\", \"w\"]}}}}"
checkMRDI "{\"_type\": {\"params\": \"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\", \"name\": \"Matrix\"}, \"data\": [[[[[\"0\", \"0\", \"2\", \"0\"], [\"1\", \"1\"]], [[\"0\", \"1\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"1\", \"1\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"0\", \"1\"], [\"-1\", \"1\"]]], [[[\"0\", \"2\", \"0\", \"0\"], [\"1\", \"1\"]], [[\"1\", \"0\", \"1\", \"0\"], [\"-1\", \"1\"]]]]], \"_ns\": {\"Macaulay2\": [\"https://macaulay2.com\", \"@VERSION@\"]}, \"_refs\": {\"cfaa114f-9d5a-44e1-abbb-a0ee2ca94fe4\": {\"_type\": {\"params\": {\"_type\": \"Ring\", \"data\": \"QQ\"}, \"name\": \"PolynomialRing\"}, \"data\": {\"variables\": [\"x\", \"y\", \"z\", \"w\"]}}}}"
///

TEST ///
-- save/load Oscar objects
checkMRDI = x -> assert BinaryOperation(symbol ===,
    loadMRDI saveMRDI(x, Namespace => "Oscar"), x)
checkMRDI ZZ
checkMRDI QQ
checkMRDI 5
checkMRDI(1/2)
R = ZZ[x,y,z,w]
checkMRDI R
checkMRDI random(3, R)

-- objects we can load but can't save yet
checkLoad = (x, mrdi) -> assert BinaryOperation(symbol ===, x, loadMRDI mrdi)
checkLoad("hello", "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":\"String\",\"data\":\"hello\"}")
checkLoad(3.14, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":\"Float64\",\"data\":\"3.14\"}")
checkLoad(ZZ/101, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":\"FiniteField\",\"data\":\"101\"}")
checkLoad(3/4, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":{\"name\":\"QQFieldElem\",\"params\":{\"_type\":\"QQField\"}},\"data\":\"3//4\"}")
checkLoad(7_QQ, "{\"_ns\":{\"Oscar\":[\"https://github.com/oscar-system/Oscar.jl\",\"1.6.0\"]},\"_type\":{\"name\":\"QQFieldElem\",\"params\":{\"_type\":\"QQField\"}},\"data\":\"7\"}")
///

TEST ///
-- save/load errors
checkError = (f, msg) -> (
    (ret, err) := trap f();
    assert Equation(msg, toString err))
checkError(
    () -> saveMRDI(5, Namespace => "NonExistent"),
    "unknown namespace: NonExistent")
checkError(
    () -> loadMRDI "{\"_ns\":{\"UnknownSystem\":[\"https://example.com\",\"1.0\"]},\"_type\":\"ZZ\",\"data\":\"5\"}",
    "unknown namespace: UnknownSystem")
checkError(
    () -> loadMRDI "{\"_ns\":{\"Macaulay2\":[\"https://macaulay2.com\",\"1.0\"]},\"_type\":\"NoSuchType\",\"data\":\"5\"}",
    "unknown type: NoSuchType")
///

TEST ///
-- custom namespace
addNamespace("TestSystem", "https://example.com/test", "0.1")
addSaveMethod(ZZ, identity, Name => "TestInt", Namespace => "TestSystem")
addLoadMethod("TestInt", (params, data) -> value data, Namespace => "TestSystem")
s = saveMRDI(99, Namespace => "TestSystem")
validateMRDI s
assert Equation(99, loadMRDI s)
needsPackage "JSON"
h = fromJSON s
assert(h#"_ns"#?"TestSystem")
///


TEST ///
-- validation
validateMRDI saveMRDI 5
validateMRDI saveMRDI ZZ
validateMRDI saveMRDI QQ
validateMRDI saveMRDI (ZZ/101)
validateMRDI saveMRDI GF(2, 3)
validateMRDI saveMRDI (QQ[x])
validateMRDI saveMRDI (QQ[x][y][z])
R = QQ[x,y,z,w]
I = monomialCurveIdeal(R, {1, 2, 3})
validateMRDI saveMRDI I_0
validateMRDI saveMRDI I
validateMRDI saveMRDI gens I
validateMRDI saveMRDI(ZZ, Namespace => "Oscar")
validateMRDI saveMRDI(QQ, Namespace => "Oscar")
validateMRDI saveMRDI(5, Namespace => "Oscar")
validateMRDI saveMRDI(1/2, Namespace => "Oscar")
R = ZZ[x,y,z,w]
validateMRDI saveMRDI(R, Namespace => "Oscar")
validateMRDI saveMRDI(random(3, R), Namespace => "Oscar")
///

TEST ///
-- validation errors
checkError = (mrdi, msg) -> (
    (ret, err) := trap validateMRDI mrdi;
    assert Equation(msg, toString err))
checkError(
    "{\"_ns\":{\"Macaulay2\":[\"https://macaulay2.com\",\"1.0\"]}}",
    "expected a '_type' key")
checkError(
    "{\"_type\":42}",
    "expected value of '_type' to be a string or object")
checkError(
    "{\"_type\":\"ZZ\",\"_refs\":{\"not-a-uuid\":{\"_type\":\"ZZ\"}}}",
    "expected all keys of '_refs' to be UUID's, but got not-a-uuid")
checkError(
    "{\"_type\":\"ZZ\",\"_ns\":\"bad\"}",
    "expected value of '_ns' to be an object")
checkError(
    "[1,2,3]",
    "expected an object, but got {1,2,3}")
///
