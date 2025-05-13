--		Copyright 1993-2002 by Daniel R. Grayson
-*
  Methods are the most common type of dynamic dispatch, along with Hooks.
  The method function is stored under the youngest mutable hash table in the
  method key sequence.
*-

needs "option.m2"

-----------------------------------------------------------------------------
-- Local variables
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

-- see lists.m2
all' := (L, p) -> not any(L, x -> not p x)

noMethErr := M -> concatenate("no method found for applying ", silentRobustString(45, 3, M), " to:");
printArgs := (i, arg, out) -> horizontalJoin("     argument ", i, " :  ",
    (if out then silentRobustNet else silentRobustNetWithClass)(60, 5, 3, arg));

noMethodSingle = (M, args, outputs) -> toString stack     (  noMethErr M, printArgs(" ", args, outputs) )
noMethod       = (M, args, outputs) -> toString stack join( {noMethErr M},
    if class args === Sequence and 0 < #args and #args <= 4 then apply(#args,
	i -> printArgs(toString (i+1), args#i, if outputs#?i then outputs#i else false))
    else {   printArgs(" ",            args,   args === ()) }) -- TODO: do better here, in what way?

-- TODO: what is this for exactly?
badClass := meth -> (i, args) -> (
     if i === -1 then error(silentRobustString(45,3,meth),": expected an output class, but got: ", silentRobustString(45,3,args))
     else error(silentRobustString(45,3,meth),": expected argument ",toString (i+1)," to be a type, but it was: ", silentRobustString(45,3,args#i)))

-- TODO: handle this in the interpreter, the same way that it is handled for function closures
chkopt0 := k -> if not ( instance(k, Symbol) ) then error "expected SYMBOL => VALUE"
chkopt  := o -> if not ( class o === Option and #o === 2 and instance(o#0, Symbol) ) then error "expected SYMBOL => VALUE"
chkopts := x -> if class x === OptionTable then scan(keys x,chkopt0) else if class x === List then scan(x,chkopt) else error "expected list of optional arguments"
badopts := x -> "options must be declared as a list, an option table, or as true for arbitrary options"

-----------------------------------------------------------------------------
-- MethodFunction* type declarations and basic constructors
-----------------------------------------------------------------------------

MethodFunction = new Type of CompiledFunctionClosure
MethodFunction.synonym = "method function"

-- TODO: are these two really useful?
MethodFunctionSingle = new Type of FunctionClosure
MethodFunctionSingle.synonym = "method function with a single argument"

MethodFunctionBinary = new Type of FunctionClosure
MethodFunctionBinary.synonym = "associative binary method function"

MethodFunctionWithOptions = new Type of FunctionClosure
MethodFunctionWithOptions.synonym = "method function with options"

-----------------------------------------------------------------------------
-- helpers for method
-----------------------------------------------------------------------------
-- from interpreter:
-- newmethod1
-- newmethod1234c

notImplemented = x -> error concatenate between(" ", splice {x, "not implemented yet"})

-----------------------------------------------------------------------------

-- see fold.m2, where this is turned into a method
-- TODO: add Left associative vs Right associative methods
foldL = (f, x, L) -> (scan(L, y -> x = f(x, y)); x)

-- TODO: combine these, if it doesn't hurt recursion limits
-- Common code for methods without options
binaryCaller := (M, key, args, outputs) -> (
    if (f := lookup key) =!= null then f args else error noMethod(M, args, outputs))
-- Common code for methods with options
binaryCaller' := (M, key, args, outputs, dispatcher) -> (
    if (f := lookup key) =!= null then dispatcher f else error noMethod(M, args, outputs))

BinaryNoOptions := outputs -> (
    -- This type is essentially the same as SingleNoOption: it installs a method
    -- for Sequence with dispatching done in top level instead of the interpreter
    -- TODO: this implementation cuts recursion depth by more than 6, do better!
    dispatchBy := if outputs === true then identity else class;
    methodFunction := newmethod1(args -> error noMethod(methodFunction, args, outputs), outputs, MethodFunctionBinary);
    -- Note: these methods may be overriden in order to implement
    -- specializations for handling more than two objects at once
    methodFunction List     :=
    methodFunction Sequence := args -> (
	-- Common code for every associative method without options
	if #args == 0 then return binaryCaller(methodFunction, 1 : methodFunction, (), outputs);
	if #args == 1 and (f := lookup(methodFunction, dispatchBy args#0)) =!= null then return f(args#0);
	-- TODO: a rudimentary caching of the lookup call here would be a significant benefit
	binaryLookup := (x, y) -> binaryCaller(methodFunction, (methodFunction, dispatchBy x, dispatchBy y), (x, y), outputs);
	foldL(binaryLookup, args#0, drop(args, 1)));
    methodFunction)
BinaryWithOptions := (opts, outputs) -> (
    -- chkopts opts;
    if instance(opts, List) then opts = new OptionTable from opts;
    dispatchBy := if   outputs === true then identity else class;
    dispatcher := if      opts === true then ((o, args) -> if #o === 0 then (f -> f args) else (f -> f(o, args)))
    else if instance(opts, OptionTable) then ((o, args) -> (f -> (f o) args))
    else error badopts opts;
    -- TODO: this can be simplified when https://github.com/Macaulay2/M2/issues/1878 is fixed
    functionClosure := methodFunction := opts >> o -> args -> (
	-- Common code for every associative method with options
	-- this is essentially a method installed on (methodFunction, VisibleList)
	if not instance(args, VisibleList) then args = 1:args;
	if #args == 0 then return binaryCaller'(methodFunction, 1 : methodFunction, (), outputs, dispatcher(o, ()));
	if #args == 1 and (f := lookup(methodFunction, dispatchBy args#0)) =!= null then return (dispatcher(o, args#0)) f;
	-- Note: specializations for simultaneous computation may be implemented
	-- by installing method functions on types that inherit from VisibleList
	if (f = lookup(methodFunction, class args)) =!= null and f =!= functionClosure then return (dispatcher(o, args)) f;
	-- TODO: a rudimentary caching of the lookup call here would be a significant benefit
	binaryLookup := (x, y) -> binaryCaller'(methodFunction, (methodFunction, dispatchBy x, dispatchBy y), (x, y), outputs, dispatcher(o, (x, y)));
	foldL(binaryLookup, args#0, drop(args, 1)));
    methodFunction = new MethodFunctionBinary from methodFunction;
    -- Note: these methods may be overriden in order to implement
    -- specializations for handling more than two objects at once
    installMethod(methodFunction, List,     functionClosure);
    installMethod(methodFunction, Sequence, functionClosure);
    methodFunction)

-----------------------------------------------------------------------------

-- Common code for methods with options or other special calling routines
singleCaller := (M, key, args, outputs, dispacher) -> (
    if (f := lookup key) =!= null then dispacher f else error noMethodSingle(M, args, outputs))

SingleNoOptions := outputs -> (
    -- TODO: this implementation cuts recursion depth in half
    -- TODO: make the debugger print the line that called the method instead of this line
    methodFunction := newmethod1(args -> error noMethodSingle(methodFunction, args, outputs), outputs, MethodFunctionSingle))
SingleWithOptions := (opts, outputs) -> (
    -- TODO: https://github.com/Macaulay2/M2/issues/1878
    -- chkopts opts;
    if instance(opts, List) then opts = new OptionTable from opts;
    dispatchBy := if   outputs === true then identity else class;
    dispatcher := if      opts === true then ((o, arg) -> if #o === 0 then (f -> f arg) else (f -> f(o, arg)))
    else if instance(opts, OptionTable) then ((o, arg) -> (f -> (f o) arg))
    else error badopts opts;
    methodFunction := opts >> o -> arg -> (
	-- Common code for every method with options and a single argument
	singleCaller(methodFunction, (methodFunction, dispatchBy arg), arg, outputs, dispatcher(o, arg)));
    methodFunction = new MethodFunctionSingle from methodFunction)

-----------------------------------------------------------------------------

MultipleArgsWithOptions := (methopts,opts,outputs) -> (
     if instance(opts,List) then opts = new OptionTable from opts;
     local innerMethodFunction;
     methodFunction := new MethodFunctionWithOptions from (opts >> o -> arg -> innerMethodFunction(o,arg));
    -- TODO: this implementation cuts recursion depth by 3!
     innerMethodFunction = newmethod1234c(
	  methodFunction,
	  args -> error noMethod(methodFunction,args,outputs),
	  badClass methodFunction,
	  outputs,
	  opts =!= true
	  );
     methodFunction)
MultipleArgsWithOptionsGetMethodOptions := meth -> (frames (frames meth)#0#1)#0#0 -- this recovers the value of methopts

MultipleArgsNoOptions := (methopts,outputs) -> (
    -- Note: this implementation is perfectly tuned for recursion!
     methodFunction := newmethod1234c(
	  MethodFunction,
	  args -> error noMethod(methodFunction,args,outputs),
	  badClass methodFunction,
	  outputs,
	  null
	  );
     methodFunction)
MultipleArgsNoOptionsGetMethodOptions := meth -> (frames (frames meth)#0#1)#0#0

-----------------------------------------------------------------------------

-- TODO: also support return type polymorphism
-- https://github.com/Macaulay2/M2/issues/1375
setReturnTypes := (methodFunction, returnType) -> (
    if returnType === Thing       then Thing else
    if instance(returnType, Type) then typicalValues#methodFunction = returnType
    else error("expected typical value ", toString returnType, " to be a type"))

getReturnTypes := methodFunction -> if typicalValues#?methodFunction then typicalValues#methodFunction

-----------------------------------------------------------------------------
-- method
-----------------------------------------------------------------------------
-- also see methods in code.m2
-- TODO: https://github.com/Macaulay2/M2/issues/1690
-- TODO: https://github.com/Macaulay2/M2/issues/62

methodDefaults := new OptionTable from {
    Binary       => false,
    -- Thing or Type (for single arg dispatch) or list of Thing or Type (for multiple inheritance)
    -- if a list, it's assumed to continue with the default, which is Thing ...
    Dispatch     => {Thing, Thing, Thing, Thing},
    Options      => null,
    TypicalValue => Thing,
    }

method = methodDefaults >> opts -> args -> (
     if args =!= () then error "expected only optional arguments";
     chk := c -> c === Thing or c === Type;
     if not if instance(opts.Dispatch,List) then all'(opts.Dispatch, chk) else chk opts.Dispatch
     then error "expected Dispatch option to be: Thing, Type, or a list of those";
     singleDispatch := chk opts.Dispatch;
     outputs := if not singleDispatch then apply(opts.Dispatch, c -> c === Type) else opts.Dispatch === Type;
     saveCurrentFileName := currentFileName;		    -- for debugging
     saveCurrentRowNumber := currentRowNumber();	    -- for debugging
     methodFunction := (
        if opts.Options === null then (
	    if opts.Binary    then BinaryNoOptions(outputs) else
	    if singleDispatch then SingleNoOptions(outputs)
	    else MultipleArgsNoOptions(opts, outputs))
	else (
	    if opts.Binary    then BinaryWithOptions(opts.Options, outputs) else
	    if singleDispatch then SingleWithOptions(opts.Options, outputs)
	    else MultipleArgsWithOptions(opts, opts.Options, outputs)));
    setReturnTypes(methodFunction, opts.TypicalValue);
    methodFunction)

-- get the options used when a method was declared
-- TODO: doesn't work for MethodFunctionSingle, MethodFunctionBinary
methodOptions = method(TypicalValue => OptionTable)
methodOptions Function := methodOptions Symbol := f -> null
methodOptions MethodFunctionWithOptions := MultipleArgsWithOptionsGetMethodOptions
methodOptions MethodFunction := MultipleArgsNoOptionsGetMethodOptions
methodOptions Command := f -> methodOptions f#0

-- get the options of function, method function, or various other objects
options = method(Dispatch => Thing, TypicalValue => OptionTable)
options Command  := C   -> options C#0
options Sequence := key -> (
    if (m := lookup key) =!= null then options m
    else error("no method installed for ", toString key))
options List := L -> apply(L, options)

oftab := new HashTable from {
    -- MethodFunctionWithOptions
    functionBody(method(Options => {}))                    => f -> notImplemented(),
    functionBody(method(Options => {}, Dispatch => Thing)) => f -> notImplemented(),
    -- FunctionClosure, MethodFunctionSingle, MethodFunctionBinary
    functionBody(  {} >> identity)                         => f -> first frame f,
    -- TODO: changing to true fixes https://github.com/Macaulay2/M2/issues/1881
    -- but introduces a different bug
    functionBody(true >> identity)                         => f -> null,
    }

-- TODO: this should return either an OptionTable or true, if any option is accepted
options Function := f -> if oftab#?(fb := functionBody f) then oftab#fb f

-----------------------------------------------------------------------------
-- Install various generic methods
-----------------------------------------------------------------------------

setupMethods := (args, symbols) -> (
     scan(symbols, n -> (
	  if value' n =!= n then error concatenate("symbol ",toString n," redefined");
	  f := method args;
	  globalAssignFunction(n,f);
	  n <- f;
	  )))

-- TODO: move and set the typical value of cokernel, coimage, comodule, image, module
setupMethods((), { 
	  entries, baseName, borel, gcdCoefficients,
	  diff, diff', contract, contract', isMember,
	  target, source,
	  getChangeMatrix, cover, coverMap, super, terms,
	  cokernel, coimage, comodule, image, someTerms, scanKeys, scanValues,
	  substitute, ambient, remainder, quotientRemainder, remainder', quotientRemainder',
	  coefficients, monomials, size, sum, product, nullhomotopy, module, raw,
	  content, leadTerm, leadCoefficient, leadMonomial, components,
	  assign, realPart, imaginaryPart, conjugate,
	  relations, inverse, numeric, numericInterval, floor, ceiling, round, degree, multidegree,
	  presentation, dismiss, precision, 
	  norm, clean, fraction, part,
	  hasEngineLinearAlgebra, nullSpace,
      isBasicMatrix, basicDet, basicInverse, basicKernel, basicRank, basicSolve, basicRankProfile,
      minimize
	  })

gradedModule = method(Dispatch => Thing)

assert = method()
assert Thing := x -> assert' x

use = method(Dispatch => Thing)
use Thing := identity

dual = method(Options => true)

default = method()
--default Type := (X) -> (
--     m := lookup(X,symbol default);
--     if m === null then error "no method found";
--     m ())

random = method(Options => {
	  MaximalRank => false,
	  Density => 1.,
	  UpperTriangular => false,
	  Height => 10
	  })
random Type := opts -> (X) -> (
     m := lookup(symbol random,X);
     if m === null then error "no method found";
     (m opts) X)

generators = method(
     Options => {
	  CoefficientRing => null			    -- used just for rings, default is most recent coefficient ring, else dig down
	  }
     )

minimalPresentation = method(
     Options => {
	  Exclude => {}
	  })
prune = method(
     Options => {
	  Exclude => {}
	  })
debug = method()
status = method (
     Options => new OptionTable from {
     	  TotalPairs => true,
     	  PairsRemaining => false,
     	  Monomials => false
     	  })

-- sort
-- TODO: see sortBy in classes.m2
-- cf. https://github.com/Macaulay2/M2/issues/1154
sort = method(
    Options => {
	DegreeOrder   => null,     -- used to be Ascending
	MonomialOrder => Ascending
	})
rsort       = method(Options => options sort)
sortColumns = method(Options => options sort)


matrix = method (
     Options => {
     	  Degree => null
     	  })
map = method(
     Options => {
     	  Degree => null,
     	  DegreeMap => null,
     	  DegreeLift => null
     	  })

setupMethods(Dispatch => Thing, {transpose} )
setupMethods(TypicalValue => Boolean,
     {isBorel, isWellDefined, isInjective, isSurjective, isUnit,
      isSubset, isHomogeneous, isField
	  })
setupMethods(TypicalValue => ZZ, {
	binomial, char, degreeLength, dim,
	numgens, numColumns, numRows, pdim, rank})

degrees = method(TypicalValue => List)
length = method(TypicalValue => ZZ, Dispatch => Thing)
codim = method( Options => true )

-- defined in d/actors4.d
format' := format
format = method(Dispatch => Thing, TypicalValue => String)
format RR :=
format CC :=
format String   := String => x -> format' x
format Sequence := String => s -> format' s
protect symbol format

-- use /// around strings w/ backslashes
formatNoEscaping = x -> (
    if match("\\\\", x) then concatenate("/// ", x, " ///")
    else format x)

toString = method(Dispatch => Thing, TypicalValue => String)
toString Thing := simpleToString			    -- if all else fails...
toString String := identity
toString Symbol := simpleToString

toExternalString = method(Dispatch => Thing, TypicalValue => String)
toExternalString Keyword := s -> concatenate("symbol ", simpleToString s)
toExternalString Symbol := s -> (
     n := simpleToString s;
     if not isGlobalSymbol n then error("can't convert local symbol or invisible global symbol ",s," to external string");
     if getGlobalSymbol n =!= s then (
	  syns := findSynonyms s;
	  if syns#?0 then n = syns#0
	  else error("can't convert symbol ",s," to external string because it is shadowed by ", getGlobalSymbol n, " and there is no synonym")
	  );
     if value' s === s then n else concatenate("symbol ", n))
toExternalString Boolean := simpleToString
toExternalString Nothing := simpleToString

toExternalString Thing := x -> (
     if hasAttribute(x,ReverseDictionary) then return toString getAttribute(x,ReverseDictionary);
     error("can't convert anonymous object of class ",toString class x," to external string"))

setupMethods(Dispatch => Thing, {max,min,directSum,vars})
net = method(Dispatch => Thing, TypicalValue => Net)
factor = method( Options => { } )

cohomology = method( Options => { 
	  Degree => 0		  -- for local cohomology and sheaf cohomology
	  } )
homology = method( Options => { } )

width  = method(TypicalValue => ZZ)
height = method(TypicalValue => ZZ)
depth  = method(TypicalValue => ZZ)

width File := fileWidth
height File := fileHeight

width Net := netWidth
height Net := netHeight
depth Net := netDepth
length Net := n -> #n

width String := stringWidth
height String := s -> 1
depth String := s -> 0

-----------------------------------------------------------------------------

toList = method(Dispatch => Thing)
toList BasicList := toList Set := toList String := toList Pseudocode := List => toList1

-----------------------------------------------------------------------------

oldflatten := flatten
erase symbol flatten
flatten = method(Dispatch => Thing)
flatten VisibleList := VisibleList => oldflatten

-----------------------------------------------------------------------------

dictionary = method()
dictionary Keyword := s -> Core.Dictionary
dictionary Symbol := s -> (				    -- eventually every symbol will know what dictionary it's in, perhaps
     n := toString s;
     scan(dictionaryPath, d -> if d#?n and d#n === s then break d))
dictionary Thing := x -> if hasAttribute(x,ReverseDictionary) then dictionary getAttribute(x,ReverseDictionary)

-----------------------------------------------------------------------------
value = method()
value Symbol := value Pseudocode := value'		    -- compiled code
value String := x -> value' x
value Nothing := x -> null
-----------------------------------------------------------------------------

scanValues(HashTable,Function) := (x,f) -> scanPairs(x, (k,v) -> f v)

scanKeys(HashTable,Function) := (x,f) -> scanPairs(x, (k,v) -> f k)
scanKeys(Database,Function) := (x,f) -> (
     	  s := firstkey x;
     	  while s =!= null do (
	       f s;
	       s = nextkey x;
	       ))

-- TODO: eventually move this to lists.m2
select' = select
select = method(Options => true)
select(ZZ,            Function) := List      => {} >> o -> select'
select(ZZ, BasicList, Function) := BasicList => {} >> o -> select'
select(    BasicList, Function) := BasicList => {} >> o -> select'
select(    BasicList, Type)     := BasicList => {} >> o -> (L, T) -> select(L, e -> instance(e, T))
-- two more methods installed in regex.m2

selectKeys = method()
selectKeys(ZZ, HashTable, Function) := HashTable => (n, x, f) -> (
    selectPairs(n, x, (k, v) -> f k))
selectKeys(HashTable, Function) := HashTable => (x, f) -> (
    selectPairs(x, (k, v) -> f k))

selectValues = method()
selectValues(ZZ, HashTable, Function) := HashTable => (n, x, f) -> (
    selectPairs(n, x, (k, v) -> f v))
selectValues(HashTable, Function) := HashTable => (x, f) -> (
    selectPairs(x, (k, v) -> f v))

select(ZZ, HashTable, Function) := HashTable => {} >> o -> lookup(
    selectValues, ZZ, HashTable, Function)
select(HashTable, Function) := HashTable => {} >> o -> lookup(
    selectValues, HashTable, Function)

select(ZZ, Set, Function) := Set => {} >> o -> lookup(
    selectKeys, ZZ, HashTable, Function)
select(Set, Function) := Set => {} >> o -> lookup(
    selectKeys, HashTable, Function)

oldnumerator := numerator
erase symbol numerator
numerator = method()
numerator QQ := oldnumerator

olddenominator := denominator
erase symbol denominator
denominator = method()
denominator QQ := olddenominator

toExternalString Option := z -> concatenate splice (
     if precedence z > precedence z#0 then ("(",toExternalString z#0,")") else toExternalString z#0,
     " => ",
     if precedence z > precedence z#1 then ("(",toExternalString z#1,")") else toExternalString z#1
     )
toString Option := z -> concatenate splice (
     if precedence z > precedence z#0 then ("(",toString z#0,")") else toString z#0,
     " => ",
     if precedence z > precedence z#1 then ("(",toString z#1,")") else toString z#1
     )

ultimate = method()
ultimate(Function,Thing) := (f,x) -> (
     while try (ox := x; x = f x; ox =!= x) else false do ();
     x)

between = method()
between(Thing,VisibleList) := List => (m,v) -> mingle(v,#v-1:m)

length Dictionary := s -> #s
length String := s -> #s
length VisibleList := s -> #s

Nothing == Nothing := Boolean => (x,y) -> x === y			    -- actually, x and y must both be "null"!

-- installation of assignment methods
installAssignmentMethod = method()
installAssignmentMethod(Symbol,HashTable,HashTable,Option) := (op,X,Y,o) -> (
     (typ,f) := toSequence o;
     if not instance(f,Function) then error "expected assignment method to be a function";
     if numparms f =!= 3 and numparms f =!= -1 then error "expected assignment method to be a function of 3 arguments";
     installMethod((op,symbol =),X,Y,o))
installAssignmentMethod(Symbol,HashTable,HashTable,Function) := (op,X,Y,f) -> (
     if numparms f =!= 3 and numparms f =!= -1 then error "expected assignment method to be a function of 3 arguments";
     installMethod((op,symbol =),X,Y,f))

installAssignmentMethod(Symbol,HashTable,Option) :=  (op,Y,o) -> (
     (typ,f) := toSequence o;
     if not instance(f,Function) then error "expected assignment method to be a function";
     if numparms f =!= 2 and numparms f =!= -1 then error "expected assignment method to be a function of 2 arguments";
     installMethod((op,symbol =),Y,o))
installAssignmentMethod(Symbol,HashTable,Function) := (op,Y,f) -> (
     if numparms f =!= 2 and numparms f =!= -1 then error "expected assignment method to be a function of 2 arguments";
     installMethod((op,symbol =),Y,f))

binaryOperators   = join(fixedBinaryOperators,    flexibleBinaryOperators, augmentedAssignmentOperators)
prefixOperators   = join(fixedPrefixOperators,    flexiblePrefixOperators)
postfixOperators  = join(fixedPostfixOperators,   flexiblePostfixOperators)
flexibleOperators = join(flexibleBinaryOperators, flexiblePrefixOperators, flexiblePostfixOperators, augmentedAssignmentOperators)
fixedOperators    = join(fixedBinaryOperators,    fixedPrefixOperators,    fixedPostfixOperators)
allOperators      = join(fixedOperators, flexibleOperators)

scan(flexibleBinaryOperators, op -> (
	  installAssignmentMethod(op, Type, Type, (X,Y,am) -> installAssignmentMethod(op, X, Y, am));
	  undocumented' ((op, symbol =), Type, Type);
	  ))
scan(flexiblePrefixOperators, op -> (
	  installAssignmentMethod(op, Type, (X,am) -> installAssignmentMethod(op, X, am));
	  undocumented' ((op, symbol =), Type);
	  ))
scan(flexiblePostfixOperators, op -> (
	  installAssignmentMethod(op, Type, (X,am) -> installAssignmentMethod(op, X, am));
	  undocumented' ((op, symbol =), Type);
	  ))
-----------------------------------------------------------------------------
-- helper functions usable in documentation
-----------------------------------------------------------------------------

foo := method(Options => {})
foodict := first localDictionaries foo
---- we can get into an infinite loop by doing this: (it's like printing the contents of a mutable hash table
-- codeHelper#(functionBody value foodict#"f") = g -> {
--      ("-- method functions:", code methods value (first localDictionaries g)#"methodFunction")
--      -- ("-- option table opts:", value (first localDictionaries g)#"opts")
--      }
bar := lookup(foo,Sequence)

-----------------------------------------------------------------------------
-- hooks
-----------------------------------------------------------------------------
-- also see hooks in code.m2
-- TODO: get this to work with lookup and flagLookup
-- TODO: get this to work on HashTables
-- TODO: get this to work with codeHelper
-- TODO: https://github.com/Macaulay2/M2/issues/1153

protect symbol Hooks
protect symbol HookAlgorithms
protect symbol HookPriority

-- hooks not bound to a type or hash table are stored here
GlobalHookStore = new MutableHashTable

getHookStore = (key, create) -> (
    -- retrieve (or create) the mutable hash table of Hooks based on a method key
    -- TODO: drop is needed because of a bug in youngest; see https://github.com/Macaulay2/M2/issues/1610
    obj := youngest drop(key, 1);
    store :=
    if instance(obj, MutableHashTable) then ( if       obj.?Hooks then       obj.Hooks else if create then       obj.Hooks = new MutableHashTable ) else
    if instance(obj, HashTable)        then ( if obj.cache.?Hooks then obj.cache.Hooks else if create then obj.cache.Hooks = new MutableHashTable );
    -- TODO: currently youngest only returns mutable hash tables, so the line above never occurs,
    -- but we keep it because eventually youngest should work for hash tables M as well, probably
    -- by looking at M.cache.timestamp. In particular, this would allow putting hooks on modules.
    if store === null then GlobalHookStore else store)

addHook = method(
    Options => {
	Strategy => null
	-- Priority, Description, ...?
	}
    )
addHook(Symbol,                  Function) := opts -> (key,        hook) -> addHook(1:key,                        hook, opts)
addHook(Sequence,                Function) := opts -> (key,        hook) -> addHook(getHookStore(key, true), key, hook, opts)
addHook(MutableHashTable, Thing, Function) := opts -> (store, key, hook) -> (
    -- this is the hashtable of Hooks for a specific key, which stores HookAlgorithms and HookPriority
    store#key ??= new MutableHashTable from {
	HookAlgorithms => new MutableHashTable, -- a mutable hash table "strategy key" => "strategy code"
	HookPriority   => new MutableList};     -- a mutable list of strategy keys, in order
    store = store#key;
    ind := #store.HookPriority; -- index to add the hook in the list; TODO: use Priority to insert in the middle?
    alg := if opts.Strategy =!= null then opts.Strategy else ind;
    if not store.HookAlgorithms#?alg then
    store.HookPriority#ind = alg;
    store.HookAlgorithms#alg = hook)

-- tracking debugInfo
threadLocal infoLevel
pushInfoLevel :=  n -> (
    if infoLevel === null then infoLevel = -1;
    infoLevel = infoLevel + n; n)
popInfoLevel  := (n, s) -> (infoLevel = infoLevel - n; s)

debugHooksLevel = debugLevel

-- This function is mainly used by runHooks, printing a line like this:
 -- (quotient,Ideal,Ideal) with Strategy => Monomial from -*Function[../../Macaulay2/packages/Saturation.m2:196:30-205:82]*-
debugInfo = (func, key, strategy, infoLevel) -> if debugHooksLevel > infoLevel then printerr(
    toString key, if strategy =!= null then (" with Strategy => ", toString strategy), " from ", toString func)

-- run a single hook
runHook := (hook, key, alg, args, opts) -> (
    pushInfoLevel 1;
    debugInfo(hook, key, alg, infoLevel);
    popInfoLevel(1, if options hook === null then hook(args) else (
	    hookOpts := select(keys options hook, k -> opts#?k) / (k -> k => opts#k);
	    hook(args, new OptionTable from hookOpts))))

runHooks = method(Options => true)
runHooks(Symbol,                  Thing) := true >> opts -> (key,        args) -> runHooks(1:key,                         args, opts)
runHooks(Sequence,                Thing) := true >> opts -> (key,        args) -> runHooks(getHookStore(key, false), key, args, opts)
runHooks(MutableHashTable, Thing, Thing) := true >> opts -> (store, key, args) -> (
    store = if store#?key then store#key else (
	if debugHooksLevel > 1 then printerr("runHooks: no hooks installed for ", toString key); return );
    alg := if opts.?Strategy then opts.Strategy;
    type := class alg;
    -- if Strategy is not given, run through all available hooks
    if alg === null then scan(reverse store.HookPriority, alg -> (
	    result := runHook(store.HookAlgorithms#alg, key, alg, args, opts ++ { Strategy => alg });
	    if not instance(result, Nothing) then break result)) else
    -- if Strategy is given, and it is among the known strategies, run only that hook
    if store.HookAlgorithms#?alg  then runHook(store.HookAlgorithms#alg,  key, alg,  args, opts) else
    -- otherwise, if the class of alg is a known strategy, run only that hook
    -- TODO: implement reverse lookup to find strategy installed under ancestors of type?
    if store.HookAlgorithms#?type then runHook(store.HookAlgorithms#type, key, type, args, opts) else
    -- otherwise, give an error with the list of possible strategies
    error("unrecognized Strategy => '", toString alg, "' for ", toString key, newline,
	"  available strategies are: ", demark_", " \\ toExternalString \ new List from store.HookPriority))

-- helper for hookifying methods
-- runs the hooks, if none succeed, runs the default algorithm f
tryHooks = (key, args, f) -> if (c := runHooks(key, args)) =!= null then c else f args

-- and keys
protect QuotientRingHook

-----------------------------------------------------------------------------
-- stashing or caching computed values for future reference in functions that take a mutable hash table as input
-----------------------------------------------------------------------------

-- deprecated, but kept for backwards compatibility
cacheValue = key -> f -> x -> ( try x.cache else x.cache = new CacheTable )#key ??= f(x)
stashValue = key -> f -> x -> x#key ??= f(x)

codeHelper#(functionBody (cacheValue null) null) = g -> {
     ("-- function f:", value (first localDictionaries g)#"f")
     }
codeHelper#(functionBody (stashValue null) null) = g -> {
     ("-- function f:", value (first localDictionaries g)#"f")
     }

-- helper for hookifying and caching methods
-- if a cached value isn't found on X, runs the hooks, if none succeed, runs the default algorithm f
-- TODO: simplify usage
cacheHooks = (ckey, X, mkey, args, f) -> (X -> X.cache#ckey ??= tryHooks(mkey, args, f)) X

-----------------------------------------------------------------------------
-- hypertext conversion

html = method(Dispatch => Thing, TypicalValue => String)
markdown = method(Dispatch => Thing, TypicalValue => String)
mathML = method(Dispatch => Thing, TypicalValue => String)
tex = method(Dispatch => Thing, TypicalValue => String)
texMath = method(Dispatch => Thing, TypicalValue => String)
info = method(Dispatch => Thing, TypicalValue => String)
-- TODO: move this here: net = method(Dispatch => Thing, TypicalValue => String)

show = method()

-- registerFinalizer
registerFinalizer' = registerFinalizer
registerFinalizer = method()
registerFinalizer(Thing, String) := registerFinalizer'

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
