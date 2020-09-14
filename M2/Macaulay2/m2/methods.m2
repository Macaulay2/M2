--		Copyright 1993-2002 by Daniel R. Grayson

-- temporary definitions to get error messages to work before methods are working, so we can debug methods
assert( class between === Symbol )
between = (m,v) -> mingle(v,#v-1:m)			    -- provisional
assert( class toString === Symbol )
toString = x -> (					    -- provisional
     if hasAttribute(x,ReverseDictionary) then simpleToString getAttribute(x,ReverseDictionary)
     else if class x === Net then concatenate between("\n",unstack x)
     else simpleToString x
     )
silentRobustString = (wid,sec,y) -> simpleToString y
silentRobustNetWithClass = silentRobustNet = (wid,ht,sec,y) -> simpleToString y
--

MethodFunction = new Type of CompiledFunctionClosure
MethodFunctionSingle = new Type of CompiledFunctionClosure
MethodFunctionBinary = new Type of CompiledFunctionClosure
MethodFunctionWithOptions = new Type of FunctionClosure

MethodFunction.synonym = "method function"
MethodFunctionSingle.synonym = "method function with a single argument"
MethodFunctionBinary.synonym = "binary method function"
MethodFunctionWithOptions.synonym = "method function with options"

dispatcherFunctions = {}

noapp := (f,x) -> error(
     "no method for applying item of class ", toString class f, 
     " to item of class ", toString class x
     )

f := (i,arg,out) -> horizontalJoin("     argument ",i," :  ", (if out then silentRobustNet else silentRobustNetWithClass)(60,5,3,arg));
line0 := meth -> concatenate("no method found for applying ", silentRobustString(45,3,meth), " to:");

noMethodSingle = (meth,args,output) -> error toString stack( line0 meth, f(" ",args,output))
noMethod = (meth,args,outputs) -> error toString stack join( {line0 meth},
     if class args === Sequence and 0 < #args and #args <= 4
     then apply(#args, i -> f(toString (i+1),args#i,if outputs#?i then outputs#i else false))
     else {f(" ",args,
	       false					    -- do better here
	       )})
badClass := (meth,i,args) -> (
     if i == -1 then error(silentRobustString(45,3,meth),": expected an output class, but got: ", silentRobustString(45,3,args))
     else error(silentRobustString(45,3,meth),": expected argument ",toString (i+1)," to be a type, but it was: ", silentRobustString(45,3,args#i)))

methodDefaults := new OptionTable from {
     Binary => false,
     TypicalValue => Thing,
     Options => null,
     Dispatch => {Thing,Thing,Thing,Thing}                  -- Thing or Type (for single arg dispatch) or list of Thing or Type (for multiple inheritance)
     	       	    	      	   	     	       	    -- if a list, it's assumed to continue with the default, which is Thing ...
     }

BinaryNoOptions := (outputs) -> (
     methodFunction := newmethod1(args -> noMethod(methodFunction,args,outputs),outputs,MethodFunctionBinary);
     binaryLookup := (x,y) -> (
	  -- Common code for every associative method without options
	  f := lookup(methodFunction,class x,class y);
	  if f === null then noMethod(methodFunction,(x,y),outputs) else f(x,y)
	  );
     methodFunction(Sequence) := self := 
     args -> (
	  -- Common code for every associative method without options
	  if #args === 2 then binaryLookup args
	  else if #args >= 3 then (
	       r := self(args#0,args#1);
	       for i from 2 to #args-1 do r = self(r,args#i);
	       r)
	  else if #args === 1 then args#0
	  else if #args === 0 then (
	       f := lookup (1 : methodFunction);
	       if f === null then noMethod(methodFunction,args,outputs) else f args
	       )
	  else error "wrong number of arguments"
	  );
     methodFunction)

chkopt0 := k -> if not ( instance(k, Symbol) ) then error "expected SYMBOL => VALUE"
chkopt  := o -> if not ( class o === Option and #o === 2 and instance(o#0, Symbol) ) then error "expected SYMBOL => VALUE"
chkopts := x -> if class x === OptionTable then scan(keys x,chkopt0) else if class x === List then scan(x,chkopt) else error "expected list of optional arguments"

SingleArgWithOptions := (opts,outputs) -> (
     -- chkopts opts;
     local methodFunction;
     class' := if outputs then identity else class;
     if opts === true then (
	  methodFunction = opts >> 
	  o ->
	      arg -> (
	       -- Common code for every method with options, single argument, with Options => true
	       f := lookup(methodFunction, class' arg);
	       if f === null then noMethodSingle(methodFunction,arg,outputs) else 
	       if #o === 0 then f arg else f(o,arg)
	       );
	  methodFunction
	  )
     else (
	  if instance(opts, List) then opts = new OptionTable from opts;
	  methodFunction = opts >> 
	  o ->
	      arg -> (
	       -- Common code for every method with options, single argument, not Options => true
	       f := lookup(methodFunction, class' arg);
	       if f === null then noMethodSingle(methodFunction,arg,outputs) else (f o) arg
	       );
	  methodFunction
	  )
     )

BinaryWithOptions := (opts,outputs) -> (
     -- chkopts opts;
     error "associative methods with options not implemented yet"
     )

MultipleArgsWithOptions := (methopts,opts,outputs) -> (
     if instance(opts,List) then opts = new OptionTable from opts;
     local innerMethodFunction;
     methodFunction := new MethodFunctionWithOptions from (opts >> o -> arg -> innerMethodFunction(o,arg));
     innerMethodFunction = newmethod1234c(
	  methodFunction,
	  args -> noMethod(methodFunction,args,outputs),
	  (i,args) -> badClass(methodFunction,i,args),
	  outputs,
	  opts =!= true
	  );
     methodFunction)     
MultipleArgsWithOptionsGetMethodOptions := meth -> (frames (frames meth)#0#1)#0#0 -- this recovers the value of methopts

MultipleArgsNoOptions := (methopts,outputs) -> (
     methodFunction := newmethod1234c(
	  MethodFunction,
	  args -> noMethod(methodFunction,args,outputs),
	  (i,args) -> badClass(methodFunction,i,args),
	  outputs,
	  null
	  )
     )
MultipleArgsNoOptionsGetMethodOptions := meth -> (frames (frames meth)#0#1)#0#0

all' := (x,f) -> (
     r := true;
     scan(x, i -> if not f i then (r = false; break));
     r)

method = methodDefaults >> opts -> args -> (
     if args =!= () then error "expected only optional arguments";
     chk := c -> c === Thing or c === Type;
     if not if instance(opts.Dispatch,List) then all'(opts.Dispatch, chk) else chk opts.Dispatch
     then error "expected Dispatch option to be: Thing, Type, or a list of those";
     singleArgDispatch := chk opts.Dispatch;
     outputs := if not singleArgDispatch then apply(opts.Dispatch, c -> c === Type) else opts.Dispatch === Type;
     saveCurrentFileName := currentFileName;		    -- for debugging
     saveCurrentLineNumber := currentLineNumber;	    -- for debugging
     methodFunction := (
	  if opts.Options === null then (
       	       if opts.Binary then BinaryNoOptions(outputs)
       	       else if singleArgDispatch then newmethod1 (args -> noMethodSingle(methodFunction,args,outputs), outputs, MethodFunctionSingle)
       	       else MultipleArgsNoOptions(opts, outputs))
	  else (
       	       if opts.Binary then BinaryWithOptions(opts.Options,outputs)
       	       else if singleArgDispatch then SingleArgWithOptions (opts.Options, outputs)
       	       else MultipleArgsWithOptions(opts, opts.Options, outputs)
	       )
	  );
     if opts.TypicalValue =!= Thing then (
	  if not instance(opts.TypicalValue,Type) then error("expected typical value ", toString opts.TypicalValue, " to be a type");
	  typicalValues#methodFunction = opts.TypicalValue;
	  );
     methodFunction)

setupMethods := (args, symbols) -> (
     scan(symbols, n -> (
	  if value' n =!= n then error concatenate("symbol ",toString n," redefined");
	  f := method args;
	  globalAssignFunction(n,f);
	  n <- f;
	  )))

setupMethods((), { 
      localRing,
	  entries, borel, gcdCoefficients, singularLocus,
	  Hom, diff, diff', contract, contract', subsets, partitions, member,
	  koszul, symmetricPower, trace, target, source,
	  getChangeMatrix, poincare, cover, coverMap, super, poincareN, terms,
	  cokernel, coimage, comodule, image, someTerms, scanKeys, scanValues,
	  substitute, rank, complete, ambient, topComponents, baseName, remainder, quotientRemainder, remainder', quotientRemainder', quotient',
	  coefficients, monomials, size, sum, product, exponents, nullhomotopy, module, raw, exp,
	  hilbertFunction, content, leadTerm, leadCoefficient, leadMonomial, components,
	  leadComponent, degreesRing, degrees, assign, numgens, realPart, imaginaryPart, conjugate,
	  autoload, relations, cone, standardForm, inverse, numeric, round, degree, multidegree,
	  presentation, dismiss, precision, 
	  norm, clean, numColumns, numRows, fraction, part, coefficient, preimage, minimalPrimes, decompose,
	  chi, hasEngineLinearAlgebra, nullSpace,
      isBasicMatrix, basicDet, basicInverse, basicKernel, basicRank, basicSolve, basicRankProfile
	  })

use = method(Dispatch => Thing)
use Thing := identity

dual = method(Options => true)

default = method()
--default Type := (X) -> (
--     m := lookup(X,symbol default);
--     if m === null then error "no method found";
--     m ())

determinant = method(Options => { Strategy => null })

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
status = method (
     Options => new OptionTable from {
     	  TotalPairs => true,
     	  PairsRemaining => false,
     	  Monomials => false
     	  })

minimalBetti = method(Options => true)

sopts := 
Options => {
     DegreeOrder => null,				    -- used to be Ascending
     MonomialOrder => Ascending
     }

sortColumns = method sopts
sort = method sopts
rsort = method sopts

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
	  isSubset,isHomogeneous, isIsomorphism, isPrime, isPseudoprime, isField, isConstant
	  })
setupMethods(TypicalValue => ZZ,
     {binomial,degreeLength,height,char,pdim,dim,depth,width,euler,genus})
setupMethods(TypicalValue => List,
     {eulers, genera})

length = method(TypicalValue => ZZ, Dispatch => Thing)
codim = method( Options => true )
radical = method( Options=>{ Unmixed=>false, CompleteIntersection => null, Strategy => Decompose } )
regularity = method( TypicalValue => ZZ, Options => { Weights => null } )
associatedPrimes = method( TypicalValue => List, Options =>{ Strategy => 1 } )

-- defined in d/actors4.d
format' := format
format = method(Dispatch => Thing, TypicalValue => String)
format RR :=
format CC :=
format String   := String => x -> format' x
format Sequence := String => s -> format' s
protect symbol format

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

options = method(Dispatch => Thing, TypicalValue => OptionTable)
setupMethods(Dispatch => Thing, {max,min,directSum,intersect,vars})
net = method(Dispatch => Thing, TypicalValue => Net)
factor = method( Options => { } )

cohomology = method( Options => { 
	  Degree => 0		  -- for local cohomology and sheaf cohomology
	  } )
homology = method( Options => { } )

trim    = method ( Options => {
	  Strategy => Complement				    -- or null
	  -- DegreeLimit => {}
	  } )
mingens = method ( Options => { 
	  Strategy => Complement				    -- or null
	  -- DegreeLimit => {}
	  } )

mathML = method(Dispatch => Thing, TypicalValue => String)

width File := fileWidth
height File := fileHeight

width Net := netWidth
height Net := netHeight
depth Net := netDepth

width String := s -> #s
height String := s -> 1
depth String := s -> 0

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
select(ZZ, HashTable, Function) := HashTable => {} >> o -> select'
select(    BasicList, Function) := BasicList => {} >> o -> select'
select(    HashTable, Function) := HashTable => {} >> o -> select'
select(    BasicList, Type)     := BasicList => {} >> o -> (L, T) -> select(L, e -> instance(e, T))
-- two more methods installed in regex.m2

oldnumerator := numerator
erase symbol numerator
numerator = method()
numerator QQ := oldnumerator

olddenominator := denominator
erase symbol denominator
denominator = method()
denominator QQ := olddenominator

emptyOptionTable := new OptionTable
options     Ring := x -> null
options Sequence := s -> (
     m := lookup s;
     if m === null then error "method not found";
     options m)

notImplemented = x -> error "not implemented yet"

oftab := hashTable {
     functionBody (method(Options => {})) => f -> notImplemented(),
     functionBody (method(Options => {}, Dispatch => Thing)) => f -> notImplemented(),
     functionBody ({} >> identity) => f -> first frame f,
     functionBody (true >> identity) => f -> null
     }
options Function := OptionTable => f -> (
     fb := functionBody f;
     if oftab#?fb then oftab#fb f
     )

options Command := OptionTable => f -> options f#0

computeAndCache := (M,options,Name,goodEnough,computeIt) -> (
     if not M#?Name or not goodEnough(M#Name#0,options) 
     then (
	  ret := computeIt(M,options);
	  M#Name = {options,ret};
	  ret)
     else M#Name#1
     )

exitMethod = method(Dispatch => Thing)
exitMethod ZZ := i -> exit i
exitMethod Sequence := x -> exit 0
quit = Command (() -> exit 0)
erase symbol exit
exit = Command exitMethod

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
-- helper functions useable in documentation
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

ck := f -> ( assert( f =!= null ); assert( instance(f, Function) ); f )
dispatcherFunctions = join (dispatcherFunctions, {
	  -- lookup(method(),Sequence),
	  -- ck lookup(method(Options => {}),Sequence)
	  })

-----------------------------------------------------------------------------
-- hooks

addHook = method()
removeHook = method()
runHooks = method(Options => true)

addHook   (MutableHashTable,Thing,Function) := (obj,key,hook) -> obj#key = if obj#?key then prepend(hook,obj#key) else {hook}
removeHook(MutableHashTable,Thing,Function) := (obj,key,hook) -> if obj#?key then obj#key = delete(obj#key,hook)
runHooks  (MutableHashTable,Thing,Thing   ) := true >> opts -> (obj,key,arg ) -> (if obj#?key then scan(obj#key, hook -> (
          result := (if options hook =!= null then (
               hookOpts := select(keys options hook, k -> opts#?k) / (k -> k => opts#k);
               hook(arg, new OptionTable from hookOpts)
               ) else hook arg 
          );
          if not instance(result, Nothing) then break result)))

addHook   (HashTable,Thing,Function) := (obj,key,hook) -> (c := obj.cache; c#key = if c#?key then prepend(hook,c#key) else {hook})
removeHook(HashTable,Thing,Function) := (obj,key,hook) -> (c := obj.cache; if c#?key then c#key = delete(c#key,hook))
runHooks  (HashTable,Thing,Thing   ) := true >> opts -> (obj,key,arg ) -> (c := obj.cache; if c#?key then scan(c#key, hook -> (
          result := (if options hook =!= null then ( 
               hookOpts := select(keys options hook, k -> opts#?k) / (k -> k => opts#k);
               hook(arg, new OptionTable from hookOpts)
               ) else hook arg);
          if not instance(result, Nothing) then break result)))

addHook   (Symbol,Function) := (sym,hook) -> sym <- if value sym =!= sym then prepend(hook,value sym) else {hook}
removeHook(Symbol,Function) := (sym,hook) -> if value sym =!= sym then sym <- delete(value sym,hook)
runHooks  (Symbol,Thing   ) := true >> opts -> (sym,arg ) -> if value sym =!= sym then scan(value sym, hook -> (
          result := (if options hook =!= null then ( 
               hookOpts := select(keys options hook, k -> opts#?k) / (k -> k => opts#k);
               hook(arg, new OptionTable from hookOpts)
               ) else hook arg);
          if not instance(result, Nothing) then break result))

-- and keys
protect QuotientRingHook

-----------------------------------------------------------------------------
-- stashing or caching computed values for future reference in functions that take a mutable hash table as input

CacheFunction = new Type of FunctionClosure
CacheFunction.synonym = "a cache function"
net CacheFunction := f -> "-*a cache function*-"
cacheValue = key -> f -> new CacheFunction from (x -> (
     	  c := try x.cache else x.cache = new CacheTable;
     	  if c#?key then (
	       val := c#key;
	       if class val === CacheFunction then (
		    remove(c,key);
		    c#key = val x)
	       else val
	       )
	  else c#key = f x))
stashValue = key -> f -> new CacheFunction from (x -> (
     	  if x#?key then (
	       val := x#key;
	       if class val === CacheFunction then (
		    remove(x,key);
		    x#key = val x)
	       else val
	       )
	  else x#key = f x))

codeHelper#(functionBody (cacheValue null) null) = g -> {
     ("-- function f:", value (first localDictionaries g)#"f")
     }
codeHelper#(functionBody (stashValue null) null) = g -> {
     ("-- function f:", value (first localDictionaries g)#"f")
     }

-----------------------------------------------------------------------------
-- hypertext conversion

html = method(Dispatch => Thing, TypicalValue => String)
markdown = method(Dispatch => Thing, TypicalValue => String)
tex = method(Dispatch => Thing, TypicalValue => String)
texMath = method(Dispatch => Thing, TypicalValue => String)
htmlWithTex = method(Dispatch => Thing, TypicalValue => String)
info = method(Dispatch => Thing, TypicalValue => String)
-- TODO: move this here: net = method(Dispatch => Thing, TypicalValue => String)

show = method()

-- method options

methodOptions = method(TypicalValue => OptionTable)
methodOptions Function := methodOptions Symbol := f -> null
methodOptions MethodFunctionWithOptions := MultipleArgsWithOptionsGetMethodOptions
methodOptions MethodFunction := MultipleArgsNoOptionsGetMethodOptions
methodOptions Command := f -> methodOptions f#0

-- values of functions by lookup
lookupfuns = new MutableHashTable
storefuns = new MutableHashTable
lookupfuns#toString = x -> f -> if hasAttribute(x,PrintNames) then getAttribute(x,PrintNames) else f x
storefuns #toString = (x,e) -> (
     if not instance(e,String) then error "expected a string";
     setAttribute(x,PrintNames,e))
Function Thing = (f,x,e) -> (
     if not storefuns#?f then error("no method for storing values of function ", toString f);
     storefuns#f (x,e))

-- defined in d/actors4.d
locate' = locate -- TODO: why does (net, FunctionBody) in nets.m2 need locate'?
locate = method(Dispatch => Thing, TypicalValue => Sequence)
locate Nothing    := Sequence => x -> locate' x
locate Function   := Sequence => x -> locate' x
locate Pseudocode := Sequence => x -> locate' x
locate Sequence   := Sequence => x -> locate' x
locate Symbol     := Sequence => x -> locate' x
protect symbol locate

-- baseName
baseName Thing := R -> (
     if hasAttribute(R,ReverseDictionary) then (
	  x := getAttribute(R,ReverseDictionary);
	  if not mutable x then error("baseName: base name ",toString x," is not mutable, hence not available for use as a variable");
	  x)
     else error "baseName: no base name available"
     )

-- exp
exp = method()
exp CC := CC => exp'
exp RR := exp QQ := exp ZZ := RR => exp'
exp RingElement := RingElement => r -> (
     try
     promote(exp lift(r,RR),ring r)
     else try
     promote(exp lift(r,CC),ring r)
     else (
	  n := 1;
	  rn := r;
	  e := try 1/1 + rn else error "exp: expected an algebra over QQ";
	  while true do (
	       n = n+1;
	       rn = (1/n)*rn*r;
	       if rn == 0 then break e;
	       e = e + rn;
	       )))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
