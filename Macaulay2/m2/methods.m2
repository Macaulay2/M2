--		Copyright 1993-2002 by Daniel R. Grayson

-- temporary definitions to get error messages to work before methods are working, so we can debug methods
assert( class between === Symbol )
between = (m,v) -> mingle(v,#v-1:m)
assert( class toString === Symbol )
toString = x -> (
     if ReverseDictionary#?x then simpleToString ReverseDictionary#?x
     else if class x === Net then concatenate between("\n",unstack x)
     else simpleToString x
     )
silentRobustString = (wid,sec,y) -> simpleToString y
silentRobustNetWithClass = silentRobustNet = (wid,ht,sec,y) -> simpleToString y
--

MethodFunction = new Type of FunctionClosure
MethodFunctionWithOptions = new Type of MethodFunction

dispatcherFunctions = {}

noapp := (f,x) -> error(
     "no method for applying item of class ", toString class f, 
     " to item of class ", toString class x
     )

f := (i,arg,out) -> horizontalJoin("     argument ",i," :  ", (if out then silentRobustNet else silentRobustNetWithClass)(60,5,3,arg));
line0 := meth -> concatenate("no method found for applying ", silentRobustString(45,3,meth), " to:");

noMethodSingle := (meth,args,output) -> error toString stack( line0 meth, f(" ",args,output))
noMethod := (meth,args,outputs) -> error toString stack join( {line0 meth},
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
     methodFunction := newmethod1(args -> noMethod(methodFunction,args,outputs),outputs);
     binaryLookup := (x,y) -> (
	  -- Common code for every associative method without options
	  f := lookup(methodFunction,class x,class y);
	  if f === null then noMethod(methodFunction,(x,y),outputs) else f(x,y)
	  );
     methodFunction(Sequence) := self := 
     args -> (
	  -- Common code for every associative method without options
	  if #args === 2 then binaryLookup args
	  else if #args >= 3 then self prepend(self(args#0,args#1),drop(args,2))
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
     if class opts =!= OptionTable then opts = new OptionTable from opts;
     methodFunction := opts >> 
     o ->
         arg -> (
	  -- Common code for every method with options, single argument
	  f := lookup(methodFunction, class arg);
	  if f === null then noMethodSingle(methodFunction,arg,outputs) else (f o) arg
	  );
     methodFunction)

BinaryWithOptions := (opts,outputs) -> (
     -- chkopts opts;
     error "associative methods with options not implemented yet"
     )

MultipleArgsWithOptions := (methopts,opts,outputs) -> (
     if class opts =!= OptionTable then opts = new OptionTable from opts;
     local innerMethodFunction;
     methodFunction := new MethodFunctionWithOptions from (opts >> o -> arg -> innerMethodFunction(o,arg));
     innerMethodFunction = newmethod1234c(methodFunction, args -> noMethod(methodFunction,args,outputs), (i,args) -> badClass(methodFunction,i,args), outputs, true);
     methodFunction)     
MultipleArgsWithOptionsGetMethodOptions := meth -> (frames (frames meth)#0#1)#0#0 -- this recovers the value of methopts

MultipleArgsNoOptions := (methopts,outputs) -> (
     local innerMethodFunction;
     methodFunction := new MethodFunction from (x -> innerMethodFunction x);
     innerMethodFunction = newmethod1234c(methodFunction,args -> noMethod(methodFunction,args,outputs), (i,args) -> badClass(methodFunction,i,args), outputs, false);
     methodFunction)
MultipleArgsNoOptionsGetMethodOptions := meth -> (frames meth)#0#0 -- this recovers the value of methopts

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
     methodFunction := (
	  if opts.Options === null then (
       	       if opts.Binary then BinaryNoOptions(outputs)
       	       else if singleArgDispatch then newmethod1 (args -> noMethodSingle(methodFunction,args,outputs), outputs)
       	       else MultipleArgsNoOptions(opts, outputs))
	  else (
       	       if opts.Binary then BinaryWithOptions(opts.Options,outputs)
       	       else if singleArgDispatch then SingleArgWithOptions (opts.Options, outputs)
       	       else MultipleArgsWithOptions(opts, opts.Options, outputs)
	       )
	  );
     if opts.TypicalValue =!= Thing then typicalValues#methodFunction = opts.TypicalValue;
     methodFunction)

setup := (args, symbols) -> (
     scan(symbols, n -> (
	  if value n =!= n then error concatenate("symbol ",toString n," redefined");
	  f := method args;
	  globalAssignFunction(n,f);
	  n <- f;
	  )))

setup((), { 
	  entries, borel, gcdCoefficients, singularLocus,
	  Hom, diff, diff', contract, contract', subsets, partitions, member,
	  koszul, symmetricPower, coefficientRing, trace, target, source,
	  getChangeMatrix, poincare, cover, coverMap, super, poincareN, terms,
	  dual, cokernel, coimage, comodule, image, generators, allGenerators, someTerms, scanKeys, scanValues,
	  substitute, rank, complete, ambient, topComponents, baseName, remainder, quotientRemainder, remainder', quotientRemainder', quotient',
	  degree, coefficients, monomials, size, sum, product, exponents, nullhomotopy, module, raw,
	  hilbertFunction, content, leadTerm, leadCoefficient, leadMonomial, components,
	  leadComponent, degreesRing, degrees, annihilator, assign, numgens, conjugate,
	  autoload, minprimes, relations, cone, random, standardForm, inverse,
	  determinant, presentation, dismiss, degreesMonoid, submatrix,
	  truncate, fraction, part, coefficient, preimage
	  })

use = method()
use Thing := identity

minimalPresentation = method(Options=>{Variable => null})

status = method (
     Options => new OptionTable from {
     	  TotalPairs => true,
     	  PairsRemaining => false,
     	  Monomials => false
     	  })

basis = method(
     Options => new OptionTable from {
	  Heft => null,
	  Truncate => false,
	  Limit => -1,
	  Variables => null
     	  })

sopts := 
Options => {
     DegreeOrder => Ascending,
     MonomialOrder => Ascending
     }

sortColumns = method sopts
sort = method sopts
rsort = method sopts

mopts := Options => {
     Degree => null,					    -- for use with matrices
     DegreeMap => null					    -- for use in ring maps
     }

matrix = method mopts
map = method mopts

setup(Dispatch => Thing, {transpose} )
setup(TypicalValue => Boolean,
     {isBorel, isWellDefined, isInjective, isSurjective, isUnit, match,
	  isSubset,isHomogeneous, isIsomorphism, isPrime, isField, isConstant
	  })
setup(TypicalValue => ZZ,
     {length,codim,binomial,degreeLength,height,char,pdim,dim,depth,width,regularity,euler,genus})
setup(TypicalValue => List,
     {eulers, genera})

radical = method( Options=>{ Unmixed=>false, CompleteIntersection => null } )

primaryDecomposition = method( TypicalValue => List, Options => { Strategy => null } )
associatedPrimes = method( TypicalValue => List, Options =>{ Strategy => 1 } )

toString = method(Dispatch => Thing, TypicalValue => String)
toString Thing := simpleToString			    -- if all else fails...
toString String := identity
toString Symbol := simpleToString

toExternalString = method(Dispatch => Thing, TypicalValue => String)
toExternalString Keyword := s -> concatenate("symbol ", simpleToString s)
toExternalString Symbol := s -> (
     n := simpleToString s;
     if isGlobalSymbol n and getGlobalSymbol n === s then if value s === s then n else concatenate("symbol ", n)
     else error("can't convert local variable or shadowed or invisible global variable '",n,"' to external string"))
toExternalString Boolean := simpleToString
toExternalString Nothing := simpleToString

toExternalString Thing := x -> (
     if ReverseDictionary#?x then return toString ReverseDictionary#x;
     error("can't convert anonymous object of class ",toString class x," to external string"))

options = method(Dispatch => Thing, TypicalValue => OptionTable)
setup(Dispatch => Thing, {max,min,directSum,intersect,vars})
net = method(Dispatch => Thing, TypicalValue => Net)
factor = method( Options => { } )

cohomology = method( Options => { 
	  Degree => 0		  -- for local cohomology and sheaf cohomology
	  } )
homology = method( Options => { } )

trim    = method ( Options => {
	  -- DegreeLimit => {}
	  } )
mingens = method ( Options => { 
	  -- DegreeLimit => {}
	  } )

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
dictionary Thing := x -> if ReverseDictionary#?x then dictionary ReverseDictionary#x

-----------------------------------------------------------------------------
oldvalue := value
value = method()
value Symbol := value Pseudocode := oldvalue
value String := x -> oldvalue x
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
options Sequence := s -> if lookup s =!= null then options lookup s

 optionFunction1 := {} >> identity
 optionFunction2 := method(Options => {})
 optionFunction3 := method(Options => {}, Dispatch => Thing)
options Function := OptionTable => function -> (
     if functionBody function === functionBody optionFunction1 then first frame function
     else if functionBody function === functionBody optionFunction2 then notImplemented()
     else if functionBody function === functionBody optionFunction3 then notImplemented()
     )

computeAndCache := (M,options,Name,goodEnough,computeIt) -> (
     if not M#?Name or not goodEnough(M#Name#0,options) 
     then (
	  ret := computeIt(M,options);
	  M#Name = {options,ret};
	  ret)
     else M#Name#1
     )

exitMethod := method(Dispatch => Thing)
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

match(String,String) := X -> null =!= regex X

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

sourceFileStamp = () -> concatenate(
     "-- line ",
     toString currentLineNumber(),
     " in ",
     if not isAbsolutePath currentFileName and currentFileName != "stdio" then ("/",relativizeFilename ("/",concatenate(currentDirectory, currentFileName)))
     else currentFileName
     )

TEST = method()
TEST String := s -> (
     currentPackage#"test inputs"#(currentPackage#"test number",currentFileName,currentLineNumber()) = concatenate( sourceFileStamp(), newline, s);
     currentPackage#"test number" = currentPackage#"test number" + 1;
     )
TEST List := y -> TEST \ y

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
runHooks = method()

addHook   (MutableHashTable,Thing,Function) := (obj,key,hook) -> if obj#?key then obj#key = append(obj#key,hook) else obj#key = {hook}
removeHook(MutableHashTable,Thing,Function) := (obj,key,hook) -> if obj#?key then obj#key = delete(obj#key,hook)
runHooks  (MutableHashTable,Thing,Thing   ) := (obj,key,arg ) -> if obj#?key then scan(obj#key, hook -> hook arg)

addHook   (HashTable,Thing,Function) := (obj,key,hook) -> (c := obj.cache; if c#?key then c#key = append(c#key,hook) else c#key = {hook})
removeHook(HashTable,Thing,Function) := (obj,key,hook) -> (c := obj.cache; if c#?key then c#key = delete(c#key,hook))
runHooks  (HashTable,Thing,Thing   ) := (obj,key,arg ) -> (c := obj.cache; if c#?key then scan(c#key, hook -> hook arg))

-- and keys
protect QuotientRingHook

-----------------------------------------------------------------------------
-- stashing or caching computed values for future reference in functions that take a mutable hash table as input

cacheValue = key -> f -> x -> (
     c := x.cache;
     if c#?key then c#key else c#key = f x)
stashValue = key -> f -> x -> if x#?key then x#key else x#key = f x

-----------------------------------------------------------------------------
-- hypertext conversion

html = method(Dispatch => Thing, TypicalValue => String)
tex = method(Dispatch => Thing, TypicalValue => String)
texMath = method(Dispatch => Thing, TypicalValue => String)
mathML = method(Dispatch => Thing, TypicalValue => String)
info = method(Dispatch => Thing, TypicalValue => String)

-----------------------------------------------------------------------------
-- method options

methodOptions = method()
methodOptions Function := f -> null
methodOptions MethodFunctionWithOptions := MultipleArgsWithOptionsGetMethodOptions
methodOptions MethodFunction := MultipleArgsNoOptionsGetMethodOptions

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
