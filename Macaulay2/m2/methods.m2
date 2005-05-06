--		Copyright 1993-2002 by Daniel R. Grayson

noapp := (f,x) -> error(
     "no method for applying item of class ", toString class f, 
     " to item of class ", toString class x
     )

f := (i,arg) -> horizontalJoin("     argument ",i," :  ",silentRobustNetWithClass(60,5,3,arg));
line0 := meth -> concatenate("no method found for applying ", silentRobustString(45,3,meth), " to:");

noMethodSingle := (meth,args) -> error toString stack( line0 meth, f(" ",args))
noMethod := (meth,args) -> error toString stack join( {line0 meth},
     if class args === Sequence and 0 < #args and #args <= 4
     then apply(#args, i -> f(toString (i+1),args#i))
     else {f(" ",args)})

methodDefaults := new OptionTable from {
     SingleArgumentDispatch => false,
     Associative => false,
     TypicalValue => Thing,
     Options => null
     }

methodFunctionOptions = new MutableHashTable
methodOptions = new MutableHashTable
methodDispatchFunctions = new MutableHashTable

AssociativeNoOptions := () -> (
     methodFunction := newmethod1 (args -> noMethod(methodFunction,args));
     binaryLookup := (x,y) -> (
	  -- Common code for every associative method without options
	  f := lookup(methodFunction,class x,class y);
	  if f === null then noMethod(methodFunction,(x,y)) else f(x,y)
	  );
     methodFunction(Sequence) := self := 
     args -> (
	  -- Common code for every associative method without options
	  if #args === 2 then binaryLookup args
	  else if #args >= 3 then self prepend(self(args#0,args#1),drop(args,2))
	  else if #args === 1 then args#0
	  else if #args === 0 then (
	       f := lookup (1 : methodFunction);
	       if f === null then noMethod(methodFunction,args) else f args
	       )
	  else error "wrong number of arguments"
	  );
     methodDispatchFunctions#self = true;
     methodFunction)

chkopt0 := k -> if not ( instance(k, Symbol) ) then error "expected SYMBOL => VALUE"
chkopt  := o -> if not ( class o === Option and #o === 2 and instance(o#0, Symbol) ) then error "expected SYMBOL => VALUE"
chkopts := x -> if class x === OptionTable then scan(keys x,chkopt0) else if class x === List then scan(x,chkopt) else error "expected list of optional arguments"

SingleArgWithOptions := opts -> (
     -- chkopts opts;
     if class opts =!= OptionTable then opts = new OptionTable from opts;
     methodFunction := opts >> 
     options ->
         arg -> (
	  -- Common code for every method with options, single argument
	  f := lookup(methodFunction, class arg);
	  if f === null then noMethodSingle(methodFunction,arg) else (f options) arg
	  );
     methodOptions#methodFunction = opts;
     methodFunction)

AssociativeWithOptions := opts -> (
     -- chkopts opts;
     error "associative methods with options not implemented yet"
     )

MultipleArgsWithOptions := opts -> (
     -- chkopts opts;
     if class opts =!= OptionTable then opts = new OptionTable from opts;
     methodFunction := opts >> 
     options ->
         arg -> (
	  -- Common code for methods with options, multiple arguments.
	  -- Dispatches on type of argument.
	  f := lookup(methodFunction, class arg);
	  if f === null then noMethod(methodFunction,arg) else (f options) arg
	  );
     methodOptions#methodFunction = opts;
     self := methodFunction(Sequence) := 
     options ->
        args -> (
	  -- Common code for every method with options, multiple arguments
	  -- Dispatches on type of arguments ('args' is a sequence).
	  f := lookup prepend(methodFunction,apply(args,class));
	  if f === null then noMethod(methodFunction,args) else (f options) args
	  );
     methodDispatchFunctions#self = true;
     methodFunction)

MultipleArgsNoOptions := () -> (
     methodFunction := newmethod123c(,args -> noMethod(methodFunction,args), {});
     self := methodFunction Sequence := newmethod123c( methodFunction, args -> noMethod(methodFunction,args), {} );
     methodDispatchFunctions#self = true;
     methodFunction)     

method = methodDefaults >> options -> args -> (
     if args =!= () then error "expected only optional arguments";
     methodFunction := (
	  if options.Options === null then (
       	       if options.Associative then AssociativeNoOptions()
       	       else if options.SingleArgumentDispatch then newmethod1 (args -> noMethodSingle(methodFunction,args))
       	       else MultipleArgsNoOptions())
	  else (
       	       if options.Associative then AssociativeWithOptions options.Options
       	       else if options.SingleArgumentDispatch then SingleArgWithOptions options.Options
       	       else MultipleArgsWithOptions options.Options
	       )
	  );
     if options.TypicalValue =!= Thing then typicalValues#methodFunction = options.TypicalValue;
     methodFunctionOptions#methodFunction = options;	    -- not the options to the method itself!
     methodFunction)

methodOptions#method = methodDefaults 			    -- hack in the first one

setup := (args, symbols) -> (
     scan(symbols, n -> (
	  if value n =!= n then error concatenate("symbol ",toString n," redefined");
	  f := method args;
	  n <- f;
	  )))

setup((), { 
	  entries, borel, prune, gcdCoefficients, singularLocus,
	  Hom, diff, diff', contract, contract', subsets, partitions, member,
	  koszul, symmetricPower, coefficientRing, trace, target, source,
	  getChangeMatrix, poincare, cover, coverMap, super, poincareN, terms,
	  dual, cokernel, coimage, image, generators, allGenerators, someTerms, scanKeys, scanValues,
	  substitute, rank, complete, ambient, top, baseName, remainder, quotientRemainder, remainder', quotientRemainder', quotient',
	  degree, coefficients, monomials, size, sum, product, exponents, nullhomotopy, module, raw,
	  hilbertFunction, content, leadTerm, leadCoefficient, leadMonomial, components,
	  leadComponent, degreesRing, degrees, annihilator, assign, numgens, conjugate,
	  autoload, minprimes, relations, cone, random, standardForm, inverse,
	  det, presentation, use = symbol use, dismiss, degreesMonoid, submatrix,
	  truncate, fraction
	  })

use Thing := identity

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

setup(SingleArgumentDispatch => true, {transpose} )
setup(TypicalValue => Boolean,
     {isBorel, isWellDefined, isInjective, isSurjective, isUnit, match,
	  isSubset,isHomogeneous, isIsomorphism, isPrime, isField, isConstant
	  })
setup(TypicalValue => ZZ,
     {length,codim,binomial,degreeLength,height,char,pdim,dim,depth,width,regularity,euler,genus})
setup(TypicalValue => List,
     {eulers, genera})

use HashTable := x -> (
     if x.?use then x.use x; 
     x)

radical = method( Options=>{ Unmixed=>false, CompleteIntersection => null } )

primaryDecomposition = method(
     TypicalValue => List,
     Options => {
	  PrintLevel => 0,
	  Strategy => null})

ass = method(
     TypicalValue => List,
     Options =>{
	  PrintLevel => 0,
	  Strategy => 2})

quotient = method(
     Options => {
	  --DegreeLimit => {},
	  --BasisElementLimit => infinity,
	  --PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null
	  }
     )

simpleToString = toString
toString = method(SingleArgumentDispatch => true, TypicalValue => String)
toString Thing := simpleToString			    -- if all else fails...
toString String := identity
toString Symbol := simpleToString

toExternalString = method(SingleArgumentDispatch => true, TypicalValue => String)
toExternalString Keyword := s -> concatenate("symbol ", format simpleToString s)
toExternalString Symbol := s -> (
     n := simpleToString s;
     if isGlobalSymbol n and getGlobalSymbol n === s then (
	  if value s === s then n
	  else concatenate("symbol ",if n == " " then format n else n)
	  )
     else error("can't convert local variable or shadowed or invisible global variable '",n,"' to external string"))

options = method(SingleArgumentDispatch=>true, TypicalValue => OptionTable)
setup(SingleArgumentDispatch=>true, {max,min,directSum,intersect,vars})
net = method(SingleArgumentDispatch=>true, TypicalValue => Net)
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
flatten = method(SingleArgumentDispatch=>true)
flatten VisibleList := VisibleList => oldflatten

-----------------------------------------------------------------------------

dictionary = method()
dictionary Keyword := s -> Macaulay2Core.Dictionary
dictionary Symbol := s -> (				    -- eventually every symbol will know what dictionary it's in, perhaps
     n := toString s;
     scan(globalDictionaries, d -> if d#?n and d#n === s then break d))
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
  optionFunction := {} >> () -> ()

-- new function 'sameFunctionBody', tells whether two functions are perhaps different
-- closures of the same function body.

-- this works for any function created with ">>", not just with "method"!
options Function := OptionTable => function -> (
     if sameFunctionBody(function, optionFunction) then first frame function
     else null)

computeAndCache := (M,options,Name,goodEnough,computeIt) -> (
     if not M#?Name or not goodEnough(M#Name#0,options) 
     then (
	  ret := computeIt(M,options);
	  M#Name = {options,ret};
	  ret)
     else M#Name#1
     )

exitMethod := method(SingleArgumentDispatch => true)
exitMethod ZZ := i -> exit i
exitMethod Sequence := () -> exit 0
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

match(String,String) := X -> 0 < length matches X

-- installation of assignment methods
installAssignmentMethod = method()
installAssignmentMethod(Symbol,HashTable,HashTable,Option) := 
installAssignmentMethod(Symbol,HashTable,HashTable,Function) := (op,X,Y,f) -> installMethod((op,symbol =),X,Y,f)
installAssignmentMethod(Symbol,HashTable,Option) := 
installAssignmentMethod(Symbol,HashTable,Function) := (op,Y,f) -> installMethod((op,symbol =),Y,f)

-----------------------------------------------------------------------------
-- helper functions useable in documentation
-----------------------------------------------------------------------------

TEST = method()
testnumber = 0
TEST Function := TEST String := s -> (
     currentPackage#"test inputs"#(testnumber,currentFileName) = s;
     testnumber = testnumber + 1;
     )
TEST List := y -> TEST \ y

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
