--		Copyright 1993-1999 by Daniel R. Grayson

noapp := (f,x) -> error(
     "no method for applying item of class ", toString class f, 
     " to item of class ", toString class x
     )

-----------------------------------------------------------------------------

protect Options

noMethod := args -> (
     if class args === Sequence 
     then if 0 < #args and #args <= 3 
     then error("no method found for items of classes ",toString apply(args, class))
     else error("no method found for item of class Sequence and length ",toString(#args))
     else error("no method found for item of class ", toString class args)
     )

methodDefaults := new OptionTable from {
     SingleArgumentDispatch => false,
     Associative => false,
     TypicalValue => Thing,
     Options => null
     }

methodFunctionOptions = new MutableHashTable

AssociativeNoOptions := () -> (
     methodFunction := newmethod1 noMethod;
     binaryLookup := (x,y) -> (
	  -- Common code for every associative method without options
	  f := lookup(methodFunction,class x,class y);
	  if f === null then noMethod(x,y)
	  else f(x,y)
	  )
     ;
     methodFunction(Sequence) := self := 
     args -> (
	  -- Common code for every associative method without options
	  if #args === 2 then binaryLookup args
	  else if #args >= 3 then self prepend(self(args#0,args#1),drop(args,2))
	  else if #args === 1 then args#0
	  else if #args === 0 then noMethod args
	  else error "wrong number of arguments"
	  )
     ;
     methodFunction)

SingleArgWithOptions := opts -> (
     if class opts =!= OptionTable then opts = new OptionTable from opts;
     methodFunction := opts ==> 
     options ->
         arg -> (
	  -- Common code for every method with options, single argument
	  f := lookup(methodFunction, class arg);
	  if f === null then noMethod arg
	  else (f options) arg
	  )
     ;
     methodFunction)

AssociativeWithOptions := opts -> error "associative methods with options not implemented yet"

MultipleArgsWithOptions := opts -> (
     if class opts =!= OptionTable then opts = new OptionTable from opts;
     methodFunction := opts ==> 
     options ->
         arg -> (
	  -- Common code for methods with options, multiple arguments.
	  -- Dispatches on type of argument.
	  f := lookup(methodFunction, class arg);
	  if f === null then noMethod arg else (f options) arg
	  )
     ;
     methodFunction(Sequence) := 
     options ->
        args -> (
	  -- Common code for every method with options, multiple arguments
	  -- Dispatches on type of arguments ('args' is a sequence).
	  f := lookup prepend(methodFunction,apply(args,class));
	  if f === null then noMethod args else (f options) args
	  )
     ;
     methodFunction)

MultipleArgsNoOptions := () -> (
     methodFunction := newmethod123c(,noMethod, {});
     methodFunction(Sequence) := newmethod123c( methodFunction, noMethod, {} );
     methodFunction)     

method = methodDefaults ==> options -> () -> (
     methodFunction := (
	  if options.Options === null then (
       	       if options.Associative then AssociativeNoOptions()
       	       else if options.SingleArgumentDispatch then newmethod1 noMethod
       	       else MultipleArgsNoOptions())
	  else (
       	       if options.Associative then AssociativeWithOptions options.Options
       	       else if options.SingleArgumentDispatch then SingleArgWithOptions options.Options
       	       else MultipleArgsWithOptions options.Options
	       )
	  );
     if options.TypicalValue =!= Thing then typicalValues#methodFunction = options.TypicalValue;
     methodFunctionOptions#methodFunction = options;
     methodFunction)

setup := (args, symbols) -> (
     scan(symbols, n -> (
	  if Symbols#?n then error concatenate("function redefined");
	  f := method args;
	  Symbols#f = n;
	  n <- f;
	  )))

setup((), { 
	  borel, prune, gcdCoefficients, singularLocus, 
	  Hom, diff, diff', contract, contract', exteriorPower, subsets, partitions, member,
	  koszul, symmetricPower, basis, coefficientRing, trace,
	  getChangeMatrix, poincare, cover, super, poincareN, terms,
	  dual, cokernel, coimage, image, generators, someTerms, scanKeys, scanValues, stats, 
	  substitute, rank, complete, ambient, top, transpose, baseName,
	  degree, coefficients, size, sum, product,
	  exponents, nullhomotopy,
	  hilbertFunction, content, leadTerm, leadCoefficient, leadMonomial, 
	  leadComponent, degreesRing, newDegreesRing, degrees, annihilator, assign, numgens,
	  autoload, ggPush, minprimes, relations, cone, random,
	  det, presentation, symbol use, degreesMonoid, newDegreesMonoid, submatrix,
	  truncate, fraction
	  })
setup(TypicalValue => RR, {realPart, imaginaryPart})
setup(TypicalValue => Boolean,
     {isBorel, isWellDefined, isInjective, isSurjective, isUnit,
	  isSubset,isHomogeneous, isIsomorphism, isPrime, isField
	  })
setup(TypicalValue => ZZ,
     {length,codim,binomial,degreeLength,height,char,pdim,dim,depth,width,regularity})
setup(TypicalValue => List,
     {euler, genera})

use Thing := identity

use HashTable := x -> (
     if x.?use then x.use x; 
     x)

radical = method( Options=>{ Unmixed=>false, CompleteIntersection => null } )
toString = method(SingleArgumentDispatch => true, TypicalValue => String)
toExternalString = method(SingleArgumentDispatch => true, TypicalValue => String)
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

width File := fileWidth; erase symbol fileWidth
width Net := netWidth; erase symbol netWidth
height Net := netHeight; erase symbol netHeight
depth Net := netDepth; erase symbol netDepth
width String := s -> #s
height String := s -> 1
depth String := s -> 0

-----------------------------------------------------------------------------

oldflatten := flatten
erase symbol flatten
flatten = method(SingleArgumentDispatch=>true)
flatten VisibleList := VisibleList => oldflatten
coker = cokernel

source = method()
source Thing := (h) -> (
     if h#?(symbol source) then h.source
     else if (class h)#?(symbol source) then (class h)#?(symbol source)
     else error ( toString h, " of class ", toString class h, " has no source" ))

target = method()
target Thing := (h) -> (
     if h.?target then h.target
     else if (class h)#?(symbol target) then (class h)#?(symbol target)
     else error (toString h | " of class " | toString class h | " has no target"))

gens = generators

-----------------------------------------------------------------------------
oldvalue := value
erase symbol value
value = method()
value Symbol := value String := oldvalue
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

erase symbol newmethod1
erase symbol newmethod123c

emptyOptionTable := new OptionTable
options     Ring := x -> null
options Sequence := s -> if lookup s =!= null then options lookup s
  optionFunction := {} ==> () -> ()
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

lines = x -> (
     l := separate x;
     if l#-1 === "" then drop(l,-1) else l)


between = method()
between(Thing,VisibleList) := List => (m,v) -> mingle(v,#v-1:m)

