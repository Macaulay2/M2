--		Copyright 1994 by Daniel R. Grayson

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

method = args -> processArgs(
  args,
  methodDefaults,
  options -> () -> (
      if options.Options === null then (
	if options.Associative then (
	  methodFunction := newmethod1 noMethod;
	  sequenceMethod := methodFunction(Sequence) :=
	  args -> (
	    -- Common code for every associative method without options
	    if #args === 2 
	    then ((x,y) -> (
		f := lookup(methodFunction,class x,class y);
		if f === null then noMethod args
		else f(x,y))
	      ) args
	    else if #args >= 3 
	    then sequenceMethod prepend(sequenceMethod(args#0,args#1),drop(args,2))
	    else if #args === 1 then args#0
	    else if #args === 0 then noMethod args
	    else error "wrong number of arguments"
	    ))
	else if options.SingleArgumentDispatch
	then methodFunction = newmethod1 noMethod
	else (
	  if false -- options.FirstArgumentDispatch
	  then (
	    methodFunction = newmethod1 noMethod;
	    methodFunction(Sequence) :=
	    args -> (
	      -- Common code for methods that dispatch on first argument
	      -- and receive a sequence of arguments.
	      -- Using 'code f'?  Try 'code methods f'.
	      -- Using 'browse'?  Try looking at the METHODS.
	      f := lookup(methodFunction, class args#0);
	      if f === null then noMethod args else f args
	      )
	    )
	  else (
	    methodFunction = newmethod123c(,noMethod, {});
	    methodFunction(Sequence) := newmethod123c( methodFunction, noMethod, {} ))))
      else (
	opts := new OptionTable from options.Options;
	methodFunction = 
	args -> processArgs(args,opts,
	  -- Common code for every method with options.
	  -- Using 'code f'?  Try 'code methods f'.
	  -- Using 'browse'?  Try looking at the METHODS.
	  options -> args -> (
	    f := lookup(methodFunction, class args);
	    if f === null then noMethod args
	    else (f options)(args)));
	OptionsRegistry#methodFunction = opts;
	methodFunction(Sequence) := options -> args -> (
	  -- Common code for every method with options
	  if #args === 2 
	  then ((x,y) -> (
	      f := lookup(methodFunction,class x,class y);
	      if f === null then noMethod args
	      else (f options)(x,y))
	    ) args
	  else if #args === 3 
	  then ((x,y,z) -> (
	      f := lookup(methodFunction,class x,class y,class z);
	      if f === null then noMethod args else (f options)(x,y,z))
	    ) args
	  else if #args === 1 
	  then ((x) -> (
	      f := lookup(methodFunction,class x);
	      if f === null then noMethod args else (f options)(x))
	    ) args
	  else if #args === 0
	  then noMethod args
	  else error "wrong number of arguments"
	  )
	);
      if options.TypicalValue =!= Thing then typicalValues#methodFunction = options.TypicalValue;
      methodFunctionOptions#methodFunction = options;
      methodFunction
      )
  )

OptionsRegistry#method = methodDefaults

setup := (args, symbols) -> (
     scan(symbols, n -> (
	  if Symbols#?n then error concatenate("function redefined");
	  f := method args;
	  Symbols#f = n;
	  n <- f;
	  )))

setup((), { 
	  borel, codim, 
	  lcmDegree, gcdDegree, prune, euler, genera, gcdCoefficients,
	  singularLocus, 
	  dim, Hom, diff, contract, exteriorPower, subsets, partitions, member,
	  koszul, symmetricPower, basis, coefficientRing, trace, binomial,
	  getchange, poincare, cover, super, poincareN, terms,
	  dual, cokernel, coimage, image, generators, someTerms, scanKeys, scanValues, stats, 
	  substitute, rank, complete, ambient, top, transpose, length, baseName,
	  degree, degreeLength, coefficients, size, sum, product,
	  exponents, height, depth, width, regularity, nullhomotopy,
	  hilbertFunction, content, monoid, leadTerm, leadCoefficient, leadMonomial, 
	  leadComponent, degreesRing, newDegreesRing, degrees, annihilator, assign, numgens,
	  autoload, ggPush, char, minprimes, relations, cone, pdim, random,
	  det, presentation, symbol use, degreesMonoid, newDegreesMonoid, submatrix,
	  truncate, fraction
	  })
setup(TypicalValue => Module, {subquotient})
setup(TypicalValue => RR, {realPart, imaginaryPart})
setup(TypicalValue => CC, {conjugate})
setup(TypicalValue => Boolean,
     {isBorel, isWellDefined, isInjective, isSurjective, isUnit,
	  isSubset,isHomogeneous, isIsomorphism, isPrime, isField
	  })
setup(TypicalValue => FractionField, {frac})
setup(TypicalValue => Ring, {ring})
setup(TypicalValue => Net, {betti})

use Thing := identity

use HashTable := x -> (
     if x.?use then x.use x; 
     x)

radical = method( Options=>{ Unmixed=>false, CompleteIntersection => null } )
toString = method(SingleArgumentDispatch => true, TypicalValue => String)
toExternalString = method(SingleArgumentDispatch => true, TypicalValue => String)
ideal = method(SingleArgumentDispatch=>true, TypicalValue => Ideal)
options = method(SingleArgumentDispatch=>true, TypicalValue => OptionTable)
submodule = method(SingleArgumentDispatch=>true, TypicalValue => Module)
setup(SingleArgumentDispatch=>true, {max,min,directSum,intersect,vars})
net = method(SingleArgumentDispatch=>true, TypicalValue => Net)
expression = method(SingleArgumentDispatch=>true, TypicalValue => Expression)
hilbertPolynomial = method(
     Options => { Projective => true }, 
     TypicalValue => ProjectiveHilbertPolynomial )
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
flatten List     := List     => oldflatten
flatten Sequence := Sequence => oldflatten
coker = cokernel

source = (h) -> (
     if h#?(symbol source) then h.source
     else if (class h)#?(symbol source) then (class h)#?(symbol source)
     else error ( toString h, " of class ", toString class h, " has no source" ))

target = (h) -> (
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
options Thing := X -> emptyOptionTable
options Function := OptionTable => function -> (
     if OptionsRegistry#?function then OptionsRegistry#function
     else emptyOptionTable
     )
options Symbol := s -> select(apply(pairs OptionsRegistry, (f,o) -> if o#?s then f), i -> i =!= null)

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
quit = new Command from (() -> exit 0)
erase symbol exit
exit = new Command from exitMethod

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


