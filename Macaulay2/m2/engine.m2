--		Copyright 1993-2002 by Daniel R. Grayson

-- basic type

RawObject.name = "RawObject"
RawObject.synonym = "raw object"

-- monomials

RawMonomial.name = "RawMonomial"
RawMonomial.synonym = "raw monomial"

RawMonomial == RawMonomial := (x,y) -> x === y
ZZ == RawMonomial := (i,x) -> x == i

expression RawMonomial := x -> (
     v := rawMonomialExponents x;
     if #v === 0 then expression 1
     else new Product from apply(v, (i,e) -> new Power from {vars i, e})
     )
net RawMonomial := x -> net expression x
degree RawMonomial := x -> rawDegree x
gcd(RawMonomial,RawMonomial) := (x,y) -> rawGCD(x,y)


-- monomial orderings

RawMonomialOrdering.name = "RawMonomialOrdering"
RawMonomialOrdering.synonym = "raw monomial ordering"

Eliminate = new SelfInitializingType of BasicList
new Eliminate from ZZ := (Eliminate,n) -> Eliminate {n}
expression Eliminate := v -> (
     if #v === 1 
     then new FunctionApplication from {Eliminate, v#0}
     else new FunctionApplication from {Eliminate, toList v})
ProductOrder = new SelfInitializingType of BasicList

net RawMonomialOrdering := o -> stack lines toString o

isSmall := i -> class i === ZZ and i < 2^15 and i > -2^15
isCount := i -> class i === ZZ and i >= 0 and i < 2^15
isListOfIntegers := x -> class x === List and all(x,i -> class i === ZZ)
isListOfListsOfIntegers := x -> class x === List and all(x,isListOfIntegers)
checkCount := i -> if not isCount i then error "expected a small positive integer"

fixup := method(SingleArgumentDispatch => true)
err := o -> error ("unrecognized ordering option " | toString o)

deglist := {}						    -- filled in below each time
getdegs := (m,n) -> (
     -- get m-th through n-th degree, using 1 when there aren't enough
     join ( take(deglist, {m,n}), (n - (#deglist - 1)):1)
     )
numvars := 0						    -- re-initialized below each time
varcount := 0						    -- re-initialized below each time
MonSize := 32						    -- re-initialized below each time
invert := false	    					    -- re-initialized below each time
bump := n -> varcount = varcount + n
optionSizes := hashTable {
     (Lex,8) => LexTiny,
     (Lex,16) => LexSmall,
     (GRevLex,8) => GRevLexTiny,
     (GRevLex,16) => GRevLexSmall
     }
fix := key -> if optionSizes#?(key,MonSize) then optionSizes#(key,MonSize) else key
optionInverses := hashTable {
     Lex => GroupLex,
     RevLex => GroupLex,				    -- sigh
     GRevLex => GroupLex				    -- sigh
     }
fix1 := key -> if invert and optionInverses#?key then optionInverses#key else key
intOption := (key,n) -> (
     key = fix fix1 key;
     checkCount n;
     bump n;
     key => n)
grevOption := (key,v) -> (
     key = fix key;
     if class v === ZZ then grevOption (key,getdegs(varcount, varcount+v-1))
     else if isListOfIntegers(v) then (
	  bump(#v);
	  key => v)
     else error "expected an integer or a list of integers")
optionFixes := hashTable {
     Weights => (key,val) -> key => val,
     Component => (key,val) -> key => val,
     Lex => intOption,
     LexSmall => intOption,
     LexTiny => intOption,
     RevLex => intOption,
     GroupLex => intOption,
     NCLex => intOption,
     GRevLex => grevOption,
     GRevLexSmall => grevOption,
     GRevLexTiny => grevOption
     }
     
ordOption := o -> fixup ( o => numvars - varcount )
symbolFixes := hashTable {
     Component => o -> Component => null,
     RevLex => ordOption,
     GRevLex => ordOption,
     Lex => ordOption,
     GRevLexSmall => ordOption,
     LexSmall => ordOption,
     GRevLexTiny => ordOption,
     LexTiny => ordOption
     }

fixup Thing := err
fixup List := v -> toSequence v / fixup
fixup Sequence := v -> v / fixup
fixup Eliminate := e -> Weights => getdegs(0, e#0-1)
fixup ProductOrder := o -> fixup(toSequence o / (i -> GRevLex => i))
fixup Symbol := o -> if symbolFixes#?o then symbolFixes#o o else err o
fixup ZZ := n -> fixup( GRevLex => n )
fixup Option := o -> (
     key := o#0;
     val := o#1;
     if optionFixes#?key then optionFixes#key (key,val)
     else error ("unrecognized ordering option keyword : " | toString key)
     )

makeMonomialOrdering = (monsize,inverses,nvars,degs,weights,ordering) -> (
     -- 'monsize' is the old MonomialSize option, usually 8 or 16, or 'null' if unset
     -- 'inverses' is true or false, and tells whether the old "Inverses => true" option was used.
     -- 'nvars' tells the total number of variables, as a minimum, meaning it could be 0 if
     --      you don't know.  Any extra variables will be ordered with GRevLex or GroupLex.
     -- 'degs' is a list of integers, the first components of the multi-degrees of the variables
     --      if it's too short, additional degrees are taken to be 1.  Could be an empty list.
     -- 'weights' is the list of integers or the list of lists of integers provided by the user under
     --    the *separate* Weights option.  Could be an empty list.
     -- 'ordering' is a list of ordering options, e.g., { Lex => 4, GRevLex => 4 }
     --    If it's not a list, we'll make a list of one element from it.
     if monsize =!= null then (
	  if class monsize =!= ZZ then error "expected an integer as MonomialSize option";
	  if monsize <= 8 then MonSize = 8
	  else if monsize <= 16 then MonSize = 16
	  else MonSize = 32);
     invert = inverses;
     if not isListOfIntegers degs then error "expected a list of integers";
     deglist = degs;
     varcount = 0;
     numvars = nvars;
     if isListOfListsOfIntegers weights then ()
     else if isListOfIntegers weights then weights = {weights}
     else error "expected a list of integers or a list of lists of small integers";
     if class ordering =!= List then ordering = {ordering};
     ordering = join(weights / (i -> Weights => i), ordering);
     t := toList splice fixup ordering;
     if varcount < nvars then t = append(t,fixup(GRevLex => nvars - varcount));
     rawMonomialOrdering t)

RawMonomialOrdering ** RawMonomialOrdering := RawMonomialOrdering => rawMonomialOrderingProduct

-- monoids

RawMonoid.name = "RawMonoid"
RawMonoid.synonym = "raw monoid"
net RawMonoid := o -> stack lines toString o

-- rings

RawRing.name = "RawRing"
RawRing.synonym = "raw ring"
net RawRing := o -> stack lines toString o
ZZ.RawRing = rawZZ()
--		Copyright 1993-2002 by Daniel R. Grayson

-- ring elements

RawRingElement.name = "RawRingElement"
RawRingElement.synonym = "raw ringe element"
RawRingElement == RawRingElement := (x,y) -> x === y

RawRing _ ZZ := (R,n) -> rawRingVar(R,n);
ZZ _ RawRing := (n,R) -> rawFromNumber(R,n)
RR _ RawRing := (n,R) -> rawFromNumber(R,n)
BigReal _ RawRing := (n,R) -> rawFromNumber(R,n)
RawRingElement _ RawRing := (x,R) -> rawPromote(R,x)


RawRingElement == RawRingElement := (x,y) -> x === y

ring RawRingElement := rawRing
degree RawRingElement := rawMultiDegree
size RawRingElement := rawTermCount
someTerms(RawRingElement,ZZ,ZZ) := (f,i,n) -> rawGetTerms(f,i,i+n-1)
leadCoefficient RawRingElement := rawLeadCoefficient
leadMonomial RawRingElement := rawLeadMonomial
denominator RawRingElement := rawDenominator
numerator RawRingElement := rawNumerator
isHomogeneous RawRingElement := rawIsHomogeneous
fraction(RawRing,RawRingElement,RawRingElement) := rawFraction

RawRingElement + ZZ := (x,y) -> x + y_(rawRing x)
ZZ + RawRingElement := (y,x) -> y_(rawRing x) + x
RawRingElement - ZZ := (x,y) -> x - y_(rawRing x)
ZZ - RawRingElement := (y,x) -> y_(rawRing x) - x
RawRingElement * ZZ := (x,y) -> x * y_(rawRing x)
ZZ * RawRingElement := (y,x) -> y_(rawRing x) * x
RawRingElement == ZZ := (x,y) -> x === y_(rawRing x)
ZZ == RawRingElement := (y,x) -> y_(rawRing x) === x

RawRingElement // ZZ := (x,y) -> x // y_(rawRing x)
ZZ // RawRingElement := (y,x) -> y_(rawRing x) // x

-- monomial ideals

RawMonomialIdeal.name = "RawMonomialIdeal"
RawMonomialIdeal.synonym = "raw monomial ideal"

-- free modules

RawFreeModule.name = "RawFreeModule"
RawFreeModule.synonym = "raw ring"

degrees RawFreeModule := rawMultiDegree

ZZ _ RawFreeModule := (i,F) -> (
     if i === 0 then rawZero F
     else error "expected integer to be 0"
     )

RawRing ^ ZZ := (R,i) -> rawFreeModule(R,i)
RawRing ^ List := (R,i) -> rawFreeModule(R,toSequence( - flatten splice i ))

rank RawFreeModule := rawRank

RawFreeModule ** RawFreeModule := rawTensor

-- vectors

RawVector.name = "RawVector"
RawVector.synonym = "raw ring"

RawFreeModule _ ZZ := (F,i) -> rawTerm(F,1_(rawRing F),i)
RawVector == RawVector := Boolean => (v,w) -> v === W

isHomogeneous RawVector := Boolean => rawIsHomogeneous
entries RawVector := List => rawVectorEntries
module RawVector := RawFreeModule => rawFreeModule
ring RawVector := RawRing => rawRing @@ rawFreeModule

-- matrices

RawMatrix.name = "RawMatrix"
RawMatrix.synonym = "raw matrix"

RawMatrix == RawMatrix := (v,w) -> v === w

RawVector == ZZ := 
RawMatrix == ZZ := (v,n) -> if n === 0 then rawIsZero v else error "comparison with nonzero integer"

ZZ == RawVector := 
ZZ == RawMatrix := (v,n) -> n == v

net RawMatrix := o -> stack lines toString o
target RawMatrix := o -> rawTarget o
source RawMatrix := o -> rawSource o
transposeSequence := t -> pack(#t, mingle t)
isHomogeneous RawMatrix := rawIsHomogeneous
entries RawMatrix := m -> (
     c := rawMatrixColumns m;
     if #c === 0
     then toList ( rank target m : {} )
     else transposeSequence ( rawVectorEntries \ c )
     )

ZZ * RawMatrix := (n,f) -> (
     R := rawRing rawTarget f;
     n_R * f
     )

RawMatrix ** RawMatrix := rawTensor

-- mutable matrices

RawMutableMatrix.name = "RawMutableMatrix"
RawMutableMatrix.synonym = "raw mutable matrix"

RawMutableMatrix == RawMutableMatrix := (v,w) -> v === w
net RawMutableMatrix := o -> stack lines toString o

-- ring maps

RawRingMap.name = "RawRingMap"
RawRingMap.synonym = "raw ring map"

RawRingMap == RawRingMap := (v,w) -> v === w
net RawRingMap := o -> stack lines toString o

-- lapck mutable matrices

LMatrixRR.name = "RawMatrixRR"
LMatrixRR.synonym = "raw real lapack matrix"

LMatrixRR == LMatrixRR := (v,w) -> v === w
net LMatrixRR := o -> stack lines toString o

LMatrixCC.name = "RawMatrixCC"
LMatrixCC.synonym = "raw complex lapack matrix"

LMatrixCC == LMatrixCC := (v,w) -> v === w
net LMatrixCC := o -> stack lines toString o

-- clean up

