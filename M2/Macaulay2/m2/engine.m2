--		Copyright 1993-2002 by Daniel R. Grayson

-- basic type

ReverseDictionary#RawObject = symbol RawObject
RawObject.synonym = "raw object"
raw RawObject := x -> error "'raw' received a raw object"
net RawObject := o -> net toString o

-- monomials

ReverseDictionary#RawMonomial = symbol RawMonomial
RawMonomial.synonym = "raw monomial"

RawMonomial == RawMonomial := (x,y) -> x === y
RawMonomial : RawMonomial := (x,y) -> rawColon(x,y)
ZZ == RawMonomial := (i,x) -> x == i

standardForm RawMonomial := m -> new HashTable from toList rawSparseListFormMonomial m
expression RawMonomial := x -> (
     v := rawSparseListFormMonomial x;
     if #v === 0 then expression 1
     else new Product from apply(v, (i,e) -> new Power from {vars i, e})
     )
exponents(ZZ,RawMonomial) := (nvars,x) -> (
     z := new MutableList from (nvars : 0);
     scan(rawSparseListFormMonomial x, (i,e) -> z#i = z#i + e);
     toList z)
net RawMonomial := x -> net expression x
degree RawMonomial := x -> error "degree of raw monomial not defined (no monoid)"
gcd(RawMonomial,RawMonomial) := (x,y) -> rawGCD(x,y)

-- monomial orderings

ReverseDictionary#RawMonomialOrdering = symbol RawMonomialOrdering
RawMonomialOrdering.synonym = "raw monomial ordering"

Eliminate = new SelfInitializingType of BasicList
new Eliminate from ZZ := (Eliminate,n) -> Eliminate {n}
expression Eliminate := v -> (
     if #v === 1 
     then new FunctionApplication from {Eliminate, v#0}
     else new FunctionApplication from {Eliminate, toList v})
ProductOrder = new SelfInitializingType of BasicList

isSmall := i -> class i === ZZ and i < 2^15 and i > -2^15
isCount := i -> class i === ZZ and i >= 0 and i < 2^15
isListOfIntegers := x -> class x === List and all(x,i -> class i === ZZ)
isListOfListsOfIntegers := x -> class x === List and all(x,isListOfIntegers)
checkCount := i -> if not isCount i then error "expected a small positive integer"

fixup := method(Dispatch => Thing)
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

processMonSize := monsize -> (
     if monsize =!= null then (
	  if class monsize =!= ZZ then error "expected an integer as MonomialSize option";
	  if monsize <= 8 then MonSize = 8
	  else if monsize <= 16 then MonSize = 16
	  else MonSize = 32))

optionSizes := hashTable {
     (Lex,8) => LexTiny,
     (Lex,16) => LexSmall,
     (GRevLex,8) => GRevLexTiny,
     (GRevLex,16) => GRevLexSmall
     }
fix := key -> if optionSizes#?(key,MonSize) then optionSizes#(key,MonSize) else key
optionInverses := hashTable {
     Lex => key -> GroupLex,
     RevLex => key -> GroupRevLex,
     GroupRevLex => key -> GroupRevLex,
     GroupLex => key -> GroupLex
--     GRevLex => key -> (
--	  newkey := GroupRevLex;
--	  stderr << "--warning: for rawMonomialOrdering, replacing monomial ordering " << key << " by " << newkey << endl;
--	  newkey)
     }
fix1 := key -> (
     if invert then (
	  if not optionInverses#?key then error ("Inverses => true not compatible with ",toString key);
	  optionInverses#key key)
     else key)
intOption := (key,n) -> (
     key = fix fix1 key;
     checkCount n;
     bump n;
     key => n)
grevOption := (key,v) -> (
     key = fix fix1 key;
     if class v === ZZ then v = toList (v:1);
     if not isListOfIntegers(v) then error "expected an integer or a list of integers";
     -- scan(v, i -> if i <= 0 then error "GRevLex expected positive weights"); -- why check this?
     bump(#v);
     key => v)
optionFixes := hashTable {
     Weights => (key,val) -> key => val,
     Position => (key,val) -> key => val,
     MonomialSize => (key,val) -> (processMonSize val; null),
     Lex => intOption,
     LexSmall => intOption,
     LexTiny => intOption,
     RevLex => intOption,
     GroupLex => intOption,
     GroupRevLex => intOption,
     NCLex => intOption,
     GRevLex => grevOption,
     GRevLexSmall => grevOption,
     GRevLexTiny => grevOption
     }
     
ordOption := o -> if numvars > varcount then fixup ( o => numvars - varcount )
symbolFixes := hashTable {
     -- Component has no other occurence in our code, so this must be something we never implemented:
     -- Position => o -> Component => null,
     GLex => o -> (Weights => numvars:1, fixup Lex),
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
fixup Eliminate := e -> Weights => toList (e#0 : 1)
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
     -- 'nvars' tells the total number of variables.  Any extra variables will be ordered with GRevLex or GroupLex.
     -- 'degs' is a list of integers, the first components of the multi-degrees of the variables
     --      if it's too short, additional degrees are taken to be 1.  Could be an empty list.
     -- 'weights' is the list of integers or the list of lists of integers provided by the user under
     --    the *separate* Weights option.  Could be an empty list.
     -- 'ordering' is a list of ordering options, e.g., { Lex => 4, GRevLex => 4 }
     --    If it's not a list, we'll make a list of one element from it.
     processMonSize monsize;
     invert = inverses;
     if not isListOfIntegers degs then error "expected a list of integers";
     deglist = degs;
     varcount = 0;
     numvars = nvars;
     weights = splice \ splice weights;
     if isListOfListsOfIntegers weights then null
     else if isListOfIntegers weights then weights = {weights}
     else error "Weights: expected a list of integers or a list of lists of small integers";
     scan(weights, wt -> if # wt != nvars then error("Weights: expected weight vector of length ",toString nvars," but got ",toString (#wt)));
     if class ordering =!= List then ordering = {ordering};
     ordering = join(weights / (i -> Weights => i), ordering);
     t := toList splice nonnull fixup ordering;
     if varcount < nvars then t = append(t,fixup(GRevLex => nvars - varcount));
     if not any(t, x -> class x === Option and x#0 === Position) then t = append(t, Position => Up);
     logmo := new FunctionApplication from {rawMonomialOrdering,t};
     (t,value logmo, logmo))

RawMonomialOrdering ** RawMonomialOrdering := RawMonomialOrdering => rawProductMonomialOrdering

-- monoids

ReverseDictionary#RawMonoid = symbol RawMonoid
RawMonoid.synonym = "raw monoid"

-- rings

ReverseDictionary#RawRing = symbol RawRing
RawRing.synonym = "raw ring"
ZZ.RawRing = rawZZ()

-- ring elements (polynomials)

ReverseDictionary#RawRingElement = symbol RawRingElement
RawRingElement.synonym = "raw ring element"
RawRingElement == RawRingElement := (x,y) -> x === y

RawRing _ ZZ := (R,n) -> rawRingVar(R,n)
raw Number := x -> x_((class x).RawRing)
raw BigNumber := x -> x_((ring x).RawRing)
Number _ RawRing := (n,R) -> rawFromNumber(R,n)
RawRingElement _ RawRing := (x,R) -> rawPromote(R,x)

RawRingElement == RawRingElement := (x,y) -> x === y

ring RawRingElement := rawRing
degree RawRingElement := rawMultiDegree
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
RawRingElement == ZZ := (x,y) -> if y === 0 then rawIsZero x else x === y_(rawRing x)
ZZ == RawRingElement := (y,x) -> if y === 0 then rawIsZero x else y_(rawRing x) === x

RawRingElement // ZZ := (x,y) -> x // y_(rawRing x)
ZZ // RawRingElement := (y,x) -> y_(rawRing x) // x

compvals := hashTable { 0 => symbol == , 1 => symbol > , -1 => symbol < }
comparison := n -> compvals#n
RawRingElement ? RawRingElement := (f,g) -> comparison rawCompare(f,g)

-- monomial ideals

ReverseDictionary#RawMonomialIdeal = symbol RawMonomialIdeal
RawMonomialIdeal.synonym = "raw monomial ideal"

-- free modules

ReverseDictionary#RawFreeModule = symbol RawFreeModule
RawFreeModule.synonym = "raw ring"

RawFreeModule ++ RawFreeModule := rawDirectSum

degrees RawFreeModule := rawMultiDegree

ZZ _ RawFreeModule := (i,F) -> (
     if i === 0 then rawZero(F,F,0)
     else error "expected integer to be 0"
     )

RawRing ^ ZZ := (R,i) -> rawFreeModule(R,i)
RawRing ^ List := (R,i) -> (
     i = splice i;
     v := - flatten i;
     if #v === 0 then rawFreeModule(R, #i ) 
     else rawFreeModule(R,toSequence v ))

rank RawFreeModule := rawRank

RawFreeModule == RawFreeModule := (v,w) -> v === w
RawFreeModule ** RawFreeModule := rawTensor

-- matrices

ReverseDictionary#RawMatrix = symbol RawMatrix
RawMatrix.synonym = "raw matrix"

ReverseDictionary#RawMutableMatrix = symbol RawMutableMatrix
RawMutableMatrix.synonym = "raw mutable matrix"

rawExtract = method()

rawExtract(RawMatrix,ZZ,ZZ) := 
rawExtract(RawMutableMatrix,ZZ,ZZ) := (m,r,c) -> rawMatrixEntry(m,r,c)

rawExtract(RawMatrix,Sequence,Sequence) := 
rawExtract(RawMutableMatrix,Sequence,Sequence) := (m,r,c) -> rawSubmatrix(m,splice r,splice c)

RawMatrix _ Sequence := 
RawMutableMatrix _ Sequence := (m,rc) -> ((r,c) -> rawExtract(m,r,c)) rc

rawExtractColumns = method()
rawExtractColumns(RawMatrix,Sequence) :=
rawExtractColumns(RawMutableMatrix,Sequence) := (m,c) -> rawExtract(m, 0 .. rank target m - 1, c)

RawMutableMatrix == RawMutableMatrix := RawMatrix == RawMatrix := rawIsEqual

RawMatrix == ZZ := RawMutableMatrix == ZZ := (v,n) -> if n === 0 then rawIsZero v else error "comparison with nonzero integer"
ZZ == RawMatrix := ZZ == RawMutableMatrix := (n,v) -> if n === 0 then rawIsZero v else error "comparison with nonzero integer"

target RawMatrix := o -> rawTarget o
source RawMatrix := o -> rawSource o
transposeSequence := t -> pack(#t, mingle t)
isHomogeneous RawMatrix := rawIsHomogeneous
entries RawMutableMatrix := entries RawMatrix := m -> table(rawNumberOfRows m,rawNumberOfColumns m,(i,j)->rawMatrixEntry(m,i,j))

ZZ * RawMatrix := (n,f) -> (
     R := rawRing rawTarget f;
     n_R * f
     )
QQ * RawMatrix := (n,f) -> (
     R := rawRing rawTarget f;
     n_R * f
     )

RawMatrix ** RawMatrix := rawTensor

rawConcatColumns = (mats) -> rawConcat toSequence mats
rawConcatRows = (mats) -> rawDual rawConcat apply(toSequence mats,rawDual)
rawConcatBlocks = (mats) -> rawDual rawConcat apply(toSequence mats, row -> rawDual rawConcat toSequence (raw \ row))

new RawMatrix from RawRingElement := (RawMatrix,f) -> rawMatrix1(rawFreeModule(ring f,1),1,1:f,0)
new RawMatrix from RawMutableMatrix := rawMatrix
new RawMutableMatrix from RawMatrix := rawMutableMatrix

RawMutableMatrix _ Sequence = (M,ij,val) -> ((i,j) -> (rawSetMatrixEntry(M,i,j,val); val)) ij

degree RawMatrix := rawMultiDegree
degrees RawMatrix :=f -> {rawMultiDegree rawTarget f,rawMultiDegree rawSource f}

-- computations

ReverseDictionary#RawComputation = symbol RawComputation
RawComputation.synonym = "raw computation"
status RawComputation := opts -> c -> rawStatus1 c
RawComputation_ZZ := (C,i) -> rawResolutionGetMatrix(C,i)

-- Groebner bases

show = method()
RawMatrix % RawComputation := (m,g) -> rawGBMatrixRemainder(g,m)
show RawComputation := C -> rawShowComputation C

-- ring maps

ReverseDictionary#RawRingMap = symbol RawRingMap
RawRingMap.synonym = "raw ring map"
RawRingMap == RawRingMap := (v,w) -> v === w

-- clean up


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
