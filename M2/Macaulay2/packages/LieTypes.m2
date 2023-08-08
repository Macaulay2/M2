-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
    "LieTypes",
    Version => "0.81",
    Date => "Jan 22, 2023",
    Headline => "common types and methods for Lie groups and Lie algebras",
    Authors => {
	  {Name => "Dave Swinarski", Email => "dswinarski@fordham.edu"},
	  {
	      Name => "Paul Zinn-Justin", -- starting with version 0.6
	      Email => "pzinn@unimelb.edu.au",
	      HomePage => "http://blogs.unimelb.edu.au/paul-zinn-justin/"}
	  },
    Keywords => {"Lie Groups and Lie Algebras"},
    PackageImports => {"ReesAlgebra"},
    DebuggingMode => false,
    Certification => {
	 -- same article as for package ConformalBlocks
	  "journal name" => "The Journal of Software for Algebra and Geometry",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Software for computing conformal block divisors on bar M_0,n",
	  "acceptance date" => "2 August 2018",
	  "published article URI" => "https://msp.org/jsag/2018/8-1/p08.xhtml",
	  "published article DOI" => "10.2140/jsag.2018.8.81",
	  "published code URI" => "https://msp.org/jsag/2018/8-1/jsag-v8-n1-x08-LieTypes.m2",
	  "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/LieTypes.m2",
	  "release at publication" => "923fbcc7c77b23f510bb0d740e00fc1722a2f397",	    -- git commit number in hex
	  "version at publication" => "0.5",
	  "volume number" => "8",
	  "volume URI" => "https://msp.org/jsag/2018/8-1/"
	  }
    )

export {
    --for the LieAlgebra type:
    "LieAlgebra",
    "simpleLieAlgebra",
    "dualCoxeterNumber", 
    "highestRoot",
    "starInvolution",
    "killingForm",
    "weylAlcove",
    "positiveRoots",
    "positiveCoroots",
    "simpleRoots",
    "dynkinDiagram",
    "isSimple",
    "cartanMatrix",
    "𝔞", "𝔟", "𝔠", "𝔡", "𝔢", "𝔣", "𝔤",
    "subLieAlgebra",
    --for the LieAlgebraModule type
    "LieAlgebraModule", 
    "irreducibleLieAlgebraModule", "LL", "ω",
--    "isIsomorphic",
    "casimirScalar",
    "weightDiagram",
    "tensorCoefficient",
    "fusionProduct",
    "fusionCoefficient",
--    "MaxWordLength",
    "LieAlgebraModuleFromWeights",
    "trivialModule",
    "adjointModule",
    "isIrreducible",
    "character",
    "adams",
    "qdim",
    "branchingRule"
    }

-- Access hasAttribute, getAttribute
debug Core
-*
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Summary, Version 0.1, August 2012
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

We define two types that are used by the ConformalBlocks package:
LieAlgebra
LieAlgebraModule

Objects of both types are hash tables.

LieAlgebras have two keys: RootSystemType, LieAlgebraRank
The functions available for LieAlgebras are:
simpleLieAlgebra
dualCoxeterNumber
highestRoot
simpleRoots
simpleCoroots
positiveRoots
starInvolution
killingForm
weylAlcove
cartanMatrix
subLieAlgebra
directSum

LieAlgebraModules have two keys: LieAlgebra and DecompositionIntoIrreducibles
The functions available for LieAlgebraModules are:
dimension
weights
casimirScalar
tensor product decomposition
fusion coefficient
branching rules
trivial module, adjoint module

Most of the lines of code below are to implement 
* Freudenthal's formula for the multiplicity of a weight w in the irreducible g-module with highest weight v
* the Racah-Speiser algorithm for computing tensor product decompositions
* the Kac-Walton algorithm for computing fusion product decompositions 
Many of these functions are copied over from early versions of Swinarski's ConformalBlocks package.  

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Summary, Version 0.5, June 2018
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

Fixed a minor bug in multiplicity function (needed to allow for options, since multiplicity is 
a method with options.)  Changed the LieAlgebra and LieAlgebraModule classes to print out the
global variable names instead of the hash table contents. 

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Summary, Version 0.6, January 2023
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

* Improved output methods
* Introduced shorthand LL for simple (irreducible) modules
* Fixed and exported LieAlgebraModuleFromWeights
* Fixed and optimized tensor product of modules
* Added ^** and ^ for modules
* Added/exported trivialModule
* Additional sanity checks
* Allow inputting weights as vectors
* isIrreducible is now a method
* use of VirtualTally rather than HashTable for its methods
* Added/exported character method
* character and weightDiagram have 4 strategies, JacobiTrudi, JacobiTrudi', Weyl and Freudenthal
  (Weyl seems slower for small reps, but significantly faster for large highest weights)
* adams, symmetricPower, exteriorPower added/exported
* added PZJ as coauthor

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Summary, Version 0.7, January 2023
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
* replaced isIsomorphic with ==, turned isSimple into a method
* fixed and optimized fusionProduct
* added PZJ as author due to novel implementation of Kac-Watson
* Weyl product formula for dim and qdim (principal specialization)
* added/exported method dynkinDiagram
* added/exported method adjointModule
* reintroduced and exported cartanMatrix, cartanMatrixQQ now calls cartanMatrix
* allow alternate ordering of arguments of weylAlcove, irreducibleModule to fix inconsistency

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Summary, Version 0.8, January 2023
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
* fraktur for shorthand of Lie algebras
* semi-simple Lie algebras are possible, use ++
* subLieAlgebra, branchingRule, supports general semi-simple Lie algebras
* define a Lie algebra based on its Cartan matrix
* M @ M' for tensor product of modules over different Lie algebras
* improved caching of characters

*-


-- hopefully will be integrated into Core: cache into i^th argument
cacheValue' = (i,key) -> f -> new CacheFunction from ( x -> (
c:=(x#i).cache;
key':=replace(i,key,x);
if c#?key' then c#key' else c#key'=f x
) )

-- helper functions for semisimple Lie algebras
split := (w,L) -> ( -- split weight of semisimple algebra according to simple parts
    L=prepend(0,accumulate(plus,0,L)); -- why does accumulate suck
    apply(#L-1,i->w_(toList(L#i..L#(i+1)-1)))
    )
unsplit = (v,L,i) -> ( -- from a weight of one summand to the whole
    toList(sum(i,j->L#j):0) | v | toList(sum(i+1..#L-1,j->L#j):0)
    )

-----------------------------------------------------------------------
-- LieAlgebra= {
--   LieAlgebraRank => ZZ | Sequence, dim of Cartan subalgebra
--   RootSystemType => String | Sequence, type A through G
--   }

LieAlgebra = new Type of HashTable  
LieAlgebra.GlobalAssignHook = globalAssignFunction
LieAlgebra.GlobalReleaseHook = globalReleaseFunction

cartanMatrixQQ := (type, m) -> promote(cartanMatrix(type,m),QQ)
characterRing = method()
characterRing (String,ZZ) := memoize( (type,m) -> (
    Q:=sum \ entries inverse cartanMatrixQQ(type,m);
    l:=lcm(denominator\Q);
    Q=apply(Q,q->lift(q*l,ZZ));
    x:=getSymbol "x";
    ZZ(monoid [x_1..x_m,Inverses=>true,MonomialOrder=>{Weights=>Q,Lex}])
    ))
characterRing (Sequence,Sequence) := memoize( (type,m) -> if #m == 0 then ZZ[Inverses=>true,MonomialOrder=>Lex] else ( -- tensor apply(type,m,characterRing))
	R := tensor apply(type,m,characterRing);
	vrs := split(gens R,m);
	R#"maps" = apply(#m, i -> map(R,characterRing(type#i,m#i),vrs#i)); -- ideally this should be generated automatically by tensor
	R
	))

characterRing LieAlgebra := g -> characterRing(g#"RootSystemType",g#"LieAlgebraRank")

simpleLieAlgebra = method(
    TypicalValue => LieAlgebra
    )
simpleLieAlgebra(String,ZZ) := (type,m) -> (
    if not isSimple(type,m) then (
    	if not member(type,{"A","B","C","D","E","F","G"}) then error "The simple Lie algebras over the complex numbers have types A, B, C, D, E, F, or G";
    	if type=="A" and m<= 0 then error "The rank for type A must be >= 1.";
    	if type=="B" and m<= 1 then error "The rank for type B must be >= 2.";
    	if type=="C" and m<= 1 then error "The rank for type C must be >= 2.";
    	if type=="D" and m<= 2 then error "The rank for type D must be >= 3.";
    	if type=="E" and not member(m,{6,7,8}) then error "The rank for type E must be 6, 7, or 8.";
    	if type=="F" and m!=4 then error "The rank for type F must be 4.";
    	if type=="G" and m!=2 then error "The rank for type G must be 2.";
	);
    new LieAlgebra from {
	"LieAlgebraRank"=>m,
	"RootSystemType"=>type,
	subLieAlgebra => hashTable { null => id_(ZZ^m) } -- any Lie algebra is a subalgebra of itself... but we can't hardcode it cause can't have a loop in an immutable HashTable... annoying
	}
    )

fraktur := hashTable { ("A",𝔞),("B",𝔟),("C",𝔠),("D",𝔡),("E",𝔢),("F",𝔣),("G",𝔤) }
describe1 := (type,m) -> (hold fraktur#(type))_m
describe LieAlgebra := g -> Describe (
    if isSimple g then describe1(g#"RootSystemType",g#"LieAlgebraRank")
     else DirectSum apply(g#"RootSystemType",g#"LieAlgebraRank",describe1)
     )

expression LieAlgebra := g -> (
    if hasAttribute(g,ReverseDictionary) then expression getAttribute(g,ReverseDictionary)
    else unhold describe g
    )
net LieAlgebra := net @@ expression;
texMath LieAlgebra := texMath @@ expression;

LieAlgebra ++ LieAlgebra := directSum
directSum LieAlgebra := identity
LieAlgebra.directSum = args -> if #args == 1 then args#0 else (
    subList := apply(args, g -> g#subLieAlgebra);
    subs := null;
    scan(subList, s -> subs = if subs===null then applyKeys(s,sequence) else combine(subs,s,append,directSum,identity)); -- collisions shouldn't occur
    new LieAlgebra from {
    "RootSystemType" => join apply(args, g -> sequence g#"RootSystemType" ),
    "LieAlgebraRank" => join apply(args, g -> sequence g#"LieAlgebraRank" ),
    subLieAlgebra => applyKeys(subs,s->if all(s,h->h===null) then null else directSum apply(#args,i->if s#i===null then args#i else s#i)) -- messy; collisions shouldn't happen because identity embedding excluded
    })

rank LieAlgebra := g -> plus sequence g#"LieAlgebraRank"

isSimple = method(TypicalValue => Boolean)
isSimple (String,ZZ) := (type,m) -> (
    (type=="A" and m>=1)
    or ((type=="B" or type=="C") and m>=2)
    or (type=="D" and m>=3)
    or (type=="E" and m>=6 and m<=8)
    or (type=="F" and m==4)
    or (type=="G" and m==2)
    )
isSimple LieAlgebra := g -> class g#"RootSystemType" === String and class g#"LieAlgebraRank" === ZZ and isSimple(g#"RootSystemType",g#"LieAlgebraRank") -- should we test each time?

dynkinDiagram = method(TypicalValue => Net)
dynkinA := (l,m,flag) -> stack ( -- flag = part of diagram
    (if flag then "---" else "") | demark("---",m-l+1:"o"),
    concatenate apply(l..m,i->if i==l and not flag then toString l else pad(4,toString i))
    )
dynkinDiagram (String,ZZ,ZZ) := (type,m,shift) -> if not isSimple(type,m) then error "can only draw simple Lie algebra Dynkin diagram" else (
    if type=="A" then dynkinA (1+shift,m+shift,false)
    else if type=="B" then dynkinA (1+shift,m-1+shift,false) | ("=>=o"||pad(4,toString(m+shift)))
    else if type=="C" then dynkinA (1+shift,m-1+shift,false) | ("=<=o"||pad(4,toString(m+shift)))
    else if type=="D" then dynkinA (1+shift,m-2+shift,false) | ((" o"|toString(m-1+shift))||"/"||""||"\\"||(" o"|toString(m+shift)))^2
    else if type=="E" then "        o 2"||"        |"|| (dynkinA (1+shift,1+shift,false)|dynkinA(3+shift,m+shift,true))
    else if type=="F" then dynkinA (1,2,false) | ("=>=o---o"||"   3   4")
    else if type=="G" then "o≡<≡o"||(toString(shift+1)|pad(4,toString(shift+2)))
    )
dynkinDiagram (String,ZZ) := (type,m) -> dynkinDiagram(type,m,0)
dynkinDiagram LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if isSimple g then dynkinDiagram(type,m) else (
    	L:=prepend(0,accumulate(plus,0,m)); -- why does accumulate suck
    	horizontalJoin between("   ",apply(#m,i->dynkinDiagram(type#i,m#i,L#i)))
	)
    )

LieAlgebra == LieAlgebra := (V,W)-> (V===W)

dualCoxeterNumber = method(
    TypicalValue => ZZ
    )
dualCoxeterNumber(String,ZZ) := memoize((type,m) -> (--see Appendix 13.A, [DMS]
    if type == "A" then return m+1;
    if type == "B" then return 2*m-1;
    if type == "C" then return m+1;
    if type == "D" then return 2*m-2;
    if type == "E" and m==6 then return 12;
    if type == "E" and m==7 then return 18;
    if type == "E" and m==8 then return 30;
    if type == "F" then return 9;
    if type == "G" then return 4
    ))
dualCoxeterNumber(LieAlgebra) := (g) -> (--see Appendix 13.A, [DMS]
    if not isSimple g then error "Lie algebra not simple";
    dualCoxeterNumber(g#"RootSystemType",g#"LieAlgebraRank")
    )


highestRoot = method(
    TypicalValue => List
    )
highestRoot(String,ZZ) := memoize((type, m) -> (--see Appendix 13.A, [DMS]
    if type == "A" and m==1 then return {2};
    if type == "A" and m >= 2 then return flatten {{1}, apply(m-2,i->0),{1}};
    if type == "B" and m==2 then return flatten {0,2};
    if type == "B" and m>=3 then return flatten {{0},{1}, apply(m-2,i->0)};
    if type == "C" then return flatten {{2}, apply(m-1,i->0)};
    if type == "D" and m==3 then return {0,1,1};
    if type == "D" and m>=4 then return flatten {{0},{1}, apply(m-2,i->0)};
    --July 2011: changed numbering of nodes in Dynkin diagram to match WeylGroups
    if type == "E" and m==6 then return {0,1,0, 0,0,0};
    if type == "E" and m==7 then return {1,0,0,0, 0,0,0};
    if type == "E" and m==8 then return {0,0,0,0, 0,0,0,1};
    if type == "F" then return {1,0,0,0};
    if type == "G" then return {0,1}
))

highestRoot(Sequence,Sequence):=memoize((type,m)-> join apply(type,m,highestRoot))

highestRoot(LieAlgebra) := (g) -> highestRoot(g#"RootSystemType",g#"LieAlgebraRank")

starInvolution = method()
starInvolution(String,ZZ,List) := (type, m, w) ->  ( N:=#w;
    if type == "A" then return reverse w;
    if type == "B" or type == "C" or type == "F" or type == "G" then return w;
    if type == "E" and m!= 6 then return w;
    if type == "D" and even(m) then return w;
    if type == "D" and odd(m) then (x:=w;
        return append(drop(x,{#x-2,#x-2}),w_(#w-2)));
    if type == "E" and m== 6 then return {w_5,w_1,w_4,w_3,w_2,w_0};
    )
starInvolution(Sequence,Sequence,List) := (type,m,w) -> (
    w = split(w,m);
    flatten apply(#w, i -> starInvolution(type#i,m#i,w#i))
)

starInvolution(LieAlgebra,List) := (g,v) -> starInvolution(g#"RootSystemType",g#"LieAlgebraRank",v)
starInvolution(LieAlgebra,Vector) := (g,v) -> starInvolution(g,entries v)

starInvolution(Vector,LieAlgebra) :=
starInvolution(List,LieAlgebra) := (v,g) -> starInvolution(g,v) -- for backwards compat

-- shorthand notation
scan(pairs fraktur, (let,sym) ->
    globalAssign(sym, new ScriptedFunctor from { subscript => n -> simpleLieAlgebra(let,n), symbol texMath => "\\mathfrak "|replace(".","\\L$&",let)})
    )

LieAlgebra#AfterPrint = g -> (
    if isSimple g then "simple ",
    class g,
    if #(g#subLieAlgebra)>1 then (
	lst := nonnull keys g#subLieAlgebra; -- list of supalgebras
	mins := select(lst, h -> not any(lst, k -> k#subLieAlgebra#?h)); -- find minimal elements
	", subalgebra of ",
	toSequence between(", ",mins)
	)
 )


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- The LieAlgebraModule type
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------


-- LieAlgebraModule= {
--   LieAlgebra => 
--   }
--Functions: weights, dimension, **

LieAlgebraModule = new Type of HashTable 
LieAlgebraModule.GlobalAssignHook = globalAssignFunction
LieAlgebraModule.GlobalReleaseHook = globalReleaseFunction
LL = new ScriptedFunctor from { subscript => w -> g -> irreducibleLieAlgebraModule(g,w) }
LL.texMath = ///{\mathcal L}///

describe LieAlgebraModule := M -> Describe (
    dec := M#"DecompositionIntoIrreducibles";
    g := Parenthesize expression M#"LieAlgebra";
    if #dec == 0 then expression 0
    else DirectSum apply(sort pairs dec,(v,mul) -> ((expression LL)_(unsequence toSequence v) g)^mul)
    )
expression LieAlgebraModule := M -> if hasAttribute(M,ReverseDictionary) then expression getAttribute(M,ReverseDictionary) else unhold describe M;

net LieAlgebraModule := net @@ expression
texMath LieAlgebraModule := texMath @@ expression

new LieAlgebraModule from Sequence := (T,s) -> new LieAlgebraModule from {
    "LieAlgebra" => s#0,
    "DecompositionIntoIrreducibles" => if class s#1 === VirtualTally then s#1 else new VirtualTally from s#1,
    cache => new CacheTable
    }

--simpleLieAlgebra LieAlgebraModule := M -> M#"LieAlgebra" -- no longer works now Lie algebras aren't always simple

LieAlgebraModule_ZZ := (M,i) -> irreducibleLieAlgebraModule(M#"LieAlgebra",(sort keys M#"DecompositionIntoIrreducibles")#i)
LieAlgebraModule_* := M -> apply(sort keys M#"DecompositionIntoIrreducibles", v -> irreducibleLieAlgebraModule(M#"LieAlgebra",v))

isIrreducible = method()
isIrreducible LieAlgebraModule := M -> values M#"DecompositionIntoIrreducibles" == {1}

LieAlgebraModule ^ ZZ :=
LieAlgebraModule ^ QQ := (M,q) -> (
    if q==1 then M
    else new LieAlgebraModule from (
	M#"LieAlgebra",
	if q==0 then {} else applyValues(M#"DecompositionIntoIrreducibles", a -> try lift(a*q,ZZ) else error "multiplicity not integer")
	)
)

LieAlgebraModule#AfterPrint = M -> (
    if isIrreducible M then "irreducible "
    else if any(values M#"DecompositionIntoIrreducibles",a->a<0) then "virtual ",
    class M,
    " over ",
    M#"LieAlgebra"
 )

trivialModule = method(TypicalValue => LieAlgebraModule)
trivialModule LieAlgebra := g -> irreducibleLieAlgebraModule(toList(rank g:0),g)

LieAlgebraModule ^** ZZ := (cacheValue'(0,symbol ^**)) ((M,n) -> (
	if n<0 then "error nonnegative powers only";
    	if n==0 then trivialModule M#"LieAlgebra"
    	else if n==1 then M
    	else M**(M^**(n-1)) -- order matters for speed purposes
    ))

adjointWeight := (type,m) -> splice (
    if type == "A" then if m==1 then {2} else {1,m-2:0,1}
    else if type == "B" then if m==2 then {0,2} else {0,1,m-2:0}
    else if type == "C" then {2,m-1:0}
    else if type == "D" then if m==3 then {0,1,1} else {0,1,m-2:0}
    else if type == "E" then if m==6 then {0,1,4:0} else if m==7 then {1,6:0} else {7:0,1}
    else if type == "F" then {1,3:0}
    else if type == "G" then {0,1}
    )

adjointModule = method(TypicalValue => LieAlgebraModule)
adjointModule LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if isSimple g then irreducibleLieAlgebraModule(g,adjointWeight(type,m))
    else new LieAlgebraModule from (g, tally apply(#m, i -> unsplit(adjointWeight(type#i,m#i),m,i)))
    )

dim LieAlgebra := g -> dim adjointModule g

starInvolution LieAlgebraModule := M -> (
    g:=M#"LieAlgebra";
    new LieAlgebraModule from (
    	g,
    	applyKeys(M#"DecompositionIntoIrreducibles", v -> starInvolution(g,v))
	)
    )
dual LieAlgebraModule := {} >> o -> lookup(starInvolution,LieAlgebraModule)



LieAlgebraModule == LieAlgebraModule := (V,W)-> (V===W)

-*
isIsomorphic = method(
    TypicalValue => Boolean
    )
isIsomorphic(LieAlgebraModule,LieAlgebraModule) := (M,N) -> ( -- actually this is the same as ===
    if M#"LieAlgebra" != N#"LieAlgebra" then return false;
    M#"DecompositionIntoIrreducibles"===N#"DecompositionIntoIrreducibles"
)
*-

LieAlgebraModule == ZZ := (M,n) -> if n=!=0 then error "attempted to compare module to nonzero integer" else #(M#"DecompositionIntoIrreducibles") == 0

directSum LieAlgebraModule := identity
LieAlgebraModule.directSum = args -> (
    if not same apply(args, M -> M#"LieAlgebra") then error "modules must be over the same Lie algebra";
    new LieAlgebraModule from (
	(first args)#"LieAlgebra",
	sum(args,M->M#"DecompositionIntoIrreducibles")
	)
)
LieAlgebraModule ++ LieAlgebraModule := directSum

ωsub := i -> Subscript{symbol ω,i};
ω=new ScriptedFunctor from { subscript => ωsub }
irreducibleLieAlgebraModule = method(
    TypicalValue => LieAlgebraModule
    )
irreducibleLieAlgebraModule(LieAlgebra,List) := (g,v) -> (
    v = deepSplice v;
    if #v != rank g or not all(v, a -> class a === ZZ) then error "invalid highest weight";
    new LieAlgebraModule from (g,{v => 1})
    )
irreducibleLieAlgebraModule(LieAlgebra,VisibleList) := (g,v) -> irreducibleLieAlgebraModule(g,toList v)
irreducibleLieAlgebraModule(LieAlgebra,Vector) := (g,v) -> irreducibleLieAlgebraModule(g,entries v)
irreducibleLieAlgebraModule(LieAlgebra,ZZ) := (g,v) -> irreducibleLieAlgebraModule(g,{v})
irreducibleLieAlgebraModule(LieAlgebra,Expression) := (g,v) -> (
        ω.subscript = i -> apply(rank g,j->if j+1==i then 1 else 0 );
        irreducibleLieAlgebraModule(g,first(value v,ω.subscript=ωsub))
    )
irreducibleLieAlgebraModule(Thing,LieAlgebra) := (v,g) -> irreducibleLieAlgebraModule(g,v)

-*-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Private functions for LieAlgebraModule
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

We implement the Lie theoretic ingredients needed to compute the weights in an irreducible Lie algebra module and their multiplicities
We need: 
--a list of the positive roots
--the ability to compute casimirScalars
---->To get casimirScalars, we need the so-called quadratic form matrix, which can be looked up or computed from the Cartan matrix

Cartan matrices and the Killing form are also implemented in the WeylGroups package.  I am using my own 
implementations because I want the Cartan matrix over QQ (so I can invert it) and so that the Killing form is scaled to make 
(theta,theta) = 2, where theta is the highest root.  This is a popular convention in the conformal blocks literature that is not used in WeylGroups. 

To avoid shadowing, I have named my function cartanMatrixQQ

PZJ: actually there's so much shadowing already...

*-

cartanMatrix = method ( TypicalValue => Matrix )

cartanMatrix LieAlgebra := g -> cartanMatrix (g#"RootSystemType",g#"LieAlgebraRank")

cartanMatrix(Sequence,Sequence) := memoize((type,m) -> directSum apply(type,m,cartanMatrix))

cartanMatrix (String,ZZ) := memoize((type, m) -> (
    if not isSimple(type,m) then error "not simple type";
    M:={};
    if type=="A" then (
        return matrix apply(m, i-> apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0))
    );
    if type=="B" then (
        M = apply(m-2, i ->  apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M, apply(m, j -> if j==(m-2)-1 then -1 else if j==(m-2)then 2 else if j==(m-2)+1 then -2 else 0));
        M = append(M, apply(m, j -> if j==(m-1)-1 then -1 else if j==(m-1) then 2 else if j==(m-1)+1 then -1 else 0));
        return matrix M
    );
    if type=="C" then (
        M = apply(m-2, i -> apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M, apply(m, j -> if j==m-2-1 then -1 else if j==m-2 then 2 else if j==m-2+1 then -2 else 0));
        M = append(M, apply(m, j -> if j==m-1-1 then -1 else if j==m-1 then 2 else if j==m-1+1 then -1 else 0));
        return transpose matrix M
    );
    if type=="D" then (
        M = apply(m-3, i -> apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M,apply(m, j -> if j==m-3-1 then -1 else if j==m-3 then 2 else if j==m-3+1 then -1 else if j==m-3+2 then -1 else 0));
        M = append(M,apply(m, j -> if j==m-2 then 2 else if j==m-2-1 then -1 else 0));
        M = append(M,apply(m, j -> if j==m-1 then 2 else if j==m-1-2 then -1 else 0));
        return matrix M
    );
    if type=="E" and m==6 then (
        return matrix {{2, 0, -1, 0, 0, 0}, {0, 2, 0, -1, 0, 0}, {-1, 0, 2, -1, 0, 0}, {0, -1, -1, 2, -1, 0}, {0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, -1, 2}});
    if type=="E" and m==7 then (
	return matrix {{2, 0, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0}, {0, -1, -1, 2, -1, 0, 0}, {0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, -1, 2}});
    if type=="E" and m==8 then (
	return matrix {{2, 0, -1, 0, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0, 0}, {0, -1, -1, 2, -1, 0, 0, 0}, {0, 0, 0, -1, 2, -1, 0, 0}, {0, 0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, 0, -1, 2}});
    if type == "F" then return matrix({{2,-1,0,0},{-1,2,-2,0},{0,-1,2,-1},{0,0,-1,2}});
    if type == "G" then return matrix({{2,-1},{-3,2}});
    ))


--We code what Di Francesco, Mathieu, and Senechal call the quadratic form matrix
--For types A,D,E, it is the inverse of the Cartan matrix.  See paragraph 1, [DMS] p. 498 and (13.51), [DMS] p. 499 
--For the other types Appendix 13.A, [DMS]

quadraticFormMatrix = method ( TypicalValue => Matrix )

quadraticFormMatrix LieAlgebra := g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    quadraticFormMatrix (type,m) 
    )

quadraticFormMatrix (Sequence,Sequence) := memoize((type,m) -> directSum apply(type,m,quadraticFormMatrix))

quadraticFormMatrix (String,ZZ) := memoize((type, m) -> ( M:={};
    if type=="A" or type =="D" or type=="E" then return (cartanMatrixQQ(type,m))^-1;
    if type =="B" then (
        M=apply(m-1, i -> append(apply(m-1, j -> if j+1<=i+1 then 2*(j+1) else 2*(i+1 )),i+1));
	M = append(M,append(apply(m-1,j->j+1),m/2));
	return (1/2)*matrix(M) 
	);
    if type =="C" then (
	M=apply(m, i -> apply(m, j -> if j+1<=i+1 then (j+1)/1 else (i+1 )));
	return (1/2)*matrix(M)
	);
    if type =="F" then return matrix {{2,3,2,1},{3,6,4,2},{2,4,3,3/2},{1,2,3/2,1}};
    if type =="G" then return matrix {{2/3,1},{1,2}}
    ))

killingForm = method(
    TypicalValue => QQ
    )
killingForm(Sequence,Sequence,List,List) :=
killingForm(String,ZZ,List,List) := memoize((type, m, v,w) -> (
    (matrix{v}*quadraticFormMatrix(type,m)*matrix transpose{w})_(0,0)
))
--killingForm(String,ZZ,Vector,Vector) := (type,m,v,w) -> (transpose matrix v *quadraticFormMatrix(type,m)*w)_0
killingForm(LieAlgebra,List,List) := (g,v,w) -> (matrix{v}*quadraticFormMatrix g*matrix transpose{w})_(0,0)
killingForm(LieAlgebra,Vector,Vector) := (g,v,w) -> (transpose matrix v *quadraticFormMatrix g*w)_0


--This function returns the weights in the Weyl alcove
weylAlcove = method(
    TypicalValue => List
    )     
weylAlcove(String,ZZ,ZZ) := memoize((type, m, l) -> ( pl:={};
    if l==0 then return {apply(m, i -> 0)};
    if m==1 then return apply(l+1,i->{i});
    if type=="A" or type == "C" then (
        pl={{append(apply(m-1, i -> 0),l)}};
        for k from 0 to l-1 do (
            pk:=weylAlcove(type,m-1,l-k);
            pk=apply(#pk, q -> append(pk_q,k));
            pl=append(pl,pk));
        return sort flatten pl
    );
    if type != "A" and type != "C" then (
        pl=weylAlcove("A",m,l);    
	Theta :=highestRoot(type,m);
	answer:=delete(null, apply(#pl, i -> if killingForm(type, m, pl_i, Theta) <= l then pl_i));
        return sort answer
    )
))

weylAlcove(LieAlgebra,ZZ) := (g,l)-> if not isSimple g then error "Lie algebra not simple" else weylAlcove(g#"RootSystemType",g#"LieAlgebraRank",l)

weylAlcove(ZZ,LieAlgebra) := (l,g) -> weylAlcove(g,l)

--For definitions and formulas of Casimir scalars, see (13.127), [DMS] p. 512
--For the definition and formula for rho, see: (13.46), [DMS] p. 499
    
casimirScalar = method(
    TypicalValue => QQ
    )
casimirScalar(Sequence,Sequence,List) :=
casimirScalar(String,ZZ,List) := (type, m, w) -> (
    rho:=apply(plus sequence m,h->1/1);
    killingForm(type,m,w,w) + 2*killingForm(type,m,w,rho)
)

casimirScalar(LieAlgebraModule) := (M) -> (
    if not isIrreducible M then error "Casimir scalar on irreducible modules only";
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    v:=first keys M#"DecompositionIntoIrreducibles";
    casimirScalar(type,m,v)
)

simpleRoots = method(
    TypicalValue => List
)
  
simpleRoots(String,ZZ) := memoize((type,m) -> (
    entries cartanMatrix(type,m)
))

simpleRoots(LieAlgebra):=(g) -> entries cartanMatrix g


positiveRoots = method(
    TypicalValue => List
)

--In Freudenthal's formula, we need to sum over the positive roots
positiveRoots(String,ZZ):= (type,m) -> (
    simpleroots:=simpleRoots(type,m);
    answer:={};
    answer1:={};
    es:={};
    es2:={};
    em:={};
    subs:={};
    eiplusej:={};
    if type=="A" then (
	return delete(null, flatten apply(m, i -> apply(m, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
    );
    if type=="B" then (
	answer1 = delete(null, flatten apply(m-1, i -> apply(m-1, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
        es=apply(m, i -> sum apply(m-i, k -> simpleroots_(m-1-k)));
        subs=subsets(es,2);
        eiplusej=apply(#subs,h -> sum subs_h);
        return flatten {answer1,es,eiplusej}
    );
    if type=="C" then (
	answer1 = delete(null, flatten apply(m-1, i -> apply(m-1, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
        twoes:=apply(m, i -> if i<m-1 then sum(apply(m-i-1, k -> 2*simpleroots_(m-2-k)))+ simpleroots_(m-1) else simpleroots_(m-1));
        subs=subsets(twoes,2);
        eiplusej=apply(#subs,h -> sum subs_h);
        eiplusej=apply(#eiplusej,h -> apply(m, t-> lift((1/2)*eiplusej_h_t,ZZ)));
        return flatten {answer1,twoes,eiplusej}
    );
    if type=="D" then (
        answer1 = delete(null, flatten apply(m-1, i -> apply(m-1, j -> if j==i then simpleroots_i else if j > i then sum apply(j-i+1, k -> simpleroots_(i+k)))));
        em=(1/2)*(simpleroots_(m-1)-simpleroots_(m-2));
        em=apply(#em,k-> lift(em_k,ZZ));
        es={em};
        for i from 0 to m-2 do (
            es = append(es,es_(#es-1)+simpleroots_(m-2-i))
        );
        subs=subsets(es,2);
        eiplusej=apply(#subs,h -> sum subs_h);
        return flatten {answer1,eiplusej}
    );
    if type=="E" and m==6 then (
	return {{0, 0, 0, 0, -1, 2}, {0, 0, 0, -1, 1, 1}, {0, -1, -1, 1, 0, 1}, {-1, -1, 1, 0, 0, 1}, {1, -1, 0, 0, 0, 1}, {0, 1, -1, 0, 0, 1}, {-1, 1, 1, -1, 0, 1}, {1, 1, 0, -1, 0, 1}, {-1, 0, 0, 1, -1, 1}, {1, 0, -1, 1, -1, 1}, {-1, 0, 0, 0, 1, 0}, {1, 0, -1, 0, 1, 0}, {1, 0, -1, 1, 0, -1}, {0, 0, 1, 0, 0, -1}, {0, -1, -1, 2, -1, 0}, {-1, -1, 1, 1, -1, 0}, {0, 1, -1, 1, -1, 0}, {-1, 1, 1, 0, -1, 0}, {1, 0, 1, -1, 0, 0}, {0, 2, 0, -1, 0, 0}, {2, 0, -1, 0, 0, 0}, {-1, 0, 2, -1, 0, 0}, {1, 1, 0, 0, -1, 0}, {1, -1, 0, 1, -1, 0}, {-1, 0, 0, 1, 0, -1}, {1, 1, 0, -1, 1, -1}, {1, -1, 0, 0, 1, -1}, {-1, 1, 1, -1, 1, -1}, {-1, -1, 1, 0, 1, -1}, {0, 1, -1, 0, 1, -1}, {0, -1, -1, 1, 1, -1}, {0, 0, 0, -1, 2, -1}, {0, 1, 0, 0, 0, 0}, {0, -1, 0, 1, 0, 0}, {0, 0, 1, -1, 1, 0}, {0, 0, 1, 0, -1, 1}});
    if type=="E" and m==7 then (
	return {{0, 0, 0, 0, 0, -1, 2}, {0, 0, 0, 0, -1, 1, 1}, {0, 0, 0, -1, 1, 0, 1}, {0, -1, -1, 1, 0, 0, 1}, {-1, -1, 1, 0, 0, 0, 1}, {0, 1, -1, 0, 0, 0, 1}, {-1, 1, 1, -1, 0, 0, 1}, {-1, 0, 0, 1, -1, 0, 1}, {-1, 0, 0, 0, 1, -1, 1}, {-1, 0, 0, 0, 0, 1, 0}, {1, -1, 0, 0, 0, 0, 1}, {1, 1, 0, -1, 0, 0, 1}, {1, 0, -1, 1, -1, 0, 1}, {1, 0, -1, 0, 1, -1, 1}, {1, 0, -1, 0, 0, 1, 0}, {0, 0, 1, 0, -1, 1, -1}, {0, 0, 1, -1, 1, 0, -1}, {0, -1, 0, 1, 0, 0, -1}, {0, 1, 0, 0, 0, 0, -1}, {0, 0, 0, -1, 2, -1, 0}, {0, -1, -1, 1, 1, -1, 0}, {0, 1, -1, 0, 1, -1, 0}, {-1, -1, 1, 0, 1, -1, 0}, {-1, 1, 1, -1, 1, -1, 0}, {1, -1, 0, 0, 1, -1, 0}, {1, 1, 0, -1, 1, -1, 0}, {-1, 0, 0, 1, 0, -1, 0}, {1, -1, 0, 1, -1, 0, 0}, {1, 1, 0, 0, -1, 0, 0}, {-1, 0, 2, -1, 0, 0, 0}, {2, 0, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0}, {1, 0, 1, -1, 0, 0, 0}, {-1, 1, 1, 0, -1, 0, 0}, {0, 1, -1, 1, -1, 0, 0}, {-1, -1, 1, 1, -1, 0, 0}, {0, -1, -1, 2, -1, 0, 0}, {0, 0, 1, 0, 0, -1, 0}, {1, 0, -1, 1, 0, -1, 0}, {1, 0, -1, 0, 1, 0, -1}, {-1, 0, 0, 0, 1, 0, -1}, {1, 0, -1, 1, -1, 1, -1}, {-1, 0, 0, 1, -1, 1, -1}, {1, 1, 0, -1, 0, 1, -1}, {-1, 1, 1, -1, 0, 1, -1}, {0, 1, -1, 0, 0, 1, -1}, {1, -1, 0, 0, 0, 1, -1}, {-1, -1, 1, 0, 0, 1, -1}, {0, -1, -1, 1, 0, 1, -1}, {0, 0, 0, -1, 1, 1, -1}, {0, 0, 0, 0, -1, 2, -1}, {1, 0, 0, 0, 0, 0, 0}, {-1, 0, 1, 0, 0, 0, 0}, {0, 0, -1, 1, 0, 0, 0}, {0, 1, 0, -1, 1, 0, 0}, {0, -1, 0, 0, 1, 0, 0}, {0, 1, 0, 0, -1, 1, 0}, {0, 1, 0, 0, 0, -1, 1}, {0, -1, 0, 1, -1, 1, 0}, {0, -1, 0, 1, 0, -1, 1}, {0, 0, 1, -1, 0, 1, 0}, {0, 0, 1, -1, 1, -1, 1}, {0, 0, 1, 0, -1, 0, 1}});
    if type=="E" and m==8 then (
	return {{0, 0, 0, 0, 0, 0, -1, 2}, {0, 0, 0, 0, 0, -1, 1, 1}, {0, 0, 0, 0, -1, 1, 0, 1}, {0, 0, 0, -1, 1, 0, 0, 1}, {0, -1, -1, 1, 0, 0, 0, 1}, {-1, -1, 1, 0, 0, 0, 0, 1}, {0, 1, -1, 0, 0, 0, 0, 1}, {-1, 1, 1, -1, 0, 0, 0, 1}, {-1, 0, 0, 1, -1, 0, 0, 1}, {-1, 0, 0, 0, 1, -1, 0, 1}, {-1, 0, 0, 0, 0, 1, -1, 1}, {1, -1, 0, 0, 0, 0, 0, 1}, {1, 1, 0, -1, 0, 0, 0, 1}, {1, 0, -1, 1, -1, 0, 0, 1}, {1, 0, -1, 0, 1, -1, 0, 1}, {1, 0, -1, 0, 0, 1, -1, 1}, {0, 0, 1, 0, -1, 0, 0, 1}, {0, 0, 1, -1, 1, -1, 0, 1}, {0, 0, 1, -1, 0, 1, -1, 1}, {0, -1, 0, 1, 0, -1, 0, 1}, {0, -1, 0, 1, -1, 1, -1, 1}, {0, 1, 0, 0, 0, -1, 0, 1}, {0, 1, 0, 0, -1, 1, -1, 1}, {0, -1, 0, 0, 1, 0, -1, 1}, {0, 0, 1, 0, -1, 0, 1, -1}, {0, 0, 1, -1, 1, -1, 1, -1}, {0, 0, 1, -1, 0, 1, 0, -1}, {0, -1, 0, 1, 0, -1, 1, -1}, {0, -1, 0, 1, -1, 1, 0, -1}, {0, 1, 0, 0, 0, -1, 1, -1}, {0, 1, 0, 0, -1, 1, 0, -1}, {0, -1, 0, 0, 1, 0, 0, -1}, {0, 1, 0, -1, 1, 0, 0, -1}, {0, 0, -1, 1, 0, 0, 0, -1}, {-1, 0, 1, 0, 0, 0, 0, -1}, {1, 0, 0, 0, 0, 0, 0, -1}, {0, 0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, -1, 1, 1, -1, 0}, {0, -1, -1, 1, 0, 1, -1, 0}, {-1, -1, 1, 0, 0, 1, -1, 0}, {1, -1, 0, 0, 0, 1, -1, 0}, {0, 1, -1, 0, 0, 1, -1, 0}, {-1, 1, 1, -1, 0, 1, -1, 0}, {1, 1, 0, -1, 0, 1, -1, 0}, {-1, 0, 0, 1, -1, 1, -1, 0}, {1, 0, -1, 1, -1, 1, -1, 0}, {-1, 0, 0, 0, 1, 0, -1, 0}, {1, 0, -1, 0, 1, 0, -1, 0}, {1, 0, -1, 1, 0, -1, 0, 0}, {0, 0, 1, 0, 0, -1, 0, 0}, {0, -1, -1, 2, -1, 0, 0, 0}, {-1, -1, 1, 1, -1, 0, 0, 0}, {0, 1, -1, 1, -1, 0, 0, 0}, {-1, 1, 1, 0, -1, 0, 0, 0}, {1, 0, 1, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0, 0}, {2, 0, -1, 0, 0, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0, 0}, {1, 1, 0, 0, -1, 0, 0, 0}, {1, -1, 0, 1, -1, 0, 0, 0}, {-1, 0, 0, 1, 0, -1, 0, 0}, {1, 1, 0, -1, 1, -1, 0, 0}, {1, -1, 0, 0, 1, -1, 0, 0}, {-1, 1, 1, -1, 1, -1, 0, 0}, {-1, -1, 1, 0, 1, -1, 0, 0}, {0, 1, -1, 0, 1, -1, 0, 0}, {0, -1, -1, 1, 1, -1, 0, 0}, {0, 0, 0, -1, 2, -1, 0, 0}, {0, 1, 0, 0, 0, 0, -1, 0}, {0, -1, 0, 1, 0, 0, -1, 0}, {0, 0, 1, -1, 1, 0, -1, 0}, {0, 0, 1, 0, -1, 1, -1, 0}, {1, 0, -1, 0, 0, 1, 0, -1}, {1, 0, -1, 0, 1, -1, 1, -1}, {1, 0, -1, 1, -1, 0, 1, -1}, {1, 1, 0, -1, 0, 0, 1, -1}, {1, -1, 0, 0, 0, 0, 1, -1}, {-1, 0, 0, 0, 0, 1, 0, -1}, {-1, 0, 0, 0, 1, -1, 1, -1}, {-1, 0, 0, 1, -1, 0, 1, -1}, {-1, 1, 1, -1, 0, 0, 1, -1}, {0, 1, -1, 0, 0, 0, 1, -1}, {-1, -1, 1, 0, 0, 0, 1, -1}, {0, -1, -1, 1, 0, 0, 1, -1}, {0, 0, 0, -1, 1, 0, 1, -1}, {0, 0, 0, 0, -1, 1, 1, -1}, {0, 0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, 0, 1, -1}, {0, 0, 0, 0, 0, 1, -1, 0}, {0, 0, 0, 0, 1, -1, 0, 0}, {0, 0, 0, 1, -1, 0, 0, 0}, {0, 1, 1, -1, 0, 0, 0, 0}, {0, -1, 1, 0, 0, 0, 0, 0}, {1, 1, -1, 0, 0, 0, 0, 0}, {-1, 1, 0, 0, 0, 0, 0, 0}, {1, -1, -1, 1, 0, 0, 0, 0}, {-1, -1, 0, 1, 0, 0, 0, 0}, {1, 0, 0, -1, 1, 0, 0, 0}, {-1, 0, 1, -1, 1, 0, 0, 0}, {0, 0, -1, 0, 1, 0, 0, 0}, {1, 0, 0, 0, -1, 1, 0, 0}, {-1, 0, 1, 0, -1, 1, 0, 0}, {0, 0, -1, 1, -1, 1, 0, 0}, {0, 1, 0, -1, 0, 1, 0, 0}, {0, -1, 0, 0, 0, 1, 0, 0}, {1, 0, 0, 0, 0, -1, 1, 0}, {-1, 0, 1, 0, 0, -1, 1, 0}, {0, 0, -1, 1, 0, -1, 1, 0}, {0, 1, 0, -1, 1, -1, 1, 0}, {0, 1, 0, 0, -1, 0, 1, 0}, {0, -1, 0, 0, 1, -1, 1, 0}, {0, -1, 0, 1, -1, 0, 1, 0}, {0, 0, 1, -1, 0, 0, 1, 0}, {1, 0, -1, 0, 0, 0, 1, 0}, {-1, 0, 0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 0, 0, 1}, {1, 0, 0, 0, 0, 0, -1, 1}, {-1, 0, 1, 0, 0, 0, -1, 1}, {0, 0, -1, 1, 0, 0, -1, 1}, {0, 1, 0, -1, 1, 0, -1, 1}});
    if type=="F" and m==4 then (
	return {{0, 0, 0, 1}, {1, 0, 0, -1}, {-1, 1, 0, -1}, {0, -1, 2, -1}, {1, 0, 0, 0}, {-1, 1, 0, 0}, {0, -1, 2, 0}, {0,1,0,-2}, {1,-1,2,-2}, {-1, 0, 2, -2}, {-1, 0, 0, 2}, {1, -1, 0, 2}, {0, 1, -2, 2}, {2, -1, 0, 0}, {1, 1, -2, 0}, {-1, 2, -2, 0}, {0, 0, 1, -1}, {0, 1, -1, 0}, {1, -1, 1, 0}, {1, 0, -1, 1}, {-1, 0, 1, 0}, {-1, 1, -1, 1}, {0, -1, 1, 1}, {0, 0, -1, 2}});
    if type=="G" and m==2 then return {{-3, 2}, {-1, 1}, {0, 1}, {2, -1}, {3, -1}, {1, 0}};
)

positiveRoots(Sequence,Sequence):=memoize((type,m)->flatten toList apply(#m, i -> apply(positiveRoots(type#i,m#i),v->unsplit(v,m,i))))

positiveRoots(LieAlgebra):=(g) -> positiveRoots(g#"RootSystemType",g#"LieAlgebraRank")

positiveCoroots = method(
    TypicalValue => List
)

positiveCoroots(String,ZZ) :=
positiveCoroots(Sequence,Sequence) := memoize((type,m)->(
	pr:=positiveRoots(type,m);
	if all(sequence type, t -> t==="A" or t==="D" or t==="E") then return pr;
	apply(pr, v -> (
		r := 2 / killingForm(type,m,v,v);
		apply(v, k -> lift(r*k,ZZ))
		))
    ))

positiveCoroots(LieAlgebra):=(g) -> positiveCoroots(g#"RootSystemType",g#"LieAlgebraRank")


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Exported functions for Lie algebra modules 
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

multiplicity(List,LieAlgebraModule) := o -> (w,M) -> (
    W:=weightDiagram(M);
    W_w 
)
multiplicity(Vector,LieAlgebraModule) := o -> (w,M) -> multiplicity(entries w,M)

stdVars := memoize ( (type,m) -> (
	vrs := gens characterRing(type,m);
    	if type == "A" then apply(m+1, i -> (if i==m then 1 else vrs_i) * (if i==0 then 1 else vrs_(i-1)^-1))
    	else if type=="B" then apply(m, i -> (if i==m-1 then vrs_i^2 else vrs_i) * (if i==0 then 1 else vrs_(i-1)^-1))
    	else if type == "C" then apply(m, i -> vrs_i * (if i==0 then 1 else vrs_(i-1)^-1))
    	else if type == "D" then apply(m-2, i -> vrs_i*(if i==0 then 1 else vrs_(i-1)^-1)) | {vrs_(m-2)*vrs_(m-1)*vrs_(m-3)^-1,vrs_(m-1)*vrs_(m-2)^-1}
    	))

characterAlgorithms := new MutableHashTable;
-- Jacobi Trudi formulae
elemSym = memoize((L,i) -> (
    if i<0 or i>#L then 0
    else sum(subsets(L,i),product)
    ))
characterAlgorithms#"JacobiTrudi'" = (type,m,v) -> ( -- good for high rank algebras, small weights
    if type != "A" or m<=3 then return;
    z := stdVars(type,m);
    conj:=reverse splice apply(m,i -> v#i : i+1);
    if #conj == 0 then 1_(characterRing(type,m)) else det matrix table(#conj,#conj,(i,j)->elemSym(z,conj#i+j-i))
    )
completeSym = memoize((L,i) -> (
    if i<0 then 0
    else sum(compositions(#L,i),c->product(L,c,(v,k)->v^k))
    ))
characterAlgorithms#"JacobiTrudi" = (type,m,v) -> (
    if type != "A" then return;
    z := stdVars(type,m);
    pows := apply(m+1,j->sum(j..m-1,k->v#k));
    det matrix table(m+1,m+1,(i,j)->completeSym(z,pows#i+j-i))
    )

-- Weyl character formula
characterAlgorithms#"Weyl" = (type,m,v) -> ( -- good for low rank algebras
    z := stdVars(type,m);
    if type == "A" then (
	pows := apply(m+1,j->sum(j..m-1,k->1+v#k));
    	num := det matrix table(m+1,m+1,(i,j)->z_i^(pows#j));
    	den := product(m+1,j->product(j,i->z_i-z_j)); --  type A Weyl denominator formula
	)
    else if type=="B" then (
	pows = apply(m,j->sum(j..m-2,k->1+v#k)+(1+v#(m-1))//2);
	par := (1+v#(m-1)) % 2; -- shift of 1/2 to avoid half-integer powers
	num = (last gens characterRing(type,m))^(1-par)*det matrix table(m,m,(i,j)->z_i^(pows#j+par)-z_i^(-pows#j));
    	den = product(m,i->z_i-1)*product(m,j->product(j,i->(z_i^-1 - z_j)*(1-z_i*z_j^-1))); --  type B Weyl denominator formula
	)
    else if type == "C" then (
	pows = apply(m,j->sum(j..m-1,k->1+v#k));
    	num = det matrix table(m,m,(i,j)->z_i^(pows#j)-z_i^(-pows#j));
    	den = product(m,i->z_i-z_i^-1)*product(m,j->product(j,i->(z_i^-1 - z_j)*(1-z_i*z_j^-1))); --  type C Weyl denominator formula
	)
    else if type == "D" then (
	pows = append(apply(m-1,j->sum(j..m-3,k->1+v#k)+(2+v#(m-2)+v#(m-1))//2),(v#(m-1)-v#(m-2))//2);
	par = (v#(m-2)+v#(m-1)) % 2; -- shift of 1/2 to avoid half-integer powers
    	num1 := det matrix table(m,m,(i,j)->z_i^(pows#j+par)+z_i^(-pows#j));
	num2 := det matrix table(m,m,(i,j)->z_i^(pows#j+par)-z_i^(-pows#j));
    	den = product(m,j->product(j,i->(z_i^-1 - z_j)*(1-z_i*z_j^-1))); --  type D Weyl denominator formula
	num = (last gens characterRing(type,m))^(-par)*(num1+num2)//2;
	)
    else return;
    num//den
)

--In the next two functions we implement Freudenthal's recursive algorithm for computing the weights in a Lie algebra module and their multiplicities
--The function Freud computes the set of weights in a Lie algebra module without their multiplicities
Freud = memoize ((type,m,v) -> (
    simpleroots:=simpleRoots(type,m);
    if all(v, a -> a < 0) then return set{v};
    answer:=set {v};
    for i from 0 to #v-1 do
        for j from 1 to v_i do
            answer = answer + Freud(type,m,v-j*simpleroots_i);
    answer
))

-*
--the function weightsAboveMu computes the weights above mu=w in the weight diagram of lambda=v
weightsAboveMu = memoize( (type,m,v,w) -> (
    Omega:=Freud(type,m,v);
    if w==v then return {};
    simpleroots:=simpleRoots(type,m);
    answer:={};
    k:=0;
    for i from 0 to #simpleroots-1 do (
        k=0;
        while isSubset(set {w+k*(simpleroots_i)},Omega) do (
            if k>0 then answer = append(answer,w+k*(simpleroots_i));
            k=k+1;
    ));
    answer=unique answer;
    alllevels:={answer};
    for i from 0 to #answer-1 do (
        alllevels = append(alllevels,weightsAboveMu(type,m,v,answer_i))
    );
    unique flatten alllevels
))


multiplicityOfWeightInLieAlgebraModule = memoize((type,m,v,w) -> (
    rho:=toList(m:1);
    if v==w then return 1;
    Omega:=Freud(type,m,v);
    if not member(w,Omega) then return 0;
--    L:=weightsAboveMu(type,m,v,w);
    posroots:=positiveRoots(type,m);
    rhs:=0;
    local w';
    scan(posroots, a -> (
        w'=w+a;
        while member(w',Omega) do (
	    rhs=rhs+killingForm(type,m,w',a)*multiplicityOfWeightInLieAlgebraModule(type,m,v,w');
	    w'=w'+a;
	    )));
    lhs:=killingForm(type,m,v+rho,v+rho)-killingForm(type,m,w+rho,w+rho);
    lift(2*rhs/lhs,ZZ)
))


characterAlgorithms#"Freudenthal" = (type,m,v) -> (
    R := characterRing(type,m);
    sum(toList Freud(type,m,v), w -> multiplicityOfWeightInLieAlgebraModule(type,m,v,w) * R_w)
    )
*-

-- this is a rewrite of commented out multiplicityOfWeightInLieAlgebraModule
characterAlgorithms#"Freudenthal" = (type,m,v) -> (
    R:=characterRing(type,m);
    rho:=toList(m:1);
    Omega:=Freud(type,m,v);
    mults:=new MutableHashTable from Omega;
    posroots:=positiveRoots(type,m);
    -- sort, removing highest weight
    Omega=drop(apply(reverse sort apply(toList Omega,w->R_w),first @@ exponents),1);
    scan(Omega, w -> (
    	    rhs:=0;
    	    scan(posroots, a -> (
        	    w':=w+a;
        	    while mults#?w' do (
	    		rhs=rhs+killingForm(type,m,w',a)*mults#w';
	    		w'=w'+a;
	    		)));
    	    lhs:=killingForm(type,m,v+rho,v+rho)-killingForm(type,m,w+rho,w+rho);
    	    mults#w = lift(2*rhs/lhs,ZZ);
	    ));
    sum(pairs mults,(w,mu) -> mu * R_w) -- is there a nicer way of writing this?
)



-- last strategy = first choice
scan({"JacobiTrudi","Freudenthal","Weyl","JacobiTrudi'"}, strat -> addHook(symbol character,characterAlgorithms#strat,Strategy=>strat))

character = method(
    Options=>{Strategy=>null},
    TypicalValue => RingElement
    )

character1 = memoize((type,m,v,o)->runHooks(symbol character,(type,m,v),o))
character2 = memoize((type,m,v,o) -> (
	v=split(v,m);
	R:=characterRing(type,m);
	product(#m,i->R#"maps"#i character1(type#i,m#i,v#i,o))
	))
character (String,ZZ,List) := o -> (type,m,v) -> character1(type,m,v,o) -- tricky to memoize a method with options
character (Sequence,Sequence,List) := o -> (type,m,v) -> character2(type,m,v,o) -- tricky to memoize a method with options
character (LieAlgebra,List) := o -> (g,v) -> if rank g == 0 then 1_(characterRing g) else character(g#"RootSystemType",g#"LieAlgebraRank",v,o) -- annoying special case, otherwise wrong ring
character (LieAlgebra,Vector) := o -> (g,v) -> character(g,entries v,o)
character LieAlgebraModule := o -> (cacheValue character) ((M) -> sum(pairs M#"DecompositionIntoIrreducibles",(v,a) -> a * character (M#"LieAlgebra",v,o)))

weightDiagram = method(
    Options=>{Strategy=>null},
    TypicalValue=>VirtualTally
    )

weightDiagram LieAlgebraModule := o -> (M) -> new VirtualTally from listForm character(M,o)
weightDiagram(LieAlgebra,Vector) := weightDiagram(LieAlgebra,List) := o -> (g,v) -> new VirtualTally from listForm character(g,v,o)

fac := memoize((type,m) -> ( -- possible denominator in Weyl product formula factors
    lcm append(apply(positiveCoroots(type,m), u -> numerator (killingForm(type,m,u,u)/2)),1) -- append is for g=0
    ))

qden := memoize((type,m,qnum) -> (
    rho:=toList(plus sequence m : 1);
    d:=fac(type,m);
    product(positiveRoots(type,m), a -> qnum lift(d*killingForm(type,m,rho,a),ZZ))
    ))

qdim1 = (M,qnum) -> ( -- used internally by dim and qdim: Weyl product formula
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    rho:=toList(plus sequence m : 1);
    d:=fac(type,m);
    (sum(pairs M#"DecompositionIntoIrreducibles", (w,mu) ->
	mu * product(positiveRoots g, a -> qnum lift(d*killingForm(g,w+rho,a),ZZ))
	))//qden(type,m,qnum)
    )

dim LieAlgebraModule := (cacheValue dim) (M -> qdim1(M,identity))

-- we use one q ring for everyone, to simplify
q:=getSymbol "q"
R:=ZZ(monoid[q,Inverses=>true,MonomialOrder=>Lex])
R':=ZZ(monoid[q]) -- annoying: can't take quotients with Inverses => true
-*
cyclotomic = memoize ( n -> ( -- clever but silly implementation
	facs := first \ toList factor (if odd n then R'_0^n-1 else R'_0^(n//2)+1);
	facs#(maxPosition(first\degree\facs))
	))
*-
cyclotomic = memoize ( n -> (
	P := R'_0^n - 1;
	scan(1..n//2, d -> if n%d==0 then P = P // cyclotomic d);
	P
	))

qring := memoize ( (n,d) -> R'/((map(R',R',{R'_0^d})) cyclotomic n ) )
qnum := n->sum(n,i->R_0^(2*i-n+1))

qdim = method()
qdim LieAlgebraModule := (cacheValue qdim) (M -> qdim1(M,qnum))
qdim (LieAlgebraModule,ZZ) := (M,l) -> (
    g:=M#"LieAlgebra";
    if not isSimple g then error "Lie algebra not simple";
    (map(qring(l+dualCoxeterNumber g,2*fac(g#"RootSystemType",g#"LieAlgebraRank")),R)) qdim M
    )


LieAlgebraModuleFromWeights = method(
    TypicalValue => LieAlgebraModule
    )
LieAlgebraModuleFromWeights(RingElement,LieAlgebra) := (c0,g) -> (
    if ring c0 =!= characterRing g then error "wrong ring";
    c:=c0;
    --find and peel off irreducibles
    decompositionData := while c!=0 list ( (v,mu) := first listForm leadTerm c ) do (
	if any(v,a->a<0) then error "not a valid weight diagram";
	c = c - mu*character(g,v);
    	);
    new LieAlgebraModule from {
    	"LieAlgebra" => g,
    	"DecompositionIntoIrreducibles" => new VirtualTally from decompositionData,
    	cache => new CacheTable from { character => c0 }
    	}
    )
-- another algorithm would be to apply the same Racah/Brauer/Klimyk algorithm as tensor product (with second weight = trivial one)
-- not clear which is faster

LieAlgebraModuleFromWeights(VirtualTally,LieAlgebra) := (W,g) -> (
    R := characterRing g;
    LieAlgebraModuleFromWeights(sum(pairs W,(w,a) -> a*R_w),g)
    )

adams = method( TypicalValue => LieAlgebraModule )
adams (ZZ,LieAlgebraModule) := (k,M) -> (
    g:=M#"LieAlgebra";
    if k==0 then new LieAlgebraModule from (g,{})
    else if k==1 then M
    else if k==-1 then starInvolution M
    else LieAlgebraModuleFromWeights(applyKeys(weightDiagram M, w -> k*w),g) -- primitive but works
)

symmetricPower(ZZ,LieAlgebraModule) := (cacheValue'(1,symmetricPower)) ((n,M) -> (
    if n<0 then error "nonnegative powers only";
    if n==0 then trivialModule M#"LieAlgebra"
    else if n==1 then M
    else (directSum apply(1..n, k -> adams(k,M) ** symmetricPower(n-k,M)))^(1/n)
    ))

exteriorPower(ZZ,LieAlgebraModule) := o -> (cacheValue'(1,exteriorPower)) ((n,M) -> (
    if n<0 then error "nonnegative powers only";
    if n==0 then trivialModule M#"LieAlgebra"
    else if n==1 then M
    else (directSum apply(1..n, k -> (adams(k,M) ** exteriorPower(n-k,M))^((-1)^(k-1)) ))^(1/n)
    ))

LieAlgebraModule @ LieAlgebraModule := (M,M') -> new LieAlgebraModule from (
    M#"LieAlgebra" ++ M'#"LieAlgebra",
    combine(M#"DecompositionIntoIrreducibles",M'#"DecompositionIntoIrreducibles",join,times,plus)
    )

---------------------------------------------------------
---------------------------------------------------------
--Tensor product decomposition
---------------------------------------------------------
--------------------------------------------------------- 
-*
--Action of word in Coxeter group or affine Coxeter group on weights
wordAction = (type,m,l,I,v) -> (
    simpleroots:=simpleRoots(type,m);
    w:=v;
    J:=reverse I; 
    for j from 0 to #J-1 do (     
        if J_j >0 then (
	    rho:=apply(#w, i-> 1);
            w=w+rho;
            w = w-(w_(J_j-1))*simpleroots_(J_j-1);
            w=w-rho);
        if J_j ==0 then (
            theta:=highestRoot(type,m);
            theta=apply(#theta, i -> lift(theta_i,ZZ));
            l0:=lift(l-killingForm(type,m,w,theta),ZZ);
            w = w+(l0+1)*theta);
    );
    w
)

squarefreeWordsOfLengthP = (L,p) -> (
    if p==0 then return {};
    if p==1 then return apply(#L, i -> {L_i});
    wlm1:=squarefreeWordsOfLengthP(L,p-1);
    answer:=delete(null, flatten apply(#L, i -> apply(#wlm1, j -> if L_i != wlm1_j_0 then prepend(L_i,wlm1_j))));
    answer
)

isIdentity = (type,m,l,w) -> (
    fdw:=apply(m, i -> apply(m, j -> if i==j then 1 else 0));
    apply(m, i -> wordAction(type,m,l,w,fdw_i)) == fdw      
)

*-

LieAlgebraModule ** LieAlgebraModule := (V,W) -> ( -- cf Humpheys' intro to LA & RT sec 24 exercise 9
    g:=V#"LieAlgebra";
    if g != W#"LieAlgebra" then error "V and W must be modules over the same Lie algebra";
    if V =!= W and dim W < dim V then (V,W)=(W,V); -- maybe should first test if characters already computed?
    wd:=weightDiagram V;
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    sr:=simpleRoots g;
    rho:=toList(rank g:1);
    ans := new MutableHashTable;
    add := (w,a) -> if ans#?w then ( s := ans#w+a; if s!=0 then ans#w = s else remove(ans,w) ) else ans#w = a;
    scanPairs(W#"DecompositionIntoIrreducibles", (w,a) -> -- loop over highest weights of W
    	scanPairs(wd, (v,b) -> ( -- loop over all weights of V
    		u:=v+w+rho;
		t:=1; i:=-1;
		while not any(u,zero) and ((i=position(u,j->j<0)) =!= null) do (
	    	    u=u-u#i*sr#i;
	    	    t=-t;
	    	    );
		if i === null then add(u-rho,a*b*t);
		)));
    new LieAlgebraModule from (g,ans)
    )

tensorCoefficient = method(
    TypicalValue=>ZZ)
tensorCoefficient(LieAlgebraModule, LieAlgebraModule,LieAlgebraModule) := (U,V,W) -> (
	if not isIrreducible W then error "third module must be irreducible";
    	nu:=first keys W#"DecompositionIntoIrreducibles";
    	fullTensorProduct:=(U**V)#"DecompositionIntoIrreducibles";
    	fullTensorProduct_nu
    )


---------------------------------------------------------
---------------------------------------------------------
--Fusion product decomposition
---------------------------------------------------------
--------------------------------------------------------- 

-*
fusionReflectionData = memoize( (type,m,l,maxwordlength,remainingWeights) -> (
    Pl:=weylAlcove(type,m,l);
    wl:=1;
    --initialize;
    remainingWeights=toList(set(remainingWeights)-set(Pl));
    found:= set Pl;
    answer:= set apply(#Pl, i -> {Pl_i,{}});
    fixed:={};
    S:=apply(m+1,i->i);
    while #remainingWeights >0 and wl<=maxwordlength do (
        words:=squarefreeWordsOfLengthP(S,wl);
        for i from 0 to #words-1 do (
            if isIdentity(type,m,l,words_i) then continue;
            newremainingWeights:={};
            for j from 0 to #remainingWeights-1 do (
                if wordAction(type,m,l,words_i,remainingWeights_j)==remainingWeights_j then (
                    answer = answer + set {{remainingWeights_j,reverse(words_i)}};
                    fixed = append(fixed,remainingWeights_j)) else newremainingWeights=append(newremainingWeights,remainingWeights_j)
            );
            remainingWeights=newremainingWeights;
            im:=apply(#Pl, j -> wordAction(type,m,l,words_i,Pl_j));
            if member(im,found) then continue else (
                found = found + set(im);
                remainingWeights=toList(set(remainingWeights)-set(im));
                answer=answer+set apply(#im, k -> {im_k,reverse(words_i)});
            )
        );
        wl=wl+1);
    if #remainingWeights==0 then return {sort toList(answer),sort fixed,true,remainingWeights} else return {sort toList(answer), sort fixed,false,remainingWeights}
))
*-

fusionProduct = method(
--    TypicalValue=>HashTable,Options=>{MaxWordLength=>10})
    TypicalValue=>LieAlgebraModule)

-- TODO: allow for arbitrary number of args just like tensor and directSum

-*
-- try to define abbreviated syntax? something like (except fusionProduct should output a fusion module)
FusionModule := new Type of LieAlgebraModule
LieAlgebraModule _ ZZ := (M,l) -> new FusionModule from merge(M,hashTable{"Level"=>l},last)
-- expression fusionModule := -- TODO
FusionModule ** LieAlgebraModule := (F,W) -> fusionProduct(F,W,F#"Level")
LieAlgebraModule ** FusionModule := (W,F) -> fusionProduct(W,F,F#"Level")
FusionModule ** FusionModule := (F,F') -> if F#"Level" != F'#"Level" then error "modules must have same level" else fusionProduct(F,F',F#"Level")
*-

fusionProduct(LieAlgebraModule,LieAlgebraModule,ZZ) := (V,W,l) -> (
    g:=V#"LieAlgebra";
    l = l + dualCoxeterNumber g;
    if g != W#"LieAlgebra" then error "V and W must be modules over the same Lie algebra";
    if not isSimple g then error "Lie algebra not simple";
    wd:=weightDiagram V;
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    sr:=simpleRoots(type,m);
    rho:=toList(m:1);
    pc:=positiveCoroots g;
    pr:=positiveRoots g;
--    Q:=quadraticFormMatrix g;
--    Q:=quadraticFormMatrix (type,m);
--    pr':=apply(pr, u -> entries(lift(Ci*vector u,ZZ))); -- possibly reinstate after non simply laced fix
--    pr':=apply(pr, u -> (2/killingForm(g,u,u))*entries(Q*vector u));
    ans := new MutableHashTable;
    add := (w,a) -> if ans#?w then ( s := ans#w+a; if s!=0 then ans#w = s else remove(ans,w) ) else ans#w = a;
    scanPairs(W#"DecompositionIntoIrreducibles", (w,a) -> -- loop over highest weights of W
    	scanPairs(wd, (v,b) -> ( -- loop over all weights of V
    		u:=v+w+rho;
		-- first recenter it using translations
		cnt:=0; i:=0;
        	while cnt < #pr do (
--		    s := sum(u,pr'#i,times);
		    s := killingForm(g,u,pr#i); -- is the same just more explicit
		    sn := numerator s; sd := denominator s; -- in non simply laced types, there can be a denimonator
		    if sd == 1 and sn % l == 0 then break else if s < -l or s > l then (
			u=u-((sn+l*sd)//(2*l*sd))*l*pr#i;
			cnt=0;
			) else cnt=cnt+1;
		    i=i+1; if i==#pr then i=0;
            	    );
		if cnt == #pr then (
		    -- then end with usual algo
		    -- except the any(u,zero) not needed, filtered already
		    t:=1;
		    while (i=position(u,j->j<0)) =!= null do (
		    	u=u-u#i*sr#i;
		    	t=-t;
		    	);
		    add(u-rho,a*b*t);
		    )
		)));
    new LieAlgebraModule from (g,ans)
    )

-*
fusionProduct(LieAlgebraModule,LieAlgebraModule,ZZ) := memoize( opts-> (M,N,l) -> (
    wl:= opts.MaxWordLength;
    if M#"LieAlgebra" != N#"LieAlgebra" then error "The Lie algebra modules must be over the same Lie algebra.";
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    if not isIrreducible M or not isIrreducible N then error "modules need to be irreducible";
    lambda:=first keys M#"DecompositionIntoIrreducibles";
    mu:=first keys N#"DecompositionIntoIrreducibles";
    wd:=pairs weightDiagram(g,lambda);
    wd=apply(#wd, i -> {wd_i_0+mu,wd_i_1});
    rd:=fusionReflectionData(type,m,l,wl,apply(#wd, i -> wd_i_0));
    if rd_2 == false then error "Need to allow longer words";
    fixed:=rd_1;
    rd=hashTable(rd_0);
    Pl:=weylAlcove(type,m,l);
    wtsinPl:=delete(null, apply(#wd, i -> if member(wd_i_0,Pl) and not member(wd_i_0,fixed) then wd_i));
    wdh:=new MutableHashTable from wtsinPl;
    for i from 0 to #wd-1 do (
        if member(wd_i_0,Pl) then continue;
        if member(wd_i_0,fixed) then continue;
        word:=rd#(wd_i_0);
        e:=#word;
        e=(-1)^e;
        im:=wordAction(type,m,l,word,wd_i_0);
        wdh#im = wdh#im + (e)*(wd_i_1)
    );
    wdh=pairs(wdh);
    newwdh:=delete(null, apply(#wdh, i -> if wdh_i_1 != 0 then wdh_i));
    if #newwdh == 1 and newwdh_0_1 == 1 then return irreducibleLieAlgebraModule(newwdh_0_0,simpleLieAlgebra(type,m));
    return new LieAlgebraModule from (simpleLieAlgebra(type,m),newwdh)
))
*-

fusionCoefficient=method(
--    TypicalValue=>ZZ,Options=>{MaxWordLength=>10})
    TypicalValue=>ZZ)
fusionCoefficient(LieAlgebraModule,LieAlgebraModule,LieAlgebraModule,ZZ) := (U,V,W,l) -> (
    if not isIrreducible W then error "third module must be irreducible";
    nu:=first keys W#"DecompositionIntoIrreducibles";
    fullFusionProduct:=(fusionProduct(U,V,l))#"DecompositionIntoIrreducibles";
    fullFusionProduct_nu
)

-- branching rule
blocks = C -> ( -- given a Cartan (or adjacency) matrix, decompose into irreducible blocks
    n:=numRows C;
    L:=toList(0..n-1);
    B:={};
    while #L>0 do (
	-- start a new block
	i:=first L; L=drop(L,1);
	b:={i}; j:=0;
	while j<#b do (
	    L':=select(L,k->C_(b#j,k)!=0); -- we're assuming undirected adjacency or Cartan
	    b=b|L';
	    scan(L',k->L=delete(k,L));
	    j=j+1;
	    );
    	B=append(B,b);
    	);
    B
)

lieTypeFromCartan := C -> ( -- used internally. returns (type,m,order) where order is permutation of rows/cols
    -- in principle one could conceive not permuting at all but it would require some rewrite (positiveRoots, etc)
    B:=blocks C;
    type':=(); m':=(); L:={}; -- L is permutation of rows/columns to match normal Cartan matrix
    scan(B, b -> (
	    c:=C^b_b;
	    n:=numRows c;
	    -- first pass, covers 99% of cases
	    t:=scan("A".."G",t->if c === (try cartanMatrix(t,n)) then break t);
	    if t === null then (
		-- let's try harder
		local c';
		t=scan("A".."G",t->(
			c'=try cartanMatrix(t,n);
			if c'=!=null and det c == det c' and sort sum entries c == sort sum entries c' -- fun fact: characterizes uniquely
			then break t;
			));
		if t === null then error ("not the Cartan matrix of a semi-simple Lie algebra");
		-- just try every permutation, damnit
		p:=scan(permutations n,p->if c_p^p==c' then break p);
		if p === null then error ("not the Cartan matrix of a semi-simple Lie algebra");
    	    	b=b_p;
		);
	    type'=append(type',t); m'=append(m',n);
	    L=L|b;
	    ));
    (type',m',L)
    )

new LieAlgebra from Matrix := (T,C) -> ( -- define a Lie algebra based on its Cartan matrix
    if numColumns C == 0 then return new LieAlgebra from {"LieAlgebraRank"=>(),"RootSystemType"=>(),subLieAlgebra=>hashTable{null=>id_(ZZ^0)}};
    (type,m,L):=lieTypeFromCartan C;
    h:=directSum apply(type,m,simpleLieAlgebra); -- lazy though avoids unsequence, worrying about rings etc
    assert(cartanMatrix h == C_L^L);
    h
    )

subLieAlgebra = method ( TypicalValue => LieAlgebra )

subLieAlgebra (LieAlgebra, List) := (g,S) -> subLieAlgebra(g,if #S==0 then map(ZZ^(rank g),0,0) else matrix transpose apply(S,s ->
	if class s === ZZ then apply(rank g, j -> if j+1 == s then 1 else 0)
	else if instance(s,Vector) and rank class s == rank g then entries s
	else if class s === List and #s == rank g then s
	else error "wrong argument"))

-*
    -- identify the sub-Dynkin diagram
    S=deepSplice S;
    if #S == 0 then return new LieAlgebra from {"LieAlgebraRank"=>(),"RootSystemType"=>()}
    S=apply(S,i->i-1);
    C:=(cartanMatrix g)^S_S;
    h:=new LieAlgebra from C;
    )
*-

subLieAlgebra (LieAlgebra,Matrix) := (g,M) -> ( -- matrix of coroots
    -- in the simply laced case it'd be simply transpose M * cartanMatrix g * M. in general have to work harder
    if numRows M != rank g then error "wrong size of coroots";
    G := transpose M * inverse quadraticFormMatrix g * M; -- new inverse quadratic form <coroot_i|coroot_j>
    D := diagonalMatrix apply(numColumns M,i->2/G_(i,i)); -- inverse square norm of new simple coroots
    C := lift(D * G,ZZ);
    (type,m,L):=lieTypeFromCartan C;
    M=M_L; -- permuted matrix of coroots
    if M == id_(ZZ^(rank g)) then return g; -- not necessary but simpler
    subs:=hashTable{null=>id_(ZZ^(plus m))};
    subs=merge(applyValues(applyKeys(g#subLieAlgebra, k -> if k===null then g else k), A -> A*M),subs,last);
    new LieAlgebra from {
	"LieAlgebraRank"=>unsequence m,
	"RootSystemType"=>unsequence type,
	subLieAlgebra=>subs
	}
    )
    

branchingRule = method ( TypicalValue => LieAlgebraModule )

branchingRule (LieAlgebraModule, Matrix) :=
branchingRule (LieAlgebraModule, List) := (M,S) -> branchingRule(M,subLieAlgebra(M#"LieAlgebra",S))

branchingRule (LieAlgebraModule, LieAlgebra) := (M,h) -> ( -- here h must be a (known) subalgebra of that of M
    g:=M#"LieAlgebra";
    if g===h then return M; -- annoying special case
    S:=try h#subLieAlgebra#g else error "not a Lie subalgebra";
    --    f:=if class S===List then a -> a_S else a -> entries(transpose S*vector a);
    f:=a -> entries(transpose S*vector a); -- lame but what we get for using Lists rather than vectors
    LieAlgebraModuleFromWeights(applyKeys(weightDiagram M,f,plus),h)
    )


beginDocumentation()

doc ///
    Key
       LieTypes
    Headline
       Common types for Lie groups and Lie algebras
    Description
        Text 
            This package defines types used by the ConformalBlocks package and may someday be used by other packages as well.  If you would like to see a type or function added to this package (or better yet, if you would like to write types or functions for this package), please contact Dan Grayson, Mike Stillman, or Dave Swinarski.  
///

doc ///
    Key
       LieAlgebra
    Headline
        class for Lie algebras
    Description
        Text
    	    This class represents Lie algebras.  Currently only semi-simple Lie algebras over the complex numbers are supported.
	    An object of type @TT "LieAlgebra"@ is a hash table whose keys record the rank of the Lie algebra and the type of the root system.
        Example
	    g=simpleLieAlgebra("A",1)
	    h=simpleLieAlgebra("E",6)
	    g++h
	Text
	    If you have access to unicode fraktur, you can use the shorthand
	Example
	    𝔣_4
	Text
	    See also @TO (NewFromMethod,LieAlgebra,Matrix)@.
///

doc ///
    Key
        simpleLieAlgebra
	(simpleLieAlgebra,String,ZZ)
    Headline
        construct a simple Lie algebra
    Usage
        simpleLieAlgebra("A",1)
    Inputs
        t:String
            the type of the root system of the desired Lie algebra
        k:ZZ
            the rank of the desired Lie algebra
    Outputs
        g:LieAlgebra
            the simple Lie algebra with the given rank and type	        
    Description
        Text
            The classification of simple Lie algebras over the complex numbers is well known.
	    There are four infinite families (types $\mathfrak{a}_n$, $\mathfrak{b}_n$, $\mathfrak{c}_n$, $\mathfrak{d}_n$) corresponding to the Lie algebras
	    $\mathfrak{sl}(n+1,\mathbb{C})$, $\mathfrak{so}(2n+1,\mathbb{C})$, $\mathfrak{sp}(2n,\mathbb{C})$, $\mathfrak{so}(2n,\mathbb{C})$ respectively,
	    and five exceptional simple Lie algebras, $\mathfrak{e}_6$, $\mathfrak{e}_7$, $\mathfrak{e}_8$, $\mathfrak{f}_4$, $\mathfrak{g}_2$.

        Example
            --simpleLieAlgebra(sl_2)
	    simpleLieAlgebra("A",1)
	    --simpleLieAlgebra(sp_10)
	    simpleLieAlgebra("E",6)
///	 	 

TEST ///
    assert(A=simpleLieAlgebra("A",1); A#"LieAlgebraRank"===1 and A#"RootSystemType"==="A" and isSimple A)
///

doc ///
    Key
	(symbol ==, LieAlgebra, LieAlgebra)
    Headline
        tests equality of LieAlgebra
    Usage
        g == h
    Inputs
        g:LieAlgebra
	h:LieAlgebra
    Outputs
        b:Boolean
    Description
        Text
	    This function tests equality of the underlying hash tables of $g$ and $h$ are the same.    
	       
        Example
	    g=simpleLieAlgebra("A",2)
	    h=simpleLieAlgebra("A",2)
	    g==h
///

TEST ///
    assert(simpleLieAlgebra("A",2) == simpleLieAlgebra("A",2))
///


doc ///
    Key
        dualCoxeterNumber
	(dualCoxeterNumber,LieAlgebra)
	(dualCoxeterNumber,String,ZZ)
    Headline
        returns the dual Coxeter number of a simple Lie algebra
    Usage
        dualCoxeterNumber(g)
    Inputs
        g:LieAlgebra
	    a simple Lie algebra
    Outputs
        n:ZZ
	    the dual Coxeter number of g
    Description
        Text
	    The dual Coxeter number is defined as the sum of the comarks plus 1.  See Di Francesco, Mathieu, and Senechal, {\it Conformal Field Theory}, Springer Graduate Texts in Theoretical Physics, Formula (13.35) and Appendix A.
	      
        Example
	    dualCoxeterNumber("A",2)	
	    g=simpleLieAlgebra("A",2)
	    dualCoxeterNumber(g)
///

TEST ///
    assert(dualCoxeterNumber("A",2) === 3)
///

doc ///
    Key
        highestRoot
	(highestRoot,LieAlgebra)
    Headline
        returns the highest root of a simple Lie algebra
    Usage
        highestRoot(g), highestRoot("A",2)
    Inputs
        g:LieAlgebra
    Outputs
        t:List
    Description
        Text  
            Let R be an irreducible root system of rank m, and choose a base of simple roots $\Delta = \{\alpha_1,...,\alpha_m\}$.  Then there is a unique root $\theta$ such that when $\theta$ is expanded in terms of the simple roots, i.e. $\theta= \sum c_i \alpha_i$, the sum $\sum c_i$ is maximized.  The formulas implemented here are taken from the tables following Bourbaki's {\it Lie Groups and Lie Algebras} Chapter 6.
	    
	Text       
	    In the example below, we see that for $sl_3$, the highest root $\theta$ is $\omega_1+ \omega_2$, where $\omega_1$ and $\omega_2$ are the fundamental dominant weights.
	    
	Example
	    highestRoot("A",2)
///

TEST ///
    assert(highestRoot("A",2) === {1,1})
///	

doc ///
    Key
        positiveRoots
	(positiveRoots,LieAlgebra)
        positiveCoroots
	(positiveCoroots,LieAlgebra)
    Headline
        returns the positive (co)roots of a simple Lie algebra
    Usage
        positiveRoots(g), positiveCoroots(g)
    Inputs
        g:LieAlgebra
    Outputs
        t:List
    Description
        Text  
            Let R be an irreducible root system of rank m, and choose a base of simple roots $\Delta = \{\alpha_1,...,\alpha_m\}$.
	    This function returns all the roots that are nonnegative linear combinations of the simple roots (expressed in the basis of fundamental weights).
	    The formulas implemented here are taken from the tables following Bourbaki's {\it Lie Groups and Lie Algebras} Chapter 6.
	    
	Text       
	    In the example below, we see that for $sl_3$, the positive roots are $\alpha_1$, $\alpha_2$, and $\alpha_1+\alpha_2$.
	    
	Example
	    sl3=simpleLieAlgebra("A",2)
	    positiveRoots(sl3)
///

TEST ///
    assert(set positiveRoots(simpleLieAlgebra("A",2)) === set {{2, -1}, {1, 1}, {-1, 2}})
///	

doc ///
    Key
        simpleRoots
	(simpleRoots,String,ZZ)
	(simpleRoots,LieAlgebra)
    Headline
        returns the simple roots of a simple Lie algebra
    Usage
        simpleRoots(g), simpleRoots("A",2)
    Inputs
        g:LieAlgebra
    Outputs
        t:List
///

doc ///
    Key
        starInvolution
	(starInvolution,LieAlgebraModule)
	(dual,LieAlgebraModule)
    Headline
        computes w* for a weight w
    Usage
        starInvolution(w,g)
    Inputs
        w:List
	g:LieAlgebra
    Description
        Text
	    Let $\mathbf{g}$ be a Lie algebra.  We give three equivalent descriptions of an involution * on the weights of $\mathbf{g}$: 
	    
	Text 
	    1. The involution * is given by $-w_0$, where $w_0$ is the longest word in the Weyl group $W(\mathbf{g})$.
		  
	Text
	    2. If $\mu$ is a dominant integral weight, and $V_{\mu}$ is the irreducible Lie algebra module with highest weight $\mu$, then $\mu^*$ is the highest weight of the dual module $(V_{\mu})^*$.
		  
	Text 
	    3. If the Dynkin diagram of $\mathbf{g}$ has an involution, then * corresponds to the action of this involution on weights.
		  
        Text
            The formulas implemented have been adapted from Di Francesco, Mathieu, and Senechal, {\it Conformal Field Theory}, Springer Graduate Texts in Theoretical Physics, p. 511.  Some changes are needed because we use the Bourbaki ordering of the roots in type E instead of the [DMS] ordering.
	       
	Text     
	    In the example below, we see that for $sl_3$, $\omega_1^* = \omega_2.$
        
	Example
	     g=simpleLieAlgebra("A",2)
	     starInvolution(LL_(1,0)(g))
///

TEST ///
    g=simpleLieAlgebra("A",2)
    assert(starInvolution({1,0},g) === {0,1})
///



doc ///
    Key
        killingForm
	(killingForm,LieAlgebra,List,List)
	(killingForm,LieAlgebra,Vector,Vector)
    Headline 
        computes the scaled Killing form applied to two weights
    Usage 
        killingForm(g,v,w)
    Inputs 
        g:LieAlgebra
	v:List
	w:List
    Description
        Text
	    Let $\mathbf{g}$ be a Lie algebra.  The Killing form on $\mathbf{g}$ is the symmetric bilinear form given by $(x,y) = Tr(ad x ad y)$.  It can restricted to a Cartan subalgebra $\mathbf{h}$ and transferred to $\mathbf{h}^*$, yielding a symmetric bilinear form on weights.  One popular convention is to scale the Killing form so that $(\theta,\theta) =2$, where $\theta$ is the highest root.
	    
        Example
            g=simpleLieAlgebra("A",2)
	    killingForm(g,{1,0},{0,1})
///

TEST ///
    g=simpleLieAlgebra("A",2)
    assert(killingForm(g,{1,0},{0,1}) === 1/3)
    assert(lift(matrix table(simpleRoots g,simpleRoots g,(v,w)->killingForm(g,v,w)),ZZ) == cartanMatrix g) -- true for all simply laced
///
	
doc ///
    Key
        weylAlcove
	(weylAlcove,String,ZZ,ZZ)
	(weylAlcove,LieAlgebra,ZZ)
	(weylAlcove,ZZ,LieAlgebra)
    Headline 
        the dominant integral weights of level less than or equal to l
    Usage 
        weylAlcove(g,l)
    Inputs 
        g:LieAlgebra
        l:ZZ
    Description
        Text
            Let $\mathbf{g}$ be a Lie algebra, and let $l$ be a nonnegative integer.
	    Choose a Cartan subalgebra $\mathbf{h}$ and a base $\Delta= \{ \alpha_1,\ldots,\alpha_n\}$ of simple roots of $\mathbf{g}$.
	    These choices determine a highest root $\theta$. (See @TO highestRoot@).
	    Let $\mathbf{h}_{\mathbf{R}}^*$ be the real span of $\Delta$, and let $(,)$ denote the Killing form, normalized so that $(\theta,\theta)=2$.
	    The fundamental Weyl chamber is $C^{+} = \{ \lambda \in \mathbf{h}_{\mathbf{R}}^*  : (\lambda,\alpha_i) \ge 0, i=1,\ldots,n \}$.
	    The fundamental Weyl alcove is the subset of the fundamental Weyl chamber such that $(\lambda,\theta) \leq l$.
	    This function computes the set of integral weights in the fundamental Weyl alcove.
	    
        Text
            In the example below, we see that the Weyl alcove of $sl_3$ at level 3 contains 10 integral weights.
	    
	Example 
	    g=simpleLieAlgebra("A",2)
	    weylAlcove(g,3)
///

TEST ///
    g=simpleLieAlgebra("A",2)
    assert(set(weylAlcove(g,3)) === set {{0, 0}, {1, 0}, {0, 1}, {1, 1}, {2, 0}, {2, 1}, {0, 2}, {1, 2}, {3, 0}, {0, 3}}) 
///	
	

doc ///
    Key
        LieAlgebraModule
    Headline
        class for Lie algebra modules
    Description
        Text 
    	    This class represents Lie algebra modules.  Currently only modules over semi-simple Lie algebras over the complex numbers are supported.
	    An object of type LieAlgebraModule is a hash table recording the Lie algebra and the decomposition of the module into irreducible Lie algebra modules, which are indexed by their highest weights.
	    
	Example
	    g=simpleLieAlgebra("A",2)
	    M=irreducibleLieAlgebraModule(g,{1,1})
///

doc ///
    Key
        irreducibleLieAlgebraModule
	(irreducibleLieAlgebraModule,LieAlgebra,List)
	(irreducibleLieAlgebraModule,LieAlgebra,Vector)
	LL
	ω
    Headline
        construct the irreducible Lie algebra module with given highest weight
    Usage
        irreducibleLieAlgebraModule(w,g)
        irreducibleLieAlgebraModule(g,w)
    Inputs
        w:List
	    the highest weight of the desired module
	g:LieAlgebra     
    Outputs
        M:LieAlgebraModule
    Description
        Text
            This function creates the irreducible Lie algebra module with a given highest weight.
	Example
	    g=simpleLieAlgebra("A",2)
            irreducibleLieAlgebraModule(g,{1,1})
        Text
	    One can also use the shorthand LL:
	Example
            LL_(1,1) (g)
	Text
	    as well as the shorthand ω:
	    LL_(ω_2) (g)
///

TEST ///
    assert(irreducibleLieAlgebraModule({1,1},simpleLieAlgebra("A",2)) === new LieAlgebraModule from (simpleLieAlgebra("A",2),{{1,1}=>1} ))
///	
		
doc ///
    Key 
	(multiplicity,List,LieAlgebraModule)
	(multiplicity,Vector,LieAlgebraModule)
    Headline
        compute the multiplicity of a weight in a Lie algebra module
    Usage
        multiplicity(v,M)
    Inputs
        v:List
	M:LieAlgebraModule
    Outputs
        k:ZZ
    Description
	Text     
	    The example below shows that the $sl_3$ module with highest weight $(2,1)$ contains the weight $(-1,1)$ with multiplicity 2.
         
	Example
	    g=simpleLieAlgebra("A",2)
	    V=irreducibleLieAlgebraModule({2,1},g)
	    multiplicity({-1,1},V)
    SeeAlso
        weightDiagram
	     
///

TEST ///
    assert(multiplicity({-1,1},irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2))) === 2)
///

doc ///
    Key
	(dim,LieAlgebraModule)
    Headline
        computes the dimension of a Lie algebra module as a vector space over the ground field
    Usage
        dim(V)
    Inputs 
        V:LieAlgebraModule
    Outputs
        k:ZZ
    Description
        Example
	    g=simpleLieAlgebra("A",2)
	    V=irreducibleLieAlgebraModule({1,0},g)
	    dim(V)
///
TEST ///
    g=simpleLieAlgebra("A",2)
    V=irreducibleLieAlgebraModule({1,0},g)
    assert(dim(V) === 3)
    W=irreducibleLieAlgebraModule({5,2},g)
    assert(dim W == sum values weightDiagram W)
///

doc ///
    Key
        weightDiagram
	(weightDiagram,LieAlgebraModule)
	(weightDiagram,LieAlgebra,List)
	(weightDiagram,LieAlgebra,Vector)
	[weightDiagram,Strategy]
    Headline
        computes the weights in a Lie algebra module and their multiplicities
    Usage
        weightDiagram(V)
    Inputs
        V:LieAlgebraModule
    Outputs
        T:VirtualTally
    Description
        Text
	    Let $V$ be the irreducible $\mathbf{g}$-module with highest weight $v$.  This function returns a tally whose keys are the weights appearing in $V$ and whose values are the multiplicities of these weights.
	    An optional argument {\tt "Strategy"} allows to specify which algorithm to use, see @TO character@.
	     
        Example
	     g=simpleLieAlgebra("A",2)
	     V=irreducibleLieAlgebraModule({2,1},g)
	     weightDiagram(V)
	     
    SeeAlso
        (multiplicity,List,LieAlgebraModule)
	character
///

TEST ///
    assert(weightDiagram(irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2))) === new VirtualTally from {{{-1, 1}, 2}, {{1, 0}, 2}, {{3, -1}, 1}, {{-2, 0}, 1}, {{0, -1}, 2}, {{2, -2}, 1}, {{-2, 3}, 1}, {{0, 2}, 1}, {{2, 1}, 1}, {{-1, -2}, 1}, {{1, -3}, 1}, {{-3, 2}, 1}})
///	

	

doc ///
    Key
	(symbol **, LieAlgebraModule, LieAlgebraModule)
    Headline
        tensor product of LieAlgebraModules
    Usage
        U ** V
    Inputs
        U:LieAlgebraModule
	V:LieAlgebraModule
    Outputs
        W:LieAlgebraModule
    Description
        Text
	    Computes the tensor product of two Lie algebra modules.  
	       
        Example
	    g=simpleLieAlgebra("A",2)
	    U=irreducibleLieAlgebraModule({4,2},g)
	    V=irreducibleLieAlgebraModule({3,1},g)
	    U**V
	    
    SeeAlso
        tensorCoefficient
///

TEST ///
    assert(irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2)) ** irreducibleLieAlgebraModule({1,2},simpleLieAlgebra("A",2)) === new LieAlgebraModule from (simpleLieAlgebra("A",2), {{{1, 1}, 2}, {{3, 0}, 1}, {{1, 4}, 1}, {{3, 3}, 1}, {{0, 0}, 1}, {{0, 3}, 1}, {{2, 2}, 2}, {{4, 1}, 1}} ))
///

doc ///
    Key
	(symbol ++, LieAlgebraModule, LieAlgebraModule)
	(directSum, LieAlgebraModule)
    Headline
        direct sum of LieAlgebraModules
    Usage
        U ++ V
    Inputs
        U:LieAlgebraModule
	V:LieAlgebraModule
    Outputs
        W:LieAlgebraModule
    Description
        Text
	    Computes the direct sum of two Lie algebra modules.  
	    
        Example
	    g=simpleLieAlgebra("A",2)
	    U=irreducibleLieAlgebraModule({4,2},g)
	    V=irreducibleLieAlgebraModule({3,1},g)
	    U++V
///

TEST ///
    assert(irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2)) ** irreducibleLieAlgebraModule({1,2},simpleLieAlgebra("A",2)) === new LieAlgebraModule from (simpleLieAlgebra("A",2), {{{1, 1}, 2}, {{3, 0}, 1}, {{1, 4}, 1}, {{3, 3}, 1}, {{0, 0}, 1}, {{0, 3}, 1}, {{2, 2}, 2}, {{4, 1}, 1}} ))
///

doc ///
    Key
        tensorCoefficient
	(tensorCoefficient,LieAlgebraModule,LieAlgebraModule,LieAlgebraModule)     
    Headline
        computes the multiplicity of W in U tensor V
    Usage
        tensorCoefficient(U,V,W)
    Inputs
        U:LieAlgebraModule
	V:LieAlgebraModule
	W:LieAlgebraModule
    Outputs
        k:ZZ
    Description
        Text
	    This function implements the Racah-Speiser algorithm; see Di Francesco, Mathieu, and Senechal, {\it Conformal Field Theory}, Springer Graduate Texts in Theoretical Physics, Section 13.5.2. 
	       
	Text     
	    Given three irreducible Lie algebra modules $U$, $V$, and $W$, the function returns the multiplicity of $W$ in $U \otimes V$.  In Type A, these are related to the Littlewood-Richardson coefficients (though in this package, irreducible representations are indexed by the Dynkin labels of their highest weights, rather than by partitions).  
	   
        Text
	    The example below shows that for $g=sl_3$ and $\lambda=2 \omega_1 + \omega_2$, $\mu= \omega_1 + 2 \omega_2$, and $\nu= 2 \omega_1 + 2 \omega_2$, the tensor product of $sl_3$ modules $V_{\lambda} \otimes V_{\mu}$ contains two copies of $V_{\nu}$.
	       
        Example
	    g=simpleLieAlgebra("A",2)
	    U=irreducibleLieAlgebraModule({2,1},g)
	    V=irreducibleLieAlgebraModule({1,2},g)
	    W=irreducibleLieAlgebraModule({2,2},g)
	    tensorCoefficient(U,V,W)
    SeeAlso
        (symbol **, LieAlgebraModule, LieAlgebraModule)
///


TEST ///
    g=simpleLieAlgebra("A",2);
    U=irreducibleLieAlgebraModule({2,1},g);
    V=irreducibleLieAlgebraModule({1,2},g);
    W=irreducibleLieAlgebraModule({2,2},g);
    assert(tensorCoefficient(U,V,W) === 2)         
///
		


doc ///
    Key
        fusionCoefficient
	(fusionCoefficient,LieAlgebraModule,LieAlgebraModule,LieAlgebraModule,ZZ)     
    Headline
        computes the multiplicity of W in the fusion product of U and V
    Usage
        fusionCoefficient(U,V,W,l)
    Inputs
        U:LieAlgebraModule
	V:LieAlgebraModule
	W:LieAlgebraModule
        l:ZZ	
    Description
        Text
	    This function implements the Kac-Walton algorithm; see Di Francesco, Mathieu, and Senechal, {\it Conformal Field Theory}, Springer Graduate Texts in Theoretical Physics, Section 16.2.2.  
	    
	Text    
	    Given three irreducible Lie algebra modules $U$, $V$, and $W$, the function returns the multiplicity of $W$ in the fusion product of $U$ and $V$ at level $l$.  (We are abusing notation and terminology a little here; the fusion product is really a product for modules over an affine Lie algebra.  However, since the Kac-Walton algorithm is defined entirely using the combinatorics of the root system of the underlying finite-dimensional Lie algebra, we may therefore use the Kac-Walton algorithm to define a product on Lie algebra modules as well.)
       
       
	Text
	    The example below shows that for $g=sl_3$ and $\lambda=2 \omega_1 + \omega_2$, $\mu= \omega_1 + 2 \omega_2$, and $\nu= \omega_1 +  \omega_2$, the level 3 fusion product  $V_{\lambda} \otimes_3  V_{\mu}$ contains one copy of $V_{\nu}$.
	    
        Example
	    g=simpleLieAlgebra("A",2);
	    U=irreducibleLieAlgebraModule({2,1},g);
	    V=irreducibleLieAlgebraModule({1,2},g);
	    W=irreducibleLieAlgebraModule({1,1},g);
	    fusionCoefficient(U,V,W,3)
///

doc ///
    Key
       LieAlgebraModuleFromWeights
       (LieAlgebraModuleFromWeights,VirtualTally,LieAlgebra)
       (LieAlgebraModuleFromWeights,RingElement,LieAlgebra)
    Headline
       finds a Lie algebra module based on its weights
    Usage
        LieAlgebraModuleFromWeights(T,g)
    Inputs
        T:Tally
	g:LieAlgebra
    Description
        Example
	    g=simpleLieAlgebra("A",2);
	    U=irreducibleLieAlgebraModule({1,1},g);
	    M=U**U
	    T=weightDiagram M
            LieAlgebraModuleFromWeights(T,g)
///
doc ///
    Key
        fusionProduct
	(fusionProduct,LieAlgebraModule,LieAlgebraModule,ZZ)     
    Headline
        computes the multiplicities of irreducibles in the decomposition of the fusion product of U and V
    Usage
        fusionProduct(U,V,l)
    Inputs
        U:LieAlgebraModule
	V:LieAlgebraModule
        l:ZZ
    Description
        Text
	    This function implements the Kac-Walton algorithm; see Di Francesco, Mathieu, and Senechal, {\it Conformal Field Theory}, Springer Graduate Texts in Theoretical Physics, Section 16.2.2.  
	    
 	Text   
	    Given two irreducible Lie algebra modules $U$ and $V$, the function returns the fusion product of $U$ and $V$ at level $l$.  (We are abusing notation and terminology a little here; the fusion product is really a product for modules over an affine Lie algebra.  However, since the Kac-Walton algorithm is defined entirely using the combinatorics of the root system of the underlying finite-dimensional Lie algebra, we may therefore use the Kac-Walton algorithm to define a product on Lie algebra modules as well.)  
	    
	    
        Text
	    The example below shows that for $g=sl_3$ and $\lambda=2 \omega_1 + \omega_2 = (2,1)$, $\mu= \omega_1 + 2 \omega_2 = (1,2)$, the level 3 fusion product  $V_{(2,1)} \otimes_3  V_{(1,2)}$ contains one copy of $V_{(0,0)}$ and one copy of $V_{(1,1)}$.
	    
        Example
	    g=simpleLieAlgebra("A",2);
	    U=irreducibleLieAlgebraModule({2,1},g);
	    V=irreducibleLieAlgebraModule({1,2},g);
	    fusionProduct(U,V,3)
///


TEST ///
    g=simpleLieAlgebra("A",2);
    U=irreducibleLieAlgebraModule({2,1},g);
    V=irreducibleLieAlgebraModule({1,2},g);
    W=irreducibleLieAlgebraModule({1,1},g);
    assert(fusionCoefficient(U,V,W,3) === 1)         
///



doc ///
    Key
        casimirScalar
	(casimirScalar,LieAlgebraModule)
    Headline
        computes the scalar by which the Casimir operator acts on an irreducible Lie algebra module
    Usage
        casimirScalar(V)
    Inputs 
        V:LieAlgebraModule
    Outputs
        k:QQ
    Description
        Text
	    The Casimir operator is an element of the universal enveloping algebra that acts by a scalar on each irreducible Lie algebra module.  One has $c(\mu) = (\mu,\mu) + 2(\mu,\rho)$, where $\rho$ is half the sum of the positive weights and (,) is the Killing form scaled so that $(\theta,\theta)=2$, where $\theta$ is the highest root.  See Di Francesco, Mathieu, and Senechal, {\it Conformal Field Theory}, Springer Graduate Texts in Theoretical Physics, (13.127) p. 512, and (13.46) p. 499.
	    
	Text     
            In the example below, we see that the Casimir operator acts as multiplication by 8/3 on the standard representation of $sl_3$.  
         
	Example
	    g=simpleLieAlgebra("A",2)
	    V=irreducibleLieAlgebraModule({1,0},g)
	    casimirScalar(V)
///

TEST ///
    g=simpleLieAlgebra("A",2)
    V=irreducibleLieAlgebraModule({1,0},g)
    assert(casimirScalar(V) === 8/3)
///

-*
doc ///
    Key
        isIsomorphic
	(isIsomorphic,LieAlgebraModule,LieAlgebraModule)
    Headline
        tests whether two Lie algebra modules are isomorphic
    Usage
        isIsomorphic(V,W)
    Inputs
        V:LieAlgebraModule
	W:LieAlgebraModule
    Outputs
        b:Boolean
    Description
        Text
	    To test whether two Lie algebra modules are isomorphic, we first test whether they are modules over the same Lie algebra, and if so, then test whether they have the same decomposition into irreducible Lie algebra modules.
        
	Example
	    g=simpleLieAlgebra("A",2)
	    M=irreducibleLieAlgebraModule({2,1},g)
	    N=irreducibleLieAlgebraModule({1,2},g)
	    Z=irreducibleLieAlgebraModule({0,0},g)
	    isIsomorphic(M,N)
	    isIsomorphic(M,M)
	    isIsomorphic(M,M**Z)
	    isIsomorphic(M**N,N**M)
///

TEST ///
    g=simpleLieAlgebra("A",2);
    M=irreducibleLieAlgebraModule({2,1},g);
    N=irreducibleLieAlgebraModule({1,2},g);
    Z=irreducibleLieAlgebraModule({0,0},g);
    assert(isIsomorphic(M,N) === false)
    assert(isIsomorphic(M,M) === true)
    assert(isIsomorphic(M,M**Z) === true)
    assert(isIsomorphic(M**N,N**M) ===true)
///

doc ///
    Key
        MaxWordLength
        [fusionCoefficient, MaxWordLength]
        [fusionProduct, MaxWordLength]
    Headline
        Optional argument to specify the allowable length of words in the affine Weyl group when computing fusion products.
    Description
        Text
            The Weyl group of a simple Lie algebra is finite; in contrast, the affine Weyl group of an affine Lie algebra is infinite.  To keep Macaulay2 from trying to compute infinitely long words in this group, the default length of allowed words is set to max \{10, rank($\mathbf{g}$)+1\}.   The user may override this with the optional argument "MaxWordLength".  If the word length is too small, the program will return an error.

///
*-

doc ///
    Key
        character
	(character,LieAlgebraModule)
	(character,LieAlgebra,List)
	(character,LieAlgebra,Vector)
	[character,Strategy]
    Headline
        Computes the character of a Lie algebra module
    Usage
        character V
    Inputs
        V:LieAlgebraModule
    Outputs
        C:RingElement
    Description
        Text
	    An optional argument {\tt "Strategy"} allows to specify which algorithm to use:
	    {\tt "Freudenthal"} for Freudenthal's recursive algorithm; see Humphreys, {\it Introduction to Lie Algebras and Representation Theory}, Section 22.3.
	    {\tt "Weyl"} for Weyl's character formula (in classical types).
	    {\tt "JacobiTrudi"} and {\tt "JacobiTrudi'"} for Jacobi-Trudi and dual Jacobi-Trudi formulae (in type A).
    SeeAlso
        weightDiagram
///

TEST ///
    g=simpleLieAlgebra("D",4);
    M=LL_(1,1,0,0) g;
    N=LL_(1,0,0,1) g;
    assert(character(M**N) == character M * character N)
///

doc ///
    Key
        isIrreducible
	(isIrreducible,LieAlgebraModule)
    Headline
        Whether a Lie algebra module is irreducible or not
    Description
        Example
	    g=simpleLieAlgebra("A",2)
	    M=irreducibleLieAlgebraModule({2,1},g)
	    isIrreducible M
	    isIrreducible(M++M)
	    isIrreducible(M**M)
///	

TEST ///
    g=simpleLieAlgebra("A",2);
    assert(isIrreducible irreducibleLieAlgebraModule({2,1},g))
///

doc ///
    Key
        trivialModule
	(trivialModule,LieAlgebra)
    Headline
        The trivial module of a Lie algebra
    Description
        Text
	    Returns the one-dimensional module with zero highest weight.
///

doc ///
    Key
        adjointModule
	(adjointModule,LieAlgebra)
    Headline
        The adjoint module of a Lie algebra
    Description
        Text
	    Returns the module corresponding to the adjoint representation of a Lie algebra.
        Example
	    g=simpleLieAlgebra("A",2)
	    adjointModule g
	    adjointModule (g++g)
///

TEST ///
    g=simpleLieAlgebra("A",2);
    M=irreducibleLieAlgebraModule({2,1},g);
    assert(M ** trivialModule g === M)
    assert(dim adjointModule(g++g)==2*dim adjointModule g)
///

doc ///
    Key
    	adams
	(adams,ZZ,LieAlgebraModule)
    Headline
        Computes the action of the nth Adams operator on a Lie algebra module
    Usage
        adams(n,M)
    Inputs
	n:ZZ
        M:LieAlgebraModule
    Outputs
        M':LieAlgebraModule
///

doc ///
    Key
    	(symmetricPower,ZZ,LieAlgebraModule)
	(exteriorPower,ZZ,LieAlgebraModule)
    Headline
        Computes the nth symmetric / exterior tensor power of a Lie algebra module
    Usage
        symmetricPower(n,M)
        exteriorPower(n,M)
    Inputs
	n:ZZ
        M:LieAlgebraModule
    Outputs
        M':LieAlgebraModule
///

TEST ///
    g=simpleLieAlgebra("A",3);
    M=irreducibleLieAlgebraModule({1,0,0},g);
    assert(exteriorPower(2,M) === irreducibleLieAlgebraModule({0,1,0},g));
    assert(exteriorPower(3,M) === irreducibleLieAlgebraModule({0,0,1},g));
    scan(1..5, i -> assert(symmetricPower(i,M) === irreducibleLieAlgebraModule({i,0,0},g)));
///

doc ///
    Key
	(symbol ^**,LieAlgebraModule,ZZ)
    Headline
        Computes the nth tensor power of a Lie algebra module
    Usage
        M^**n
    Inputs
        M:LieAlgebraModule
	n:ZZ
    Outputs
        M':LieAlgebraModule
///

TEST ///
    g=simpleLieAlgebra("B",3);
    M=irreducibleLieAlgebraModule({1,0,1},g);
    c=character M;
    scan(1..4, n -> assert(character(M^**n) == c^n))
///

doc ///
    Key
       qdim
       (qdim,LieAlgebraModule)
       (qdim,LieAlgebraModule,ZZ)
    Headline
       Compute principal specialization of character or quantum dimension
    Usage
       qdim M
       qdim(M,l)
    Inputs
        M:LieAlgebraModule
	l:ZZ
    Outputs
        P:RingElement
    Description
        Text
	    @TT "qdim M"@ computes the principal specialization of the character of @TT "M"@.
	    @TT "qdim (M,l)"@ evaluates it modulo the appropriate cyclotomic polynomial,
	    so that upon specialization of the variable $q$ to be the corresponding root of unity of smallest positive argument,
	    it provides the quantum dimension of @TT "M"@.
	Example
	    g=simpleLieAlgebra("A",2)
	    W=weylAlcove(g,3)
	    L=LL_(1,1) (g)
	    M=matrix table(W,W,(v,w)->fusionCoefficient(L,LL_v g,LL_w g,3))
	    first eigenvalues M
	    qdim L
	    qdim (L,3)
///

TEST ///
    g=simpleLieAlgebra("B",3);
    L=LL_(1,0,0) g;
    M=LL_(0,1,1) g;
    assert(qdim(L,3) * qdim(M,3) == qdim(fusionProduct(L,M,3),3))
///

doc ///
    Key
    	dynkinDiagram
	(dynkinDiagram,LieAlgebra)
    Headline
    	Provide the Dynkin diagram of a simple Lie algebra
    Description
	Example
	    g=simpleLieAlgebra("F",4)
	    dynkinDiagram g
///

doc ///
    Key
    	cartanMatrix
	(cartanMatrix,LieAlgebra)
    Headline
    	Provide the Cartan matrix of a simple Lie algebra
    Description
	Example
	    g=simpleLieAlgebra("G",2)
	    cartanMatrix g
///

TEST ///
    assert(cartanMatrix simpleLieAlgebra("B",2) == matrix {{2,-2},{-1,2}})
///

doc ///
    Key
    	subLieAlgebra
	(subLieAlgebra,LieAlgebra,List)
	(subLieAlgebra,LieAlgebra,Matrix)
    Headline
        Define a sub-Lie algebra of an existing one
    Usage
       subLieAlgebra(g,S)
    Inputs
        g:LieAlgebra
	S:{List,Matrix}
    Outputs
        h:LieAlgebra
    Description
        Text
	   @TT "S"@ must be a subset of vertices of the Dynkin diagram of @TT "g"@ (as labelled by @TO dynkinDiagram@);
	   or a matrix whose columns are the simple coroots of the subalgebra expanded in the basis of simple coroots of @TT "g"@.
	Example
	   g=𝔢_8; dynkinDiagram g
	   subLieAlgebra(g,{1,2,3,4,5,8})
	   h=𝔣_4; dynkinDiagram h
	   subLieAlgebra(h,matrix transpose{{1,0,0,0},{0,1,0,0},{0,0,1,0},-{2,3,2,1}}) -- simple coroots 1,2,3 and opposite of highest root
    Caveat
        If @TT "S"@ is a matrix, does not check if the map of root lattices leads to a valid Lie algebra embeddng.
///

TEST ///
g = simpleLieAlgebra("E",8)
h = subLieAlgebra(g,{2,3,4,5})
assert ( h#"LieAlgebraRank" === 4 and h#"RootSystemType" === "D" )
k = subLieAlgebra(h,{1,3,4})
assert ( k#"LieAlgebraRank" === (1,1,1) and k#"RootSystemType" === ("A","A","A") )
///

doc ///
    Key
        branchingRule
        (branchingRule,LieAlgebraModule,List)
        (branchingRule,LieAlgebraModule,Matrix)
        (branchingRule,LieAlgebraModule,LieAlgebra)
    Headline
        A Lie algebra module viewed as a module over a Lie subalgebra 
    Usage
        branchingRule(V,S)
    Inputs
        V:LieAlgebraModule
	S:{List,Matrix,LieAlgebra}
    Outputs
        V':LieAlgebraModule
    Description
        Text
	   @TT "S"@ must be a subset of vertices of the Dynkin diagram of the Lie algebra of @TT "V"@, or a matrix, see @TO subLieAlgebra@;
	   or a sub-Lie algebra.
	   Returns @TT "V"@ viewed as a module over the Lie subalgebra determined by @TT "S"@.
	Example
	    g=simpleLieAlgebra("D",4);
	    M=adjointModule g;
	    branchingRule(M,{1,2,3})
///

TEST ///
g=simpleLieAlgebra("A",2);
M=LL_(4,2) g;
assert(dim branchingRule(M,{1}) == dim M)
h=subLieAlgebra(g,matrix vector {2,2})
assert(branchingRule(LL_(1,0)(g),h) == LL_2(h))
///

doc ///
    Key
       (symbol ++,LieAlgebra,LieAlgebra)
       (directSum,LieAlgebra)
    Headline
        Take the direct sum of Lie algebras
    Description
        Text
	   Starting from simple Lie algebras, one can take direct sums and produce semi-simple ones:
	Example
	   g=simpleLieAlgebra("D",4);
	   h=simpleLieAlgebra("G",2);
	   g++h
	   directSum(g,g,h)
///

doc ///
    Key
       (symbol @,LieAlgebraModule,LieAlgebraModule)
    Headline
        Take the tensor product of modules over different Lie algebras
    Description
        Text
	   Produces a module over the direct sum of the Lie algebras of the two modules.
	Example
	   LL_(1,2,3,4) (simpleLieAlgebra("D",4)) @ LL_(5,6) (simpleLieAlgebra("G",2))
        Text
	   A complicated way to define usual tensor product @TO (symbol **,LieAlgebraModule,LieAlgebraModule)@ would be using the diagonal embedding:
	Example
	   g := simpleLieAlgebra("A",1)
	   h := g ++ g
	   gdiag := subLieAlgebra(h,matrix {{1},{1}})
	   M = LL_5 (g); M' = LL_2 (g);
	   M @ M'
	   branchingRule(oo,gdiag)
	   M ** M'
///

TEST ///
g=simpleLieAlgebra("A",2);
h=simpleLieAlgebra("B",2);
k=g++h
A=LL_(1,2) g
B=LL_(2,1) h
M=LL_(1,2,2,1) k;
assert ( M == A @ B )
assert(character(M,Strategy=>"Weyl")==character(M,Strategy=>"Freudenthal"))
///

doc ///
    Key
        (NewFromMethod,LieAlgebra,Matrix)
    Headline
        Define a Lie algebra from its Cartan matrix
    Description
        Text
	   @TT "new LieAlgebra from M"@

	   If M is a valid Cartan matrix, it will reorder if needed the rows/columns of M to a standard form and then output the
	   corresponding Lie algebra @TT "g"@.
	Example
	    M = matrix {{2, 0, -3, 0}, {0, 2, 0, -1}, {-1, 0, 2, 0}, {0, -1, 0, 2}}
	    h := new LieAlgebra from M
	    cartanMatrix h
///


undocumented ( {
    (describe,LieAlgebra),(expression,LieAlgebra),(net,LieAlgebra),(texMath,LieAlgebra),
    (describe,LieAlgebraModule),(expression,LieAlgebraModule),(net,LieAlgebraModule),(texMath,LieAlgebraModule),
    (symbol ==,LieAlgebraModule,LieAlgebraModule), (symbol ==,LieAlgebraModule,ZZ),
    (NewFromMethod,LieAlgebraModule,Sequence),
    (symbol ^,LieAlgebraModule,QQ),
    (irreducibleLieAlgebraModule,LieAlgebra,ZZ), (irreducibleLieAlgebraModule,LieAlgebra,VisibleList), (irreducibleLieAlgebraModule,LieAlgebra,Expression), (irreducibleLieAlgebraModule,Thing,LieAlgebra),
    (dynkinDiagram,String,ZZ),(cartanMatrix,String,ZZ),(cartanMatrix,Sequence,Sequence),(isSimple,String,ZZ),isSimple,(isSimple,LieAlgebra),
    (dim,LieAlgebra),(rank,LieAlgebra),
    (character,String,ZZ,List),(character,Sequence,Sequence,List),
    (dynkinDiagram,String,ZZ,ZZ),
    (positiveRoots,Sequence,Sequence),(positiveRoots,String,ZZ),(positiveCoroots,Sequence,Sequence),(positiveCoroots,String,ZZ),
    (killingForm,String,ZZ,List,List),(killingForm,Sequence,Sequence,List,List),
    (starInvolution,List,LieAlgebra),(starInvolution,Vector,LieAlgebra),(starInvolution,LieAlgebra,List),(starInvolution,LieAlgebra,Vector),(starInvolution,String,ZZ,List),(starInvolution,Sequence,Sequence,List),
    (casimirScalar,String,ZZ,List),(casimirScalar,Sequence,Sequence,List),
    (highestRoot,String,ZZ),(highestRoot,Sequence,Sequence),
    } | values fraktur)

endPackage "LieTypes" 
