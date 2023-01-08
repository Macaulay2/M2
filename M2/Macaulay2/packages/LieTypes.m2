-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version
newPackage(
    "LieTypes",
    Version => "0.6",
    Date => "Jan 6, 2023",
    Headline => "common types for Lie groups and Lie algebras",
    Authors => {
	  {Name => "Dave Swinarski", Email => "dswinarski@fordham.edu"}
	  },
    Keywords => {"Lie Groups and Lie Algebras"},
    PackageImports => {"ReesAlgebra"},
    DebuggingMode => true,
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
    "KillingForm",
    "weylAlcove",
    --for the LieAlgebraModule type
    "LieAlgebraModule", 
    "irreducibleLieAlgebraModule", "LL",
    "isIsomorphic",
    "casimirScalar",
    "weightDiagram",
    "tensorCoefficient",
    "fusionProduct",
    "fusionCoefficient",
    "MaxWordLength",
    "positiveRoots",
    "LieAlgebraModuleFromWeights",
    "trivialModule",
    "isIrreducible",
    "character"
    }

tim=0;

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

LieAlgebras have three keys: RootSystemType, LieAlgebraRank, and isSimple
The functions available for LieAlgebras are:
simpleLieAlgebra
dualCoxeterNumber
highestRoot
starInvolution
KillingForm
weylAlcove

LieAlgebraModules have two keys: LieAlgebra and DecompositionIntoIrreducibles
The functions available for LieAlgebraModules are:
dimension
weights
casimirScalar
tensor product decomposition
fusion coefficient

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
-- Summary, Version 0.6, January 2023 (Paul Zinn-Justin)
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

* Improved output methods
* Introduced shorthand LL for simple (irreducible) modules
* Fixed and exported LieAlgebraModuleFromWeights
* Fixed and optimized tensor product of modules
* Added ^** and ^ for modules
* Added trivialModule
* Additional sanity checks
* Allow inputting weights as vectors
* isIrreducible is now a method
* use of Tally rather than HashTable for its methods
* Added character method
* character and weightDiagram have 2 strategies, Weyl or Freudenthal
  (Weyl seems slower for small reps, but significantly faster for large highest weights)
*-



-----------------------------------------------------------------------
-- LieAlgebra= {
--   LieAlgebraRank => ZZ, dim of Cartan subalgebra
--   RootSystemType => String, type A through G
--   isSimple => Boolean
--   }

LieAlgebra = new Type of HashTable  
LieAlgebra.GlobalAssignHook = globalAssignFunction
LieAlgebra.GlobalReleaseHook = globalReleaseFunction
describe LieAlgebra := g -> Describe (
    if g#"isSimple" then (expression simpleLieAlgebra) (g#"RootSystemType",g#"LieAlgebraRank")
    else concatenate("Nonsimple Lie algebra, type ",toString(g#"RootSystemType"),", rank ",toString(g#"LieAlgebraRank"))
)	
expression LieAlgebra := g -> if hasAttribute(g,ReverseDictionary) then expression getAttribute(g,ReverseDictionary) else unhold describe g;
net LieAlgebra := X -> net expression X;
texMath LieAlgebra := X -> texMath expression X;

rank LieAlgebra := g -> g#"LieAlgebraRank"

LieAlgebra == LieAlgebra := (V,W)-> (V===W)

simpleLieAlgebra = method(
    TypicalValue => LieAlgebra
    )
simpleLieAlgebra(String,ZZ) := (type,m) -> (
    if not member(type,{"A","B","C","D","E","F","G"}) then error "The simple Lie algebras over the complex numbers have types A, B, C, D, E, F, or G";
    if type=="A" and m<= 0 then error "The rank for type A must be >= 1.";
    if type=="B" and m<= 1 then error "The rank for type B must be >= 2.";
    if type=="C" and m<= 1 then error "The rank for type C must be >= 2.";
    if type=="D" and m<= 2 then error "The rank for type D must be >= 3.";
    if type=="E" and not member(m,{6,7,8}) then error "The rank for type E must be 6, 7, or 8.";
    if type=="F" and m!=4 then error "The rank for type F must be 4.";
    if type=="G" and m!=2 then error "The rank for type G must be 2.";
    new LieAlgebra from {"LieAlgebraRank"=>m,"RootSystemType"=>type,"isSimple"=>true,cache=>new CacheTable from {
	    x:=getSymbol "x";
	    "Ring" => ZZ(monoid [x_1..x_m,Inverses=>true,MonomialOrder=>Lex])}}
    )
-*simpleLieAlgebra(IndexedVariable) := (v) -> (
    if #v > 2 or not member(v#0,{symbol sl, symbol so, symbol sp}) or not instance(v#1,ZZ) then error "Input not understood; enter sl_k, sp_k, or so_k, or use the syntax simpleLieAlgebra(\"A\",1) instead";
    k:=v#1;
    if v#0 == symbol sl and k >= 2 then return simpleLieAlgebra("A",k-1);
    if v#0 == symbol so and odd(k) and k>=5  then return simpleLieAlgebra("B",lift((k-1)/2,ZZ));
    if v#0 == symbol sp and even(k) and k >= 4 then return simpleLieAlgebra("C",lift(k/2,ZZ));
    if v#0 == symbol so and even(k) and k >= 8 then return simpleLieAlgebra("D",lift(k/2,ZZ));
    )
*-
    
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
    ));   
dualCoxeterNumber(LieAlgebra) := memoize((g) -> (--see Appendix 13.A, [DMS]
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    dualCoxeterNumber(type,m)	  
    )); 


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
));

highestRoot(LieAlgebra) := memoize((g) -> (--see Appendix 13.A, [DMS]
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";   
    highestRoot(type,m)
));

starInvolution = method()
starInvolution(String,ZZ,List) := memoize((type, m, w) ->  ( N:=#w;
    if type == "A" then return reverse w;
    if type == "B" or type == "C" or type == "F" or type == "G" then return w;
    if type == "E" and m!= 6 then return w;
    if type == "D" and even(m) then return w;
    if type == "D" and odd(m) then (x:=w;
        return append(drop(x,{#x-2,#x-2}),w_(#w-2)));
    if type == "E" and m== 6 then return {w_5,w_1,w_4,w_3,w_2,w_0};
    ));
starInvolution(String,ZZ,Vector) := (type,m,w) -> starInvolution(type,m,entries w)
starInvolution(List,LieAlgebra) := memoize((v,g) -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";   
    starInvolution(type,m,v)
));
starInvolution(Vector,LieAlgebra) := (v,g) -> starInvolution(entries v,g)

ring LieAlgebra := g -> g.cache#"Ring"
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
LL = new ScriptedFunctor from { subscript => w -> g -> irreducibleLieAlgebraModule(deepSplice toList w,g) }
LL.texMath = ///{\mathcal L}///

describe LieAlgebraModule := M -> Describe (
    dec := M#"DecompositionIntoIrreducibles";
    g := Parenthesize expression M#"LieAlgebra";
    if #dec == 0 then expression 0
    else fold((a,b)->a++b, -- TODO rewrite using DirectSum to avoid error: at print: recursion limit of 300 exceeded
	apply(pairs dec,(v,mul) -> ((expression LL)_(toSequence v) g)^mul))
    )
expression LieAlgebraModule := M -> if hasAttribute(M,ReverseDictionary) then expression getAttribute(M,ReverseDictionary) else unhold describe M;

net LieAlgebraModule := V -> net expression V
texMath LieAlgebraModule := V -> texMath expression V


isIrreducible = method()
isIrreducible LieAlgebraModule := M -> (
    dec := M#"DecompositionIntoIrreducibles";
    #dec == 1 and first values dec == 1
    )

LieAlgebraModule ^ ZZ := (M,n) -> (
    if n<0 then error "nonnegative powers only"
    else if n==1 then M
    else new LieAlgebraModule from hashTable {
	"LieAlgebra" => M#"LieAlgebra",
	"DecompositionIntoIrreducibles" => if n==0 then new Tally else applyValues(M#"DecompositionIntoIrreducibles", a -> a*n)
	}
    )

LieAlgebraModule#AfterPrint = V -> (
    if isIrreducible V then "irreducible ",
    V#"LieAlgebra",
    " - module"
 )

trivialModule = method(TypicalValue => LieAlgebraModule)
trivialModule LieAlgebra := g -> irreducibleLieAlgebraModule(toList(g#"LieAlgebraRank":0),g)

LieAlgebraModule ^** ZZ := (M,n) -> (
    if n<0 then "error nonnegative powers only"
    else if n==0 then trivialModule M#"LieAlgebra"
    else if n==1 then M
    else M**(M^**(n-1)) -- order matters for speed purposes
    )

starInvolution LieAlgebraModule := M -> new LieAlgebraModule from {
    "LieAlgebra" => M#"LieAlgebra",
    "DecompositionIntoIrreducibles" => applyKeys(M#"DecompositionIntoIrreducibles", v -> starInvolution(v,M#"LieAlgebra"))
    }
dual LieAlgebraModule := {} >> o -> lookup(starInvolution,LieAlgebraModule)


isIsomorphic = method(
    TypicalValue => Boolean
    )
isIsomorphic(LieAlgebraModule,LieAlgebraModule) := (M,N) -> ( -- actually this is the same as ===
    if M#"LieAlgebra" != N#"LieAlgebra" then return false;
    M#"DecompositionIntoIrreducibles"===N#"DecompositionIntoIrreducibles"
)

LieAlgebraModule == ZZ := (M,n) -> if n=!=0 then error "attempted to compare module to nonzero integer" else #(M#"DecompositionIntoIrreducibles") == 0

directSum LieAlgebraModule := identity
LieAlgebraModule.directSum = args -> (
    if not same apply(args, M -> M#"LieAlgebra") then error "modules must be over the same Lie algebra";
    new LieAlgebraModule from {
	"LieAlgebra"=>(first args)#"LieAlgebra",
	"DecompositionIntoIrreducibles"=>sum(args,M->M#"DecompositionIntoIrreducibles")
	}
)
LieAlgebraModule ++ LieAlgebraModule := directSum

irreducibleLieAlgebraModule = method(
    TypicalValue => LieAlgebraModule
    )
irreducibleLieAlgebraModule(List,LieAlgebra) := (v,g) -> (
    if #v != rank g or not all(v, a -> class a === ZZ) then error "wrong highest weight";
    new LieAlgebraModule from {"LieAlgebra"=>g,"DecompositionIntoIrreducibles"=>tally {v}}
    )
irreducibleLieAlgebraModule(Vector,LieAlgebra) := (v,g) -> irreducibleLieAlgebraModule(entries v,g)


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
*-

cartanMatrixQQ = memoize((type, m) ->( M:={};
	  i:=0;
    if type=="A" then (
        return matrix apply(m, i-> (1/1)*apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0))
    );
    if type=="B" then (
        M = apply(m-2, i ->  (1/1)*apply(m, j -> if j==i-1 then -1 else if j==i then 2 else if j==i+1 then -1 else 0)); 
        M = append(M, (1/1)*apply(m, j -> if j==(m-2)-1 then -1 else if j==(m-2)then 2 else if j==(m-2)+1 then -2 else 0)); 
        M = append(M, (1/1)*apply(m, j -> if j==(m-1)-1 then -1 else if j==(m-1) then 2 else if j==(m-1)+1 then -1 else 0));
        return matrix M
    );
    if type=="C" then (
        M = apply(m-2, i -> (1/1)*apply(m, j -> if j==i-1 then -1/1 else if j==i then 2 else if j==i+1 then -1 else 0)); 
        M = append(M, (1/1)*apply(m, j -> if j==m-2-1 then -1 else if j==m-2 then 2 else if j==m-2+1 then -2 else 0)); 
        M = append(M, (1/1)*apply(m, j -> if j==m-1-1 then -1 else if j==m-1 then 2 else if j==m-1+1 then -1 else 0));
        return transpose matrix M
    );
    if type=="D" then (
        M = apply(m-3, i -> (1/1)*apply(m, j -> if j==i-1 then -1/1 else if j==i then 2 else if j==i+1 then -1 else 0));
        M = append(M,(1/1)*apply(m, j -> if j==m-3-1 then -1 else if j==m-3 then 2 else if j==m-3+1 then -1 else if j==m-3+2 then -1 else 0));
        M = append(M,(1/1)*apply(m, j -> if j==m-2 then 2 else if j==m-2-1 then -1 else 0));
        M = append(M,(1/1)*apply(m, j -> if j==m-1 then 2 else if j==m-1-2 then -1 else 0));
        return matrix M
    );
    if type=="E" and m==6 then (
        return matrix {{2/1, 0, -1, 0, 0, 0}, {0, 2, 0, -1, 0, 0}, {-1, 0, 2, -1, 0, 0}, {0, -1, -1, 2, -1, 0}, {0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, -1, 2}});  
    if type=="E" and m==7 then (
	return matrix {{2/1, 0, -1, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0}, {0, -1, -1, 2, -1, 0, 0}, {0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, -1, 2}});
    if type=="E" and m==8 then (
	return matrix {{2/1, 0, -1, 0, 0, 0, 0, 0}, {0, 2, 0, -1, 0, 0, 0, 0}, {-1, 0, 2, -1, 0, 0, 0, 0}, {0, -1, -1, 2, -1, 0, 0, 0}, {0, 0, 0, -1, 2, -1, 0, 0}, {0, 0, 0, 0, -1, 2, -1, 0}, {0, 0, 0, 0, 0, -1, 2, -1}, {0, 0, 0, 0, 0, 0, -1, 2}});
    if type == "F" then return matrix({{2/1,-1,0,0},{-1,2,-2,0},{0,-1,2,-1},{0,0,-1,2}});
    if type == "G" then return matrix({{2/1,-1},{-3,2}});
    ));


--We code what Di Francesco, Mathieu, and Senechal call the quadratic form matrix
--For types A,D,E, it is the inverse of the Cartan matrix.  See paragraph 1, [DMS] p. 498 and (13.51), [DMS] p. 499 
--For the other types Appendix 13.A, [DMS]


quadraticFormMatrix = memoize((type, m) -> ( M:={};
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
	  ));	 
    


KillingForm = method(
    TypicalValue => QQ
    )     
KillingForm(String,ZZ,List,List) := memoize((type, m, v,w) ->   (
    ((matrix({(1/1)*v})*(quadraticFormMatrix(type,m))*matrix(transpose({(1/1)*w}))) )_(0,0)
));
KillingForm(LieAlgebra,List,List) := memoize((g, v,w) ->   (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";	  
    (matrix({(1/1)*v})*(quadraticFormMatrix(type,m))*matrix(transpose({(1/1)*w})))_(0,0)
));

 
    
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
	answer:=delete(null, apply(#pl, i -> if KillingForm(type, m, pl_i, Theta) <= l then pl_i));
        return sort answer
    )
));  

weylAlcove(ZZ,LieAlgebra) := memoize( (l,g)-> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    weylAlcove(type,m,l) 
))
    



--For definitions and formulas of Casimir scalars, see (13.127), [DMS] p. 512
--For the definition and formula for rho, see: (13.46), [DMS] p. 499
    
casimirScalar = method(
    TypicalValue => QQ
    )     
casimirScalar(String,ZZ,List) := memoize((type, m, w) -> (
    rho:=apply(m,h->1/1);
    KillingForm(type,m,w,w) + 2*KillingForm(type,m,w,rho)
));
casimirScalar(LieAlgebraModule) := memoize((M) -> (
    if not isIrreducible M then error "Casimir scalar on irreducible modules only";
    g:=M#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    v:=first keys M#"DecompositionIntoIrreducibles";
    casimirScalar(type,m,v)	  
));
  
simpleRoots = (type,m) -> (
    C:=cartanMatrixQQ(type,m);     
    entries lift(C,ZZ)    
)


positiveRoots = method(
    TypicalValue => List
)     

--In Freudenthal's formula, we need to sum over the positive roots
positiveRoots(String,ZZ):= memoize((type,m) -> (
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
))

positiveRoots(LieAlgebra):=(g) -> (
    positiveRoots(g#"RootSystemType",g#"LieAlgebraRank")  
);

--In the next four functions we implement Freudenthal's recursive algorithm for computing the weights in a Lie algebra module and their multiplicities
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


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- Exported functions for Lie algebra modules 
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

multiplicityOfWeightInLieAlgebraModule = memoize((type,m,v,w) -> (
    rho:=apply(m, i -> 1);
    if v==w then return 1;
    Omega:=Freud(type,m,v);
    if not member(w,Omega) then return 0;
--    L:=weightsAboveMu(type,m,v,w);
    posroots:=positiveRoots(type,m);
    rhs:=0;
    k:=0;
    for a from 0 to #posroots-1 do (
        k=1;
        while member(w+k*(posroots_a),Omega) do (
	    rhs=rhs+KillingForm(type,m,w+k*(posroots_a),posroots_a)*multiplicityOfWeightInLieAlgebraModule(type,m,v,w+k*(posroots_a));
	    k=k+1;
	    ));
    lhs:=KillingForm(type,m,v+rho,v+rho)-KillingForm(type,m,w+rho,w+rho);
    lift(2*rhs/lhs,ZZ)
))




multiplicity(List,LieAlgebraModule) := o -> (w,M) -> (
    W:=weightDiagram(M);
    W_w 
)
multiplicity(Vector,LieAlgebraModule) := o -> (w,M) -> multiplicity(entries w,M)

character = method(
    Options=>{Strategy=>null},
    TypicalValue => RingElement
    )

stdVars = method()
stdVars(LieAlgebra) := (cacheValue stdVars) ( g -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    R := ring g;
    if type == "A" then apply(m+1, i -> (if i==m then 1 else R_i) * (if i==0 then 1 else R_(i-1)^-1))
    else if type=="B" then apply(m, i -> (if i==m-1 then R_i^2 else R_i) * (if i==0 then 1 else R_(i-1)^-1))
    else if type == "C" then apply(m, i -> R_i * (if i==0 then 1 else R_(i-1)^-1))
    else if type == "D" then apply(m-2, i -> R_i*(if i==0 then 1 else R_(i-1)^-1)) | {R_(m-2)*R_(m-1)*R_(m-3)^-1,R_(m-1)*R_(m-2)^-1}
    ))

characterAlgorithms := new MutableHashTable;
-- Jacobi Trudi formulae
elemSym = memoize((L,i) -> (
    if i<0 or i>#L then 0
    else sum(subsets(L,i),product)
    ))
characterAlgorithms#"JacobiTrudi'" = (g,v) -> ( -- good for high rank algebras, small weights
    type:=g#"RootSystemType";
    if type != "A" then return;
    z := stdVars g;
    m:=g#"LieAlgebraRank";
    conj:=reverse splice apply(m,i -> v#i : i+1);
    if #conj == 0 then 1_(ring g) else det matrix table(#conj,#conj,(i,j)->elemSym(z,conj#i+j-i))
    )
completeSym = memoize((L,i) -> (
    if i<0 then 0
    else sum(compositions(#L,i),c->product(L,c,(v,k)->v^k))
    ))
characterAlgorithms#"JacobiTrudi" = (g,v) -> ( -- good for high rank algebras, high weights
    type:=g#"RootSystemType";
    if type != "A" then return;
    z := stdVars g;
    m:=g#"LieAlgebraRank";
    pows := apply(m+1,j->sum(j..m-1,k->v#k));
    det matrix table(m+1,m+1,(i,j)->completeSym(z,pows#i+j-i))
    )

-- Weyl character formula
characterAlgorithms#"Weyl" = (g,v) -> ( -- good for low rank algebras
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    z := stdVars g;
    if type == "A" then (
	pows := apply(m+1,j->sum(j..m-1,k->1+v#k));
    	num := det matrix table(m+1,m+1,(i,j)->z_i^(pows#j));
    	den := product(m+1,j->product(j,i->z_i-z_j)); --  type A Weyl denominator formula
	)
    else if type=="B" then (
	pows = apply(m,j->sum(j..m-2,k->1+v#k)+(1+v#(m-1))//2);
	par := (1+v#(m-1)) % 2; -- shift of 1/2 to avoid half-integer powers
	num = (ring g)_(m-1)^(1-par)*det matrix table(m,m,(i,j)->z_i^(pows#j+par)-z_i^(-pows#j));
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
	num = (ring g)_(m-1)^(-par)*(num1+num2)//2;
	)
    else return;
    num//den -- would factoring be faster?
)

characterAlgorithms#"Freudenthal" = (g,v) -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    R:= ring g;
    sum(toList Freud(type,m,v), w -> multiplicityOfWeightInLieAlgebraModule(type,m,v,w) * R_w)
    )

-- last strategy = first choice
scan({"Freudenthal","Weyl","JacobiTrudi","JacobiTrudi'"}, strat -> addHook((character,LieAlgebraModule),characterAlgorithms#strat,Strategy=>strat))
character (LieAlgebra,List) := o -> (g,v) ->  if g.cache#?(character,v) then g.cache#(character,v) else g.cache#(character,v) = runHooks((character,LieAlgebraModule),(g,v),o)
character (LieAlgebra,Vector) := o -> (g,v) -> character(g,entries v,o)
character LieAlgebraModule := o -> (M) -> sum(pairs M#"DecompositionIntoIrreducibles",(v,a) -> a * character (M#"LieAlgebra",v,o))

weightDiagram = method(
    Options=>{Strategy=>null},
    TypicalValue=>Tally
    )

weightDiagram LieAlgebraModule := o -> (M) -> new Tally from listForm character(M,o)
weightDiagram(LieAlgebra,Vector) := weightDiagram(LieAlgebra,List) := o -> (g,v) -> new Tally from listForm character(g,v,o)

dim LieAlgebraModule := M -> sum values weightDiagram M

findOneHighestWeight = (type,m,W) -> (
    DeltaPlus :=positiveRoots(type,m);
    K:=keys W;
    v:={};
    for i from 0 to #K-1 do (
        v=K_i;
        if apply(DeltaPlus, w-> member(w+v,set(K))) == apply(#DeltaPlus, j -> false) then return v   
    );
    return null;
)



LieAlgebraModuleFromWeights = method(
    TypicalValue => LieAlgebraModule
    )
LieAlgebraModuleFromWeights(Tally,LieAlgebra) := (W,g) -> (
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    --find and peel off irreducibles
    decompositionData := while (v:=findOneHighestWeight (type,m,W)) =!= null list (v,mu:=W#v) do (
        WDv:=weightDiagram (g,v); assert(WDv#v === 1);
	W=W-applyValues(WDv,i->i*mu);
    );
    new LieAlgebraModule from {"LieAlgebra"=>g,"DecompositionIntoIrreducibles"=>new Tally from decompositionData}
)     

---------------------------------------------------------
---------------------------------------------------------
--Tensor product decomposition
---------------------------------------------------------
--------------------------------------------------------- 

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
            l0:=lift(l-KillingForm(type,m,w,theta),ZZ);
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

LieAlgebraModule ** LieAlgebraModule := (V,W) -> ( -- cf Humpheys' intro to LA & RT sec 24 exercise 9
    g:=V#"LieAlgebra";
    if g != W#"LieAlgebra" then error "V and W must be modules over the same Lie algebra";
    wd:=weightDiagram V;
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    sr:=simpleRoots(type,m);
    rho:=toList(m:1);
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
    new LieAlgebraModule from {"LieAlgebra"=>g,"DecompositionIntoIrreducibles"=>new Tally from ans}
    )

tensorCoefficient = method(
    TypicalValue=>ZZ)
tensorCoefficient(LieAlgebraModule, LieAlgebraModule,LieAlgebraModule) := memoize((U,V,W) -> (
	if not isIrreducible W then error "third module must be irreducible";
    	nu:=first keys W#"DecompositionIntoIrreducibles";
    	fullTensorProduct:=(U**V)#"DecompositionIntoIrreducibles";
    	fullTensorProduct_nu
    ))


---------------------------------------------------------
---------------------------------------------------------
--Fusion product decomposition
---------------------------------------------------------
--------------------------------------------------------- 

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

fusionProduct = method(
    TypicalValue=>HashTable,Options=>{MaxWordLength=>10})

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
    return new LieAlgebraModule from {"LieAlgebra"=>simpleLieAlgebra(type,m),"DecompositionIntoIrreducibles"=>new Tally from newwdh,"isIrreducible"=>false};
))


fusionCoefficient=method(
    TypicalValue=>ZZ,Options=>{MaxWordLength=>10})
fusionCoefficient(LieAlgebraModule,LieAlgebraModule,LieAlgebraModule,ZZ) := memoize(opts -> (U,V,W,l) -> (
    wl:=opts.MaxWordLength;	  
    g:=U#"LieAlgebra";
    type:=g#"RootSystemType";
    m:=g#"LieAlgebraRank";
    fullFusionProduct:=(fusionProduct(U,V,l,MaxWordLength=>wl))#"DecompositionIntoIrreducibles";
    if fullFusionProduct#?(first keys W#"DecompositionIntoIrreducibles") then return lift(fullFusionProduct#(first keys W#"DecompositionIntoIrreducibles"),ZZ) else return 0
))

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
    	    This class represents Lie algebras.  Currently only simple Lie algebras over the complex numbers are supported.  An object of type LieAlgebra is a hash table whose keys record whether the Lie algebra is simple, the rank of the Lie algebra, and the type of the root system.
	     
        Example
	    g=simpleLieAlgebra("A",1)
	    g=simpleLieAlgebra("E",6)                    
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
            The classification of simple Lie algebras over the complex numbers is well known.  There are four infinite families (types A, B, C, D) corresponding to the Lie algebras $sl(n+1,\mathbb{C})$, $so(2n+1,\mathbb{C})$, $sp(2n,\mathbb{C})$, $so(2n,\mathbb{C})$ respectively, and five exceptional simple Lie algebras, E6, E7, E8, F4, G2.  
	    	   
        Example
            --simpleLieAlgebra(sl_2)
	    simpleLieAlgebra("A",1)
	    --simpleLieAlgebra(sp_10)
	    simpleLieAlgebra("E",6)
///	 	 

TEST ///
    assert(A=simpleLieAlgebra("A",1); A#"LieAlgebraRank"===1 and A#"RootSystemType"==="A" and A#"isSimple")
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
	(highestRoot,String,ZZ)
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
	(positiveRoots,String,ZZ)
	(positiveRoots,LieAlgebra)
    Headline
        returns the positive roots of a simple Lie algebra
    Usage
        positiveRoots(g), positiveRoots("A",2)
    Inputs
        g:LieAlgebra
    Outputs
        t:List
    Description
        Text  
            Let R be an irreducible root system of rank m, and choose a base of simple roots $\Delta = \{\alpha_1,...,\alpha_m\}$.  This function returns all the roots that are nonnegative linear combinations of the simple roots.    The formulas implemented here are taken from the tables following Bourbaki's {\it Lie Groups and Lie Algebras} Chapter 6.
	    
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
        starInvolution
	(starInvolution,List,LieAlgebra)
	(starInvolution,Vector,LieAlgebra)
	(starInvolution,String,ZZ,List)
	(starInvolution,String,ZZ,Vector)
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
	     starInvolution({1,0},g)
///

TEST ///
    g=simpleLieAlgebra("A",2)
    assert(starInvolution({1,0},g) === {0,1})
///



doc ///
    Key
        KillingForm
	(KillingForm,LieAlgebra,List,List)
	(KillingForm,String,ZZ,List,List)
    Headline 
        computes the scaled Killing form applied to two weights
    Usage 
        KillingForm(g,v,w)
    Inputs 
        g:LieAlgebra
	v:List
	w:List
    Description
        Text
	    Let $\mathbf{g}$ be a Lie algebra.  The Killing form on $\mathbf{g}$ is the symmetric bilinear form given by $(x,y) = Tr(ad x ad y)$.  It can restricted to a Cartan subalgebra $\mathbf{h}$ and transferred to $\mathbf{h}^*$, yielding a symmetric bilinear form on weights.  One popular convention is to scale the Killing form so that $(\theta,\theta) =2$, where $\theta$ is the highest root.
	    
        Example
            g=simpleLieAlgebra("A",2)
	    KillingForm(g,{1,0},{0,1})
///

TEST ///
    g=simpleLieAlgebra("A",2)
    assert(KillingForm(g,{1,0},{0,1}) === 1/3)
///	
	
doc ///
    Key
        weylAlcove
	(weylAlcove,String,ZZ,ZZ)
	(weylAlcove,ZZ,LieAlgebra)
    Headline 
        the dominant integral weights of level less than or equal to l
    Usage 
        weylAlcove(l,g)
    Inputs 
        l:ZZ
        g:LieAlgebra
    Description
        Text
            Let $\mathbf{g}$ be a Lie algebra, and let $l$ be a nonnegative integer.  Choose a Cartan subalgebra $\mathbf{h}$ and a base $\Delta= \{ \alpha_1,\ldots,\alpha_n\}$ of simple roots of $\mathbf{g}$.  These choices determine a highest root $\theta$. (See @TO highestRoot@).   Let $\mathbf{h}_{\mathbf{R}}^*$ be the real span of $\Delta$, and let $(,)$ denote the Killing form, normalized so that $(\theta,\theta)=2$.  The fundamental Weyl chamber is $C^{+} = \{ \lambda \in \mathbf{h}_{\mathbf{R}}^*  : $(\lambda,\alpha_i)$ >= 0, i=1,\ldots,n \}$.  The fundamental Weyl alcove is the subset of the fundamental Weyl chamber such that $(\lambda,\theta) \leq l$.  This function computes the set of integral weights in the fundamental Weyl alcove.  
	    
        Text
            In the example below, we see that the Weyl alcove of $sl_3$ at level 3 contains 10 integral weights.
	    
	Example 
	    g=simpleLieAlgebra("A",2)
	    weylAlcove(3,g)
///

TEST ///
    g=simpleLieAlgebra("A",2)
    assert(set(weylAlcove(3,g)) ===set {{0, 0}, {1, 0}, {0, 1}, {1, 1}, {2, 0}, {2, 1}, {0, 2}, {1, 2}, {3, 0}, {0, 3}}) 
///	
	

doc ///
    Key
        LieAlgebraModule
    Headline
        class for Lie algebra modules
    Description
        Text 
    	    This class represents Lie algebra modules.  Currently only modules over simple Lie algebras over the complex numbers are supported.  An object of type LieAlgebraModule is a hash table recording the Lie algebra and the decomposition of the module into irreducible Lie algebra modules, which are indexed by their highest weights. 
	    
	Example
	    g=simpleLieAlgebra("A",2)
	    M=irreducibleLieAlgebraModule({1,1},g)                   
///

doc ///
    Key
        irreducibleLieAlgebraModule
	(irreducibleLieAlgebraModule,List,LieAlgebra)
	(irreducibleLieAlgebraModule,Vector,LieAlgebra)
	LL
    Headline
        construct the irreducible Lie algebra module with given highest weight
    Usage
        irreducibleLieAlgebraModule(w,g)
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
            irreducibleLieAlgebraModule({1,1},g)
        Text
	    One can also use the shorthand LL:
	Example
            LL_(1,1) (g)
///

TEST ///
    assert(irreducibleLieAlgebraModule({1,1},simpleLieAlgebra("A",2)) === new LieAlgebraModule from {"LieAlgebra"=>simpleLieAlgebra("A",2),"DecompositionIntoIrreducibles"=>new Tally from {{1,1}=>1} })
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
        T:Tally
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
    assert(weightDiagram(irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2))) === new Tally from {{{-1, 1}, 2}, {{1, 0}, 2}, {{3, -1}, 1}, {{-2, 0}, 1}, {{0, -1}, 2}, {{2, -2}, 1}, {{-2, 3}, 1}, {{0, 2}, 1}, {{2, 1}, 1}, {{-1, -2}, 1}, {{1, -3}, 1}, {{-3, 2}, 1}})
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
    assert(irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2)) ** irreducibleLieAlgebraModule({1,2},simpleLieAlgebra("A",2)) === new LieAlgebraModule from {"LieAlgebra"=>simpleLieAlgebra("A",2),"DecompositionIntoIrreducibles"=>new Tally from {{{1, 1}, 2}, {{3, 0}, 1}, {{1, 4}, 1}, {{3, 3}, 1}, {{0, 0}, 1}, {{0, 3}, 1}, {{2, 2}, 2}, {{4, 1}, 1}} })
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
    assert(irreducibleLieAlgebraModule({2,1},simpleLieAlgebra("A",2)) ** irreducibleLieAlgebraModule({1,2},simpleLieAlgebra("A",2)) === new LieAlgebraModule from {"LieAlgebra"=>simpleLieAlgebra("A",2),"DecompositionIntoIrreducibles"=>new Tally from {{{1, 1}, 2}, {{3, 0}, 1}, {{1, 4}, 1}, {{3, 3}, 1}, {{0, 0}, 1}, {{0, 3}, 1}, {{2, 2}, 2}, {{4, 1}, 1}} })
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
           The Weyl group of a simple Lie algebra is finite; in contrast, the affine Weyl group of an affine Lie algebra is infinite.  To keep Macaulay2 from trying to compute infinitely long words in this group, the default length of allowed words is set to 10.   The user may override this with the optional argument @TO "MaxWordLength"@. 
       
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
       (LieAlgebraModuleFromWeights,Tally,LieAlgebra)
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
            The Weyl group of a simple Lie algebra is finite; in contrast, the affine Weyl group of an affine Lie algebra is infinite.  To keep Macaulay2 from trying to compute infinitely long words in this group, the default length of allowed words is set to 10.   The user may override this with the optional argument @TO "MaxWordLength"@. 
	    
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
	(casimirScalar,String,ZZ,List)
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

TEST ///
    g=simpleLieAlgebra("A",2);
    M=irreducibleLieAlgebraModule({2,1},g)
    assert(M ** trivialModule g == M)
///
	    
endPackage "LieTypes" 
