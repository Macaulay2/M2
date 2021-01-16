-- -*- coding: utf-8-unix -*-
-- Code for Simplicial Complexes

--------------------------------------------------------------------------------
-- Copyright 2006, 2010  Sorin Popescu, Gregory G. Smith, and Mike Stillman
-- 
-- You may redistribute this program under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2 of the
-- License, or any later version.
--------------------------------------------------------------------------------

-- Janko added class Face to be used in other packages
-- should not affect the functionality present previously

newPackage(
	"SimplicialComplexes",
    	Version => "1.2", 
    	Date => "March 4, 2011",
    	Authors => {
	     {Name => "Sorin Popescu", Email => "sorin@math.sunysb.edu", HomePage => "http://www.math.sunysb.edu/~sorin/"},
	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca", HomePage => "http://www.mast.queensu.ca/~ggsmith"},
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"}
	     },
    	Headline => "simplicial complexes",
	Keywords => {"Combinatorial Commutative Algebra"},
    	DebuggingMode => false,
    	PackageExports => {"GenericInitialIdeal"}
    	)

export {"SimplicialComplex",
     "simplicialComplex",
     "boundary","fVector","isPure","label",
     "faces","facets","link",
     "simplicialChainComplex",
     "buchbergerComplex",
     "lyubeznikComplex",
     "superficialComplex",
     "faceIdeal",
     "Face",
     "vertices",
     "face",
     "useFaceClass",
     "isSubface",
     "isFaceOf",
     "skeleton",
     "Flag",
     "algebraicShifting",
     "Multigrading",
     "star",
     "joinSimplicial"}

complement := local complement
complement = (m) -> (
     A := ring m;
     F := matrix{{product gens A}};
     contract(m,F))

debug Core; -- for rawKoszulMonomials

SimplicialComplex = new Type of HashTable

SimplicialComplex.synonym = "simplicial complex"

---- the output line should describe the class of the output [drg]
-- SimplicialComplex#{Standard,AfterPrint} = Delta -> (
--      << endl;
--      << concatenate(interpreterDepth:"o") << lineNumber << " : Facets of the simplicial complex "
--      << endl;)

net SimplicialComplex := Delta -> net Delta.facets

simplicialComplex = method(TypicalValue => SimplicialComplex)

newSimplicialComplex := (I,F) ->
     new SimplicialComplex from {
	  symbol ring => ring I,
	  symbol faceIdeal => I,
	  symbol facets => F,
	  symbol cache => new CacheTable
	  }
     
simplicialComplex MonomialIdeal := (I) -> (
     R := ring I;
     if not isPolynomialRing R or isQuotientRing R
     then error "expected a polynomial ring";
     if not isSquareFree I then
         error "expected squarefree monomial ideal";
     newSimplicialComplex(I, complement generators dual I)
     )



---------------------------------------------------------
-- frame procedure of simplicialComplex now renamed (unchanged)
-- to simplicialComplexM to allow as input also a list of faces
-- added by Janko 

simplicialComplex List := SimplicialComplex => (facets) -> (
     if #facets === 0 then error "expected at least one facet";
     if class facets#0 === Face then (
        return simplicialComplexM(apply(facets,j->product vertices j));
     ) else (
        return simplicialComplexM(facets))
)
-------------------------------------------------------------



simplicialComplexM=method()
simplicialComplexM List := SimplicialComplex => (faces) -> (
     if #faces === 0 then error "expected at least one facet";
     R := class faces#0;
     if not isPolynomialRing R or not all(faces, m -> class m === R) then error "expected elements of a polynomial ring";
     I := matrix {faces};
     L := monomialIdeal complement I;
     J := dual L;
     newSimplicialComplex(J, complement generators L)
     )

dual SimplicialComplex := {} >> o -> (D) -> (
     newSimplicialComplex(monomialIdeal complement D.facets,
	  complement generators D.faceIdeal)
     )

ring SimplicialComplex := (D) -> D.ring
coefficientRing SimplicialComplex := (D) -> coefficientRing D.ring


------------------------------------------------------
-- added option to return a list of Faces
-- added by Janko

facets = method(Options=>{useFaceClass=>false})
facets SimplicialComplex := opt->(D) -> (
if opt.useFaceClass then return(matrixToFaces(D.facets));
D.facets)

-----------------------------------------------------


ideal SimplicialComplex := (D) -> ideal D.faceIdeal
monomialIdeal SimplicialComplex := (D) -> D.faceIdeal

link = method()
link(SimplicialComplex, RingElement) := (D,f) -> (
     simplicialComplex monomialIdeal((ideal support f) + ((ideal D) : f)))

SimplicialComplex == SimplicialComplex := (D,E) -> D.faceIdeal === E.faceIdeal

lcmMonomials = (L) -> (
     R := ring L#0;
     x := max \ transpose apply(L, i -> first exponents i);
     R_x)

lcmM = (L) -> (
-- lcmM finds the lcm of a list of monomials; the quickest method Sorin knows
    m := intersect \\ monomialIdeal \ L;
    m_0)


dim SimplicialComplex := (D) -> max apply(first entries D.facets, s -> # support(s)) - 1


---------------------------------------
-- frame procedure for original "faces" now renamed
-- (unchanged) to facesM, which has the option
-- to return also a list of Faces
-- added by Janko

faces = method(Options=>{useFaceClass=>false})
faces(ZZ,SimplicialComplex) := opt->(r,D) -> (
if opt.useFaceClass then (
   matrixToFaces(facesM(r,D))
) else (
   facesM(r,D))
)

-- list of list of all faces
faces SimplicialComplex := opt->(C)->(
j:=0;
f := j -> faces(j,C,opt);
new MutableHashTable from toList apply(-1..dim C,j->j=>f j))


-------------------------------------------------
-- convert Matrix with Monomials to List of Faces
-- added by Janko

matrixToFaces=method()
matrixToFaces(Matrix):=M->(
if M==0 then return({});
apply((entries M)#0,face))

-------------------------------------------------

facesM=method()
facesM (ZZ, SimplicialComplex) := (r,D) -> (
     R := ring D;
     if not D.cache.?faces then (
         D.cache.faces = new MutableHashTable;
	 B := (coefficientRing R) (monoid [gens R, SkewCommutative=>true]);
	 D.cache.faces.ideal = substitute(D.faceIdeal,B);
	 );
     if r < -1 or r > dim D then matrix(R, {{}})
     else (
	  if not D.cache.faces#?r then (
               J := D.cache.faces.ideal;
               D.cache.faces#r = substitute(matrix basis(r+1,coker gens J), vars R));
     	  D.cache.faces#r
     ))

protect labels
protect ones
boundary = method()
boundary (ZZ,SimplicialComplex) := (r,D) -> (
     R := ring D;
     if D.cache.?labels then (
	  m1 := D.cache.labels#r;
	  m2 := D.cache.labels#(r-1);
	  Sext := D.cache.labels.ring;
     	  ones := D.cache.labels.ones;
	  S := target ones;
	  F := source map(S^1,, ones m2);
	  bd := ones map(Sext, rawKoszulMonomials(numgens Sext, raw m2,raw m1));
	  bd = map(F,,bd);
	  bd
	  )
     else (
     	  b1 := faces(r,D);
     	  b2 := faces(r-1,D);
     	  ones = map(coefficientRing R,R, toList(numgens R:1));
     	  ones map(R, rawKoszulMonomials(numgens R,raw b2,raw b1))
     	  )
     )

chainComplex SimplicialComplex := (D) -> (
     d := dim D;
     C := if d < -1 then (ring D)^0[-1]
          else if d === -1 then (ring D)^1
          else chainComplex apply(0..d, r -> boundary(r,D));
     if D.cache.?labels then C[0] else C[1]
     )

-------- Labelled code ---------------------
makeLabels = (D,L,i) -> (
     -- D is a simplicial complex
     -- L is a list of monomials 
     -- i is an integer
     F := first entries faces(i,D);
     Sext := D.cache.labels.ring;
     if #F == 0 
     then matrix{{1_Sext}} 
     else
          matrix {apply(F, m -> (
			 s := rawIndices raw m;
	       		 lcmM L_s
			 ))}
     )

L=null						    -- used as a key below

label = method()
label(SimplicialComplex, List) := (D,L) -> (
     if #L === 0 then
	  remove(D.cache,symbol labels)
     else (
	  D.cache.labels = new MutableHashTable;
	  S := ring(L#0);
	  M := monoid [Variables=>#L]; 
	  Sext := S M;
	  D.cache.labels.ring = Sext;
     	  L = apply(#L, i -> L_i * Sext_i);
	  D.cache.labels.L = L;
	  D.cache.labels.ones = map(S, Sext, toList(#L:1_S));
	  D.cache.labels#-1 = matrix{{1_Sext}};
	  for i from 0 to dim D do
	       D.cache.labels#i = makeLabels(D,L,i);
	  )
     )

simplicialChainComplex = method()
simplicialChainComplex (List,SimplicialComplex) := (L, D) -> (
     label(D,L);
     d := dim D;
     C := chainComplex(apply(0..d, r -> boundary(r,L,D)));
     -- label(D,{}) -- removes cached labels
     C
     )

homology(ZZ,SimplicialComplex,Ring) := Module => opts -> (i,Delta,R) -> (
     homology(i, chainComplex Delta ** R))
homology(ZZ,SimplicialComplex) := Module => opts -> (i,Delta) -> (
     homology(i, chainComplex Delta))
homology(Nothing,SimplicialComplex,Ring) :=
homology(SimplicialComplex,Ring) := GradedModule => opts -> (Delta,R) -> (
     homology(chainComplex Delta ** R))
homology(Nothing,SimplicialComplex) :=
homology(SimplicialComplex) := GradedModule => opts -> Delta -> (
     homology(chainComplex Delta))

------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- 20/07/2018 Lorenzo: some changes

-- Fixed fVector to make it work also when the underlying ring is  multigraded.
-- Added the option Flag to return the finer f-vector for the multigraded case.

fVector = method(TypicalValue => List, Options => {Flag => false})
fVector SimplicialComplex := opts -> D -> (
     I := ideal D;
     if not opts.Flag then (
         S := newRing(ring D, Degrees => {#(gens ring D):1});
         maptoS := map(S, ring D);
         I = maptoS(I);
     );
     N := poincare cokernel generators I;
     if opts.Flag then (
     if not isBalanced(D) then (
         stderr << "-- the grading does not correspond to a proper d-coloring." << endl;
         return new HashTable from {}
     );
         R := newRing(ring N, Degrees => apply(gens ring N, g -> apply(gens ring N, f -> if index(f) == index(g) then 1 else 0)));
         maptoR := map(R, ring N);
         N = maptoR(N);
     );
     if N == 0 then (
         new HashTable from {-1 => 0}
     )
     else (
         d := dim D + 1;
         apply(gens ring N, t -> while 0 == substitute(N, t => 1) do N = N // (1-t));
         supp := apply(flatten entries monomials(N), m -> degree m);
         allsubsets := apply(subsets(#(gens ring N)), s -> apply(toList(0..#(gens ring N)-1), l -> if member(l,s) then 1 else 0));
         flagh := L -> coefficient((flatten entries monomials part(L, N))#0, part(L, N));
     flagf := M -> sum(supp, m -> if all(m,M, (i,j) -> j >= i) then flagh(m) else 0);
     h := j -> sum(supp, s -> if sum(s)==j then flagh(s) else 0);
     f := j -> sum(0..j+1, i -> binomial(d-i, d-j-1)*h(i));
     if opts.Flag then (
         new HashTable from apply(allsubsets, j -> j => flagf(j))
     )
     else new HashTable from prepend(-1=>1, apply(toList(0..d-1), j -> j => f(j)))
     )
     )

-- Check if the grading on the ring defines a proper (dim(D)+1)-coloring on D. Used by fVector. Not exported.

isBalanced = (D) -> (
     d := dim D +1;
     m := true;
     if not d == #(degree first gens ring D) then (
         m = false;
     );
     apply(flatten entries faces(1,D), f -> if max(degree f) > 1 then m = false);
     return m;
     );

-- helper functions for algebraicShifting. Not exported.
shiftMonomial = (m) -> (
    variables := flatten entries vars ring m;
    D := unique degrees ring m;
    P := apply(D, d -> flatten entries basis(d, ring m));
    f := (Q, v) -> {position(Q, q -> member(v, q)), position(Q_(position(Q, q -> member(v,q))), b -> b == v)};
    multisupp := MultiSupp(m);
    deg := degree(m);
    auxlist := flatten apply(deg, d -> toList(0..d-1));
    sm := 1;
    apply(auxlist, multisupp, (i, j) -> sm = sm * (P_((f(P, j))_0))_((f(P, j))_1+i) );
    return sm
    );

MultiSupp = (m) -> (
    multisupp := {};
    while m != 1 do (multisupp = append(multisupp, (support(m))_0);
        m=m//((support(m))_0););
    return multisupp
    );


shift = (I) -> (
    shiftgens := apply( I_*, g -> shiftMonomial(g));
    return ideal shiftgens
    );


-- Compute the algebraic shifting of a simplicial complex and the colored shifting if the ring is multigraded.
algebraicShifting = method (Options => {Multigrading => false})
algebraicShifting SimplicialComplex := opts -> S -> (
    if not opts.Multigrading then (
    R := newRing(ring S, Degrees => {#(gens ring S):1});
    f := map(R, ring S);
    g := map(ring S, R);
    J := g(shift(gin(f(ideal S), Multigraded => opts.Multigrading)));
    return simplicialComplex monomialIdeal J
    )
    else (
        sI := monomialIdeal shift(gin(ideal S, Multigraded => opts.Multigrading));
    return simplicialComplex sI
    )
    )
-- Compute the i-th skeleton of a simplicial complex
skeleton = method ()
skeleton (ZZ, SimplicialComplex) :=  (n, S) -> (
     simplicialComplex(flatten entries faces(n,S))
     )

-- Compute the star w.r.t. a face
star = method ()
star (SimplicialComplex, RingElement) := (S, f) -> (simplicialComplex(monomialIdeal(S):monomialIdeal(f)))

-- The simplicial join of two simplicial complexes defined over different rings
joinSimplicial = method ()
joinSimplicial (SimplicialComplex, SimplicialComplex) := (A, B) -> (
     T := tensor(ring A, ring B);
     f := map(T, ring A);
     g := map(T, ring B);
     return simplicialComplex(monomialIdeal(f(ideal(A))+g(ideal(B))));
     )

SimplicialComplex * SimplicialComplex := joinSimplicial
------------------------------------------------------------------------
------------------------------------------------------------------------


boundary SimplicialComplex := (D) -> (
     F := first entries facets D;
     L := flatten apply(F, m -> apply(support m, x -> m//x));
     if #L === 0 then 
         simplicialComplex monomialIdeal (1_(ring D))
     else
     	 simplicialComplex L
     )

isPure = method(TypicalValue => Boolean)
isPure SimplicialComplex := Boolean => (D) -> (
     F := first entries facets D;
     L := unique apply(F, m -> # support m);
     #L <= 1
     )


--------------------------------------------------------------------------
-- Buchberger complex of a monomial ideal (aka improved Taylor complex) --
-- (see Eisenbud-Hosten-Popescu)          
--------------------------------------------------------------------------

lcmMRed = method()
lcmMRed (List) := (L) -> (
-- lcmMRed finds the reduced lcm of a list of monomials
    m := intersect \\ monomialIdeal \ L;
    m_0//(product support m_0))

faceBuchberger = (m, L) -> (
-- true iff the monomial m (in #L vars) is in the Buchberger complex
     x := rawIndices raw m;
     mon := lcmMRed(L_x);
     all(L, n -> mon//n == 0))

buchbergerComplex = method()
buchbergerComplex(List,Ring) := (L,R) -> (
     P := ideal apply(gens R, x -> x^2);
     nonfaces := {};
     d := 1;
     while (L1 := flatten entries basis(d,coker gens P); #L1 > 0) do (
	  L1 = select(L1, m -> not faceBuchberger(m,L));
	  << "new nonfaces in degree " << d << ": " << L1 << endl;	  
	  nonfaces = join(nonfaces,L1);
	  if #nonfaces > 0 then
	      P = P + ideal nonfaces;
	  d = d+1;
          );
     simplicialComplex monomialIdeal matrix(R, {nonfaces})
     )

buchbergerComplex(MonomialIdeal) := (I) -> (
     buchbergerComplex(flatten entries gens I, ring I))

--------------------------------------------------------------------------
-- Lyubeznik/Superficial type resolutions                               --
-- (see Eisenbud-Hosten-Popescu)                                        --
--------------------------------------------------------------------------

isSuperficial = method()
isSuperficial List := (L) -> (
-- isSuperficial checks if a list of monomials is already superficially oredred
-- that is every monomial in the list does not strictly divide the lcm of the previous ones
     R := ring(L_0);
     all(1..#L-1, i-> (previous:=lcmMonomials(take(L,i)); 
	       (previous//product(support previous))//(L_i) == 0))
     )

faceLyubeznik = (m,L) -> (
-- true iff the monomial m (in #L vars) defines a face in the Lyubeznik complex
     x := rawIndices raw m;
     all(0..#L-1, i -> (L1:=L_(select(x, j->j>i));
	       if (#L1==0) then true else lcmMonomials(L1)//L_i == 0)))

lyubeznikComplex = method()
lyubeznikComplex(List,Ring) := (L,R) -> (
     if not isSuperficial(L) then error "expected a superficially ordered list of monomials";
     P := ideal apply(gens R, x -> x^2);
     nonfaces := {};
     d := 1;
     while (L1 := flatten entries basis(d,coker gens P); #L1 > 0) do (
	  L1 = select(L1, m -> not faceLyubeznik(m,L));
	  << "new nonfaces in degree " << d << ": " << L1 << endl;	  
	  nonfaces = join(nonfaces,L1);
	  if #nonfaces > 0 then
	      P = P + ideal nonfaces;
	  d = d+1;
          );
     simplicialComplex monomialIdeal matrix(R, {nonfaces})
     )

lyubeznikComplex(MonomialIdeal) := (I) -> (
     lyubeznikComplex(flatten entries gens I, ring I))


faceSuperficial = (m,L) -> (
-- true iff the monomial m (in #L vars) defines a face in the Superficial complex
     x := rawIndices raw m;
     R := ring(L_0);
     all(0..#L-1, n -> (smallerMons := L_(select(x, j->j<n+1)); 
	                largerMons := L_(select(x, j->j>n));
	                smallerLcmRed := if (#smallerMons==0) then 1_R else lcmMRed(smallerMons);
			lcmMonomials(join({smallerLcmRed}, largerMons))//L_n==0)))
   


superficialComplex = method()
superficialComplex(List, Ring) := (L,R) -> (
     if not isSuperficial(L) then error "expected a superficially ordered list of monomials";
     P := ideal apply(gens R, x -> x^2);
     nonfaces := {};
     d := 1;
     while (L1 := flatten entries basis(d,coker gens P); #L1 > 0) do (
	  L1 = select(L1, m -> not faceSuperficial(m,L));
	  << "new nonfaces in degree " << d << ": " << L1 << endl;	  
	  nonfaces = join(nonfaces,L1);
	  if #nonfaces > 0 then
	      P = P + ideal nonfaces;
	  d = d+1;
          );
     simplicialComplex monomialIdeal nonfaces
     )

superficialComplex(MonomialIdeal) := (I) -> (
     superficialComplex(flatten entries gens I, ring I))


-----------------------------------------------------------------------
-- Defining a class Face
-- to be used in other package in particular in the KustinMiller package
-- additions by Janko

Face = new Type of MutableHashTable

-- vertices of a face
vertices=method()
vertices Face := F -> F.vertices

-- pretty print
net Face := (f)-> (
v:=vertices(f);
if #v==0 then return(net({}));
horizontalJoin(apply(v,j->net(j)|net(" "))))

-- after print
Face#{Standard,AfterPrint} = m -> (
  n:=#vertices(m);
  if n==0 then vstr:="empty face";
  if n==1 then vstr="face with "|n|" vertex";
  if n>1 then vstr="face with "|n|" vertices";
      << endl;
      << concatenate(interpreterDepth:"o") << lineNumber << " : "
      << vstr|" in "|net(ring m)
      << endl;)

-- dimension
dim Face := F->-1+#(vertices F)

-- ring of a face
ring Face :=F->F.ring;

-- construct a face from a List of vertices
face=method()
face(List):= (L)-> new Face from {symbol vertices => L, symbol ring=> ring L#0}
face(List,PolynomialRing):= (L,R)-> new Face from {symbol vertices => L, symbol ring=>R}

-- construct a face from a monomial
face(RingElement):= (m)-> face(support m,ring m)

-- compare two faces
Face == Face := (F,G)->(
(#(vertices F))==(#(vertices G)) and set vertices F === set vertices G)

-- test if a face is a subface of another
isSubface=method()
isSubface(Face,Face):=(F,G)->(
isSubset(set vertices F,set vertices G))

-- test if a face is a face of a complex
isFaceOf=method()
isFaceOf(Face,SimplicialComplex):=(F,C)->(
fc:=facets(C,useFaceClass=>true);
#(select(1,fc,G->isSubface(F,G)))>0)


-- substitute a face to another ring
substitute(Face,PolynomialRing):=(F,R)->(
v:=vertices(F);
face(apply(v,j->sub(j,R)),R))

-- substitute a complex to another ring
substitute(SimplicialComplex,PolynomialRing):=(C,R)->(
simplicialComplex((entries sub(C.facets,R)))#0)


-*
installPackage "SimplicialComplexes"
R = QQ[a..e]
D = simplicialComplex monomialIdeal(a*b*c*d*e)
substitute(D,R)
F=(faces(1,D,useFaceClass=>true))#0
isFaceOf(F,D)
*-



beginDocumentation()

document { Key => SimplicialComplexes,
     Headline => "simplicial complexes",
     EM "SimplicialComplexes", " is a package for manipulating simplicial
     complexes.",
     PARA{},
     "A simplicial complex on a set of vertices 
     is a collection of subsets 
     ", TT "D", " of
     these vertices, such that if ", TT "F", " is in ", TT "D", ", 
     then every subset of ", TT "F", " is also in ", TT "D", ".
     In Macaulay2, the vertices are variables in a polynomial ring,
     and each subset is represented as a product of the
     corresponding variables.",
     PARA{},
     "There is a bijection between simplicial complexes and squarefree
     monomial ideals.  This package exploits this correspondence by
     using commutative algebra routines to perform most of the necessary
     computations.",
     PARA{},
     "This package includes the following functions:",
     UL {
      TO (symbol *, SimplicialComplex, SimplicialComplex),
      TO algebraicShifting,
	  TO boundary,
	  TO buchbergerComplex,
	  TO (chainComplex,SimplicialComplex),
	  TO (coefficientRing,SimplicialComplex),
	  TO (dim,SimplicialComplex),
	  TO (dual,SimplicialComplex),
	  TO faces,
	  TO facets,
	  TO fVector,
	  TO (homology,SimplicialComplex),
	  TO (ideal,SimplicialComplex),
	  TO isPure,
	  TO joinSimplicial,
	  TO label,
	  TO lyubeznikComplex,
	  TO (monomialIdeal,SimplicialComplex),
	  TO (ring,SimplicialComplex),
	  TO simplicialComplex,
	  TO simplicialChainComplex,
	  TO skeleton,
	  TO superficialComplex
	  }
--    (TO "chainComplex", "(D) -- the chain complex of D"),
--	  (TO "boundary", "(r,D) -- the boundary map from r faces to r-1 faces"),
--	  (TO "dim", "(D) -- the dimension of D"),
--	  (TO "boundary", "(D) -- the boundary simplicial complex of D"),
--	  (TO "dual", "(D) -- the dual simplicial complex"),
--	  (TO "faces", "(r,D) -- a matrix of squarefree monomials corresponding to 
--	       the faces of dimension r of D"),
--	  (TO "facets", "(D) -- return the matrix of maximal faces"),
--	  (TO "ideal", "(D) -- return the ideal of minimal non-faces")
--	  }
     }
     
document {  Key => SimplicialComplex,
     TT "SimplicialComplex", " -- the class of simplical complexes",
     PARA{},
     "Some support routines",
     UL {
	  (TO "support", "(m) -- the support of the monomial m, as a list of integers")
	  }
     }
document {
     Key => {(dim, SimplicialComplex)},
     Headline => "dimension of a simplicial complex",
     Usage => "dim D",
     Inputs => {"D"
	  },
     Outputs => {ZZ => "the maximum number of vertices in a face minus one"
	  },
     "The following simplicial complex consists of a tetrahedron,
     with two triangles attached, two more edges and an isolated
     vertex.  Since the largest facet has 4 vertices, this 
     complex has dimension 3.",
     EXAMPLE {
          "R = ZZ[a..h];",
	  "D = simplicialComplex{a*b*c*d, a*b*e, c*d*f, f*g, g*a, h}",
	  "dim D"
	  },
     "The void complex has dimension minus infinity, while the
     irrelevant complex has dimension -1.",
     EXAMPLE {
	  "void = simplicialComplex monomialIdeal 1_R;",
	  "dim void",
	  "irrelevant = simplicialComplex {1_R};",
	  "dim irrelevant"
	  },
     SeeAlso => {SimplicialComplexes,
	  isPure}
     }

document { 
     Key => {simplicialComplex,(simplicialComplex,MonomialIdeal),
	  (simplicialComplex,List)},
     Headline => "create a simplicial complex",
     Usage => "simplicialComplex I\nsimplicialComplex L",
     Inputs => {
	  "I" => MonomialIdeal => "the ideal of minimal nonfaces (Stanley-Reisner ideal)",
	  "L" => List => "a list of monomials representing the facets"
          },
     Outputs => {
	  SimplicialComplex => {"the simplicial complex determined by the nonfaces ", TT "I", " 
	  or generated by the given faces ", TT "L"}
          },
     "A simplicial complex on a set of vertices 
     is a collection of subsets 
     ", TT "D", " of
     these vertices, such that if ", TT "F", " is in ", TT "D", ", 
     then every subset of ", TT "F", " is also in ", TT "D", ".
     In Macaulay2, the vertices are variables in a polynomial ring,
     and each subset is represented as a product of the
     corresponding variables.",
     PARA{},
     "A simplicial complex is determined either by its nonfaces or by its faces.
     The monomials corresponding to the nonfaces are a basis of an ideal,
     called the Stanley-Reisner ideal, and
     it suffices to specify the minimal nonfaces, which generate the ideal.
     The monomials corresponding to the faces do not form the basis of an ideal,
     but it suffices to specify the maximal faces, which are called 
     ", EM "facets", ".  The function ", TO "simplicialComplex", " accepts either
     the ideal of nonfaces or the list of facets as input.",
     PARA{},
     "In our first example we construct the octahedron by specifying its
     ideal of nonfaces.",
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "I = monomialIdeal(a*f, b*d, c*e);",
	  "Octahedron = simplicialComplex I"
          },
     "Note that ", ofClass SimplicialComplex, " is displayed by showing its
     facets.  We see that there are eight facets to the octahedron.  Alternatively,
     we could have defined the octahedron by this list of facets.",
     EXAMPLE {
	  "L = {d*e*f, b*e*f, c*d*f, b*c*f, 
	       a*d*e, a*b*e, a*c*d, a*b*c}",
	  "Octahedron' = simplicialComplex L",
	  "Octahedron == Octahedron'",
	  "fVector Octahedron"
	  },
     "There are two \"trivial\" simplicial complexes: the void 
     complex and the irrelevant complex.  The void complex
     has no faces.  This complex cannot be constructed from
     its facets, since it has none.",
     EXAMPLE {
	  "void = simplicialComplex monomialIdeal 1_R",
	  "fVector void",
	  "dim void"
	  },
     "The irrelevant complex, which should be distinguished 
     from the void complex, has a unique face of dimension -1, 
     the empty set.",
     EXAMPLE {
	  "irrelevant = simplicialComplex monomialIdeal gens R",
	  "fVector irrelevant",
	  "dim irrelevant",
	  "irrelevant' = simplicialComplex {1_R}",
	  "irrelevant' == irrelevant"
	  },
     "As in Miller-Sturmfels, Combinatorial Commutative
     Algebra, we would avoid making such a big deal about 
     the difference between these complexes if it did not
     come up so much.  Many formulas for betti numbers, 
     dimensions of local cohomology, etc., depend on this
     distinction.",
     Caveat => UL {
	   {"The ring of ", TT "I", " or ", TT "L", 
		" must be a polynomial ring."},
	   {"The ", TO2(coefficientRing, "coefficient ring"),
	   " matters: for instance,
	   it is used when 
	   computing the corresponding chain complex."},
	   "This construction routine does some computation, as it
	   computes both the facets and the ideal of non-faces."
	   },
     SeeAlso => {SimplicialComplexes, fVector}
     }

document { 
     Key => boundary,
     Headline => "boundary operator",
     SeeAlso => {SimplicialComplexes}
     }
document { 
     Key => (boundary,ZZ,SimplicialComplex),
     Headline => "the boundary map from i-faces to (i-1)-faces",
     Usage => "M = boundary(i,D)",
     Inputs => {
	  "i",
	  "D"
          },
     Outputs => {
	  "M" => Matrix => {"the boundary map from ", TT "i", 
	       "-faces to ", TT "(i-1)", "-faces of ", TT "D"}
          },
     "The columns of the matrix ", TT "M", " are indexed by the ", TT "i", "-faces of
     ", TT "D", ", and the rows are indexed by the ", TT "(i-1)", "-faces, in the order
     given by ", TO faces, ".  ", TT "M", " is defined over the ", 
     TO2((coefficientRing,SimplicialComplex),"coefficient ring"), " of ", TT "D", ".",
     "The boundary maps for the standard 3-simplex, defined over ", TT "ZZ", ".",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "D = simplicialComplex {a*b*c*d}",
	  "boundary(0,D)",
	  "faces(0,D)",
          "boundary(1,D)",
	  "faces(1,D)",
	  "boundary(2,D)",
	  "faces(2,D)",
	  "boundary(3,D)",
	  "faces(3,D)",
	  "boundary(4,D)"
	  },
     "The boundary maps depend on the ",
     TO2((coefficientRing,SimplicialComplex),"coefficient ring"), 
     " as the following examples illustrate.",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f);",
	  "boundary(1,D)",
	  "R' = ZZ/2[a..f];",
	  "D' = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f);",
	  "boundary(1,D')"
	  },
     SeeAlso => {SimplicialComplexes, (chainComplex,SimplicialComplex), faces}
     }
document { 
     Key => (boundary,SimplicialComplex),
     Headline => "the boundary simplicial complex of D",
     Usage => "boundary D",
     Inputs => {
	  "D"
          },
     Outputs => {
	  SimplicialComplex => {"the boundary simplicial complex, 
	   i.e. the subcomplex of ", TT "D", 
	   " consisting of all nonmaximal faces of ", TT "D"},
          },
     "The boundary of the standard 3-simplex is the 2-sphere.",
     EXAMPLE {
          "R = ZZ[a..d];",
          "simplex = simplicialComplex{a*b*c*d}",
	  "sphere = boundary simplex",
	  "fVector sphere",
	  "fVector simplex"  
	  },
     "Facets may be of different dimensions.",
     EXAMPLE {
          "R = ZZ[a..g];",
          "D = simplicialComplex{a*b*c,a*d,d*f,g*c,e,f*g}",
	  "E = boundary D",
	  "fVector D",
	  "fVector E"
	  },
     SeeAlso => {SimplicialComplexes, fVector, isPure, facets}
     }


document { 
     Key => {buchbergerComplex, (buchbergerComplex,List,Ring), (buchbergerComplex,MonomialIdeal)},
     Headline => "Buchberger complex of a monomial ideal",
     Usage => "buchbergerComplex(L,R)\nbuchbergerComplex I",
     -- Inputs => {
     --      },
     -- Outputs => {
     --      },
     -- Consequences => {
     --      },     
     -- "description",
     -- EXAMPLE {
     --      },
     -- Caveat => {},
 
     SeeAlso => {SimplicialComplexes}
     }


document { 
     Key => {lyubeznikComplex, (lyubeznikComplex,List,Ring), (lyubeznikComplex,MonomialIdeal)},
     Headline => "Simplicial complex supporting the Lyubeznik resolution of a  monomial ideal",
     Usage => "lyubeznikComplex(L,R)\nlyubeznikComplex I",
     -- Inputs => {
     --      },
     -- Outputs => {
     --      },
     -- Consequences => {
     --      },     
     -- "description",
     -- EXAMPLE {
     --      },
     -- Caveat => {},

     SeeAlso => {SimplicialComplexes}
     }

document { 
     Key => {superficialComplex, (superficialComplex,List,Ring), (superficialComplex,MonomialIdeal)},
     Headline => "Simplicial complex supporting a superficial resolution of a monomial ideal",
     Usage => "superficialComplex(L,R)\nsuperficialComplex I",
     -- Inputs => {
     --      },
     -- Outputs => {
     --      },
     -- Consequences => {
     --      },     
     -- "description",
     -- EXAMPLE {
     --      },
     -- Caveat => {},

     SeeAlso => {SimplicialComplexes}
     }




document { 
     Key => {isPure,(isPure,SimplicialComplex)},
     Headline => "whether the facets are equidimensional",
     Usage => "isPure D",
     Inputs => {
	  "D" => SimplicialComplex
          },
     Outputs => {
	  Boolean => {TO true, " if the facets of ", TT "D", " all have the same dimension, 
	       and ", TO false, " otherwise"}
          },
     EXAMPLE {
          "R = ZZ[a..f];",
	  "D = simplicialComplex {a*b*c, a*b*d, d*e*f} ",
	  "isPure D"
	  },
     EXAMPLE {
	  "E = simplicialComplex {a*b*c, b*d, d*e*f} ",
	  "isPure E"
          },
     SeeAlso => {SimplicialComplexes, (dim,SimplicialComplex),facets}
     }
document { 
     Key => (ring,SimplicialComplex),
     Usage => "R = ring D",
     Inputs => {
	  "D"
          },
     Outputs => {
	  "R" => Ring => {"the polynomial ring used to define ", TT "D"}
          },
     "The vertices of every simplicial complex are variables in the polynomial ring ", TT "R", ",
     and subsets of vertices, such as faces, are represented as squarefree monomials in ", TT "R", ".",
     EXAMPLE {
          "R = QQ[a..d];",
	  "D = simplicialComplex monomialIdeal(a*b*c*d);",
	  "ring D",
	  "coefficientRing D",
          "S = ZZ[w..z];",
	  "E = simplicialComplex monomialIdeal(w*x*y*z);",
	  "ring E",
	  "coefficientRing E"
          },
     PARA{},
     "There is a bijection between simplicial complexes and squarefree
     monomial ideals.  This package exploits this correspondence by
     using commutative algebra routines to perform most of the necessary
     computations.",
     Caveat => {"Some operations depend on the choice of ring, or its coefficient ring"},
     SeeAlso => {SimplicialComplexes, (coefficientRing, SimplicialComplex)}
     }
document { 
     Key => (coefficientRing,SimplicialComplex),
     Usage => "coefficientRing D",
     Inputs => {
	  "D"
          },
     Outputs => {
	  Ring => {"the coefficients of the defining ",
	       TO2((ring,SimplicialComplex),"polynomial ring"),
	       " of ", TT "D"}
          },
     EXAMPLE {
          "R = QQ[a..d];",
	  "D = simplicialComplex monomialIdeal(a*b*c*d);",
	  "ring D",
	  "coefficientRing D",
          "S = ZZ[w..z];",
	  "E = simplicialComplex monomialIdeal(w*x*y*z);",
	  "ring E",
	  "coefficientRing E"
          },
     "Some computations depend on the choice of coefficient ring, for example,
     the boundary maps and the chain complex of D.",
     EXAMPLE {
	  "chainComplex D",
	  "chainComplex E"
          },
     SeeAlso => {SimplicialComplexes, 
	  (ring,SimplicialComplex), 
	  (chainComplex,SimplicialComplex), 
	  boundary}
     }
document { 
     Key => {label, (label,SimplicialComplex,List)},
     Headline => "labels with monomials the faces of simplicial complex",
     -- Usage => "",
     -- Inputs => {
     --      },
     -- Outputs => {
     --      },
     -- Consequences => {
     --      },     
     -- "description",
     -- EXAMPLE {
     --      },
     -- Caveat => {},
     -- SeeAlso => {}
     }
document { 
     Key => {link,(link,SimplicialComplex,RingElement)},
     Headline => "link of a face in a simplicial complex",
     Usage => "link(D,f)",
     Inputs => {
	  "D" => SimplicialComplex,
	  "f" => RingElement => {"a monomial representing a face of the simplicial complex ", TT "D"}
          },
     Outputs => {
	  SimplicialComplex => {"the link of ", TT "f", " in ", TT "D"}
          },
     TEX "The link of a face $f$ in $D$ is the simplicial complex whose faces 
     are the subsets $g$ whose intersection
     with $f$ is empty, where $f \\cup g$ is a face of $D$.",
     EXAMPLE {
	  "R = QQ[x0,x1,x2,x3,x4,x5,x6];",
	  "D = simplicialComplex {x0*x1*x3, x1*x3*x4, x1*x2*x4, x2*x4*x5,
	       x2*x3*x5, x3*x5*x6, x3*x4*x6, x0*x4*x6,
	       x0*x4*x5, x0*x1*x5, x1*x5*x6, x1*x2*x6,
	       x0*x2*x6, x0*x2*x3}",
	  "link(D,x0)",
	  "link(D,x0*x2)"
	  },
     SeeAlso => {SimplicialComplexes
	  }
     }
document { 
     Key => (dual,SimplicialComplex),
     Headline => "the Alexander dual of a simplicial complex",
     Usage => "dual D",
     Inputs => {
	  "D"
          },
     Outputs => {
	  SimplicialComplex => {"the Alexander dual of ", TT "D"}
          },
     "The Alexander dual of ", TT "D", " is the simplicial complex
     whose faces are the complements of the nonfaces of ", TT "D", ".
     The Alexander dual of a square is the disjoint union of
     two edges.",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "D = simplicialComplex {a*b,b*c,c*d,d*a}",
          "dual D"
	  },
     PARA{},
     "The Alexander dual is homotopy equivalent to the complement of ", TT "D", " in the sphere generated
     by all of the variables in the ", TO2((ring,SimplicialComplex),"ring"),
     " of ", TT "D", ".  In particular, it depends on the number of variables.",
     EXAMPLE {
	  "R = ZZ[a..e]",
	  "E = simplicialComplex {a*b,b*c,c*d,d*a}",
          "dual E"
          },
     "The projective dimension of the face ring of D equals the
     regularity of the face ideal of the Alexander dual of D
     see e.g., Corollary 5.59 of Miller-Sturmfels, Combinatorial
     Commutative Algebra.",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)",
	  "A = dual D",
	  "pdim (R^1/(ideal D))",
	  "regularity ideal A"
	  },
     PARA{},
     "Alexander duality interchanges extremal betti numbers of the face ideals.
     Following example 3.2 in Bayer-Charalambous-Popescu, ", EM "Extremal betti
     numbers and applications to monomial ideals", ", we have ",
     EXAMPLE {
	  "R = QQ[x0,x1,x2,x3,x4,x5,x6];",
	  "D = simplicialComplex {x0*x1*x3, x1*x3*x4, x1*x2*x4, x2*x4*x5,
	       x2*x3*x5, x3*x5*x6, x3*x4*x6, x0*x4*x6,
	       x0*x4*x5, x0*x1*x5, x1*x5*x6, x1*x2*x6,
	       x0*x2*x6, x0*x2*x3}",
	  "I = ideal D",
	  "J = ideal dual D",
	  "betti res I",
	  "betti res J"
	  },
     SeeAlso => {SimplicialComplexes, (dual,MonomialIdeal)}
     }

///
-- Greg and Mike were working on this when Greg had to go home
-- 7/13/05  Good example though!
     "Hochster gives a formula relating the homology of the Alexander dual 
     to the betti numbers of the Stanley-Reisner ideal, see e.g., 
     Corollary 1.40 in
     Miller-Sturmfels, Combinatorial Commutative Algebra. ",
     EXAMPLE {
	  --R = QQ[a..f];
	  R = QQ[a..f, Degrees => {
                          {1, 1, 0, 0, 0, 0, 0}, 
                          {1, 0, 1, 0, 0, 0, 0}, 
                          {1, 0, 0, 1, 0, 0, 0}, 
			  {1, 0, 0, 0, 1, 0, 0}, 
			  {1, 0, 0, 0, 0, 1, 0}, 
			  {1, 0, 0, 0, 0, 0, 1}}]
	  oct = simplicialComplex monomialIdeal(a*b,c*d,e*f)
	  cube = dual oct
	  lk = (D,m) -> simplicialComplex monomialIdeal(ideal support m + ((ideal D):m));
	  F = link(oct,a)
	  rank HH_1(F)
	  C = res ideal cube
	  tally degrees C_3
	  checkHochster = (D,face) -> (
	       R := ring D;
	       face' := (product gens R) // face;
	       D' := dual D;
	       h := apply(0..dim D', i -> (
     	           rank HH_(i-1)(link(D',face'))));
	       C := res ideal D;
	       b := apply(0..dim D', i -> (
			 d := tally degrees C_(i+1);
			 if d#?(degree face) then d#(degree face) else 0));
	       (b,h))
          checkHochster(cube,b*d*e*f)
	  checkHochster(oct,a*c)
	  checkHochster(oct,a*b)
	  checkHochster(oct,c*d*e*f)
	  checkHochster(cube,a*b*c*d*e)
	  },
///

document { 
     Key => {faces,(faces,ZZ,SimplicialComplex)},
     Headline => "the i-faces of a simplicial complex ",
     Usage => "faces(i,D)",
     Inputs => {
	  "i" => ZZ => "the dimension of the faces",
	  "D" => SimplicialComplex
          },
     Outputs => {
	  Matrix => {"with one row, whose entries are squarefree
	       monomials representing the faces of dimension ", 
	       TT "i", " of ", TT "D"}
          },
     "In Macaulay2, every ", TO2(SimplicialComplex, "simplicial complex"),
     " is equipped with a polynomial ring, and the matrix of i-faces
     is defined over this ring.",
     PARA {
     	  "This triangulation of the real projective plane has 6
     	  vertices, 15 edges and 10 triangles."
	  },
     EXAMPLE {
	  "R = ZZ[a..f]",
	  "D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,
	                                      b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)",
          "faces(-1,D)",
	  "faces(0,D)",
	  "faces(1,D)",
	  "faces(2,D)",
	  "fVector D"
          },
     PARA{},
     "To avoid repeated computation, 
     the matrix of ", TT "i", "-faces is cached at ", 
     TT "D.cache.faces#i", ".
     This function will use this value if it has already been 
     computed.",
     SeeAlso => {SimplicialComplexes,
	  facets,
	  boundary,
	  fVector
	  }
     }

document { 
     Key => (ideal,SimplicialComplex),
     Headline => "the ideal of minimal nonfaces (the Stanley-Reisner ideal)",
     Usage => "ideal D",
     Inputs => {
	  "D"
          },
     Outputs => {
	  Ideal => {"which is generated by monomials representing
	  the minimal nonfaces of ", TT "D"}
          },
     "In Macaulay2, every ", TO2(SimplicialComplex, "simplicial complex"),
     " is equipped with a polynomial ring, and the Stanley-Reisner ideal
     is contained in this ring.",
     PARA {
     	  "The 3-dimensional sphere has a unique minimal nonface
     	  which corresponds to the interior."
	  },
     EXAMPLE {
	  "R = ZZ[a..e];",
	  "sphere = simplicialComplex {b*c*d*e,a*c*d*e,a*b*d*e,a*b*c*e,a*b*c*d}",
	  "ideal sphere"
	  },
     "The simplicial complex from example 1.8
     in Miller-Sturmfels, Combinatorial Commutative Algebra,
     consists of a triangle (on vertices ", TT "a,b,c", 
	  "), two edges connecting ", TT "c", " to ", TT "d", 
	  " and ", TT "b", " to ", TT "d", 
	  ", and an isolated vertex ", TT "e", ".",
     EXAMPLE {
	  "D = simplicialComplex {e, c*d, b*d, a*b*c}",
	  "ideal D"
	  },
     "There are six minimal nonfaces of ", TT "D", ".",
     PARA{},
     "This routine is identical to ", TO (monomialIdeal,SimplicialComplex),
     ", except for the ", TO2(Type,"type"), " of the output.",
     PARA{},
     "Note that no computatation is performed by this routine; all the
     computation was done while constructing the simplicial complex.",
     SeeAlso => {SimplicialComplexes, 
	  simplicialComplex, 
	  facets, 
	  (monomialIdeal,SimplicialComplex)}
     }
document { 
     Key => (monomialIdeal,SimplicialComplex),
     Headline => "the monomial ideal of minimal nonfaces (the Stanley-Reisner ideal)",
     Usage => "monomialIdeal D",
     Inputs => {
	  "D"
          },
     Outputs => {
	  MonomialIdeal => {"which is generated by monomials representing
	  the minimal nonfaces of ", TT "D"}
          },
     "In Macaulay2, every ", TO2(SimplicialComplex, "simplicial complex"),
     " is equipped with a polynomial ring, and the Stanley-Reisner ideal
     is contained in this ring.",
     PARA {
     	  "The 3-dimensional sphere has a unique minimal nonface
     	  which corresponds to the interior."
	  },
     EXAMPLE {
	  "R = ZZ[a..e];",
	  "sphere = simplicialComplex {b*c*d*e,a*c*d*e,a*b*d*e,a*b*c*e,a*b*c*d}",
	  "monomialIdeal sphere"
	  },
     "The simplicial complex from example 1.8
     in Miller-Sturmfels, Combinatorial Commutative Algebra,
     consists of a triangle (on vertices ", TT "a,b,c", 
	  "), two edges connecting ", TT "c", " to ", TT "d", 
	  " and ", TT "b", " to ", TT "d", 
	  ", and an isolated vertex ", TT "e", ".",
     EXAMPLE {
	  "D = simplicialComplex {e, c*d, b*d, a*b*c}",
	  "monomialIdeal D"
	  },
     "There are six minimal nonfaces of ", TT "D", ".",
     PARA{},
     "This routine is identical to ", TO (ideal,SimplicialComplex),
     ", except for the ", TO2(Type,"type"), " of the output.",
     PARA{},
     "Note that no computatation is performed by this routine; all the
     computation was done while constructing the simplicial complex.",
     SeeAlso => {SimplicialComplexes, 
	  simplicialComplex, 
	  facets, 
	  (ideal,SimplicialComplex)}
     }

document { 
     Key => {facets, (facets,SimplicialComplex)},
     Headline => "the facets of a simplicial complex",
     Usage => "facets D",
     Inputs => {
	  "D" => SimplicialComplex
          },
     Outputs => {
	  Matrix => {"with one row, whose entries are squarefree
	       monomials representing the facets (maximal faces) of ", TT "D"}
          },
     "In Macaulay2, every ", TO2(SimplicialComplex, "simplicial complex"),
     " is equipped with a polynomial ring, and the resulting matrix of facets
     is defined over this ring.",
     PARA {
     	  "The 3-dimensional sphere has a unique minimal nonface
     	  which corresponds to the interior."
	  },
     EXAMPLE {
	  "R = ZZ[a..e];",
	  "sphere = simplicialComplex monomialIdeal(a*b*c*d*e)",
	  "facets sphere"
	  },
     "The following ", TO faces, " generate a simplicial complex
     consisting of a triangle (on vertices ", TT "a,b,c", 
	  "), two edges connecting ", TT "c", " to ", TT "d", 
	  " and ", TT "b", " to ", TT "d",
	  ", and an isolated vertex ", TT "e", ".",
     EXAMPLE {
	  "D = simplicialComplex {e, c*d, b*d, a*b*c, a*b, c}",
	  "facets D"
	  },
     "There are four facets of ", TT "D", ".",
     PARA{},
     "Note that no computatation is performed by this routine; all the
     computation was done while constructing the simplicial complex.",
     PARA{},
     "A simplicial complex is displayed by listing its facets, and so this
     function is frequently unnecessary.",
     SeeAlso => {SimplicialComplexes, 
	  simplicialComplex, 
	  faces
	  }
     }


-------------------------------------------------------------
-------------------------------------------------------------
-- 20/07/2018 Lorenzo: new/modified documentation

document {
     Key => {skeleton,(skeleton,ZZ,SimplicialComplex)},
     Headline => "the n-skeleton of the simplicial complex D",
     Usage => "skeleton(n,D)",
     Inputs => {
      "i" => ZZ,
      "D" => SimplicialComplex
          },
     Outputs => {
      SimplicialComplex => {"the ", TT "n","-skeleton of a simplicial complex,
       i.e. the subcomplex of all subfaces of dimension at most ", TT "n"},
          },
     "The 2-skeleton of the 5-simplex.",
     EXAMPLE {
          "R = ZZ[a..f];",
          "simplex = simplicialComplex{a*b*c*d*e*f}",
      "skel = skeleton(2,simplex)",
      "fVector simplex",
      "fVector skel"
      },
     SeeAlso => {SimplicialComplexes, fVector, faces}
     }

document {
     Key => {fVector,(fVector,SimplicialComplex),[fVector,Flag]},
     Headline => "the f-vector of a simplicial complex",
     Usage => "f = fVector D",
     Inputs => {
      "D" => SimplicialComplex,
      Flag => Boolean => "the flag f-vector if the simplicial complex is properly defined over a multigraded ring."
          },
     Outputs => {
      "f" => {"such that ", TT "f#i",
      " is the number of faces in ", TT "D",
      " of dimension ", TT "i", " for ", TT "-1 <= i <= dim D", " or of squarefree degree ", TT "i."}
          },
     "The pentagonal bipyramid has 7 vertices, 15 edges
     and 10 triangles.",
     EXAMPLE {
      "R = ZZ[a..g];",
      "bipyramid = simplicialComplex monomialIdeal(
      a*g, b*d, b*e, c*e, c*f, d*f)",
      "f = fVector bipyramid",
      "f#0",
      "f#1",
      "f#2"
          },
     "Every simplicial complex other than the void
     complex has a unique face of dimension -1.",
     EXAMPLE {
      "void = simplicialComplex monomialIdeal 1_R",
      "fVector void"
      },
     "For a larger examp;le we consider the polarization
     of an artinian monomial ideal from section 3.2 in
     Miller-Sturmfels, Combinatorial Commutative Algebra.",
     EXAMPLE {
      "S = ZZ[x_1..x_4, y_1..y_4, z_1..z_4];",
      "I = monomialIdeal(x_1*x_2*x_3*x_4,
           y_1*y_2*y_3*y_4,
           z_1*z_2*z_3*z_4,
           x_1*x_2*x_3*y_1*y_2*z_1,
           x_1*y_1*y_2*y_3*z_1*z_2,
           x_1*x_2*y_1*z_1*z_2*z_3);",
          "D = simplicialComplex I;",
      "fVector D"
      },
      "The boundary of the 3-dimensional cross-polytope is
      3-colorable. If we define this simplicial complex over
      a ", TT "Z^3", "-graded ring we can ask for its flag
      f-vector.",
      EXAMPLE {
      "grading = {{1,0,0},{1,0,0},{0,1,0},{0,1,0},{0,0,1},{0,0,1}};",
      "S = ZZ[x_1..x_6, Degrees => grading];",
      "I = monomialIdeal(x_1*x_2,x_3*x_4,x_5*x_6);",
      "fVector simplicialComplex I",
      "fVector(simplicialComplex I, Flag => true)"
      },
     Caveat => {
     "The option ", TT "Flag", " checks if the multigrading corresponds to a properly d-coloring of "
     , TT "D", ", where d is the dimension of ", TT "D", " plus one. If it is not the case the output
     is an empty HashTable."
     },
     PARA{},
     "The f-vector is computed using the Hilbert series
     of the Stanley-Reisner ideal.  For example, see
     Hosten and Smith's
     chapter Monomial Ideals, in Computations in
     Algebraic Geometry with Macaulay2, Springer 2001.",
     SeeAlso => {SimplicialComplexes,
      faces}
     }

--These are documented in the above node.
undocumented { "Flag" }


document {
     Key => {algebraicShifting,(algebraicShifting,SimplicialComplex),[algebraicShifting,Multigrading]},
     Headline => "the algebraic shifting of a simplicial complex",
     Usage => "A = algebraicShifting D",
     Inputs => {
     "D" => SimplicialComplex,
     Multigrading => Boolean => "If true it returns the colored algebraic shifting w.r.t. the multigrading of the underlying ring."
          },
     Outputs => {
     "A" => {"The algebraic shifting of the simplicial complex ", TT "D", ". If ", TT "Multigrading => true", " then it returns the so called colored shifted complex."}
          },
     "The boundary of the stacked 4-polytope on 6 vertices. Algebraic shifting preserves the f-vector.",
     EXAMPLE {
      "R=QQ[x_1..x_6];",
      "I=monomialIdeal(x_2*x_3*x_4*x_5,x_1*x_6);",
      "stacked = simplicialComplex(I)",
      "shifted = algebraicShifting(stacked)",
      "fVector stacked",
      "fVector shifted"
          },
     "An empty triangle is a shifted complex.",
     EXAMPLE {
     "R=QQ[a,b,c];",
     "triangle = simplicialComplex{a*b,b*c,a*c};",
     "algebraicShifting(triangle) == triangle "
     },
     "The multigraded algebraic shifting does not preserve the Betti numbers.",
     EXAMPLE {
      "grading = {{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,0,1}};",
      "R=QQ[x_{1,1},x_{1,2},x_{1,3},x_{2,1},x_{3,1}, Degrees=>grading];",
      "delta = simplicialComplex({x_{1,3}*x_{2,1}*x_{3,1},x_{1,1}*x_{2,1},x_{1,2}*x_{3,1}})",
      "shifted = algebraicShifting(delta, Multigrading => true)",
      "prune (homology(delta))_1",
      "prune (homology(shifted))_1"
     },
     "References:",
     PARA {},
     "G. Kalai, Algebraic Shifting, Computational Commutative Algebra and Combinatorics, 2001;",
      PARA {},
     "S. Murai, Betti numbers of strongly color-stable ideals and squarefree strongly color-stable ideals, Journal of Algebraic Combinatorics."
     }

--These are documented in the above node.
undocumented { "Multigrading" }

document {
     Key => {star,(star,SimplicialComplex,RingElement)},
     Headline => "star of a face in a simplicial complex",
     Usage => "star(D,f)",
     Inputs => {
      "D" => SimplicialComplex,
      "f" => RingElement => {"a monomial representing a face of the simplicial complex ", TT "D"}
          },
     Outputs => {
      SimplicialComplex => {"the star of ", TT "f", " in ", TT "D"}
          },
     TEX "The star of a face $f$ in $D$ is the simplicial complex whose faces
     are the subsets $g$ with $f \\cup g$ is a face of $D$.",
     PARA {},
     " The bow-tie complex.",
     EXAMPLE {
      "R = QQ[x_1..x_5];",
      "bowtie = simplicialComplex {x_1*x_2*x_3,x_3*x_4*x_5}",
      "star(bowtie,x_3)",
      "star(bowtie,x_1*x_2)"
      },
     PARA {},
     " The 3-simplex and a copy of its boundary glued along a triangle.",
     EXAMPLE {
      "R = QQ[a..e];",
      "D = simplicialComplex {a*b*c*d, b*c*e, b*d*e, c*d*e}",
      "star(D,b*c*d)",
      "star(D,b)"
      },
     SeeAlso => {SimplicialComplexes, link
      }
     }

document {
     Key => {joinSimplicial,(joinSimplicial,SimplicialComplex,SimplicialComplex)},
     Headline => "the join of two simplicial complexes",
     Usage => "joinSimplicial(D,E)",
     Inputs => {
      "D" => SimplicialComplex,
      "E" => SimplicialComplex
          },
     Outputs => {
      SimplicialComplex => {"the join of ", TT "D", " and ", TT "E"}
          },
     TEX "The join of two simplicial complexes $D$ and $E$ is the simplicial complex whose faces
     are the union of faces of $D$ and $E$. If $D$ is the simplicial complex consisting of a
     single vertex then the join is the cone over $E$. If $D$ consists of two isolated vertices
     then the join is the suspension of $E$.",
     PARA {},
     " The cone over a bow-tie complex.",
     EXAMPLE {
      "R = QQ[x_1..x_5];",
      "bowtie = simplicialComplex {x_1*x_2*x_3, x_3*x_4*x_5};",
      "S = QQ[v];",
      "singleton = simplicialComplex {v};",
      "singleton * bowtie"
      },
     PARA {},
     " The octahedron is the suspension of a square.",
     EXAMPLE {
      "R = QQ[a..d];",
      "square = simplicialComplex {a*b, b*c, c*d, a*d};",
      "S = QQ[p,q];",
      "poles = simplicialComplex {p, q};",
      "octahedron = joinSimplicial(poles,square)"
      },
      PARA {},
      " The join of an exagon and a pentagon.",
      EXAMPLE {
      "R = ZZ[x_1..x_6];",
      "exagon = simplicialComplex {x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_5,x_5*x_6,x_1*x_6};",
      "S = ZZ[y_1..y_5];",
      "pentagon = simplicialComplex {y_1*y_2,y_2*y_3,y_3*y_4,y_4*y_5,y_1*y_5};",
      "sphere = joinSimplicial(exagon,pentagon)",
      "fVector(sphere)"
      },
      Caveat => {
      "The two simplicial complexes have to be defined over different polynomial rings."
      },
      SeeAlso => {SimplicialComplexes, (symbol *,SimplicialComplex,SimplicialComplex)
      }
     }

document {
     Key => (symbol *,SimplicialComplex,SimplicialComplex),
     Headline => "the join of two simplicial complexes",
     Usage => "  J = D * E",
     Inputs => {
      "D" => SimplicialComplex,
      "E" => SimplicialComplex
      },
     Outputs => {
      "J" => SimplicialComplex
      },

     PARA{}, "Computes join of ",TT "D"," and ",TT "E",".",

     PARA{}, "See also ",TO joinSimplicial,".",

     }

------------------------------------------------------------------------

-- 20/07/2018 Lorenzo: new tests

----------------------------------------------
-- Boundary of the 4-Cross-Polytope
----------------------------------------------

TEST ///
S = QQ[x_{1,1}..x_{2,4}, Degrees => {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
I = monomialIdeal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,3},x_{1,4}*x_{2,4})
D = simplicialComplex(I)
assert( (fVector(D))#2 == 32)
assert( (fVector(D, Flag => true))#{1,1,0,0} == 4)
///

------------------------------------------------
-- Test
------------------------------------------------
TEST ///
R1 = QQ[x,y,z]
S1 = simplicialComplex {x*y*z}
R2 = QQ[a,b]
S2 = simplicialComplex {a,b}
J = S1 * S2
T = ring J
assert((fVector(S1 * S2))#1 == (fVector(S1))#1 + (fVector(S1))#0*(fVector(S2))#0)
assert(star(J, x*y*z) == J)
assert(#(flatten entries facets(star(J, a))) == 1)
///
--------------------------------------------------
-- Real Projective plane
-------------------------------------------------
TEST ///
R = ZZ[a..f]
RP2 = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
skel = skeleton(1, RP2)
-- removing facets creates holes captured by HH_1
assert(rank HH_1 skel == (fVector(RP2))#2)
assert(dim skel == 1)
S = ZZ[v]
v = simplicialComplex {v}
-- the cone is contractible
conewrtv = joinSimplicial(v, RP2)
assert(prune HH_1 conewrtv == 0)
///

---------------------------------------------------------------------------
-- Example from Betti numbers of strongly color-stable ideals and
-- squarefree strongly color-stable ideals
-- Satoshi Murai
-----------------------------------------------------------------------
TEST ///
grading = {{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,0,1}}
R = QQ[x_{1,1},x_{1,2},x_{1,3},x_{2,1},x_{3,1}, Degrees => grading]
delta = simplicialComplex({x_{1,3}*x_{2,1}*x_{3,1},x_{1,1}*x_{2,1},x_{1,2}*x_{3,1}})
shifted = algebraicShifting(delta, Multigrading => true)
assert((fVector(delta))#0 == (fVector(shifted))#0)
assert((fVector(delta))#1 == (fVector(shifted))#1)
assert((fVector(delta))#2 == (fVector(shifted))#2)
assert(prune homology delta != prune homology shifted)
///

----------------------------------------------
-- Boundary of the 4-Cross-Polytope
----------------------------------------------
TEST ///
grading = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}
S = QQ[x_{1,1}..x_{2,4}, Degrees => grading]
I = monomialIdeal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,3},x_{1,4}*x_{2,4})
cross = simplicialComplex(I)
assert( (fVector(cross))#2 == 32)
assert( (fVector(cross, Flag => true))#{1,1,0,0} == 4)
assert(dim skeleton(2,cross) == 2)
assert((fVector(skeleton(2,cross)))#1 == (fVector(cross))#1)
multishifted = algebraicShifting(cross, Multigrading => true)
stdshifted = algebraicShifting(cross)
assert( cross == multishifted)
assert( cross != stdshifted)
///
------------------------------------------------------------------------
-- Cartwright-Sturmfels ideals associated to graphs and linear spaces --
-- Aldo Conca, Emanuela De Negri, Elisa Gorla --
-- Ex. 1.10
------------------------------------------------------------------------

TEST ///
row_grading = {{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}
S=QQ[x_{1,1}..x_{2,3}, Degrees => row_grading]
I = ideal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,2},x_{1,2}*x_{2,3},x_{1,3}*x_{2,3})
multigin = ideal(x_{1,1}^2*x_{2,3},x_{1,1}*x_{1,2}*x_{2,3},x_{1,1}*x_{2,1},x_{1,2}*x_{2,1},x_{1,3}*x_{2,1},x_{1,1}*x_{2,2},x_{1,2}*x_{2,2})
stdgin = ideal(x_{1,1}^2,x_{1,1}*x_{1,2},x_{1,2}^2,x_{1,1}*x_{1,3},x_{1,2}*x_{1,3},x_{1,3}^3,x_{1,3}^2*x_{2,1})
assert(gin(I, AttemptCount => 10, Multigraded => true) == multigin)
assert(gin(I, AttemptCount => 10) == stdgin)
///

------------------------------------------------------------------------
-- Stacked 3-sphere on 7 vertices
------------------------------------------------------------------------
TEST ///
S = QQ[x_1..x_7]
I = monomialIdeal(x_2*x_3*x_4*x_5,x_3*x_4*x_5*x_6,x_1*x_6,x_1*x_7,x_2*x_7)
st73 = simplicialComplex I
shifted = algebraicShifting (st73)
assert( (fVector(st73))#3 == (fVector(shifted))#3)
assert(prune homology st73 == prune homology shifted)
assert(not member(x_1*x_2*x_3*x_4, flatten entries facets(shifted)))
///

------------------------------------------------------------------------
-- Test
------------------------------------------------------------------------
TEST///
S = QQ[a..f]
D = simplicialComplex({a*b*c,b*c*d,d*e*f})
stD = star(D, d)
assert(star(D, d) == simplicialComplex({d*e*f,b*c*d}))
T = QQ[v]
conev = stD * simplicialComplex({v})
assert( (fVector(conev))#0 == 6 )
assert( (fVector(conev))#1 == 11 )
assert( (fVector(conev))#2 == 8 )
///


------------------------------------------------------------------------------

TEST ///

kk = ZZ
R = kk[x]

void = simplicialComplex monomialIdeal(1_R)
assert isPure void
assert(dim void == -infinity)
assert(faces(0,void) == 0)
assert(faces(-1,void) == 0)
dual void
C = chainComplex void
assert( C.dd^2 == 0 )
assert(HH_0(void) == 0)
assert(HH_-1(void) == 0)
fVector void
assert(boundary void  == void)

irrelevant = simplicialComplex monomialIdeal gens R
assert isPure irrelevant
assert(dim irrelevant === -1)
assert(faces(0,irrelevant) == 0)
assert(numgens source faces(-1,irrelevant) === 1)
assert(irrelevant == dual irrelevant)
C = chainComplex irrelevant
assert( C.dd^2 == 0 )
assert(HH_0(irrelevant) == 0)
assert(HH_-1(irrelevant) == R^1)
assert(fVector irrelevant === new HashTable from {-1=>1})
assert(boundary irrelevant == void)

D5 = simplicialComplex {1_R}
D5 == irrelevant
///


TEST ///
kk = ZZ
R = kk[x_1..x_4]
D6 = simplicialComplex monomialIdeal gens R
time A6 = dual D6
time C = chainComplex A6;
assert( C.dd^2 == 0 )
C
time prune HH(C)
fVector D6

D7 = simplicialComplex monomialIdeal 1_R
dual D7
fVector D7
///
-- examples
-----------------------------------------
-- Miller and Sturmfels, example 1.8 ----
-----------------------------------------
TEST ///
kk = ZZ
R = kk[a..e]
D = simplicialComplex monomialIdeal(a*d, a*e, b*c*d, d*e, c*e, b*e)
assert not isPure D
fVector D
ideal dual D == monomialIdeal (a*b*c*d, a*b*e, a*c*e, d*e)
fVector boundary D
boundary D
S = ZZ/32003[u,v,w,x,y]
label(D, {u,v,w,x,y})
C = chainComplex D
assert( C.dd^2 == 0 )
prune HH(C)
label(D,{})
///
-----------------------------------------
-- torus  : Munkres page 15 example 3 ---
-----------------------------------------
TEST ///
kk = QQ
R = kk[a..j]
D = simplicialComplex{a*b*i, a*e*i, i*b*j, j*c*b, j*c*a, j*a*e,
     e*i*f, i*h*f, i*h*j, j*e*d, j*g*d, j*h*g, g*h*f, f*e*d,
     d*f*a, f*b*a, f*g*c, f*b*c, g*c*a, g*d*a}
assert isPure D
C = chainComplex D
assert( C.dd^2 == 0 )
prune HH(C)
D' = dual D
C' = chainComplex D'
assert( C'.dd^2 == 0 )
prune HH(C')
fVector D
boundary D
fVector boundary D
///
----------------------------------------------
-- Klein bottle : Munkres page 18 example 5 --
----------------------------------------------
TEST ///
kk = ZZ/2
R = kk[a..j]
D = simplicialComplex {a*b*i, a*e*i, b*i*j, b*c*j, a*c*j, 
     a*d*j, e*f*i, f*h*i, h*i*j, d*e*j, e*g*j, g*h*j, 
     f*g*h, d*e*f, a*d*f, a*b*f, c*f*g, b*c*f, a*c*g, a*e*g}
isPure D
C = chainComplex D
assert( C.dd^2 == 0 )
prune HH(C)
fVector D
///
---------------------------
-- Real Projective Plane --
---------------------------
TEST ///
kk = ZZ
R = kk[a..f]
D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
C = chainComplex D
assert( C.dd^2 == 0 )
prune HH(C)
fVector D
boundary D
fVector boundary D
///
----------------------------------------
-- Degenerations of Abelian surfaces ---
-- Gross and Popescu, math.AG/9609001 --
----------------------------------------
-- n >= 13
TEST ///
kk = QQ
abelian = (n) -> (
     R := kk[local x_0..local x_(n-1)];
     L1 = toList apply(0..n-1, i -> x_i * x_((i+3)%n) * x_((i+4)%n));
     L2 = toList apply(0..n-1, i -> x_i * x_((i+1)%n) * x_((i+4)%n));
    join(L1,L2))

D = simplicialComplex abelian 8
numgens source faces(0,D)
numgens source faces(1,D)
numgens source faces(2,D)
numgens source faces(3,D)
C = chainComplex D
assert( C.dd^2 == 0 )
prune HH(C)
transpose gens ideal D     
fVector D
///
------------------------------
-- Simplex with labelling ----
------------------------------
TEST ///
R = ZZ[a..e]
D = simplicialComplex monomialIdeal product gens R
D = dual simplicialComplex monomialIdeal gens R
S = ZZ/32003[u,v,x,y,z]
L = {x^2, x*y, x*z, y^2, y*z}
label(D,L)
C = chainComplex D
assert( C.dd^2 == 0 )
///
------------------------------
-- testing the chain complexes
TEST ///
R = ZZ/101[a..e]
D = simplicialComplex monomialIdeal product gens R
boundary(0,D)
boundary(1,D)
boundary(2,D)
boundary(3,D)
boundary(4,D)
C = chainComplex D
assert( C.dd^2 == 0 )
HH_3(C)
HH_2(C)
prune oo
///

TEST ///
kk = ZZ
R = kk[a..h]
I = monomialIdeal(a*b*c*d,e*f*g*h)
D = simplicialComplex I
fVector D
chainComplex D
E = simplicialComplex{a*b*c*d, e*f*g*h}
dual D
dual E
faces(2,D)
faces(3,D)
faces(4,D)
faces(5,D)
faces(6,D)
faces(7,D)
faces(-1,D)
faces(-2,D)
faces(0,D)

assert try (simplicialComplex {};false) else true
///

TEST ///
R = ZZ/101[symbol x_0 .. symbol x_3]
D = simplicialComplex {x_0 * x_1 * x_2, x_1 * x_2 * x_3}
facets D
dual D
faces(0,D)
chainComplex D
dual D
///
----------------------
-- link of a face ----
----------------------
TEST ///
R = ZZ[a..e]
D = simplicialComplex {b*c,c*a,a*e,a*d,c*d,d*e}
I = ideal D
assert(link(D,a) == simplicialComplex{c,d,e})

D = simplicialComplex {b*c,c*a,a*e,a*d,c*d,d*e,a*c*d,a*d*e}
assert(link(D,a) == simplicialComplex{c*d,d*e})
assert(link(D,a*d) == simplicialComplex{c,e})
assert(link(D,c*d) == simplicialComplex{a})
///
------------------------------------------------------------------
-- Buchberger/Lyubeznik/Superficial complexes of a monomial ideal --
------------------------------------------------------------------
TEST ///
debug SimplicialComplexes
S=ZZ/32003[x,y,z]
L={x^3,x*y,x*z,y^2,y*z,z^2}
R = ZZ/32003[a..f]
D = buchbergerComplex(L,R)
label(D,L)
-- peek D.cache.labels
boundary(0,D)
boundary(1,D)
C = chainComplex D
assert(C.dd^2 == 0)
prune(HH C)
scan(0..dim D, i -> assert(HH_(i+1)(C) == 0))
assert(HH_0(C) == S^1/(ideal L))
assert isHomogeneous C
C.dd
----
E = lyubeznikComplex(L,R)
label(E,L)
B = chainComplex E
assert(B.dd^2 == 0)
betti B
prune(HH B)
scan(0..dim E, i -> assert(HH_(i+1)(B) == 0))
assert(HH_0(B) == S^1/(ideal L))
assert isHomogeneous B
----
F = superficialComplex(L,R)
label(F,L)
A = chainComplex F
betti A
prune(HH A)
scan(0..dim F, i -> assert(HH_(i+1)(A) == 0))
assert(HH_0(A) == S^1/(ideal L))
assert isHomogeneous A
///

--------------------------------------------------------------------------------
-- A generic monomial ideal (Buchberger complex supports the minimal resolution)
--------------------------------------------------------------------------------
TEST ///
debug SimplicialComplexes
S=ZZ/32003[x,y,z]
L={y*z,x^2*z^2,x^2*y^2}
R = ZZ/32003[a..c]
D = buchbergerComplex(L,R)
label(D,L)
C = chainComplex D
assert(C.dd^2 == 0)
betti C
prune(HH C)
E = superficialComplex(L,R)
label(E,L)
betti chainComplex E
///

TEST ///
-- This had been a bug around 0.9.95...
S = QQ[x_1..x_5];

Delta = simplicialComplex {x_1*x_2*x_3, x_2*x_4, x_3*x_4, x_5};

C = chainComplex Delta
C.dd
assert(C.dd_0 * C.dd_1 == 0)
assert(C.dd_1 * C.dd_2 == 0)
///

-----------------------------------------------------------
-- tests added by Janko

TEST ///
R = QQ[x_0..x_4]
D1 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4)
S = QQ[x_0..x_4,T]
D2 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4,T)
assert(substitute(D1,S)==D2)
///


TEST ///
R = QQ[x_0..x_4]
D1 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4)
F1=(faces(1,D1,useFaceClass=>true))#0
S = QQ[x_0..x_4,T]
D2 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4,T)
F2=(faces(1,D2,useFaceClass=>true))#0
assert(substitute(F1,S)==F2)
///

TEST ///
R = QQ[x_0..x_4]
D = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4)
F=(faces(1,D,useFaceClass=>true))#0
assert(isFaceOf(F,D))
///

TEST ///
R = QQ[x_0..x_4]
D = simplicialComplex monomialIdeal(x_0*x_1,x_2*x_3*x_4)
fc=faces(D,useFaceClass=>true)
assert(apply(-1..2,j->#fc#j)==(1,5,9,6))
///

TEST ///
R = QQ[x_0..x_4]
F = face (x_0*x_1)
G = face (x_0*x_1*x_2)
assert(isSubface(F,G))
assert(dim(F)==1);
assert(dim(G)==2);
assert(ring(F)===R)
///

TEST ///
R = QQ[x_0..x_4]
F = face (x_0*x_1)
assert(set vertices(F) === set {x_0,x_1})
///

TEST ///
R = QQ[x_0..x_4]
F = face (x_0*x_1)
assert(set vertices(F) === set {x_0,x_1})
///


TEST ///
R = QQ[a..e]
D = simplicialComplex monomialIdeal(a*b*c*d*e)
assert(D==simplicialComplex facets(D,useFaceClass =>true))
///

-------------------------------------------------------------------
-- Documentation added by Janko


doc ///
  Key
    Face
  Headline
   The class of faces of simplicial complexes.
  Description
   Text
        The class of faces of simplicial complexes on the variables of a polynomial ring.
        The faces are @TO MutableHashTable@s F with two @TO keys@
        
        F.vertices is a @TO List@ of vertices in the @TO PolynomialRing@ F.ring

   Example
     R=QQ[x_0..x_4];
     F=face {x_0,x_2}
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     D=simplicialComplex I
     fc=faces(1,D,useFaceClass=>true)
     select(fc,j->j==F)
  SeeAlso
     SimplicialComplex
     faces
     facets
///


doc ///
  Key
    (symbol ==,Face,Face)
  Headline
   Compare two faces.
  Usage
    F==G
  Inputs
    F:Face
    G:Face
  Outputs
    :Boolean
  Description
   Text
        Checks whether F and G are equal.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1}
     G1=face {x_1,x_0}
     G2=face {x_1,x_2}
     F==G1
     F==G2
  SeeAlso
     Face
     face
///


doc ///
  Key
    face
    (face,List)
    (face,List,PolynomialRing)
    (face,RingElement)
  Headline
    Generate a face.
  Usage
    face(L)
    face(L,R)
    face(m)
  Inputs
    L:List
    R:PolynomialRing
    m:RingElement
        a monomial
  Outputs
    :Face
  Description
   Text
        Generates a face out of a list L or a squarefree monomial.
        If L is not empty or a monomial the argument R is not required.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1}
  SeeAlso
     SimplicialComplex
     faces
     facets
///

doc ///
  Key
    (dim,Face)
  Headline
    The dimension of a face.
  Usage
    dimension(F)
  Inputs
    F:Face
  Outputs
    :ZZ
      bigger or equal to -1
  Description
   Text
        Returns the dimension of a @TO Face@, i.e., the number of @TO vertices@ F minus 1.

   Example
     K=QQ;
     R=K[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     D=simplicialComplex I
     fc=faces(D,useFaceClass=>true)
     apply(-1..1, j->apply(fc#j,dim))
  SeeAlso
     face
     facets
     faces
///

doc ///
  Key
    vertices
    (vertices,Face)
  Headline
    The vertices of a face of a simplicial complex.
  Usage
    vertices(F)
  Inputs
    F:Face
  Outputs
    :List
  Description
   Text
        Returns a @TO List@ with the vertices of a @TO Face@ of a simplicial complex.

   Example
     K=QQ;
     R=K[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     D=simplicialComplex I
     fc=facets(D,useFaceClass=>true)
     vertices fc#1
  SeeAlso
     face
     facets
     faces
///

doc ///
  Key
    isSubface
    (isSubface,Face,Face)
  Headline
    Test whether a face is a subface of another face.
  Usage
    isSubface(F,G)
  Inputs
    F:Face
    G:Face
  Outputs
    :Boolean
  Description
   Text
        Test whether F is a subface of G.

   Example
     K=QQ;
     R=K[x_0..x_4];
     G=face {x_0,x_1,x_2}
     F1=face {x_0,x_2}
     F2=face {x_0,x_3}
     isSubface(F1,G)
     isSubface(F2,G)
///

doc ///
  Key
    (substitute,Face,PolynomialRing)
  Headline
    Substitute a face to a different ring.
  Usage
    substituteFace(F,R)
  Inputs
    F:Face
    R:PolynomialRing
  Outputs
    :Face
  Description
   Text
        Substitute a face to a different ring.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1,x_2}
     S=R**K[y]
     substitute(F,S)
///

doc ///
  Key
    (ring,Face)
  Headline
    Ring of a face.
  Usage
    ring(F)
  Inputs
    F:Face
  Outputs
    :Ring
  Description
   Text
        Ring of a face.

   Example
     K=QQ;
     R=K[x_0..x_4];
     F=face {x_0,x_1,x_2}
     ring F
///


doc ///
  Key
    (substitute,SimplicialComplex,PolynomialRing)
  Headline
    Substitute a simplicial complex to a different ring.
  Usage
    substitute(C,R)
  Inputs
    C:SimplicialComplex
    R:PolynomialRing
  Outputs
    :SimplicialComplex
  Description
   Text
        Substitute a simplicial complex to a different ring. R should contain the variables of the @TO ring@ of C.

   Example
     K=QQ;
     R=K[x_0..x_4];
     I=monomialIdeal(x_0*x_1,x_1*x_2,x_2*x_3,x_3*x_4,x_4*x_0);
     C=simplicialComplex I
     S=R**K[y]
     C1=substitute(C,S)
     ring C1
  SeeAlso
     (substitute,Face,PolynomialRing)
///

doc ///
  Key
    isFaceOf
    (isFaceOf,Face,SimplicialComplex)
  Headline
    Substitute a face to a different ring.
  Usage
    substitute(F,R)
  Inputs
    F:Face
    R:PolynomialRing
  Outputs
    :Face
  Description
   Text
        Substitute a face to a different ring.

   Example
     K=QQ;
     R=K[x_1..x_5];
     C=simplicialComplex monomialIdeal (x_1*x_2,x_3*x_4*x_5)
     F1=face {x_1,x_2}
     F2=face {x_1,x_3}
     isFaceOf(F1,C)
     isFaceOf(F2,C)
///

doc ///
  Key
    (net,Face)
  Headline
    Printing a face.
  Usage
    net(F)
  Inputs
    F:Face
  Outputs
    :Net
  Description
   Text
        Prints a face. The vertices are printed without any brackets and with one space between them. Also prints the polynomial ring which contains the vertices.

   Example
     K=QQ;
     R=K[x_0..x_4];
     face {x_0,x_1}
///

doc ///
  Key
    useFaceClass
    [faces,useFaceClass]
    [facets,useFaceClass]
  Headline
    Option to return faces in the class Face
  Description
   Text
    @TO Boolean@ @TO Option@ to return in the methods @TO faces@ and @TO facets@ a @TO List@ of @TO Face@s instead of a @TO Matrix@.
///

doc ///
  Key
    (faces,SimplicialComplex)
  Headline
    Compute all faces of a simplicial complex.
  Usage
    faces(C)
  Inputs
    C:SimplicialComplex
  Outputs
    :MutableHashTable
  Description
   Text
        Return a list of lists of the faces of a simplicial complex.

   Example
    K=QQ;
    R=K[x_1..x_5];
    C=simplicialComplex monomialIdeal (x_1*x_2,x_3*x_4*x_5)
    fc=faces(C,useFaceClass=>true)
    fc#2
 ///


-------------------------------------------------------------------
-- some previously missing documentation

doc ///
  Key
    (net,SimplicialComplex)
  Headline
    Printing a simplicial complex.
  Usage
    net(C)
  Inputs
    C:SimplicialComplex
  Outputs
    :Net
  Description
   Text
        Prints a simplicial complex. 

   Example
    K=QQ;
    R=K[x_1..x_5];
    C=simplicialComplex monomialIdeal (x_1*x_2,x_3*x_4*x_5)
///

doc ///
  Key
    (symbol ==,SimplicialComplex,SimplicialComplex)
  Headline
   Compare two simplicial complexes.
  Usage
    C1==C2
  Inputs
    C1:SimplicialComplex
    C2:SimplicialComplex
  Outputs
    :Boolean
  Description
   Text
        Checks whether C1 and C2 are equal.

   Example
    K=QQ;
    R=K[x_1..x_3];
    C1=simplicialComplex monomialIdeal (x_1*x_2*x_3)
    C2=simplicialComplex {face {x_1,x_2},face {x_2,x_3},face {x_3,x_1}}
    C1==C2
///

doc ///
  Key
    faceIdeal
  Headline
    Key to simplicial complex.
  Description
   Text
      This is a @TO Key@ to a @TO SimplicialComplex@ C storing its Stanley-Reisner ideal, which will be returned by C.faceIdeal.

      The Stanley-Reisner ideal of C can also be obtained by @TO ideal@ C.

   Example
    K=QQ;
    R=K[x_1..x_5];
    C=simplicialComplex monomialIdeal (x_1*x_2,x_3*x_4*x_5)
    C.faceIdeal
    ideal C
  SeeAlso
   (ideal,SimplicialComplex)
///



doc ///
  Key    
    (chainComplex,SimplicialComplex)
    simplicialChainComplex
    (simplicialChainComplex,List,SimplicialComplex)
  Headline
    The chain complex of boundary maps.
  Usage
    chainComplex C
  Inputs
    C:SimplicialComplex
  Outputs
    :ChainComplex
  Description
   Text
     The @TO ChainComplex@ of @TO boundary@ maps from i-faces to (i-1)-faces.

   Example
    R = QQ[a..f];
    D = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f);
    R' = ZZ/2[a..f];
    D' = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f);
    c = chainComplex D
    c' = chainComplex D'
    c.dd_1
    c'.dd_1
  SeeAlso
   boundary
///


doc ///
  Key    
    (homology,ZZ,SimplicialComplex)
  Headline
    Compute the homology of a simplicial complex.
  Usage
    homology(j,C)
  Inputs
    j:ZZ
    C:SimplicialComplex
  Outputs
    :Module
  Description
   Text
     Compute the j-th reduced homology of C with coefficients in @TO (coefficientRing,SimplicialComplex)@ C.

   Example
    R=ZZ[x_0..x_5];
    D=simplicialComplex apply({{x_0, x_1, x_2}, {x_1, x_2, x_3}, {x_0, x_1, x_4}, {x_0, x_3, x_4}, {x_2, x_3, x_4}, {x_0, x_2, x_5}, {x_0, x_3, x_5}, {x_1, x_3, x_5}, {x_1, x_4, x_5}, {x_2, x_4, x_5}},face)
    prune homology(1,D)
  SeeAlso
    (homology,ZZ,SimplicialComplex,Ring)
   boundary
   (chainComplex,SimplicialComplex)
///

doc ///
  Key    
    (homology,ZZ,SimplicialComplex,Ring)
  Headline
    Compute the homology of a simplicial complex.
  Usage
    homology(j,C,R)
  Inputs
    j:ZZ
    C:SimplicialComplex
    R:Ring
  Outputs
    :Module
  Description
   Text
     Compute the j-th reduced homology of C with coefficients in R.

   Example
    R=ZZ[x_0..x_5];
    D=simplicialComplex apply({{x_0, x_1, x_2}, {x_1, x_2, x_3}, {x_0, x_1, x_4}, {x_0, x_3, x_4}, {x_2, x_3, x_4}, {x_0, x_2, x_5}, {x_0, x_3, x_5}, {x_1, x_3, x_5}, {x_1, x_4, x_5}, {x_2, x_4, x_5}},face)
    prune homology(1,D,ZZ)
    prune homology(1,D,QQ)
    prune homology(1,D,ZZ/2)
  SeeAlso
    (homology,ZZ,SimplicialComplex)
   boundary
   (chainComplex,SimplicialComplex)
///

doc ///
  Key    
    (homology,SimplicialComplex,Ring)
    (homology,Nothing,SimplicialComplex,Ring)
  Headline
    Compute the homology of a simplicial complex.
  Usage
    homology(C,R)
  Inputs
    C:SimplicialComplex
    R:Ring
  Outputs
    :GradedModule
  Description
   Text
     The graded module of reduced homologies of C with coefficients in R.

   Example
    R=ZZ[x_0..x_5];
    D=simplicialComplex apply({{x_0, x_1, x_2}, {x_1, x_2, x_3}, {x_0, x_1, x_4}, {x_0, x_3, x_4}, {x_2, x_3, x_4}, {x_0, x_2, x_5}, {x_0, x_3, x_5}, {x_1, x_3, x_5}, {x_1, x_4, x_5}, {x_2, x_4, x_5}},face)
    homology(D)
    homology(D,QQ)
    homology(D,ZZ/2)
  SeeAlso
    (homology,SimplicialComplex)
    (homology,ZZ,SimplicialComplex)
    (homology,ZZ,SimplicialComplex,Ring)
///

doc ///
  Key    
    (homology,SimplicialComplex)
    (homology,Nothing,SimplicialComplex)
  Headline
    Compute the homology of a simplicial complex.
  Usage
    homology C
  Inputs
    C:SimplicialComplex
  Outputs
    :GradedModule
  Description
   Text
     The graded module of reduced homologies of C with coefficients in R.

   Example
    R=ZZ[x_0..x_5];
    D=simplicialComplex apply({{x_0, x_1, x_2}, {x_1, x_2, x_3}, {x_0, x_1, x_4}, {x_0, x_3, x_4}, {x_2, x_3, x_4}, {x_0, x_2, x_5}, {x_0, x_3, x_5}, {x_1, x_3, x_5}, {x_1, x_4, x_5}, {x_2, x_4, x_5}},face)
    homology D
  SeeAlso
    (homology,SimplicialComplex,Ring)
    (homology,ZZ,SimplicialComplex)
    (homology,ZZ,SimplicialComplex,Ring)
///



-------------------------------------------------------------------

-*
check ("SimplicialComplexes",UserMode=>false)
loadPackage "SimplicialComplexes"
installPackage "SimplicialComplexes"
installPackage("SimplicialComplexes",RerunExamples=>true,UserMode=>false)
*-

-----------------------------------------------------------

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SimplicialComplexes pre-install"
-- End:
