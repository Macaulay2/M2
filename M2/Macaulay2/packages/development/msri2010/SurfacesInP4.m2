needsPackage "BGG"
newPackage(
        "SurfacesInP4",
        Version => "0.1", 
        Date => "January 8, 2010",
        Authors => {
	     {Name => "Hans-Christian Graf v. Bothmer", Email => "bothmer@math.uni-hannover.de", HomePage => "http://www.crcg.de/wiki/Hans-Christian_Graf_v._Bothmer"},
	     {Name => "Florian Geiss", Email => "fg@math.uni-sb.de", HomePage => "http://www.math.uni-sb.de/ag/schreyer/fg/"}
	     },
        Headline => "Construction of surfaces in P4",
        DebuggingMode => true
        )
needsPackage "BGG"

--- TODO:
---
--- * maybe we should use the type "CohomolgyTable" from BBG???
--- * make construcSurface a Method
--- * automatically find good windows
--- * figure out how to automatically do tests
--- * check many more surfaces
--- * put real roots into a separate file
--- * fix BBG in the case where beilinson returns an empty matrix
--- * check smoothness?
--- * adjunction?

export {
     hilbertPolynomialFromInvariants,
     guessCohomologyTable,
     diag,
     guessDifferentials,
     constructSurface,
     numPosRoots
     }

----
----
---- WE NEED TO FIX THIS....

--realroots.m2
--
-- Written by Dan Grayson & Frank Sottile
--
-- The purpose of this Macaulay2 package is to provide tools
-- for elimination and solving, with a particular emphasis on 
-- counting and isolating real zeroes of ideals in QQ[X]
---------------------------------------------------------------------------
--   Documentation
---------------------------------------------------------------------------
--eliminant(h, C) computes the eliminant an element h of an Artinian ring 
   --             as the minimal polynomial of h by finding a minimal linear
   --             relation among the powers of h.  C is a polynomial ring
   --             in one variable Z and the eliminant is in C
   --------------------------------------------------------------------------
--eliminantNaive(h, Z) Computes the eliminant of an element h of an Artinian 
   --                  ring naively as a kernel, returning a polynomial in Z
   --------------------------------------------------------------------------
--regularRep(f) Computes the matrix of multiplication by an element f
   --           of an Artinian ring 
   --------------------------------------------------------------------------
--charPoly(h, Z) Computes the characteristic polynomial of multiplication
   --            by an element h  an Artinian ring, returning a polynomial 
   --            in Z; this is the eliminant when the ideal is reduced
   --------------------------------------------------------------------------
--SturmSequence(f) Computes the Sturm sequence of a univariate polynomial f
   --------------------------------------------------------------------------
--sign(n) Computes the sign of a number
   --------------------------------------------------------------------------
--signAtMinusInfinity(g) Computes the sign of g(-infinity), g is univariate
   --                    with real coefficients
   --------------------------------------------------------------------------
--signAtZero(g) Computes the sign of g(0), g is univariate with real coefficients
   --------------------------------------------------------------------------
--signAtInfinity(g) Computes the sign of g(-infinity), g is univariate
   --               with real coefficients
   --------------------------------------------------------------------------
--numRealSturm(f) Uses Sturm sequences to compute the number of real roots
   --             of a polynomial f with real coefficients
   --------------------------------------------------------------------------
--numPosRoots(f) Uses Sturm sequences to compute the number of positive real
   --            roots of a polynomial f with real coefficients
   --------------------------------------------------------------------------
--numNegRoots(f) Uses Sturm sequences to compute the number of negative real
   --            roots of a polynomial f with real coefficients
   --------------------------------------------------------------------------
--variations(c) Computes the number of changes of sign in a sequence c
   --------------------------------------------------------------------------
--traceForm(h) Computes the trace quadratic form of an element h in an 
   --          Artinian ring
   --------------------------------------------------------------------------
--traceFormSignature(h) Computes the rank and signature of the trace quadratic 
   --                   form of an element h in an Artinian ring of characteristic 0
   --------------------------------------------------------------------------
--numRealTrace(A) Computes the number of real points in Spec(A), where 
   --             A is an Artinian ring with characteristic zero
----------------------------------------------------------------------------
--   Routines
----------------------------------------------------------------------------
--eliminant(h, C) computes the eliminant an element h of an Artinian ring 
   --             as the minimal polynomial of h by finding a minimal linear
   --             relation among the powers of h.  C is a polynomial ring
   --             in one variable Z and the eliminant is in C
   --
eliminant = (h, C) -> (
     Z := C_0;
     A := ring h;
     assert( dim A == 0 );
     F := coefficientRing A;
     assert( isField F );
     assert( F === coefficientRing C );
     B := basis A;
     d := numgens source B;
     M := fold((M, i) -> M || 
	       substitute(contract(B, h^(i+1)), F), 
               substitute(contract(B, 1_A), F), 
	       flatten subsets(d, d));
     N := ((ker transpose M)).generators;
     P := matrix {toList apply(0..d, i -> Z^i)} * N;
          (flatten entries(P))_0)
---------------------------------------------------------------------------
--eliminantNaive(h, Z) Computes the eliminant of an element h of an Artinian 
   --                  ring naively as a kernel, returning a polynomial in Z
   --
eliminantNaive = (h, Z) -> (
     A := ring h;
     assert( dim A == 0 );
     F := coefficientRing A;
     assert( isField F );
     (ker(map(A, F[Z], {h})))_0 
     )
--------------------------------------------------------------------------
--regularRep(f) Computes the matrix of multiplication by an element f
   --           of an Artinian ring 
   --
regularRep = f -> (
     assert( dim ring f == 0 );
     b := basis ring f;
     k := coefficientRing ring f;
     substitute(contract(transpose b, f*b), k))
--------------------------------------------------------------------------
--charPoly(h, Z) Computes the characteristic polynomial of multiplication
   --            by an element h  an Artinian ring, returning a polynomial 
   --            in Z; this is the eliminant when the ideal is reduced
   --
charPoly = (h, Z) -> (
     A := ring h;
     F := coefficientRing A;
     S := F[Z];
     Z  = value Z;     
     mh := regularRep(h) ** S;
     Idz := S_0 * id_(S^(numgens source mh));
     det(Idz - mh))
-----------------------------------------------------------------------
--SturmSequence(f) Computes the Sturm sequence of a univariate polynomial f
   --
SturmSequence = f -> (
     assert( isPolynomialRing ring f );
     assert( numgens ring f === 1 );
     R := ring f;
     assert( char R == 0 );
     x := R_0;
     n := first degree f;
     c := new MutableList from toList (0 .. n);
     if n >= 0 then (
     	  c#0 = f;
          if n >= 1 then (
               c#1 = diff(x,f);
               scan(2 .. n, i -> c#i = - c#(i-2) % c#(i-1));
               ));
     toList c)
--------------------------------------------------------------------------
--sign(n) Computes the sign of a number
   --
sign := n -> (if n < 0 then -1 
              else if n == 0 then 0
	      else if n > 0 then 1);
--------------------------------------------------------------------------
--signAtMinusInfinity(g) Computes the sign of g(-infinity), g is univariate
   --                    with real coefficients
   --
signAtMinusInfinity := g -> sign((if odd first degree g 
	                          then -1 else 1) * 
			         leadCoefficient g)
--------------------------------------------------------------------------
--signAtZero(g) Computes the sign of g(0), g is univariate with real coefficients
   --
signAtZero := g -> (
                    R := ring g;
                    sign ( g _ (1_R) ))
--------------------------------------------------------------------------
--signAtInfinity(g) Computes the sign of g(-infinity), g is univariate
   --               with real coefficients
   --
signAtInfinity := g -> sign leadCoefficient g; 
--------------------------------------------------------------------------
--numRealSturm(f) Uses Sturm sequences to compute the number of real roots
   --             of a polynomial f with real coefficients
   --
numRealSturm = f -> (
     c := SturmSequence f;
     variations (signAtMinusInfinity \ c) 
         - variations (signAtInfinity \ c))
---------------------------------------------------------------------------
--numPosRoots(f) Uses Sturm sequences to compute the number of positive real
   --            roots of a polynomial f with real coefficients
   --
numPosRoots = f -> (  
     c := SturmSequence f;
     variations (signAtZero \ c) 
         - variations (signAtInfinity \ c))
--------------------------------------------------------------------------
--numNegRoots(f) Uses Sturm sequences to compute the number of negative real
   --            roots of a polynomial f with real coefficients
   --
numNegRoots = f -> ( 
     c := SturmSequence f;
     variations (signAtMinusInfinity \ c) 
         - variations (signAtZero \ c))
--------------------------------------------------------------------------
--variations(c) Computes the number of changes of sign in a sequence c
   --
variations = c -> (
     n := 0;
     last := 0;
     scan(c, x -> if x != 0 then (
	       if last < 0 and x > 0 or last > 0 
	          and x < 0 then n = n+1;
	       last = x;
	       ));
     n)
--------------------------------------------------------------------------
--traceForm(h) Computes the trace quadratic form of an element h in an 
   --          Artinian ring
   --
traceForm = h -> (
     assert( dim ring h == 0 );
     b  := basis ring h;
     k  := coefficientRing ring h;
     mm := substitute(contract(transpose b, h * b ** b), k);
     tr := matrix {apply(first entries b, x ->
	       trace regularRep x)};
     adjoint(tr * mm, source tr, source tr))
--------------------------------------------------------------------------
--traceFormSignature(h) Computes the rank and signature of the trace quadratic 
   --                   form of an element h in an Artinian ring of characteristic 0
   --
traceFormSignature = h -> (
     A := ring h;
     assert( dim A == 0 );
     assert( char A == 0 );
     S := QQ[Z];
     TrF := traceForm(h) ** S;
     IdZ := Z * id_(S^(numgens source TrF));
     f := det(TrF - IdZ);
     << "The trace form S_h with h = " << h << 
       " has rank " << rank(TrF) << " and signature " << 
       numPosRoots(f) - numNegRoots(f) << endl; )
--------------------------------------------------------------------------
--numRealTrace(A) Computes the number of real points in Spec(A), where 
   --             A is an Artinian ring with characteristic zero
   --
numRealTrace = A -> (
     assert( dim A == 0 );
     assert( char A == 0 );
     S := QQ[Z];
     TrF := traceForm(1_A) ** S;
     IdZ := Z * id_(S^(numgens source TrF));
     f := det(TrF - IdZ);
     numPosRoots(f)-numNegRoots(f))
--------------------------------------------------------------------------


----
----
----

hilbertPolynomialFromInvariants = (deg, sectionalGenus, speciality, geometricGenus) -> (
     -- to get the ring in which hilbertPolynomials are usually expressed
     i:=(vars ring hilbertPolynomial(ZZ[ttt], Projective => false))_(0,0); 
     (i+4)*(i+3)*(i+2)*(i+1)/24 - (i+1)*i/2 * deg + i*(sectionalGenus-1) - 1 + speciality - geometricGenus
     )

guessCohomologyTable = method()
guessCohomologyTable(RingElement,ZZ,ZZ) := (hilbPoly, lo,hi) -> (
    iv:= (vars ring hilbPoly)_(0,0);
    M := mutableMatrix(ZZ,5,hi-lo+1);
    apply(lo..hi, j -> M_(4- numPosRoots(sub(H, matrix{{iv + j}}) ),j-lo) = abs sub(sub(hilbPoly,matrix{{j*1_QQ}}),ZZ));
    matrix M)

-- calculate the free E-Modules corresponding to a diagonal in the
-- cohomology table
diag = (M,k,E)->E^(flatten toList apply(max(k,0)..min(k+4,4),j->toList(M_(j-k,j):(j-4))))


-- guess the differenials
guessDifferentials = (cohTable,E) -> (
     -- test if the cohTable is square 5x5
     if (rank source cohTable,rank target cohTable) != (5,5)
     	  then error "The cohomology Table must be square 5x5";
     -- test if the cohTable gives a monad
     if apply({-4,-3,-2,2,3,4},k->diag(cohTable,k,E)==0) != toList(6:true)
	  then error "The cohomology Table must be 0 ouside of the 3 central diagonals";
     -- OK
     -- start with generic beta
     beta := random(diag(cohTable,1,E),diag(cohTable,0,E));
     -- find alpha among the syzygies
     sbeta := syz beta;
     betti (alpha := sbeta*random(source sbeta,diag(cohTable,-1,E)));
     chainComplex{beta,alpha}
     )

---
--- This must be made a Method !!!!
---

-- construct a surface from a monad over the exterior algebra
constructSurface = method()
constructSurface(ChainComplex,PolynomialRing):=(monadE,S) -> (
     -- apply BBG to get the beilinson Monad
     alphaBeil := beilinson(monadE.dd_2,S);
     betaBeil := beilinson(monadE.dd_1,S);
     --
     -- BBG does not give correctly graded 0-Matrices!!!!
     --
     if alphaBeil == 0 then alphaBeil = map(source betaBeil,S^{},0);
     if betaBeil == 0 then betaBeil = map(S^{},target alphaBeil, 0);
     --
     -- !!!!!!!!
     --
     -- test if it is a monad
     if betaBeil*alphaBeil != 0 then error "betaBeil*alphaBeil != 0";
     -- is beta surjective?
     if codim coker betaBeil < 5 then error "beta is not surjective";
     -- is alpha injektive?
     if ker alphaBeil != 0 then error "alpha is not injective";
     -- the Ideal of the Surface is the homology of the Monad     
     I := prune homology(betaBeil,alphaBeil);
     betti (fI := res I);  
     betti (fphi := res coker transpose fI.dd_1);
     saturate ideal fphi.dd_2
     )
constructSurface(Matrix,PolynomialRing,PolynomialRing) := (M,E,S) -> (
     constructSurface(guessDifferentials(M,E),S)
     )
  
     
beginDocumentation()

doc ///
Key
  SurfacesInP4
Headline 
  Construction of surfaces in P4
///



 
doc ///
Key 
  hilbertPolynomialFromInvariants
Headline 
  the Hilbert polynomial from a surface in P4 with given invariants
Usage 
  h = hilbertPolynomialFromInvariants(d,pi,q,pg)
Inputs 
  d:ZZ 
    the degree of the surface
  pi:ZZ 
    the sectional genus of the surface
  q:ZZ
    the speciality of the surface
  pg:ZZ
    the geometric genus of the surface
Outputs
  h:RingElement
    The Hilbert polynomial of the surface
Description
  Text
  Example
    hilbertPolynomialFromInvariants(8,5,1,0)
Caveat
  This function only works for surfaces in P4
SeeAlso
///

doc ///
Key
  guessCohomologyTable
Headline
  find a plausible cohomology table in a given range from a given Hilbert polynomial of a surface in P4 
Usage
  M=guessCohomologyTable(H,Low,High)
Inputs
  H:RingElement
    the Hilbert polynomial of the variety
  Low:ZZ
    lower bound for j
  High:ZZ
    upper bound for j
Outputs
  M:Matrix
    expected cohomology table. M_{(4-i,j-Low)}=h^i(P^4,I_X(j)). 
Consequences
Description
  Text
  Example
    H = hilbertPolynomialFromInvariants(8,5,1,0)
    M = guessCohomologyTable(H,-1,4)
Caveat
  works also for other varieties in P4
SeeAlso
///

doc ///
Key
  guessDifferentials
Headline
  try to construct a monad of the exterior algebra from a given cohomology table
Usage
  monadE = guessDifferential(M,E)
Inputs
  M:Matrix
    a square 5x5 window of the cohomology table of the surface
  E:PolynomialRing
    Exterior Algebra in 5 variables
Outputs
  monadE:ChainComplex
         a chain Complex over E whose grading matches the given cohomology table 
Consequences
Description
  Text
  Example
    K = ZZ/32003
    E = K[e_0..e_4,SkewCommutative=>true]
    S = K[x_0..x_4]
    H = hilbertPolynomialFromInvariants(8,5,1,0)
    M = guessCohomologyTable(H,-1,3)
    monadE = guessDifferentials(M,E)
    betti monadE
Caveat
  works only when the cohomology table gives a Monad
  A -alpha-> B -beta-> C, 
  i.e when only the 3 diagonals in the middle have values. Furthermore
  beta is chosen generically and alpha generically among the syzygies of beta.
  There are many surfaces where the monad has to be chosen more carefully.
SeeAlso
///

doc ///
Key 
  constructSurface
Headline 
  try to construct a surface
///

doc ///
Key
  (constructSurface,ChainComplex,PolynomialRing)
Headline
  try to construct a surface from a given monad over the Exterior Algebra
Usage
  I=constructSurface(C,S)
Inputs
  C:ChainComplex
    a monad over the exterior algebra
  S:PolynomialRing
    Symmetric Algebra in 5 variables
Outputs
  I:Ideal
    ideal of a surface with the given monad as part of the tate resolution
Consequences
Description
  Text
  Example
    K = ZZ/32003
    E = K[e_0..e_4,SkewCommutative=>true]
    S = K[x_0..x_4]
    H = hilbertPolynomialFromInvariants(8,5,1,0)
    M = guessCohomologyTable(H,-1,3)
    monadE = guessDifferentials(M,E)
    betti res constructSurface(monadE,S)
Caveat
     this only gives a surface if the monad is carefully chosen.
SeeAlso
     guessDifferentials
///

doc ///
Key
  (constructSurface,Matrix,PolynomialRing,PolynomialRing)
Headline
  try to construct a surface from a given cohomology table
Usage
  I=constructSurface(M,E,S)
Inputs
  M:Matrix
    a square window of the cohomology table of the surface
  E:PolynomialRing
    Exterior Algebra in 5 variables
  S:PolynomialRing
    Symmetric Algebra in 5 variables
Outputs
  I:Ideal
    ideal of a surface with the given cohomology table 
Consequences
Description
  Text
  Example
    K = ZZ/32003
    E = K[e_0..e_4,SkewCommutative=>true]
    S = K[x_0..x_4]
    H = hilbertPolynomialFromInvariants(8,5,1,0)
    M = guessCohomologyTable(H,-1,3)
    betti res constructSurface(M,E,S)
Caveat
  works only when the cohomology table gives a Beilinson Monad
  A -alpha-> B -beta-> C, 
  i.e when only the 3 diagonals in the middle have values. Furthermore
  beta is chosen generically and alpha generically among the syzygies of beta.
  There are many surfaces where this does not work.
SeeAlso
///

TEST ///
H = hilbertPolynomialFromInvariants(8,5,1,0)
i = (vars ring H)_(0,0);
assert(H ==  1/24 *( i^4 + 10 *i^3 - 61*i^2 + 50 *i) + 1)
///

TEST /// 
H = hilbertPolynomialFromInvariants(8,5,1,0);
assert(guessCohomologyTable(H,-1,4) == matrix{{0,0,0,0,0,0},{4,0,0,0,0,0},{0,1,1,0,0,0},{0,0,0,1,1,0},{0,0,0,0,0,6}})
///

TEST /// 
H = hilbertPolynomialFromInvariants(8,5,1,0);
Low=-1;
High=4;
M=guessCohomologyTable(H,Low,High);
assert( M_(4-3,-1-Low) == 4)
///

TEST ///
-- Abo's degree 8 surface
K = ZZ/32003
E = K[e_0..e_4,SkewCommutative=>true]
S = K[x_0..x_4]
H = hilbertPolynomialFromInvariants(8,5,1,0)
M = guessCohomologyTable(H,-1,3)
monadE = guessDifferentials(M,E)
expectedBetti = new BettiTally from {(0,{0},0) => 1, (1,{4},4) => 6, (2,{5},5) => 4, (2,{6},6)
      => 5, (3,{6},6) => 1, (3,{7},7) => 4, (4,{8},8) => 1}
assert(betti res constructSurface(monadE,S) == expectedBetti)
///

TEST ///
-- The rational normal scroll
K = ZZ/32003
E = K[e_0..e_4,SkewCommutative=>true]
S = K[x_0..x_4]
H = hilbertPolynomialFromInvariants(3,0,0,0)
M = guessCohomologyTable(H,-2,2)
monadE = guessDifferentials(M,E)
expectedBetti = new BettiTally from {(0,{0},0) => 1, (1,{2},2) => 3, (2,{3},3) => 2}
assert(betti res constructSurface(monadE,S) == expectedBetti)
///
