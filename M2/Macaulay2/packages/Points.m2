-- -*- coding: utf-8 -*-
newPackage(
	"Points",
    	Version => "2.0", 
    	Date => "29 June 2008, revised by DE June 2016",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.uiuc.edu/Macaulay2/"},
     	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca"},
	     {Name => "Stein A. Strømme", Email => "stromme@math.uib.no"},
	     {Name => "David Eisenbud", Email => "de@msri.org"}	     
	     },
    	Headline => "computing with sets of points",
	PackageExports => {"LexIdeals"},
    	DebuggingMode => false
    	)

export {
--   Points in affine space
     "affinePointsMat",
     "affinePoints",
     "affinePointsByIntersection",
     "affineMakeRingMaps",
---------     
--    points in projective space
     "randomPointsMat",
     "AllRandom",
     "points",
     "randomPoints",
     "omegaPoints",
     "expectedBetti",
     "minMaxResolution"
     }

///
restart
loadPackage("Points", Reload=>true)
randomPointsMat
omegaPoints
///


affineMakeRingMaps = method (TypicalValue => List)
affineMakeRingMaps (Matrix, Ring) := List => (M,R) -> (
     K := coefficientRing R;
     pts := entries transpose M;
     apply(pts, p -> map(K, R, p))
     )

addNewMonomial = (M,col,monom,maps) -> (
     -- M is an s by s+1 matrix, s=#points
     -- monom is a monomial
     -- maps is a list of s ring maps, which will give the values
     --  of the monom at the points
     -- replaces the 'col' column of M with the values of monom
     --    at the s points.
     scan(#maps, i -> M_(i,col) = maps#i monom)
     )

affinePointsByIntersection = method(TypicalValue => List)
affinePointsByIntersection (Matrix,Ring) := (M,R) -> (
     flatten entries gens gb intersect apply (
       entries transpose M, p -> ideal apply(#p, i -> R_i - p#i)))

reduceColumn = (M,Mchange,H,c) -> (
     -- M is a mutable matrix
     -- Mchange is either null, or a matrix with same number of columns as M
     -- H is a hash table: H#r == c if column c has pivot for row r 
     -- returns true if the element reduces to 0
     r := numRows M - 1;
     while r >= 0 do (
	  a := M_(r,c);
	  if a != 0 then (
	       -- is there a pivot?
	       if not H#?r then (
		    b := 1/a; -- was 1//a
		    columnMult(M, c, b);
		    if Mchange =!= null then columnMult(Mchange, c, b);
		    H#r = c;
		    return false;
		    )
	       else (
	       	    pivotc := H#r;
	       	    columnAdd(M, c, -a, pivotc);
		    if Mchange =!= null then columnAdd(Mchange, c, -a, pivotc);
	       ));
     	  r = r-1;
	  );
     true
     )

affinePointsMat = method()
affinePointsMat(Matrix,Ring) := (M,R) -> (
     -- The columns of M form the points.  M should be a matrix of size
     -- n by s, where n is the number of variables of R
     --
     K := coefficientRing R;
     s := numgens source M;
     -- The local data structures:
     -- (P,PC) is the matrix which contains the elements to be reduced
     -- Fs is used to evaluate monomials at the points
     -- H is a hash table used in Gaussian elimination: it contains the
     --    pivot columns for each row
     -- L is the sum of monomials which is still to be done
     -- Lhash is a hashtable: Lhash#monom = i means that only 
     --    R_i*monom, ..., R_n*monom should be considered
     -- G is a list of GB elements
     -- inG is the ideal of initial monomials for the GB
     Fs := affineMakeRingMaps(M,R);
     P := mutableMatrix map(K^s, K^(s+1), 0);
     H := new MutableHashTable; -- used in the column reduction step
     Lhash := new MutableHashTable; -- used to determine which monomials come next
     L := 1_R;
     Lhash#L = 0; -- start with multiplication by R_0
     thiscol := 0;
     inG := trim ideal(0_R);
     inGB := forceGB gens inG;
     Q := {}; -- the list of standard monomials
     --ntimes := 0;
     while (L = L % inGB) != 0 do (
	  --ntimes = ntimes + 1;
	  --if #Q === s then print "got a basis";
	  --print("size of L = "| size(L));
	  -- First step: get the monomial to consider
	  monom := someTerms(L,-1,1);
	  L = L - monom;
	  -- Now fix up the matrix P
          addNewMonomial(P,thiscol,monom,Fs);
          isLT := reduceColumn(P,null,H,thiscol);
	  if isLT then (
	       -- we add to G, inG
	       inG = inG + ideal(monom);
	       inGB = forceGB gens inG;
	       )
	  else (
	       -- we modify L, Lhash, thiscol, and also PC
	       Q = append(Q, monom);
	       L = L + sum apply(toList(Lhash#monom .. numgens R - 1), i -> (
			 newmon := monom * R_i;
			 Lhash#newmon = i;
			 newmon));
	       thiscol = thiscol + 1;
	       )
	  );
     --print("ntimes "|ntimes|" std+inG "|#Q + numgens inG);
     stds := transpose matrix{Q};
     A := transpose matrix{apply(Fs, f -> f stds)};
     (A, stds)
     )

affinePoints = method()
affinePoints (Matrix,Ring) := (M,R) -> (
     -- The columns of M form the points.  M should be a matrix of size
     -- n by s, where n is the number of variables of R
     K := coefficientRing R;
     s := numgens source M;
     -- The local data structures:
     -- (P,PC) is the matrix which contains the elements to be reduced
     -- Fs is used to evaluate monomials at the points
     -- H is a hash table used in Gaussian elimination: it contains the
     --    pivot columns for each row
     -- L is the sum of monomials which is still to be done
     -- Lhash is a hashtable: Lhash#monom = i means that only 
     --    R_i*monom, ..., R_n*monom should be considered
     -- G is a list of GB elements
     -- inG is the ideal of initial monomials for the GB
     Fs := affineMakeRingMaps(M,R);
     P := mutableMatrix map(K^s, K^(s+1), 0);
     PC := mutableMatrix map(K^(s+1), K^(s+1), 0);
     for i from 0 to s-1 do PC_(i,i) = 1_K;
     H := new MutableHashTable; -- used in the column reduction step
     Lhash := new MutableHashTable; -- used to determine which monomials come next
     L := 1_R;
     Lhash#L = 0; -- start with multiplication by R_0
     thiscol := 0;
     G := {};
     inG := trim ideal(0_R);
     inGB := forceGB gens inG;
     Q := {}; -- the list of standard monomials
     nL := 1;
     while L != 0 do (
	  -- First step: get the monomial to consider
	  L = L % inGB;
	  monom := someTerms(L,-1,1);
	  L = L - monom;
	  -- Now fix up the matrices P, PC
          addNewMonomial(P,thiscol,monom,Fs);
	  columnMult(PC, thiscol, 0_K);
	  PC_(thiscol,thiscol) = 1_K;
          isLT := reduceColumn(P,PC,H,thiscol);
	  if isLT then (
	       -- we add to G, inG
	       inG = inG + ideal(monom);
	       inGB = forceGB gens inG;
	       g := sum apply(toList(0..thiscol-1), i -> PC_(i,thiscol) * Q_i);
	       G = append(G, PC_(thiscol,thiscol) * monom + g);
	       )
	  else (
	       -- we modify L, Lhash, thiscol, and also PC
	       Q = append(Q, monom);
	       f := sum apply(toList(Lhash#monom .. numgens R - 1), i -> (
			 newmon := monom * R_i;
			 Lhash#newmon = i;
			 newmon));
	       nL = nL + size(f);
	       L = L + f;
	       thiscol = thiscol + 1;
	       )
	  );
--     print("number of monomials considered = "|nL);
     (Q,inG,G)
     )

-----------------Homogeneous codes

randomPointsMat = method(Options =>{AllRandom =>false})
randomPointsMat(Ring, ZZ) := opts -> (R,n) -> (
	d := numgens R;
	if opts.AllRandom == true then return random(R^d, R^n);
	
	m1 := id_(R^d)|transpose matrix(R,{toList(d:1)});
	if n<=d+1 then return m1_(toList(0..n-1));
	
	m3 := random(R^d,R^(n-d-1));
	m1 | m3
	)


points = (pointsmat) -> (
        mm := vars ring pointsmat;
	if rank source mm =!= rank target pointsmat then 
		error "wrong size matrix";
        ids := toSequence apply(rank source pointsmat, 
	    i -> image(mm * (syz transpose pointsmat_{i})));
        ideal intersect ids
	)
    
randomPoints = (r,n) -> (
	x := symbol x;
	R := ZZ/101[x_0 .. x_r];
	pmat := randomPointsMat(R,n);
	points pmat
	)


testPoints = ()->(
    	a := symbol a;
    	b := symbol b;
    	c := symbol c;		
	R := ZZ/101[a,b,c];
	pmat := matrix(R,{{1,0,0},{0,1,0},{0,0,1}});
	assert(points pmat == image matrix(R, {{a*b, a*c, b*c}}))
	)

omegaPoints = (pointsmat) -> (
	dualmat := syz pointsmat;
	s := (rank source dualmat)-1;
	n := rank source pointsmat;
	r := (rank target pointsmat)-1;
	mm := vars ring pointsmat;
	if rank source mm =!= r+1 then
		error "wrong size matrix";
	R := ring pointsmat;
	mult := matrix(R, table(n, n^2,(i,j) -> (
		if j//n==j%n and j%n==i then 1 else 0)));
	prod := mult*((transpose pointsmat)**dualmat);
	mm = (identity (R^{1}))**mm;
	(mm**(identity (R^(s+1))))*(generators kernel prod)
	)
testOmegaPoints = () -> (
	R := ZZ/101[vars(0..6)];
	testmat := random(R^7,R^11);
	-- testmat = matrix(R, {{1,0,1,5},{0,1,1,11}});
	w := omegaPoints(testmat);
	assert(rank source w == 18 and rank target w == 4)
		)


expectedBetti = (r,n) -> (
	e := 1;
	while binomial(r+e,e)<= n do e=e+1;
	d := e-1;
	a:=n-binomial(r+d,d);
	toprow := apply(toList(1..r),i->
		max((binomial(d+i-1,i-1))*(binomial(r+d,d+i))-a*(binomial(r,i-1)),
		0));
	bottomrow := apply(toList(1..r), i->
		max(a*(binomial(r,i))-(binomial(d+i,i))*(binomial(r+d,d+i+1)),
		0));
	top := apply(toList(0..r),i -> (
		if i == 0 then (0,{0},0)=>1 else
		(i,{},d+i) => toprow#(i-1) ));
	bottom := apply(toList(1..r),i -> (
		(i,{},d+i+1) => bottomrow#(i-1) ));
	new BettiTally from join(top, bottom)
	)
///
restart	
loadPackage("Points", Reload =>true)
expectedBetti(3,5)
minMaxResolution(3,5)
r=3;n=5
	R = ZZ/2[x_(0..n-1)];
	expectedBetti(r,n)
	lexIdeal(R,{1,3,1})
	betti resolution 
///

minMaxResolution = (r,n) -> (
	e := 1;
	while binomial(r+e,e)<= n do e=e+1;
	H := apply(e, i -> binomial(r+i-1,i));
	H = append(H,n-binomial(r+e-1,e-1));
	if last H =!= 0 then H = append(H,0);	
	x := symbol x;
	R := ZZ/2[x_0..x_(r-1)];
	<<"min"<<endl<< expectedBetti(r,n)<<endl;
	<<"max"<<endl<<betti resolution lexIdeal(R,H)<<endl;
)

-- First examples where expected resolution fails.
-- December 25, 1995: 
-- Unfortunately VERY slow in this system (the resolution is fast,
-- but the random number generation and intersections are very slow
-- for example the first uses 65 seconds of cpu time on a sparc 10!!
-- June 2016:
-- now first example takes .07 seconds on a mac air.

eg1 := ()->(
	res randomPoints(6,11)
	)
eg2 := ()->(
	res randomPoints(7,12)
	)
eg3 := ()->(
	res randomPoints(8,13)
	)
eg4 := ()->(
	res randomPoints(10,16)
	)

-- The following method should be much better:

eg := (r,n) -> (
--	print expectedBetti(r,n);
	R := ZZ/31991[vars (0..r)];
	w := omegaPoints(randomPointsMat(R,n));
--	betti resolution( w, DegreeLimit => 1)
	)


beginDocumentation()
doc ///
   Key
    Points
   Headline
    A package for making and studying points in affine and projective spaces
   Description
    Text
     The package has routines for points in affine and projective spaces. The affine
     code, some of which uses the Buchberger-Moeller algorithn to more quickly
     compute the ideals of points in affine space,
     was written by Stillman, Smith and Stromme. The projective code was
     written separately by Eisenbud and Popescu.
     
     The purpose of the projective code was to find as many counterexamples
     as possible to the minimal resolution conjecture; it was of use in the 
     research for the paper 
     "Exterior algebra methods for the minimal resolution conjecture",
     by  David Eisenbud, Sorin Popescu, Frank-Olaf Schreyer and Charles Walter
     (Duke Mathematical Journal. 112 (2002), no.2, 379-395.)
     The first few of these counterexamples are:
     (6,11),
     (7,12),
     (8,13),
     (10,16),
     where the first integer denotes the ambient dimension and the second the 
     number of points. Examples are known in every projective space of dimension >=6
     except for P^9.
///

--documentation for the code for points in projective space
doc ///
   Key
    randomPoints
   Headline
    ideal of a random set of points
   Usage
    i = randomPoints(r,n)
   Inputs
    r:ZZ
     number of points
    n:ZZ
     ambient dimension
   Outputs
    i:Ideal
     ideal of the random points
   Description
    Text
     The script defines a ring R with r+1 variables, and calls
     points(R,randomPointsMat(r,n))
    Example
     betti res randomPoints(11,5)
   SeeAlso
    randomPointsMat
    points
///


doc ///
   Key
    expectedBetti
   Headline
    The betti table of r points in Pn according to the minimal resolution conjecture
   Usage
    L = expectedBetti(r,n)
   Inputs
    r:ZZ
     ambient dimension
    n:ZZ
     number of points
   Outputs
    L:List
   Description
    Text
     The output is the smallest conceivable betti table for a set of
     r points in P^n, which is predicted (incorrectly) by the minimal resolution conjecture.
    Example
     expectedBetti(11,5)
   Caveat
    The MRC is false, so these are sometimes not the actual betti numbers.
   SeeAlso
    expectedBetti
    minMaxResolution
///

doc ///
   Key
    minMaxResolution
   Headline
    Min and max conceivable Betti tables for generic points
   Usage
    minMaxResolution(r,n)
   Inputs
    r:ZZ
     ambient dimension
    n:ZZ
     number of points
   Description
    Text
     prints betti tables corresponding to the minimal resolution conjecture
     and to the lex ideal with the same hilbert function
    Example
     minMaxResolution(3,5)
   SeeAlso
    expectedBetti
///

doc ///
   Key
    omegaPoints
   Headline
    linear part of the presentation of canonical module of points
   Usage
    m = omegaPoints pointsmat
   Inputs
    pointsmat:Matrix
     matrix of ZZ, representing a set of points
   Outputs
    m:Matrix
     linear part of the presentation matrix of the canonical module
   Description
    Text
     given an r+1 x n matrix
     over a ring with r+1 variables, interpreted as a set of 
     n points in P^r, the script produces the linear part
     of the presentation matrix of w_{>=-1}, where w is the
     canonical module of the cone over the points.  It is
     necessary for this to assume that no subset of n+1
     of the points is linearly dependent.  The presentation
     is actually a presentation of w if the points do not
     lie on a rational normal curve (so there are no
     quadratic relations on w_{>=-1}) and impose independent
     conditions on quadrics (so the homogeneous coordinate
     ring is 3-regular, and w is generated in degree -1.
    Example
     R = ZZ/101[vars(0..4)]
     p = randomPointsMat(R,11)
     w = omegaPoints p
     degree (R^1/(points p))
     degree coker w
     betti res (R^1/(points p))
     betti res coker w
   SeeAlso
///


doc ///
   Key
    randomPointsMat
    (randomPointsMat, Ring, ZZ)
    [randomPointsMat, AllRandom]
   Headline
    matrix of homogeneous coordinates of random points in projective space
   Usage
    m=randomPointsMat(R,n)    
   Inputs
    R:Ring
     homogeneous coordinate ring of projective space Pm
    n:ZZ
     number of points
   Outputs
    m:Matrix
     of ZZ
   Description
    Text
     Produces a random m+1 x n matrix of scalars, with columns representing the coordinates
     of the point. The first m+1 x m+1 submatrix is the identity.
    Example
     R = ZZ/31991[vars(0..3)]
     randomPointsMat(R,3)
     randomPointsMat(R,3, AllRandom=>true)
     randomPointsMat(R,7)
///


doc ///
   Key
    points
   Headline
    make the ideal of a set of points
   Usage
    i = points pointsMat
   Inputs
    pointsMat:Matrix
     matrix whose columns are the homogeneous cooredinates of the points
   Outputs
    i:Ideal
   Description
    Text
    Example
     R = ZZ/101[vars(0..4)]
     pointsMat = randomPointsMat(R,11)
     points pointsMat
   SeeAlso
    randomPointsMat
///

doc ///
   Key
    AllRandom
   Headline
    Option to randomPointsMat.
   Description
    Text
     Default is false, in which case the first (up to) r+1 points
     returned are the standard simplex; if true, all the points are random.
   SeeAlso
    randomPointsMat
///


---documentation for the affine code:
document {
     Key => {affineMakeRingMaps, (affineMakeRingMaps,Matrix,Ring)},
     Headline => "evaluation on points",
     Usage => "affineMakeRingMaps(M,R)",
     Inputs => {
     	  "M" => Matrix => "in which each column consists of the coordinates of a point",
	  "R" => PolynomialRing => "coordinate ring of the affine space containing the points",
	  },
     Outputs => {List => "of ring maps corresponding to evaluations at each point"},
     "Giving the coordinates of a point in affine space is equivalent to giving a
     ring map from the polynomial ring to the ground field: evaluation at the point.  Given a
     finite collection of points encoded as the columns of a matrix,
     this function returns a corresponding list of ring maps.",
     EXAMPLE lines ///
     M = random(ZZ^3, ZZ^5)
     R = QQ[x,y,z]
     phi = affineMakeRingMaps(M,R)
     apply (gens(R),r->phi#2 r)
     phi#2
     ///
     }


---the affine code documentation
document {
     Key => {affinePoints, (affinePoints,Matrix,Ring)},
     Headline => "produces the ideal and initial ideal from the coordinates
     of a finite set of points",
     Usage => "(Q,inG,G) = points(M,R)",
     Inputs => {
     	  "M" => Matrix => "in which each column consists of the coordinates of a point",
	  "R" => PolynomialRing => "coordinate ring of the affine space containing the points",
	  },
     Outputs => {
          "Q" => List => "list of standard monomials",
 	  "inG" => Ideal => "initial ideal of the set of points",
 	  "G" => List => "list of generators for Grobner basis for ideal of points"
 	  },
     "This function uses the Buchberger-Moeller algorithm to compute a grobner basis
     for the ideal of a finite number of points in affine space.  Here is a simple
     example.",
     EXAMPLE lines ///
     M = random(ZZ^3, ZZ^5)
     R = QQ[x,y,z]
     (Q,inG,G) = affinePoints(M,R)
     monomialIdeal G == inG
     ///,
     PARA{},
     "Next a larger example that shows that the Buchberger-Moeller algorithm in ",
     TT "points", " may be faster than the alternative method using the intersection
     of the ideals for each point.",
     EXAMPLE lines ///
     R = ZZ/32003[vars(0..4), MonomialOrder=>Lex]
     M = random(ZZ^5, ZZ^150)
     time J = affinePointsByIntersection(M,R);
     time C = affinePoints(M,R);
     J == C_2  
     ///,
     SeeAlso => {affinePointsByIntersection}
     }

document {
     Key => {affinePointsMat, (affinePointsMat,Matrix,Ring)},
     Headline => "produces the matrix of values of the standard monomials
     on a set of points",
     Usage => "(A,stds) = affinePointsMat(M,R)",
     Inputs => {
     	  "M" => Matrix => "in which each column consists of the coordinates of a point",
	  "R" => PolynomialRing => "coordinate ring of the affine space containing the points",
	  },
     Outputs => {
          "A" => Matrix => "standard monomials evaluated on points",
 	  "stds" => Matrix => "whose entries are the standard monomials",
 	  },
     "This function uses the Buchberger-Moeller algorithm to compute a the matrix ",
     TT "A", " in which the columns are indexed by standard monomials, the rows are
     indexed by points, and the entries are given by evaluation.  The ordering of
     the standard monomials is recorded in the matrix ", TT "stds", " which has a
     single column.
     Here is a simple
     example.",
     EXAMPLE lines ///
     M = random(ZZ^3, ZZ^5)
     R = QQ[x,y,z]
     (A,stds) = affinePointsMat(M,R)
     ///,
     Caveat => "Program does not check that the points are distinct.",
     SeeAlso => {affinePoints},
     }

document {
     Key => {affinePointsByIntersection, (affinePointsByIntersection,Matrix,Ring)},
     Headline => "computes ideal of point set by intersecting maximal ideals",
     Usage => "affinePointsByIntersection(M,R)",
     Inputs => {
     	  "M" => Matrix => "in which each column consists of the coordinates of a point",
	  "R" => PolynomialRing => "coordinate ring of the affine space containing the points",
	  },
     Outputs => {
 	  List => "grobner basis for ideal of a finite set of points",
 	  },
     "This function computes the ideal of a finite set of points by intersecting
     the ideals for each point.  The coordinates of the points are the columns in
     the input matrix ", TT "M", ".",
     EXAMPLE lines ///
     M = random(ZZ^3, ZZ^5)
     R = QQ[x,y,z]
     affinePointsByIntersection(M,R)
     ///,
     SeeAlso => {affinePoints},
     }

TEST///
     M = random(ZZ^3, ZZ^3)
     M = id_(ZZ^3)
     R = QQ[x,y,z]
     (Q,inG,G) = affinePoints(M,R)
     assert( G == {x+y+z-1, z^2-z, y*z, y^2-y})
///
TEST///
setRandomSeed 0
m = randomPoints(3,5) 
R = ring m
assert(m == ideal(
  R_1*R_3-R_2*R_3,R_0*R_3-R_2*R_3,R_1*R_2-R_2*R_3,R_0*R_2-R_2*R_3,R_0*R_1-R_2*R_3)
)
///

TEST///
setRandomSeed 0
R = ZZ/101[a,b,c]
p = randomPointsMat(R,6)
assert(omegaPoints p == R^{1}**matrix {{-36*a+36*b, 19*a-19*b, -30*a+30*c, 19*a-19*c}, {-49*a+b, 0, -24*a+c, 0}, {0,
      32*a+b, 0, 32*a+c}}
)
///

TEST///
assert( 
    expectedBetti(3,5) == new BettiTally from {(0,{0},0) => 1, (1,{},2) => 5, (1,{},3) => 0, (2,{},3) => 5, (2,{},4)
      => 0, (3,{},4) => 0, (3,{},5) => 1}
)
///

TEST///
R = ZZ/11[vars(0..2)]
setRandomSeed 0
assert(
    randomPointsMat(R,5) == R**matrix {{1, 0, 0, 1, -3}, {0, 1, 0, 1, 1}, {0, 0, 1, 1, 3}}
    )
assert(randomPointsMat(R,3,AllRandom=>true) == R**matrix {{-4, 3, -3}, {-3, 3, -3}, {-1, -4, 5}})
///

TEST ///
R = ZZ/32003[vars(0..4), MonomialOrder=>Lex]
M = matrix(ZZ/32003,  {{0, -9, 4, -2, -4, -9, -10, 6, -8, 0}, 
            {1, 0, -10, 9, 3, -4, 1, 1, -10, -3}, 
	    {5, 7, -4, -5, -7, 7, 4, 6, -3, 2}, 
	    {2, 8, 6, -6, 4, 3, 8, -10, 7, 8}, 
	    {-9, -9, 0, 4, -3, 9, 4, 4, -4, -4}})
phi = affineMakeRingMaps(M,R)
apply (gens(R),r->phi#2 r)
assert ( {4, -10, -4, 6, 0} == apply (gens(R),r->phi#2 r) )

J = affinePointsByIntersection(M,R);
///

TEST ///
R = ZZ/32003[vars(0..4), MonomialOrder=>Lex]
M = matrix(ZZ/32003,  {{0, -9, 4, -2, -4, -9, -10, 6, -8, 0}, 
            {1, 0, -10, 9, 3, -4, 1, 1, -10, -3}, 
	    {5, 7, -4, -5, -7, 7, 4, 6, -3, 2}, 
	    {2, 8, 6, -6, 4, 3, 8, -10, 7, 8}, 
	    {-9, -9, 0, 4, -3, 9, 4, 4, -4, -4}})
phi = affineMakeRingMaps(M,R)
apply (gens(R),r->phi#2 r)
assert ( {4, -10, -4, 6, 0} == apply (gens(R),r->phi#2 r) )

J = affinePointsByIntersection(M,R);
C = affinePoints(M,R);
assert ( J == C_2 )
assert ( C_1 == ideal(e^6,d*e^3,d^2*e,d^3,c,b,a) )
assert ( C_0 == sort apply (standardPairs monomialIdeal C_2, p -> p#0) )
assert (
     (affinePointsMat(M,R))#0 == 
      matrix(ZZ/32003, {{1, -9, 81, -729, 6561, 4957, 2, -18, 162, 4}, {1, -9, 81, -729, 6561,
      4957, 8, -72, 648, 64}, {1, 0, 0, 0, 0, 0, 6, 0, 0, 36}, {1, 4, 16, 64, 256, 1024,
      -6, -24, -96, 36}, {1, -3, 9, -27, 81, -243, 4, -12, 36, 16}, {1, 9, 81, 729, 6561,
      -4957, 3, 27, 243, 9}, {1, 4, 16, 64, 256, 1024, 8, 32, 128, 64}, {1, 4, 16, 64,
      256, 1024, -10, -40, -160, 100}, {1, -4, 16, -64, 256, -1024, 7, -28, 112, 49}, {1,
      -4, 16, -64, 256, -1024, 8, -32, 128, 64}})
)
assert ( first entries transpose (affinePointsMat(M,R))#1 == C_0 )
///

{*
--test of affinePoints
TEST///
C = affinePoints(M,R);
assert ( J == C_2 )
assert ( C_1 == ideal(e^6,d*e^3,d^2*e,d^3,c,b,a) )
assert ( C_0 == sort apply (standardPairs monomialIdeal C_2, p -> p#0) )
assert (
     (affinePointsMat(M,R))#0 == 
      matrix(ZZ/32003, {{1, -9, 81, -729, 6561, 4957, 2, -18, 162, 4}, {1, -9, 81, -729, 6561,
      4957, 8, -72, 648, 64}, {1, 0, 0, 0, 0, 0, 6, 0, 0, 36}, {1, 4, 16, 64, 256, 1024,
      -6, -24, -96, 36}, {1, -3, 9, -27, 81, -243, 4, -12, 36, 16}, {1, 9, 81, 729, 6561,
      -4957, 3, 27, 243, 9}, {1, 4, 16, 64, 256, 1024, 8, 32, 128, 64}, {1, 4, 16, 64,
      256, 1024, -10, -40, -160, 100}, {1, -4, 16, -64, 256, -1024, 7, -28, 112, 49}, {1,
      -4, 16, -64, 256, -1024, 8, -32, 128, 64}})
)
assert ( first entries transpose (affinePointsMat(M,R))#1 == C_0 )
///
*}

end--
uninstallPackage "Points"
restart
installPackage "Points"
viewHelp Points

check "Points"

 
 
----------------
--------------

toString C_1
restart
errorDepth = 0


uninstallPackage "Points"
installPackage "Points"
R = ZZ/32003[vars(0..4), MonomialOrder=>Lex]
M = matrix(ZZ/32003,  {{0, -9, 4, -2, -4, -9, -10, 6, -8, 0}, 
            {1, 0, -10, 9, 3, -4, 1, 1, -10, -3}, 
	    {5, 7, -4, -5, -7, 7, 4, 6, -3, 2}, 
	    {2, 8, 6, -6, 4, 3, 8, -10, 7, 8}, 
	    {-9, -9, 0, 4, -3, 9, 4, 4, -4, -4}})

phi = affineMakeRingMaps(M,R)
apply (gens(R),r->phi#2 r)
assert ( {4, -10, -4, 6, 0} == apply (gens(R),r->phi#2 r) )


phi#2
time J = affinePointsByIntersection(M,R)
transpose matrix{oo}



time C = points(M,R)
transpose gens ideal C_2

M = random(ZZ^3, ZZ^5)
R = QQ[x,y,z]
phi = affineMakeRingMaps(M,R)
apply (gens(R),r->phi#2 r)
phi#2

R = ZZ/32003[vars(0..4), MonomialOrder=>Lex]
M = random(ZZ^5, ZZ^150)

time J = affinePointsByIntersection(M,R);
transpose matrix{oo}

time C = points(M,R);
transpose gens ideal C_2
assert(J == C_2)

R = ZZ/32003[vars(0..4)]

K = ZZ/32003
R = K[vars(0..7), MonomialOrder=>Lex]
R = K[vars(0..7)]
M = random(K^8, K^500)
time C = points(M,R);
time J = affinePointsByIntersection(M,R);
assert(C_2 == J)

K = ZZ/32003
R = K[x_0 .. x_39]
M = random(K^40, K^80)
time C = points(M,R);


getColumnChange oo_0
apply(Fs, f -> f(a*b*c*d))
B = sort basis(0,2,R)
B = sum(flatten entries basis(0,2,R))
B = matrix{reverse terms B}
P = transpose matrix {apply(Fs, f -> f (transpose B))}
B * syz 
transpose oo
 -- column reduction:

P = mutableMatrix P 
H = new MutableHashTable
reduceColumn(P,null,H,0)
reduceColumn(P,null,H,1)
P
reduceColumn(P,null,H,2)
reduceColumn(P,null,H,3)
reduceColumn(P,null,H,4)
reduceColumn(P,null,H,5)
reduceColumn(P,null,H,6)
reduceColumn(P,null,H,7)
reduceColumn(P,null,H,8)
reduceColumn(P,null,H,9)
P
reduceColumn(P,null,H,10)
reduceColumn(P,null,H,11)
reduceColumn(P,null,H,12)
P

M = matrix{{1,2,3,4}}

K = ZZ/32003
M ** K
