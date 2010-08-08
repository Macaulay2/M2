-- -*- coding: utf-8 -*-
newPackage(
	"PointsNew",
    	Version => "1.0", 
    	Date => "12 August 2010",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu", HomePage => "http://www.math.uiuc.edu/Macaulay2/"},
     	     {Name => "Gregory G. Smith", Email => "ggsmith@mast.queensu.ca"},
	     {Name => "Stein A. Strømme", Email => "stromme@math.uib.no"},
	     {Name => "Samuel Lundqvist", Email => "samuel@math.su.se"}
	     },
    	Headline => "Computing with sets of affine points and functionals (i.e. FGLM conversion)",
    	DebuggingMode => true
    	)
-- Current developers
-- Past developers
-- Contributors
-- Acknowledgements
export {
     pointsMat,
     points,
     pointsByIntersection,
     makeRingMaps,
     nfPoints,
     separators,
     FGLM,
     stdmons
     }


debug Core

makeRingMaps = method (TypicalValue => List)
makeRingMaps (Matrix, Ring) := List => (M,R) -> (
     K := coefficientRing R;
     pts := entries transpose M;
     apply(pts, p -> map(K, R, p))
     )

reduceColumn = (M,Mchange,H,c) -> (
     -- M is a mutable matrix
     -- Mchange is either null, or a matrix with same number of columns as M
     -- H is a hash table: H#r == c if column c has pivot for row r 
     -- returns true if the element reduces to 0
     M = raw M;
     if Mchange =!= null then Mchange = raw Mchange;
     r := rawNumberOfRows M - 1;
     while r >= 0 do (
	  a := M_(r,c);
	  if a != 0 then (
	       -- is there a pivot?
	       if not H#?r then (
		    b := 1//a;
		    rawMatrixColumnScale(M, b, c, false);
		    if Mchange =!= null then rawMatrixColumnScale(Mchange, b, c, false);		    
		    H#r = c;
		    return false;
		    )
	       else (
	       	    pivotc := H#r;
	       	    rawMatrixColumnChange(M, c, -a, pivotc, false);
		    if Mchange =!= null then rawMatrixColumnChange(Mchange, c, -a, pivotc, false);
	       ));
     	  r = r-1;
	  );
     true
     )
     -- Samuel Lundqvist jan 2010
addNewMonomialFGLM = (M,col, StoK, mon, G, basisS) -> (
     -- M is an s by s+1 matrix, s=#points
     -- monom is a monomial
     -- replaces the 'col' column of M with the vector (v_1,...,v_s), where
     -- nf(mon) = v_1 m_1 + \cdots + v_s m_s.
     l := flatten entries (coefficients (mon % G, Monomials => basisS))_1;
     scan(#l, i -> M_(i,col) = StoK(l#i))
     )


addNewMonomial = (M,col,monom,maps) -> (
     -- M is an s by s+1 matrix, s=#points
     -- monom is a monomial
     -- maps is a list of s ring maps, which will give the values
     --  of the monom at the points
     -- replaces the 'col' column of M with the values of monom
     --    at the s points.vi
     scan(#maps, i -> M_(i,col) = maps#i monom)
     )

pointsByIntersection = method(TypicalValue => List)
pointsByIntersection (Matrix,Ring) := (M,R) -> (
     flatten entries gens gb intersect apply (
       entries transpose M, p -> ideal apply(#p, i -> R_i - p#i)))

-- Mike/Stein 2008? 
-- Bugfix: All variables are now local, Samuel, Aug 2010.
pointsMat = method()
pointsMat(Matrix,Ring) := (M,R) -> (
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
     Fs := makeRingMaps(M,R);
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
	       f := sum apply(toList(Lhash#monom .. numgens R - 1), i -> (
			 newmon := monom * R_i;
			 Lhash#newmon = i;
			 newmon));
	       L = L + f;
	       thiscol = thiscol + 1;
	       )
	  );
     --print("ntimes "|ntimes|" std+inG "|#Q + numgens inG);
     stds := transpose matrix{Q};
     A := transpose matrix{apply(Fs, f -> f stds)};
     (A, stds)
     )

-- Stein/Mike 2008?
-- Bugfix: All variables are now local, Samuel, aug 2010.
points = method()
points (Matrix,Ring) := (M,R) -> (
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
     Fs := makeRingMaps(M,R);
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
	  rawMatrixColumnScale(raw PC, raw(0_K), thiscol, false);
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



-- The separators of the points as linear combinations of the standard monomials
-- stds are the standard monomials returned by pointsMat
-- Ainv is the inverse of the matrix returned by pointsMat
-- Samuel Lundqvist, jan 2010
separators = method()
separators (Matrix, Matrix) := (stds, Ainv) -> (
     transpose (Ainv) * (matrix entries stds)
     )


-- The normal form of a polynomial using Ainv and linear algebra
-- p is the polynomial of which we want to compute nf
-- phi are the ring maps returned from makeRingMaps 
-- stds are the standard monomials returned by pointsMat
-- Ainv is the inverse of the matrix returned by pointsMat
-- Samuel Lundqvist jan 2010

nfPoints = method()
nfPoints (RingElement, List, Matrix, Matrix) := (p, phi, stds, Ainv) -> (
     --Evaluate the vector on the points
     v := transpose matrix {apply (phi, r -> r p)};
     w := Ainv * v;
     --Fix the stds
     stdsniceform := transpose (matrix (entries (stds)));
     --return the normal form
     first (first entries (stdsniceform*w))
     )

-- Samuel Lundqvist aug 2010
stdmons = method()
stdmons(PolynomialRing, GroebnerBasis) := (S,Gb) -> (
     I := monomialIdeal(leadTerm (Gb));
     basisSmodI := flatten (entries (basis (S/I)));
     -- we want the monomials to lie in S, not in S/I 
     SmodItoS := map(S,S/I);
     apply(basisSmodI, i -> SmodItoS(i))
     )

-- Samuel Lundqvist jan 2010
--stdmons = method()
--stdmons(PolynomialRing, Ideal) := (S,I) -> (
--     basisSmodI = flatten (entries (basis (S/I)));
--     SmodItoS = map(S,S/I);
--     apply(basisSmodI, i -> SmodItoS(i))
--)

-- Samuel Lundqvist jan 2010, copied from Points.
FGLM = method() 
FGLM (GroebnerBasis, PolynomialRing, Option) := (GS,S,monOrd) -> (   
     --Determine the standard monomials.
     basisS := stdmons (S,GS);
     s := length basisS;   
     --from now on, we will compute over the ring R.
     -- R := symbol R; 
     R := newRing(S, monOrd);
     RtoS := map(S,R);
     K := coefficientRing R;
     StoK := map(K,S);
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
     --Fs := makeRingMaps(M,R);
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
	  L = L % inGB; --Remove multiples of elements in inGB
	  monom = someTerms(L,-1,1); --Pick the largest elementet i L.
	  L = L - monom;
	  -- Now fix up the matrices P, PC
	  use S;
          addNewMonomialFGLM(P,thiscol,StoK,RtoS(monom),GS,basisS);
	  use R;
	  rawMatrixColumnScale(raw PC, raw(0_K), thiscol, false);
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
     (R,G)
     )

beginDocumentation()

document {
     Key => PointsNew,
     "A package to compute with points in affine and projective spaces",
     {*
     Subnodes => {
	  -- Mike wanted this: TO (points,Matrix,Ring)
	  }
     *}
     }
document {
     Key => {nfPoints, (nfPoints,RingElement,List,Matrix,Matrix)},
     Headline => "Normal form wrt standard monomials using linear algebra",
     Usage => "makeRingMaps(p,phi,std,Ainv)",
     Inputs => {
     	  "p" => RingElement => "The polynomial for which we want to compute the normal form",
	  "phi" => List => "The ring maps",
	  "std" => Matrix => "in which each column consists of the coordinates of a point",
	  "Ainv" => Matrix => "Inverse of the matrix achieved by evaluating the standard monomials on the input points",
	  },
     Outputs => {RingElement => "The normal form of f wrt the standard monomials"},
     "Computing normal forms with respect to a vanishing ideal of points should be done by linear algebra and not by means of a Gröbner basis. 
     The timing below indicates that the speedup is drastic, even for toy examples. ",
     EXAMPLE lines ///
     
     M = random(ZZ^10, ZZ^15);
     R = QQ[a..j];
     (A, std) = pointsMat(M, R);
     phi = makeRingMaps(M,R);
     Ainv = inverse A;
     f = b^3*c^10;
     timing f1 = nfPoints(f, phi, std, Ainv)
     --0.005261 seconds
     (Q,inG,G) = points(M,R)
     Gb = forceGB (matrix {G});
     timing f2= f % Gb
     --14.7422 seconds
     --The normal forms are the same
     f1 == f2
     --True
     ///
     }

document {
     Key => {makeRingMaps, (makeRingMaps,Matrix,Ring)},
     Headline => "evaluation on points",
     Usage => "makeRingMaps(M,R)",
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
     phi = makeRingMaps(M,R)
     phi#2
     ///
     }



document {
     Key => {points, (points,Matrix,Ring)},
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
     (Q,inG,G) = points(M,R)
     monomialIdeal G == inG
     ///,
     PARA{},
     "Next a larger example that shows that the Buchberger-Moeller algorithm in ",
     TT "points", " may be faster than the alternative method using the intersection
     of the ideals for each point.",
     EXAMPLE lines ///
     R = ZZ/32003[vars(0..4), MonomialOrder=>Lex]
     M = random(ZZ^5, ZZ^150)
     time J = pointsByIntersection(M,R);
     time C = points(M,R);
     J == C_2  
     ///,
     SeeAlso => {pointsByIntersection}
     }



document {
     Key => {pointsMat, (pointsMat,Matrix,Ring)},
     Headline => "produces the matrix of values of the standard monomials
     on a set of points",
     Usage => "(A,stds) = pointsMat(M,R)",
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
     
     ///,
     Caveat => "Program does not check that the points are distinct.",
     SeeAlso => {points},
     }

document {
     Key => {pointsByIntersection, (pointsByIntersection,Matrix,Ring)},
     Headline => "computes ideal of point set by intersecting maximal ideals",
     Usage => "pointsByIntersection(M,R)",
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
     pointsByIntersection(M,R)
     ///,
     SeeAlso => {points},
     }
document {
     Key => {FGLM,  (FGLM, GroebnerBasis, PolynomialRing, Option)},
       Headline => "Uses the FGLM algorithm to change a Groebner basis for a zero-dimensional ideal wrt to a monomial ordering mo1 to another 
     Groebner basis with respect to a monomial ordering mo2.",
     Usage => "G2 = points(std,G1,R,mo2)",
     Inputs => {
	  "G1" => GroebnerBasis => "A Groebner basis for the ideal wrt mo1", 
	  "R" => PolynomialRing => "The polynomial ring",
	  "mo2" => Option =>"The output monomial ordering"
	  },
     Outputs => { "S2" => PolynomialRing => "The polynomial ring where G2 lives",
	   	  "G2" => List => "The Groebner basis wrt to mo2"
		  },
     EXAMPLE lines ///
     M = random(ZZ^12, ZZ^32);
     -- 32 points in QQ^12

     R = QQ[a..l]
     --Compute a Gröbner basis for I(M) with respect to DegRevLex using the BM-algorithm
     (Q,inG,Gd) = points(M,R);
     (DegLexGb = forceGB matrix {Gd};
     IR = ideal gens DegLexGb;
    --Convert the basis to a Lex-base using FGLM
     timing((S1,FGLMLexGb) = FGLM(DegLexGb, R, MonomialOrder => Lex);) 
     -- 2.44324 seconds
     -- Compute a Gröbner basis for I(M) with respect to Lex (in S) by
     -- using the BM-algorithm
     S2 = newRing(R, MonomialOrder => Lex)  
     timing((Q2,inG2,PointsLexGb) = points(M,S2);)
     --Map the result from FGLM (which is in S1) to S2
     S1toS2 = map(S2,S1);  
     FGLMLexGb = apply(FGLMLexGb, p -> S1toS2(p));
    --Check that they are equal (sort is used since "==" does not
    --apply for GB:s)
     gens forceGB matrix  {sort FGLMLexGb} == gens forceGB matrix {sort PointsLexGb}
     -- true
     -- Now, to show that we gain speed, 
     -- compute a Lex Gröbner basis from the generators of IR.
     -- First map I from R to S2
     RtoS2 = map(S2,R);
     IS2 = ideal RtoS2 gens IR;
     timing (BuchbergerLex = gens gb IS2)
    -- 30.9826 seconds
     --Check that the result agrees with the FGLM result
     sort(BuchbergerLex) == gens forceGB matrix  {sort FGLMLexGb}
     -- true
     ///,
     SeeAlso => {points},
     }

