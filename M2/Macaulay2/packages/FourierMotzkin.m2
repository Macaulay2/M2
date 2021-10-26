-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 1998, 1999, 2000, 2006, 2008, 2010  Gregory G. Smith
-- 
-- You may redistribute this program under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 2 of the
-- License, or any later version.
--------------------------------------------------------------------------------

---------------------------------------------------------------------------
-- PURPOSE: compute the polar dual of a rational convex polyhedral cone 
--          using Fourier-Motzkin elimination
-- PROGRAMMER : Gregory G. Smith 
-- UPDATE HISTORY : 2 July 2000, 5 March 2006, 1 July 2008, 
--                  10 December 2008
---------------------------------------------------------------------------
newPackage(
	"FourierMotzkin",
    	Version => "1.2", 
    	Date => "10 December 2008",
    	Authors => {{
		  Name => "Gregory G. Smith", 
		  HomePage => "http://www.mast.queensu.ca/~ggsmith",
		  Email => "ggsmith@mast.queensu.ca"}},
    	Headline => "convex hulls and polar cones",
	Keywords => {"Convex Geometry"},
    	DebuggingMode => false
    	)

export "fourierMotzkin"


-- Transposition along the antidiagonal; used to compute row-reduced 
-- echelon form of a matrix
rotateMatrix = method();
rotateMatrix Matrix := Matrix => M -> (
     n := rank source M;
     m := rank target M;
     matrix table(n, m, (i,j) -> M_(m-j-1, n-i-1)))


-- Determine if an row vector/inequality is redundant; see Exercise 2.15 
-- in Ziegler.
-- 'V' : a list of sets of integers.  Each entry contains indices of the 
--       original rays which do NOT vanish at the corresponding row vector.
-- 'S' : a set of integers; the original rays for the row vector.
isRedundant = method();
isRedundant (List, Set) := Boolean => (V, S) -> (
     -- the row vector is redundant iff 'S' contains an entry in 'V'
     flag := 0;
     k := 0;
     numRow := #V;  -- equals the number of inequalities
     while ((flag < 1) and (k < numRow)) do (
	  if isSubset(V#k, S) then flag = flag + 1;
	  k = k + 1);
     flag === 1)


-- Eliminates the first variable in the inequalities 'A' using the double 
-- description version of Fourier-Motzkin elimination
-- 'A' : a list of lists of integers.  Each entry is a corresponds to a 
--       row vector in the system of inequalities.
-- 'V' : a list of sets of integers.  Each entry contains indices of the 
--       original rays which do NOT vanish at the corresponding row vector;  
--       the complement of the 'V_i' appearing in Exercise 2.15 in Ziegler.
-- 's': an integer.  The index of the variable being eliminated
-- Output is a list '{projA, projV}'.  'projA' a list of lists of integers;
-- each entry is a corresponds to a row vector in the projected system of 
-- inequalities.  'projV' is a list of sets of integers; each entry 
-- contains indices of the original rays which do NOT vanish at the 
-- corresponding row vector in 'projA'
fourierMotzkinElimination = method();
fourierMotzkinElimination (List, List, ZZ) := List => (A, V, s) -> (
     -- initializing local variables
     numCol := 0;
     if A =!= {} then numCol = #(A#0);
     numRow := #A;  -- equal to the length of V
     pos := {};
     neg := {};
     projA := {};
     projV := {};
     -- divide the inequalities into three groups.
     k := 0;
     while (k < numRow) do (
	  if (A#k#0 < 0) then neg = append(neg, k)
	  else if (A#k#0 > 0) then pos = append(pos, k)
	  else (
	       projA = append(projA, A#k);
	       projV = append(projV, V#k));
	  k = k+1);	  
     -- generate new inequalities.
     scan(pos, i -> 
	  scan(neg, j -> (
		    vertices := V#i + V#j;
		    if not isRedundant(projV, vertices) 
		    then (
			 iRow := A#i;
			 jRow := A#j;
			 iCoeff := - jRow#0;
			 jCoeff := iRow#0;
			 a := iCoeff*iRow + jCoeff*jRow;
			 projA = append(projA, a);
			 projV = append(projV, vertices)))));
     -- don't forget the implicit inequalities '-t <= 0'.
     scan(pos, i -> (
	  vertices := V#i + set{s};
	  if not isRedundant(projV, vertices) 
	  then (
	       projA = append(projA, A#i);
	       projV = append(projV, vertices))));
     -- remove the first column 
     projA = apply(projA, e -> primitive e_{1..(numCol-1)});
     -- remove redundant inequalities
     irredundant := select(toList(0..#projA-1), 
	  i -> not isRedundant(delete(projV#i,projV),projV#i));
     projA = apply(irredundant, i -> projA#i);
     projV = apply(irredundant, i -> projV#i);     
     {projA, projV})   


-- divides a list of integers by their gcd.
primitive = method();
primitive List := List => L -> (
     -- finding greatest common divisor
     n := #L-1;
     g := abs(L#n);
     while (n > 0) do (
	  n = n-1;
	  g = gcd(g, L#n);
	  if g === 1 then n = 0);
     if g === 1 then L 
     else apply(L, i -> i // g))


-- Converts a list of 'QQ' to 'ZZ' by multiplying by a common denominator
toZZ = method();
toZZ List := List => L -> (
     -- finding common denominator
     d := apply(L, e -> denominator e);
     l := lcm d;
     apply(L, e -> (numerator(l*e))))


-- Computes the dual representation for the polyhedral cone
fourierMotzkin = method();

--   INPUT : 'Z' a matrix; the columns are the rays generating the cone
--	     'H' a matrix; the columns are the rays generaing the linear 
--               space in the cone.  
--  OUTPUT : a sequence (A,E) :
--           'A' a matrix; the columns are the rays generating the polar 
--               cone
--           'E' a matrix; the columns are the rays generating the linear 
--               space in the polar cone
-- COMMENT :  'cone(Z) + affine(H) = {x : A^t * x <= 0, E^t * x = 0}'
fourierMotzkin (Matrix, Matrix) := Sequence => (Z, H) -> (
     -- checking for input errors
     R := ring source Z;
     if (R =!= ring source H) then
     error ("expected matrices over the same ring");
     if (rank target Z =!= rank target H) then
     error ("expected matrices to have the same number of rows");
     -- making 'QQ' versions of the input
     local Y;
     local B;
     outputZZ := false; 
     if (R === ZZ) then (
	  outputZZ = true;
	  Y = substitute(Z, QQ);
	  B = substitute(H, QQ))
     else if (R === QQ) then (
	  Y = Z;
	  B = H)
     else error ("expected a matrix over 'ZZ' or 'QQ'");
     -- expressing 'cone(Y) + affine(B)' in the form {x : Ax <= 0}
     d := rank target Y;
     if (rank source B > 0) then Y = Y | B | -B;
     n := rank source Y;
     A := Y | -id_(QQ^d);
     -- computing the row echelon form of 'A'
     A = gens gb rotateMatrix A;
     L := rotateMatrix leadTerm A;
     A = rotateMatrix A;
     -- find pivots
     numRow := rank target A;     -- numRow <= d
     i := 0;
     pivotCol := {};
     while (i < numRow) do (
	  j := 0;
	  while ((j < n+d) and (L_(i,j) =!= 1_QQ)) do (j = j+1);
	  pivotCol = append(pivotCol, j);
	  i = i+1);
     -- computing the row-reduced echelon form of 'A'
     A = ((submatrix(A, pivotCol))^(-1)) * A;
     -- converting 'A' into a list of integer row vectors 
     A = entries A;
     A = apply(A, e -> primitive toZZ e);
     -- creating the vertex list 'V' for double description and listing the
     -- variables 'T' which remain to be eliminated
     V := {};
     T := toList(0..(n-1));
     scan(pivotCol, e -> (
	       if (e < n) then (
	       	    T = delete(e, T);
	       	    V = append(V, set{e}))));
     -- separating inequalities 'A' and equalities 'E'
     eqnRow := {};
     ineqnRow := {};
     scan(numRow, i -> (
	       if (pivotCol#i >= n) then eqnRow = append(eqnRow, i)
	       else ineqnRow = append(ineqnRow, i)));	  
     E := apply(eqnRow, i -> A#i);
     E = apply(E, e -> e_{n..(n+d-1)});
     A = apply(ineqnRow, i -> A#i);
     A = apply(A, e -> e_(T | toList(n..(n+d-1)))); 
     -- successive projections eliminate the remaining variables 'T'
     if (A =!= {}) then
     scan(T, t -> (
	       D := fourierMotzkinElimination(A, V, t);
	       A = apply(D#0, e -> primitive e);
	       V = D#1;
	       ));
     -- output formatting
     --A = apply(A, e -> primitive e);
     if (A === {}) then A = map(ZZ^d, ZZ^0, 0)
     else A = transpose matrix A;
     if (E === {}) then E = map(ZZ^d, ZZ^0, 0)
     else E = transpose matrix E;
     if (outputZZ === false) then (
	  A = substitute(A, QQ); 
	  E = substitute(E, QQ));
     (sort A, sort E))


--   INPUT : 'Z' a matrix; the columns are the rays generating the cone
fourierMotzkin Matrix := Sequence => Z -> (
     -- creating zero equalities
     R := ring target Z;
     d := rank target Z;
     H := map(R^d, R^0, 0);
     -- calls fourierMotzkin(Matrix, Matrix)
     fourierMotzkin(Z,H))

beginDocumentation()

document { 
	Key => FourierMotzkin,
	Headline => "for convex hull and vertex enumeration",

	"A convex cone is ", EM "polyhedral", " if it is a finite
	intersection of halfspaces.  A convex cone is ", EM " finitely
	generated ", " if it is the set of all nonnegative linear
	combinations of a finite set of vectors.  The fundamental
	theorem for cones states that a convex cone is polyhedral if
	and only if it is finitely generated.  ",
	
	PARA{}, TT "FourierMotzkin", " is a Macaulay2 implementation of
	the Double Description Method (of Fourier, Dines and Motzkin)
	for converting between these two basic representations for
	convex cones.  For polytopes, this allows one to convert
	between the convex hull of a finite point set and the bounded
	intersection of halfspaces.",
	
	PARA{}, "Here are some examples illustrating some uses of this
	package.",
	UL {
	     {TO "Finding the facets of the cyclic polytope"},
	     {TO "Finding the facets of the permutahedron"},
	     {TO "Applications to multigraded polynomial rings"}	     
	     },
	
	PARA{}, "This package is intended for use with relatively small
	polyhedra.  For larger polyhedra, please consider ",
	HREF("http://www.ifor.math.ethz.ch/~fukuda/cdd_home/cdd.html","cdd"),
	", ", HREF("http://cgm.cs.mcgill.ca/~avis/C/lrs.html","lrs"),
	" or ", HREF("http://www.qhull.org/","qhull"), "; also see ",
	HREF("http://www.math.tu-berlin.de/polymake/", "polymake"),
	".",

	PARA{}, "For an introduction to polyhedra and Fourier-Motzkin
	elimination, we recommend Chapter 2 in ",
	HREF("http://www.math.tu-berlin.de/~ziegler/", "Gunter
	M. Ziegler's"), " ", EM "Lectures on Polytopes", ", Graduate
	Texts in Mathematics 152, Springer-Verlag, New York, 1995.
	For historical comments, see Section 12.2 in ",
	HREF("http://homepages.cwi.nl/~lex/", "Alexander
	Schrijver's"), " ", EM "Theory of Linear and Integer
	Programming", " Wiley-Interscience Series in Discrete
	Mathematics, John Wiley and Sons, Chichester, 1986.",
	
	PARA{}, "We thank ",
	HREF("http://page.mi.fu-berlin.de/rbirkner/indexen.htm", "Rene Birkner"),
	" for help debugging the package."
	     
	}

document {     
     Key => {fourierMotzkin, (fourierMotzkin,Matrix), (fourierMotzkin,Matrix,Matrix)},
     Headline => "interchange inequality/generator representation of a
     polyhedral cone",
     Usage => "(A',B') = fourierMotzkin(A,B)",
     Inputs => {
	  "A" => {"a ", TO Matrix, " with entries in ", TO ZZ, " or ", TO QQ},
	  "B" => {"a ", TO Matrix, " with entries in ", TO ZZ, " or ", TO QQ, "(this input is optional)"}
	  },
     Outputs => {
	  "A'" => {"a ", TO Matrix, " with entries in ", TO ZZ, " or ", TO QQ},
	  "B'" => {"a ", TO "Matrix", " with entries in ", TO ZZ, " or ", TO QQ}
	  },
     
     PARA{}, "The input pair ", TT "(A,B)", " describes a rational
     polyhedral cone generated by nonnegative linear combinations of
     the column vectors of ", TT "A", " and containing the linear
     space generated by the column vectors of ", TT "B", ".  Dually,
     the pair ", TT "(A,B)", " describes a rational polyhedral cone as
     the intersection of closed halfspaces; the set of vectors ", 
     TT "x", " satisfying ", TT "(transpose A) * x <= 0", " and ", 
     TT "(transpose B) * x == 0", ".  If the convex cone spans its
     ambient affine space, then the input ", TT "B", " may be omitted.",
     
     PARA{}, "The output pair ", TT "(A',B')", " is the dual description
     of the same cone.  In particular, the output pair ", 
     TT "(A',B')", " is valid input.  If the convex cone spans its ambient
     affine space, then the output ", TT "B'", " will be zero.",
     
     PARA{}, "For example, consider the convex cone generated by the 26
     columns of the following matrix.  Using Fourier-Motzkin
     elimination, we see that this cone is the intersection of 6
     halfspaces and spans 3-space.",
     
     EXAMPLE {
	  "rays = transpose matrix(QQ, {{1,1,6},{1,2,4},{1,2,5},
	       {1,2,6},{1,3,4},{1,3,5},{1,3,6},{1,4,4},{1,4,5},
	       {1,4,6},{1,5,4},{1,5,5},{1,5,6},{1,5,7},{1,6,3},
	       {1,6,4},{1,6,5},{1,6,6},{1,6,7},{1,7,4},{1,7,5},
	       {1,7,6},{1,7,8},{1,8,4},{1,8,5},{1,8,6}})",
    	  "halfspaces = fourierMotzkin rays",
	  "numgens source halfspaces#0",
	  "extremalRays = fourierMotzkin halfspaces",
	  "numgens source extremalRays#0"
	  },
     
     "Using Fourier-Motzkin elimination a second time, we see that
     this cone is in fact generated by 6 vectors.",
     
     PARA{}, "Here are some further examples illustrating fourierMotzkin
     elimination.",
     UL {
	  {TO "Finding the facets of the cyclic polytope"},
	  {TO "Finding the facets of the permutahedron"},	     
	  }
     }

document {     
     Key => "Finding the facets of the cyclic polytope",
     
     "The ", EM "cyclic polytope", " is the convex hull of distinct
     points on the moment curve.  The function ", TT "cyclicPolytope",
     " produces a matrix such that columns generate the cyclic ", 
     TT "d", "-polytope with ", TT "n", " vertices.",
        
     EXAMPLE {
	  "cyclicPolytope = (d,n) -> map(ZZ^d, ZZ^n, (i,j) -> j^(i+1));",
	  "vertices = cyclicPolytope(4,8)"
	  },
     
     PARA{}, "To find the halfspace representation for the convex hull
     of these points, we first pass from 4-space to 5-space.
     Specifically, we make the cyclic polytope into a polyhedral cone
     by placing it a height 1 (we make the extra coordinate the
     zeroeth coordinate).",
	  
     EXAMPLE {
	  "homogenizePolytope = V -> (
     R := ring V;
     n := numgens source V;
     map(R^1, R^n, {toList(n:1)}) || V);",
	  "polyCone = homogenizePolytope vertices",
	  "H = fourierMotzkin polyCone",
	  "halfspaces = H#0",
	  "numgens source halfspaces"
	  },
     
     "Since ", TT "H#1", " is zero, the polyhedral cone spans 5-space.
     The columns in the matrix ", TT "halfspaces", " describe a
     complete minimal system of inequalities for the convex hull of
     these points.  In particular, this polytope has 20 facets.",
          
     PARA{}, "To see the combinatorial structure of this polytope, we
     compute the facet-vertex incidence matrix.",

     EXAMPLE {
	  "inequalityVector = transpose submatrix(halfspaces,{0},)",
	  "inequalityMatrix = transpose submatrix(halfspaces,{1..4},)",
          "ones = map(ZZ^1,ZZ^8,{toList(8:1)})",
	  "M = (inequalityMatrix * vertices) + (ones ** inequalityVector)",
	  "incidence = matrix table(20,8, (i,j) -> if M_(i,j) == 0 then 1 else 0)"
	  },

     "From the rows of the matrix, we see Gale's evenness condition:
     every segment of consecutive ", TT "1", "'s is of even length if
     it is not an initial or a final segment.  For more information,
     see Theorem 0.7 in ",
     HREF("http://www.math.tu-berlin.de/~ziegler/", "Gunter
     M. Ziegler's"), " ", EM "Lectures on Polytopes", ", Graduate
     Texts in Mathematics 152, Springer-Verlag, New York, 1995."
     }

document { 
     Key => "Applications to multigraded polynomial rings",
     Headline => "finding Heft",
     
     "A vector configuration is ", EM "acyclic", " if it has a positive
     linear functional.  Using ", TT "fourierMotzkin", " we can
     determine if a vector configuration has a positive linear
     functional.",

     PARA{}, "Given an acyclic vector configuration (as a list of lists
     of ", TO ZZ, " or ", TO QQ, "), the
     function ", TT "findHeft", " finds a ", TO List, "
     representing a positive linear functional.",
     
     EXAMPLE {
          "findHeft := vectorConfig -> (
     A := transpose matrix vectorConfig;
     B := (fourierMotzkin A)#0;
     r := rank source B;
     heft := first entries (matrix{toList(r:-1)} * transpose B);
     g := gcd heft;
     if g > 1 then heft = apply(heft, h -> h //g);
     heft);"
     },
     	  
     PARA{}, "If a polynomial ring as a multigrading determined by a
     vector configuration, then a positive linear functional plays the
     role of a ", TO Heft, " vector.",

     PARA{}, "For example, ", TT "S", " is the Cox homogeneous
     coordinate ring for second Hirzebruch surface (under an
     appropriate choice of basis for the Picard group).",

     EXAMPLE {
	  "vectorConfig = {{1,0},{-2,1},{1,0},{0,1}}",
	  "hft = findHeft vectorConfig",
	  "S = QQ[x_1,x_2,y_1,y_2, Heft => hft, Degrees => vectorConfig];",
	  "irrelevantIdeal = intersect(ideal(x_1,x_2), ideal(y_1,y_2))",
	  "res (S^1/irrelevantIdeal)",
	  },
     
     "The Betti numbers correspond to the f-vector of the polytope
     associated to the second Hirzebruch surface.",

     PARA{}, "Similarly, ", TT "R", " is the Cox homogeneous coordinate
     ring for the blowup of the projective plane at three points
     (under an appropriate choice of basis for the Picard group).",

     EXAMPLE {
	  "vectorConfig = {{1,0,0,0},{0,1,0,0},{0,-1,1,0},{0,1,-1,1},
	  {1,0,-1,1},{-1,0,0,1}}",
          "hft = findHeft vectorConfig",
	  "R = QQ[x_1..x_6, Heft => hft, Degrees => vectorConfig];",
	  "irrelevantIdeal = ideal(x_3*x_4*x_5*x_6,x_1*x_4*x_5*x_6,x_1*x_2*x_5*x_6,
     x_1*x_2*x_3*x_6,x_2*x_3*x_4*x_5,x_1*x_2*x_3*x_4)",
          "res (R^1/irrelevantIdeal)"
	  },
     
     "Again, the Betti numbers correspond to the f-vector of the
     polytope associated to this toric variety.",
	  
     PARA{}, "For more information about resolutions of the irrelevant
     ideal of a toric variety, see subsection 4.3.6 in ",
     HREF("http://www.math.umn.edu/~ezra/", "Ezra Miller"), " and ",
     HREF("http://math.berkeley.edu/~bernd/","Bernd Sturmfels'"), " ",
     EM "Combinatorial commutative algebra", ", Graduate Texts in
     Mathematics 277, Springer-Verlag, New York, 2005.",

     PARA{}, "For more information about vector configurations is
     subsections 6.2 & 6.4 in ",
     HREF("http://www.math.tu-berlin.de/~ziegler/", "Gunter
     M. Ziegler's"), " ", EM "Lectures on Polytopes", ", Graduate
     Texts in Mathematics 152, Springer-Verlag, New York, 1995."
     }

document {     
     Key => "Finding the facets of the permutahedron",
     
     "The ", EM "permutahedron", " is the convex hull of all vectors
     that are obtained by permuting the coordinates of the vector ",
     TT "(1,2, ..., d)", ".  The function ", TT "permutahedron",
     " produces a matrix such that columns generate the cyclic ", 
     TT "d", " permutahedron in ", TT "(d+1)", "-space.",
        
     EXAMPLE {
	  "permutahedron = d -> transpose matrix permutations toList(1..d+1);",
	  "vertices = permutahedron 3",
	  },
     
     PARA{}, "To find the halfspace representation for permutahedron, we
     first pass from 4-space to 5-space.  Specifically, we make the
     permutahedron into a polyhedral cone by placing it a height 1
     (we make the extra coordinate the zeroeth coordinate).",

     EXAMPLE {
	  "homogenizePolytope = V -> (
     R := ring V;
     n := numgens source V;
     map(R^1, R^n, {toList(n:1)}) || V);",
	  "H = fourierMotzkin homogenizePolytope vertices",
	  "transpose H#1",
	  "halfspaces = H#0;",
	  "numgens source halfspaces",
	  },
     
     "Since ", TT "H#1", " has one column vector, the polyhedral cone
     spans a 4-dimensional subspace of 5-space.  Indeed, the sum of
     the components for each vertex is 10.  The columns in the matrix
     ", TT "halfspaces", " describe a complete minimal system of
     inequalities for permutahedron.  In particular, this polytope has
     14 facets.",
          
     PARA{}, "To see the combinatorial structure of this polytope, we
     compute the facet-vertex incidence matrix.",
     
     EXAMPLE {
	  "inequalityMatrix = transpose submatrix(halfspaces,{1..4},)",
	  "M = inequalityMatrix * vertices",
	  "incidence = matrix table(14,24, (i,j) -> if M_(i,j) == 0 then 1 else 0)",
	  "vertexDegree = map(ZZ^1,ZZ^14,{toList(14:1)}) * incidence",
	  "facets = transpose(incidence * transpose map(ZZ^1,ZZ^24,{toList(24:1)}))"
	  },

     "We see that each vertex has degree 3.  Moreover, there are 8
     hexagonal facets and 6 quadrilateral facets.  For pictures, see
     pages 17-18 in ", HREF("http://www.math.tu-berlin.de/~ziegler/",
     "Gunter M. Ziegler's"), " ", EM "Lectures on Polytopes", ",
     Graduate Texts in Mathematics 152, Springer-Verlag, New York,
     1995."  }
     
TEST ///
C = transpose matrix{{1,1,1,1}}
assert(C == (fourierMotzkin fourierMotzkin C)#0)
///

TEST ///
C = transpose matrix(QQ, {{0,0,1}, {1,0,1}, {0,1,1}})
assert( (entries transpose C) == 
     (entries transpose ((fourierMotzkin fourierMotzkin C)#0)) )
///

TEST ///
C = map(ZZ^3,ZZ^0,0)
H = transpose matrix{{1,0,-1},{0,1,-1}}
P = fourierMotzkin (C,H)
assert(P#0 == C)
assert(P#1 == transpose matrix{{1,1,1}})
///

TEST ///
C = transpose matrix{{1,1,0}, {0,1,1}}
H = transpose matrix{{1,0,-1}}
fourierMotzkin (C,H)
P = fourierMotzkin fourierMotzkin (C,H)
assert(P#0 == transpose matrix{{0,1,1}})
assert(P#1 == H)
///

TEST ///
M =  matrix{{1,1,1,1,1,1,1,1},{ -1,1,-2,2,1,-2,2,-1},{2,2,1,-1,-2,-1,1,-2}}
dualM =  matrix {{ -2, -2, -2, -3, -3, -2, -3, -3}, 
     { -1, 1, 0, -1, 1, 0, -1, 1}, {0, 0, -1, -1, -1, 1, 1, 1}}
assert( (fourierMotzkin M)#0 == dualM)
assert( (fourierMotzkin M_{4..7,0..3})#0 == dualM)
assert( (fourierMotzkin M_{7,0..6})#0 == dualM)
///

TEST ///
crossPoly = transpose matrix {{1, 1, 1, 1, -1}, {1, -1, -1, 1, -1},
     {1, -1, 1, -1, -1}, {1,-1, 1, 1, -1}, {1, 1, -1, -1, 1}, 
     {1, 1, -1, 1, 1}, {1, 1, 1, -1, 1}, {1, 1, 1, 1, 1}, 
     {1, -1, -1, -1, 1}, {1, -1, -1, 1, 1}, {1, -1, 1, -1, 1}, 
     {1, -1, 1, 1, 1}, {1, 1, -1, -1, -1}, {1, 1, -1, 1, -1}, 
     {1, 1, 1, -1, -1}, {1, -1, -1, -1, -1}}
cube =   matrix {{ -1, -1, -1, -1, -1, -1, -1, -1}, 
     { -1, 1, 0, 0, 0, 0, 0, 0}, {0, 0, -1, 1, 0, 0, 0, 0}, 
     {0, 0, 0, 0, -1, 1, 0, 0}, {0, 0, 0, 0, 0, 0, -1, 1}}
assert( (fourierMotzkin crossPoly)#0 == cube)
///

TEST ///
diamond = transpose matrix{
     {1/2, -1, -1},
     {1/2, -1,  1},
     {1/2,  1, -1},
     {1/2,  1,  1}}
dualDiamond = sort transpose matrix {{ -2/1, -1/1, 0/1}, { -2/1, 0/1, -1/1}, 
      { -2/1,1/1, 0/1}, { -2/1, 0/1, 1/1}}
assert( (fourierMotzkin diamond)#0 == dualDiamond)
///

TEST ///
avisIn2 = transpose matrix {{1, -1, 0, -1, 0, 0}, {1, -1, 0, 0, 0, -1}, 
     {1, 0, -1, -1, 0, 0}, {1, 0, -1, 0, -1, 0}, {1, 0, 0, 0, -1, -1},
     {0, -1, 1, 0, 0, 1}, {0, 1, -1, 0, 1, 0}, {0, 0, 0, -1, 1, 1}, 
     {0, 0, 1, 1, -1, 0}, {0, 1, 0, 1, 0, -1}, {2, -1, -1, -1, -1, -1}, 
     {0, 1, 0, 0, 0, 0}, {0, 0, 1, 0, 0, 0}, {0, 0, 0, 1, 0, 0}, 
     {0, 0, 0, 0, 1, 0}, {0, 0, 0, 0, 0, 1}}
avisExt2 = sort transpose matrix {{ -1, 0, 0, 0, 0, 0}, { -1, -1, -1, 0, 0, 0}, 
     { -2, 0, -1, 0, -1, 0}, { -1, 0, 0, -1, -1, 0}, { -2, 0, -1, -1, -1, 0},
     { -2, -1, -1, 0, -1, 0}, { -2, -1, 0, 0, 0, -1}, { -1, 0, 0, -1, 0, -1}, 
     { -2, -1, 0, -1, 0, -1}, { -2, -1, -1, 0, 0, -1}, { -2, -1, -1, 0, -1, -1}, 
     { -4, -2, -3, 0, -1, -2}, { -4, -3, -2, 0, -2, -1}, { -2, -1, 0, -1, -1, -1}, 
     { -2, 0, 0, -1, -1, -1}, { -3, -1, 0, -1, -1, -2}, { -2, 0, -1, -1, -1, -1}, 
     { -3, 0, -1, -1, -2, -1}}
assert( (fourierMotzkin avisIn2)#0 == avisExt2)
///

end

------------------------------------------------------------

restart
uninstallPackage "FourierMotzkin"
path = prepend(homeDirectory | "Code/", path);
installPackage "FourierMotzkin"
check "FourierMotzkin"


