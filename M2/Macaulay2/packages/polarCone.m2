-- Fourier-Motzkin  elimination and related routines.
-- Greg Smith & Mike Stillman
-- last updated: 16 June 2000
--
-- routines in this file: primitive
--                        lcm
--                        toZZ
--                        isNotRedundant
--                        fourierMotzkin
--                        polarCone
--
------------------------------------------------------------
-- Routines for passing between ZZ and QQ.
------------------------------------------------------------

-- divides a list of integers by their gcd.
primitive := (L) -> (
     -- L should be a list of integers.
     if class L#0 =!= ZZ
     then error "expected non-empty list of integers";
     n := #L-1;
     g := L#n;
     while n > 0 do (
	  n = n-1;
	  g = gcd(g, L#n);
	  if g === 1 then n = 0
	  );
     if g === 1 then L 
     else apply(L, i -> i // g)
     );

-- determines the least common multiple
lcm2 := (a,b) -> (a*b) // (gcd(a,b));
lcm := (L) -> (
     -- L should be a list of integers
     R := ring L#0;
     l := 1_R;
     scan(L, i -> (l = lcm2(l, i)));
     l
     );

-- converts a list of QQ to ZZ by multiplying by a common 
-- denominator.
toZZ := (L) -> (
     -- L should be a list of QQ elements
     if class L#0 =!= QQ
     then error "expected non-empty list of 'QQ'";
     d := apply(L, e -> denominator e);
     l := lcm d;
     apply(L, e -> (numerator(l*e)))
     );

------------------------------------------------------------
-- Main functions
------------------------------------------------------------

-- determines if an inequality (halfspace) is redundant. 
isNotRedundant := (raySet, allRaySets, iter) -> (
     -- raySet: a list of integers
     -- allRaySets: a list of lists of integers
     -- iter: number of iterations through Fourier-Motzkin.
     x := 0;
     -- if the number of elements in 'raySet' exceeds the 
     -- number of 'iter+1', then the inequality is 
     -- redundant; see Exercise 2.15 (iv) in Zeigler.
     if (#raySet > iter+1) then x = 3;
     -- if the set 'raySet' contains the set of any other
     -- inequality in this iteration, then the new 
     -- inequality is redundant; see Exercise 2.15 (i) in 
     -- Zeigler.
     k := 0;
     m := #allRaySets;
     while ((x < 3) and (k < m)) do (
	  if isSubset(allRaySets#k, raySet) then x = x + 1;
	  k = k + 1;
	  );
     x === 2
     );


-- eliminates the last variable in 'halfspaces' using
-- the double description version of Fourier-Motzkin 
-- elimination.
fourierMotzkin := (halfspaces, rays, spot, iter) -> (
     -- halfspaces: list of lists of integers.  Each list defines 
     --    one inequality.
     -- rays: list of lists of integers.  Each list contains the
     --    indices of the original rays that maximize the corresponding
     --    inequality in 'halfspaces'.
     -- spot: an integer, which is the variable being eliminated.
     -- iter: the number of times through Fourier-Motzkin so far.
     -- The returned value:
     --    is a list of two items:
     --    a) a list of lists of integers: the new inequalities.
     --    b) a list of lists of integers: the new rays.
     pos := {};
     neg := {};
     newPlanes := {};
     newRays := {};
     m := #halfspaces;  -- also #rays
     n := #halfspaces#0;
     -- divide the inequalities into three groups.
     k := 0;
     while k < m do (
	  if halfspaces#k#spot < 0 then 
	  neg = append(neg, k)
	  else if halfspaces#k#spot > 0 then 
	  pos = append(pos, k)
	  else (
	       newPlanes = append(newPlanes, halfspaces#k);
	       newRays = append(newRays, rays#k);
	       );
	  k = k + 1;
	  );	    
     -- generate new irredundant inequalities.
     --<< "    " << #pos * #neg << endl;
     scan(pos, i -> 
	  scan(neg, j -> (
		    raySet := rays#i +  rays#j;
		    if isNotRedundant(raySet, rays, iter) 
		    then (
			 hi := halfspaces#i;
			 hj := halfspaces#j;
			 ci := -hj#spot;
			 cj := hi#spot;
			 h := ci*hi + cj*hj;
			 newPlanes = append(newPlanes, 
			      primitive h);
			 newRays = append(newRays, raySet);
			 );
		    )));
     {newPlanes, newRays}
     );


-- computes the polar cone.
polarCone = method();
polarCone(Matrix,Matrix) := (A,E) -> (
     --A := L#0;
     --E := L#1;
     d := numgens target A;
     n := numgens source A;
     AT := transpose A;
     -- if the cone is not full-dimensional, we restrict to
     -- a subspace.
     Q := null;
     implicitEqs := syz AT;
     if not (implicitEqs == 0) then (
	  M := substitute(leadTerm implicitEqs, QQ);
	  AT = compress transpose(substitute(A, QQ) % M);
	  n = numgens target AT;
	  Q = compress (id_(QQ^d) % M);
	  rowsOfQ := entries Q;
	  Q = matrix apply(rowsOfQ, toZZ);
	  d = numgens source AT;
	  );     
     -- express the cone as an intersection of halfspaces
     -- in more variables.
     equalities := -id_(QQ^d) || substitute(AT, QQ);
     inequalities := map(QQ^d, QQ^n, 0) || -id_(QQ^(n));
     B := transpose(inequalities % equalities);
     ineqs := entries B;
     halfspaces := apply(ineqs, toZZ);
     rays := apply(n, i -> set {i});
     -- successive projections eliminate the extra 
     -- variables.
     doubleD := {halfspaces, rays};
     scan(reverse toList(d..n+d-1), i -> (
	       doubleD = fourierMotzkin(doubleD#0, 
		    doubleD#1, i, n+d-i);
	       --<< i << " " << #(doubleD#0) << " " << #((doubleD#0#0)) << endl;
	       )
	  );
     -- output formating.
     if (doubleD#0 =!= {}) then 
       halfspaces = transpose ((matrix doubleD#0)_{0..d-1})
     else  
       halfspaces = map(ZZ^d, ZZ^n, 0);
     --
     if (Q =!= null) then (
	  halfspaces = Q * halfspaces;
	  E = compress(implicitEqs | E);
	  );
     (halfspaces, E)
     --  
     --if (Q === 1) then {halfspaces, E}
     --else {Q*(halfspaces), compress(implicitEqs | E)}
     );

polarCone(Matrix) := (A) -> polarCone(A,map(target A,ZZ^0,0));

document { (polarCone, Matrix),
     Synopsis => (),
     }


document { (polarCone, Matrix,Matrix),
     Headline => "find the polar to a convex polyhedral cone",
     Usage => {
	  TT "polarCone(m,n)", " -- find the polar cone"
	  },
     Synopsis => {
	  "C = polarCone(D,E)",
	  "D" => {"an r by c matrix over ZZ"},
	  "E" => {"an r by d matrix over ZZ"},
	  "C" => {"a sequence of two matrices ", TT "D', E'", "both having
	       d rows."
	       }
	  },
     PARA,
     "Given a cone in R^d, {x in R^d | there exists y in R^c, blah blah blah}",
     EXAMPLE {
	  "D = matrix{{}}",
	  "E = ...",
	  "C = polarCone(D,E)",
	  },
     "The polar cone of C is the original cone",
     EXAMPLE "polarCone C",
     "Also mention: where the algorithm is from.  Author.",
     CAVEAT=>"D and E should be matrices over ZZ.  If they are matrices over QQ, then...",
     "Also mention: how to get convex hull of a set of points...  IE have several examples."
     }


------------------------------------------------------------
-- Examples
------------------------------------------------------------
TEST ///
-- planar example: see section 1.2 in Ziegler.
load "polarCone.m2"
ineqs = {{-1,-4,9},
     {-2,-1,4},
     {1,-2,0},
     {1,0,-4},
     {2,1,-11},
     {-2,6,-17},
     {-6,-1,6}};
points = {{1,6,2},
     {1,2,1},
     {7,8,2},
     {4,3,1},
     {4,2,1},
     {6,3,2}};
ineqsM = transpose matrix ineqs;
dualIneqs = set entries transpose (polarCone ineqsM)#0;
assert(isSubset(set points, dualIneqs) === true)
assert(isSubset(dualIneqs, set points) === true)
///


TEST ///
-- cube is dual to the octahedron
load "polarCone.m2"
octa = {{1,1,1,1},
     {-1,1,1,1},
     {1,-1,1,1},
     {1,1,-1,1},
     {-1,-1,1,1},
     {-1,1,-1,1},
     {1,-1,-1,1},
     {-1,-1,-1,1}};
cube = {{0,0,-1,-1},
     {0,-1,0,-1},
     {-1,0,0,-1},
     {0,0,1,-1},
     {0,1,0,-1},
     {1,0,0,-1}};
octaM = transpose matrix octa;
dualOcta = set entries transpose (polarCone octaM)#0;
assert(isSubset(set cube, dualOcta) === true)
assert(isSubset(dualOcta, set cube) === true)
cubeM = transpose matrix cube;
dualCube = set entries transpose (polarCone cubeM)#0;
assert(isSubset(set octa, dualCube) === true)
assert(isSubset(dualCube, set octa) === true)
///

///
-- not full-dimensional example: image of cyclic polytope.
load "polarCone.m2"
cyclicPolytope = (d,n) -> map(ZZ^(d+1), ZZ^n, (i,j) -> j^i);
C = cyclicPolytope(4,8)
P = polarCone C
M = id_(ZZ^5) || random(ZZ^3, ZZ^5)
P = polarCone (M*C)
///


///
-- traveling salesman polytope:
load "polarCone.m2"

STSP = (n) -> (
     if (n === 3) then (
	  {{1,2,3}}
	  )
     else (
	  J := {};
	  L := STSP(n-1);
	  scan(L, i -> (
		    scan(0..(n-2), k -> (
		    	      v := {};
		    	      scan(0..k, j -> (
					v = append(v, i#j))
				   );
		    	      v = append(v,n);
		    	      scan((k+1)..(n-2), j -> (
					v = append(v, i#j))
				   );
			      J = append(J, v);	      
			      )
		    	 );
	       	    )
	       );
	  J
	  )
     );

charVector = (J) -> (
     m := binomial(#J#0,2)+1;
     points := map(QQ^m, QQ^1, 0);
     E := id_(QQ^(m-1)) || map(QQ^1, QQ^(m-1), i -> 1);
     scan(J, L -> (
     	       vector := map(QQ^m, QQ^1, 0);
     	       first := L#0;
     	       place := 0;
     	       local second;
     	       scan(1..(#L-1), k -> (
	       		 second = L#k;
	       		 if (second > first) then (
	       	    	      place = (first-1)*(#L)-
			      binomial(first,2)+
	       	    	      (second-first-1);
	       	    	      )
	       		 else (
	       	    	      place = (second-1)*(#L)-
			      binomial(second,2)+
	       	    	      (first-second-1);
	       	    	      );
	       		 vector = vector + (id_(QQ^m))_{place};
	       		 first = second;
	       		 )
     	  	    );
     	       second = L#0;
     	       place = (second-1)*(#L)-binomial(second,2)+
	       (first-second-1);
     	       vector = vector + (E)_{place};
	       points = points | vector;
     	       )
	  );
     compress points
     );
     
P = charVector(STSP(5));
transpose P
benchmark "polarCone P"
P = substitute(P,ZZ)
time polarCone P
-- PORTA = ~0.34  M2 = 0.3875
P = charVector(STSP(6));
transpose P
P = substitute(P,ZZ)
benchmark "polarCone P"
time polarCone P
-- PORTA = ~1.25  M2 = 169.89
///

///
-- Added by MES
load "polarCone.m2"
polarCone(transpose matrix{{1,1,0},{1,0,0},{0,1,1},{0,0,0}})
polarCone oo

polarCone(transpose matrix{{1,1,1,1}})
polarCone oo -- BUG: need minimal generators for the kernel...
polarCone oo -- same here.

polarCone(transpose matrix(QQ,{{1,1,1,1}})) -- BUG: doesn't work yet.

polarCone(transpose matrix{{0}}) -- BUG: array index out of bounds
polarCone(transpose matrix{{0,0}}) -- same problem.

polarCone(transpose matrix{{1,1}},transpose matrix{{1,0}}) -- is this correct?
polarCone oo

polarCone(transpose matrix{{1,1,1}}, transpose matrix{{1,2,3,4}}) -- check for this right off?
polarCone(transpose matrix(QQ,{{1,1,1}}), transpose matrix{{1,2,3,4}}) -- check for this right off?

polarCone(transpose matrix{{1,1,1},{-1,-1,-1}})
polarCone oo -- BUG: array index out of bounds


///
