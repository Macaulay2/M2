-- Convex hulls via Fourier-Motzkin from Ziegler's book

----------------------------------------------
-- Routines that should really be elsewhere --
----------------------------------------------

primitive = (v) -> (
    if class v =!= List or class v#0 =!= ZZ
	then error "expected non-empty list of integers";
	n := #v-1;
	g := v#n;
	while n > 0 do (
	    n = n-1;
	    g = gcd(g, v#n);
		if g === 1 then n=0);
	if g === 1 then v else
	  apply(v, i -> i//g))

lcm = (a,b) -> (a*b)//(gcd(a,b))

lcms = (v) -> (
    -- return the least common multiple of a list of integers
    if class v =!= List or class v#0 =!= ZZ
	then error "expected non-empty list of integers";
	n := #v-1;
	g := v#n;
	while n > 0 do (
	    n = n-1;
	    g = lcm(g, v#n);
		);
	g)

toZZ = (v) -> (
    x := apply(v, a -> denominator a);
	a := lcms x;
	apply(v, i -> (numerator(a*i))))

--------------------------
-- Convex Hull routines --
--------------------------

isFacet = (v, V, a, b) -> (
    -- v is a facet iff the only sets in V which contain
	-- v are V#a, V#b.
	x := positions(V, w -> isSubset(w,v));
	if #x < 2 then error "something is wrong in isFacet";
	#x === 2)		
	
elimination = (ineq, V, spot) -> (
    -- eliminate the last variable in 'ineq'.
	-- 'ineq' is a list of lists of integers, each of 
	-- the same length 'n'.
	-- 'V' is a list of sets, of the same length as 'ineq'.
	-- returns a list of inequalities and a corresponding list
	-- of vertices.
	pos := {};
	neg := {};
	result := {};
	resultV := {};
	m := #ineq;
	n := #ineq#0;
	-- Divide the inequalties into their respective parts
	i := 0;
	while i < m do (
		if ineq#i#spot < 0 then
		    neg = append(neg,i)
		else if ineq#i#spot > 0 then
		    pos = append(pos,i)
		else (
			result = append(result,ineq#i);
			resultV = append(resultV,V#i);
			);
	    i=i+1;
		);
	-- For each pos, neg: check the criterion.  If ok,
	-- add the corresponding inequality to the result,
	-- and the union of the two sets to resultV.
	scan(pos, a -> scan(neg, b -> (
			newV := V#a +  V#b;
			if isFacet(newV, V, a,b) then (
				hi := ineq#a;
				hj := ineq#b;
				ci := -hj#spot;
				cj := hi#spot;
				h := ci*hi + cj*hj;
				result = append(result,primitive h);
				resultV = append(resultV,newV);
			    );
	    )));
	{result, resultV}
	)

conv0 = (A) -> (
     d := numgens target A;
     n := numgens source A;
     -- assumed for now: the last d columns of A are full rank.
     -- THIS ASSUMPTION MUST BE REMOVED!!
     mq := -id_(QQ^d) || transpose substitute(A,QQ);
     mm := map(QQ^d,QQ^n,0) || -id_(QQ^(n));
     B := transpose(mm % mq);
     ineqs0 := entries B;
     ineqs := apply(ineqs0, toZZ);
     V := apply(n, i -> set {i});
     {ineqs, V})

conv = (A) -> (
    -- returns the inequalities defining the facets of the
	-- convex hull of the columns of the matrix 'pts'
	-- 'pts' should be a matrix over ZZ, currently of full
	-- rank, although we will remove this restriction soon.
	-- 
	d := numgens target A;
	n := numgens source A;
    P := conv0 A;
	scan(reverse toList(d..n+d-1), i ->
	    P = elimination(P#0, P#1, i));
	--{matrix P#0, P#1}
	{(matrix P#0)_{0..d-1}, P#1}
	)

conv = (A) -> (
    -- returns the inequalities defining the facets of the
	-- convex hull of the columns of the matrix 'pts'
	-- 'pts' should be a matrix over ZZ, currently of full
	-- rank, although we will remove this restriction soon.
	-- 
	d := numgens target A;
	n := numgens source A;
    P := conv0 A;
	scan(reverse toList(d..n+d-1), i ->
	    P = elimination(P#0, P#1, i));
	--{matrix P#0, P#1}
	transpose ((matrix P#0)_{0..d-1})
	)

--------------
-- Examples --
--------------

addRowOfOnes = (m) -> (
    n := numgens source m;
	m2 := map(ZZ^1, ZZ^n, {toList(n:1)});
	m2 || m)

addColumns = (m) -> (
    n := numgens source m;
	R := ring m;
	ones := map(R^n, R^1, (i,j) -> 1);
	m * ones)
	
permutations = (n) -> (
    if #n === 1 then {n}
	else (
	    flatten apply(#n, i -> (
	        m := drop(n,{i,i});
			apply(permutations m, p -> prepend(n#i,p))))))
		
cyclicPolytope = (d,n) -> map(ZZ^(d+1), ZZ^n, (i,j) -> j^i)

permutahedron = (n) -> (
    addRowOfOnes transpose matrix permutations toList(1..n))

hypersimplex = (d,k) -> (
    x := subsets(d+1,k);
	addRowOfOnes transpose matrix apply(x, s -> (
	    x = set s;
	    apply(d+1, i -> if member(i,x) then 1 else 0))))
///

path = prepend("C:M2Projects:ConvexHulls:", path)
load "conv.m2"

-- a first example to test with 'elimination'

m = matrix(QQ,{{1,1,0},{1,0,1},{0,1,1}})
det m

m^(-1)

m = matrix{{1,1,1,1,1,1},
	       {1,0,0,1,1,1},
           {0,1,0,2,1,-1},
		   {0,0,1,3,-1,0}}
P = conv0 m
time P = elimination(P#0,P#1,5)
time P = elimination(P#0,P#1,4)

m = transpose matrix{{1,3,0},
	{1,2,1},
	{1,1,2},
	{1,0,3},
	{1,2,0},
	{1,1,1},
	{1,0,2},
	{1,1,0},
	{1,0,1},
	{1,0,0}}
time conv m

m = cyclicPolytope(4,8)
m = m_(reverse toList(0..7))
time conv m

m = cyclicPolytope(5,8)
m = m_(reverse toList(0..7))
time conv m

m = cyclicPolytope(4,9)
m = m_(reverse toList(0..8))
time conv m

m = cyclicPolytope(3,14)
m = m_(reverse toList(0..13))
time conv m

m = cyclicPolytope(4,14)
m = m_(reverse toList(0..13))
time conv m

time conv hypersimplex(5,2)
time conv hypersimplex(6,3)

-- A simple cone: find the inequalities, and then the extremal edges.
m = matrix {{-1, -1, 0, 0}, {0, -1, 1, 1}}
m = matrix{{1,1,1,1},{1,1,1,1}}
m = matrix{{1,1,1},{2,1,0},{0,1,2}}
syz m
syz transpose m
conv m
conv conv m
///
