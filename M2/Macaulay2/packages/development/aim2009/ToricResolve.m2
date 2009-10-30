--Program to perform a resolution of singularities for a NormalToricVariety
--Can be used to generate unimodular triangulations.
--We'll assume that the initial toric variety is complete and projective 
--(ie the triangulation spans all of R^n, and is regular)

--Written by Diane Maclagan, maclagan@math.stanford.edu
--Edited by Christine Berkesch and Alexandra Seceleanu
--Last updated: October 29, 2009.

--Input:  NormalToricVariety
--Output: NormalToricVariety


----------------------------------------------------------------
-- To consider for the future:
----------------------------------------------------------------
-- Need prime # for makePos? If yes, add check. If no, change documentation. 
-- combinatorialStellarSubdivision for non-simplicial cones?
-- maxPrime vs minPrime --experiment


----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
needsPackage "NormalToricVarieties"

----------------------------------------------------------------
--Preliminary procedures
----------------------------------------------------------------

--Procedure to return the largest prime dividing a number 
maxPrime=n->(
     f:=factor(n);
     return(max apply(#f,i->((f#i)#0)));
);

--Procedure to return the smallest prime dividing a number 
minPrime=n->(
     f:=factor(n);
     return(min apply(#f,i->((f#i)#0)));
);

---------------------------------------------------------------

--Procedure to find the mod p representative of a vector v with
--all entries between 0 (inclusive) and p (exclusive)
makePos=(v,pp)->(
	local i,j;
	i=0;
	while i<#v do (
		while v_i < 0 do (
			v=v+pp*(apply(#v,j->(if j==i then 1 else 0)));
		);
		while v_i > pp-1 do (
			v=v-pp*(apply(#v,j->(if j==i then 1 else 0)));
		);
		i=i+1;
	);
	v
)

--------------------------------------------------------------------------------

--returns the dimension of the affine span of a face of the fan of X
--dim (List,NormalToricVariety):= (sigma,X) ->( rank matrix (rays X)_sigma )
--this will now be cached under rank(List)


-- computes the lattice index of a facet in (ambient lattice intersect its linear span)
latticeIndex = method()
latticeIndex (List,NormalToricVariety):= (sigma,X) ->(
	if #sigma>1 then smartMinors matrix (rays X)_sigma
)

--smartMinors should be changed to minors after minors is updated
smartMinors = method()
smartMinors (Matrix):= M ->(
	N := (smithNormalForm M)_0;  --N := hermite M;
	product toList apply(rank N, i-> N_(i,i) )
)

--Do we want this function? It's redundant.
--Procedure to blow up a NormalToricVariety by introducing a ray
blowup = method(TypicalValue=>NormalToricVariety)
blowup (NormalToricVariety,List)  :=  (X,rho)->(
	stellarSubdivision(X,rho)
)

findNewRay = method()
--Strategy: maxPrime
findNewRay (NormalToricVariety,ZZ):=(X,maxInd) ->(
	sigma := (select(1,keys X.cache.index, j -> X.cache.index#j == maxInd))#0;
	--Now find the vector in this sigma to add
	p := maxPrime(floor maxInd);
	R := ZZ/p;
	Ap := substitute(transpose matrix (rays X)_sigma, R);
	K := transpose gens ker Ap;
	k := flatten entries substitute(matrix({(entries(K))_0}),ZZ);
	k = matrix {makePos(k,p)};
	newA := flatten entries (k*matrix (rays X)_sigma);
	newA = toSequence ((1/gcd(newA))*newA);
	newA = toList apply(newA,x->lift(x,ZZ)) 
	--output next ray (as a List) at which to subdivide
)

-- This should call a new function that caches latticeIndex whenever it's computed.
-- computes maximal lattice index of facets of X (X is NOT required to be pure or simplicial)
maxIndex = method()
maxIndex (NormalToricVariety):= X ->(
	if (not X.cache.?index) then X.cache.index = hashTable apply(max X, sigma -> sigma => latticeIndex(sigma,X)) 
	else X.cache.index = hashTable apply(max X, sigma -> (
		if X.cache.index#?sigma then sigma => X.cache.index#sigma else sigma => latticeIndex(sigma,X)));
	max values X.cache.index	
)

-- make a simplicial toric variety smooth
makeSmooth = method()
makeSmooth (NormalToricVariety) := X ->(
	Xsimp := X;
	maxInd := maxIndex Xsimp;
	--Now the main part of the procedure
	count := 0;
	while(maxInd > 1) do (
		count = count +1;	     
		newA := findNewRay(Xsimp,maxInd);
		print("blowup # ", count, " at ", newA, " with facet index ", maxInd);
		time( Xsimp = stellarSubdivision( Xsimp, newA ) );
		--Now update maxInd
		maxInd = maxIndex Xsimp;
	);
	Xsimp
)

resolveSingularities = method() 
resolveSingularities (NormalToricVariety) := X ->(
	Xsimp:=X;
	if isSimplicial X then Xsimp = X else Xsimp = makeSimplicial X;
	if isSmooth Xsimp then Xsimp else makeSmooth Xsimp
)
--First make X simplicial, then make it smooth. 

end


--=========================================================================


PP2 = projectiveSpace 2;
#rays PP2
max PP2

rho = {{-1, -1}, {1, 0}, {0, 1}, {2,0}}
chi = {{0, 1,3}, {0, 2}, {1, 2,3}}
X = normalToricVariety(rho,chi)

rho = {{1,0,1},{0,1,1},{-1,0,1},{0,-1,1}}
chi = {{0,1,2,3}}
X = normalToricVariety(rho,chi)
--fan X
sigma = (max X)_0
A=rays X
M = matrix toList apply(sigma,i->toList A_i)

rho = {{2,0,2},{0,1,1},{-1,0,1},{0,-1,1}}
chi = {{0,1,2,3}}
X = normalToricVariety(rho,chi)
Y = fan X


rho = {{3,-2},{0,1}}
chi = {{0,1}}
X = normalToricVariety(rho,chi)
Y = fan X


rho = {{1,1,1},{1,1,-1},{1,-1,1},{-1,1,1},{1,-1,-1},{-1,1,-1},{-1,-1,1},{-1,-1,-1}}
chi = {{0,1,2,4},{0,2,3,6},{0,1,3,5},{1,4,5,7},{2,4,6,7},{3,5,6,7}}
X = normalToricVariety(rho,chi)
fan X


-- The cube: 
Q = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
time resolveSingularities Q



--===============================================================================
--==Examples===


-- the following are already smooth so applying resolvesingularities will just give same X back
print("Example 1")
X = projectiveSpace 4
time resolveSingularities X

print("Example 2")
X = hirzebruchSurface 2
time resolveSingularities X

print("Example 3")
-- this is not smooth
X = weightedProjectiveSpace {1,2,3};
--resolveSingularities X
time resolveSingularities X
time resolveSingularities2 X

print("Example 4")
U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
time resolveSingularities U

print("Example 5")
--here the maximal cones are the rays 
U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
time resolveSingularities U'

print("Example 6")
--this takes too long
--here makeSimplicial does significant work: codim 2 cones need to be subdiveded
Q = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
time Y1 = resolveSingularities Q	  
time Y2 = resolveSingularities2 Q


---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

doc ///
  Key
	resolveSingularities
	(resolveSingularities, NormalToricVariety)
  Headline
	resolve the singularities of a normal toric variety
  Usage
	Y = resolveSingularities X
  Inputs
	X:NormalToricVariety
  Outputs
	Y:NormalToricVariety
	  a resolution of {\tt X}
  Description
   Text
   Text
   Example
   Text
   Example
--  Caveat
--  SeeAlso
///




doc ///
  Key
	blowup
	(blowup,NormalToricVariety,Matrix)
  Headline
	the blowup of a normal toric variety
  Usage
	blowup(X,v)
  Inputs
	X = NormalToricVariety
	v = Matrix
  Outputs
	Y:NormalToricVariety
	  The normal toric variety obtained by adding the ray defined by {\tt v} to the fan of {\tt X}. 
  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
	The input matrix {\tt v} must be a column vector in the ambient space of the fan of {\tt X}.
--  SeeAlso
///

end
