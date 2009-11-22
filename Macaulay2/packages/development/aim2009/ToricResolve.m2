--Program to perform a resolution of singularities for a NormalToricVariety
--Can be used to generate unimodular triangulations.
--(i.e. the triangulation spans all of R^n, and is regular)
--In general, we assume that the input NormalToricVariety does not have 
--quotient singularities. The method will resolve some of these, but not 
--all. (see documentation for makeSmooth and resolveSingularities)
----------------------------------------------------------------

----------------------------------------------------------------
--Written by Diane Maclagan, maclagan@math.stanford.edu
--Edited by Christine Berkesch and Alexandra Seceleanu
--Last updated: November 21, 2009.
----------------------------------------------------------------

----------------------------------------------------------------
-- Questions/Problems:
----------------------------------------------------------------
--I tried to add a Strategy option to findNewRay for (maxPrime, minPrime), 
--but I don't know what to call the input "min" or "max." I used String, 
--but then you must put quotation marks on the input line. (It's currently 
--commented out.)
--
--There is a problem with makeSmooth (and thus also resolveSingularities). 
--It would be fixed if we checked the input variety with "isPrimitive" and 
--then "makePrimitive" (if the first is false). Greg had said he wanted to 
--assume there were no quotient singularities, but maybe this would change 
--his mind. 
----------------------------------------------------------------

----------------------------------------------------------------
-- To consider for the future:
----------------------------------------------------------------
-- Add: Strategy options for (maxPrime, minPrime) in findNewRay? 
--	(Then experiment with resolutions.)
-- Think about: Is it necessary to input a prime # to makePos? 
-- Later implement: combinatorialStellarSubdivision for non-simplicial cones?
----------------------------------------------------------------

----------------------------------------------------------------
needsPackage "NormalToricVarieties"
----------------------------------------------------------------
----------------------------------------------------------------
--Internal procedures (no documentation)
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

-- Procedure to compute the maximal lattice index for the facets of X.
-- Note: (X is NOT required to be pure or simplicial)
maxIndex = method()
maxIndex (NormalToricVariety):= X ->(
	max apply(max X,sigma-> latticeIndex(sigma,X))
)

-------------------------------------------------------------------------
--This is already in NormalToricVarieties, but it doesn't load correctly 
--for me unless it's here.
-------------------------------------------------------------------------
-- This returns the primitive vector of a vector in list form.
makePrimitive = method()
makePrimitive List := w -> (g = gcd w; apply(w, e -> e//g))

-------------------------------------------------------------------------
--This is currently an internal procedure, but a possible Strategy option
--could be added (currently commented out).
-------------------------------------------------------------------------
--Procedure to find a new ray (as a List) at which to subdivide.
findNewRay = method()
findNewRay (NormalToricVariety,ZZ):=(X,maxInd) ->(
	sigma := (select(1,max X, C -> latticeIndex(C,X) == maxInd))#0;
	--Now find the vector in this sigma to add
	p := maxPrime(floor maxInd);
	R := ZZ/p;
	Ap := substitute(transpose matrix (rays X)_sigma, R);
	K := transpose gens ker Ap;
	k := flatten entries substitute(matrix({(entries(K))_0}),ZZ);
	k = matrix {makePos(k,p)};
	newA := flatten entries (k*matrix (rays X)_sigma);
	makePrimitive newA --output new vector in list form
)
--
-- Procedure to find a new ray (as a List) at which to subdivide.
--findNewRay = method() 
--findNewRay (NormalToricVariety,ZZ,String) := (X,maxInd,options) ->(
--	sigma := (select(1,max X, C -> latticeIndex(C,X) == maxInd))#0;
--	--Now find the vector in this sigma to add
--	p := 0;
--	if options == "min" then p = minPrime(floor maxInd) else if options == "max" then p = maxPrime(floor maxInd); ----add error if neither strategy is chosen?
--	R := ZZ/p;
--	Ap := substitute(transpose matrix (rays X)_sigma, R);
--	K := transpose gens ker Ap;
--	k := flatten entries substitute(matrix({(entries(K))_0}),ZZ);
--	k = matrix {makePos(k,p)};
--	newA := flatten entries (k*matrix (rays X)_sigma);
--	makePrimitive newA) --output new vector in list form
----Default is max
--findNewRay (NormalToricVariety,ZZ) := (X,maxInd) ->(
--	findNewRay (X,maxInd,"max")
--)
--
--Combining maxIndex with this new method, one could play with 
--different resolutions by introducing different rays one at a time.

----------------------------------------------------------------
--Procedures
----------------------------------------------------------------
-- resolve a simplicial normal toric variety
makeSmooth = method()
makeSmooth (NormalToricVariety) := X ->(
	Xsimp := X; 
	maxInd := maxIndex Xsimp;
	--Now the main part of the procedure
	count := 0;
	while(maxInd > 1) do (
		count = count +1;	     
		newA := findNewRay(Xsimp,maxInd);
		print concatenate{"blowup #",toString count," at ",toString newA," with lattice index ",toString maxInd};
		Xsimp = stellarSubdivision( Xsimp, newA );
		--Now update maxInd
		maxInd = maxIndex Xsimp;
	);
	Xsimp
)
-- THERE IS A PROBLEM: If X is one-dimensional, this doesn't work properly. 
-- For example, 	X = weightedProjectiveSpace {1,2}; 
-- 			Y = makeSmooth X;
--			isSmooth Y --false
--			X === Y --true

resolveSingularities = method() 
resolveSingularities (NormalToricVariety) := X ->(
	Xsimp:=X;
	if isSimplicial X then Xsimp = X else Xsimp = makeSimplicial X;
	if isSmooth Xsimp then Xsimp else makeSmooth Xsimp
)
end

---------------------------------------
-- DOCUMENTATION
---------------------------------------
beginDocumentation()

doc ///
  Key
	makeSmooth
	(makeSmooth, NormalToricVariety)
  Headline
	resolve a simplicial normal toric variety
  Usage
	Y = makeSmooth X
  Inputs
	X:NormalToricVariety
  Outputs
	Y:NormalToricVariety
  Description
	Resolves the singularities of a simplicial {\tt X}, 
	yielding a smooth {\tt Y}.
  Text
	Given a simplicial @TO NormalToricVariety@ {\tt X}, 
	makeSmooth {\tt X} is obtained by successive 
	@TO stellarSubdivision@s of the fan of X until each 
	cone in the fan is contained in some basis for the 
	ambient lattice. 
  Text
	As a simple example, consider the affine (simplicial) 
	@TO NormalToricVariety@ determined by the 
	two-dimensional cone in \RR^2 with rays (4,-1) and (0,1) and 
	another with rays (3,-2) and (0,1). 
  Example
	U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	V = makeSmooth U;
	expression V
	isSmooth V
  Example
	X = normalToricVariety({{3,-2},{0,1}},{{0,1}});
	Y = makeSmooth X;
	expression Y
  Text
	We now consider a small @weightedProjectiveSpace@. 
  Example
	X = weightedProjectiveSpace {1,2,3};
	Y = makeSmooth X;
	expression Y
	isSmooth Y
  Text
	If {\tt X} is already smooth, makeSmooth X will return {\tt X}.
  Example
	X = projectiveSpace 5;
	Y = makeSmooth X;
	X === Y
  Text
	Here is a three-dimensional affine non-simplicial 
	normalToricVariety {\tt X}. To make it smooth, we 
	first apply @TO makeSimplicial@ to {\tt X}. 
	These two methods combine to give the command 
	@TO resolveSingularities@.
  Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	Y = makeSimplicial X;
	Z = makeSmooth Y;
	expression Z
	isSmooth Z
	U = resolveSingularities X;
	U === Z	
  Text
	Here is the normalToricVariety whose fan is determined 
	by the cube centered at the origin in three dimensions 
	with vertices whose entries are +1 and -1.	
  Example
	X = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	Y = makeSimplicial X;
	Z = makeSmooth Y;
	expression Z
	isSmooth Z
  Text
	The input variety need not be pure-dimensional.
  Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,1},{1,0,-1}},{{0,1,2,3},{0,4}});
	Y = makeSmooth X;
	Z = makeSimplicial Y;
	expression Z
--------------------------
--------------------------
  Text
	THERE IS A PROBLEM: If X is one-dimensional, this doesn't work properly. 
  Example
	X = normalToricVariety({{2}},{{0}}); 
 	Y = makeSmooth X;
	isSmooth Y --false
	X === Y --true
  Example
	X = weightedProjectiveSpace {1,2}; 
 	Y = makeSmooth X;
	isSmooth Y --false
	X === Y --true
--------------------------
--------------------------
  Caveat
	It is assumed that {\tt X} is simplicial.
  SeeAlso
	TO isSmooth
	TO makeSmooth
	TO makeSimplicial
	TO resolveSingularities
	TO stellarSubdivision
///

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
	This function returns the resolution of X obtained 
	by applying @TO makeSimplicial@ and then @TO makeSmooth@. 
  Text
	A simplicial @TO NormalToricVariety@ 
	{\tt X} is smooth exactly when 
	its fan is composed of simplicial cones whose generators 
	are subsets of bases of the ambient lattice.
  Text
	As a simple example, consider the affine (simplicial) 
	@TO NormalToricVariety@ {\tt X} determined by the cone 
	with rays (4,-1) and (0,1) in \ZZ^2. 
  Example
	U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	V = resolveSingularities U;
	expression V
	isSmooth V
  Text
	The input variety need not be full-dimensional.
  Example
	U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
	V' = resolveSingularities U';
	expression V'
	isSmooth V'
	U' === V' --true
--  Example
--	--Again, we see quotient singularities are not resolved.
--	U' = normalToricVariety({{8,-2},{0,1}},{{0},{1}});
--	V' = resolveSingularities U';
--	expression V'
--	isSmooth V' --false
--	U' === V' --true
--
--
  Text 
	Showing again that the input need not be 
	full-dimensional, this example would also 
	be a good one on which to try the Strategy 
	(min,max) option.
  Example
	U' = normalToricVariety({{-2,3,0},{1,0,0},{0,-1,0},{0,0,1}},{{0,1},{0,2},{1,2},{2,3}});
	V' = resolveSingularities U';
	expression V'
	isSmooth V'
	U' === V' 
  Text
	Now consider a small @weightedProjectiveSpace@. 
  Example
	X = weightedProjectiveSpace {1,2,3};
	Y = resolveSingularities X;
	expression Y
  Text
	If {\tt X} is already smooth, resolveSingularities X will return {\tt X}.
  Example
	X = projectiveSpace 5;
	Y = resolveSingularities X;
	X === Y
  Text
	Here is a three-dimensional affine non-simplicial 
	normalToricVariety {\tt X}. 
  Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,-1}},{{0,1,2,3}});
	Y = resolveSingularities X;
	expression Y
	isSmooth Y
  Text
	Here is the normalToricVariety whose fan is determined 
	by the cube centered at the origin in three dimensions 
	with vertices whose entries are +1 and -1.	
  Example
	X = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
	Y = resolveSingularities X;
	expression Y
  Text
	The input variety need not be pure-dimensional.
  Example
	X = normalToricVariety({{1,0,0},{0,1,0},{0,0,1},{1,1,1},{1,0,-1}},{{0,1,2,3},{0,4}});  	Y = resolveSingularities X;
	expression Y
--------------------------
--------------------------
  Text
	THERE IS A PROBLEM: Because of the problems with 
	makeSmooth, this method resolves higher dimensional 
	quotient singularities if the variety has dimension 
	at least two. 
  Example
	X = weightedProjectiveSpace {1,2}; 
 	Y = makeSmooth X;
	isSmooth Y --false
	X === Y --true
  Example
	X = normalToricVariety({{2,0},{0,1}},{{0,1}})
	Y = resolveSingularities X --blowup #1 at {1, 0} with lattice index 2
	expression Y --{{2, 0}, {0, 1}, {1, 0}} , {{1, 2}}
--------------------------
--------------------------
Caveat
	The PROBLEM with one-dimensional varieties in 
	@TO makeSmooth@ affects resolveSingularities.
	Also, it is assumed that the input does not have 
	quotient singularities.
  SeeAlso
	TO makeSimplicial
	TO makeSmooth
	TO stellarSubdivision
///

--------------------------------------------------------------------------
-- This method has already been added. Is more documentation needed?
--------------------------------------------------------------------------
dim (List,NormalToricVariety):= (sigma,X) ->(
	if (not X.cache.?cones) then X.cache.cones = new MutableHashTable;
	if (not X.cache.cones#?sigma) then (
		X.cache.cones#sigma = if #sigma>1 then (
			N := (smithNormalForm matrix (rays X)_sigma)_0;
			(rank N,  product toList apply(rank N, i-> N_(i,i)))
			) else (1,1));
	X.cache.cones#sigma#0
)

--------------------------------------------------------------------------
-- The method latticeIndex was already added. Documentation for it is below.
--------------------------------------------------------------------------
latticeIndex = method()
latticeIndex (List,NormalToricVariety):= (sigma,X) ->(
	if (not X.cache.?cones) then X.cache.cones = new MutableHashTable;
	if (not X.cache.cones#?sigma) then ( 
		X.cache.cones#sigma = if #sigma>1 then (
			N := (smithNormalForm matrix (rays X)_sigma)_0;
			(rank N,  product toList apply(rank N, i-> N_(i,i)))
			) else (1,1));
	X.cache.cones#sigma#1
)

doc ///
  Key
	latticeIndex
	(latticeIndex,List,NormalToricVariety)
  Headline
	the index in the ambient lattice of a lattice generated 
	by primitive vectors in the fan of a normal toric variety
  Usage
	m = latticeIndex(sigma, X)
  Inputs
	X:NormalToricVariety
	sigma:List
	  of integers indexing a subset of (rays X)
  Outputs
	m:ZZ
	  The index in the ambient lattice of the lattice generated by the rays indexed by {\tt sigma}.
  Description
	Given a cone sigma in a @TO normalToricVariety@, 
	the lattice (or group) generated by the integer vectors 
	in sigma need not coincide with the collection of 
	integer vectors found in its linear span. This method
	computes the number of (translated) copies it takes to 
	cover this a priori larger collection of integral points. 
  Text
	A cone corresponds to a nonsingular variety when 
	it is simplicial and has latticeIndex equal to 1.
  Text
	We provide an example of an affine singular 
	@TO normalToricVariety@, as well as a complete one. 
  Example
	U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
	sigma = (max U)#0;
	latticeIndex(sigma,U)
	tau = {0};
	latticeIndex(tau,U)
  Text
	Here we see a smooth divisor within a singular variety. 
  Example
	X = weightedProjectiveSpace {1,2,3,5,17};
	sigma = (max X)#0;
	latticeIndex(sigma,X)
	tau = {0,1,2};
	latticeIndex(tau,X)	
  Caveat
	It is not checked that {\tt sigma} actually defines a face of the fan of X.
--  SeeAlso
///

-------------------------------------------------------------
--We no longer use this, but might it be useful for someone?
-------------------------------------------------------------
--smartMinors should be changed to minors after minors is updated
smartMinors = method()
smartMinors (ZZ,Matrix):= (r,M) ->(
	N := (smithNormalForm M)_0;  --N := hermite M;
	product toList apply(r, i-> N_(i,i) )
)

smartMinors (Matrix):= M ->(
	N := (smithNormalForm M)_0;  --N := hermite M;
	product toList apply(rank N, i-> N_(i,i) )
)

----------------------------------------------
-- This function is now unnecessary.
----------------------------------------------
blowup = method(TypicalValue=>NormalToricVariety)
blowup (NormalToricVariety,List)  :=  (X,rho)->(
	stellarSubdivision(X,rho)
)

doc ///
  Key
	blowup
	(blowup,NormalToricVariety,Matrix)
  Headline
	the blowup of a normal toric variety
  Usage
	Y = blowup(X,v)
  Inputs
	X = NormalToricVariety
	v = List
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


---------------------------------------
-- TEST
---------------------------------------

TEST ///
PP4 = projectiveSpace 4;
assert(makeSmooth PP4 === PP4)
assert(resolveSingularities PP4 === PP4)
assert(apply(max PP4,i-> latticeIndex(i,PP4)) == apply(max PP4,i-> 1))
///

TEST ///
PP0 = projectiveSpace 0;
assert(makeSmooth PP0 === PP0)
assert(resolveSingularities PP0 === PP0)
assert(apply(max PP0,i-> latticeIndex(i,PP0)) == apply(max PP0,i-> 1))
///

TEST ///
FF2 = hirzebruchSurface 2;
assert(makeSmooth FF2 === FF2)
assert(resolveSingularities FF2 === FF2)
assert(apply(max FF2,i-> latticeIndex(i,FF2)) == apply(max FF2,i-> 1))
///

TEST ///
X = weightedProjectiveSpace {1,2,3};
assert(makeSmooth X === normalToricVariety({{-2,3},{1,0},{0,-1},{-1,2},{0,1},{-1,1}},{{3,4},{1,4},{0,3},{2,5},{0,5},{1,2}}))
assert(resolveSingularities X === normalToricVariety({{-2,3},{1,0},{0,-1},{-1,2},{0,1},{-1,1}},{{3,4},{1,4},{0,3},{2,5},{0,5},{1,2}}))
assert(apply(max X,i-> latticeIndex(i,X)) == {3,2,1})
///

TEST ///
U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
assert(rays U == {{4,-1},{0,1}})
assert(max U == {{0,1}})
assert(makeSmooth U === normalToricVariety({{4,-1},{0,1},{1,0}},{{1,2},{0,2}}))
assert(resolveSingularities U === normalToricVariety({{4,-1},{0,1},{1,0}},{{1,2},{0,2}}))
assert(apply(max U,i-> latticeIndex(i,U)) == {4})
///

TEST ///
U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
assert(rays U' == {{4,-1},{0,1}})
assert(max U' == {{0},{1}})
assert(makeSmooth U' === U')
assert(resolveSingularities U' === U')
assert(apply(max U',i-> latticeIndex(i,U')) == apply(max U',i-> 1))
///

end