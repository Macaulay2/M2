--Program to perform a resolution of singularities for a toric variety
--Can be used to generate unimodular triangulations.
--We'll assume that the initial toric variety is complete and projective 
--(ie the triangulation spans all of R^n, and is regular)

--Written by Diane Maclagan, maclagan@math.stanford.edu
--Last updated: November 9, 2002

--Input:  A list "A" of points (the first lattice points on the 1D rays of the 
--fan, and a list "Delta" of simplices in the cones.  The points are lists of 
--numbers.
--Vertices should be numbered from zero onward
--For example:  A={{1,0},{0,1},{-1,-1}}, Delta={{0,1},{1,2},{2,0}} 
--is the (smooth) toric variety P^2.

--Output:  A list {A,Delta}, where A is the list of rays in the smooth 
--fan, and Delta is the list of simplices.


----------------------------------------------------------------
----------------------------------------------------------------
--Preliminary procedures
----------------------------------------------------------------

--Procedure to return the largest prime dividing a number 
maxPrime=n->(
     f:=factor(n);
     return(max apply(#f,i->((f#i)#0)));
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

----------------------------------------------------------------

makeSimplicial = method(TypicalValue => NormalToricVariety)
makeSimplicial (NormalToricVariety):= NormalToricVariety => X ->(
     Y := fan X;
     return normalToricVariety(makeSimplicial Y)
     )


makeSimplicial (Fan)  := Fan =>  X->(
     Y := X;
     done := false;
     while not done do (
	  for i from 3 to dim Y do apply( cones(i,Y), sigma -> if #(rays sigma) > i then (
		    rho := transpose matrix{flatten entries(rays sigma)_0};
		    Y = stellarSubdivision (Y,rho);
		    )
	          );
  	     done = true;
	     );
     return Y
     )

blowup = method(TypicalValue=>NormalToricVariety)
blowup (NormalToricVariety,Matrix)  :=  (X,rho)->(
     Y := fan X;
     normalToricVariety stellarSubdivision (Y,rho)    
     )

-------------------------------------------------------------------------------------------


--need to make work for non max cones
resolveSingularities = method (TypicalValue => NormalToricVariety)
resolveSingularities (NormalToricVariety) := NormalToricVariety =>   (X)->(
    	if not isSimplicial(X) then  Xsimp=makeSimplicial X else Xsimp=X;
	A:=rays Xsimp;
	--Next we add the determinant to each simplex in our data
     	if (not Xsimp.cache.?index) then (
	     Xsimp.cache.index={};
	     maxDet:=0;
	     scan(max Xsimp,sigma->(
		       detSigma=if #sigma>1 then abs(det(matrix toList apply(sigma,i->toList A_i)))else 1;
		       maxDet=max(maxDet,detSigma);
		       Xsimp.cache.index=append(Xsimp.cache.index,detSigma);
	     	       )
	   	  );
	     );
	maxDet := max Xsimp.cache.index;
	--Now the main part of the procedure
	while(maxDet>1) do (
		--First find a simplex with the maximum determinant
		simplex:=(max Xsimp)_(position(Xsimp.cache.index,j->(j==maxDet)));
		--Now find the vector in this simplex to add
		p:=maxPrime(floor maxDet);
		R:=ZZ/p;
		Ap:=substitute(transpose matrix toList apply(simplex,i->toList A_i), R);
		K:=transpose gens ker Ap;
		k:=flatten entries substitute(matrix({(entries(K))_0}),ZZ);
		k=matrix {makePos(k,p)};
		newA= flatten entries (k*matrix(toList apply(simplex,i->toList A_i)));
		gcdnewA=gcd(newA);
		newA=toSequence ((1/gcdnewA)*newA);
		newA=apply(newA,x->lift(x,ZZ));
		Xsimp = blowup(Xsimp,transpose matrix {toList newA});
		A = rays Xsimp;
		--Now update the maxDet
		maxDet=0;
		Xsimp.cache.index={};
		scan(max Xsimp,sigma->(
			  detSigma=if # sigma>1 then abs(det(matrix toList apply(sigma,i->toList A_i))) else 1;
			  maxDet=max(maxDet,detSigma);
			  Xsimp.cache.index=append(Xsimp.cache.index,detSigma);
	     		  )
	   	     );
		);
	return Xsimp
)


--=====================================================

--==Examples===

-- the following are already smooth so applying resolvesingularities will just give same X back
X = projectiveSpace 4
X = hirzebruchSurface 2

-- this is not smooth
X = weightedProjectiveSpace {1,2,3};
resolveSingularities X
time resolveSingularities X

U = normalToricVariety({{4,-1},{0,1}},{{0,1}});
resolveSingularities U

--here the maximal cones are the rays 
U' = normalToricVariety({{4,-1},{0,1}},{{0},{1}});
resolveSingularities U'

--this takes too long
--here makeSimplicial does significant work: codim 2 cones need to be subdiveded
Q = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});
resolveSingularities Q


---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

doc ///
  Key	
	makePos
	(makePos, List, ZZ)
  Headline
	find a mod p representative of a vector with entries between 0 (inclusive) and p (exclusive)
  Usage
	makePos(v,p)
  Inputs
	v:List
	  a List of integers
	p:ZZ
	  an integer
  Outputs
	w:List 
	  a mod p representative of {\tt v}
  Description
   Text
	This is used to find primitive vectors while resolving the singularities of a simplicial normal toric variety.
   Text
   Example
   Text
   Example
  Caveat The List input/output should really be a matrix.
--  SeeAlso
///


doc ///
  Key
	makeSimplicial
	(makeSimplicial, NormalToricVariety)
	(makeSimplicial, Fan)
  Headline
	make simplicial
  Usage
	makeSimplicial X	
  Inputs
	X:NormalToricVariety or Fan
  Outputs
	Y:NormalToricVariety or Fan 
	  depends on input -----WHAT IS THE CORRECT WAY TO DOCUMENT THIS?
  Description
   Text
   Text
   Example
   Text
   Example
  Caveat
  SeeAlso
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
