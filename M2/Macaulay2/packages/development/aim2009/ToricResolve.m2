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


----------------------------------------------------------------

--Procedure to form a matrix out of the vectors of A indexed by sigma
--mat=(A,sigma)->(
--return(matrix apply(sigma,i->A_i)););

----------------------------------------------------------------


--Procedure to return the index of a cone


----------------------------------------------------------------

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
method resolveSingularities (typicalValue => NormalToricVariety)
resolveSingularities :=   X->(
    	A:=rays X;
	if not isSimplicial(X) then  Xsimp=makeSimplicial X else Xsimp=X;
	--Next we add the determinant to each simplex in our data
     	if (not Xsimp.cache.?index) then (
	     Xsimp.cache.index={};
	     maxDet:=0;
	     scan(max Xsimp,sigma->(
		       detSigma=abs(det(matrix toList apply(sigma,i->toList A_i)));
		       maxDet=max(maxDet,detSigma);
		       Xsimp.cache.index=append(Xsimp.cache.index,detSigma);
	     	       );
	   	  );
	     );
	maxDet := max Xsimp.cache.index;
	--Now the main part of the procedure
	while(maxDet>1) do (
		--First find a simplex with the maximum determinant
		simplex:=(max Xsimp)_(position(Xsimp.cache.index,j->(j==maxDet)));
		--Now find the vector in this simplex to add
		p:=maxPrime(maxDet);
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
			  detSigma=abs(det(matrix toList apply(sigma,i->toList A_i)));
			  maxDet=max(maxDet,detSigma);
			  Xsimp.cache.index=append(Xsimp.cache.index,detSigma);
	     		  )
	   	     );
		);
	return Xsimp
);


==========================================================
makeSimplicial = method
makeSimplicial NormalToricVariety  := NormalToricVariety =>  X->(
     Y := fan X;
     done := false;
     while not done do (
	  for i from 3 to dim Y do apply( cones(i,Y), sigma -> if #(rays sigma) > i then (
		    rho := transpose matrix{flatten entries(rays sigma)_0};
		    Y = stellarSubdivision (Y,rho);
		    )
	       );
	  done = true;
	  );
     return normalToricVariety Y
     )


makeSimplicial Fan  := Fan =>  X->(
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




---------------------------------------
-- DOCUMENTATION
---------------------------------------


beginDocumentation()

document {
     Key => {makePos, (makePos, List, ZZ)}
     Headline => "find a mod p representative of a vector with entries between 0 (inclusive) and p (exclusive)",
     Usage => "makePos(v,p)",
     Inputs => {
		"v" => "a List of integers",
		"p" => "an integer", 
		},
     Outputs => {List => "a mod p representative of a vector"},
--     Consequences => {
          -- each effect is a hypertext list
--          },
     "This is used to choose a rays while making a simplicial NormalToricVariety smooth.",
     EXAMPLE {
          "m2code",
          "m2code",
          "m2code"
          },
--     "There can be explanatory prose here in the form of a hypertext list.",
     Caveat => {"The List input should really be a matrix."}
     }



end
