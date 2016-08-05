







-- PURPOSE : Computing the interior lattice points of a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the interior lattice points
interiorLatticePoints = method(TypicalValue => List)
interiorLatticePoints Polyhedron := (cacheValue symbol interiorLatticePoints)(P -> (
     L := latticePoints P;
     select(L,e -> inInterior(e,P))))










--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isReflexive = method(TypicalValue => Boolean)
isReflexive Polyhedron := (cacheValue symbol isReflexive)(P -> isLatticePolytope P and inInterior(matrix toList(ambDim P:{0}),P) and isLatticePolytope polar P)


-- PURPOSE : Checks if a lattice polytope is reflexive




-- PURPOSE : Checks if the polyhedron is a lattice polytope
--   INPUT : 'P'  a Polyhedron, which must be compact
--  OUTPUT : 'true' or 'false'
-- COMMENT : Tests if the vertices are in ZZ
isLatticePolytope = method()
isLatticePolytope Polyhedron := Boolean => P -> isCompact P and liftable(vertices P,ZZ)

     


-- PURPOSE : Triangulating a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : A list of the simplices of the triangulation. Each simplex is given by a list 
--    	     of its vertices.
--COMMENTS : The triangulation is build recursively, for each face that is not a simplex it takes 
--     	     the weighted centre of the face. for each codim 1 face of this face it either takes the 
--     	     convex hull with the centre if it is a simplex or triangulates this in the same way.
triangulate = method()
triangulate Polyhedron := P -> (
     -- Defining the recursive face triangulation
     -- This takes a polytope and computes all facets. For each facet that is not a simplex, it calls itself
     -- again to replace this facet by a triangulation of it. then it has a list of simplices triangulating 
     -- the facets. Then it computes for each of these simplices the convex hull with the weighted centre of 
     -- the input polytope. The weighted centre is the sum of the vertices divided by the number of vertices.
     -- It returns the resulting list of simplices in a list, where each simplex is given by a list of its 
     -- vertices.
     -- The function also needs the dimension of the Polyhedron 'd', the list of facets of the original 
     -- polytope, the list 'L' of triangulations computed so far and the dimension of the original Polytope.
     -- 'L' contains a hash table for each dimension of faces of the original Polytope (i.e. from 0 to 'n').
     -- If a face has been triangulated than the list of simplices is saved in the hash table of the 
     -- corresponding dimension with the weighted centre of the original face as key.
     recursiveFaceTriangulation := (P,d,originalFacets,L,n) -> (
	  -- Computing the facets of P, given as lists of their vertices
	  F := intersectionWithFacets({(set P,set{})},originalFacets);
	  F = apply(F, f -> toList(f#0));
	  d = d-1;
	  -- if the facets are at least 2 dimensional, then check if they are simplices, if not call this 
	  -- function again
	  if d > 1 then (
	       F = flatten apply(F, f -> (
			 -- Check if the face is a simplex
			 if #f != d+1 then (
			      -- Computing the weighted centre
			      p := (sum f)*(1/#f);
			      -- Taking the hash table of the corresponding dimension
			      -- Checking if the triangulation has been computed already
			      if L#d#?p then L#d#p
			      else (
				   -- if not, call this function again for 'f' and then save this in 'L'
				   (f,L) = recursiveFaceTriangulation(f,d,originalFacets,L,n);
				   L = merge(L,hashTable {d => hashTable{p => f}},(x,y) -> merge(x,y,));
				   f))
			 else {f})));
	  -- Adding the weighted centre to each face simplex
	  q := (sum P)*(1/#P);
	  P = apply(F, f -> f | {q});
	  (P,L));
     -- Checking for input errors
     if not isCompact P then error("The polytope must be compact!");
     n := dim P;
     -- Computing the facets of P as lists of their vertices
     (HS,v) := halfspaces P;
     (hyperplanesTmp,w) := hyperplanes P;
     originalFacets := apply(numRows HS, i -> intersection(HS,v, hyperplanesTmp || HS^{i}, w || v^{i}));
     originalFacets = apply(originalFacets, f -> (
	       V := vertices f;
	       (set apply(numColumns V, i -> V_{i}),set {})));
     -- Making a list of the vertices of P
     P = vertices P;
     P = apply(numColumns P, i -> P_{i});
     if #P == n+1 then {P} else (
	  d := n;
	  -- Initiating the list of already computed triangulations
	  L := hashTable apply(n+1, i -> i => hashTable {});
	  (P,L) = recursiveFaceTriangulation(P,d,originalFacets,L,n);
	  P))



-- PURPOSE : Computing the volume of a full dimensional polytope
--   INPUT : 'P',  a compact polyhedron
--  OUTPUT : QQ, giving the volume of the polytope
volume = method(TypicalValue => QQ)
volume Polyhedron := P -> (
     d := dim P;
     -- Checking for input errors
     if  not isCompact P then error("The polyhedron must be compact, i.e. a polytope.");
     -- If P is not full dimensional then project it down
     if d != ambDim P then (
	  A := substitute((hyperplanes P)#0,ZZ);
	  A = inverse (smithNormalForm A)#2;
	  n := ambDim P;
	  A = A^{n-d..n-1};
	  P = affineImage(A,P));
     -- Computing the triangulation of P
     P = triangulate P;
     -- Computing the volume of each simplex without the dimension factor, by 
     -- taking the absolute of the determinant of |v_1-v_0..v_d-v_0|
     P = apply(P, p -> abs det matrix transpose apply(toList(1..d), i -> flatten entries(p#i - p#0)));
     -- Summing up the volumes and dividing out the dimension factor
     (sum P)/(d!))
	       
-- PURPOSE : Computing a point in the relative interior of a Polyhedron 
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'p',  a point given as a matrix
interiorPoint = method(TypicalValue => Matrix)
interiorPoint Polyhedron := P -> (
     -- Checking for input errors
     if isEmpty P then error("The polyhedron must not be empty");
     Vm := vertices P | promote(rays P,QQ);
     n := numColumns Vm;
     ones := matrix toList(n:{1/n});
     -- Take the '1/n' weighted sum of the vertices
     Vm * ones)


-- PURPOSE : Computing the face of a Polyhedron where a given weight attains its minimum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Polyhedron 'P'
--  OUTPUT : a Polyhedron, the face of 'P' where 'v' attains its minimum
minFace (Matrix,Polyhedron) := (v,P) -> (
   -- Checking for input errors
   if numColumns v =!= 1 or numRows v =!= ambDim(P) then error("The vector must lie in the same space as the polyhedron");
   C := dualCone tailCone P;
   V := vertices P;
   R := rays P;
   LS := linSpace P;
   -- The weight must lie in the dual of the tailcone of the polyhedron, otherwise there is 
   -- no minimum and the result is the empty polyhedron
   if contains(C,v) then (
      -- Compute the values of 'v' on the vertices of 'V'
      Vind := flatten entries ((transpose v)*V);
      -- Take the minimal value(s)
      Vmin := min Vind;
      Vind = positions(Vind, e -> e == Vmin);
      -- If 'v' is in the interior of the dual tailCone then the face is exactly spanned 
      -- by these vertices
      if inInterior(v,C) then convexHull(V_Vind,LS | -LS)
      else (
         -- Otherwise, one has to add the rays of the tail cone that are orthogonal to 'v'
         Rind := flatten entries ((transpose v)*R);
         Rind = positions(Rind, e -> e == 0);
         convexHull(V_Vind,R_Rind | LS | -LS)
      )
   ) else emptyPolyhedron ambDim P
)


-- PURPOSE : Computing the face of a Polyhedron where a given weight attains its maximum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Polyhedron 'P'
--  OUTPUT : a Polyhedron, the face of 'P' where 'v' attains its maximum
maxFace (Matrix,Polyhedron) := (v,P) -> minFace(-v,P)


--   INPUT : '(p,P)',  where 'p' is a point given by a matrix and 'P' is a Polyhedron
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Polyhedron) := (p,P) -> (
     hyperplanesTmp := hyperplanes P;
     hyperplanesTmp = (hyperplanesTmp#0 * p)-hyperplanesTmp#1;
     all(flatten entries hyperplanesTmp, e -> e == 0) and (
	  HS := halfspaces P;
	  HS = (HS#0 * p)-HS#1;
	  all(flatten entries HS, e -> e < 0)))


boundaryMap (ZZ,Polyhedron) := (i,P) -> (
     L1 := facesAsPolyhedra(dim P - i,P);
     L2 := facesAsPolyhedra(dim P - i + 1,P);
     L1 = apply(L1, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     L2 = apply(L2, e -> (Vm := vertices e; apply(numColumns Vm, i -> Vm_{i})));
     transpose matrix apply(L1, l1 -> (
	       apply(L2, l2 -> (
			 if isSubset(set l2,set l1) then (
			      l3 := toList(set l1 - set l2);
			      l3 = apply(l3, e -> position(l1, e1 -> e1 == e));
			      l := #l3; 
			      k := #l2; 
			      (-1)^(k*l + sum l3 - substitute((l^2-l)/2,ZZ))) else 0)))))





   
   
 
 


    

-- PURPOSE : Scaling respectively the multiple Minkowski sum of a polyhedron
--   INPUT : '(k,P)',  where 'k' is a strictly positive rational or integer number and 
--     	    	             'P' is a Polyhedron
--  OUTPUT : The polyehdron 'P' scaled by 'k'
QQ * Polyhedron := (k,P) -> (
   -- Checking for input errors
   if k <= 0 then error("The factor must be strictly positiv");
   vertP := vertices P;
   vertP = promote(vertP, QQ);
   raysP := promote(rays P, QQ);
   linP := promote(linealitySpace P, QQ);
   convexHull(k * vertP, raysP, linP)
)

ZZ * Polyhedron := (k,P) -> promote(k,QQ) * P

-- PURPOSE : Computing the affine hull
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : the affine hull of 'P' as a Polyhedron
affineHull = method(TypicalValue => Polyhedron)
affineHull Polyhedron := P -> (
     M := vertices P;
     R := promote(rays P,QQ);
     -- subtracting the first vertex from all other vertices
     N := (M+M_{0}*(matrix {toList(numColumns M:-1)}))_{1..(numColumns M)-1};
     convexHull(M_{0},N | -N | R | -R));
 

