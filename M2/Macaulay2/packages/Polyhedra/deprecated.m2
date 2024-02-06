warned := new MutableHashTable
warning := (name, version) -> if not warned#?name then (
     warned#name = true;
     stderr << "Warning: This method is deprecated and will be removed in version " << version << " of Polyhedra. Please consider using " << name << " instead." << endl;
     )

triangulate = method()
triangulate Polyhedron := P -> (
   warning("regularTriangulation or barycentricTriangulation", "1.11");
   barycentricTriangulation P)


cellDecompose = method(TypicalValue => List)
cellDecompose (Polyhedron,Matrix) := (P,w) -> (
   warning("regularSubdivision", "1.11");
   regularSubdivision (P,w))



--   INPUT : '(M,v,N,w)',  where all four are matrices (although v and w are only vectors), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v and Nx=w} 
--  OUTPUT : 'P', the polyhedron
intersection(Matrix,Matrix,Matrix,Matrix) := {} >> o -> (M,v,N,w) -> (
   warning "polyhedronFromHData";
   polyhedronFromHData(M,v,N,w)
)


--   INPUT : '(M,N)',  two matrices where either 'P' is the Cone {x | Mx<=0, Nx=0} if 'M' and 'N' have the same source space 
--     	    	       or, if 'N' is only a Column vector the Polyhedron {x | Mx<=v} 
--  OUTPUT : 'P', the Cone or Polyhedron
intersection(Matrix,Matrix) := {} >> o -> (M,N) -> (
     -- Checking for input errors
     if ((numColumns M =!= numColumns N and numColumns N =!= 1) or (numColumns N == 1 and numRows M =!= numRows N)) and N != 0*N 
     then error("invalid condition vector for half-spaces");
     -- Decide whether 'M,N' gives the Cone C={p | M*p >= 0, N*p = 0}
     if numColumns M == numColumns N and numColumns N != 1 then (
	  warning("coneFromHData", "1.11");
	  coneFromHData(M,N)
	  -- or the Polyhedron P={p | M*p >= N != 0}
	  )
     else (	
	  warning("polyhedronFromHData", "1.11");
	  polyhedronFromHData(M, N)
	  )
     )

intersection Matrix := {} >> o -> M -> (
   warning("coneFromHData", "1.11");
   coneFromHData M
)

