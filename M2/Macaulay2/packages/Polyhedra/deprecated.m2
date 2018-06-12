triangulate = method()
triangulate Polyhedron := P -> (
     << "Warning: This method is deprecated and will be removed in version 1.11 of Polyhedra. Please use regularTriangulation or barycentricTriangulation instead." << endl;
barycentricTriangulation P)


cellDecompose = method(TypicalValue => List)
cellDecompose (Polyhedron,Matrix) := (P,w) -> (
     << "Warning: This method is deprecated and will be removed in version 1.11 of Polyhedra. Please use regularSubdivision instead." << endl;
regularSubdivision (P,w))


--   INPUT : '(M,v,N,w)',  where all four are matrices (although v and w are only vectors), such
--     	    	      	  that the polyhedron is given by P={x | Mx<=v and Nx=w} 
--  OUTPUT : 'P', the polyhedron
intersection(Matrix,Matrix,Matrix,Matrix) := (M,v,N,w) -> (
   << "Warning: This method is deprecated and will be removed in version 1.11 of Polyhedra. Please consider using polyhedronFromHData instead." << endl;
   polyhedronFromHData(M,v,N,w)
)


--   INPUT : '(M,N)',  two matrices where either 'P' is the Cone {x | Mx<=0, Nx=0} if 'M' and 'N' have the same source space 
--     	    	       or, if 'N' is only a Column vector the Polyhedron {x | Mx<=v} 
--  OUTPUT : 'P', the Cone or Polyhedron
intersection(Matrix,Matrix) := (M,N) -> (
   << "Warning: This method is deprecated and will be removed in version 1.11 of Polyhedra. Please consider using ";
	-- Checking for input errors
	if ((numColumns M =!= numColumns N and numColumns N =!= 1) or (numColumns N == 1 and numRows M =!= numRows N)) and N != 0*N then 
		error("invalid condition vector for half-spaces");
	-- Decide whether 'M,N' gives the Cone C={p | M*p >= 0, N*p = 0}
	if numColumns M == numColumns N and numColumns N != 1 then (
      << "coneFromHData instead." << endl;
      coneFromHData(M,N)
	-- or the Polyhedron P={p | M*p >= N != 0}
	) else (	
      << "polyhedronFromHData instead." << endl;
      polyhedronFromHData(M, N)
   )
)
   

intersection Matrix := M -> (
   << "Warning: This method is deprecated and will be removed in version 1.11 of Polyhedra. Please consider using coneFromHData instead." << endl;
   coneFromHData M
)


