-------------------------------------------------------------------------------
-- For fan and cone
isPointed = method(TypicalValue => Boolean)
-- PURPOSE : Checks if the input is smooth
isSmooth = method(TypicalValue => Boolean)



-------------------------------------------------------------------------------
-- For polyhedron and polyhedral complex

-- PURPOSE : Giving the vertices
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the vertices of P as column vectors
vertices = method()
boundaryMap = method(TypicalValue => Matrix)









-------------------------------------------------------------------------------
-- For polyhedron and cone
minFace = method()
maxFace = method()


-- PURPOSE : Checking if a point is an interior point of a Polyhedron or Cone 
inInterior = method(TypicalValue => Boolean)

 
-- PURPOSE : Tests if the first Polyhedron/Cone is a face of the second Polyhedron/Cone
isFace = method(TypicalValue => Boolean)

-- PURPOSE : Tests whether the intersection of two Cones is a face of both
--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : '(Boolean,Cone)'   (true,the intersection),if their intersection is a face of each and 
--     	                        (false,the intersection) otherwise. If the two cones do not lie in 
--     	    	      	   	the same ambient space it returns the empty polyhedron instead of 
--     	    	      	   	the intersection
areCompatible = method()



-- PURPOSE : Giving the defining affine half-spaces
--   INPUT : 'P'  a Polyhedron or Cone
--  OUTPUT : '(M,v)', where M and v are matrices and P={x in H | Mx<=v}, where 
--		 H is the intersection of the defining affine hyperplanes
--     for cones, 'v' is omitted.
halfspaces = method()

facets = method()

-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)

-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron or Cone 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine half-spaces
--     for cones, 'w' is omitted.
hyperplanes = method()


-------------------------------------------------------------------------------
-- Fan and PolyhedralComplex

-- PURPOSE : Tests if a Fan or PolyhedralComplex is complete
--   INPUT : 'POF'  a Fan or PolyhedralComplex
--  OUTPUT : 'true' or 'false'
isComplete = method(TypicalValue => Boolean)

-- PURPOSE : Checks if the Fan or PolyhedralComplex is of pure dimension
--   INPUT : 'PC'  a Fan or PolyhedralComplex
--  OUTPUT : 'true' or 'false'
isPure = method(TypicalValue => Boolean)


maxObjects = method(TypicalValue => List)
objectsOfDim = method(TypicalValue => List)





-------------------------------------------------------------------------------
-- Other

-- PURPOSE : Computing the sublattice basis for a given matrix of lattice points or for the lattice points
--     	     of a given polytope
sublatticeBasis = method(TypicalValue => Matrix)

-- PURPOSE : Computing a polyhedron as the intersection of affine half-spaces and hyperplanes
intersection = method()
