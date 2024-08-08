-------------------------------------------------------------------------------
-- For all objects

--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim = method(TypicalValue => ZZ)
ambDim PolyhedralObject := X -> getProperty(X, ambientDimension)

isSimplicial = method(TypicalValue => Boolean)
isSimplicial PolyhedralObject := X -> getProperty(X, simplicial)


-- This method is never triggered, presumably because M2 applies isWellDefined for MutableHashTables
-- isWellDefined PolyhedralObject := X -> getProperty(X, isWellDefined)


isFullDimensional = method(TypicalValue => Boolean)
isFullDimensional PolyhedralObject := X -> ambDim X == dim X


-- PURPOSE : Giving the rays
--   INPUT : 'PO'  a PolyhedralObject
--  OUTPUT : a Matrix, containing the rays of PO as column vectors
rays PolyhedralObject := {} >> o -> PO -> getProperty(PO, rays)

linealitySpace = method(TypicalValue => Matrix)
linealitySpace PolyhedralObject := PO -> getProperty(PO, computedLinealityBasis)


isWellDefined PolyhedralObject := Ph -> true

-- 
-- 	if instance(X,Cone) then (isPointed X and numColumns rays X == dim X)
-- 	else if instance(X,Fan) then all(maxCones X,isSimplicial)
-- 	else if instance(X,Polyhedron) then (isCompact X and numColumns vertices X == dim X + 1)
-- 	else all(maxPolyhedra X,isSimplicial)))


--   INPUT : 'PO'  a Polyhedron, Cone, Fan or Polyhedral Complex
--  OUTPUT : an integer, the dimension of the Polyhedron, Cone, Fan or Polyhedral Complex,
--           where for the last two it is defined as the maximum dimension of the subobjects
dim PolyhedralObject := PO -> getProperty(PO, computedDimension)

-- PURPOSE : Computing the faces of codimension 'k' of 'P'
faces = method(TypicalValue => HashTable)
faces PolyhedralObject := PO -> getProperty(PO, computedFacesThroughRays)
--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'PO'  a polyhedralObject
--  OUTPUT : a List, containing the indices of rays used for the faces
faces(ZZ, PolyhedralObject) := (k,PO) -> (
   result := faces PO;
   if result#?k then result#k
   else {}
)

-- PURPOSE : Computing the f-vector of a polyhedron
--   INPUT : 'P'  a Polyhedron or fan
--  OUTPUT : polyhedral case: a List of integers, starting with the number of vertices and going up in dimension
--           cone case: a List of integers, starting with the number of vertices and going up in dimension
fVector = method(TypicalValue => List)
fVector PolyhedralObject := PO -> getProperty(PO, computedFVector)




-------------------------------------------------------------------------------
-- For fan and cone
isPointed = method(TypicalValue => Boolean)
-- isSmooth = method(TypicalValue => Boolean)



-------------------------------------------------------------------------------
-- For polyhedron and polyhedral complex

-- PURPOSE : Giving the vertices
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : a Matrix, containing the vertices of P as column vectors
vertices = method()
nVertices = method(TypicalValue => ZZ)








-------------------------------------------------------------------------------
-- For polyhedron and cone
minFace = method()
maxFace = method()

-- Translate faces given via ray indices to representation via facet indices
dualFaceRepresentationMap = method();

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

latticeVolume = method(TypicalValue => QQ)
