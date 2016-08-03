-- Defining the new type PolyhedralObject
PolyhedralObject = new Type of PolyhedraHash
globalAssignment PolyhedralObject

-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron or Cone 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine half-spaces
--     for cones, 'w' is omitted.
hyperplanes = method()
hyperplanes PolyhedralObject := PO -> (
   getProperty(PO, computedHyperplanes)
)


-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)
linSpace PolyhedralObject := P -> linealitySpace P




-- PURPOSE : Giving the defining affine half-spaces
--   INPUT : 'P'  a Polyhedron or Cone
--  OUTPUT : '(M,v)', where M and v are matrices and P={x in H | Mx<=v}, where 
--		 H is the intersection of the defining affine hyperplanes
--     for cones, 'v' is omitted.
halfspaces = method()
halfspaces PolyhedralObject := P -> facets P

facets = method()
facets PolyhedralObject := PO -> (
   getProperty(PO, computedFacets)
)



-- PURPOSE : Computing the f-vector of a polyhedron
--   INPUT : 'P'  a Polyhedron or fan
--  OUTPUT : polyhedral case: a List of integers, starting with the number of vertices and going up in dimension
--           cone case: a List of integers, starting with the number of vertices and going up in dimension
fVector = method(TypicalValue => List)
fVector PolyhedralObject := PO -> (
   getProperty(PO, computedFVector)
)

-- PURPOSE : Computing the faces of codimension 'k' of 'P'
faces = method(TypicalValue => List)
faces PolyhedralObject := PO -> (
   getProperty(PO, computedFacesThroughRays)
)
--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a cone
--  OUTPUT : a List, containing the indices of rays used for the faces
faces(ZZ,PolyhedralObject) := (k,PO) -> (
   result := faces PO;
   if result#?k then result#k
   else {}
)


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
