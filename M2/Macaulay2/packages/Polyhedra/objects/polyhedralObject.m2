-- Defining the new type PolyhedralObject
PolyhedralObject = new Type of PolyhedraHash
globalAssignment PolyhedralObject

-- PURPOSE : Giving the defining affine hyperplanes
--   INPUT : 'P'  a Polyhedron or Cone 
--  OUTPUT : '(N,w)', where M and v are matrices and P={x in HS | Nx=w}, where 
--		 HS is the intersection of the defining affine half-spaces
--     for cones, 'w' is omitted.
hyperplanes = method()
hyperplanes PolyhedralObject := P -> P#"hyperplanes"

-- PURPOSE : Giving a basis of the lineality space
linSpace = method(TypicalValue => Matrix)
linSpace PolyhedralObject := P -> linealitySpaceBasis P

linealitySpaceBasis = method(TypicalValue => Matrix)
linealitySpaceBasis PolyhedralObject := Matrix => (cacheValue symbol computedLinealityBasis)(P -> (
      if instance(P, Cone) then applyFittingRule(P, computedLinealityBasis)
      else error "Not implemented yet."
   )
)



-- PURPOSE : Giving the defining affine half-spaces
--   INPUT : 'P'  a Polyhedron or Cone
--  OUTPUT : '(M,v)', where M and v are matrices and P={x in H | Mx<=v}, where 
--		 H is the intersection of the defining affine hyperplanes
--     for cones, 'v' is omitted.
halfspaces = method()
halfspaces PolyhedralObject := P -> P#"halfspaces"

rays PolyhedralObject := Matrix => (cacheValue symbol computedRays)(P ->(
      if instance(P, Cone) then applyFittingRule(P, computedRays)
      else error "Not implemented yet."
   )
)

-- PURPOSE : Computing the f-vector of a polyhedron
--   INPUT : 'P'  a Polyhedron or fan
--  OUTPUT : polyhedral case: a List of integers, starting with the number of vertices and going up in dimension
--           cone case: a List of integers, starting with the number of vertices and going up in dimension
fVector = method(TypicalValue => List)

-- PURPOSE : Computing the faces of codimension 'k' of 'P'
faces = method(TypicalValue => List)


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
