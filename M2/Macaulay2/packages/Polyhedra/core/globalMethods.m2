--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim = method(TypicalValue => ZZ)
ambDim PolyhedralObject := X -> getProperty(X, ambientDimension)

isSimplicial = method(TypicalValue => Boolean)
isSimplicial PolyhedralObject := X -> getProperty(X, simplicial)


isFullDimensional = method(TypicalValue => Boolean)
isFullDimensional PolyhedralObject := X -> getProperty(X, fullDimensional)


-- PURPOSE : Giving the rays
--   INPUT : 'PO'  a PolyhedralObject
--  OUTPUT : a Matrix, containing the rays of PO as column vectors
rays PolyhedralObject := PO -> getProperty(PO, rays)

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
faces = method(TypicalValue => MutableHashTable)
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
