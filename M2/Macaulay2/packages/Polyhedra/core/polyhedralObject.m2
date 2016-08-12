
hyperplanes PolyhedralObject := PO -> (
   getProperty(PO, computedHyperplanes)
)


linSpace PolyhedralObject := P -> linealitySpace P




halfspaces PolyhedralObject := P -> facets P

facets PolyhedralObject := PO -> (
   getProperty(PO, computedFacets)
)



fVector PolyhedralObject := PO -> (
   getProperty(PO, computedFVector)
)

faces PolyhedralObject := PO -> (
   getProperty(PO, computedFacesThroughRays)
)
--   INPUT : 'k'  an integer between 0 and the dimension of
--     	     'C'  a cone
--  OUTPUT : a List, containing the indices of rays used for the faces
faces(ZZ, PolyhedralObject) := (k,PO) -> (
   result := faces PO;
   if result#?k then result#k
   else {}
)


