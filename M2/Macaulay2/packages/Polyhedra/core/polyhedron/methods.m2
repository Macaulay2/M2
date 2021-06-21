-- PURPOSE : Computing the inner normalFan of a polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'F',  a Fan, the inner normalFan of 'P'
normalFan = method(TypicalValue => Fan)
normalFan Polyhedron := P -> (
   getProperty(P, computedNormalFan)
)      


vertices Polyhedron := P -> (
   getProperty(P, computedVertices)
)

--   INPUT : '(P,Q)'  two Polyhedra
--  OUTPUT : 'true' or 'false'
isFace(Polyhedron,Polyhedron) := (P,Q) -> (
   -- Checking if the two polyhedra lie in the same space and computing the dimension difference
   if not isEmpty P then (
      CP := getProperty(P, underlyingCone);
      CQ := getProperty(Q, underlyingCone);
      isFace(CP, CQ)
   ) else return ambDim P == ambDim Q
)


-- PURPOSE : Tests if a Polyhedron is empty
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isEmpty Polyhedron := P -> getProperty(P, empty)


-- PURPOSE : Tests if a Polyhedron is compact
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
isCompact = method(TypicalValue => Boolean)
isCompact Polyhedron := P -> getProperty(P, computedCompact)


-- PURPOSE : Computing the lattice points of a compact Polyhedron 
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the lattice points of 'P'
latticePoints = method(TypicalValue => List)
latticePoints Polyhedron := P -> (
   getProperty(P, computedLatticePoints)
)



-- PURPOSE: Getting data from the vertex side that determines polyhedron
--          completely, avoid fourierMotzkin. Always pick best possible data.
getSufficientVertexData = method()
getSufficientVertexData Polyhedron := P -> (
   if hasProperties(P, {computedVertices, rays, computedLinealityBasis}) then (
      return(vertices P, rays P, linealitySpace P)
   ) else if hasProperties(P, {points, inputRays, inputLinealityGenerators}) then (
      return (
         getProperty(P, points),
         getProperty(P, inputRays),
         getProperty(P, inputLinealityGenerators)
      )
   ) else (
      return(vertices P, rays P, linealitySpace P)
   )
)

facesAsPolyhedra = method();
facesAsPolyhedra(ZZ, Polyhedron) := (d, P) -> (
   vertP := vertices P;
   raysP := rays P;
   linP := linealitySpace P;
   result := faces(d, P);
   apply(result, f -> convexHull(vertP_(f#0), raysP_(f#1), linP))
)


hyperplanes Polyhedron := P -> getProperty(P, computedHyperplanes)
linSpace Polyhedron := P -> linealitySpace P
halfspaces Polyhedron := P -> facets P
facets Polyhedron := P -> getProperty(P, facets)


-- PURPOSE : Scaling respectively the multiple Minkowski sum of a polyhedron
--   INPUT : '(k,P)',  where 'k' is a strictly positive rational or integer number and 
--     	    	             'P' is a Polyhedron
--  OUTPUT : The polyhedron 'P' scaled by 'k'
QQ * Polyhedron := (k,P) -> (
   -- Checking for input errors
   if k <= 0 then error("The factor must be strictly positive");
   vertP := vertices P;
   vertP = promote(vertP, QQ);
   raysP := promote(rays P, QQ);
   linP := promote(linealitySpace P, QQ);
   convexHull(k * vertP, raysP, linP)
)

ZZ * Polyhedron := (k,P) -> promote(k,QQ) * P


-- PURPOSE : Checks if the polyhedron is a lattice polytope
--   INPUT : 'P'  a Polyhedron
--  OUTPUT : 'true' or 'false'
-- COMMENT : Tests if the vertices are in ZZ
isLatticePolytope = method()
isLatticePolytope Polyhedron := Boolean => P -> getProperty(P, lattice)


-- PURPOSE : Computing the interior lattice points of a compact Polyhedron
--   INPUT : 'P',  a Polyhedron
--  OUTPUT : 'L',  a list containing the interior lattice points
interiorLatticePoints = method(TypicalValue => List)
interiorLatticePoints Polyhedron := (cacheValue symbol interiorLatticePoints)(P -> (
     L := latticePoints P;
     select(L,e -> inInterior(e,P))))


isWellDefined Polyhedron := P -> getProperty(P, isWellDefined)


nVertices Polyhedron := P -> getProperty(P, nVertices)

cone (Polyhedron) := P->(
   getProperty(P,underlyingCone)
)

dualFaceRepresentationMap Polyhedron := P -> (
   if not isCompact P then error("Only works for bounded polyhedra for now.");
   C := getProperty(P, underlyingCone);
   getProperty(C, facetRayDataConverter)
)

regularTriangulation = method()
regularTriangulation Polyhedron := P -> (
   if not isCompact P then error("Triangulation can only be produced for polytopes (i.e. compact polyhedra).");
   regularFineTriangulation vertices P
)
