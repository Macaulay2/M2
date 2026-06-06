dualFaceRepresentationMap Cone := C -> (
   getProperty(C, facetRayDataConverter)
)

facesAsCones = method();
facesAsCones(ZZ, Cone) := (d, C) -> (
   raysC := rays C;
   linC := linealitySpace C;
   result := faces(d, C);
   apply(result, f -> coneFromVData(raysC_f, linC))
)


-- PURPOSE : Checks if the input is smooth
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isSmooth Cone := {} >> o -> C -> getProperty(C, smooth)


-- PURPOSE : Tests if a Cone is pointed
--   INPUT : 'C'  a Cone
--  OUTPUT : 'true' or 'false'
isPointed Cone := C -> (
   getProperty(C, pointed)
)


hilbertBasis = method(Options => true)
hilbertBasis Cone := List => {} >> o -> (C -> (
      if isPointed C then getProperty(C, computedHilbertBasis)
      else error("Hilbert basis not implemented for non-pointed cones yet.")
   )
)


--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Cone 'C'
--  OUTPUT : a Cone, the face of 'P' where 'v' attains its maximum
maxFace (Matrix,Cone) := (v,C) -> minFace(-v,C)


-- PURPOSE : Computing the face of a Cone where a given weight attains its minimum
--   INPUT : '(v,P)',  a weight vector 'v' given by a one column matrix over ZZ or QQ and a 
--     	     	       Cone 'C'
--  OUTPUT : a Cone, the face of 'P' where 'v' attains its minimum
minFace (Matrix,Cone) := (v,C) -> (
     -- Checking for input errors
     if numColumns v =!= 1 or numRows v =!= ambDim(C) then error("The vector must lie in the same space as the polyhedron");
     R := rays C;
     LS := linSpace C;
     C = dualCone C;
     -- The weight must lie in the dual of the cone, otherwise there is 
     -- no minimum and the result is the empty polyhedron
     if contains(C,v) then (
	  -- Take the rays of the cone that are orthogonal to 'v'
	  Rind := flatten entries ((transpose v)*R);
	  Rind = positions(Rind, e -> e == 0);
	  coneFromVData(R_Rind,LS))
     else emptyPolyhedron ambDim C)   


-- PURPOSE : Computing an interior vector of a cone
--   INPUT : 'C',  a Cone
--  OUTPUT : 'p',  a point given as a matrix 
interiorVector = method(TypicalValue => Matrix)
interiorVector Cone := C -> (
     Rm := rays C;
     if numColumns Rm == 0 then map(ZZ^(ambDim C),ZZ^1,0)
     else (
	  ones := matrix toList(numColumns Rm:{1});
	  -- Take the sum of the rays
	  iv := Rm * ones;
	  transpose matrix apply(entries transpose iv, w -> (g := abs gcd w; apply(w, e -> e//g)))));

--   INPUT : '(p,C)',  where 'p' is a point given by a matrix and 'C' is a Cone
--  OUTPUT : 'true' or 'false'
inInterior (Matrix,Cone) := (p,C) -> (
     hyperplanesTmp := hyperplanes C;
     all(flatten entries(hyperplanesTmp*p), e -> e == 0) and (
	  HS := halfspaces C;
	  all(flatten entries(HS*p), e -> e > 0)))

--   INPUT : '(C1,C2)'  two Cones
--  OUTPUT : 'true' or 'false' depending on whether C1 is a face of C2.
isFace(Cone,Cone) := (C1,C2) -> (
   c := dim C2 - dim C1;
   -- Checking if the two cones lie in the same space and the dimension difference is positive
   if ambDim(C1) == ambDim(C2) and c >= 0 then (
      raysC2 := rays C2;
      linC2 := linealitySpace C2;
      raysC1 := rays C1;
      linC1 := linealitySpace C1;
      if not (image linC2 == image linC1) then return false;
      rcm := rayCorrespondenceMap(raysC1, linC1, raysC2);
      L := sort for k in keys rcm list (
         p := rcm#k;
         if p == -1 then return false;
         p
      );
      goodFaces := faces(c, C2);
      any(goodFaces, f -> sort f === L)
   )
   else false
)

     

-- PURPOSE : Computing the dual cone
--   INPUT : 'C',  a Cone
--  OUTPUT : The dual Cone, which is {v | v*c>=0 forall c in C}
dualCone = method(TypicalValue => Cone)
dualCone Cone := C -> (
   result := new CacheTable;
   if hasProperty(C, inequalities) then result#inputRays = transpose getProperty(C, inequalities);
   if hasProperty(C, equations) then result#inputLinealityGenerators = transpose getProperty(C, equations);
   if hasProperty(C, inputRays) then result#inequalities = transpose getProperty(C, inputRays);
   if hasProperty(C, inputLinealityGenerators) then result#equations = transpose getProperty(C, inputLinealityGenerators);
   if hasProperty(C, rays) then result#facets = transpose getProperty(C, rays);
   if hasProperty(C, computedLinealityBasis) then result#computedHyperplanes = transpose getProperty(C, computedLinealityBasis);
   if hasProperty(C, facets) then result#rays = transpose getProperty(C, facets);
   if hasProperty(C, computedHyperplanes) then result#computedLinealityBasis = transpose getProperty(C, computedHyperplanes);
   return new Cone from {ambientDimension => ambDim C, cache => result}
)

-- PURPOSE: Getting data from the ray side that determines cone completely,
--          avoid fourierMotzkin. Always pick best possible data.
getSufficientRayData = method()
getSufficientRayData Cone := C -> (
   if hasProperties(C, {rays, computedLinealityBasis}) then (
      return (rays C, linealitySpace C)
   ) else if hasProperties(C, {inputRays, inputLinealityGenerators}) then (
      return (getProperty(C, inputRays), getProperty(C, inputLinealityGenerators))
   ) else (
      return (rays C, linealitySpace C)
   );
)



hyperplanes Cone := C -> getProperty(C, computedHyperplanes)
linSpace Cone := P -> linealitySpace P
halfspaces Cone := P -> facets P
facets Cone := C -> getProperty(C, facets)

isWellDefined Cone := C -> getProperty(C, isWellDefined)
