-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone
compute#Cone = new MutableHashTable

Cone == Cone := (C1,C2) -> (
   contains(C1, C2) and contains(C2, C1)
)

internalConeConstructor = method()
internalConeConstructor HashTable := inputProperties -> (
   resultHash := sanitizeConeInput inputProperties;
   constructTypeFromHash(Cone, resultHash)
)


sanitizeConeInput = method()
sanitizeConeInput HashTable := given -> (
   rayProperties := {inputRays, inputLinealityGenerators, rays, computedLinealityBasis};
   facetProperties := {inequalities, equations, facets, computedHyperplanes};
   remainingProperties := keys given;
   remainingProperties = select(remainingProperties, p -> all(rayProperties, rp -> rp=!=p));
   remainingProperties = select(remainingProperties, p -> all(facetProperties, rp -> rp=!=p));
   result := apply(remainingProperties, rp -> rp=>given#rp);
   for rp in rayProperties do (
      if given#?rp then (
         primitive := makeRaysUniqueAndPrimitive given#rp;
         result = append(result, rp=>primitive)
      )
   );
   for fp in facetProperties do (
      if given#?fp then (
         primitive := makeFacetsUniqueAndPrimitive given#fp;
         result = append(result, fp=>primitive)
      )
   );
   new HashTable from result
)


coneFromMinimalVData = method(TypicalValue => Cone)
coneFromMinimalVData(Matrix, Matrix) := (iRays, linealityGenerators) -> (
     -- checking for input errors
     if numRows iRays =!= numRows linealityGenerators then error("rays and linSpace generators must lie in the same space");
     result := new HashTable from {
         ambientDimension => numRows iRays,
         rays => iRays,
         computedLinealityBasis => linealityGenerators
     };
     internalConeConstructor result
)


coneFromMinimalHData = method(TypicalValue => Cone)
coneFromMinimalHData(Matrix, Matrix) := (ineq, eq) -> (
   if numColumns ineq =!= numColumns eq then error("facets and hyperplanes must lie in same space");
   result := new HashTable from {
      ambientDimension => numColumns ineq,
      facets => ineq,
      computedHyperplanes => eq
   };
   internalConeConstructor result
)


-- PURPOSE : Computing the positive hull of a given set of rays lineality 
--		 space generators
coneFromVData = method(TypicalValue => Cone)

--   INPUT : 'Mrays'  a Matrix containing the generating rays as column vectors
--		 'LS'  a Matrix containing the generating rays of the 
--				lineality space as column vectors
--  OUTPUT : 'C'  a Cone
-- COMMENT : The description by rays and lineality space is stored in C as well 
--		 as the description by defining half-spaces and hyperplanes.
coneFromVData(Matrix,Matrix) := (Mrays,LS) -> (
   if numRows Mrays =!= numRows LS then error("rays and linSpace generators must lie in the same space");
   result := new HashTable from {
      ambientDimension => numRows Mrays,
      inputRays => Mrays,
      inputLinealityGenerators => LS
   };
   internalConeConstructor result
)

--   INPUT : 'M',  a matrix, such that the Cone is given by C={x | Mx>=0} 
--  OUTPUT : 'C', the Cone
coneFromHData = method(TypicalValue => Cone)

coneFromHData Matrix := ineq -> (
   coneFromHData(ineq, map(ZZ^0,ZZ^(numColumns ineq), 0))
)

coneFromHData(Matrix, Matrix) := (ineq, eq) -> (
   if numColumns ineq != numColumns eq then error("Column dimensions of inequalities and equations do not agree.");
   result := new HashTable from {
      inequalities => ineq,
      equations => eq
   };
   return internalConeConstructor result

)



--   INPUT : 'R'  a Matrix containing the generating rays as column vectors
coneFromVData Matrix := R -> (
   r := ring R;
   -- Generating the zero lineality space LS
   LS := map(target R, r^0,0);
   coneFromVData(R,LS)
)


--   INPUT : '(C1,C2)'  two cones
coneFromVData(Cone,Cone) := (C1,C2) -> (
   local iRays;
   local linealityGens;
   if hasProperties(C1, {rays, computedLinealityBasis}) then (
      iRays = rays C1;
      linealityGens = linealitySpace C1;
   ) else if hasProperties(C1, {inputRays, inputLinealityGenerators}) then (
      iRays = getProperty(C1, inputRays);
      linealityGens = getProperty(C1, inputLinealityGenerators);
   ) else (
      iRays = rays C1;
      linealityGens = linealitySpace C1;
   );
   if hasProperties(C2, {rays, computedLinealityBasis}) then (
      iRays = iRays | rays C2;
      linealityGens = linealityGens | linealitySpace C2;
   ) else if hasProperties(C2, {inputRays, inputLinealityGenerators}) then (
      iRays = iRays | getProperty(C2, inputRays);
      linealityGens = linealityGens | getProperty(C2, inputLinealityGenerators);
   ) else (
      iRays = iRays | rays C2;
      linealityGens = linealityGens | linealitySpace C2;
   );
   coneFromVData(iRays, linealityGens)
)


--   INPUT : 'L',   a list of Cones, Polyhedra, rays given by R, 
--     	    	    and (rays,linSpace) given by '(R,LS)'
coneFromVData List := L -> (
   -- Turn everything into cones.
   cones := apply(L, 
      l -> (
         if not instance(l, Cone) then coneFromVData l
         else l
      )
   );
   result := cones#0;
   cones = drop(cones, 1);
   -- Adding the cones is not expensive, since we will not do fourierMotzkin
   -- every time.
   for cone in cones do (
      result = coneFromVData(result, cone);
   );
   result
)
