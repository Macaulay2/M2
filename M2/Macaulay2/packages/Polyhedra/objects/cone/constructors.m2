-- Defining the new type Cone
Cone = new Type of PolyhedralObject
Cone.synonym = "convex rational cone"
globalAssignment Cone
compute#Cone = new MutableHashTable

Cone == Cone := (C1,C2) -> C1 === C2



coneFromRayData = method(TypicalValue => Cone)
coneFromRayData(Matrix, Matrix) := (iRays, linealityGenerators) -> (
     -- checking for input errors
     if numRows iRays =!= numRows linealityGenerators then error("rays and linSpace generators must lie in the same space");
     result := new Cone from {
         ambientDimension => numRows iRays,
         symbol cache => new CacheTable
     };
     setProperty(result, computedRays, iRays);
     setProperty(result, computedLinealityBasis, linealityGenerators);
     result
)


coneFromFacetData = method(TypicalValue => Cone)
coneFromFacetData(Matrix, Matrix) := (ineq, eq) -> (
   if numColumns ineq =!= numColumns eq then error("facets and hyperplanes must lie in same space");
   result := new Cone from {
      ambientDimension => numColumns ineq,
      symbol cache => new CacheTable
   };
   setProperty(result, computedFacets, ineq);
   setProperty(result, computedHyperplanes, eq);
   result
)



-- PURPOSE : Computing the positive hull of a given set of rays lineality 
--		 space generators
posHull = method(TypicalValue => Cone)

--   INPUT : 'Mrays'  a Matrix containing the generating rays as column vectors
--		 'LS'  a Matrix containing the generating rays of the 
--				lineality space as column vectors
--  OUTPUT : 'C'  a Cone
-- COMMENT : The description by rays and lineality space is stored in C as well 
--		 as the description by defining half-spaces and hyperplanes.
posHull(Matrix,Matrix) := (Mrays,LS) -> (
   if numRows Mrays =!= numRows LS then error("rays and linSpace generators must lie in the same space");
   result := new Cone from {
      ambientDimension => numRows Mrays,
      symbol cache => new CacheTable
   };
   setProperty(result, inputRays, Mrays);
   setProperty(result, inputLinealityGenerators, LS);
   result
)

--   INPUT : 'M',  a matrix, such that the Cone is given by C={x | Mx>=0} 
--  OUTPUT : 'C', the Cone
intersection Matrix := M -> (
   r := ring M;
   N := transpose map(source M, r^1, 0); 
   intersection(M, N)
)



--   INPUT : 'R'  a Matrix containing the generating rays as column vectors
posHull Matrix := R -> (
   r := ring R;
   -- Generating the zero lineality space LS
   LS := map(target R, r^1,0);
   posHull(R,LS)
)


--   INPUT : '(C1,C2)'  two cones
--   Q: Is this used anywhere?
posHull(Cone,Cone) := (C1,C2) -> (
   local iRays;
   local linealityGens;
   if hasProperties(C1, {computedRays, computedLinealityBasis}) then (
      iRays = rays C1;
      linealityGens = linealitySpace C1;
   ) else if hasProperties(C1, {inputRays, inputLinealityGenerators}) then (
      iRays = getProperty(C1, inputRays);
      linealityGens = getProperty(C1, inputLinealityGenerators);
   ) else (
      iRays = rays C1;
      linealityGens = linealitySpace C1;
   );
   if hasProperties(C2, {computedRays, computedLinealityBasis}) then (
      iRays = iRays | rays C2;
      linealityGens = linealityGens | linealitySpace C2;
   ) else if hasProperties(C2, {inputRays, inputLinealityGenerators}) then (
      iRays = iRays | getProperty(C2, inputRays);
      linealityGens = linealityGens | getProperty(C2, inputLinealityGenerators);
   ) else (
      iRays = iRays | rays C2;
      linealityGens = linealityGens | linealitySpace C2;
   );
   posHull(iRays, linealityGens)
)
