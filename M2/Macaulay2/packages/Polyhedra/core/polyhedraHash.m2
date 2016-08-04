-- Defining the new type PolyhedraHash
PolyhedraHash = new Type of MutableHashTable
globalAssignment PolyhedraHash

-- This MutableHashTable will store all methods computing properties of
-- polyhedral objects for easy access via the getProperties method. This will make
-- interfacing polymake much easier.
compute = new MutableHashTable

alternative = new MutableHashTable

fourierMotzkinWrapper = method()
fourierMotzkinWrapper(Matrix, Matrix) := (A, B) -> (
   if alternative#?fourierMotzkin then alternative#fourierMotzkin(A, B)
   else fourierMotzkin(A, B)
)


--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim = method(TypicalValue => ZZ)
ambDim PolyhedraHash := X -> (
   getProperty(X, ambientDimension)
)

isSimplicial = method(TypicalValue => Boolean)
isSimplicial PolyhedraHash := X -> (
   getProperty(X, simplicial)
)

isFullDimensional = method(TypicalValue => Boolean)
isFullDimensional PolyhedraHash := X -> (
   getProperty(X, fullDimensional)
)


rays PolyhedraHash := PH -> (
   getProperty(PH, computedRays)
)


linealitySpace = method(TypicalValue => Matrix)
linealitySpace PolyhedraHash := PH -> (
   getProperty(PH, computedLinealityBasis)
)

-- 
-- 	if instance(X,Cone) then (isPointed X and numColumns rays X == dim X)
-- 	else if instance(X,Fan) then all(maxCones X,isSimplicial)
-- 	else if instance(X,Polyhedron) then (isCompact X and numColumns vertices X == dim X + 1)
-- 	else all(maxPolyhedra X,isSimplicial)))


--   INPUT : 'PH'  a Polyhedron, Cone, Fan or Polyhedral Complex
--  OUTPUT : an integer, the dimension of the Polyhedron, Cone, Fan or Polyhedral Complex,
--           where for the last two it is defined as the maximum dimension of the subobjects
dim PolyhedraHash := PH -> (
   getProperty(PH, computedDimension)
)

getProperty = method()
getProperty(PolyhedraHash, Symbol) := (PH, property) -> (
   accessProperty := (cacheValue property)(X -> (
      type := class X;
      << "Computing property " << property << " of " << type << endl;
      if compute#type#?property then (
         return compute#type#property X
      ) else (
         error("No method to compute property ", property," for type ", type, ".")
      )
   ));
   if PH#?property then return PH#property else accessProperty PH
)

setProperty = method()
setProperty(PolyhedraHash, Symbol, Thing) := (PH, property, value) -> (
   if not hasProperty(PH, property) then PH.cache#property = value
   else << "Warning: Property " << property << " already assigned." << endl
)

hasProperty = method()
hasProperty(PolyhedraHash, Thing) := (PH, property) -> (
   hasProperties(PH, {property})
)

hasProperties = method()
hasProperties(PolyhedraHash, List) := (PH, properties) -> (
   givenProperties := getAvailableProperties PH;
   result := apply(properties, p-> positions(givenProperties, g -> g===p));
   all(result, r -> #r > 0)
)

getAvailableProperties = method()
getAvailableProperties PolyhedraHash := PH -> (
   result := keys PH;
   result = flatten { result, keys PH.cache};
   result = select(result, r-> r =!= cache);
   result
)

constructTypeFromHash = method()
constructTypeFromHash(Type, HashTable) := (PType, H) -> (
   result := new PType from {
      symbol cache => new CacheTable
   };
   for key in keys H do (
      << "Setting property " << key << endl;
      setProperty(result, key, H#key);
   );
   result
)

net PolyhedraHash := X -> (
   properties := getAvailableProperties X;
   horizontalJoin flatten (
      "{",
      -- prints the parts vertically
      stack (horizontalJoin \ sort apply(toList properties, 
         property -> (
            val := getProperty(X, property);
            local rhs;
            -- Avoid recursion, e.g. for normalFans
            if instance(val, PolyhedraHash) then
               rhs = class val
            else
               rhs = val;
            (net property, " => ", net rhs)
         )
      )),
      "}" 
   )
)



-- Debug export
export {
   "hasProperties",
   "getAvailableProperties",
   "hasProperty",
   "getProperty"
}


