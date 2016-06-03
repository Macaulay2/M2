-- Defining the new type PolyhedraHash
PolyhedraHash = new Type of MutableHashTable
globalAssignment PolyhedraHash

-- This MutableHashTable will store all methods computing properties of
-- polyhedral objects for easy access via the getProperties method. This will make
-- interfacing polymake much easier.
compute = new MutableHashTable



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
   else error "Property already assigned."
)

hasProperty = method()
hasProperty(PolyhedraHash, Symbol) := (PH, property) -> (
   hasProperties(PH, {property})
)

hasProperties = method()
hasProperties(PolyhedraHash, List) := (PH, properties) -> (
   givenProperties := getAvailableProperties PH;
   result := apply(properties, p-> positions(givenProperties, g -> g==p));
   all(result, r -> #r > 0)
)

getAvailableProperties = method()
getAvailableProperties PolyhedraHash := PH -> (
   result := keys PH;
   result = flatten { result, keys PH.cache};
   result = select(result, r-> r != cache);
   result
)

net PolyhedraHash := X -> (
   properties := getAvailableProperties X;
   horizontalJoin flatten (
      "{",
      -- prints the parts vertically
      stack (horizontalJoin \ sort apply(toList properties, property -> (net property, " => ", net getProperty(X, property)))),
      "}" 
   )
)



-- Debug export
export {
   "hasProperties",
   "getAvailableProperties",
   "hasProperty"
}


