-- Defining the new type PolyhedralObject
PolyhedralObject = new Type of MutableHashTable
globalAssignment PolyhedralObject

-- This MutableHashTable will store all methods computing properties of
-- polyhedral objects for easy access via the getProperties method. This will make
-- interfacing polymake much easier.
compute = new MutableHashTable
alternative = new MutableHashTable

export{
   "loadAlternative",
   "dropAlternatives"
}


loadAlternative = method()
loadAlternative String := name -> (
   if name == "lrs" then insertAlternatives(lrs)
)

dropAlternatives = method()
installMethod(dropAlternatives, o -> (alternative = new MutableHashTable))

insertAlternatives = method()
insertAlternatives MutableHashTable := newAlternatives -> (
   mergeMutableHashTables(alternative, newAlternatives)
)


mergeMutableHashTables = method()
mergeMutableHashTables(MutableHashTable, MutableHashTable) := (old, given) -> (
   for key in keys given do (
      if not old#?key then (
         old#key = given#key
      ) else if instance(old#key, MutableHashTable) and instance(given#key, MutableHashTable) then (
         mergeMutableHashTables(old#key, given#key);
      ) else if instance(old#key, MethodFunction) and instance(given#key, MethodFunction) then (
         -- Warning?
         old#key = given#key
      )
   )
)



fourierMotzkinWrapper = method()
fourierMotzkinWrapper(Matrix, Matrix) := (A, B) -> (
   if debugLevel > 2 then << "Executing fourierMotzkin." << endl;
   if alternative#?fourierMotzkin then (
      if debugLevel > 2 then << "Using alternative fourierMotzkin." << endl;
      alternative#fourierMotzkin(A, B)
   )
   else fourierMotzkin(A, B)
)


--   INPUT : 'P'  a Polyhedron 
--  OUTPUT : an integer, the dimension of the ambient space
ambDim = method(TypicalValue => ZZ)
ambDim PolyhedralObject := X -> getProperty(X, ambientDimension)

isSimplicial = method(TypicalValue => Boolean)
isSimplicial PolyhedralObject := X -> getProperty(X, simplicial)


isFullDimensional = method(TypicalValue => Boolean)
isFullDimensional PolyhedralObject := X -> getProperty(X, fullDimensional)

rays PolyhedralObject := PO -> getProperty(PO, computedRays)

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

polyhedralClass = method()
polyhedralClass PolyhedralObject := PO -> (
   if instance(PO, Cone) then Cone
   else if instance(PO, Polyhedron) then Polyhedron
   else if instance(PO, Fan) then Fan
   else if instance(PO, PolyhedralComplex) then PolyhedralComplex
   else class PO
)


getProperty = method()
getProperty(PolyhedralObject, Symbol) := (PO, property) -> (
   accessProperty := (cacheValue property)(X -> (
      polyhedralType := polyhedralClass X;
      type := class X;
      if debugLevel > 3 then << "Computing property " << property << " of " << polyhedralType << endl;
      if alternative#?type and alternative#type#?property then (
         if debugLevel>2 then << "Using alternative method for computing " << property << " of " << type << endl;
         -- If the user has defined a derived type and made a method for
         -- computing this property, use this.
         return alternative#type#property X
      ) else if alternative#?polyhedralType and alternative#polyhedralType#?property then (
         if debugLevel>2 then << "Using loaded method for computing " << property << " of " << polyhedralType << endl;
         -- If an alternative method has been loaded, use this
         return alternative#type#property X
      ) else if compute#polyhedralType#?property then (
         if debugLevel>2 then << "Using default method for computing " << property << " of " << polyhedralType << endl;
         -- If there is a default method, use this
         return compute#polyhedralType#property X
      ) else (
         error("No method to compute property ", property," for polyhedralType ", polyhedralType, ".")
      )
   ));
   if PO#?property then return PO#property else accessProperty PO
)

setProperty = method()
setProperty(PolyhedralObject, Symbol, Thing) := (PO, property, value) -> (
   if not hasProperty(PO, property) then PO.cache#property = value
   else << "Warning: Property " << property << " already assigned." << endl
)

hasProperty = method()
hasProperty(PolyhedralObject, Thing) := (PO, property) -> (
   hasProperties(PO, {property})
)

hasProperties = method()
hasProperties(PolyhedralObject, List) := (PO, properties) -> (
   givenProperties := getAvailableProperties PO;
   result := apply(properties, p-> positions(givenProperties, g -> g===p));
   all(result, r -> #r > 0)
)

getAvailableProperties = method()
getAvailableProperties PolyhedralObject := PO -> (
   result := keys PO;
   result = flatten { result, keys PO.cache};
   result = select(result, r-> r =!= cache);
   result
)

constructTypeFromHash = method()
constructTypeFromHash(Type, HashTable) := (PType, H) -> (
   result := new PType from {
      symbol cache => new CacheTable
   };
   for key in keys H do (
      if debugLevel > 3 then << "Setting property " << key << endl;
      setProperty(result, key, H#key);
   );
   result
)

net PolyhedralObject := X -> (
   properties := getAvailableProperties X;
   horizontalJoin flatten (
      "{",
      -- prints the parts vertically
      stack (horizontalJoin \ sort apply(toList properties, 
         property -> (
            val := getProperty(X, property);
            local rhs;
            -- Avoid recursion, e.g. for normalFans
            if not (instance(val, Matrix) or instance(val, Vector)) then
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


