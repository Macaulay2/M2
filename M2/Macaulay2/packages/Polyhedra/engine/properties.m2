-- Defining the new type PolyhedralObject
PolyhedralObject = new Type of MutableHashTable
globalAssignment PolyhedralObject

-- This MutableHashTable will store all methods computing properties of
-- polyhedral objects for easy access via the getProperties method. This will make
-- interfacing polymake much easier.
compute = new MutableHashTable
alternative = new MutableHashTable


fourierMotzkinWrapper = method()
fourierMotzkinWrapper(Matrix, Matrix) := (A, B) -> (
   if debugLevel > 2 then << "Executing fourierMotzkin." << endl;
   if alternative#?fourierMotzkin then (
      if debugLevel > 2 then << "Using alternative fourierMotzkin." << endl;
      alternative#fourierMotzkin(A, B)
   ) else (
      (R, L) := fourierMotzkin(A, B);
      (R, linealityWorkaround L)
   )
)


linealityWorkaround = method()
linealityWorkaround Matrix := M -> (   
   -- << numRows M << " " << numColumns M << endl;
   if numColumns M == 0 then M
   else sort transpose matrix apply(entries transpose hermite M, m -> primitive m)
)



polyhedralClass = method()
polyhedralClass PolyhedralObject := PO -> (
   if instance(PO, Cone) then Cone
   else if instance(PO, Polyhedron) then Polyhedron
   else if instance(PO, Fan) then Fan
   else if instance(PO, PolyhedralComplex) then PolyhedralComplex
   else class PO
)


getProperty = method()
getProperty(PolyhedralObject, Thing) := (PO, property) -> (
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
setProperty(PolyhedralObject, Thing, Thing) := (PO, property, value) -> (
   if not hasProperty(PO, property) then PO.cache#property = value
   else if debugLevel > 3 then << "Warning: Property " << property << " already assigned." << endl
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
   if PO#?cache then result = flatten { result, keys PO.cache}
   else << "Warning: Your PolyhedralObject does not have a cache." << endl;
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


