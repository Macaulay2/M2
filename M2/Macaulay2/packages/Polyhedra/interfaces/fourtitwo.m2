fourtitwoPolyhedra = new MutableHashTable
fourtitwoPolyhedra#Cone = new MutableHashTable

fourtitwoPolyhedra#Cone#computedHilbertBasis = method()
fourtitwoPolyhedra#Cone#computedHilbertBasis Cone := C -> (
   needsPackage "FourTiTwo";
   InputType := (value getGlobalSymbol "InputType");
   if debugLevel > 2 then << "Using FourTiTwo." << endl;
   inputMatrix := (facets C) || (hyperplanes C) || ( - hyperplanes C);
   hb := transpose hilbertBasis(transpose inputMatrix, InputType => "lattice");
   hb = promote(hb, ring inputMatrix) // inputMatrix;
   apply(numColumns hb, i -> hb_{i})
)
