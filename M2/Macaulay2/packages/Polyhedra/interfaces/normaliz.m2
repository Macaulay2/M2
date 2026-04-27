normalizPolyhedra = new MutableHashTable
normalizPolyhedra#Cone = new MutableHashTable

HBFromNormalizCone := RC -> (
   HB := transpose RC#"gen";
   apply(numColumns HB, i -> HB_{i})
   )

normalizPolyhedra#Cone#computedHilbertBasis = method()
normalizPolyhedra#Cone#computedHilbertBasis Cone := C -> (
   needsPackage "Normaliz";
   normaliz := (value getGlobalSymbol "normaliz");
   if debugLevel > 2 then << "Using Normaliz." << endl;
   if not isPointed C then error("Hilbert basis not implemented for non-pointed cones");
   if hasProperty(C, rays) then (
      return HBFromNormalizCone normaliz(transpose rays C, "integral_closure")
   ) else if hasProperties(C, {facets, computedHyperplanes}) then (
      return HBFromNormalizCone normaliz({(facets C, "inequalities"), (hyperplanes C, "equations")})
   ) else if hasProperty(C, inputRays) then (
      return HBFromNormalizCone normaliz(transpose getProperty(C, inputRays), "integral_closure")
   ) else if hasProperties(C, {inequalities, equations}) then (
      return HBFromNormalizCone normaliz({(getProperty(C, inequalities), "inequalities"), (getProperty(C, equations), "equations")})
   ) else (
      if debugLevel > 2 then << "Normaliz computing with rays." << endl;
      return HBFromNormalizCone normaliz(transpose rays C, "integral_closure")
   )
)

