compute#Fan#isWellDefined = method()
compute#Fan#isWellDefined Fan := F -> (
   cones := getProperty(F, honestMaxObjects);
   n := #cones;
   for i from 0 to n-1 do (
      Ci := cones#i;
      for j from i to n-1 do (
         Cj := cones#j;
         if not commonFace(Ci, Cj) then return false
      )
   );
   return true
)


compute#Fan#smooth = method()
compute#Fan#smooth Fan := F -> (
   R := rays F;
   L := transpose linealitySpace F;
   MC := maxCones F;
   MC = apply(MC, m -> R_m);
   all(MC, r -> spanSmoothCone(transpose r, L))
)


compute#Fan#computedFVector = method()
compute#Fan#computedFVector Fan := F -> (
   reverse apply(dim F + 1, d -> #faces(dim F - d,F))
)


compute#Fan#simplicial = method()
compute#Fan#simplicial Fan := F -> (
   if hasProperty(F, honestMaxObjects) then (
      mc := getProperty(F, honestMaxObjects);
      return all(mc, cone -> isSimplicial cone)
   );
   R := rays F;
   L := linealitySpace F;
   MC := maxCones F;
   MC = apply(MC, m -> R_m);
   all(MC, 
      r -> (
         testmat := r | L;
         (numColumns testmat) == (rank testmat)
      )
   )
)

compute#Fan#pure = method()
compute#Fan#pure Fan := F -> (
   d := dim F;
   if hasProperty(F, honestMaxObjects) then (
      mc := getProperty(F, honestMaxObjects);
      return all(mc, cone -> (dim cone) == d)
   );
   R := rays F;
   L := linealitySpace F;
   MC := maxCones F;
   MC = apply(MC, m -> R_m);
   all(MC, r -> d == (rank (r | L)))
)

compute#Fan#computedDimension = method()
compute#Fan#computedDimension Fan := F -> (
   R := rays F;
   MC := maxCones F;
   L := linealitySpace F;
   MC = apply(MC, m -> R_m);
   MC = apply(MC, r -> rank (r | L));
   max MC
)

compute#Fan#honestMaxObjects = method()
compute#Fan#honestMaxObjects Fan := F -> (
   R := rays F;
   MC := maxCones F;
   L := linealitySpace F;
   apply(MC, m -> coneFromVData(R_m, L))
)

compute#Fan#computedComplete = method()
compute#Fan#computedComplete Fan := F -> (
   n := dim F;
   if n != ambDim F then return false;
   symmDiff := (X,Y) -> (
      summand1 := select(X, x -> position(Y, y->y==x) === null); 
      summand2 := select(Y, y -> position(X, x->y==x) === null); 
      flatten {summand1, summand2}
   );
   MC := getProperty(F, honestMaxObjects);
   Lfaces := {};
   CFsave := {};
   scan(MC, 
      C -> (
         if dim C == n then (
            R := rays C;
            L := linealitySpace C;
            CFacets := toList getProperty(C, facetsThroughRayData);
            CFacets = apply(CFacets, facet -> coneFromVData(R_facet, L));
            CFsave = flatten {CFsave, {CFacets}};
            Lfaces = symmDiff(Lfaces, CFacets);
         )
         else return false
      )
   );
   Lfaces == {}
)


compute#Fan#rays = method()
compute#Fan#rays Fan := F -> (
   if hasProperty(F, inputRays) then (
      given := getProperty(F, inputRays);
      makeRaysUniqueAndPrimitive given
   ) else (
      -- Could also compute this from the honestMaxObjects?
      error("No input rays given.")
   )
)


compute#Fan#computedFacesThroughRays = method()
compute#Fan#computedFacesThroughRays Fan := F -> (
   MC := getProperty(F, honestMaxObjects);
   raysF := rays F;
   linealityF := linealitySpace F;
   result := new MutableHashTable;
   for i from 0 to dim F do result#i = {};
   for C in MC do (
      dimC := dim C;
      raysC := rays C;
      facesC := faces C;
      rc := rayCorrespondenceMap(raysC, linealityF, raysF);
      for i in keys facesC do (
         codimiCones := facesC#i;
         codimiCones = apply(codimiCones,
            c -> (
               sort apply(c, e -> rc#e)
            )
         );
         result#i = sort unique flatten {result#i, codimiCones};
      );
   );
   return hashTable pairs result
)

compute#Fan#generatingObjects = method()
compute#Fan#generatingObjects Fan := F -> (
   if hasProperty(F, inputCones) then (
      cones := getProperty(F, inputCones);
      if hasProperty(F, inputRays) then (
         inputRaysF := getProperty(F, inputRays);
         raysF := rays F;
         linealityF := linealitySpace F;
         rc := rayCorrespondenceMap(inputRaysF, linealityF, raysF);
         cones = apply(cones,
            c -> (
               sort apply(c, e->rc#e)
            )
         );
      );
      cones = unique apply(cones, c -> sort c);
      result := {};
      for cone in cones do (
         test := all(cones,
            c -> (
               n := #((set c) * (set cone));
               if n == #cone then (
                  cone == c
               ) else (
                  true
               )
            )
         );
         if test then result = append(result, cone);
      );
      result
   ) else (
      -- Given honestMaxObj, compute these?
      error("No input cones given");
   )
)

compute#Fan#smoothCones = method()
compute#Fan#smoothCones Fan := F -> (
   result := {};
   raysF := rays F;
   linealityF := linealitySpace F;
   cones := getProperty(F, computedFacesThroughRays);
   for i in keys cones do (
      for cone in cones#i do (
         if spanSmoothCone(transpose(raysF_cone), transpose(linealityF)) then (
            result = append(result, cone)
         )
      )
   );
   result
)

compute#Fan#ambientDimension = method()
compute#Fan#ambientDimension Fan := F -> (
   if hasProperty(F, rays) then return numRows rays F
   else if hasProperty(F, computedLinealityBasis) then return numRows linealitySpace F
   else if hasProperty(F, inputRays) then return numRows getProperty(F, inputRays)
   else if hasProperty(F, inputLinealityGenerators) then return numRows getProperty(F, inputLinealityGenerators)
   else error("No property available to compute ambient dimension.")
)


compute#Fan#pointed = method()
compute#Fan#pointed Fan := F -> (
   all(getProperty(F, honestMaxObjects), C -> isPointed C)
)

