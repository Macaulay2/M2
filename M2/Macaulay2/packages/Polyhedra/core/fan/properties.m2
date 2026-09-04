-- See core/cone/properties.m2 for why these exist under getInputRays/
-- getInputLinealityGenerators rather than the (protected) property names.
getInputRays Fan := (cacheValue symbol inputRays) (
   F -> error("No input rays set for this Fan.")
)

getInputLinealityGenerators Fan := (cacheValue symbol inputLinealityGenerators) (
   F -> error("No input lineality generators set for this Fan.")
)


compute#Fan#isWellDefined = method()
compute#Fan#isWellDefined Fan := F -> (
   indexLists := maxCones F;
   cones := maxCones(F, Cone);
   n := #cones;
   for i from 0 to n-1 do (
      ki := indexLists#i;
      Ci := cones#i;
      if(#ki != numColumns rays Ci) then(
         if debugLevel > 0 then << "The cone " << ki << " has redundant rays." << endl;
         return false;
      );
      for j from i to n-1 do (
         kj := indexLists#j;
         Cj := cones#j;
         if not commonFace(Ci, Cj) then (
            if debugLevel > 0 then << "The cones " << ki << " and " << kj << " do not intersect in a common face." << endl;
            return false
         )
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
   toList apply(0..(dim F), d -> #faces(dim F - d,F))
)


compute#Fan#simplicial = method()
compute#Fan#simplicial Fan := F -> (
   -- Reuse already-materialized cones if we happen to have them, rather
   -- than forcing them just for this check; otherwise fall back to the
   -- cheaper combinatorial (ray submatrix rank) computation below.
   if F.cache#?(symbol maxCones => Cone) then (
      return all(maxCones(F, Cone), cone -> isSimplicial cone)
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
   if F.cache#?(symbol maxCones => Cone) then (
      return all(maxCones(F, Cone), cone -> (dim cone) == d)
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

compute#Fan#computedComplete = method()
compute#Fan#computedComplete Fan := F -> (
   n := dim F;
   if n != ambDim F then return false;
   symmDiff := (X,Y) -> (
      summand1 := select(X, x -> position(Y, y->y==x) === null); 
      summand2 := select(Y, y -> position(X, x->y==x) === null); 
      flatten {summand1, summand2}
   );
   MC := maxCones(F, Cone);
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


-- Fan's lineality space is always set directly at construction time (see
-- fan(Matrix,Matrix,List) in core/fan/constructors.m2), so this is an
-- error-stub cache accessor, same idea as getUnderlyingFan; it is never
-- actually triggered in practice.
linealitySpace Fan := (cacheValue symbol computedLinealityBasis) (
   F -> error("No lineality space set for this Fan.")
)

rays Fan := {} >> o -> (cacheValue rays) (
   F -> (
      if hasProperty(F, inputRays) then (
         given := getInputRays F;
         LS := linealitySpace F;
         makeRaysUniqueAndPrimitive(given, LS)
      ) else (
         -- Could also compute this from maxCones(F, Cone)?
         error("No input rays given.")
      )
   )
)


compute#Fan#computedFacesThroughRays = method()
compute#Fan#computedFacesThroughRays Fan := F -> (
   MC := maxCones(F, Cone);
   raysF := rays F;
   dimF := dim F;
   linealityF := linealitySpace F;
   result := new MutableHashTable;
   for i from 0 to dim F do result#i = {};
   for C in MC do (
      dimC := dim C;
      raysC := rays C;
      facesC := faces C;
      rc := rayCorrespondenceMap(raysC, linealityF, raysF);
      for i in keys facesC do (
         codimInF := i + dimF - dimC;
         codimiCones := facesC#i;
         codimiCones = apply(codimiCones,
            c -> (
               sort apply(c, e -> rc#e)
            )
         );
         result#codimInF = sort unique flatten {result#codimInF, codimiCones};
      );
   );
   return hashTable pairs result
)

compute#Fan#generatingObjects = method()
compute#Fan#generatingObjects Fan := F -> (
   if hasProperty(F, inputCones) then (
      cones := getProperty(F, inputCones);
      if hasProperty(F, inputRays) then (
         inputRaysF := getInputRays F;
         raysF := rays F;
         linealityF := linealitySpace F;
         rc := rayCorrespondenceMap(inputRaysF, linealityF, raysF);
         cones = apply(cones,
            c -> (
               cnew := sort apply(c, e->rc#e);
               select(cnew, e -> e != -1)
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
   else if hasProperty(F, inputRays) then return numRows getInputRays F
   else if hasProperty(F, inputLinealityGenerators) then return numRows getInputLinealityGenerators F
   else error("No property available to compute ambient dimension.")
)


compute#Fan#pointed = method()
compute#Fan#pointed Fan := F -> (
   all(maxCones(F, Cone), C -> isPointed C)
)

