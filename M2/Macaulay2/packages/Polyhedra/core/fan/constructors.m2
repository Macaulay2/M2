-- Defining the new type Fan
Fan = new Type of PolyhedralObject
globalAssignment Fan
compute#Fan = new MutableHashTable;


Fan == Fan := (F1, F2) -> (
   r1 := rays F1;
   l1 := linealitySpace F1;
   r2 := rays F2;
   if numRows r1 != numRows r2 then return false;
   if numColumns r1 != numColumns r2 then return false;
   m1 := maxCones F1;
   m2 := maxCones F2;
   if #m1 != #m2 then return false;
   rayMap := rayCorrespondenceMap(r1, l1, r2);
   m1Mapped := apply(m1,
      maxCone -> (
         sort apply(maxCone, l -> rayMap#l)
      )
   );
   m2Mapped := apply(m2, maxCone -> sort maxCone);
   (sort m1Mapped) == (sort m2Mapped)
)


-- PURPOSE : Building the Fan 'F'
--   INPUT : 'L',  a list of cones and fans in the same ambient space
--  OUTPUT : The fan of all Cones in 'L' and all Cones in of the fans in 'L' and all their faces
fan = method(TypicalValue => Fan)
fan(Matrix, Matrix, List) := (irays, linealityGens, icones) -> (
   if numRows irays != numRows linealityGens then error("Rays and lineality must have same ambient dimension.");
   irays = makeRaysPrimitive(irays);
   lineality := makeRaysPrimitive(linealityGens);
   result := new HashTable from {
      ambientDimension => numRows irays,
      rays => irays,
      computedLinealityBasis => lineality,
      generatingObjects => icones
   };
   fan result
)


fan(Matrix, Matrix, Sequence) := (irays, linealityGens, icones) -> (
   fan(irays, linealityGens, toList icones)
)

fan(Matrix, List) := (irays, icones) -> (
   r := ring irays;
   linealityGens := map(target irays, r^0, 0);
   fan(irays, linealityGens, icones)
)

fan(Matrix, Sequence) := (irays, icones) -> (
   fan(irays, toList icones)
)

fan HashTable := inputProperties -> (
   resultHash := sanitizeFanInput inputProperties;
   constructTypeFromHash(Fan, resultHash)
)


sanitizeFanInput = method()
sanitizeFanInput HashTable := given -> (
   rayProperties := {inputRays, inputLinealityGenerators, rays, computedLinealityBasis};
   remainingProperties := keys given;
   remainingProperties = select(remainingProperties, p -> all(rayProperties, rp -> rp=!=p));
   result := apply(remainingProperties, rp -> rp=>given#rp);
   for rp in rayProperties do (
      if given#?rp then (
         primitive := makeRaysPrimitive given#rp;
         result = append(result, rp=>primitive)
      )
   );
   new HashTable from result
)


--   INPUT : 'C',  a Cone
--  OUTPUT : The Fan given by 'C' and all of its faces
fan Cone := C -> (
   raysC := rays C;
   linealityC := linealitySpace C;
   n := numColumns raysC;
   mc := {toList (0..(n-1))};
   result := fan(raysC, linealityC, mc);
   setProperty(result, honestMaxObjects, {C});
   result
)

addCone = method();
addCone(Fan, Cone) := (F, C) -> (
   if ambDim F != ambDim C then error("Fan and Cone must live in same ambient space.");
   linF := linealitySpace F;
   linC := linealitySpace C;
   if image linF != image linC then error("Cannot add cone with different lineality space.");
   joinedRays := makeRaysUniqueAndPrimitive(rays F | rays C);
   mc := maxCones F;
   map := rayCorrespondenceMap(rays F, joinedRays);
   mc = apply(mc, c-> apply(c, e->map#e));
   map = rayCorrespondenceMap(rays C, joinedRays);
   newCone := toList apply(numColumns rays C, i -> map#i);
   mc = append(mc, newCone);
   result := new HashTable from {
      ambientDimension => ambDim F,
      rays => joinedRays,
      computedLinealityBasis => linF,
      inputCones => mc
   };
   fan result
)

addCone(Cone, Fan) := (C, F) -> addCone(F, C)

addCone(List, Fan) := (L, F) -> (
   if not all(L, l->instance(l, Cone)) then error("List does not contain cones");
   result := F;
   for cone in L do (
      result = addCone(result, cone)
   );
   result
)

fan List := inputCones -> (
   if instance(inputCones, Cone) then error("Why did I arrive here?");
   if not all(inputCones, c -> instance(c, Cone)) then error("This constructor needs a list of cones.");
   result := fan(inputCones#0);
   for i from 1 to #inputCones - 1 do (
      result = addCone(result, inputCones#i);
   );
   result
)
