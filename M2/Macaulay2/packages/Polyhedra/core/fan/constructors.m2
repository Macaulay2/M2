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
   n := max flatten icones;
   if numColumns irays < n then error("The number of indices exceeds the number of vectors");
   if (numRows irays != numRows linealityGens) then error("Rays and lineality must have same ambient dimension.");
   lineality := makeRaysPrimitive(mingens image linealityGens);
   result := new HashTable from {
      inputRays => irays,
      computedLinealityBasis => lineality,
      inputCones => icones
   };
   internalFanConstructor result
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

internalFanConstructor = method()
internalFanConstructor HashTable := inputProperties -> (
   resultHash := sanitizeFanInput inputProperties;
   constructTypeFromHash(Fan, resultHash)
)

fanFromGfan = method()
fanFromGfan List := gfanOutput -> (
-- 0 rays -> Matrix
-- 1 lineality -> Matrix
-- 2 cones -> List<List>
-- 3 dimension -> ZZ
-- 4 pure -> bool
-- 5 simplicial -> bool
-- 6 fVector -> List
   numberOfGfanOutputs := 7;
   if #gfanOutput != numberOfGfanOutputs then
      error("fanFromGfan was given a list with " | toString(#gfanOutput)
         | " inputs and " | toString(numberOfGfanOutputs) | " are required.");
   R := gfanOutput#0;
   L := gfanOutput#1;
   
   -- Perform some basic sanity checks on the fan. If the fan is empty (i.e.
   -- there are no rays and no lineality space), then all of the other values
   -- need to agree with that (there cannot be any cones, the dimension must
   -- be zero, and the f-vector must be empty).
   if ((numColumns R == 0) and (numColumns L == 0))
   and ((#(gfanOutput#2) != 0) or (gfanOutput#3 != 0) or (#(gfanOutput#6) != 0))
   then error("Inconsistent input into fanFromGfan");
   
   if (numColumns R == 0) then R = map(ZZ^(numRows L), ZZ^0, 0);
   if (numColumns L == 0) then L = map(ZZ^(numRows R), ZZ^0, 0);
   result := fan(R, L, gfanOutput#2);
   setProperty(result, computedFVector, gfanOutput#6);
   setProperty(result, pure, gfanOutput#4);
   setProperty(result, simplicial, gfanOutput#5);
   setProperty(result, computedDimension, gfanOutput#3);
   return result;
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
   setProperty(result, honestMaxObjects, new HashTable from {mc#0 => C});
   result
)

addCone = method();
addCone(Fan, Cone) := (F, C) -> (
   if ambDim F != ambDim C then error("Fan and Cone must live in same ambient space.");
   linF := linealitySpace F;
   linC := linealitySpace C;
   if image linF != image linC then error("Cannot add cone with different lineality space.");
   joinedRays := makeRaysUniqueAndPrimitive(rays F | rays C, linF);
   mc := maxCones F;
   map := rayCorrespondenceMap(rays F, linF, joinedRays);
   mc = apply(mc, c-> apply(c, e->map#e));
   map = rayCorrespondenceMap(rays C, linF, joinedRays);
   newCone := toList apply(numColumns rays C, i -> map#i);
   mc = append(mc, newCone);
   result := new HashTable from {
      ambientDimension => ambDim F,
      rays => joinedRays,
      computedLinealityBasis => linF,
      inputCones => mc
   };
   internalFanConstructor result
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
