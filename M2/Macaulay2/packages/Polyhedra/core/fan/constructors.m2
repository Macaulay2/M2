-- Defining the new type Fan
Fan = new Type of PolyhedralObjectFamily
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
   result := new HashTable from {
      ambientDimension => numRows irays,
      computedRays => irays,
      computedLinealityBasis => linealityGens,
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
   constructTypeFromHash(Fan, inputProperties)
)
