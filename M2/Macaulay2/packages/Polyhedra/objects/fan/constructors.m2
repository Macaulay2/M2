-- Defining the new type Fan
Fan = new Type of PolyhedralObjectFamily
globalAssignment Fan

compute#Fan = new MutableHashTable;

-- PURPOSE : Building the Fan 'F'
--   INPUT : 'L',  a list of cones and fans in the same ambient space
--  OUTPUT : The fan of all Cones in 'L' and all Cones in of the fans in 'L' and all their faces
fan = method(TypicalValue => Fan)
fan(Matrix, Matrix, List) := (irays, linealityGens, icones) -> (
   result := new HashTable from {
      ambientDimension => numRows irays,
      computedRays => irays,
      computedLinealityBasis => linealityGens,
      maximalCones => icones
   };
   fan result
)

fan(Matrix, Matrix, Sequence) := (irays, linealityGens, icones) -> (
   fan(irays, linealityGens, toList icones)
)

fan(Matrix, List) := (irays, icones) -> (
   r := ring irays;
   linealityGens := map(target irays, r^1, 0);
   fan(irays, linealityGens, icones)
)

fan(Matrix, Sequence) := (irays, icones) -> (
   fan(irays, toList icones)
)

fan HashTable := inputProperties -> (
   constructTypeFromHash(Fan, inputProperties)
)
