-- Witness set classifications
-- Point representations:
--  * Direct
--  * Proxy - linear projection
--  * Proxy - rational map
-- Ambient space structure:
--  * Affine
--  * Multi-affine
--  * Projective
--  * Multi-projective
-- Point structure:
--  * All points on the slice
--  * Fewer points
--  * Points from different slices?
--  * Others?

-- General class
WitnessSet = new Type of MutableHashTable
-- Which classifications should get their own sub-classes if any?
-- Sub-classes should have extra structure/methods over the parent.
-- ProxyWitnessSet?  ProjectiveWitnessSet?  Multi-affine/projective?

-- Here's a constructor for an inclusive version of WitnessSet
witnessSet = method(TypicalValue=>WitnessSet, Options=>{ProxyMap=>null,Projective=>false})
witnessSet (Ideal,List,List) := o -> (F,S,P) ->
  new WitnessSet from {
      Equations => I, -- should equations be stored as Ideal or PolySystem?
      Slice => S, -- a list of slice matrices, one for each factor
      Points => P,
      Projective => o.Projective,
      Map => o.ProxyMap
      }
witnessSet (Ideal,Matrix,List) := o -> (F,M,P) -> witnessSet(F,{M},P,Options=>o)
-- etc.

dim WitnessSet := W -> sum(W.Slice, numrows) -- should these return the upstairs or downstairs (co)dim?
codim WitnessSet := {} >> o -> W -> numgens ring W - dim W
ring WitnessSet := W -> ring W.Equations
degree WitnessSet := W -> #W.Points
ideal WitnessSet := W -> W.Equations
