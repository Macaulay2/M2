-- multi-affine witness sets 

MultiAffineWSet = new Type of WSet

-- User has to construct the upstairs WSet -- this is problem dependent
multiAffineWSet = method(TypicalValue=>MultiAffineWSet)
multiAffineWSet (PolySystem,SlicingVariety,List) := (F,S,pts) -> (
      R := ring F;
      -- assert isHomogeneous F.PolyMap; --!!! 
      new MultiAffineWSet from {
      	  "ambient" => multiAffineSpace R,
	  "equations" => F, 
      	  "slice" => S,
      	  "points" => pts
      	  }
      )
dim MultiAffineWSet := W -> codim W#"slice"
degree MultiAffineWSet := W -> # points W 

net MultiAffineWSet := W -> net "multiAffineWSet(dim=" | net dim W | ",deg=" | net degree W | ")" 

slicingVariety MultiAffineWSet := W -> W#"slice" 

-- this returns points "downstairs"
points MultiAffineWSet := W -> W#"points"

