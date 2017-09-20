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

------------------------------------
-- NAGtypes.m2 functionality
------------------------------------

-- Abstract classes ------------------------
WSet = new Type of MutableHashTable
Ambient = new Type of MutableHashTable
SlicingVariety = new Type of MutableHashTable

-- WSet ------------------------------------
WSet.synonym = "(abstract) witness set"
net WSet := W -> net "error: net method is not defined for..."
dim WSet := W -> error "not implemented"
codim WSet := {} >> o -> W -> error "not implemented"
ambient WSet := W -> error "not implemented"
degree WSet := W -> error "not implemented"
points = method()
points WSet := W -> error "not implemented"

-- low-level methods
-- isOn = method()
-- isOn (Point,WSet) := (P,W) -> error "not implemented"

moveSlicingVariety = method(TypicalValue=>WSet) 
moveSlicingVariety (WSet, SlicingVariety) := (W,S) -> error "not implemented"

-- Ambient ---------------------------------
Ambient.synonym = "(abstract) ambient space"
net Ambient := A -> net "error: net method is not defined for..."
dim Ambient := A -> error "not implemented"

-- SlicingVariety -------------------(hacked to work with AffineSpace)-----------------
RationalMap = Thing -- !!!
SlicingVariety.synonym = "(abstract) slice"
slicingVariety = method()
slicingVariety(Ambient,RationalMap) := (A,M) -> new SlicingVariety from {"ambient"=>A, "map"=>M}
net SlicingVariety := S -> net "slice of codim " | net codim S
codim SlicingVariety := S -> rank target map S 
dim SlicingVariety := S -> dim ambient S - codim S  
ambient SlicingVariety := S -> S#"ambient"
map SlicingVariety := o -> S -> S#"map"

-- generates a slice in the same class as S
randomSlicingVariety = method()
randomSlicingVariety SlicingVariety := S -> error "not implemented"


