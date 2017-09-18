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
net WSet := W -> error "not implemented"
dim WSet := W -> error "not implemented"
codim WSet := {} >> o -> W -> error "not implemented"
ambient WSet := W -> error "not implemented"
degree WSet := W -> error "not implemented"
points = method()
points WSet := W -> error "not implemented"

-- low-level methods
-- isOn = method()
-- isOn (Point,WSet) := (P,W) -> error "not implemented"

wSet = method(TypicalValue=>WSet) -- a.k.a. movePoints
wSet (WSet, SlicingVariety) := (W,S) -> error "not implemented"

-- Ambient ---------------------------------
Ambient.synonym = "(abstract) ambient space"
net Ambient := A -> error "not implemented"
dim Ambient := A -> error "not implemented"

randomSlicingVariety = method()
randomSlicingVariety SlicingVariety := S -> error "not implemented"

-- SlicingVariety ------------------------------------
SlicingVariety.synonym = "(abstract) slice"
net SlicingVariety := S -> error "not implemented"
codim SlicingVariety := S -> error "not implemented"
dim SlicingVariety := S -> dim ambient S - codim S  
ambient SlicingVariety := S -> error "not implemented"
