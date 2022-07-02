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
WSet = new Type of HashTable
Ambient = new Type of MutableHashTable
SlicingVariety = new Type of MutableHashTable

-- WSet ------------------------------------
WSet.synonym = "(abstract) witness set"
net WSet := W -> net "error: net method is not defined for..."
dim WSet := W -> error "not implemented"
codim WSet := {} >> o -> W -> error "not implemented"
ambient WSet := W -> ambient slicingVariety W
degree WSet := W -> error "not implemented"
points = method()
points WSet := W -> error "not implemented"
slicingVariety = method()
slicingVariety WSet := W -> error "not implemented"

-- low-level methods
-- isOn = method()
-- isOn (AbstractPoint,WSet) := (P,W) -> error "not implemented"

moveSlicingVariety = method(TypicalValue=>WSet) 
moveSlicingVariety (WSet, SlicingVariety) := (W,S) -> error "not implemented"

-- Ambient ---------------------------------
Ambient.synonym = "(abstract) ambient space"
net Ambient := A -> net "error: net method is not defined for..."
dim Ambient := A -> error "not implemented"

-- RationalMap ---------------------------------
RationalMap = new Type of Matrix
rationalMap = method()
rationalMap(Matrix) := M -> new RationalMap from M

matrix(RationalMap) := o -> M -> new Matrix from M
coordinateProjection = method()
coordinateProjection(Ambient,Ambient) := (B,A) -> (
    vs := take(gens ring A, numgens ring B);
    rationalMap matrix {vs}
    )
evaluate = method()
evaluate(RationalMap, AbstractPoint) := (M,P) -> point sub(matrix M, matrix P)
compose(RationalMap, RationalMap) := (M,N) -> rationalMap sub(matrix M, matrix N)

-- SlicingVariety -------------------(hacked to work with AffineSpace)-----------------
-- "map" is hacked as a (1-column) matrix that defines the slice  
SlicingVariety.synonym = "slice"
slicingVariety(Ambient,RationalMap) := (A,M) -> new SlicingVariety from {"ambient"=>A, "map"=>M}
net SlicingVariety := S -> net "slice of codim " | net codim S
codim SlicingVariety := S -> rank target map S 
dim SlicingVariety := S -> dim ambient S - codim S  
ambient SlicingVariety := S -> S#"ambient"
map SlicingVariety := o -> S -> S#"map"

-- generates a slice in the same class as S
randomSlicingVariety = method()
randomSlicingVariety SlicingVariety := S -> error "not implemented"


