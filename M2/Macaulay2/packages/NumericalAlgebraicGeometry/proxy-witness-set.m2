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

-- Parent class
WitnessSet = new Type of MutableHashTable
-- Child class for proxy witness sets
ProxyWitnessSet = new Type of WitnessSet
-- Any other sub-classes?
-- Children should have extra structure/methods over the parent.
-- ProjectiveWitnessSet?  Multi-affine/projective?

-- Here's a constructor for WitnessSet that handles all ambient space types.
-- The slice is replaced by a list of slices, and 'Projective' is an optional flag.
witnessSet = method(TypicalValue=>WitnessSet, Options=>{Projective=>false})
witnessSet (Ideal,List,List) := o -> (F,S,P) ->
  new WitnessSet from {
      Equations => I, -- should equations be stored as Ideal or PolySystem?
      Slice => S, -- a list of slice matrices, one for each factor
      Points => P,
      Projective => o.Projective,
      }
witnessSet (Ideal,Matrix,List) := o -> (F,M,P) -> witnessSet(F,{M},P,Options=>o)
-- etc.

-- proxy witness sets store equations and points in an upstairs space, with a map to a downstairs space.
-- If R and S are the coordinate rings of the upstairs and downstairs spaces respec, m: R <-- S.
-- Since m can be rational, should the map be stored as a RingMap or a List/Matrix of expressions?
proxyWitnessSet = method(TypicalValue=>ProxyWitnessSet, Options=>{Projective=>false})
proxyWitnessSet (Ideal,List,List,RingMap) := o -> (F,S,P,m) -> (
    W := witnessSet(F,S,P,Options=>o);
    W.ProxyMap = m;
    W
    )

dim WitnessSet := W -> sum(W.Slice, numrows)
codim WitnessSet := {} >> o -> W -> numgens ring W - dim W
ring WitnessSet := W -> ring W.Equations
degree WitnessSet := W -> #W.Points
ideal WitnessSet := W -> W.Equations
points = method()
points WitnessSet := W -> W.Points
downstairsPoints = method()
downstairsPoints ProxyWitnessSet := W -> (
    M := matrix W.ProxyMap;
    apply(W.Points, p -> point evaluate(M,p))
    )

------------------------------------
-- witness-set.m2 functionality
------------------------------------

-- produces PolySystem from W.Equations, squaring up if #equations > codimension
polySystem WitnessSet := W->if W.?SolutionSystem then W.SolutionSystem else 
    W.SolutionSystem = polySystem(
    	n := #equations W;
    	R := ring W;
    	m := codim W;
	if m == n then W.Equations else (
    	    M := sub(randomOrthonormalRows(m,n),coefficientRing R);
    	    sub(M,ring W) * transpose matrix{equations W}
	    )
    	)

-- this function purportedly allows the ring to be specified, but CC is hard-coded into randomUnitaryMatrix
randomSlice = method(Options=>{Ring=>CC_53})
randomSlice (ZZ,ZZ,Point) := o -> (d,n,point) -> (
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM | (-SM * transpose matrix point)
     )
randomSlice (ZZ,ZZ) := o -> (d,n) -> randomSlice(d,n,point random(o.Ring^1,o.Ring^n))   

-- checks if p is on the component represented by W.
-- W must have a full set of degree many points.
-- Should it check/flag this condition?  Should it use monodromy if the condition is false?
isOn (Point,WitnessSet) := o -> (p, W) -> (
    o = fillInDefaultOptions o;
    R := ring W;
    if # coordinates p != numgens R then error "numbers of coordinates mismatch";
    C := coefficientRing R;
    W' := moveSlice(W, randomSlice(dim W, numgens R, C, p), Options=>o);
    any(W'.Points, p'->areEqual(p',p))
    )


isOn (Point,ProxywitnessSet) := o -> (p, W) -> (
    o = fillInDefaultOptions o;
    --if # coordinates p != m then error "wrong number of coordinates",
    R := ring W;                    
    C := coefficientRing R;
    n := numgens R;
    rows := toList (0..dim W - 1);
    M := ((map C^n)^m || random(C^(n-m),C^n))^rows;
    mp := transpose (matrix p | random(C^1,C^(n-m)));
    W' := moveSlice(W, M | -M*mp);
    any(W'.Points, p'->areEqual(project(p',m),p))
    )

-- ... hypersurface V(f)
isOn (Point,RingElement) := o -> (p, f) ->  isOn (p, witnessSet(ideal f, numgens ring f - 1), o)

-- ... V(I)
isOn (Point,Ideal) := o -> (p, I) -> all(I_*, f->isOn(p,f,o))
