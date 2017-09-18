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

downstairsPoints = method()
downstairsPoints ProxyWitnessSet := W -> (
    M := matrix W.ProxyMap;
    apply(W.Points, p -> point evaluate(M,p))
    )