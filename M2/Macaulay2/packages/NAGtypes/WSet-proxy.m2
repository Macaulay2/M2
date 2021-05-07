-- proxy witness sets store equations and points in an upstairs space, with a map to a downstairs space.
-- If R and S are the coordinate rings of the upstairs and downstairs spaces respec, m: R <-- S.
-- Since m can be rational, should the map be stored as a RingMap or a List/Matrix of expressions?

-- An affine proxy witness set:
--  a WSet 
--  a rational map 
ProxyWSet = new Type of WSet

-- User has to construct the upstairs WSet -- this is problem dependent
proxyWSet = method(TypicalValue=>ProxyWSet)
proxyWSet (WSet,RationalMap,SlicingVariety) := (W,M,S) ->
  new ProxyWSet from {
      "upstairs WSet" => W, 
      "rational map" => M,
      "downstairs slice" => S
      }
proxyWSet WSet := W -> (
    A := ambient W;
    proxyWSet(W,coordinateProjection(A,A),slice W)
    )

texMath ProxyWSet := x -> texMath net x

net ProxyWSet := pr -> (
    net "proxyWSet(" | net upWSet pr | ", " | 
    net map pr | ", " | net slicingVariety pr | ")" 
    )

map ProxyWSet := RationalMap => o -> pr -> pr#"rational map" 

upWSet = method()
upWSet ProxyWSet := pr -> pr#"upstairs WSet"

slicingVariety ProxyWSet := pr -> pr#"downstairs slice" 

-- this returns points "downstairs"
points ProxyWSet := pr -> (
    M := pr#"rational map";
    apply(points upWSet pr, p -> point evaluate(M,p))
    )

pullBack = method()
pullBack(RationalMap,SlicingVariety) := (M,S) -> (
    B := ambient S;
    T := target matrix M;
    -- isCompatible(T,B);
    C := coefficientRing ring T;
    Smap := map S; -- slice = ker(slicingMap)
    slicingVariety(affineSpace ring Smap, rationalMap sub(Smap,transpose matrix M))  
    )

moveSlicingVariety(ProxyWSet,SlicingVariety) := (pr,S) -> (
    -- A = source M
    prS := pullBack(map pr,S);
    W := moveSlicingVariety(upWSet pr, prS);
    proxyWSet(W,map pr,S)
    ) 

TEST ///
debug needsPackage "NAGtypes"
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y]
A = affineSpace R 
S = slicingVariety(A, rationalMap matrix{{x+y-1}})
CC[x,y,L]
w = witnessSet( ideal(y^2+3*y*L,x-y+L), ideal(x+y+1), {point {{-1,0,1_CC}}} )
pr = proxyWSet(w,rationalMap matrix{{x},{y}},S)
errorDepth=0
pr' = moveSlicingVariety(pr, randomSlicingVariety(A,1))
points pr'
map slicingVariety pr'
///
