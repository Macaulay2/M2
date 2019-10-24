-- this file is loaded by NumericalAlgebraicGeometry
-- WSet-*.m2 are loaded by NAGtypes
-- !!! perhaps merge with witness-set.m2 ???

moveSlicingVariety(MultiAffineWSet,MultiSlicingVariety) := (W,S) -> (
    E := equations W#"equations";
    A := flatten entries map slicingVariety W;
    B := flatten entries map S;
    ptsB := track(E|A, E|B, points W, 
	NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii));
    if any(ptsB, p->status p =!= Regular) then null -- failed
    else multiAffineWSet(W#"equations",S,ptsB) 
    )

TEST ///
debug needsPackage "NAGtypes"
debug needsPackage "NumericalAlgebraicGeometry"
errorDepth = 0
A = multiAffineSpace(CC_53,{2,2},x)
use ring A 
-- multi=homogenized parabola y-z^2=0 where y=x_(0,1) and z=x_(1,1)
F = polySystem {x_(0,1)*x_(1,2)^2-x_(1,1)^2*x_(0,2),x_(1,2)-1,x_(0,2)-1}
S = multiSlicingVariety(A, {rationalMap matrix{{x_(0,1)-1}}, rationalMap map((ring A)^0,(ring A)^1,0)})
pts = {point{{1,1,1,1}},point{{1,1,-1,1}}}
W = multiAffineWSet(F,S,pts)
dim W
degree W
S = randomSlicingVariety(A,{1,0})
W' = moveSlicingVariety(W,S)
pts' = points W'
residuals = for p in pts' list 	   
{matrix evaluate(map slicingVariety W',p), evaluate(W'#"equations",p)}
assert all(flatten residuals, r->norm r < 1e-6)
///
