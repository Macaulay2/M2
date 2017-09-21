


deflate(WSet, Point) := o -> (W,P) -> deflate(W,jacobianRank(polySystem W,P),{P},o)
deflate(WSet, ZZ) := o -> (W,r) -> deflate(W,r,points W,o)
deflate(WSet, ZZ, List) := o -> (W,r,pts) ->
    F := polySystem W;
    F0 := deflate(F,r);
    pts0 := apply(pts, P->liftPointToDeflation(P,F,r));
    S0 := liftSliceToDeflation(slicingVariety W,F,r);
    W0 := wSet(F0,S0,pts0);
    M := coordinateProjection(ambient F, ambient F0);
    proxyWSet(W0,M,slicingVariety W)
    )
deflate(ProxyWSet, ZZ) := o -> (W,r) -> deflate(polySystem W,r,o)
deflate(ProxyWSet, Point) := o -> (W,P) -> (
    D := deflate(proxyWSet W,P,o);
    proxyWSet(upWSet D,compose(map W, map D),slicingVariety W)
    )

--deflationSequence = method()
--deflationSequence (WSet,P) 

jacobianRank = method()
jacobianRank(PolySystem, Point) := (F,P) -> (
    J := evaluate(jacobian F, P);
    numericalRank J
    )

deflatedWSet = method()
-- IN: 
-- F, PolySystem 
-- m, local dimension 
-- pts, witness points
-- OUT:
-- List of ProxyWitnessSet's 
deflatedWSet(PolySystem,ZZ,List) := (F,m,pts) -> (
    P := partitionViaDeflationSequence(pts,F);
    -- TO DO: make the above robust... precondition, refine when lifting, ???
    --        Is there a way to detect an incorrectly computed numerical rank?
    for wpts in P list (
	F0 := (first wpts).LiftedSystem;
	wpts0 := apply(wpts, p->p.LiftedPoint);
	S0 := new SlicingVariety;
	W0 := wSet(F0,S0,wpts0);
	M := coordinateProjection(ambient F, ambient F0);
	proxyWSet(W0,S0,M)
	-- TO DO: create a ProxyWSet using wpts and the end-of-the-chain LiftedSystem in th chain of deflations
	-- (this chain should be the same for all wpts) 
	)
    )

TEST ///
restart
debug needsPackage "NumericalAlgebraicGeometry"
CC[x_0..x_3]
A = matrix{{x_0,x_1,x_2},{x_1,x_2,x_3},{x_2,x_3,0}}
I = minors(2,A_{0,1}^{0,1}) + det A
Icubic = minors(2,A_{0,1})
cubic = witnessSet(Icubic,2)
peek cubic
errorDepth = 0
deflatedWSet(polySystem(I+ideal slice cubic),2,points cubic)  
///