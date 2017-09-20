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
	wpts
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