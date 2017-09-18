deflateWSet = method()
-- IN: 
-- F, PolySystem 
-- m, local dimension 
-- pts, witness points
-- OUT:
-- List of ProxyWitnessSet's 
deflateWSet(PolySystem,ZZ,List) := (F,m,pts) -> (
    P := partitionViaDeflationSequence(pts,F);
    -- TO DO: make the above robust... precondition, refine when lifting, ???
    --        Is there a way to detect an incorrectly computed numerical rank?
    for wpts in P list (
	-- TO DO: create a ProxyWSet using wpts and the end-of-the-chain LiftedSystem in th chain of deflations
	-- (this chain should be the same for all wpts) 
	)
    )
