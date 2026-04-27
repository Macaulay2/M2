needsPackage "MonodromySolver"
setRandomSeed 0
needsPackage "ExampleIdeals"

parametrizedNoon = n -> (
    R := CC[x_1..x_10];
    polys := {x_1*x_2^2+x_1*x_3^2+x_1*x_4^2+x_1*x_5^2+x_1*x_6^2+x_1*x_7^2+x_1*x_8^2+x_1*x_9^2+x_1*x_10^2-1.1*x_1+1,
x_2*x_1^2+x_2*x_3^2+x_2*x_4^2+x_2*x_5^2+x_2*x_6^2+x_2*x_7^2+x_2*x_8^2+x_2*x_9^2+x_2*x_10^2-1.1*x_2+1,
x_3*x_1^2+x_3*x_2^2+x_3*x_4^2+x_3*x_5^2+x_3*x_6^2+x_3*x_7^2+x_3*x_8^2+x_3*x_9^2+x_3*x_10^2-1.1*x_3+1,
x_4*x_1^2+x_4*x_2^2+x_4*x_3^2+x_4*x_5^2+x_4*x_6^2+x_4*x_7^2+x_4*x_8^2+x_4*x_9^2+x_4*x_10^2-1.1*x_4+1,
x_5*x_1^2+x_5*x_2^2+x_5*x_3^2+x_5*x_4^2+x_5*x_6^2+x_5*x_7^2+x_5*x_8^2+x_5*x_9^2+x_5*x_10^2-1.1*x_5+1,
x_6*x_1^2+x_6*x_2^2+x_6*x_3^2+x_6*x_4^2+x_6*x_5^2+x_6*x_7^2+x_6*x_8^2+x_6*x_9^2+x_6*x_10^2-1.1*x_6+1,
x_7*x_1^2+x_7*x_2^2+x_7*x_3^2+x_7*x_4^2+x_7*x_5^2+x_7*x_6^2+x_7*x_8^2+x_7*x_9^2+x_7*x_10^2-1.1*x_7+1,
x_8*x_1^2+x_8*x_2^2+x_8*x_3^2+x_8*x_4^2+x_8*x_5^2+x_8*x_6^2+x_8*x_7^2+x_8*x_9^2+x_8*x_10^2-1.1*x_8+1,
x_9*x_1^2+x_9*x_2^2+x_9*x_3^2+x_9*x_4^2+x_9*x_5^2+x_9*x_6^2+x_9*x_7^2+x_9*x_8^2+x_9*x_10^2-1.1*x_9+1,
x_10*x_1^2+x_10*x_2^2+x_10*x_3^2+x_10*x_4^2+x_10*x_5^2+x_10*x_6^2+x_10*x_7^2+x_10*x_8^2+x_10*x_9^2-1.1*x_10+1};
    ind := flatten apply(#polys,i-> -- indices for parameters
    	apply(exponents polys#i, t->(i,t))
    	);
    AR := CC[apply(ind,i->A_i)][gens R];
    polysP := for i to #polys-1 list -- system with parametric coefficients and same support 
    sum(exponents polys#i, t->A_(i,t)*AR_(t));
    polySystem transpose matrix {polysP}
    )
end ------------------------------------------------

restart ---------------------------------------------------------------------
load "noon10.m2"
nedges = 3
setRandomSeed 0
polys = parametrizedNoon 10 
(p0,x0) = createSeedPair polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume, Verbose=>true)
-* -- Anton's office machine:
mixedVolume = 59029
  node1: 59001
  node2: 59001
trackedPaths 0
     -- 651.542 seconds elapsed

o7 = (HomotopyNode{...5...}, failed)
*-

restart ---------------------------------------------------------------------
load "noon10.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedNoon 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume, Verbose=>true)
-* -- Anton's office machine:
     -- 935.657 seconds elapsed

o7 = (HomotopyNode{...5...}, 236051)
*-

restart ---PHCpack------------------------------------------------------------------
load "noon10.m2"
needsPackage "PHCpack"
polys = parametrizedNoon 10 
(p0,x0) = createSeedPair polySystem polys
specPolys = specializeSystem (p0,polys)
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime (mv,q,qsols) = mixedVolume(specPolys/toR,StartSystem => true);
-- 751.211 seconds elapsed

restart ---Bertini------------------------------------------------------------------
load "noon10.m2"
polys = parametrizedNoon 10 
(p0,x0) = createSeedPair polySystem polys
specPolys = specializeSystem (p0,polys)
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime sols = solveSystem(specPolys/toR, Software=>BERTINI);
-- 2621.02 seconds elapsed

restart --M2engine------------------------------------------------------------------
load "noon10.m2"
polys = parametrizedNoon 10 
(p0,x0) = createSeedPair polySystem polys
specPolys = specializeSystem (p0,polys)
elapsedTime sols = solveSystem specPolys;
-- 250.841 seconds elapsed -- finds only 59027 

