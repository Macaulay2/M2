debug needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 0

-- Problem (1)^9 = 42 in G(3,6)
Pblm = randomSchubertProblemInstance(
    {{1},{1},{1},{1},{1},{1},{1},{1},{1}},3,6
    );
conds = Pblm/first; k = 3; n = 6;
result = solveSchubertProblemViaMonodromy(conds,k,n,Verbose=>true)
end

-- old stuff  
needsPackage "NAGtools"
PH = parametricSegmentHomotopy(PS,X,P)
evaluateH(specialize(PH,vec'p0||vec'p0), transpose matrix s0,0)
setDefault(Software=>M2)
sols = preimageViaMonodromy(PH,p0,{s0})
assert (#sols == 42)
end


restart
load "NumericalSchubertCalculus/EXA/1e9-G36-SLP.m2"



