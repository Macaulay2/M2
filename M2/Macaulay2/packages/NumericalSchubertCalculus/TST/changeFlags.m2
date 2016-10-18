setRandomSeed 0
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
debug NumericalSchubertCalculus

check'changeFlags = (SchPblm,k,n) -> (
    sols := solveSchubertProblem(SchPblm, k,n, LinearAlgebra=>false);  
    sols1 := gens\gb\sols;
    assert all(sols,s->checkIncidenceSolution(s, SchPblm));
    Flags1 := SchPblm / last;
    FFF := CC_53;
    Flags2 = apply(#Flags1, i->random(FFF^6,FFF^6));
    MX := bigCellLocalCoordinates(k,n);
    conds := SchPblm / first;
    sols = apply(sols1, s->flatten take(entries s,k));
    sols2 := changeFlags(MX, sols, (conds, Flags1, Flags2));
    solsT := changeFlags'oneHomotopy(MX, sols, (conds, Flags1, Flags2));
    assert areEqual(sols2,solsT)
    )
end

restart
load "NumericalSchubertCalculus/TST/changeFlags.m2"

load "NumericalSchubertCalculus/TST/21e3-G36.m2"
check'changeFlags(SchPblm,3,6)

-- this one upsets trackHomotopy
load "NumericalSchubertCalculus/TST/2e4-G26.m2"
check'changeFlags(SchPblm,2,6)
