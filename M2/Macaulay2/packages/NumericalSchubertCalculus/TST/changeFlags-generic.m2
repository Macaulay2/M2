setRandomSeed 0
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
debug NumericalSchubertCalculus

check'changeFlags = (SchPblm,k,n) -> (
    sols := solveSchubertProblem(SchPblm, k,n, LinearAlgebra=>false);  
    --xsols := solveSchubertProblem(SchPblm, k,n, LinearAlgebra=>true);  
    sols1 := gens\gb\sols;
    assert all(sols,s->checkIncidenceSolution(s, SchPblm));
    Flags1 := SchPblm / last;
    FFF := CC_53;
    Flags2 = apply(#Flags1, i->random(FFF^n,FFF^n));
    MX := bigCellLocalCoordinates(k,n);
    conds := SchPblm / first;
    sols = apply(sols1, s->flatten take(entries s,n-k));
    solsT := changeFlags(MX, sols, (conds, Flags1, Flags2));
    sols2 := changeFlags(MX, sols, (conds, Flags1, Flags2), "one homotopy"=>false);
    assert areEqual(sortSolutions sols2, sortSolutions solsT)
    )
end

restart
load "NumericalSchubertCalculus/TST/changeFlags-generic.m2"
load "NumericalSchubertCalculus/TST/21e3-G36.m2"
time check'changeFlags(SchPblm,3,6)

restart
load "NumericalSchubertCalculus/TST/changeFlags-generic.m2"
load "NumericalSchubertCalculus/TST/2e4-G26.m2"
time check'changeFlags(SchPblm,2,6)
