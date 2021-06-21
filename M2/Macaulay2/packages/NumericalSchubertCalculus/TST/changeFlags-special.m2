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
    setRandomSeed 0;
    extra := 100; -- does not fail with 1000
    preFlag := random(FFF^n,FFF^(n+extra));
    Flags2 = apply(#Flags1, i->submatrix(preFlag,,drop(random toList(0..n+extra-1),extra))); 
    MX := bigCellLocalCoordinates(k,n);
    conds := SchPblm / first;
    sols = apply(sols1, s->flatten take(entries s,n-k));
    solsT := changeFlags'OneHomotopy(MX, sols, (conds, Flags1, Flags2));
    sols2 := changeFlags(MX, sols, (conds, Flags1, Flags2));
    assert areEqual(sortSolutions sols2, sortSolutions solsT,Tolerance=>1e-4)
    )
load "NumericalSchubertCalculus/TST/21e3-G36.m2"
check'changeFlags(SchPblm,3,6)
end
load "NumericalSchubertCalculus/TST/changeFlags-special.m2"

restart
-- this one upsets trackHomotopy
load "NumericalSchubertCalculus/TST/2e4-G26.m2"
check'changeFlags(SchPblm,2,6)
