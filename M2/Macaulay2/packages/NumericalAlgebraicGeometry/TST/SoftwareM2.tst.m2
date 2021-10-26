needsPackage "NumericalAlgebraicGeometry"
needs "NumericalAlgebraicGeometry/benchmarks.m2"

--NAGtrace 100
for predictor in {RungeKutta4,
     --Multistep,
     Tangent,Euler,Secant} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2, Predictor=>predictor);
     SM = sortSolutions M;
     assert areEqual(SM/coordinates, {{-1, 0}, {0, -1}, {0, 1}, {1, 0}} );
     )
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2);
SM = sortSolutions M;
assert all({0,3}, i->status SM#i=!=Regular) 
assert all({1,2}, i->status SM#i===Regular) 

for predictor in {RungeKutta4,Certified} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2, Predictor=>predictor, Projectivize=>true, Normalize=>true);
     SM = sortSolutions M;
     print SM;
     assert areEqual(SM/coordinates, {{-1, 0}, {0, -1}, {0, 1}, {1, 0}}, Tolerance=>1e-3 );
     )

					     
-- T = cyclic(5,CC) -- runs for a minute
T = example2()
SM = solveSystem(T_*, Software=>M2, PostProcess=>false)
assert(  
    #select(SM,s->status s === Regular) == 2
     and #select(SM,s->status s === Infinity) +
     #select(SM,s->status s === MinStepFailure) == 14 
     )

end
restart
load "NumericalAlgebraicGeometry/TST/SoftwareM2.tst.m2"
