loadPackage "NumericalAlgebraicGeometry"
needs "../benchmarks.m2"
NAGtrace 1
for predictor in {RungeKutta4,Tangent,Euler} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2enginePrecookedSLPs, SLP=>HornerForm, Predictor=>predictor);
     SM = sortSolutions M;
     assert( SM/(s->s/round)@@first == {{-1, 0}, {0, -1}, {0, 1}, {1, 0}} )
     )
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2enginePrecookedSLPs, SLP=>HornerForm);
assert all({0,2}, i->getSolution(i,SolutionAttributes=>SolutionStatus)=="INFINITY (FAILURE)") 
assert all({1,3}, i->getSolution(i,SolutionAttributes=>SolutionStatus)=="REGULAR") 

end
restart
load "SoftwareM2enginePrecookedSLPs.tst.m2"
