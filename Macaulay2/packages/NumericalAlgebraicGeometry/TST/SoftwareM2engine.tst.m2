needsPackage "NumericalAlgebraicGeometry"
needs "./../benchmarks.m2"
NAGtrace 1

I = linearExample()
assert areEqual(solveSystem(I_*)/coordinates, {{2,1}})

for predictor in {RungeKutta4,Tangent,Euler} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine, Predictor=>predictor);
     SM = sortSolutions M;
     assert areEqual(SM/coordinates, {{-1, 0}, {0, -1}, {0, 1}, {1, 0}} );
     )
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine);
assert all({0,2}, i->M#i#SolutionStatus==Infinity) 
assert all({1,3}, i->M#i#SolutionStatus==Regular) 

assert(M#3#LastT>0.99999 and M#3#RCondition>0.1 and M#3#NumberOfSteps < 20)

T = cyclic(5,CC) 
M = solveSystem(T_*, Software=>M2engine);
S = apply(#M,i->M#i#SolutionStatus);
assert( #select(S, s->s==Regular) == 70
     and #select(S, s->s==Infinity) + #select(S, s->s==MinStepFailure) == 50 )

for predictor in {RungeKutta4,Tangent,Certified} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine, Predictor=>predictor, Projectivize=>true, Normalize=>true);
     SM = sortSolutions M;
     assert areEqual( SM/coordinates, {{ -1, 0}, {0, -1}, {0, 1}, {1, 0}}, Tolerance=>0.03);
     )		    
end
restart
load "SoftwareM2engine.tst.m2"
T = cyclic(6,CC) 
M = solveSystem(T_*, Software=>M2engine);
S = apply(#M,i->M#i#SolutionStatus);
#select(S, s->s==Regular)
S = M/(s->s.RCondition);
#select(S, s->s>0.001)


