debug needsPackage "NumericalAlgebraicGeometry"
needs "NumericalAlgebraicGeometry/benchmarks.m2"
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
assert all({0,2}, i->status M#i != Regular) 
assert all({1,3}, i->status M#i == Regular) 

assert((M#3).cache.LastT>0.99999 and (M#3).cache.NumberOfSteps < 20)

T = cyclic(5,CC) 
M = solveSystem(T_*, 
    --CorrectorTolerance=>1e-6, tStepMin=>1e-7, tStep=>1e-2, 
    Software=>M2engine, PostProcess=>false);
S = apply(M,status);
assert(#select(S, s->s==Regular) == 70
    and #select(S, s->s==IncreasePrecision) + #select(S, s->s==Infinity) + #select(S, s->s==MinStepFailure) == 50 )

-- projective tracking (including Certified)
for predictor in {RungeKutta4,Tangent,Certified} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine, Predictor=>predictor, Projectivize=>true, Normalize=>true);
     SM = sortSolutions M;
     assert areEqual( SM/coordinates, {{ -1, 0}, {0, -1}, {0, 1}, {1, 0}}, Tolerance=>0.03);
     )		    
for predictor in {RungeKutta4,Tangent,Certified} do (
     T = (fekete4())_*;	    
     (S,solsS) := totalDegreeStartSystem T;
     sols = {{-.707107-1.22474*ii,1_CC}, {1.41421_CC,1_CC}, {-.707107+1.22474*ii,1_CC}, {0_CC,1_CC}};
     for a to 10 do (
	  g = exp((0.1+a)*ii);
     	  s1' = track(S,T,solsS,gamma=>g,Predictor=>predictor);
	  s1 = sortSolutions( s1' / (p->toAffineChart(1,coordinates p)|{1_CC}), Tolerance=>1e-4);
     	  s2 = sortSolutions sols;
	  assert areEqual(s1,s2,Tolerance=>1e-4)
     	  )
     )		    

-- a non-square system
CC[x,y]
solveSystem{x^2,y^2,x+y}
QQ[x,y]
solveSystem{x^2,y^2,x+y}
end

restart
load "SoftwareM2engine.tst.m2"


