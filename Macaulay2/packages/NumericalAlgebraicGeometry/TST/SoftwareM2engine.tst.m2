loadPackage "NumericalAlgebraicGeometry"
needs "./../benchmarks.m2"
NAGtrace 1

I = linearExample()
assert areEqual(solveSystem I_*, {{{2,1}}})

for predictor in {RungeKutta4,Tangent,Euler} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine, Predictor=>predictor);
     SM = sortSolutions M;
     print apply(#SM, i-> getSolution(i, SolutionAttributes=>NumberOfSteps));
     assert areEqual( SM, {{{-1, 0}}, {{0, -1}}, {{0, 1}}, {{1, 0}}} );
     )
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine);
assert all({0,2}, i->getSolution(i,SolutionAttributes=>SolutionStatus)=="INFINITY (FAILURE)") 
assert all({1,3}, i->getSolution(i,SolutionAttributes=>SolutionStatus)=="REGULAR") 

(t,rcond,ns) = getSolution(3, SolutionAttributes=>(LastT,RCondition,NumberOfSteps))
assert(t>0.99999 and rcond>0.1 and ns < 20)

T = cyclic(5,CC) 
M = solveSystem(T_*, Software=>M2engine);
S = apply(#M,i->getSolution(i,SolutionAttributes=>SolutionStatus));
assert( #select(S, s->s=="REGULAR") == 70
     and #select(S, s->s=="INFINITY (FAILURE)") + #select(S, s->s=="MIN STEP (FAILURE)") == 50 )

for predictor in {RungeKutta4,Tangent,ProjectiveNewton} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2engine, Predictor=>predictor, Projectivize=>true, Normalize=>true);
     SM = sortSolutions M;
     print apply(#SM, i-> getSolution(i, SolutionAttributes=>NumberOfSteps));
     assert areEqual( SM, {{{ -1, 0}}, {{0, -1}}, {{0, 1}}, {{1, 0}}}, Tolerance=>0.03);
     )		    
end
restart
load "SoftwareM2engine.tst.m2"
T = cyclic(6,CC) 
M = solveSystem(T_*, Software=>M2engine);
S = apply(#M,i->getSolution(i,SolutionAttributes=>SolutionStatus));
#select(S, s->s=="REGULAR")
S = apply(#M,i->getSolution(i,SolutionAttributes=>RCondition));
#select(S, s->s>0.001)


