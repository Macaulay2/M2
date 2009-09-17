restart
loadPackage ("NAG", FileName=>"../../NAG.m2");
load "../benchmarks.m2"

debug NAG; DBG = 2;
for predictor in {RungeKutta4,
     --Multistep,
     Tangent,Euler,Secant} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2, Predictor=>predictor);
     SM = sortSolutions M;
     assert( SM/(s->s/round)@@first == {{-1, 0}, {0, -1}, {0, 1}, {1, 0}} )
     )
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2);
SM = sortSolutions M;
assert all({0,3}, i->SM#i#2#1=="INFINITY (FAILURE)") 
assert all({1,2}, i->SM#i#2#1=="REGULAR") 

-- T = cyclic(5,CC) -- runs for a minute
T = example2()
SM = solveSystem(T_*, Software=>M2)
assert(  #select(SM,s->s#2#1=="REGULAR") == 2
     and #select(SM,s->s#2#1=="INFINITY (FAILURE)") == 14
     and #select(SM,s->s#2#1=="MIN STEP (FAILURE)") == 0 )


