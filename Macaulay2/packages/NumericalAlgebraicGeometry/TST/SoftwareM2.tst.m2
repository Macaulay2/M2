loadPackage ("NumericalAlgebraicGeometry", FileName=>"../../NumericalAlgebraicGeometry.m2");
needs "../benchmarks.m2"

NAGtrace 1
for predictor in {RungeKutta4,
     --Multistep,
     Tangent,Euler,Secant} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2, Predictor=>predictor);
     SM = apply(sortSolutions M, s->{first s});
     assert areEqual( SM, {{{-1, 0}}, {{0, -1}}, {{0, 1}}, {{1, 0}}} );
     )
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2);
SM = sortSolutions M;
assert all({0,3}, i->SM#i#2#1=="INFINITY (FAILURE)") 
assert all({1,2}, i->SM#i#2#1=="REGULAR") 

for predictor in {RungeKutta4,ProjectiveNewton} do (
     (S,T,solsS) = smallExample();
     M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2, Predictor=>predictor, Projectivize=>true, Normalize=>true);
     SM = apply(sortSolutions M, s->{first s});
     print SM;
     assert areEqual( SM, {{{-1, 0}}, {{0, -1}}, {{0, 1}}, {{1, 0}}}, Tolerance=>1e-3 );
     )

///
(S,T,solsS) = smallInfinityExample()
M = track(S,T,solsS, gamma=>0.6+0.8*ii, Software=>M2, Predictor=>ProjectiveNewton, Projectivize=>true);
SM = sortSolutions M;
assert all({0,3}, i->SM#i#2#1=="INFINITY (FAILURE)")
assert all({1,2}, i->SM#i#2#1=="REGULAR")
///
					     
-- T = cyclic(5,CC) -- runs for a minute
T = example2()
SM = solveSystem(T_*, Software=>M2)
assert(  #select(SM,s->s#2#1=="REGULAR") == 2
     and #select(SM,s->s#2#1=="INFINITY (FAILURE)") == 14
     and #select(SM,s->s#2#1=="MIN STEP (FAILURE)") == 0 )

end
restart
load "SoftwareM2.tst.m2"
