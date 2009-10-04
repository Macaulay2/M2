restart
loadPackage "NumericalAlgebraicGeometry"
NAGtrace 1; debug NumericalAlgebraicGeometry;
load "benchmarks.m2"

-- small example with 2 solutions -------------------------------------------
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
T = (katsuraBench 6)_*; (S,solsS) = totalDegreeStartSystem T; 
isH = false;
-- same, but projective
--R = CC[x,y,z];
--T = {x^2+y^2-z^2, x*y};
--(S,solsS) = oneRootStartSystem T;
--isH = true;

M = track(S,T,solsS, gamma=>0.8+0.6*ii,
     Software=>M2,
               --M2engine,
     	       --M2enginePrecookedSLPs,
     Projectivize=>not isH,
     Normalize=>true,
     Predictor=>--Tangent,
		--Euler,
		ProjectiveNewton
     );
print sortSolutions M
--print M /(s->first s/round)

end 
restart
setRandomSeed 0
load "track_test.m2"

-- cyclic roots -------------------------------------------------------------
n = 3;  T = (cyclic(n,CC))_*;
P = sortSolutions solveSystem(T, Software=>PHCpack);
aM = sortSolutions solveSystem T;
rM = refine(T, select(aM, v->norm matrix{first v}<100));
M = select(rM, v->norm matrix{first v} < 100);
all(0..#P-1, i->areEqual(first P#i,first M#i)) -- check if sorted solutions are equal

------------------------------------------------------------------------------
n = 5; T = cyclic(n,CC); R = ring T;
T = ideal(T_*/(f->sub(f,apply(n, i->R_i=>R_i + i)))); -- make it denser 

------------------------------------------------------------------------------
-- Strange things happen for this one: M2(RungeKutta)  gives 91 sols, PHCpack and Bertini - 90,  
--n = 5; T = (cyclic(n,CC))_*; R = ring first T;
--setRandomSeed 0;
--T = T/(f->sub(f,apply(n, i->R_i=>sum(n, j->exp(random(0.,2*pi)*ii)*R_i) + 1) )) 

-- random -------------------------------------------------------------------
setRandomSeed 0; T = randomNFactorialSystem(4,CC) -- N = 4
setRandomSeed 0; T = randomSystem(4,2,CC);

-- Katsura --------------------------------------------------------
T = katsuraBench 5

DBG = 1;
(S,solsS) = totalDegreeStartSystem T_*; isH = false;
T = ideal homogenizeSystem T_*; (S,solsS) = oneRootStartSystem T_*; isH = true;

t = currentTime(); M = track(S,T_*,solsS, 
     gamma=>0.6+0.8*ii, 
     Software=>M2,
     Predictor=>--Secant,
     	        --Euler,
                --Tangent, 
                RungeKutta4,
		--Multistep,
		--ProjectiveNewton,
     Projectivize=>--not isH,
     	       	   false, 
     MultistepDegree=>4,
     AffinePatches=>DynamicPatch,
     --SLP=>HornerForm,
     tStepMin=>0.0001,
     RandomSeed=>1
     ); currentTime()-t
#M -- # of paths
toRR sum(M,s->s#1#1)/#M -- #of steps per path on average
toRR min apply(M,s->s#1#1)
toRR max apply(M,s->s#1#1)
#select(M, v->norm matrix{first v}<100) -- # of "finite" solutions
sort(M/norm@@first)
rM = refine(T, select(M, v->norm matrix{first v}<100));
#select(rM, v->norm matrix{first v} < 100)


t = currentTime(); M = solveSystem T; currentTime()-t
t = currentTime(); P = solveSystem (T,Software=>PHCpack); currentTime()-t
t = currentTime(); B = solveSystem (T,Software=>Bertini); currentTime()-t



