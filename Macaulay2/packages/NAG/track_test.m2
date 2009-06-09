restart
--loadPackage ("NAG", FileName=>"../NAG.m2", 
--     Configuration=>{"PHCpack"=>"./phc", "Bertini"=>"./bertini", "HOM4PS2"=>"./hom4ps2_in_out"})
loadPackage "NAG";
debug NAG; DBG = 2; printingPrecision = 20; 
load "benchmarks.m2"

-- small example with 2 solutions -------------------------------------------
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
solsT = track(S,T,solsS, gamma=>1+ii,
     Software=>M2engine,
     --Software=>M2enginePrecookedSLPs,
     Projectivize=>false,
     --Predictor=>Tangent,
     Predictor=>Euler,
     --SLP=>CompiledHornerForm 
     SLP=>HornerForm
     )
norm matrix refine(T, {{1.2_CC,0.2}},  epsilon=>1e-3)
sortSolutions solsT/(s->s#0/round)

-- cyclic roots -------------------------------------------------------------
n = 3;  T = (cyclic(n,CC))_*;
P = sortSolutions solveSystem(T, Software=>PHCpack);
aM = sortSolutions solveSystem T;
rM = refine(T, select(aM, v->norm matrix{first v}<100));
M = select(rM, v->norm matrix{first v} < 100);
all(0..#P-1, i->areEqual(first P#i,first M#i)) -- check if sorted solutions are equal

------------------------------------------------------------------------------
n = 5; T = (cyclic(n,CC))_*; R = ring first T;
T = T/(f->sub(f,apply(n, i->R_i=>R_i + i))); -- make it denser 

------------------------------------------------------------------------------
-- Strange things happen for this one: M2(RungeKutta)  gives 91 sols, PHCpack and Bertini - 90,  
--n = 5; T = (cyclic(n,CC))_*; R = ring first T;
--setRandomSeed 0;
--T = T/(f->sub(f,apply(n, i->R_i=>sum(n, j->exp(random(0.,2*pi)*ii)*R_i) + 1) )) 

-- random -------------------------------------------------------------------
setRandomSeed 0
T = (randomNFactorialSystem(5,CC))_* -- N = 5
(S,solsS) = totalDegreeStartSystem T;


t = currentTime(); M = track(S,T,solsS, 
     --Software=>PHCpack,
     --gamma=>1, -- is in exceptional set
     gamma=>0.6+0.8*ii, 
     Predictor=>--Secant,
     	        --Euler,
                --Tangent, 
                RungeKutta4,
		--Multistep,
     --Projectivize=>false,
     MultistepDegree=>4,
     AffinePatches=>DynamicPatch,
     --AffinePatches=>{ matrix {{-.124234322572514-.992252907828921*ii, -.417519019676412+.908668183776921*ii, -.985089503023424-.172042643065792*ii, .33160728005355+.943417517229507*ii, -.810395490212434-.585883221677622*ii, .583357854599743-.812215250704384*ii}} }, -- RandomSeed=>0		
     SLP=>HornerForm,
     RandomSeed=>1
     ); currentTime()-t

#M -- # of paths
toRR sum(M,s->s#1#1)/#M -- #of steps per path on average
#select(M, v->norm matrix{first v}<100) -- # of "finite" solutions
sort(M/norm@@first)
rM = refine(T, select(M, v->norm matrix{first v}<100));
#select(rM, v->norm matrix{first v} < 100)


t = currentTime(); M = solveSystem T; currentTime()-t
t = currentTime(); P = solveSystem (T,Software=>PHCpack); currentTime()-t
t = currentTime(); B = solveSystem (T,Software=>Bertini); currentTime()-t



