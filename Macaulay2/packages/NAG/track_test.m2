restart
loadPackage ("NAG", FileName=>"../NAG.m2", 
     Configuration=>{"PHCpack"=>"./phc", "Bertini"=>"./bertini", "HOM4PS2"=>"./hom4ps2_in_out"})
debug NAG
DBG = 2;
printingPrecision = 20;


-- small example with 2 solutions -------------------------------------------
R = CC[x,y];
S = {x^2-1,y-1};
T = {x^2+y^2-1, x-y};
solsS = {(1,1),(-1,1)};
solsT = track(S,T,solsS, gamma=>0.2+0.3*ii, tStep=>0.2, Projectivize=>true)

-- cyclic roots -------------------------------------------------------------
cyclic = (n,kk) -> (
     R = kk[vars(100..n+99)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k -> R_((i+k)%n)))) 
       + ideal(product gens R - 1))

n = 5
T = (cyclic(n,CC))_*
--t = currentTime(); M = solveSystem T; currentTime()-t
(S,solsS) = totalDegreeStartSystem T;
t = currentTime(); M = track(S,T,solsS, 
--   gamma = 1 is in exceptional set
     gamma=>0.6+0.8*ii, 
     Predictor=>--Secant,
     	        --Euler,
                --Tangent, 
                RungeKutta4,
     AffinePatches=>{ matrix {{-.124234322572514-.992252907828921*ii, -.417519019676412+.908668183776921*ii, -.985089503023424-.172042643065792*ii, .33160728005355+.943417517229507*ii, -.810395490212434-.585883221677622*ii, .583357854599743-.812215250704384*ii}} }, -- RandomSeed=>0		
     RandomSeed=>1
     ); currentTime()-t
#M -- # of paths
#select(M, v->norm matrix{v}<10) -- # of "finite" solutions
sort(M/norm)
rM = refine(T, select(M, v->norm matrix{v}<10));
#select(rM, v->norm matrix{v} < 10)

t = currentTime(); P = solveSystem (T,Software=>PHCpack); currentTime()-t
t = currentTime(); B = solveSystem (T,Software=>Bertini); currentTime()-t
apply(sort(B#0/(c->c/realPart)) - sort toList(M/(c->c/realPart)), a->apply(a, abs))


