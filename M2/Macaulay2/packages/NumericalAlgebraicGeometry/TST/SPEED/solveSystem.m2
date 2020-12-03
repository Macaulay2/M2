restart
load (currentFileDirectory|"../../benchmarks.m2")
needsPackage "NumericalAlgebraicGeometry"
debug SLPexpressions
backtrace = true
errorDepth = 2
R = CC[x,y];
F = {x^2+y^2-1, x*y};
F = {x^10*y-1,y^10*x-1};
solveSystem F

recursionLimit = 1000
n = 5; d = 4; setRandomSeed 0; -- #sols=1024, M2:4, H:11, B:51, P:63
T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T; 
# solveSystem (T, PostProcess=>false)

n = 5; d = 5; setRandomSeed 0; -- #sols=3125, M2:30, H:78, B:402, P:550
T = (randomSystem(n,d,CC))_*; (S,solsS) = totalDegreeStartSystem T;  
# solveSystem (T, PostProcess=>false)

T = (katsuraBench 11)_*; -- #sols=1024, M2:4, H:7, B:15, P:37
# solveSystem (T, PostProcess=>false)

T = (katsuraBench 12)_*; -- #sols=2048, M2:11, H:19, B:37, P:102
# solveSystem (T, PostProcess=>false)
(S,solsS) = totalDegreeStartSystem T; 
elapsedTime time track(S,T,solsS);
end

restart
load "NumericalAlgebraicGeometry/TST/SPEED/solveSystem.m2"

