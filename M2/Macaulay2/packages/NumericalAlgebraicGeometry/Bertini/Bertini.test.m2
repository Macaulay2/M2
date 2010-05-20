-- this file is created to test Bertini interface separately from NAG package
restart
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};

/// -- larger example (commented out)
loadPackage "NumericalAlgebraicGeometry"
load "../benchmarks.m2"
T = (katsuraBench 11)_*; -- #sols=1024, M2:4, H:7, B:15, P:37                                                 
(S,solsS) = totalDegreeStartSystem T; 
///

load "Bertini.interface.m2"
BERTINIexe = "bertini"; DBG = 0;
o = new OptionTable from {gamma=>1+ii} -- NAG's "solve" and "track" pass more options (type "help track")
solsT = solveBertini(T,o)
solsT = trackBertini(S,T,solsS,o);

