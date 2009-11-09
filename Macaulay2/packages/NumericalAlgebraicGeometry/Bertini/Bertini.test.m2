-- this file is created to test Bertini interface separately from NAG package
restart
load "Bertini.interface.m2"
BERTINIexe = "bertini"; DBG = 0;
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
o = new OptionTable from {gamma=>1+ii} -- NAG's "solve" and "track" pass more options (type "help track")
solveBertini(T,o)
trackBertini(S,T,solsS,o)
