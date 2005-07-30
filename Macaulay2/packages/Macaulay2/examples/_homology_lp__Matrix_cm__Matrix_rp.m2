R = QQ[x]/x^5;
f = map(R^1,R^1,{{x^3}}, Degree => 3)
M = homology(f,f)
prune M
