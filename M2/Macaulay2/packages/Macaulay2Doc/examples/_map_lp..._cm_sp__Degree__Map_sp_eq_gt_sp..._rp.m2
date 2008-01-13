R = QQ[x,y,z];
S = QQ[t,u];
f = map(S,R,{t^2,t*u,u^2},DegreeMap => i -> 2*i)
isHomogeneous f
M = R^{1,2}
f M
f ** M
