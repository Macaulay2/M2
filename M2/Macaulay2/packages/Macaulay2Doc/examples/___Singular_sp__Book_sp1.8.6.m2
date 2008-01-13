loadPackage "Elimination";
A = QQ[u,v,x,y,z];
I = ideal "x-uv,y-uv2,z-u2"
eliminate(I,{u,v})
g = map(QQ[u,v],QQ[x,y,z],{x => u*v, y => u*v^2, z => u^2})
coimage g
