loadPackage "Elimination";
A = QQ[t,x,y,z];
I = ideal"t2+x2+y2+z2,t2+2x2-xy-z2,t+y3-z3";
eliminate(I,t)
A1 = QQ[t,x,y,z,MonomialOrder=>{1,3}];
I = substitute(I,A1);
transpose gens gb I
A2 = QQ[t,x,y,z,MonomialOrder=>Weights=>{1}];
I = substitute(I,A2);
transpose gens gb I
A3 = QQ[t,x,y,z,MonomialOrder=>Eliminate 1];
I = substitute(I,A3);
transpose gens gb I
