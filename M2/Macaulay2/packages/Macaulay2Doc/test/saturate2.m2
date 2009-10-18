-- bug reported by Leslie Roberts <robertsl@mast.queensu.ca>
K=QQ;
S=K[u,v,a,c,Degrees=>{2,3,1,2}];
P=ideal(v^3-u^3*a^3,u*v^2-c^2*a^4);
Q = saturate(P,a)

assert( Q == quotient(Q,a) )
