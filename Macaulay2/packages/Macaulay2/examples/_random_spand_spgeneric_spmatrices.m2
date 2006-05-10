R = GF(3^2,Variable => a);
random(R^3,R^4)
T = R[x,y];
random(T^3,T^{4:-1})
S = R[p..z];
genericMatrix(S,t,3,2)
genericSymmetricMatrix(S,s,3)
genericSymmetricMatrix(S,u,3)
