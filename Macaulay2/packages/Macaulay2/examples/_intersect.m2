R=ZZ/101[a..d];
I=intersect(ideal(a,b),ideal(b,c),ideal(c,d),ideal(d,a))
R=ZZ[x,y,z];
M=image matrix{{3*x},{3*x}};
N=image matrix{{5*y},{5*y}};
P=image matrix{{7*z},{7*z}};
intersect{M,N,P}
