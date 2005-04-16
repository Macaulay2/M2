R = QQ[x,y,z];
d1 = matrix {{x,y}};
d2 = map(source d1, ,{{y*z},{-x*z}});
d1 * d2 == 0
C = new ChainComplex; C.ring = R;
C#0 = target d1; C#1 = source d1; C#2 = source d2;
C.dd#1 = d1; C.dd#2 = d2;
C
HH_0 C
prune HH_1 C
D = chainComplex(matrix{{x,y}}, matrix {{y*z},{-x*z}})
degrees source D.dd_2
