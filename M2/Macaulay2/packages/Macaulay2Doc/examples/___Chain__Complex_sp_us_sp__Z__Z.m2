R = QQ[x,y,z]/(x^3,y^3,z^3,x*y*z);
C = res(coker vars R, LengthLimit=>8)
rank C_7
C.dd_3
