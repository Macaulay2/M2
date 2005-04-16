R = QQ[x,y,z,q];
f = vars R
diff(f, (x+y-z)^2)
f2 = genericMatrix(R,2,2)
diff(f2, (x+y-z)^2)
