R = QQ[a..f];
M = genericSkewMatrix(R,a,4)
pfaffians(2,M)
pfaffians(4,M)
S = QQ[y_0..y_14];
M = genericSkewMatrix(S,y_0,6)
pluecker = pfaffians(4,M);
betti res pluecker
secantvariety = pfaffians(6,M)
R = QQ[x_0..x_4]
y = {0,1,13,-13,-1}
M = matrix table(5,5, (i,j)-> x_((i+j)%5)*y_((i-j)%5))
I = pfaffians(4,M);
betti res I
