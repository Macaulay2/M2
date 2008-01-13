R = QQ[a..i];
M = genericMatrix(R,a,3,3)
I = ideal(M^3);
f = trace M
G = gb(I, DegreeLimit=>3)
f^7 % G == 0
gb(I, DegreeLimit=>7)           
f^7 % G
gb I
I = ideal(M^3);
G = gb(I, StopBeforeComputation=>true)
f^7 % I
status G
