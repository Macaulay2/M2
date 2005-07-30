gbTrace = 3;
R = ZZ[x,y,z];
f = matrix{{x^2-3, y^3-1, z^4-2}};
g = forceGB f
g === gb(f, StopBeforeComputation=>true)
gens gb f
