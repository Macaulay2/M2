gbTrace = 3
R = ZZ[x,y,z];
f = matrix{{x^2-3, y^3-1, z^4-2}};
z = koszul(2,f)
g = forceGB(f, SyzygyMatrix=>z);
syz g -- no extra computation
syz f
kernel f
